package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain.SetDomain.Default
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.util.Relation


object InvertedIdSet {
  val top = InvertedIdSet().bottom()
  val bottom = InvertedIdSet().top()
}

case class InvertedIdSet(wrapped: SetDomain.Default[Identifier] = SetDomain.Default.Bottom[Identifier]())
  extends InvertedSetDomain[Identifier, InvertedIdSet] {



  def fold(idA: Set[Identifier], id: Identifier): InvertedIdSet = wrapped match {
    case SetDomain.Default.Bottom() => this
    case SetDomain.Default.Top() => this
    case x@SetDomain.Default.Inner(value) =>
      if (idA.subsetOf(value)) wrapperFactory(x.copy(value = (value -- idA) + id))
      else wrapperFactory(x.copy(value = value -- idA))
  }

  def expand(idA: Identifier, idsB: Set[Identifier]): InvertedIdSet = wrapped match {
    case SetDomain.Default.Bottom() => this
    case SetDomain.Default.Top() => this
    case x@SetDomain.Default.Inner(value) =>
    if (value.contains(idA)) wrapperFactory(x.copy(value = (value - idA) ++ idsB)) else this
  }

  def rename(idA: Identifier, idB: Identifier): InvertedIdSet = wrapped match {
    case SetDomain.Default.Bottom() => this
    case SetDomain.Default.Top() => this
    case x@SetDomain.Default.Inner(value) =>
      if (value.contains(idA)) wrapperFactory(x.copy(value = (value - idA) + idB)) else this
  }

  def remove(ids: Set[Identifier]): InvertedIdSet =wrapped match {
    case SetDomain.Default.Bottom() => this
    case SetDomain.Default.Top() => this
    case x@SetDomain.Default.Inner(value) => wrapperFactory(x.copy(value = value -- ids))
  }

  override def toString:String = wrapped match {
    case SetDomain.Default.Bottom() => "Bottom"
    case SetDomain.Default.Top() =>    "Top"
    case SetDomain.Default.Inner(value) => value.mkString(",")
  }

  def getConstraints(id:Identifier):Set[Expression] = wrapped match {
    case SetDomain.Default.Bottom() =>      Set.empty
    case SetDomain.Default.Top() =>         Set.empty
    case SetDomain.Default.Inner(value) =>  for (v <- value) yield BinaryArithmeticExpression(id, v, ArithmeticOperator.<)
  }

  override def wrapperFactory(wrapped: Default[Identifier]): InvertedIdSet = InvertedIdSet(wrapped)

}


object UpperBoundRelation {
  val bottom = UpperBoundRelation(Relation.empty[Identifier],isBottom = true)
}

case class UpperBoundRelation( override protected val _elements: Relation[Identifier] = Relation.empty[Identifier],
                               override val isBottom:Boolean = false)
  extends RelationalDomain[UpperBoundRelation]
  with RelationalNumericalDomain[UpperBoundRelation] {

  override protected def factory(rel: Relation[Identifier]): UpperBoundRelation =
    UpperBoundRelation(rel)

  override def bottom(): UpperBoundRelation =
    UpperBoundRelation.bottom

  /**
   * This method assumes that a given expression hold
   *
   * @param expr the expression to be assumed
   * @return the state after this action
   */
  override def assume(expr: Expression): UpperBoundRelation =
    expr match {
      case Constant("true",_,_) => this
      case Constant("false",_,_) => this.bottom()
      case NegatedBooleanExpression(Constant("true",_,_)) => this.bottom()
      case NegatedBooleanExpression(Constant("false",_,_)) => this
      case NegatedBooleanExpression(NegatedBooleanExpression(x)) =>
        this.assume(x)
      case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
        assume(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op), typ))
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, _) =>
        assume(left).assume(right)
      case BinaryBooleanExpression(left, right, BooleanOperator.||, _) =>
        assume(left).lub(assume(right))
      case xp:BinaryArithmeticExpression =>
        Normalizer.conditionalExpressionToMonomes(xp) match {
          case None =>
            this
          case Some((weightedIds,constant)) =>
            // Sum of weightedIds + constant >= 0
            // We only cover the case with two variables here
            if (weightedIds.length == 2) {
              val List(first,second) = weightedIds.take(2)
              if (constant < 0) {
                if (first._1 > 0 && first._1 == -second._1 ) {
                  // af >= as + c > as
                  addBound(second._2,first._2)
                } else if (first._1 < 0 && first._1 == -second._1 ) {
                  // as >= af + c > af
                  addBound(first._2,second._2)
                } else this
              } else this
            } else this
        }
      case _ => this
    }


  def addBound(id1:Identifier,id2:Identifier):UpperBoundRelation = {
    if (elements.getLeftOrElse(id2,Set.empty).contains(id1)) return bottom()
    factory(elements.add(id1, id2))
  }

  /**
   * This method creates a variable.
   *
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action
   */
  override def createVariable(variable: Identifier, typ: Type): UpperBoundRelation =
    this

  /**
   * This method assigns a given variable to the given expression
   *
   * @param variable the variable to be assigned
   * @param expr the expression to be assigned
   * @return the state after this action
   */
  override def assign(variable: Identifier, expr: Expression): UpperBoundRelation =
    factory(elements.setLeft(variable,eval(expr)))

  /**
   * This method returns representing the value of the given identifier
   *
   * @param id the identifier
   * @return the string representing its state
   */
  override def getStringOfId(id: Identifier): String = {
    if (isBottom) return "⊥"
    if (isTop) return "T"

    val left = elements.getLeftOrElse(id,Set.empty)
    val right = elements.getRightOrElse(id,Set.empty)
    if (left.isEmpty && right.isEmpty) return "T"
    (left.map( id + "<" + _ ) ++ right.map( id + ">" + _ )).mkString(",")
  }

  /** We do not implement this */
  override def ids = ???

  /**
   * Returns the upper bounds on the given expression
   */
  def eval(expr:Expression):Set[Identifier] = {
    expr match {
      case id:Identifier => elements.getLeftOrElse(id,Set.empty)
      case xp:BinaryArithmeticExpression =>
        Normalizer.arithmeticExpressionToMonomes(xp) match {
          case None => Set.empty
          case Some((weightedIds,constant)) =>
            // Sum of weightedIds + constant.
            // We only cover the case with one variable here
            if (weightedIds.length == 1) {
              val first = weightedIds.head
              if (constant < 0 && first._1 == 1) {
                // f - a < f < up(f) for a != 0
                elements.getLeftOrElse(first._2,Set.empty) + first._2
              } else if (constant == 0 && first._1 == 1) {
                // f < up(f)
                elements.getLeftOrElse(first._2,Set.empty)
              } else Set.empty
            } else Set.empty
        }
      case _ => Set.empty
    }
  }
}


/**
 * Does not store an environment - is not defined for all existing identifiers
 *
 * @param map Maps some identifiers to upper bounds (other identifiers)
 * @param isBottom Sets the domain explicitly to top
 * @param isTop Sets the domain explicitly to bottom
 */
case class StrictUpperBounds(map: Map[Identifier, InvertedIdSet] = Map.empty[Identifier, InvertedIdSet],
                      override val isBottom: Boolean = false,
                      isTop: Boolean = false)
  extends BoxedDomain[InvertedIdSet, StrictUpperBounds]
//  with SimplifiedMergeDomain[StrictUpperBounds]
  with SimplifiedSemanticDomain[StrictUpperBounds]
  with RelationalNumericalDomain[StrictUpperBounds] {

  def functionalFactory(_value: Map[Identifier, InvertedIdSet] = Map.empty[Identifier, InvertedIdSet],
                        _isBottom: Boolean = false,
                        _isTop: Boolean = false): StrictUpperBounds =
    new StrictUpperBounds(_value, _isBottom, _isTop)

  override def get(key: Identifier): InvertedIdSet = this.map.getOrElse(key,InvertedIdSet.bottom)

  override def factory() = new StrictUpperBounds()

  override def getStringOfId(id: Identifier): String = {
    if (isBottom) return "⊥"
    if (isTop || map.getOrElse(id, InvertedIdSet.bottom).isTop) return "T"
    map.getOrElse(id, InvertedIdSet.bottom).toString
  }

  override def setToTop(id: Identifier): StrictUpperBounds =
    copy(map = map + (id -> InvertedIdSet.top))

  override def assign(variable: Identifier, expr: Expression): StrictUpperBounds =
    copy(map = map + (variable -> eval(expr)))

  override def assume(expr: Expression): StrictUpperBounds =
    expr match {
      case Constant("true",_,_) => this
      case Constant("false",_,_) => this.bottom()
      case NegatedBooleanExpression(Constant("true",_,_)) => this.bottom()
      case NegatedBooleanExpression(Constant("false",_,_)) => this
      case NegatedBooleanExpression(NegatedBooleanExpression(x)) =>
        this.assume(x)
      case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
        assume(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op), typ))
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, _) =>
        assume(left).assume(right)
      case BinaryBooleanExpression(left, right, BooleanOperator.||, _) =>
        assume(left).lub(assume(right))
      case xp:BinaryArithmeticExpression =>
        Normalizer.conditionalExpressionToMonomes(xp) match {
          case None => this
          case Some((weightedIds,constant)) =>
            // Sum of weightedIds + constant >= 0
            // We only cover the case with two variables here
            if (weightedIds.length == 2) {
              val List(first,second) = weightedIds.take(2)
              if (constant < 0) {
                if (first._1 > 0 && first._1 == -second._1 ) {
                  // af >= as + c > as
                  addBound(second._2,first._2)
                } else if (first._1 < 0 && first._1 == -second._1 ) {
                  // as >= af + c > af
                  addBound(first._2,second._2)
                } else this
              } else this
            } else this
        }
      case _ => this
    }

//  override def expand(idA: Identifier, idsB: Set[Identifier]): StrictUpperBounds =
//    copy(map = (map - idA ++ idsB.map { x => x -> map.getOrElse(idA, InvertedIdSet.top)}).mapValues(_.expand(idA, idsB)))
//
//  override def rename(idA: Identifier, idB: Identifier): StrictUpperBounds =
//    copy(map = (map - idA + (idB -> map.getOrElse(idA, InvertedIdSet.top))).mapValues(_.rename(idA, idB)))
//
//  override def remove(ids: Set[Identifier]): StrictUpperBounds =
//    copy(map = (map -- ids).mapValues(_.remove(ids)))
//
//  override def fold(idsA: Set[Identifier], idB: Identifier): StrictUpperBounds =
//    copy(map = (map -- idsA + (idB -> idsA.foldRight(InvertedIdSet.bottom)(map.getOrElse(_, InvertedIdSet.top).lub(_)))).mapValues(_.fold(idsA, idB)))
//
//  override def add(ids: Set[Identifier]): StrictUpperBounds =
//    copy(map = map ++ ids.map { x => x -> InvertedIdSet.top})

  override def createVariable(variable: Identifier, typ: Type): StrictUpperBounds =
    copy(map = map + (variable -> InvertedIdSet.top))

  override def removeVariable(id: Identifier): StrictUpperBounds =
    copy(map = (map - id).mapValues(_.remove(id)))

  override def getConstraints(ids: Set[Identifier]): Set[Expression] = {
    (for (id <- ids) yield {
      this.map.getOrElse(id, InvertedIdSet.top).getConstraints(id)
    }).flatten
  }

  /**
   * Adds a bound
   */
  def addBound(lower: Identifier, upper: Identifier): StrictUpperBounds = {
    val currentBounds = get(lower)
    copy(map = map + (lower -> currentBounds.add(upper)))
  }

  /**
   * Returns the upper bounds on the given expression
   */
  def eval(expr:Expression):InvertedIdSet = {
    expr match {
      case id:Identifier => this.get(id)
      case xp:BinaryArithmeticExpression =>
        Normalizer.arithmeticExpressionToMonomes(xp) match {
          case None => InvertedIdSet.top
          case Some((weightedIds,constant)) =>
            // Sum of weightedIds + constant.
            // We only cover the case with one variable here
            if (weightedIds.length == 1) {
              val first = weightedIds.head
              if (constant < 0 && first._1 == 1) {
                // f - a < f < up(f) for a != 0
                this.get(first._2).add(first._2)
              } else if (constant == 0 && first._1 == 1) {
                // f < up(f)
                this.get(first._2)
              } else InvertedIdSet.top
            } else InvertedIdSet.top
        }
      case _ => InvertedIdSet.top
    }
  }

}

case class Pentagons(_1: BoxedNonRelationalNumericalDomain[IntegerInterval], _2: UpperBoundRelation)
  extends SemanticCartesianProductDomain[BoxedNonRelationalNumericalDomain[IntegerInterval], UpperBoundRelation, Pentagons]
  with RelationalNumericalDomain[Pentagons]
{

  override def getStringOfId(id: Identifier) =
    this._1.getStringOfId(id) + ", " + this._2.getStringOfId(id)

  override def toString = ids map { x:Identifier => x.toString + " -> " + getStringOfId(x) } mkString "\n"

  override def factory(a: BoxedNonRelationalNumericalDomain[IntegerInterval], b: UpperBoundRelation) = Pentagons(a, b)

  override def ids = _1.ids

}

case class DoublePentagons(_1: BoxedNonRelationalNumericalDomain[DoubleInterval], _2: UpperBoundRelation)
  extends SemanticCartesianProductDomain[BoxedNonRelationalNumericalDomain[DoubleInterval], UpperBoundRelation, DoublePentagons]
  with RelationalNumericalDomain[DoublePentagons]
{

  override def getStringOfId(id: Identifier) =
    this._1.getStringOfId(id) + ", " + this._2.getStringOfId(id)

  override def toString = ids map { x:Identifier => x.toString + " -> " + getStringOfId(x) } mkString "\n"

  override def factory(a: BoxedNonRelationalNumericalDomain[DoubleInterval], b: UpperBoundRelation) = DoublePentagons(a, b)

  override def ids = _1.ids

}

