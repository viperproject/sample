package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain.SetDomain.Default
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Normalizer.Monomial
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.util.Relation

//
//object InvertedIdSet {
//  val top = InvertedIdSet().bottom()
//  val bottom = InvertedIdSet().top()
//}
//
//case class InvertedIdSet(wrapped: SetDomain.Default[Identifier] = SetDomain.Default.Bottom[Identifier]())
//  extends InvertedSetDomain[Identifier, InvertedIdSet] {
//
//
//
//  def fold(idA: Set[Identifier], id: Identifier): InvertedIdSet = wrapped match {
//    case SetDomain.Default.Bottom() => this
//    case SetDomain.Default.Top() => this
//    case x@SetDomain.Default.Inner(value) =>
//      if (idA.subsetOf(value)) wrapperFactory(x.copy(value = (value -- idA) + id))
//      else wrapperFactory(x.copy(value = value -- idA))
//  }
//
//  def expand(idA: Identifier, idsB: Set[Identifier]): InvertedIdSet = wrapped match {
//    case SetDomain.Default.Bottom() => this
//    case SetDomain.Default.Top() => this
//    case x@SetDomain.Default.Inner(value) =>
//    if (value.contains(idA)) wrapperFactory(x.copy(value = (value - idA) ++ idsB)) else this
//  }
//
//  def rename(idA: Identifier, idB: Identifier): InvertedIdSet = wrapped match {
//    case SetDomain.Default.Bottom() => this
//    case SetDomain.Default.Top() => this
//    case x@SetDomain.Default.Inner(value) =>
//      if (value.contains(idA)) wrapperFactory(x.copy(value = (value - idA) + idB)) else this
//  }
//
//  def remove(ids: Set[Identifier]): InvertedIdSet =wrapped match {
//    case SetDomain.Default.Bottom() => this
//    case SetDomain.Default.Top() => this
//    case x@SetDomain.Default.Inner(value) => wrapperFactory(x.copy(value = value -- ids))
//  }
//
//  override def toString:String = wrapped match {
//    case SetDomain.Default.Bottom() => "Top"
//    case SetDomain.Default.Top() =>    "Bottom"
//    case SetDomain.Default.Inner(value) => value.mkString(",")
//  }
//
//  def getConstraints(id:Identifier,op:ArithmeticOperator.Value):Set[Expression] = wrapped match {
//    case SetDomain.Default.Bottom() =>      Set.empty
//    case SetDomain.Default.Top() =>         Set.empty
//    case SetDomain.Default.Inner(value) =>  for (v <- value) yield BinaryArithmeticExpression(id, v, op)
//  }
//
//  override def wrapperFactory(wrapped: Default[Identifier]): InvertedIdSet = InvertedIdSet(wrapped)
//
//}


object UpperBoundRelation {
  val bottom = UpperBoundRelation(Relation.empty[Identifier],isBottom = true)
}

case class UpperBoundRelation( override protected val _elements: Relation[Identifier] = Relation.empty[Identifier],
                               override val isBottom:Boolean = false)
  extends RelationalDomain[UpperBoundRelation]
  with RelationalNumericalDomain[UpperBoundRelation]
  with BooleanExpressionSimplifier[UpperBoundRelation] {

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
  override def assumeSimplified(expr: Expression): UpperBoundRelation =
    expr match {
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.!=, typ) =>
        val newLeft = BinaryArithmeticExpression(left, right, ArithmeticOperator.>, typ)
        val newRight = BinaryArithmeticExpression(left, right, ArithmeticOperator.<, typ)
        val res = assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||, typ))
        return res
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.==, typ)
        if left.isInstanceOf[Identifier] && right.isInstanceOf[Identifier] =>
        val l = left.asInstanceOf[Identifier]
        val r = left.asInstanceOf[Identifier]

        var curState = this
        curState = elements.getLeftOrElse(l,Set.empty).foldLeft(curState)(_.addBound(r,_))
        curState = elements.getLeftOrElse(r,Set.empty).foldLeft(curState)(_.addBound(l,_))
        curState = elements.getRightOrElse(l,Set.empty).foldLeft(curState)(_.addBound(_,r))
        curState = elements.getRightOrElse(r,Set.empty).foldLeft(curState)(_.addBound(_,l))
        curState

      case xp:BinaryArithmeticExpression =>
        Normalizer.conditionalExpressionToMonomial(xp) match {
          case None =>
            this
          case Some(Monomial(weightedIds,constant)) =>
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
          case Some(Monomial(weightedIds,constant)) =>
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

//
///**
// * Does not store an environment - is not defined for all existing identifiers
// *
// * @param map Maps some identifiers to an equality class
// * @param isBottom Sets the domain explicitly to top
// * @param isTop Sets the domain explicitly to bottom
// */
//case class EqualityRelation(map: Map[Identifier, Set[Identifier]] = Map.empty[Identifier, Set[Identifier]],
//                      override val isBottom: Boolean = false,
//                      override val isTop: Boolean = false)
//  extends SimplifiedSemanticDomain[EqualityRelation]
//  with BooleanExpressionSimplifier[EqualityRelation]
//  with SimplifiedMergeDomain[EqualityRelation]
//  with RelationalNumericalDomain[EqualityRelation] {
//
//  def get(id:Identifier) = map.getOrElse(id, Set(id))
//
//  override def factory() = EqualityRelation()
//
//  override def getStringOfId(id: Identifier): String = {
//    if (isBottom) return "⊥"
//    if (isTop) return "T"
//    get(id).map{ _.toString + "==" + id.toString }.mkString(",")
//  }
//
//  override def setToTop(id: Identifier): EqualityRelation = remove(Set(id))
//
//
//  override def assign(variable: Identifier, expr: Expression): EqualityRelation =
//    eval(expr) match {
//      case Some(otherId) => setClass(get(otherId) + variable)
//      case None => this
//    }
//
//  override def assume(expr: Expression): EqualityRelation =
//    expr match {
//      case BinaryArithmeticExpression(left,right,ArithmeticOperator.==,_) =>
//        (eval(left),eval(right)) match {
//          case (Some(x),Some(y)) => setClass(get(x) ++ get(y))
//          case _ => this
//        }
//      case BinaryArithmeticExpression(left,right,ArithmeticOperator.!=,_) =>
//        (eval(left),eval(right)) match {
//          case (Some(x),Some(y)) => if(get(x).contains(y)) bottom() else this
//          case _ => this
//        }
//      case BinaryArithmeticExpression(left,right,ArithmeticOperator.>,_) =>
//        (eval(left),eval(right)) match {
//          case (Some(x),Some(y)) => if(get(x).contains(y)) bottom() else this
//          case _ => this
//        }
//      case BinaryArithmeticExpression(left,right,ArithmeticOperator.<,_) =>
//        (eval(left),eval(right)) match {
//          case (Some(x),Some(y)) => if(get(x).contains(y)) bottom() else this
//          case _ => this
//        }
//      case _ => this
//    }
//
//  override def expand(idA: Identifier, idsB: Set[Identifier]): EqualityRelation =
//    setClass(get(idA) ++ idsB).remove(Set(idA))
//
//  override def rename(idA: Identifier, idB: Identifier): EqualityRelation =
//    setClass(get(idA) + idB).remove(Set(idB))
//
//  def remove(id: Identifier): EqualityRelation = {
//    val curClass = get(id) - id
//    EqualityRelation(curClass.foldLeft(map - id){ (x:Map[Identifier,Set[Identifier]],y:Identifier) => x + (y -> curClass) })
//  }
//
//  override def remove(ids: Set[Identifier]): EqualityRelation =
//    ids.foldLeft(this)(_ remove _)
//
//  def setClass(newEqualityClass:Set[Identifier]) = {
//    EqualityRelation(newEqualityClass.foldLeft(map){ (x:Map[Identifier,Set[Identifier]],y:Identifier) => x + (y -> newEqualityClass ) })
//  }
//
//  /** if they are all in the same, fold. otherwise, new class */
//  override def fold(idsA: Set[Identifier], idB: Identifier): EqualityRelation =
//    if (get(idsA.head) == idsA) setClass(get(idsA.head) + idB).remove(idsA)
//    else setClass(Set(idB)).remove(idsA)
//
//  override def add(ids: Set[Identifier]): EqualityRelation = this // Lazy!
//
//  override def createVariable(variable: Identifier, typ: Type): EqualityRelation = this // Lazy!
//
//  override def removeVariable(id: Identifier): EqualityRelation = remove(Set(id))
//
//  override def getConstraints(ids: Set[Identifier]): Set[Expression] = {
//    (for (id <- ids) yield {
//      get(id).map(BinaryArithmeticExpression(_,id,ArithmeticOperator.==,id.typ))
//    }).flatten
//  }
//
//  /**
//   * Returns the upper bounds on the given expression
//   */
//  def eval(expr:Expression):Option[Identifier] = {
//    expr match {
//      case id:Identifier => Some(id)
//      case xp:BinaryArithmeticExpression =>
//        Normalizer.arithmeticExpressionToMonomes(xp) match {
//          case None => None
//          case Some(Monomial(weightedIds,constant)) =>
//            if (weightedIds.length == 1 && weightedIds.head._1 == 1 && constant == 0) {
//              Some(weightedIds.head._2)
//            } else None
//        }
//      case _ => None
//    }
//  }
//
//}

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

