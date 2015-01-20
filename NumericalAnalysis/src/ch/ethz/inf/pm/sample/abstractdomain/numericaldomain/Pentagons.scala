package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.SignValues._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.util.Relation


object InvertedIdSet {
  val top = InvertedIdSet(isTop = true)
  val bottom = InvertedIdSet(isBottom = true)
}

case class InvertedIdSet(
                          value: Set[Identifier] = Set.empty[Identifier],
                          isTop: Boolean = false,
                          isBottom: Boolean = false)
  extends InverseSetDomain[Identifier, InvertedIdSet]
  with Lattice.Must[InvertedIdSet] {

  def fold(idA: Set[Identifier], id: Identifier): InvertedIdSet =
    if (idA.subsetOf(value)) copy(value = (value -- idA) + id)
    else copy(value = value -- idA)

  def expand(idA: Identifier, idsB: Set[Identifier]): InvertedIdSet =
    if (value.contains(idA)) copy(value = (value - idA) ++ idsB) else this

  def rename(idA: Identifier, idB: Identifier): InvertedIdSet =
    if (value.contains(idA)) copy(value = (value - idA) + idB) else this

  def remove(ids: Set[Identifier]): InvertedIdSet =
    copy(value = value -- ids)

  def setFactory(
                  value: Set[Identifier] = Set.empty[Identifier],
                  isTop: Boolean = false,
                  isBottom: Boolean = false) =
    InvertedIdSet(value, isTop, isBottom)

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
    map.getOrElse(id, InvertedIdSet.bottom).value.mkString(",")
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


  /**
   * For each set of identifiers in the domain of f, this method merges these identifiers
   * into the given one.
   *
   * @param f The identifiers to merge
   * @return the state after the merge
   */
  override def merge(f: Replacement): StrictUpperBounds = {
    var cur = this
    for ((from, to) <- f.value) {
      if (from.size == 1 && to.size > 1) cur = cur.expand(from.head, to)
      else if (from.size > 1 && to.size == 1) cur = cur.fold(from, to.head)
      else if (from.size == 1 && to.size == 1) cur = cur.rename(from.head, to.head)
      else if (to.size == 0) cur = cur.remove(from)
      else if (from.size == 0) cur = cur.add(to)
      else new NotImplementedError("This domain only supports fold, expand, rename, remove and add; No general replacement support.")
    }
    return cur
  }

  def expand(idA: Identifier, idsB: Set[Identifier]): StrictUpperBounds =
    copy(map = (map - idA ++ idsB.map { x => x -> map.getOrElse(idA, InvertedIdSet.top)}).mapValues(_.expand(idA, idsB)))

  def rename(idA: Identifier, idB: Identifier): StrictUpperBounds =
    copy(map = (map - idA + (idB -> map.getOrElse(idA, InvertedIdSet.top))).mapValues(_.rename(idA, idB)))

  def remove(ids: Set[Identifier]): StrictUpperBounds =
    copy(map = (map -- ids).mapValues(_.remove(ids)))

  def fold(idsA: Set[Identifier], idB: Identifier): StrictUpperBounds =
    copy(map = (map -- idsA + (idB -> idsA.foldRight(InvertedIdSet.bottom)(map.getOrElse(_, InvertedIdSet.top).lub(_)))).mapValues(_.fold(idsA, idB)))

  def add(ids: Set[Identifier]): StrictUpperBounds =
    copy(map = map ++ ids.map { x => x -> InvertedIdSet.top})

  override def createVariable(variable: Identifier, typ: Type): StrictUpperBounds =
    copy(map = map + (variable -> InvertedIdSet.top))

  override def removeVariable(id: Identifier): StrictUpperBounds =
    copy(map = (map - id).mapValues(_.remove(id)))

  override def getConstraints(ids: Set[Identifier]): Set[Expression] = {
    for (id <- ids; bound <- this.map.getOrElse(id, InvertedIdSet.top).value) yield {
      BinaryArithmeticExpression(id, bound, ArithmeticOperator.<)
    }
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

case class Pentagons(_1: BoxedNonRelationalNumericalDomain[Interval], _2: StrictUpperBounds)
  extends SemanticCartesianProductDomain[BoxedNonRelationalNumericalDomain[Interval], StrictUpperBounds, Pentagons]
  with RelationalNumericalDomain[Pentagons]
{

  override def getStringOfId(id: Identifier) =
    this._1.getStringOfId(id) + ", " + this._2.getStringOfId(id)

  override def toString = ids map { x:Identifier => x.toString + " -> " + getStringOfId(x) } mkString "\n"

  override def factory(a: BoxedNonRelationalNumericalDomain[Interval], b: StrictUpperBounds) = Pentagons(a, b)


}

case class DoublePentagons(_1: BoxedNonRelationalNumericalDomain[DoubleInterval], _2: StrictUpperBounds)
  extends SemanticCartesianProductDomain[BoxedNonRelationalNumericalDomain[DoubleInterval], StrictUpperBounds, DoublePentagons]
  with RelationalNumericalDomain[DoublePentagons]
{

  override def getStringOfId(id: Identifier) =
    this._1.getStringOfId(id) + ", " + this._2.getStringOfId(id)

  override def toString = ids map { x:Identifier => x.toString + " -> " + getStringOfId(x) } mkString "\n"

  override def factory(a: BoxedNonRelationalNumericalDomain[DoubleInterval], b: StrictUpperBounds) = DoublePentagons(a, b)

}
