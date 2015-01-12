package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type


object InvertedIdSet {
  val top = InvertedIdSet(isTop = true)
  val bottom = InvertedIdSet(isBottom = true)
}

case class InvertedIdSet(
                          value: Set[Identifier] = Set.empty[Identifier],
                          isTop: Boolean = false,
                          isBottom: Boolean = false)
  extends InverseSetDomain[Identifier, InvertedIdSet] {

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
case class UpperBound(map: Map[Identifier, InvertedIdSet] = Map.empty[Identifier, InvertedIdSet],
                      override val isBottom: Boolean = false,
                      isTop: Boolean = false)
  extends BoxedDomain[InvertedIdSet, UpperBound]
  with SimplifiedSemanticDomain[UpperBound]
  with RelationalNumericalDomain[UpperBound] {

  def functionalFactory(_value: Map[Identifier, InvertedIdSet] = Map.empty[Identifier, InvertedIdSet],
                        _isBottom: Boolean = false,
                        _isTop: Boolean = false): UpperBound =
    new UpperBound(_value, _isBottom, _isTop)

  override def get(key: Identifier): InvertedIdSet = this.map.get(key) match {
    case Some(s) => s;
    case None => new InvertedIdSet();
  }

  override def factory() = new UpperBound()

  override def getStringOfId(id: Identifier): String = {
    if (isBottom) return "⊥"
    if (isTop || map.getOrElse(id, InvertedIdSet.bottom).isTop) return "T"
    map.getOrElse(id, InvertedIdSet.bottom).value.mkString(",")
  }

  override def setToTop(id: Identifier): UpperBound =
    copy(map = map + (id -> InvertedIdSet.top))

  override def assign(variable: Identifier, expr: Expression): UpperBound =
    copy(map = map + (variable -> InvertedIdSet.top))

  def addBound(lower: Identifier, upper: Identifier): UpperBound =
    copy(map = map + (lower -> InvertedIdSet(Set(upper))))

  override def assume(expr: Expression): UpperBound =
    expr match {
      case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
        assume(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op), typ))
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, _) =>
        assume(left).assume(right)
      case BinaryBooleanExpression(left, right, BooleanOperator.||, _) =>
        assume(left).lub(assume(right))
      case BinaryArithmeticExpression(left: Identifier, right: Identifier, ArithmeticOperator.<, typ) =>
        addBound(left, right)
      case BinaryArithmeticExpression(left: Identifier, right: Identifier, ArithmeticOperator.>, typ) =>
        this.addBound(right, left)
      case _ => return this
    }


  /**
   * For each set of identifiers in the domain of f, this method merges these identifiers
   * into the given one.
   *
   * @param f The identifiers to merge
   * @return the state after the merge
   */
  override def merge(f: Replacement): UpperBound = {
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

  def expand(idA: Identifier, idsB: Set[Identifier]): UpperBound =
    copy(map = (map - idA ++ idsB.map { x => x -> map.getOrElse(idA, InvertedIdSet.top)}).mapValues(_.expand(idA, idsB)))

  def rename(idA: Identifier, idB: Identifier): UpperBound =
    copy(map = (map - idA + (idB -> map.getOrElse(idA, InvertedIdSet.top))).mapValues(_.rename(idA, idB)))

  def remove(ids: Set[Identifier]): UpperBound =
    copy(map = (map -- ids).mapValues(_.remove(ids)))

  def fold(idsA: Set[Identifier], idB: Identifier): UpperBound =
    copy(map = (map -- idsA + (idB -> idsA.foldRight(InvertedIdSet.bottom)(map.getOrElse(_, InvertedIdSet.top).lub(_)))).mapValues(_.fold(idsA, idB)))

  def add(ids: Set[Identifier]): UpperBound =
    copy(map = map ++ ids.map { x => x -> InvertedIdSet.top})

  override def createVariable(variable: Identifier, typ: Type): UpperBound =
    copy(map = map + (variable -> InvertedIdSet.top))

  override def removeVariable(id: Identifier): UpperBound =
    copy(map = (map - id).mapValues(_.remove(id)))

  override def getConstraints(ids: Set[Identifier]): Set[Expression] = {
    for (id <- ids; bound <- this.map.getOrElse(id, InvertedIdSet.top).value) yield {
      BinaryArithmeticExpression(id, bound, ArithmeticOperator.<)
    }
  }
}

case class Pentagons(_1: BoxedNonRelationalNumericalDomain[Interval], _2: UpperBound)
  extends SemanticCartesianProductDomain[BoxedNonRelationalNumericalDomain[Interval], UpperBound, Pentagons]
  with RelationalNumericalDomain[Pentagons]
{

  override def getStringOfId(id: Identifier) =
    this._1.getStringOfId(id) + ", " + this._2.getStringOfId(id)

  override def toString = ids map { x:Identifier => x.toString + " -> " + getStringOfId(x) } mkString "\n"

  override def factory(a: BoxedNonRelationalNumericalDomain[Interval], b: UpperBound) = Pentagons(a, b)

}

case class DoublePentagons(_1: BoxedNonRelationalNumericalDomain[DoubleInterval], _2: UpperBound)
  extends SemanticCartesianProductDomain[BoxedNonRelationalNumericalDomain[DoubleInterval], UpperBound, DoublePentagons]
  with RelationalNumericalDomain[DoublePentagons]
{

  override def getStringOfId(id: Identifier) =
    this._1.getStringOfId(id) + ", " + this._2.getStringOfId(id)

  override def toString = ids map { x:Identifier => x.toString + " -> " + getStringOfId(x) } mkString "\n"

  override def factory(a: BoxedNonRelationalNumericalDomain[DoubleInterval], b: UpperBound) = DoublePentagons(a, b)

}