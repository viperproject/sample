package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, Type}
import ch.ethz.inf.pm.sample.oorepresentation.sil.BoolType
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.util.Predef._

case class PredicateInstancesDomain(
    map: Map[Identifier, PredicateInstanceDomain] = Map.empty,
    isTop: Boolean = true,
    override val isBottom: Boolean = false,
    defaultValue: PredicateInstanceDomain = PredicateInstanceDomain())
  extends BoxedDomain[PredicateInstanceDomain, PredicateInstancesDomain]
  with SemanticDomain[PredicateInstancesDomain] {

  import PredicateInstanceState.{Folded, Unfolded}

  def get(key: Identifier) = map.getOrElse(key, defaultValue)

  override def glb(other: PredicateInstancesDomain) = lub(other)

  def functionalFactory(
      map: Map[Identifier, PredicateInstanceDomain],
      isBottom: Boolean,
      isTop: Boolean) = {
    var newIsTop = isTop
    var newIsBottom = isBottom
    if (map.exists(_._2 == defaultValue.bottom())) {
      newIsBottom = true
    }
    // Problem: For GLB-preserving joins, we set the default value
    // of the partners temporarily to bottom to prevent losing permission
    // However, isTop may still be true, in which case glb(...) would take
    // a shortcut that would cause us to lose the permission.
    // TODO: This is extremely fragile and complicated
    if (defaultValue.isBottom) {
      newIsTop = false
    }
    PredicateInstancesDomain(map, isBottom = newIsBottom, isTop = newIsTop, defaultValue = defaultValue)
  }

  /** Returns the IDs of all predicates for which we have an instance
    * in the given state.
    */
  def ids(instState: PredicateInstanceState): Set[Identifier] =
    map.keySet.collect({
      // Only consider target edge-local identifiers
      case id @ EdgeLocalIdentifier(field :: Nil, predId)
        if get(id).value.contains(instState) => predId
    })

  /** The predicate IDs of all folded predicate instances. */
  def foldedIds: Set[Identifier] = ids(Folded)

  /** The predicate IDs of all unfolded predicate instances. */
  def unfoldedIds: Set[Identifier] = ids(Unfolded)

  /** The predicate IDs of all folded and unfolded predicate instances. */
  def foldedAndUnfoldedIds: Set[Identifier] = foldedIds ++ unfoldedIds

  def createVariable(variable: Identifier, typ: Type) = {
    add(variable, defaultValue.top())
  }

  def setToTop(variable: Identifier) =
    add(variable, defaultValue.top())

  def assign(variable: Identifier, expr: Expression) = expr match {
    case (expr: PredicateInstanceState) =>
      add(variable, defaultValue.add(expr))
  }

  override def merge(r: Replacement): PredicateInstancesDomain = {
    if (r.isEmpty()) return this
    var result = this
    val removedVariables = r.keySet().flatten

    // We remove the variables from the result state
    for (v <- removedVariables)
      result = result.remove(v)

    for (s <- r.keySet()) {
      val toSet = r.apply(s)
      assert(toSet.size == 1)

      val to = toSet.head

      // Do not use Lattice.bigLub here.
      // When we have the replacement {p0, p1} -> {p0}
      // and the current state is {p0 -> folded, p1 -> unfolded}
      // then the result should be {p0 -> bottom}, not {p0 -> top}.
      val newStates = s.map(this.get).flatMap(_.value)

      result = result.add(to, PredicateInstanceDomain().setFactory(newStates))
    }

    result
  }

  def removeVariable(variable: Identifier) = remove(variable)

  def assume(expr: Expression) = ???
  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = ???
  def setArgument(variable: Identifier, expr: Expression) = ???
  def backwardAssign(oldPreState: PredicateInstancesDomain, variable: Identifier, expr: Expression) = ???
  def backwardAccess(field: Identifier) = ???
  def access(field: Identifier) = ???
}

case class PredicateInstanceDomain(
    value: Set[PredicateInstanceState] = Set.empty,
    isTop: Boolean = true,
    isBottom: Boolean = false)
  extends InverseSetDomain[PredicateInstanceState, PredicateInstanceDomain] {

  import PredicateInstanceState.{Folded, Unfolded}

  require(value.isEmpty implies (isTop && !isBottom))
  require(value.size == 1 implies (!isTop && !isBottom))
  require(value.size == 2 implies (!isTop && isBottom))

  override def glb(other: PredicateInstanceDomain) = lub(other)

  def setFactory(
      value: Set[PredicateInstanceState],
      isTop: Boolean,
      isBottom: Boolean) = {
    var newValue = value
    var newIsTop = isTop
    var newIsBottom = isBottom

    if (value.isEmpty && isBottom) {
      newValue = Set(Folded, Unfolded)
    } else if (!isBottom && !isTop) {
      newIsTop = value.size == 0
      newIsBottom = value.size == 2
    }

    PredicateInstanceDomain(value = newValue, isTop = newIsTop, isBottom = newIsBottom)
  }

  override def toString = {
    // Do not put curly braces around the set
    if (isTop || isBottom) super.toString
    else value.mkString(", ")
  }
}

final case class PredicateInstanceState(name: String) extends Expression {
  def transform(f: (Expression) => Expression) = this
  def ids = Set.empty
  def pp = DummyProgramPoint
  def typ = BoolType

  override def toString = name
}

object PredicateInstanceState {
  val Folded = PredicateInstanceState("folded")
  val Unfolded = PredicateInstanceState("unfolded")
}