package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, Type}
import ch.ethz.inf.pm.sample.oorepresentation.sil.{Constants, PredType}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.util.Predef._
import com.weiglewilczek.slf4s.Logging

case class PredicateInstancesDomain(
    map: Map[Identifier, PredicateInstanceDomain] = Map.empty,
    isTop: Boolean = true,
    override val isBottom: Boolean = false,
    defaultValue: PredicateInstanceDomain = PredicateInstanceDomain())
  extends BoxedDomain[PredicateInstanceDomain, PredicateInstancesDomain]
  with SemanticDomain[PredicateInstancesDomain]
  with Logging {

  import PredicateInstanceState.{Folded, Unfolded}

  require(map.keySet.forall(canHandle), "cannot handle all ids")

  def canHandle(id: Identifier): Boolean = id match {
    case EdgeLocalIdentifier(_, predInstId: PredicateInstanceIdentifier) => true
    case ValueHeapIdentifier(_, predInstId: PredicateInstanceIdentifier) => true
    case AccessPathIdentifier(path) if path.last.isInstanceOf[PredicateInstanceIdentifier] => true
    case _ => false
  }

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

  /** Returns the IDs of all predicates instance for which we have an instance
    * in the given state.
    */
  def instIds(instState: PredicateInstanceState): Set[PredicateInstanceIdentifier] =
    map.keySet.collect({
      // Only consider target edge-local identifiers
      case id @ EdgeLocalIdentifier(field :: Nil, predId)
        if get(id).value.contains(instState) => predId.asInstanceOf[PredicateInstanceIdentifier]
    })

  /** The predicate instance IDs of all folded predicate instances. */
  def foldedInstIds: Set[PredicateInstanceIdentifier] = instIds(Folded)

  /** The predicate instance IDs of all unfolded predicate instances. */
  def unfoldedInstIds: Set[PredicateInstanceIdentifier] = instIds(Unfolded)

  /** The predicate instance IDs of all folded and unfolded predicate instances. */
  def foldedAndUnfoldedInstIds: Set[PredicateInstanceIdentifier] = foldedInstIds ++ unfoldedInstIds

  def ids(instState: PredicateInstanceState): Set[PredicateIdentifier] =
    instIds(instState).map(_.predId)

  def foldedIds: Set[PredicateIdentifier] = foldedInstIds.map(_.predId)

  def unfoldedIds: Set[PredicateIdentifier] = unfoldedInstIds.map(_.predId)

  def foldedAndUnfoldedIds: Set[PredicateIdentifier] = foldedAndUnfoldedInstIds.map(_.predId)

  def createVariable(variable: Identifier, typ: Type) = {
    add(variable, defaultValue.top())
  }

  def setToTop(variable: Identifier) =
    add(variable, defaultValue.top())

  def assign(variable: Identifier, expr: Expression) = expr match {
    // Hack to make it possible to set the variable to top
    // using an assignment
    case PredicateInstanceState.Top =>
      add(variable, get(variable).top())
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

  def assume(expr: Expression) = expr match {
    case BinaryArithmeticExpression(id: Identifier,
      state: PredicateInstanceState, ArithmeticOperator.==, _) =>
      add(id, get(id).add(state))
    case _ =>
      logger.debug(s"Assuming expression $expr in predicate instances domain " +
        "is not supported")
      this
  }

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

class PredicateInstanceIdentifier(
    val predId: PredicateIdentifier,
    val version: Int)
  extends VariableIdentifier(name = s"$predId#$version")(PredType) {

}

object PredicateInstanceIdentifier {
  private val nextVersion = new ThreadLocal[Int]

  def resetVersion() = {
    nextVersion.set(0)
  }

  def makeVersion(): Int = {
    val version = nextVersion.get
    nextVersion.set(version + 1)
    version
  }

  def make(predId: PredicateIdentifier): PredicateInstanceIdentifier =
    new PredicateInstanceIdentifier(predId, makeVersion())
}

final case class PredicateInstanceState(name: String) extends Expression {
  def transform(f: (Expression) => Expression) = this
  def ids = Set.empty
  def pp = DummyProgramPoint
  def typ = PredType

  override def toString = name
}

object PredicateInstanceState {
  val Folded = PredicateInstanceState("folded")
  val Unfolded = PredicateInstanceState("unfolded")

  // Dummy element that makes it possible to use assignField
  // to set a ghost variable to top
  val Top = PredicateInstanceState("top")
}