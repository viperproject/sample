package ch.ethz.inf.pm.sample.abstractdomain.vdha

import apron.{Box, Manager, Abstract1}
import ch.ethz.inf.pm.sample.abstractdomain.{VariableIdentifier, Constant, BooleanDomain, Identifier}
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import ch.ethz.inf.pm.sample.oorepresentation.DummyProgramPoint
import ch.ethz.inf.pm.sample.oorepresentation.sil.AbstractType

case class PredicateInstancesDomain(
    state: Option[Abstract1] = None,
    domain: Manager = new Box(),
    isPureBottom: Boolean = false,
    env: Set[Identifier] = Set.empty)
  extends ApronInterface[PredicateInstancesDomain] {

  import PredicateInstancesDomain._

  // TODO: What about this???
  // override def glb(other: SymbolicPredicateInstsDomain) = lub(other)

  def factory(
      state: Option[Abstract1],
      domain: Manager,
      isPureBottom: Boolean = false,
      env: Set[Identifier]) =
    PredicateInstancesDomain(state, domain, isPureBottom, env)

  private def isFolded(id: Identifier): Boolean =
    areEqual(id, Folded) == BooleanDomain.domTrue

  private def isUnfolded(id: Identifier): Boolean =
    areEqual(id, Unfolded) == BooleanDomain.domTrue

  def foldedPredInstIds: Set[VariableIdentifier] = {
    env.collect({
      // Only consider target edge-local identifiers
      case id @ EdgeLocalIdentifier(field :: Nil, predInstId)
        if isFolded(id) => predInstId.asInstanceOf[VariableIdentifier]
    })
  }

  def unfoldedPredInstIds: Set[VariableIdentifier] = {
    env.collect({
      // Only consider target edge-local identifiers
      case id @ EdgeLocalIdentifier(field :: Nil, predInstId)
        if isUnfolded(id) => predInstId.asInstanceOf[VariableIdentifier]
    })
  }
}

object PredicateInstancesDomain {
  val Folded = Constant("true", PredicateInstanceType, DummyProgramPoint)
  val Unfolded = Constant("false", PredicateInstanceType, DummyProgramPoint)
}

case object PredicateInstanceType extends AbstractType("PredInstance") {
  override def isBooleanType = true
  def isNumericalType = true
}
