package ch.ethz.inf.pm.sample.abstractdomain.vdha

import apron.Abstract1
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{Type, DummyProgramPoint}
import ch.ethz.inf.pm.sample.oorepresentation.sil.AbstractType
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.util.Predef._

case class PredicateInstanceDomain(
    value: Set[Boolean] = Set.empty,
    isTop: Boolean = true,
    isBottom: Boolean = false)
  extends InverseSetDomain[Boolean, PredicateInstanceDomain]
  with Lattice.Must[PredicateInstanceDomain] {

  require(value.isEmpty implies (isTop && !isBottom))
  require(value.size == 1 implies (!isTop && !isBottom))
  require(value.size == 2 implies (!isTop && isBottom))

  def setFactory(value: Set[Boolean], isTop: Boolean, isBottom: Boolean) = {
    var newValue = value
    var newIsTop = isTop
    var newIsBottom = isBottom

    if (value.isEmpty && isBottom) {
      newValue = Set(true, false)
    } else if (!isBottom && !isTop) {
      newIsTop = value.size == 0
      newIsBottom = value.size == 2
    }

    PredicateInstanceDomain(value = newValue, isTop = newIsTop, isBottom = newIsBottom)
  }

  override def toString = {
    if (isTop || isBottom) super.toString
    else value.map(Map(true -> "folded", false -> "unfolded")).mkString(", ")
  }
}

case class PredicateInstancesDomain(
    map: Map[Identifier, PredicateInstanceDomain] = Map.empty,
    isTop: Boolean = true,
    override val isBottom: Boolean = false,
    defaultValue: PredicateInstanceDomain = PredicateInstanceDomain())
  extends BoxedDomain[PredicateInstanceDomain, PredicateInstancesDomain]
  with SemanticDomain[PredicateInstancesDomain] {

  def get(key: Identifier) = map.getOrElse(key, defaultValue)

  def functionalFactory(
      map: Map[Identifier, PredicateInstanceDomain],
      isBottom: Boolean,
      isTop: Boolean) = {
    var newIsBottom = isBottom
    if (map.exists(_._2 == defaultValue.bottom())) {
      newIsBottom = true
    }
    PredicateInstancesDomain(map, isBottom = newIsBottom, isTop = isTop)
  }

  private def isFolded(id: Identifier): Boolean =
    get(id).value.contains(true)

  private def isUnfolded(id: Identifier): Boolean =
    get(id).value.contains(false)

  def foldedPredInstIds: Set[VariableIdentifier] = {
    map.keySet.collect({
      // Only consider target edge-local identifiers
      case id @ EdgeLocalIdentifier(field :: Nil, predInstId)
        if isFolded(id) => predInstId.asInstanceOf[VariableIdentifier]
    })
  }

  def unfoldedPredInstIds: Set[VariableIdentifier] = {
    map.keySet.collect({
      // Only consider target edge-local identifiers
      case id @ EdgeLocalIdentifier(field :: Nil, predInstId)
        if isUnfolded(id) => predInstId.asInstanceOf[VariableIdentifier]
    })
  }

  def createVariable(variable: Identifier, typ: Type) =
    add(variable, defaultValue.top())

  def setToTop(variable: Identifier) =
    add(variable, defaultValue.top())

  def assign(variable: Identifier, expr: Expression) = expr match {
    case (expr: Constant) => add(variable, defaultValue.setFactory(Set(constantToBool(expr))))
  }

  private def constantToBool(c: Constant): Boolean = c match {
    case Constant("true", _, _) => true
    case Constant("false", _, _) => false
  }

  def removeVariable(variable: Identifier) = remove(variable)

  def assume(expr: Expression) = ???

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = ???

  def setArgument(variable: Identifier, expr: Expression) = ???

  def backwardAssign(oldPreState: PredicateInstancesDomain, variable: Identifier, expr: Expression) = ???

  def backwardAccess(field: Identifier) = ???

  def access(field: Identifier) = ???
}

object PredicateInstancesDomain {
  val Folded = Constant("true", PredicateInstanceType, DummyProgramPoint)
  val Unfolded = Constant("false", PredicateInstanceType, DummyProgramPoint)
}

case object PredicateInstanceType extends AbstractType("PredInstance") {
  override def isBooleanType = true
  def isNumericalType = true
}
