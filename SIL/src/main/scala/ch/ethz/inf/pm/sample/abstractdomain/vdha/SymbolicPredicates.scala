package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.vdha.ValueDrivenHeapStateWithSymbolicPredicates._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, Type}
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import apron.{Polka, Manager, Abstract1}
import ch.ethz.inf.pm.sample.oorepresentation.sil.{AbstractType, SilCompiler}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.vdha.SymbolicPredicateDef.{RefFieldPermDomain, ValFieldPermDomain}

case class ValueDrivenHeapStateWithSymbolicPredicates[S <: SemanticDomain[S]](
    abstractHeap: HeapGraph[EdgeStateDomain[S]],
    generalValState: EdgeStateDomain[S],
    expr: ExpressionSet,
    isTop: Boolean = false)
  extends PreciseValueDrivenHeapState[
    SemanticAndSymbolicPredicateDomain[S],
    ValueDrivenHeapStateWithSymbolicPredicates[S]] {

  // Shorthand for the self-type
  type T = ValueDrivenHeapStateWithSymbolicPredicates[S]

  import ValueDrivenHeapStateWithSymbolicPredicates._
  import SymbolicPredicateInstsDomain._
  
  def factory(
      abstractHeap: HeapGraph[EdgeStateDomain[S]],
      generalValState: EdgeStateDomain[S],
      expr: ExpressionSet,
      isTop: Boolean) =
    ValueDrivenHeapStateWithSymbolicPredicates(abstractHeap, generalValState, expr, isTop)

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type) = {
    if (variable.typ.isObject) {
      val refType = SystemParameters.compiler.asInstanceOf[SilCompiler].refType
      val id = SymbolicPredicateDef.makeInstId(SymbolicPredicateDef.makeId(Some(variable.getName)))
      refType.fields += id
    }

    var state = super.createVariableForArgument(variable, typ)

    state.abstractHeap.localVarVertices.foreach(localVarVertex => {
      val variableName = localVarVertex.name
      val id = SymbolicPredicateDef.makeId(Some(variableName))
      state = state.createVariable(id, SymbolicPredicateDefType, DummyProgramPoint)
    })

    val newAbstractHeap = state.abstractHeap.copy(edges = {
      state.abstractHeap.edges.map(edge => {
        var newEdge = edge

        if (edge.source.isInstanceOf[LocalVariableVertex]) {
          val variableName = edge.source.name
          val symPredDefId = SymbolicPredicateDef.makeId(Some(variableName))
          val symPredInstId = SymbolicPredicateDef.makeInstId(symPredDefId)
          val edgeLocalId = EdgeLocalIdentifier(List.empty, symPredInstId)
          newEdge = newEdge.createTargetEdgeLocalId(symPredInstId)
          newEdge = newEdge.copy(state = newEdge.state.assign(edgeLocalId, Folded))
        }
        newEdge
      })
    })

    state.copy(abstractHeap = newAbstractHeap)
  }
}

object ValueDrivenHeapStateWithSymbolicPredicates {
  type EdgeStateDomain[S <: SemanticDomain[S]] =
    PreciseValueDrivenHeapState.EdgeStateDomain[SemanticAndSymbolicPredicateDomain[S]]

  def makeTopEdgeState[S <: SemanticDomain[S]](s: S): EdgeStateDomain[S] = {
    PreciseValueDrivenHeapState.makeTopEdgeState(
      SemanticAndSymbolicPredicateDomain(s, SymbolicPredicateDomain()).top())
  }
}

case class SymbolicPredicateDomain(
    instances: SymbolicPredicateInstsDomain = SymbolicPredicateInstsDomain(),
    definitions: SymbolicPredicateDefsDomain = SymbolicPredicateDefsDomain())
  extends RoutingSemanticCartesianProductDomain[
    SymbolicPredicateInstsDomain,
    SymbolicPredicateDefsDomain,
    SymbolicPredicateDomain] {

  def factory(
      instances: SymbolicPredicateInstsDomain,
      definitions: SymbolicPredicateDefsDomain) =
    SymbolicPredicateDomain(instances, definitions)

  def _1 = instances

  def _1canHandle(id: Identifier) =
    id.typ == SymbolicPredicateInstType

  def _2 = definitions

  def _2canHandle(id: Identifier) =
    id.typ == SymbolicPredicateDefType
}

case class SymbolicPredicateDef(
    valFieldPerms: ValFieldPermDomain = ValFieldPermDomain.top(),
    refFieldPerms: RefFieldPermDomain = RefFieldPermDomain.top())
  extends CartesianProductDomain[
    ValFieldPermDomain,
    RefFieldPermDomain,
    SymbolicPredicateDef]
  with Expression {

  def factory(a: ValFieldPermDomain, b: RefFieldPermDomain) =
    SymbolicPredicateDef(a, b)

  def _1: ValFieldPermDomain = valFieldPerms

  def _2: RefFieldPermDomain = refFieldPerms

  def addValFieldPerm(field: String): SymbolicPredicateDef =
    copy(valFieldPerms = valFieldPerms.add(field))

  def addRefFieldPerm(field: String, symbolicPredicateId: Identifier) = {
    // TODO: What should happen if there is already an ID?
    val newSymbolicPredicateIds = refFieldPerms.get(field).add(symbolicPredicateId)
    copy(refFieldPerms = refFieldPerms.add(field, newSymbolicPredicateIds))
  }

  def transform(f: (Expression) => Expression): Expression = ???

  def getIdentifiers = refFieldPerms.map.values.flatMap(_.value).toSet

  def pp = DummyProgramPoint

  def getType = SymbolicPredicateDefType
}

object SymbolicPredicateDef {
  type ValFieldPermDomain = InverseSetDomain.Must[String]
  val ValFieldPermDomain = new ValFieldPermDomain().top()

  type RefFieldPermDomain =
  FunctionalDomain.Default[String, InverseSetDomain.Must[Identifier]]
  val RefFieldPermDomain = new RefFieldPermDomain(
    defaultValue = InverseSetDomain.Must[Identifier]().top()).top()

  private val nextId = new ThreadLocal[Int]

  def makeId(variableOption: Option[String]): VariableIdentifier = {
    val name = variableOption match {
      case Some(variable) => variable
      case None =>
        val id = nextId.get
        nextId.set(id + 1)
        id.toString
    }
    VariableIdentifier("pred-" + name)(SymbolicPredicateDefType, DummyProgramPoint)
  }

  def makeInstId(defId: VariableIdentifier): VariableIdentifier =
    VariableIdentifier(defId.name)(SymbolicPredicateInstType, DummyProgramPoint)
}

case object SymbolicPredicateDefType extends AbstractType("Pred") {
  def isNumericalType = true
}

case object SymbolicPredicateInstType extends AbstractType("PredInstance") {
  override def isBooleanType = true
  def isNumericalType = true
}

case class SemanticAndSymbolicPredicateDomain[S <: SemanticDomain[S]](
    valueState: S, symbolicPredicateState: SymbolicPredicateDomain)
  extends RoutingSemanticCartesianProductDomain[
    S, SymbolicPredicateDomain, SemanticAndSymbolicPredicateDomain[S]] {

  def factory(valueState: S, symbolicPredicateState: SymbolicPredicateDomain) =
    SemanticAndSymbolicPredicateDomain(valueState, symbolicPredicateState)

  def _1 = valueState

  def _1canHandle(id: Identifier) =
    !_2canHandle(id)

  def _2 = symbolicPredicateState

  def _2canHandle(id: Identifier) =
    id.typ == SymbolicPredicateDefType || id.typ == SymbolicPredicateInstType
}

case class SymbolicPredicateDefsDomain(
    map: Map[Identifier, SymbolicPredicateDef] = Map.empty[Identifier, SymbolicPredicateDef],
    isTop: Boolean = false,
    isBottom: Boolean = false,
    defaultValue: SymbolicPredicateDef = SymbolicPredicateDef().top())
  extends BoxedDomain[SymbolicPredicateDef, SymbolicPredicateDefsDomain]
  with SemanticDomain[SymbolicPredicateDefsDomain] {

  def get(key: Identifier): SymbolicPredicateDef = map.getOrElse(key, defaultValue)

  def functionalFactory(
      value: Map[Identifier, SymbolicPredicateDef],
      isBottom: Boolean,
      isTop: Boolean) =
    SymbolicPredicateDefsDomain(value, isTop, isBottom, defaultValue)

  def removeVariable(variable: Identifier) = remove(variable)

  def createVariable(variable: Identifier, typ: Type) =
    add(variable, defaultValue.top())

  def setToTop(variable: Identifier) =
    add(variable, defaultValue.top())

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = ???

  def access(field: Identifier) = ???

  def assume(expr: Expression) = this

  def setArgument(variable: Identifier, expr: Expression) = ???

  def assign(variable: Identifier, expr: Expression) = expr match {
    case (expr: SymbolicPredicateDef) => add(variable, expr)
  }

  def backwardAssign(oldPreState: SymbolicPredicateDefsDomain, variable: Identifier, expr: Expression) = ???

  def backwardAccess(field: Identifier) = ???
}

case class SymbolicPredicateInstsDomain(
    state: Option[Abstract1] = None,
    domain: Manager = new Polka(false),
    isPureBottom: Boolean = false,
    env: Set[Identifier] = Set.empty)
  extends ApronInterface[SymbolicPredicateInstsDomain] {

  override def glb(other: SymbolicPredicateInstsDomain) = lub(other)

  def factory(
      state: Option[Abstract1],
      domain: Manager,
      isPureBottom: Boolean = false,
      env: Set[Identifier]) =
    SymbolicPredicateInstsDomain(state, domain, isPureBottom, env)
}

object SymbolicPredicateInstsDomain {
  val Folded = Constant("1", SymbolicPredicateInstType, DummyProgramPoint)
  val Unfolded = Constant("0", SymbolicPredicateInstType, DummyProgramPoint)
}