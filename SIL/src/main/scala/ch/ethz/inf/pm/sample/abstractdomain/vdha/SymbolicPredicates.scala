package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.vdha.ValueDrivenHeapStateWithSymbolicPredicates._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, Type}
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import apron.{Manager, Abstract1, Box}
import ch.ethz.inf.pm.sample.oorepresentation.sil.{SilCompiler, IntType}
import ch.ethz.inf.pm.sample.SystemParameters

case class ValueDrivenHeapStateWithSymbolicPredicates[S <: SemanticDomain[S]](
    abstractHeap: HeapGraph[EdgeStateDomain[S]],
    generalValState: EdgeStateDomain[S],
    expr: ExpressionSet,
    isTop: Boolean = false)
  extends PreciseValueDrivenHeapState[
    SemanticAndSymbolicPredicateDomain[S],
    ValueDrivenHeapStateWithSymbolicPredicates[S]] {

  import Utilities._
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
      val id = SymbolicPredicateDef.makeId(Some(variable.getName))
      refType.fields += id
    }

    val state = super.createVariableForArgument(variable, typ)

    val newGeneralValState = mapSymbolicPredicateDefs(state.generalValState,
      symbolicPredicateDefs => {
        var result = symbolicPredicateDefs
        state.abstractHeap.localVarVertices.map(localVarVertex => {
          val variableName = localVarVertex.name
          val id = SymbolicPredicateDef.makeId(Some(variableName))
          var symbolicPredicateDef = SymbolicPredicateDef().top()

          // Experimental:
          // Constrain the initial state manually by making the predicate
          // definition recursive over all reference fields
          /* for (objectField <- typ.objectFields) {
            symbolicPredicateDef = symbolicPredicateDef.addRefFieldPerm(objectField.getName, id.getName)
          } */

          result = result.add(id.getName, symbolicPredicateDef)
        })
        result
      })

    val newAbstractHeap = state.abstractHeap.copy(edges = {
      state.abstractHeap.edges.map(edge => {
        var newEdge = edge.copy(state = glbPreserveIds(edge.state, newGeneralValState))

        if (edge.source.isInstanceOf[LocalVariableVertex]) {
          val variableName = edge.source.name
          val symPredId = SymbolicPredicateDef.makeId(Some(variableName))
          val edgeLocalId = EdgeLocalIdentifier(List.empty, symPredId)
          newEdge = newEdge.createTargetEdgeLocalId(symPredId)
          newEdge = newEdge.copy(state = newEdge.state.assign(edgeLocalId, Folded))
        }
        newEdge
      })
    })

    state.copy(abstractHeap = newAbstractHeap, generalValState = newGeneralValState)
  }

  override def getFieldValue(obj: Expression, field: String, typ: Type) = {
    val originalResult = super.getFieldValue(obj, field, typ)
    var result = originalResult

    /* val apObj = obj.asInstanceOf[AccessPathIdentifier]
    val receiver = AccessPathIdentifier(apObj.path.dropRight(1))(typ.top(), DummyProgramPoint)

    // Experimental:
    // Make it possible to analyze the list traversal example
    if (obj.getType.isObject) {
      val curIdAp = AccessPathIdentifier(receiver.path ++ List("pred-list"))(IntType, DummyProgramPoint)
      val curNextIdAp = AccessPathIdentifier(apObj.path ++ List("pred-list"))(IntType, DummyProgramPoint)
      result = result.assignField(curIdAp, "pred-list", Unfolded)
      result = result.assignField(curNextIdAp, "pred-list", Folded)
    } */

    // We've lost the expression due to the assignField calls
    result.setExpression(originalResult.getExpression)
  }
}

object ValueDrivenHeapStateWithSymbolicPredicates {
  type ValFieldPermDomain = InverseSetDomain.Must[String]
  val ValFieldPermDomain = new ValFieldPermDomain().top()

  type RefFieldPermDomain =
    FunctionalDomain.Default[String, InverseSetDomain.Must[String]]
  val RefFieldPermDomain = new RefFieldPermDomain(
    defaultValue = InverseSetDomain.Must[String]().top()).top()

  type SymbolicPredicateDomain = SemanticCartesianProductDomain.Default[
    SymbolicPredicateInstsDomain, SymbolicPredicateDefsDomain]
  val SymbolicPredicateDomain = new SymbolicPredicateDomain(
    SymbolicPredicateInstsDomain().top(), SymbolicPredicateDefsDomain().top()).top()

  type EdgeStateDomain[S <: SemanticDomain[S]] =
    PreciseValueDrivenHeapState.EdgeStateDomain[SemanticAndSymbolicPredicateDomain[S]]

  def makeTopEdgeState[S <: SemanticDomain[S]](s: S): EdgeStateDomain[S] = {
    PreciseValueDrivenHeapState.makeTopEdgeState(
      SemanticAndSymbolicPredicateDomain(s, SymbolicPredicateDomain).top())
  }

  def mapSymbolicPredicateDefs[S <: SemanticDomain[S]](
      state: EdgeStateDomain[S],
      f: SymbolicPredicateDefsDomain => SymbolicPredicateDefsDomain): EdgeStateDomain[S] = {
    state.copy(
      valueState = {
        val semanticAndSymbolicPredicateState = state.valueState
        semanticAndSymbolicPredicateState.copy(
          _2 = {
            val symbolicPredicateState = semanticAndSymbolicPredicateState._2
            symbolicPredicateState.copy(_2 = f(symbolicPredicateState._2))
          })
      })
  }

  def mapSymbolicPredicateInsts[S <: SemanticDomain[S]](
       state: EdgeStateDomain[S],
       f: SymbolicPredicateInstsDomain => SymbolicPredicateInstsDomain): EdgeStateDomain[S] = {
    state.copy(
      valueState = {
        val semanticAndSymbolicPredicateState = state.valueState
        semanticAndSymbolicPredicateState.copy(
          _2 = {
            val symbolicPredicateState = semanticAndSymbolicPredicateState._2
            symbolicPredicateState.copy(_1 = f(symbolicPredicateState._1))
          })
      })
  }
}

case class SymbolicPredicateDef(
    valFieldPerms: ValFieldPermDomain = ValFieldPermDomain.top(),
    refFieldPerms: RefFieldPermDomain = RefFieldPermDomain.top())
  extends CartesianProductDomain[
    ValFieldPermDomain,
    RefFieldPermDomain,
    SymbolicPredicateDef] {

  def factory(a: ValFieldPermDomain, b: RefFieldPermDomain) =
    SymbolicPredicateDef(a, b)

  def _1: ValFieldPermDomain = valFieldPerms

  def _2: RefFieldPermDomain = refFieldPerms

  def addValFieldPerm(field: String): SymbolicPredicateDef =
    copy(valFieldPerms = valFieldPerms.add(field))

  def addRefFieldPerm(field: String, symbolicPredicateId: String) = {
    // TODO: What should happen if there is already an ID?
    val newSymbolicPredicateIds = refFieldPerms.get(field).add(symbolicPredicateId)
    copy(refFieldPerms = refFieldPerms.add(field, newSymbolicPredicateIds))
  }
}

object SymbolicPredicateDef {
  private val nextId = new ThreadLocal[Int]

  def makeId(variableOption: Option[String]): Identifier = {
    val name = variableOption match {
      case Some(variable) => s"pred-$variable"
      case None =>
        val id = nextId.get
        nextId.set(id + 1)
        s"pred-$id"
    }
    VariableIdentifier(name)(IntType, DummyProgramPoint)
  }
}

/**
 * Cartesian product supporting the operations of the semantic domain.
 *
 * Depending on the type or name of identifiers it receives, applies operations
 * to only one or both of the domains.
 *
 * @tparam T1 The type of the first domain
 * @tparam T2 The type of the second domain
 * @tparam T The type of the current domain
 */
trait RoutingSemanticCartesianProductDomain[
    T1 <: SemanticDomain[T1],
    T2 <: SemanticDomain[T2],
    T <: RoutingSemanticCartesianProductDomain[T1, T2, T]]
  extends CartesianProductDomain[T1, T2, T] with SemanticDomain[T] { this: T =>

  def isHandledByFirstDomain(id: Identifier): Boolean

  def isHandledBySecondDomain(id: Identifier): Boolean

  def getIds() = _1.getIds() ++ _2.getIds()

  def factory(ids: Set[Identifier], firstOp: () => T1, secondOp: () => T2): T = {
    val newFirst = if (ids.forall(isHandledByFirstDomain)) firstOp() else _1
    val newSecond = if (ids.forall(isHandledBySecondDomain)) secondOp() else _2
    factory(newFirst, newSecond)
  }

  def setToTop(variable: Identifier): T =
    factory(Set(variable), () => _1.setToTop(variable), () => _2.setToTop(variable))

  def assign(variable: Identifier, expr: Expression): T =
    factory(Set(variable) ++ expr.getIdentifiers, () => _1.assign(variable, expr), () => _2.assign(variable, expr))

  def setArgument(variable: Identifier, expr: Expression): T =
    factory(Set(variable) ++ expr.getIdentifiers, () => _1.setArgument(variable, expr), () => _2.setArgument(variable, expr))

  def assume(expr: Expression): T =
    factory(expr.getIdentifiers, () => _1.assume(expr), () => _2.assume(expr))

  def merge(r: Replacement): T = {
    def filter(r: Replacement, f: Identifier => Boolean): Replacement = {
      val result = new Replacement(
        isPureExpanding = r.isPureExpanding,
        isPureRemoving = r.isPureRemoving,
        isPureRenaming = r.isPureRenaming)
      r.value.map {
        case (from, to) =>
          if (from.forall(f) && to.forall(f))
            result.value += from -> to
      }
      result
    }

    val firstReplacement = filter(r, isHandledByFirstDomain)
    val secondReplacemnt = filter(r, isHandledBySecondDomain)
    factory(_1.merge(firstReplacement), _2.merge(secondReplacemnt))
  }

  def createVariable(variable: Identifier, typ: Type): T =
    factory(Set(variable), () => _1.createVariable(variable, typ), () => _2.createVariable(variable, typ))

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    val (a1, b1) = if (isHandledByFirstDomain(variable)) _1.createVariableForArgument(variable, typ, path) else (_1, Map.empty[Identifier, List[String]])
    val (a2, b2) = if (isHandledBySecondDomain(variable)) _2.createVariableForArgument(variable, typ, path) else (_2, Map.empty[Identifier, List[String]])
    (factory(a1, a2), b1 ++ b2)
  }

  def removeVariable(variable: Identifier): T =
    factory(Set(variable), () => _1.removeVariable(variable), () => _2.removeVariable(variable))

  def access(field: Identifier): T =
    factory(Set(field), () => _1.access(field), () => _2.access(field))

  def backwardAccess(field: Identifier): T =
    factory(Set(field), () => _1.backwardAccess(field), () => _2.backwardAccess(field))

  def backwardAssign(oldPreState: T, variable: Identifier, expr: Expression): T =
    factory(
      Set(variable) ++ expr.getIdentifiers,
      () => _1.backwardAssign(oldPreState._1, variable, expr),
      () => _2.backwardAssign(oldPreState._2, variable, expr))

  def getStringOfId(id: Identifier): String = {
    if (isHandledByFirstDomain(id) && isHandledBySecondDomain(id))
      "(" + _1.getStringOfId(id) + ", " + _2.getStringOfId(id) + ")"
    else if (isHandledByFirstDomain(id))
      _1.getStringOfId(id)
    else if (isHandledBySecondDomain(id))
      _2.getStringOfId(id)
    else
      ""
  }
}

case class SemanticAndSymbolicPredicateDomain[S <: SemanticDomain[S]](
    _1: S, _2: SymbolicPredicateDomain)
  extends RoutingSemanticCartesianProductDomain[
    S, SymbolicPredicateDomain, SemanticAndSymbolicPredicateDomain[S]] {

  def isHandledByFirstDomain(id: Identifier) =
    !isHandledBySecondDomain(id)

  def isHandledBySecondDomain(id: Identifier) =
    id.getName.contains("pred")

  def factory(_1: S, _2: SymbolicPredicateDomain) =
    SemanticAndSymbolicPredicateDomain(_1, _2)
}

case class SymbolicPredicateDefsDomain(
    map: Map[String, SymbolicPredicateDef] = Map.empty[String, SymbolicPredicateDef],
    isTop: Boolean = false,
    isBottom: Boolean = false,
    defaultValue: SymbolicPredicateDef = SymbolicPredicateDef())
  extends FunctionalDomain[String, SymbolicPredicateDef, SymbolicPredicateDefsDomain]
  with DummySemanticDomain[SymbolicPredicateDefsDomain] {

  def get(key: String): SymbolicPredicateDef = map.getOrElse(key, defaultValue)

  def functionalFactory(value: Map[String, SymbolicPredicateDef], isBottom: Boolean, isTop: Boolean) =
    SymbolicPredicateDefsDomain(value, isTop, isBottom, defaultValue)
}

case class SymbolicPredicateInstsDomain(
    state: Option[Abstract1] = None,
    domain: Manager = new Box(),
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
  val Folded = Constant("1", IntType, DummyProgramPoint)
  val Unfolded = Constant("0", IntType, DummyProgramPoint)
}