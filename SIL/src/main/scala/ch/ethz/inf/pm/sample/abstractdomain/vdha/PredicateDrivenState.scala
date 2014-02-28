package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.sil.{BoolType, SilCompiler}
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDrivenHeapState.EdgeStateDomain

case class PredicateDrivenHeapState[S <: SemanticDomain[S]](
    abstractHeap: HeapGraph[EdgeStateDomain[S]],
    generalValState: EdgeStateDomain[S],
    expr: ExpressionSet,
    isTop: Boolean = false)
  extends PreciseValueDrivenHeapState[
    SemanticAndPredicateDomain[S],
    PredicateDrivenHeapState[S]] {

  // Shorthand for the self-type
  type T = PredicateDrivenHeapState[S]

  import PredicateDrivenHeapState._
  import PredicateInstancesDomain._
  import PredicateDefinition._

  def factory(
      abstractHeap: HeapGraph[EdgeStateDomain[S]],
      generalValState: EdgeStateDomain[S],
      expr: ExpressionSet,
      isTop: Boolean) =
    PredicateDrivenHeapState(abstractHeap, generalValState, expr, isTop)

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type) = {
    if (variable.typ.isObject) {
      var result = super.createVariableForArgument(variable, typ)

      val edgeVerticesToPredDefId = result.abstractHeap.localVarEdges.map(edge => {
        val predDefId = PredicateDefinition.makeId()
        Set(edge.source, edge.target) -> predDefId
      }).toMap

      val predDefIds = edgeVerticesToPredDefId.values
      result = result.createNonObjectVariables(predDefIds.toSet)

      result = CondHeapGraph[EdgeStateDomain[S], T](result).mapEdges(edge => {
        edgeVerticesToPredDefId.get(edge.vertices) match {
          case Some(defId) =>
            val predInstId = defId.toPredInstId
            val edgeLocalInstId = EdgeLocalIdentifier(List(edge.field), predInstId)
            edge.state.assign(edgeLocalInstId, Folded)
          case None => edge.state
        }
      })

      result.setExpression(ExpressionSet(variable))
    } else {
      super.createVariableForArgument(variable, typ)
    }
  }

  override def getFieldValue(id: AccessPathIdentifier) = {
    val originalResult = super.getFieldValue(id)
    val receiverPath = id.path.dropRight(1)
    val receiverId = AccessPathIdentifier(receiverPath)(refType)
    val field = id.path.last

    assert(receiverPath.size == 1, "currently only support obj.field")



    var result: T = CondHeapGraph[EdgeStateDomain[S], T](originalResult)
      .evalExp(receiverId).mapCondHeaps(condHeap => {
        val recvEdge = condHeap.takenPath(receiverId.path).edges.head

        val recvPredState = recvEdge.state.valueState.predicateState
        val recvPredDefs = recvPredState.definitions
        val recvPredInsts = recvPredState.instances

        val foldedPredInstEdgeLocalIds = recvPredInsts.targetEdgeLocalIds.filter(recvPredInsts.isFolded)
        val unfoldedPredInstEdgeLocalIds = recvPredInsts.targetEdgeLocalIds.filter(recvPredInsts.isUnfolded)
        val availPredInstEdgeLocalIds = foldedPredInstEdgeLocalIds ++ unfoldedPredInstEdgeLocalIds

        if (availPredInstEdgeLocalIds.isEmpty) {
          println("there needs to be either a folded or unfolded predicate")
          Seq(condHeap)
        } else {
          val availPredInstIds = availPredInstEdgeLocalIds.map(edgeLocalIdToPredInstId)
          val foldedPredInstIds = foldedPredInstEdgeLocalIds.map(edgeLocalIdToPredInstId)
          val unfoldedPredInstIds = unfoldedPredInstEdgeLocalIds.map(edgeLocalIdToPredInstId)

          /* assert(findPerm(availPredInstIds),
            "there may at least be one predicate instance with the permission") */

          def findPerm(predInstIds: Set[VariableIdentifier]): Option[VariableIdentifier] = {
            predInstIds.find(predInstId => {
              val predDef = recvPredDefs.get(predInstId.toPredDefId)
              if (id.typ.isObject)
                predDef.refFieldPerms.map.contains(field)
              else
                predDef.valFieldPerms.value.contains(field)
            })
          }

          findPerm(unfoldedPredInstIds) match {
            case Some(unfoldedPredicateInstId) =>
              Seq(condHeap) // Nothing to do
            case None => {
              var result = condHeap

              val (recvPredInstId, hasPerm) = findPerm(foldedPredInstIds) match {
                case Some(foldedPredInstId) => (foldedPredInstId, true)
                case None => (availPredInstIds.head, false)
              }
              val recvPredDefId = recvPredInstId.toPredDefId
              var recvPredDef = recvPredDefs.get(recvPredDefId)

              // Unfold
              result = result.mapEdges(e => {
                if (e.target == recvEdge.target) {
                  val edgeLocId = EdgeLocalIdentifier(List(e.field), recvPredInstId)
                  e.state.assign(edgeLocId, Unfolded)
                } else e.state
              })

              // Add permission if necessary
              if (!hasPerm) {
                recvPredDef = if (recvEdge.target == NullVertex)
                  recvPredDef.bottom()
                else if (id.typ.isObject) {
                  val nestedPredDefId = PredicateDefinition.makeId()
                  val nestedPredDef = PredicateDefinition().top()
                  result = result.map(_.createVariable(nestedPredDefId))
                  // Always assume that the predicate instance is recursive
                  // val nestedPredDefId = recvPredDefId
                  // val nestedPredDef = recvPredDef
                  result = result.map(_.assign(nestedPredDefId, nestedPredDef))
                  recvPredDef.addRefFieldPerm(field, nestedPredDefId)
                } else {
                  recvPredDef.addValFieldPerm(field)
                }

                // Assign the new predicate definition
                result = result.map(state => {
                  state.assign(recvPredDefId, recvPredDef)
                })
              }

              // Add folded nested predicate instances
              if (id.typ.isObject) {
                // TODO: Should add ALL nested predicate instances
                val nestedPredDefId = recvPredDef.refFieldPerms.get(id.path.last).value.head.asInstanceOf[VariableIdentifier]
                val nestedPredInstId = nestedPredDefId.toPredInstId

                result = result.mapEdges(e => {
                  if (e.source == recvEdge.target) {
                    val edgeLocId = EdgeLocalIdentifier(List(e.field), nestedPredInstId)
                    val equality = BinaryArithmeticExpression(edgeLocId, Folded, ArithmeticOperator.==, BoolType)
                    e.state.createVariable(edgeLocId).assume(equality)
                  } else e.state
                })
              }

              Seq(result)
            }
          }
        }
      }).join

    // We've lost the expression due to the assignField calls
    result = result.setExpression(originalResult.expr)
    result
  }

  override protected def createObject(typ: Type) = {
    var (result, newVertex) = super.createObject(typ)

    val predDefId = PredicateDefinition.makeId()
    val predInstId = predDefId.toPredInstId
    val predInstValueHeapId = ValueHeapIdentifier(newVertex, predInstId)

    result = result.createNonObjectVariables(Set(predDefId, predInstValueHeapId))
    result = CondHeapGraph[EdgeStateDomain[S], T](result).map(
      _.assign(predInstValueHeapId, Unfolded))

    (result, newVertex)
  }

  override def assignVariable(left: Expression, right: Expression) = {
    val result = super.assignVariable(left, right)

    (left, right) match {
      case (left: VariableIdentifier, right: VertexExpression) =>
        val source = abstractHeap.localVarVertex(left.getName)
        val predInstIds = result.predInstHeapIds.map(id => VariableIdentifier(id.field)(PredicateInstanceType))
        val addedEdge = result.abstractHeap.outEdges(source).head
        var newEdge = predInstIds.foldLeft(addedEdge)(_.createTargetEdgeLocalId(_))
        newEdge = newEdge.assumeEdgeLocalIdEqualities()
        result
          .copy(abstractHeap = result.abstractHeap.copy(
            edges = result.abstractHeap.edges - addedEdge + newEdge))
          .removePredInstHeapIds()
      case _ =>
        result.removePredInstHeapIds()
    }
  }

  override def assignField(left: AccessPathIdentifier, right: Expression) = {
    super.assignField(left, right).removePredInstHeapIds()
  }

  def predInstHeapIds: Set[ValueHeapIdentifier] =
    generalValState.valueHeapIds.filter(_.typ == PredicateInstanceType)

  def removePredInstHeapIds(): PredicateDrivenHeapState[S] = {
    copy(
      generalValState = generalValState.removeVariables(predInstHeapIds),
      abstractHeap = abstractHeap.mapEdgeStates(_.removeVariables(predInstHeapIds))
    )
  }
}

object PredicateDrivenHeapState {
  type EdgeStateDomain[S <: SemanticDomain[S]] =
  PreciseValueDrivenHeapState.EdgeStateDomain[SemanticAndPredicateDomain[S]]

  def makeTopEdgeState[S <: SemanticDomain[S]](s: S): EdgeStateDomain[S] = {
    PreciseValueDrivenHeapState.makeTopEdgeState(
      SemanticAndPredicateDomain(s, PredicateDomain()).top())
  }

  def refType = SystemParameters.compiler.asInstanceOf[SilCompiler].refType

  def edgeLocalIdToPredInstId(edgeLocalId: EdgeLocalIdentifier): VariableIdentifier = {
    require(edgeLocalId.typ == PredicateInstanceType,
      "edge-local identifier must have a predicate instance type")
    VariableIdentifier(edgeLocalId.field)(PredicateInstanceType)
  }
}

case class PredicateDomain(
    instances: PredicateInstancesDomain = PredicateInstancesDomain(),
    definitions: PredicateDefinitionsDomain = PredicateDefinitionsDomain())
  extends RoutingSemanticCartesianProductDomain[
    PredicateInstancesDomain,
    PredicateDefinitionsDomain,
    PredicateDomain] {

  def factory(i: PredicateInstancesDomain, d: PredicateDefinitionsDomain) =
    PredicateDomain(i, d)

  def _1 = instances

  def _1canHandle(id: Identifier) =
    id.typ == PredicateInstanceType

  def _2 = definitions

  def _2canHandle(id: Identifier) =
    id.typ == PredicateDefinitionType
}

case class SemanticAndPredicateDomain[S <: SemanticDomain[S]](
    valueState: S, predicateState: PredicateDomain)
  extends RoutingSemanticCartesianProductDomain[
    S, PredicateDomain, SemanticAndPredicateDomain[S]] {

  def factory(valueState: S, predicateState: PredicateDomain) =
    SemanticAndPredicateDomain(valueState, predicateState)

  def _1 = valueState

  def _1canHandle(id: Identifier) =
    !_2canHandle(id)

  def _2 = predicateState

  def _2canHandle(id: Identifier) =
    id.typ == PredicateDefinitionType || id.typ == PredicateInstanceType
}
