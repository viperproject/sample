package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.sil.{BoolType, SilCompiler}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.VertexConstants._
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
      // Reset ghost fields
      val refType = SystemParameters.compiler.asInstanceOf[SilCompiler].refType
      refType.fields = refType.fields.filter(_.typ != PredicateDefinitionType)
      PredicateDefinition.resetId()

      val localVarVertices = abstractHeap.localVarVertices + LocalVariableVertex(variable)
      val objectTypes = localVarVertices.flatMap(_.typ.reachableObjectTypes)

      // Create new abstract heap (no edges yet)
      val newAbstractHeap = HeapGraph[EdgeStateDomain[S]]()
        .addNonHeapVertices(Set(NullVertex) ++ localVarVertices)
        .addHeapVertices(SUMMARY, objectTypes)

      val edgeVerticesToDefId = newAbstractHeap.localVarVertices.flatMap(localVarVertex => {
        localVarVertex.neededEdgeFieldsAndTypes.flatMap({ case (field, fieldTyp) =>
          newAbstractHeap.possibleTargetVertices(fieldTyp).map(targetVertex => {
            val predDefId = PredicateDefinition.makeId()
            refType.fields += predDefId.toPredInstId
            Set(localVarVertex, targetVertex) -> predDefId
          })
        })
      }).toMap

      // Create general value state, retaining old variable identifiers
      val newGeneralValState = generalValState.top()
        .createVariables(generalValState.variableIds.filter(_.typ != PredicateDefinitionType))
        .createVariables(newAbstractHeap.heapVertices.flatMap(_.valueHeapIds))
        .createVariables(edgeVerticesToDefId.values.toSet)

      // Create new edges
      val newEdges = newAbstractHeap.vertices.flatMap(sourceVertex => {
        sourceVertex.neededEdgeFieldsAndTypes.flatMap({ case (field, fieldTyp) =>
          newAbstractHeap.possibleTargetVertices(fieldTyp).map(targetVertex => {
            var edge = Edge(sourceVertex, newGeneralValState, field, targetVertex).createEdgeLocalIds()
            edgeVerticesToDefId.get(edge.vertices) match {
              case Some(defId) =>
                val predInstId = defId.toPredInstId
                val edgeLocalInstId = EdgeLocalIdentifier(List(field), predInstId)
                edge = edge.copy(state = edge.state.assign(edgeLocalInstId, Folded))
              case None =>
            }
            edge
          })
        })
      })

      factory(
        newAbstractHeap.addEdges(newEdges),
        newGeneralValState,
        ExpressionSet(variable),
        isTop = true)
    } else {
      super.createVariableForArgument(variable, typ)
    }
  }

  override def getFieldValue(id: AccessPathIdentifier) = {
    val originalResult = super.getFieldValue(id)

    if (id.path.size == 1)
    // No need to make any changes to predicate definitions and instances
    // when we only access a variable
      originalResult
    else {
      val refType = SystemParameters.compiler.asInstanceOf[SilCompiler].refType

      var result: PredicateDrivenHeapState[S] =
        CondHeapGraph[EdgeStateDomain[S], T](originalResult)
          .evalExp(AccessPathIdentifier(id.path.dropRight(1))(refType)).mapCondHeaps(condHeap => {
          val path = condHeap.takenPath(id.path.dropRight(1))
          val field = id.path.last

          assert(path.edges.size == 1, "currently only support paths of length 1")

          val predState = path.edges.head.state.valueState.predicateState
          val predDefs = predState.definitions
          val predInsts = predState.instances

          val availPredInstEdgeLocalIds = predInsts.targetEdgeLocalIds
            .filter(predInstEdgeLocalId => {
            predInsts.isCertainlyFolded(predInstEdgeLocalId) ||
              predInsts.isCertainlyUnfolded(predInstEdgeLocalId)
          })

          if (availPredInstEdgeLocalIds.isEmpty) {
            println("there needs to be either a folded or unfolded predicate")
            Seq(condHeap)
          } else {
            val (foldedPredInstEdgeLocalIds, unfoldedPredInstEdgeLocalIds) = availPredInstEdgeLocalIds
              .partition(predInsts.isCertainlyFolded(_))

            val availablePredInstIds = availPredInstEdgeLocalIds.map(predInstEdgeLocalId => {
              VariableIdentifier(predInstEdgeLocalId.field)(PredicateInstanceType)
            })

            val foldedPredInstIds = foldedPredInstEdgeLocalIds.map(predInstEdgeLocalId => {
              VariableIdentifier(predInstEdgeLocalId.field)(PredicateInstanceType)
            })

            val unfoldedPredInstIds = unfoldedPredInstEdgeLocalIds.map(predInstEdgeLocalId => {
              VariableIdentifier(predInstEdgeLocalId.field)(PredicateInstanceType)
            })

            def alreadyHasPermission(predInstId: VariableIdentifier): Boolean = {
              val predDef = predDefs.get(predInstId.toPredDefId)
              if (id.typ.isObject)
                predDef.refFieldPerms.map.contains(field)
              else
                predDef.valFieldPerms.value.contains(field)
            }

            assert(availablePredInstIds.count(alreadyHasPermission) <= 1,
              "there may at least be one predicate instance with the permission")

            unfoldedPredInstIds.find(alreadyHasPermission) match {
              case Some(unfoldedPredicateInstId) =>
                Seq(condHeap) // Nothing to do
              case None =>
                foldedPredInstIds.find(alreadyHasPermission) match {
                  case Some(foldedPredicateInstId) =>
                    assert(assertion = false, "should have unfolded")
                    Seq(condHeap)
                  case None =>
                    val predInstId = availablePredInstIds.head
                    val predDefId = predInstId.toPredDefId
                    val predDef = predDefs.get(predDefId)

                    val newPredicateDef = if (path.target == NullVertex)
                      predDef.bottom()
                    else if (id.typ.isObject)
                    // TODO: Currently assumes that predicates are always recursive
                    // Instead, should just create fresh predicate def id
                      predDef.addRefFieldPerm(field, predDefId)
                    else
                      predDef.addValFieldPerm(field)

                    val predInstAccPathId = AccessPathIdentifier(id.path.dropRight(1), predInstId)
                    val condHeapSeq = condHeap
                      .map(_.assign(predDefId, newPredicateDef))
                      .assignField(predInstAccPathId, Unfolded)
                    condHeapSeq
                }
            }
          }
        }).join

      // We've lost the expression due to the assignField calls
      result = result.setExpression(originalResult.expr)
      result
    }
  }

  override def createObject(typ: Type, pp: ProgramPoint, fields: Option[Set[Identifier]]) = {
    val predicateDefId = PredicateDefinition.makeId()
    val refType = SystemParameters.compiler.asInstanceOf[SilCompiler].refType
    refType.fields += predicateDefId.toPredInstId

    var result = super.createObject(typ, pp, fields)
    val expr = result.expr.getSetOfExpressions.head
    val heapVertex = expr.asInstanceOf[VertexExpression].vertex.asInstanceOf[HeapVertex]

    result = result.createVariable(predicateDefId, PredicateDefinitionType, DummyProgramPoint)

    val predicateInstId = predicateDefId.toPredInstId
    val predicateInstValueHeapId = ValueHeapIdentifier(heapVertex, predicateInstId)

    val condHeapGraph = CondHeapGraph[EdgeStateDomain[S], T](result).map(
      _.assume(BinaryArithmeticExpression(predicateInstValueHeapId, Unfolded, ArithmeticOperator.==, BoolType)))

    // Must not prune here
    result = result.copy(abstractHeap = condHeapGraph.heap, generalValState = condHeapGraph.cond)
    result = result.setExpression(result.expr)

    result
  }
}

object PredicateDrivenHeapState {
  type EdgeStateDomain[S <: SemanticDomain[S]] =
  PreciseValueDrivenHeapState.EdgeStateDomain[SemanticAndPredicateDomain[S]]

  def makeTopEdgeState[S <: SemanticDomain[S]](s: S): EdgeStateDomain[S] = {
    PreciseValueDrivenHeapState.makeTopEdgeState(
      SemanticAndPredicateDomain(s, PredicateDomain()).top())
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
