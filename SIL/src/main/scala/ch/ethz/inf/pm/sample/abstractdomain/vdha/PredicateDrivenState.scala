package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.oorepresentation.sil.SilCompiler
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

    if (id.path.size == 1)
    // No need to make any changes to predicate definitions and instances
    // when we only access a variable
      originalResult
    else {
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
                  case Some(foldedPredInstId) =>
                    val predDefId = foldedPredInstId.toPredDefId
                    val predDef = predDefs.get(predDefId)
                    val predInstAccPathId = AccessPathIdentifier(id.path.dropRight(1), foldedPredInstId)

                    var result = condHeap.evalAccessPathId(id)

                    // Should unfold all nested predicates
                    if (id.typ.isObject) {
                      val nestedPredDefId = predDef.refFieldPerms.get(id.path.last).value.head.asInstanceOf[VariableIdentifier]
                      val nestedPredInstId = nestedPredDefId.toPredInstId
                      val nestedPredInstAccPathId = AccessPathIdentifier(id.path, nestedPredInstId)
                      result = result.assignField(nestedPredInstAccPathId, Folded)
                    }

                    result = result.assignField(predInstAccPathId, Unfolded)
                    result
                  case None =>
                    val predInstId = availablePredInstIds.head
                    val predDefId = predInstId.toPredDefId
                    val predDef = predDefs.get(predDefId)

                    var result = CondHeapGraphSeq(Seq(condHeap))(condHeap.lattice)

                    val newPredicateDef = if (path.target == NullVertex)
                      predDef.bottom()
                    else if (id.typ.isObject) {
                      // val nestedPredDefId = PredicateDefinition.makeId()
                      // val nestedPredDef = PredicateDefinition().top()
                      // TODO: Currently assumes that the predicate is always recursive
                      val nestedPredDefId = predDefId
                      val nestedPredDef = predDef
                      result = result.map(_.assign(nestedPredDefId, nestedPredDef))
                      predDef.addRefFieldPerm(field, nestedPredDefId)
                    } else {
                      predDef.addValFieldPerm(field)
                    }

                    val predInstAccPathId = AccessPathIdentifier(id.path.dropRight(1), predInstId)
                    result = result
                      .map(_.assign(predDefId, newPredicateDef))
                      .assignField(predInstAccPathId, Unfolded)
                    result
                }
            }
          }
        }).join

      // We've lost the expression due to the assignField calls
      result = result.setExpression(originalResult.expr)
      result
    }
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
