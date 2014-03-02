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
        val recvPredDefs = recvEdge.state.defs

        val foldedPredInstIds = recvEdge.state.insts.foldedPredInstIds
        val unfoldedPredInstIds = recvEdge.state.insts.unfoldedPredInstIds
        val availPredInstIds = foldedPredInstIds ++ unfoldedPredInstIds

        if (availPredInstIds.isEmpty) {
          println("there needs to be either a folded or unfolded predicate")
          Seq(condHeap)
        } else {
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
                  val nestedPredDefIdOption = if (result.heap.outEdges(recvEdge.target, Some(field)).exists(_.target != NullVertex)) {
                    val nestedPredDefId = PredicateDefinition.makeId()
                    val nestedPredDef = PredicateDefinition().top()
                    result = result.map(_.createVariable(nestedPredDefId))
                    // Always assume that the predicate instance is recursive
                    // val nestedPredDefId = recvPredDefId
                    // val nestedPredDef = recvPredDef
                    result = result.map(_.assign(nestedPredDefId, nestedPredDef))
                    Some(nestedPredDefId)
                  } else None

                  recvPredDef.addRefFieldPerm(field, nestedPredDefIdOption)
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
                val nestedPredDefIds = recvPredDef.refFieldPerms.get(id.path.last).value.asInstanceOf[Set[VariableIdentifier]]
                if (!nestedPredDefIds.isEmpty) {
                  val nestedPredDefId = nestedPredDefIds.head
                  val nestedPredInstId = nestedPredDefId.toPredInstId

                  result = result.mapEdges(e => {
                    // No predicate instances on null edges
                    if (e.source == recvEdge.target && e.target != NullVertex) {
                      val edgeLocId = EdgeLocalIdentifier(List(e.field), nestedPredInstId)
                      val equality = BinaryArithmeticExpression(edgeLocId, Folded, ArithmeticOperator.==, BoolType)
                      e.state.createVariable(edgeLocId).assume(equality)
                    } else e.state
                  })
                }
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
          .removeUnwantedPredInstIds()
      case _ =>
        result.removeUnwantedPredInstIds()
    }
  }

  override def assignField(left: AccessPathIdentifier, right: Expression) = {
    var result = super.assignField(left, right).removeUnwantedPredInstIds()

    if (left.typ.isObject) {
      val receiverPath = left.path.dropRight(1)
      val receiverId = AccessPathIdentifier(receiverPath)(refType)
      val field = left.path.last

      result = CondHeapGraph[EdgeStateDomain[S], T](result).evalExp(receiverId).mapCondHeaps(condHeap => {
        val recvEdge = condHeap.takenPath(receiverId.path).edges.head
        val recvVertex = recvEdge.target
        val nonNullOutEdges = condHeap.heap.outEdges(recvVertex, Some(field)).filter(_.target != NullVertex)
        var resultingCondHeap = condHeap

        if (nonNullOutEdges.isEmpty) {
          println("nothing to do, we only assigned null to the field")
        } else {
          val recvPredDefId = recvEdge.state.insts.unfoldedPredInstIds.head.toPredDefId
          val curRecvPredDef = recvEdge.state.defs.get(recvPredDefId)
          val curNestedRecvPredDefIds = curRecvPredDef.refFieldPerms.get(field).value

          val outEdge = nonNullOutEdges.head // Assume there is only one
          val newNestedRecvPredDefIds = outEdge.state.insts.foldedPredInstIds.map(_.toPredDefId).asInstanceOf[Set[Identifier]]

          if (curNestedRecvPredDefIds.isEmpty) {
            resultingCondHeap = resultingCondHeap.map(state => {
              state.assign(recvPredDefId, curRecvPredDef.addRefFieldPerm(field, Some(newNestedRecvPredDefIds.head)))
            })
          } else {
            val repl = new Replacement()

            repl.value += (curNestedRecvPredDefIds -> newNestedRecvPredDefIds)

            resultingCondHeap = resultingCondHeap.map(state => {
              state.merge(repl)
            })
          }
        }
        Seq(resultingCondHeap)
      }).join
    }

    result
  }

  private def _tryToFoldAllLocalVars(): PredicateDrivenHeapState[S] = {
    var result = this

    this.abstractHeap.localVarVertices.foreach(localVarVertex => {
      val localVarAccessPathId = AccessPathIdentifier(List(localVarVertex.name))(localVarVertex.typ)
      result = CondHeapGraph[EdgeStateDomain[S], T](result).evalExp(localVarAccessPathId).mapCondHeaps(condHeap => {
        val recvEdge = condHeap.takenPath(localVarAccessPathId.path).edges.head
        val recvEdgeInsts = recvEdge.state.insts
        val unfoldedPredInstIds = recvEdgeInsts.unfoldedPredInstIds

        var resultingCondHeap = condHeap

        unfoldedPredInstIds.foreach(unfoldedPredInstId => {
          val unfoldedPredDef = resultingCondHeap.cond.defs.get(unfoldedPredInstId.toPredDefId)

          val canFold = unfoldedPredDef.refFieldPerms.map.forall({
            case (field, nestedPredDefId) =>
              val nestedPredInstIds = nestedPredDefId.value.asInstanceOf[Set[VariableIdentifier]].map(_.toPredInstId)
              val edgesThatNeedFoldedPredInst = resultingCondHeap.heap.outEdges(recvEdge.target, Some(field)).filter(_.target != NullVertex)
              val canFold = edgesThatNeedFoldedPredInst.forall(edge => {
                nestedPredInstIds subsetOf edge.state.insts.foldedPredInstIds
              })

              canFold
          })

          if (canFold) {
            unfoldedPredDef.refFieldPerms.map.foreach({
              case (field, nestedPredDefId) =>
                val nestedPredInstIds = nestedPredDefId.value.asInstanceOf[Set[VariableIdentifier]].map(_.toPredInstId)
                val edgesThatNeedFoldedPredInst = resultingCondHeap.heap.outEdges(recvEdge.target, Some(field)).filter(_.target != NullVertex)

                resultingCondHeap = resultingCondHeap.mapEdges(edge => {
                  if (edgesThatNeedFoldedPredInst.contains(edge)) {
                    val newState = nestedPredInstIds.foldLeft(edge.state)((state, nestedPredInstId) => {
                      val edgeLocId = EdgeLocalIdentifier(List(edge.field), nestedPredInstId)
                      val newState = state.setToTop(edgeLocId)
                      newState
                    })
                    newState
                  } else {
                    edge.state
                  }
                  })
            })

            resultingCondHeap = resultingCondHeap.mapEdges(edge => {
              if (edge == recvEdge) {
                val edgeLocId = EdgeLocalIdentifier(List(recvEdge.field), unfoldedPredInstId)
                edge.state.assign(edgeLocId, Folded)
              } else {
                edge.state
              }
            })
          }
        })

        Seq(resultingCondHeap)
      }).join
    })

    result
  }

  override def lub(other: PredicateDrivenHeapState[S]): PredicateDrivenHeapState[S] = {
    if (isBottom || other.isTop)
      return other
    if (isTop || other.isBottom)
      return this

    // Fold as much as possible before joining
    var thisFolded = _tryToFoldAllLocalVars()
    var otherFolded = other._tryToFoldAllLocalVars()

    val iso = thisFolded.abstractHeap.mcs(otherFolded.abstractHeap).vertexMap
    var (resAbstractHeap, renameMap) = thisFolded.abstractHeap.minCommonSuperGraphBeforeJoin(otherFolded.abstractHeap, iso)

    val repl = new Replacement()

    resAbstractHeap.weakEdgeEquivalenceSets.map(edges => {
      if (edges.size == 1) {
        edges
      } else {
        assert(edges.size == 2, "there should not be more than two weakly-equal edges")
        val edge = edges.head
        val otherEdge = edges.tail.head

        val predInstIds = edge.state.insts.foldedPredInstIds
        val otherPredInstIds = otherEdge.state.insts.foldedPredInstIds

        assert(predInstIds.size <= 1, "cannot handle more than one folded pred inst id")
        assert(otherPredInstIds.size <= 1, "cannot handle more than one folded pred inst id")

        if (predInstIds.size == 1 && otherPredInstIds.size == 1) {
          val predInstId = predInstIds.head
          val otherPredInstId = otherPredInstIds.head

          val predDefId = predInstId.toPredDefId
          val otherPredDefId = otherPredInstId.toPredDefId

          if (predDefId != otherPredDefId) {
            val predDef = edge.state.defs.get(predDefId)
            val otherPredDef = otherEdge.state.defs.get(otherPredDefId)

            // Do it in both directions separately

            assert(predDef.refFieldPerms.map.size == 1, "must have permission to exactly one ref field")
            assert(otherPredDef.refFieldPerms.map.size == 1, "must have permission to exactly one ref field")

            val nestedPredDefId = predDef.refFieldPerms.map.head._2.value
            val otherNestedPredDefId = otherPredDef.refFieldPerms.map.head._2.value

            if (Set(predDefId) == otherNestedPredDefId) {
              repl.value += (Set[Identifier](predDefId, otherPredDefId) -> Set(predDefId))
            } else if (Set(otherPredDefId) == nestedPredDefId) {
              repl.value += (Set[Identifier](predDefId, otherPredDefId) -> Set(otherPredDefId))
            }
          }
        }
      }
    })

    if (!repl.value.isEmpty) {
      resAbstractHeap = resAbstractHeap.copy(edges = resAbstractHeap.edges.map(edge => {
        var newState = edge.state.merge(repl)
        val edgeLocalRepl = new Replacement()

        for ((fromSet, toSet) <- repl.value) {
          val newFromSet = (fromSet -- toSet).asInstanceOf[Set[VariableIdentifier]]map(predDefId => EdgeLocalIdentifier(List(edge.field), predDefId.toPredInstId))
          val newToSet = toSet.asInstanceOf[Set[VariableIdentifier]]map(predDefId => EdgeLocalIdentifier(List(edge.field), predDefId.toPredInstId))

          edgeLocalRepl.value += (newFromSet.toSet[Identifier] -> newToSet.toSet[Identifier])
        }

        newState = newState.merge(edgeLocalRepl)

        edge.copy(state = newState)
      }))
    }

    resAbstractHeap = resAbstractHeap.joinCommonEdges()

    val valueRenameMap = Vertex.vertexMapToValueHeapIdMap(renameMap)
    val resGeneralState = thisFolded.generalValState.merge(repl).lub(otherFolded.generalValState.merge(repl).rename(valueRenameMap.toMap))

    factory(resAbstractHeap, resGeneralState, ExpressionSet())
  }

  def predInstHeapIds: Set[ValueHeapIdentifier] =
    generalValState.valueHeapIds.filter(_.typ == PredicateInstanceType)

  def removeUnwantedPredInstIds(): PredicateDrivenHeapState[S] = {
    copy(
      generalValState = generalValState.removeVariables(predInstHeapIds),
      abstractHeap = abstractHeap.mapEdgeStates(state => {
        state.removeVariables(predInstHeapIds ++ state.sourceEdgeLocalIds)
      })
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
    edgeLocalId.field.asInstanceOf[VariableIdentifier]
  }

  implicit class ExtendedEdgeStateDomain[S <: SemanticDomain[S]](state: EdgeStateDomain[S]) {
    def insts: PredicateInstancesDomain =
      state.valueState.predicateState.instances

    def defs: PredicateDefinitionsDomain =
      state.valueState.predicateState.definitions
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
