package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.{ToStringUtilities, SystemParameters}
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

  // Experimental flag to "simulate" a second analysis
  // with constrained initial state
  private val MakePredicateRecursiveFromBeginning: Boolean = false

  private val MakePredicateRecursiveWhenUnfolding: Boolean = false

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
      PredicateDefinition.resetId()

      var result = super.createVariableForArgument(variable, typ)

      val edgeVerticesToPredDefId = result.abstractHeap.localVarEdges.map(edge => {
        val predDefId = PredicateDefinition.makeId()
        Set(edge.source, edge.target) -> predDefId
      }).toMap

      val predDefIds = edgeVerticesToPredDefId.values
      result = result.createNonObjectVariables(predDefIds.toSet)

      if (MakePredicateRecursiveFromBeginning) {
        edgeVerticesToPredDefId.map({
          case (vertices, predDefId) =>
            if (!vertices.contains(NullVertex)) {
              var predDef = PredicateDefinition()
              refType.objectFields.foreach(field => {
                predDef = predDef.addRefFieldPerm(field.getName, Some(predDefId))
              })
              result = result.assignVariable(predDefId, predDef)
            }
        })
      }

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
    val receiverPath = id.path.dropRight(1)
    val receiverId = AccessPathIdentifier(receiverPath)(refType)
    val field = id.path.last

    assert(receiverPath.size == 1, "currently only support obj.field")

    // Only materialize the receiver of the field access at this point
    // It's too early to also materialize the target of the field access.
    // Unfolding after materializing the receiver may cause some edges
    // going out of the receiver vertex to be removed, so we don't need
    // to follow them when materializing the target of the field access.
    var result = materializePath(receiverId.path)

    result = CondHeapGraph[EdgeStateDomain[S], T](result)
      .evalExp(receiverId).mapCondHeaps(condHeap => {
        val recvEdge = condHeap.takenPath(receiverId.path).edges.head

        assert(!recvEdge.target.isInstanceOf[SummaryHeapVertex],
          "target of the receiver edge must not be a summary node")

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
                    var nestedPredDefId: VariableIdentifier = null
                    if (MakePredicateRecursiveWhenUnfolding) {
                      // Always assume that the predicate instance is recursive
                      nestedPredDefId = recvPredDefId
                    } else {
                      nestedPredDefId = PredicateDefinition.makeId()
                      val nestedPredDef = PredicateDefinition().top()
                      result = result.map(_.assign(nestedPredDefId, nestedPredDef))
                    }

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
                      // Old code that uses asumme (which is not implemented)
                      // val equality = BinaryArithmeticExpression(edgeLocId, Folded, ArithmeticOperator.==, BoolType)
                      // e.state.createVariable(edgeLocId).assume(equality)

                      // When there is already a folded pred instance on this edge:
                      // That edge cannot be. We would have the instance twice
                      // TODO: Should not matter what ID it is. If they overlap in terms of
                      // permissions, it is impossible
                      val newState = if (e.state.insts.foldedPredInstIds.contains(nestedPredInstId)) {
                        println("Impossible edge detected, removing it")
                        e.state.assign(edgeLocId, Unfolded).lub(e.state) // Hack to set it to bottom
                      } else {
                        e.state.assign(edgeLocId, Folded).lub(e.state)
                      }

                      newState
                    } else e.state
                  })
                }
              }

              Seq(result)
            }
          }
        }
      }).join

    // Materialize the target of the field access now
    result.materializePath(id.objPath).copy(expr = ExpressionSet(id))
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
        val addedEdge = result.abstractHeap.outEdges(source).head

        val predInstValHeapIds = addedEdge.state.valueHeapIds(addedEdge.target).filter(_.typ == PredicateInstanceType)
        val repl = new Replacement()

        predInstValHeapIds.foreach(predInstValHeapId => {
          val predInstId = VariableIdentifier(predInstValHeapId.field)(PredicateInstanceType)
          val predInstEdgeLocalId = EdgeLocalIdentifier(List(addedEdge.field), predInstId)
          repl.value += (Set[Identifier](predInstValHeapId) -> Set[Identifier](predInstEdgeLocalId))
        })

        val newEdge = addedEdge.copy(state = addedEdge.state.merge(repl))

        // Old code. The above does not use assume
        // var newEdge = predInstIds.foldLeft(addedEdge)(_.createTargetEdgeLocalId(_))
        // newEdge = newEdge.assumeEdgeLocalIdEqualities()
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

          assert(nonNullOutEdges.size == 1, "assume that there is exactly one outgoing non-null edge")

          val outEdge = nonNullOutEdges.head
          val newNestedRecvPredDefIds = outEdge.state.insts.foldedPredInstIds.map(_.toPredDefId).asInstanceOf[Set[Identifier]]

          if (curNestedRecvPredDefIds.isEmpty) {
            resultingCondHeap = resultingCondHeap.map(state => {
              state.assign(recvPredDefId, curRecvPredDef.setRefFieldPerm(field, newNestedRecvPredDefIds))
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

  def tryToFoldAllLocalVars(): PredicateDrivenHeapState[S] = {
    var result = this

    this.abstractHeap.localVarVertices.foreach(localVarVertex => {
      // Only fold local variables if it is possible to do so
      // on all local variable edges
      var candidateUnfoldedPredInstIds = result.abstractHeap.outEdges(localVarVertex).head.state.insts.unfoldedPredInstIds
      result.abstractHeap.outEdges(localVarVertex).tail.filter(_.target != NullVertex).foreach(edge => {
        candidateUnfoldedPredInstIds = candidateUnfoldedPredInstIds.intersect(edge.state.insts.unfoldedPredInstIds)
      })

      candidateUnfoldedPredInstIds.foreach(unfoldedPredInstId => {
        var candidateAbstractHeap = result.abstractHeap
        var canFold = true

        result.abstractHeap.outEdges(localVarVertex).foreach(recvEdge => {
          val unfoldedPredDef = recvEdge.state.defs.get(unfoldedPredInstId.toPredDefId)

          unfoldedPredDef.refFieldPerms.map.foreach({
            case (field, nestedPredDefId) =>
              val nestedPredInstIds = nestedPredDefId.value.asInstanceOf[Set[VariableIdentifier]].map(_.toPredInstId)
              val edgesThatNeedFoldedPredInst = result.abstractHeap.outEdges(recvEdge.target, Some(field)).filter(_.target != NullVertex)
              val canFoldThis = edgesThatNeedFoldedPredInst.forall(edge => {
                nestedPredInstIds subsetOf edge.state.insts.foldedPredInstIds
              })
              // The target vertices could have other incoming edges,
              // so we need to remove the folded instance from them too.
              val verticesToRemoveFoldedPredInstFrom = edgesThatNeedFoldedPredInst.map(_.target)
              if (!canFoldThis) {
                canFold = false
              } else {
                candidateAbstractHeap = candidateAbstractHeap.copy(edges = candidateAbstractHeap.edges.map(edge => {
                  if (verticesToRemoveFoldedPredInstFrom.contains(edge.target)) {
                    val newState = nestedPredInstIds.foldLeft(edge.state)((state, nestedPredInstId) => {
                      val edgeLocId = EdgeLocalIdentifier(List(edge.field), nestedPredInstId)
                      val newState = state.setToTop(edgeLocId)
                      newState
                    })
                    edge.copy(state = newState)
                  } else {
                    edge
                  }
                }))
              }
          })

          candidateAbstractHeap = candidateAbstractHeap.copy(edges = candidateAbstractHeap.edges.map(edge => {
            // TODO: Could be more precise. Should only assign to edges
            // that may (must?) exist in any concrete heap where the receiver edge exists.
            if (edge.target == recvEdge.target) {
              val edgeLocId = EdgeLocalIdentifier(List(edge.field), unfoldedPredInstId)
              edge.copy(state = edge.state.assign(edgeLocId, Folded))
            } else {
              edge
            }
          }))
        })

        if (canFold) {
          abstractHeap.localVarVertices.foreach(localVarVertex => {
            def hasPredInstOnEveryEdge(heap: HeapGraph[EdgeStateDomain[S]]): Boolean = {
              heap.outEdges(localVarVertex).filter(_.target != NullVertex).forall(!_.state.insts.foldedAndUnfoldedPredInstIds.isEmpty)
            }

            if (hasPredInstOnEveryEdge(abstractHeap)) {
              if (!hasPredInstOnEveryEdge(candidateAbstractHeap)) {
                println("won't fold because otherwise, we would lose all permissions to some local variable")
                canFold = false
              }
            }
          })
        }

        if (canFold) {
          result = result.copy(abstractHeap = candidateAbstractHeap)
        } else {
          println("cannot fold")
        }
      })
    })

    result
  }

  override def lub(other: PredicateDrivenHeapState[S]): PredicateDrivenHeapState[S] = {
    if (isBottom || other.isTop)
      return other
    if (isTop || other.isBottom)
      return this

    // Fold as much as possible before joining
    val thisFolded = tryToFoldAllLocalVars()
    val otherFolded = other.tryToFoldAllLocalVars()

    val allIsos = CommonSubGraphIso.allMax(from = otherFolded.abstractHeap, to = thisFolded.abstractHeap)

    var bestResultOption: Option[PredicateDrivenHeapState[S]] = None

    for (iso <- allIsos) {
      var (resAbstractHeap, renameMap) = thisFolded.abstractHeap.minCommonSuperGraphBeforeJoin(otherFolded.abstractHeap, iso.vertexMap)

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
              val predDef00 = edge.state.defs.get(predDefId)
              val predDef01 = edge.state.defs.get(otherPredDefId)
              val predDef10 = otherEdge.state.defs.get(predDefId)
              val predDef11 = otherEdge.state.defs.get(otherPredDefId)

              // Do it in both directions separately
              // TODO: Should probably always keep the predicate with the lower version
              if (predDef11.refFieldPerms.map.values.exists(_.value.contains(predDefId))) {
                repl.value += (Set[Identifier](predDefId, otherPredDefId) -> Set(predDefId))
              } else if (predDef00.refFieldPerms.map.values.exists(_.value.contains(otherPredDefId))) {
                repl.value += (Set[Identifier](predDefId, otherPredDefId) -> Set(otherPredDefId))
              } else if (predDef01.refFieldPerms.map.values.exists(_.value.contains(predDefId))) {
                repl.value += (Set[Identifier](predDefId, otherPredDefId) -> Set(otherPredDefId))
              } else if (predDef10.refFieldPerms.map.values.exists(_.value.contains(otherPredDefId))) {
                repl.value += (Set[Identifier](predDefId, otherPredDefId) -> Set(predDefId))
              } else {
                println("Could not merge predicate definitions")
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
            val newFromSet = fromSet.asInstanceOf[Set[VariableIdentifier]]map(predDefId => EdgeLocalIdentifier(List(edge.field), predDefId.toPredInstId))
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

      val result = factory(resAbstractHeap, resGeneralState, ExpressionSet()).prune()

      bestResultOption = bestResultOption match {
        case Some(bestResult) =>
          if (result.abstractHeap.edges.size < bestResult.abstractHeap.edges.size) Some(result)
          else Some(bestResult)
        case None =>
          Some(result)
      }
    }

    bestResultOption.get
  }

  override def widening(other: T): T = {
    def areGraphsIdentical(l: HeapGraph[EdgeStateDomain[S]], r: HeapGraph[EdgeStateDomain[S]]): Boolean = {
      var areGraphsIdentical = true
      for (rEdge <- r.edges) {
        areGraphsIdentical = areGraphsIdentical && {
          val edgeSet = l.edges.filter(lEdge => lEdge.source.equals(rEdge.source) && lEdge.target.equals(rEdge.target))
          // NOTE: The only change in this overridden method is that the sets
          // predicate instance IDs may differ in the two graphs.
          // The reason is that in our custom implementation of 'lub'
          // may merge predicate instance IDs
          edgeSet.size == 1 && edgeSet.head.state.ids.filterNot(_.typ == PredicateInstanceType) == rEdge.state.ids.filterNot(_.typ == PredicateInstanceType)
        }
      }
      areGraphsIdentical
    }

    val (mergedLeft, replacementLeft, mergeMapLeft) = abstractHeap.mergePointedNodes()
    val (mergedRight, replacementRight, mergeMapRight) = other.abstractHeap.mergePointedNodes()
    val rightGenValState = other.generalValState.merge(replacementRight)
    var newRight = factory(mergedRight, rightGenValState, ExpressionSet())
    val newLeft = factory(mergedLeft, generalValState.merge(replacementLeft), ExpressionSet())
    newRight = newLeft.lub(newRight)
    if (!mergedLeft.vertices.equals(newRight.abstractHeap.vertices) || !areGraphsIdentical(mergedLeft, mergedRight)) {
      return newRight
    }
    val newGeneralValState = newLeft.generalValState.widening(newRight.generalValState.merge(replacementRight))
    factory(mergedLeft.wideningAfterMerge(newRight.abstractHeap), newGeneralValState, ExpressionSet())
  }

  def predInstHeapIds: Set[ValueHeapIdentifier] =
    generalValState.valueHeapIds.filter(_.typ == PredicateInstanceType)

  def removeUnwantedPredInstIds(): PredicateDrivenHeapState[S] = {
    copy(
      generalValState = generalValState.removeVariables(predInstHeapIds),
      abstractHeap = abstractHeap.mapEdgeStates(state => {
        state.removeVariables(predInstHeapIds ++ state.sourceEdgeLocalIds.filter(_.typ == PredicateInstanceType))
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

  override def toString =
    "Instances:\n" + ToStringUtilities.indent(instances.toString) + "\n" +
    "Definitions:\n" + ToStringUtilities.indent(definitions.toString)
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
