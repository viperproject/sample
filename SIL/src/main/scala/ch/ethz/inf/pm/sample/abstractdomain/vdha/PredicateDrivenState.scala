package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.{ToStringUtilities, SystemParameters}
import ch.ethz.inf.pm.sample.oorepresentation.sil.{PredType, SilCompiler}
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

      val edgeVerticesToPredId = result.abstractHeap.localVarEdges.map(edge => {
        val predId = PredicateDefinition.makeId()
        Set(edge.source, edge.target) -> predId
      }).toMap

      val predDefIds = edgeVerticesToPredId.values
      result = result.createNonObjectVariables(predDefIds.toSet)

      if (MakePredicateRecursiveFromBeginning) {
        edgeVerticesToPredId.map({
          case (vertices, predDefId) =>
            if (!vertices.contains(NullVertex)) {
              var predDef = PredicateDefinition()
              refType.objectFields.foreach(field => {
                predDef = predDef.addRefFieldPerm(field, Some(predDefId))
              })
              result = result.assignVariable(predDefId, predDef)
            }
        })
      }

      result = CondHeapGraph[EdgeStateDomain[S], T](result).mapEdges(edge => {
        edgeVerticesToPredId.get(edge.vertices) match {
          case Some(predId) =>
            val edgeLocalPredId = EdgeLocalIdentifier(List(edge.field), predId)
            edge.state.assign(edgeLocalPredId, Folded)
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
    val field = VariableIdentifier(id.path.last)(id.typ)

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

        val recvPredDefs = recvEdge.state.predDefs

        val foldedIds = recvEdge.state.predInsts.foldedIds
        val unfoldedIds = recvEdge.state.predInsts.unfoldedIds
        val foldedAndUnfoldedIds = foldedIds ++ unfoldedIds

        if (foldedAndUnfoldedIds.isEmpty) {
          println("there needs to be either a folded or unfolded predicate")
          Seq(condHeap)
        } else {
          def findPerm(predIds: Set[VariableIdentifier]): Option[VariableIdentifier] = {
            predIds.find(predId => {
              val predDef = recvPredDefs.get(predId)
              if (id.typ.isObject)
                predDef.refFieldPerms.map.contains(field)
              else
                predDef.valFieldPerms.value.contains(field)
            })
          }

          findPerm(unfoldedIds) match {
            case Some(unfoldedPredId) =>
              Seq(condHeap) // Nothing to do
            case None => {
              var result = condHeap

              val (recvPredId, hasPerm) = findPerm(foldedIds) match {
                case Some(foldedId) => (foldedId, true)
                case None => (foldedAndUnfoldedIds.head, false)
              }
              var recvPredDef = recvPredDefs.get(recvPredId)

              // Unfold
              result = result.mapEdges(e => {
                if (e.target == recvEdge.target) {
                  val edgeLocId = EdgeLocalIdentifier(List(e.field), recvPredId)
                  e.state.assign(edgeLocId, Unfolded)
                } else e.state
              })

              // Add permission if necessary
              if (!hasPerm) {
                recvPredDef = if (recvEdge.target == NullVertex)
                  recvPredDef.bottom()
                else if (id.typ.isObject) {
                  val nestedPredIdOption = if (result.heap.outEdges(recvEdge.target, Some(field.name)).exists(_.target != NullVertex)) {
                    var nestedPredId: VariableIdentifier = null
                    if (MakePredicateRecursiveWhenUnfolding) {
                      // Always assume that the predicate instance is recursive
                      nestedPredId = recvPredId
                    } else {
                      nestedPredId = PredicateDefinition.makeId()
                      val nestedPredDef = PredicateDefinition().top()
                      result = result.map(_.assign(nestedPredId, nestedPredDef))
                    }

                    Some(nestedPredId)
                  } else None
                  recvPredDef.addRefFieldPerm(field, nestedPredIdOption)
                } else {
                  recvPredDef.addValFieldPerm(field)
                }

                // Assign the new predicate definition
                result = result.map(state => {
                  state.assign(recvPredId, recvPredDef)
                })
              }

              // Add folded nested predicate instances
              if (id.typ.isObject) {
                // TODO: Should add ALL nested predicate instances
                val nestedPredIds = recvPredDef.refFieldPerms.get(field).value.asInstanceOf[Set[VariableIdentifier]]
                if (!nestedPredIds.isEmpty) {
                  val nestedPredId = nestedPredIds.head

                  result = result.mapEdges(e => {
                    // No predicate instances on null edges
                    if (e.source == recvEdge.target && e.target != NullVertex) {
                      val edgeLocId = EdgeLocalIdentifier(List(e.field), nestedPredId)
                      // Old code that uses asumme (which is not implemented)
                      // val equality = BinaryArithmeticExpression(edgeLocId, Folded, ArithmeticOperator.==, BoolType)
                      // e.state.createVariable(edgeLocId).assume(equality)

                      // When there is already a folded pred instance on this edge:
                      // That edge cannot be. We would have the instance twice
                      // TODO: Should not matter what ID it is. If they overlap in terms of
                      // permissions, it is impossible
                      val newState = if (e.state.predInsts.foldedIds.contains(nestedPredId)) {
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

    val predId = PredicateDefinition.makeId()
    val predValueHeapId = ValueHeapIdentifier(newVertex, predId)

    result = result.createNonObjectVariables(Set(predId, predValueHeapId))
    result = CondHeapGraph[EdgeStateDomain[S], T](result).map(
      _.assign(predValueHeapId, Unfolded))

    (result, newVertex)
  }

  override def assignVariable(left: Expression, right: Expression) = {
    val result = super.assignVariable(left, right)

    (left, right) match {
      case (left: VariableIdentifier, right: VertexExpression) =>
        val source = abstractHeap.localVarVertex(left.getName)
        val addedEdge = result.abstractHeap.outEdges(source).head

        val predValHeapIds = addedEdge.state.valueHeapIds(addedEdge.target).filter(_.typ == PredType)
        val repl = new Replacement()

        predValHeapIds.foreach(predValHeapId => {
          val predId = predValHeapId.field
          val predEdgeLocalId = EdgeLocalIdentifier(List(addedEdge.field), predId)
          repl.value += (Set[Identifier](predValHeapId) -> Set[Identifier](predEdgeLocalId))
        })

        val newEdge = addedEdge.copy(state = addedEdge.state.merge(repl))

        // Old code. The above does not use assume
        // var newEdge = predInstIds.foldLeft(addedEdge)(_.createTargetEdgeLocalId(_))
        // newEdge = newEdge.assumeEdgeLocalIdEqualities()
        result
          .copy(abstractHeap = result.abstractHeap.copy(
            edges = result.abstractHeap.edges - addedEdge + newEdge))
          .removeUnwantedPredIds()
      case _ =>
        result.removeUnwantedPredIds()
    }
  }

  override def assignField(left: AccessPathIdentifier, right: Expression) = {
    var result = super.assignField(left, right).removeUnwantedPredIds()

    if (left.typ.isObject) {
      val receiverPath = left.path.dropRight(1)
      val receiverId = AccessPathIdentifier(receiverPath)(refType)
      val field = VariableIdentifier(left.path.last)(left.typ)

      result = CondHeapGraph[EdgeStateDomain[S], T](result).evalExp(receiverId).mapCondHeaps(condHeap => {
        val recvEdge = condHeap.takenPath(receiverId.path).edges.head
        val recvVertex = recvEdge.target
        val nonNullOutEdges = condHeap.heap.outEdges(recvVertex, Some(field.name)).filter(_.target != NullVertex)
        var resultingCondHeap = condHeap

        if (nonNullOutEdges.isEmpty) {
          println("nothing to do, we only assigned null to the field")
        } else {
          val recvPredId = recvEdge.state.predInsts.unfoldedIds.head
          val curRecvPredDef = recvEdge.state.predDefs.get(recvPredId)
          val curNestedRecvPredIds = curRecvPredDef.refFieldPerms.get(field).value

          assert(nonNullOutEdges.size == 1, "assume that there is exactly one outgoing non-null edge")

          val outEdge = nonNullOutEdges.head
          val newNestedRecvPredIds = outEdge.state.predInsts.foldedIds.asInstanceOf[Set[Identifier]]

          if (curNestedRecvPredIds.isEmpty) {
            resultingCondHeap = resultingCondHeap.map(state => {
              state.assign(recvPredId, curRecvPredDef.setRefFieldPerm(field, newNestedRecvPredIds))
            })
          } else {
            val repl = new Replacement()

            repl.value += (curNestedRecvPredIds -> newNestedRecvPredIds)

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
      var candidateUnfoldedPredIds = result.abstractHeap.outEdges(localVarVertex).head.state.predInsts.unfoldedIds
      result.abstractHeap.outEdges(localVarVertex).tail.filter(_.target != NullVertex).foreach(edge => {
        candidateUnfoldedPredIds = candidateUnfoldedPredIds.intersect(edge.state.predInsts.unfoldedIds)
      })

      candidateUnfoldedPredIds.foreach(unfoldedPredId => {
        var candidateAbstractHeap = result.abstractHeap
        var canFold = true

        result.abstractHeap.outEdges(localVarVertex).foreach(recvEdge => {
          val unfoldedPredDef = recvEdge.state.predDefs.get(unfoldedPredId)

          unfoldedPredDef.refFieldPerms.map.foreach({
            case (field, nestedPredId) =>
              val nestedPredIds = nestedPredId.value.asInstanceOf[Set[VariableIdentifier]]
              val edgesThatNeedFoldedPredInst = result.abstractHeap.outEdges(recvEdge.target, Some(field.getName)).filter(_.target != NullVertex)
              val canFoldThis = edgesThatNeedFoldedPredInst.forall(edge => {
                nestedPredIds subsetOf edge.state.predInsts.foldedIds
              })
              // The target vertices could have other incoming edges,
              // so we need to remove the folded instance from them too.
              val verticesToRemoveFoldedPredInstFrom = edgesThatNeedFoldedPredInst.map(_.target)
              if (!canFoldThis) {
                canFold = false
              } else {
                candidateAbstractHeap = candidateAbstractHeap.copy(edges = candidateAbstractHeap.edges.map(edge => {
                  if (verticesToRemoveFoldedPredInstFrom.contains(edge.target)) {
                    val newState = nestedPredIds.foldLeft(edge.state)((state, nestedPredInstId) => {
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
              val edgeLocId = EdgeLocalIdentifier(List(edge.field), unfoldedPredId)
              edge.copy(state = edge.state.assign(edgeLocId, Folded))
            } else {
              edge
            }
          }))
        })

        if (canFold) {
          abstractHeap.localVarVertices.foreach(localVarVertex => {
            def hasPredInstOnEveryEdge(heap: HeapGraph[EdgeStateDomain[S]]): Boolean = {
              heap.outEdges(localVarVertex).filter(_.target != NullVertex).forall(!_.state.predInsts.foldedAndUnfolded.isEmpty)
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

          val predInstIds = edge.state.predInsts.foldedIds
          val otherPredInstIds = otherEdge.state.predInsts.foldedIds

          assert(predInstIds.size <= 1, "cannot handle more than one folded pred inst id")
          assert(otherPredInstIds.size <= 1, "cannot handle more than one folded pred inst id")

          if (predInstIds.size == 1 && otherPredInstIds.size == 1) {
            val predId = predInstIds.head
            val otherPredId = otherPredInstIds.head

            if (predId != otherPredId) {
              val predDef00 = edge.state.predDefs.get(predId)
              val predDef01 = edge.state.predDefs.get(otherPredId)
              val predDef10 = otherEdge.state.predDefs.get(predId)
              val predDef11 = otherEdge.state.predDefs.get(otherPredId)

              // Do it in both directions separately
              // TODO: Should probably always keep the predicate with the lower version
              if (predDef11.refFieldPerms.map.values.exists(_.value.contains(predId))) {
                repl.value += (Set[Identifier](predId, otherPredId) -> Set(predId))
              } else if (predDef00.refFieldPerms.map.values.exists(_.value.contains(otherPredId))) {
                repl.value += (Set[Identifier](predId, otherPredId) -> Set(otherPredId))
              } else if (predDef01.refFieldPerms.map.values.exists(_.value.contains(predId))) {
                repl.value += (Set[Identifier](predId, otherPredId) -> Set(otherPredId))
              } else if (predDef10.refFieldPerms.map.values.exists(_.value.contains(otherPredId))) {
                repl.value += (Set[Identifier](predId, otherPredId) -> Set(predId))
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
            val newFromSet = fromSet.asInstanceOf[Set[VariableIdentifier]]map(predId => EdgeLocalIdentifier(List(edge.field), predId))
            val newToSet = toSet.asInstanceOf[Set[VariableIdentifier]]map(predId => EdgeLocalIdentifier(List(edge.field), predId))

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
          edgeSet.size == 1 && edgeSet.head.state.ids.filterNot(_.typ == PredType) == rEdge.state.ids.filterNot(_.typ == PredType)
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

  def predValueHeapIds: Set[ValueHeapIdentifier] =
    generalValState.valueHeapIds.filter(_.typ == PredType)

  def removeUnwantedPredIds(): PredicateDrivenHeapState[S] = {
    copy(
      generalValState = generalValState.removeVariables(predValueHeapIds),
      abstractHeap = abstractHeap.mapEdgeStates(state => {
        state.removeVariables(predValueHeapIds ++ state.sourceEdgeLocalIds.filter(_.typ == PredType))
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

  def edgeLocalIdToPredId(edgeLocalId: EdgeLocalIdentifier): VariableIdentifier = {
    require(edgeLocalId.typ == PredType,
      "edge-local identifier must have a predicate instance type")
    edgeLocalId.field.asInstanceOf[VariableIdentifier]
  }

  implicit class ExtendedEdgeStateDomain[S <: SemanticDomain[S]](state: EdgeStateDomain[S]) {
    def predInsts: PredicateInstancesDomain =
      state.valueState.predicateState.instances

    def predDefs: PredicateDefinitionsDomain =
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
    !id.isInstanceOf[VariableIdentifier]

  def _2 = definitions

  def _2canHandle(id: Identifier) =
    id.isInstanceOf[VariableIdentifier]

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
    id.typ == PredType
}
