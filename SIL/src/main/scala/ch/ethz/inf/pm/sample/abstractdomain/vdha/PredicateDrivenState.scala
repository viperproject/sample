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

  def mapEdges(f: Edge[EdgeStateDomain[S]] => EdgeStateDomain[S]): T =
    copy(
      abstractHeap = abstractHeap.copy(
        edges = abstractHeap.edges.map(e => { e.copy(state = f(e)) })
      )
    )

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

      result = result.toCondHeapGraph.mapEdges(edge => {
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

  override def getFieldValue(id: AccessPathIdentifier): T = {
    val receiverPath = id.path.dropRight(1)
    val field = VariableIdentifier(id.path.last)(id.typ)

    assert(receiverPath.size == 1, "currently only support obj.field")

    // Only materialize the receiver of the field access at this point
    // It's too early to also materialize the target of the field access.
    // Unfolding after materializing the receiver may cause some edges
    // going out of the receiver vertex to be removed, so we don't need
    // to follow them when materializing the target of the field access.
    var result = materializePath(receiverPath)

    val localVarName = receiverPath.head
    val localVarVertex = result.abstractHeap.localVarVertex(localVarName)
    val recvEdges = result.abstractHeap.outEdges(localVarVertex)

    val nonNullRecvEdges = recvEdges.filterNot(_.target == NullVertex)
    val nonNullRecvVertices = nonNullRecvEdges.map(_.target)

    assert(nonNullRecvEdges.forall(!_.target.isInstanceOf[SummaryHeapVertex]),
      "edge target must not be summary heap vertex, is materialization on?")

    val recvState = Lattice.bigLub(nonNullRecvEdges.map(_.state))

    val predDefs = recvState.predDefs

    val foldedIds = recvState.predInsts.foldedIds
    val foldedIdsWithPerm = foldedIds.filter(predDefs.get(_).hasPerm(field))
    val unfoldedIds = recvState.predInsts.unfoldedIds
    val unfoldedIdsWithPerm = unfoldedIds.filter(predDefs.get(_).hasPerm(field))
    val foldedAndUnfoldedIds = foldedIds ++ unfoldedIds

    if (foldedAndUnfoldedIds.isEmpty) {
      println("there is neither a folded or unfolded predicate instance")
      return bottom()
    }

    if (unfoldedIdsWithPerm.isEmpty) {
      val (recvPredId, hasPerm) = foldedIdsWithPerm.toList match {
        case foldedId :: Nil => (foldedId, true)
        case Nil => (foldedAndUnfoldedIds.head, false)
        case _ => sys.error("there can only be one folded predicate with permission")
      }

      var recvPredDef = predDefs.get(recvPredId)

      // Unfold
      result = result.mapEdges(e => {
        if (nonNullRecvVertices.contains(e.target)) {
          val edgeLocalId = EdgeLocalIdentifier(List(e.field), recvPredId)
          e.state.assign(edgeLocalId, Unfolded)
        } else e.state
      })

      // Add permission if necessary
      if (!hasPerm) {
        if (id.typ.isObject) {
          val fieldEdges = nonNullRecvVertices.flatMap(
            result.abstractHeap.outEdges(_, Some(field.name)))

          val nestedPredIdOption = if (fieldEdges.exists(_.target != NullVertex)) {
            val nestedPredId = PredicateDefinition.makeId()
            val nestedPredDef = PredicateDefinition().top()
            result = result.assignVariable(nestedPredId, nestedPredDef)

            Some(nestedPredId)
          } else None
          recvPredDef = recvPredDef.addRefFieldPerm(field, nestedPredIdOption)
        } else {
          recvPredDef = recvPredDef.addValFieldPerm(field)
        }

        // Assign the new predicate definition
        result = result.assignVariable(recvPredId, recvPredDef)
      }

      // Add folded nested predicate instances
      if (id.typ.isObject) {
        // TODO: Should add ALL nested predicate instances
        val nestedPredIds = recvPredDef.refFieldPerms.get(field).value

        if (!nestedPredIds.isEmpty) {
          val nestedPredId = nestedPredIds.head.asInstanceOf[VariableIdentifier]

          result = result.mapEdges(e => {
            // No predicate instances on null edges
            if (nonNullRecvVertices.contains(e.source) && e.target != NullVertex) {
              val edgeLocId = EdgeLocalIdentifier(List(e.field), nestedPredId)
              // When there is already a folded predicated instance on this edge:
              // That edge cannot be. We would have the instance twice
              // TODO: Should not matter what ID it is. If they overlap in terms of
              // permissions, it is impossible
              val newState = if (e.state.predInsts.foldedIds.contains(nestedPredId)) {
                println("Impossible edge detected, removing it")
                // Hack to set it to bottom
                e.state.assign(edgeLocId, Unfolded).lub(e.state)
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

    recvEdges.filter(_.target == NullVertex).toList match {
      case nullRecvEdge :: Nil =>
        for (foldedPredId <- nullRecvEdge.state.predInsts.foldedIds) {
          val bottom = PredicateDefinition().bottom()
          result = result.assignVariable(foldedPredId, bottom)
        }
      case Nil =>
    }

    result = result.prune()

    // Materialize the target of the field access now
    result.materializePath(id.objPath).copy(expr = ExpressionSet(id))
  }

  override protected def createObject(typ: Type) = {
    val (result, newVertex) = super.createObject(typ)

    val predId = PredicateDefinition.makeId()
    val predValueHeapId = ValueHeapIdentifier(newVertex, predId)

    val newResult = result
      .createNonObjectVariables(Set(predId, predValueHeapId))
      .toCondHeapGraph.map(_.assign(predValueHeapId, Unfolded))

    (newResult, newVertex)
  }

  override def assignVariable(left: Expression, right: Expression) = {
    val result = super.assignVariable(left, right)
    val newResult = (left, right) match {
      case (left: VariableIdentifier, right: VertexExpression) =>
        val source = abstractHeap.localVarVertex(left.getName)
        val localVarEdges = result.abstractHeap.outEdges(source)

        assert(localVarEdges.size == 1,
          "there must be exactly one local variable edge")

        val addedEdge = localVarEdges.head
        val predValHeapIds = addedEdge.state.predHeapIds(addedEdge.target)
        val repl = new Replacement()

        predValHeapIds.foreach(predValHeapId => {
          val predEdgeLocalId = EdgeLocalIdentifier(List(addedEdge.field), predValHeapId.field)
          repl.value += (Set[Identifier](predValHeapId) -> Set[Identifier](predEdgeLocalId))
        })

        val newEdge = addedEdge.copy(state = addedEdge.state.merge(repl))

        result
          .copy(abstractHeap = result.abstractHeap.copy(
            edges = result.abstractHeap.edges - addedEdge + newEdge))
      case _ =>
        result
    }

    newResult.prunePredIds()
  }

  override def assignField(left: AccessPathIdentifier, right: Expression) = {
    var result = super.assignField(left, right).prunePredIds()

    if (left.typ.isObject) {
      val receiverPath = left.path.dropRight(1)
      val receiverId = AccessPathIdentifier(receiverPath)(refType)
      val field = VariableIdentifier(left.path.last)(left.typ)

      result = result.toCondHeapGraph.evalExp(receiverId).mapCondHeaps(condHeap => {
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

  def prunePredIds(): PredicateDrivenHeapState[S] = {
    copy(
      generalValState = generalValState.removeVariables(generalValState.predHeapIds),
      abstractHeap = abstractHeap.mapEdgeStates(state => {
        state.removeVariables(generalValState.predHeapIds ++ state.sourceEdgeLocalIds.filter(_.typ == PredType))
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

  implicit class ExtendedEdgeStateDomain[S <: SemanticDomain[S]](state: EdgeStateDomain[S]) {
    def predHeapIds: Set[ValueHeapIdentifier] =
      state.valueHeapIds.filter(_.typ == PredType)

    def predHeapIds(vertex: Vertex): Set[ValueHeapIdentifier] =
      state.valueHeapIds(vertex).filter(_.typ == PredType)

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
