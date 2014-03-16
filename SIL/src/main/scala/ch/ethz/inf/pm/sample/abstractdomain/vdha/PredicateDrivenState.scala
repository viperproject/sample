package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.ToStringUtilities
import ch.ethz.inf.pm.sample.oorepresentation.sil.{BoolType, PredType}
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDrivenHeapState.EdgeStateDomain
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import com.weiglewilczek.slf4s.Logging
import ch.ethz.inf.pm.sample.abstractdomain.vdha._
import scala.Some
import scala.Some
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.vdha.GhostOpCollector
import ch.ethz.inf.pm.sample.abstractdomain.vdha.NestedPredicatesDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateInstancesDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.FoldGhostOpEvent
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicatesDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredMergeGhostOpEvent
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateInstanceDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.Edge
import ch.ethz.inf.pm.sample.abstractdomain.vdha.SummaryHeapVertex
import ch.ethz.inf.pm.sample.abstractdomain.vdha.UnfoldGhostOpEvent
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateBody
import ch.ethz.inf.pm.sample.abstractdomain.vdha.VertexExpression
import ch.ethz.inf.pm.sample.abstractdomain.vdha.ValueHeapIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.vdha.SemanticAndPredicateDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.HeapGraph
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.vdha.GhostOpCollector
import ch.ethz.inf.pm.sample.abstractdomain.vdha.NestedPredicatesDomain
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDomain
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateInstancesDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.FoldGhostOpEvent
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicatesDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredMergeGhostOpEvent
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateInstanceDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.Edge
import ch.ethz.inf.pm.sample.abstractdomain.vdha.SummaryHeapVertex
import ch.ethz.inf.pm.sample.abstractdomain.vdha.UnfoldGhostOpEvent
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateBody
import ch.ethz.inf.pm.sample.abstractdomain.vdha.VertexExpression
import ch.ethz.inf.pm.sample.abstractdomain.vdha.ValueHeapIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.vdha.SemanticAndPredicateDomain
import ch.ethz.inf.pm.sample.abstractdomain.vdha.HeapGraph

case class PredicateDrivenHeapState[S <: SemanticDomain[S]](
    abstractHeap: HeapGraph[EdgeStateDomain[S]],
    generalValState: EdgeStateDomain[S],
    expr: ExpressionSet,
    isTop: Boolean = false,
    // The subscribers is immutable like the rest of the state.
    // Subscribing results in a new state while the old state is unchanged.
    // A ghost operation collector is added by default.
    ghostOpSubscribers: Seq[GhostOpSubscriber[S]] = Seq(GhostOpCollector[S]()))
  extends PreciseValueDrivenHeapState[
    SemanticAndPredicateDomain[S],
    PredicateDrivenHeapState[S]]
  with Logging {

  // Shorthand for the self-type
  type T = PredicateDrivenHeapState[S]

  import PredicateInstanceState.{Folded, Unfolded, Top}
  import PredicateDrivenHeapState._

  /** Copies the state and adds a new ghost operation subscriber
    * to the new state.
    */
  def subscribe(sub: GhostOpSubscriber[S]): T = {
    PredicateDrivenHeapState(
      abstractHeap,
      generalValState,
      expr,
      isTop,
      ghostOpSubscribers :+ sub)
  }

  /** Publishes a ghost operation event to all subscribers. */
  def publish(event: GhostOpEvent) {
    ghostOpSubscribers.foreach(_.notify(this, event))
  }

  def factory(
      abstractHeap: HeapGraph[EdgeStateDomain[S]],
      generalValState: EdgeStateDomain[S],
      expr: ExpressionSet,
      isTop: Boolean) =
    // TODO: Just copying the ghost operation subscribers here is not elegant
    // There is a danger of losing subscribers when joining heaps
    // Ideally, one would use a set domain of subscribers.
    PredicateDrivenHeapState(abstractHeap, generalValState, expr, isTop, ghostOpSubscribers)

  /** Returns the IDs of predicates for which an instance label of the
    * given type appears on all out-going, non-null edges of a
    * given local variable vertex.
    *
    * If there are no such edges, the result is an empty set.
    */
  def certainInstIds(
      localVarVertex: LocalVariableVertex,
      predInstState: PredicateInstanceState): Set[PredicateInstanceIdentifier] = {
    val recvEdges = abstractHeap.outEdges(localVarVertex)
    certainInstIds(recvEdges, predInstState)
  }

  /** Returns the IDs of predicates for which an instance label of the
    * given type appears on all given, non-null edges.
    */
  def certainInstIds(
      edges: Set[Edge[EdgeStateDomain[S]]],
       predInstState: PredicateInstanceState): Set[PredicateInstanceIdentifier] = {
    val nonNullEdges = edges.filterNot(_.target == NullVertex)
    if (nonNullEdges.isEmpty) Set.empty
    else {
      val state = Lattice.bigLub(nonNullEdges.map(_.state))
      state.predInsts.instIds(predInstState)
    }
  }

  /** Sets the state of a predicate instance for a given access path.
    * Leaves null edges untouched.
    */
  def assignPredicateInstanceState(
      path: List[Identifier],
      predId: PredicateInstanceIdentifier,
      state: PredicateInstanceState): T = {
    val accessPathId = AccessPathIdentifier(path, predId)
    evalExp(accessPathId, allowNullReceivers = true).mapCondHeaps(condHeap => {
      val takenPath = condHeap.takenPath(accessPathId.objPath)
      if (takenPath.target == NullVertex) Seq(condHeap)
      else condHeap.assignField(accessPathId, state)
    }).join
  }

  def assumePredicateInstanceState(
      path: List[Identifier],
      predId: PredicateInstanceIdentifier,
      state: PredicateInstanceState): T = {
    val accessPathId = AccessPathIdentifier(path, predId)
    evalExp(accessPathId, allowNullReceivers = true).mapCondHeaps(condHeap => {
      val takenPath = condHeap.takenPath(accessPathId.objPath)
      if (takenPath.target == NullVertex) Seq(condHeap)
      else {
        val takenEdge = takenPath.edges.last
        val vertexToUpdate = takenPath.target
        Seq(condHeap.mapEdges(edge => {
          var newState = edge.state
          if (takenEdge == edge || (edge.target == vertexToUpdate && vertexToUpdate.isInstanceOf[DefiniteHeapVertex])) {
            val edgeLocId = EdgeLocalIdentifier(List(edge.field), predId)
            val expr = BinaryArithmeticExpression(edgeLocId, state, ArithmeticOperator.==, BoolType)
            newState = newState.assume(expr)
          }
          newState
        }))
      }
    }).join
  }

  /** Translates a field access path identifier into the corresponding
    * local variable vertex and the field identifier.
    */
  def splitAccessPathIdentifier(id: AccessPathIdentifier):
      (LocalVariableVertex, Identifier) = {
    require(id.path.size == 2, "currently only support obj.field")

    val localVarName = id.path.head.getName
    val field = id.path.last
    val localVarVertex = abstractHeap.localVarVertex(localVarName)

    (localVarVertex, field)
  }


  override def createVariableForArgument(variable: VariableIdentifier, typ: Type) = {
    val result = super.createVariableForArgument(variable, typ)

    if (variable.typ.isObject)
      result.assumePredicatesForArguments().setExpression(result.expr)
    else
      result
  }

  def assumePredicatesForArguments(): T = {
    PredicateIdentifier.reset()

    // Create a fresh predicate for each parameter edge
    val edgeVerticesToPredId = abstractHeap.localVarEdges.map(edge => {
      val predId = PredicateIdentifier.make()
      Set(edge.source, edge.target) -> predId
    }).toMap

    val predIds = edgeVerticesToPredId.values

    // First create definitions for the given IDs in all edge states,
    // then put Folded labels on the individual edges
    createNonObjectVariables(predIds.toSet).mapEdges(edge => {
      edgeVerticesToPredId.get(edge.vertices) match {
        case Some(predId) =>
          val predInstId = PredicateInstanceIdentifier.make(predId)
          val edgeLocalPredInstId = EdgeLocalIdentifier(List(edge.field), predInstId)
          edge.state.assign(edgeLocalPredInstId, Folded)
        case None => edge.state
      }
    })
  }

  override def getFieldValue(id: AccessPathIdentifier): T = {
    val (localVarVertex, field) = splitAccessPathIdentifier(id)

    // Only materialize the receiver of the field access at this point
    // It's too early to also materialize the target of the field access.
    // Unfolding after materializing the receiver may cause some edges
    // going out of the receiver vertex to be removed, so we don't need
    // to follow them when materializing the target of the field access.
    var result = materializePath(List(localVarVertex.name))

    val recvEdges = result.abstractHeap.outEdges(localVarVertex)
    val nonNullRecvEdges = recvEdges.filterNot(_.target == NullVertex)
    val nonNullRecvVertices = nonNullRecvEdges.map(_.target)

    assert(nonNullRecvEdges.forall(!_.target.isInstanceOf[SummaryHeapVertex]),
      "edge target must not be summary heap vertex, is materialization on?")

    val preds = generalValState.preds
    val foldedInstIds = certainInstIds(localVarVertex, Folded)
    val unfoldedInstIds = certainInstIds(localVarVertex, Unfolded)

    val foldedInstIdsWithPerm = foldedInstIds.filter(instId => preds.get(instId.predId).hasPerm(field))
    val unfoldedInstIdsWithPerm = unfoldedInstIds.filter(instId => preds.get(instId.predId).hasPerm(field))
    val foldedAndUnfoldedInstIds = foldedInstIds ++ unfoldedInstIds

    if (foldedAndUnfoldedInstIds.isEmpty) {
      logger.warn(s"Encountered field access $id without any folded or " +
        "unfolded predicate instance candidate on the receiver edges")
      return bottom()
    }

    if (unfoldedInstIdsWithPerm.isEmpty) {
      val (recvPredInstId, wasFolded, hasPerm) = foldedInstIdsWithPerm.toList match {
        case foldedId :: Nil => (foldedId, true, true)
        case Nil => {
          if (!unfoldedInstIds.isEmpty) (unfoldedInstIds.head, false, false)
          else (foldedInstIds.head, true, false)
        }
        case _ =>
          logger.error(s"Multiple folded predicate instances " +
            s"$foldedInstIdsWithPerm contain full permission to field $field")
          sys.exit(-1)
      }

      var recvPredBody = preds.get(recvPredInstId.predId)

      // Unfold
      result = result.assignPredicateInstanceState(
        List(localVarVertex.variable), recvPredInstId, Unfolded)

      // Add permission if necessary
      if (!hasPerm) {
        if (id.typ.isObject) {
          val fieldEdges = nonNullRecvVertices.flatMap(
            result.abstractHeap.outEdges(_, Some(field.getName)))

          if (fieldEdges.exists(_.target != NullVertex)) {
            val nestedPredId = PredicateIdentifier.make()
            val nestedPredBody = PredicateBody().top()
            result = result.assignVariable(nestedPredId, nestedPredBody)
            recvPredBody = recvPredBody.add(field, NestedPredicatesDomain(Set(nestedPredId), isTop = false))
          } else {
            recvPredBody = recvPredBody.addPerm(field)
          }
        } else {
          recvPredBody = recvPredBody.addPerm(field)
        }

        // Assign the new predicate definition
        result = result.assignVariable(recvPredInstId.predId, recvPredBody)
      }

      // Add assume that we have a folded instance of each nested predicate
      for ((f, nestedPredId) <- recvPredBody.nestedPredIdMap) {
        val instId = new PredicateInstanceIdentifier(nestedPredId, recvPredInstId.version)
        result = result.assumePredicateInstanceState(
          List(localVarVertex.variable, f), instId, Folded)
      }

      if (wasFolded) {
        // Let subscribers of ghost operations know about the unfold
        result.publish(UnfoldGhostOpEvent(
          localVarVertex.variable, recvPredInstId.predId))
      }
    }

    recvEdges.filter(_.target == NullVertex).toList match {
      case nullRecvEdge :: Nil =>
        for (foldedPredId <- nullRecvEdge.state.predInsts.foldedIds) {
          val bottom = PredicateBody().bottom()
          result = result.assignVariable(foldedPredId, bottom)
        }
      case Nil =>
    }

    result = result.prune()

    // Materialize the target of the field access now
    result.materializePath(id.objPath).copy(expr = ExpressionSet(id))
  }

  override def assignVariable(left: Expression, right: Expression) = {
    val result = super.assignVariable(left, right)

    (left, right) match {
      case (left: Identifier, VertexExpression(_, vertex: DefiniteHeapVertex)) =>
        // A vertex expression for a definite heap vertex only occurs
        // as the right-hand side of a variable assignment if that vertex
        // was just allocated

        // In this case, create a fresh predicate and a fresh unfolded instance
        // of that predicate for the newly allocated object
        val predId = PredicateIdentifier.make()
        val predInstId = PredicateInstanceIdentifier.make(predId)

        result
          .createNonObjectVariables(Set(predId))
          .assignPredicateInstanceState(List(left), predInstId, Unfolded)
      case _ =>
        result
    }
  }

  override def assignField(left: AccessPathIdentifier, right: Expression): T = {
    var result = super.assignField(left, right).prunePredIds()

    if (left.typ.isObject) {
      val (localVarVertex, field) = splitAccessPathIdentifier(left)
      val unfoldedRecvInstIds = certainInstIds(localVarVertex, Unfolded)

      val paths = result.abstractHeap.paths(left.stringPath).filter(_.target != NullVertex)
      val fieldEdges = paths.map(_.edges.last)
      val newNestedInstIds = certainInstIds(fieldEdges, Folded)

      if (!unfoldedRecvInstIds.isEmpty && !newNestedInstIds.isEmpty) {
        assert(unfoldedRecvInstIds.size == 1,
          "there must be at most one unfolded ID")
        assert(newNestedInstIds.size == 1,
          "there must be at most one new nested ID")

        val recvInstId = unfoldedRecvInstIds.head
        val nestedInstId = newNestedInstIds.head

        result = result.map(state => {
          state.transformPreds(preds => {
            preds.add(recvInstId.predId, preds.get(recvInstId.predId).addPerm(field, nestedInstId.predId))
          })
        })
      }
    }

    result
  }

  def tryToFoldAllLocalVars(): PredicateDrivenHeapState[S] = {
    var result = this

    abstractHeap.localVarVertices.foreach(localVarVertex => {
      // Only fold local variables if it is possible to do so
      // on all local variable edges
      var unfoldedPredInstIds = result.certainInstIds(localVarVertex, Unfolded)

      val foldedPredIds = result.certainInstIds(localVarVertex, Folded).map(_.predId)
      unfoldedPredInstIds = unfoldedPredInstIds.filterNot(id => foldedPredIds.contains(id.predId))

      unfoldedPredInstIds.foreach(unfoldedPredInstId => {
        var candidateResult = result
        var canFold = true

        if (result.abstractHeap.outEdges(localVarVertex).count(_.target != NullVertex) > 1) {
          logger.info(s"Not folding $unfoldedPredInstId($localVarVertex) " +
            "since there are multiple non-null receiver edges")
          canFold = false
        }

        val unfoldedPredBody = result.generalValState.preds.get(unfoldedPredInstId.predId)

        val newFoldedPredInstId = PredicateInstanceIdentifier.make(unfoldedPredInstId.predId) // Create fresh version
        candidateResult = candidateResult.assignPredicateInstanceState(
          List(localVarVertex.variable), newFoldedPredInstId, Folded)

        for ((field, nestedPredId) <- unfoldedPredBody.nestedPredIdMap) {
          val path = List(localVarVertex.variable, field)
          val paths = result.abstractHeap.paths(path.map(_.getName)).filter(_.target != NullVertex)
          val fieldEdges = paths.map(_.edges.last)

          // No need for any folded labels if there are only null edges
          if (!fieldEdges.isEmpty) {
            val presentFoldedInstIds = certainInstIds(fieldEdges, Folded)

            presentFoldedInstIds.find(_.predId == nestedPredId) match {
              case Some(presentFoldedInstId) =>
                // TODO: Could maybe also use setToTop
                // val nestedPredInstId = PredicateInstanceIdentifier()
                candidateResult = candidateResult.assignPredicateInstanceState(path, presentFoldedInstId, Top)
              case None => canFold = false
            }
          }
        }

        if (canFold) {
          abstractHeap.localVarVertices.foreach(localVarVertex => {
            def hasPredInstOnEveryEdge(state: PredicateDrivenHeapState[S]): Boolean = {
              val heap = state.abstractHeap
              val nonNullLocalVarEdges = heap.outEdges(localVarVertex).filter(_.target != NullVertex)
              if (nonNullLocalVarEdges.isEmpty) true
              else {
                val state = Lattice.bigLub(nonNullLocalVarEdges.map(_.state))
                !state.predInsts.foldedAndUnfoldedIds.isEmpty
              }
            }

            if (hasPredInstOnEveryEdge(this)) {
              if (!hasPredInstOnEveryEdge(candidateResult)) {
                logger.info(s"Not folding $unfoldedPredInstId($localVarVertex) " +
                  "to avoid loss of permissions for some local variable")
                canFold = false
              }
            }
          })
        }

        if (canFold) {
          result = candidateResult

          // Let subscribers know about the fold operation
          result.publish(FoldGhostOpEvent(
            localVarVertex.variable, unfoldedPredInstId.predId))
        }
      })
    })

    result.prunePredIds()
  }

  def findPredicateIdMerges(other: T): Replacement = {
    val repl = new Replacement()
    for (localVarVertex <- abstractHeap.localVarVertices) {
      val thisFoldedIds = certainInstIds(localVarVertex, Folded).map(_.predId)
      val otherFoldedIds = other.certainInstIds(localVarVertex, Folded).map(_.predId)

      if (!thisFoldedIds.isEmpty &&
        !otherFoldedIds.isEmpty &&
        thisFoldedIds.intersect(otherFoldedIds).isEmpty) {
        assert(thisFoldedIds.size == 1,
          "cannot handle more than one folded predicate instance")
        assert(otherFoldedIds.size == 1,
          "cannot handle more than one folded predicate instance")

        val thisFoldedId = thisFoldedIds.head
        val otherFoldedId = otherFoldedIds.head

        // TODO: Should compare predicate integer ID
        if (thisFoldedId.getName >= otherFoldedId.getName) {
          repl.value += (Set[Identifier](thisFoldedId, otherFoldedId) -> Set[Identifier](otherFoldedId))
        } else {
          repl.value += (Set[Identifier](thisFoldedId, otherFoldedId) -> Set[Identifier](thisFoldedId))
        }
      }
    }
    repl
  }

  def findPredicateInstIdMerges(other: T): Replacement = {
    val repl = new Replacement()
    for (localVarVertex <- abstractHeap.localVarVertices) {
      val thisFoldedInstIds = certainInstIds(localVarVertex, Folded)
      val otherFoldedInstIds = other.certainInstIds(localVarVertex, Folded)

      if (!thisFoldedInstIds.isEmpty &&
        !otherFoldedInstIds.isEmpty &&
        thisFoldedInstIds.intersect(otherFoldedInstIds).isEmpty) {
        assert(thisFoldedInstIds.size == 1,
          "cannot handle more than one folded predicate instance")
        assert(otherFoldedInstIds.size == 1,
          "cannot handle more than one folded predicate instance")

        val thisFoldedInstId = thisFoldedInstIds.head
        val otherFoldedInstId = otherFoldedInstIds.head

        if (thisFoldedInstId.predId == otherFoldedInstId.predId) {
          if (thisFoldedInstId.version <= otherFoldedInstId.version) {
            repl.value += (Set[Identifier](thisFoldedInstId, otherFoldedInstId) -> Set[Identifier](otherFoldedInstId))
          } else {
            repl.value += (Set[Identifier](thisFoldedInstId, otherFoldedInstId) -> Set[Identifier](thisFoldedInstId))
          }
        }
      }
    }
    repl
  }

  def applyPredicateMerge(repl: Replacement): T = {
    map(_.merge(repl)).mapEdges(edge => {
      val edgeLocalRepl = new Replacement()

      for ((fromSet, toSet) <- repl.value) {
        val fromInstSet = edge.state.predInsts.foldedInstIds.filter(instId => fromSet.contains(instId.predId))

        // When we merge a predicate ID for which we have neither a
        // folded nor unfolded label on the edge, it should not cause the the
        // target predicate instance to be top
        // TODO: It should not be necessary to do so
        // newFromSet = newFromSet.toSet[Identifier] intersect edge.state.ids
        if (!fromInstSet.isEmpty) {
          val minVersion = fromInstSet.map(_.version).min
          val toInstSet = toSet.map(to => new PredicateInstanceIdentifier(to.asInstanceOf[PredicateIdentifier], minVersion))

          val fromEdgeLocalSet = fromInstSet.map(EdgeLocalIdentifier(List(edge.field), _))
          val toEdgeLocalSet = toInstSet.map(EdgeLocalIdentifier(List(edge.field), _))

          edgeLocalRepl.value += (fromEdgeLocalSet.toSet[Identifier] -> toEdgeLocalSet.toSet[Identifier])
        }
      }
      edge.state.merge(edgeLocalRepl)
    })
  }

  def applyPredicateInstMerge(repl: Replacement): T = {
    mapEdges(edge => {
      val edgeLocalRepl = new Replacement()

      for ((fromSet, toSet) <- repl.value) {
        val filteredFromSet = edge.state.predInsts.foldedInstIds.filter(instId => fromSet.contains(instId))

        // When we merge a predicate ID for which we have neither a
        // folded nor unfolded label on the edge, it should not cause the the
        // target predicate instance to be top
        // TODO: It should not be necessary to do so
        // newFromSet = newFromSet.toSet[Identifier] intersect edge.state.ids
        if (!filteredFromSet.isEmpty) {

          val fromEdgeLocalSet = filteredFromSet.map(EdgeLocalIdentifier(List(edge.field), _))
          val toEdgeLocalSet = toSet.map(EdgeLocalIdentifier(List(edge.field), _))

          edgeLocalRepl.value += (fromEdgeLocalSet.toSet[Identifier] -> toEdgeLocalSet.toSet[Identifier])
        }
      }
      edge.state.merge(edgeLocalRepl)
    })
  }

  override def lub(other: PredicateDrivenHeapState[S]): PredicateDrivenHeapState[S] = {
    if (isBottom || other.isTop)
      return other
    if (isTop || other.isBottom)
      return this

    // Fold as much as possible before joining
    var thisFolded = tryToFoldAllLocalVars()
    var otherFolded = other.tryToFoldAllLocalVars()

    val predIdRepl = thisFolded.findPredicateIdMerges(otherFolded)

    if (!predIdRepl.isEmpty) {
      thisFolded = thisFolded.applyPredicateMerge(predIdRepl)
      otherFolded = otherFolded.applyPredicateMerge(predIdRepl)
    }

    val predInstIdRepl = thisFolded.findPredicateInstIdMerges(otherFolded)

    if (!predInstIdRepl.isEmpty) {
      thisFolded = thisFolded.applyPredicateInstMerge(predInstIdRepl)
      otherFolded = otherFolded.applyPredicateInstMerge(predInstIdRepl)
    }

    val (resAH, renameMap) = thisFolded.abstractHeap.lub(otherFolded.abstractHeap)
    val valueRenameMap = Vertex.vertexMapToValueHeapIdMap(renameMap)
    val resGeneralState = thisFolded.generalValState.lub(otherFolded.generalValState.rename(valueRenameMap.toMap))

    val result = factory(resAH, resGeneralState, ExpressionSet())

    if (!predIdRepl.isEmpty) {
      result.publish(PredMergeGhostOpEvent(predIdRepl))
    }

    result
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

  implicit class ExtendedEdgeStateDomain[S <: SemanticDomain[S]](state: EdgeStateDomain[S]) {
    def predHeapIds: Set[ValueHeapIdentifier] =
      state.valueHeapIds.filter(_.typ == PredType)

    def predHeapIds(vertex: Vertex): Set[ValueHeapIdentifier] =
      state.valueHeapIds(vertex).filter(_.typ == PredType)

    def predInsts: PredicateInstancesDomain =
      state.valueState.predicateState.instances

    def preds: PredicatesDomain =
      state.valueState.predicateState.predicates

    def transformPredState(f: PredicateDomain => PredicateDomain): EdgeStateDomain[S] = {
      state.copy(
        valueState = state.valueState.copy(
          predicateState = f(state.valueState.predicateState)))
    }

    def transformPredInsts(f: PredicateInstancesDomain => PredicateInstancesDomain): EdgeStateDomain[S] =
      transformPredState(predState => {
        predState.copy(instances = f(predState.instances))
      })

    def transformPreds(f: PredicatesDomain => PredicatesDomain): EdgeStateDomain[S] =
      transformPredState(predState => {
        predState.copy(predicates = f(predState.predicates))
      })
  }
}

case class PredicateDomain(
    instances: PredicateInstancesDomain = PredicateInstancesDomain(),
    predicates: PredicatesDomain = PredicatesDomain())
  extends RoutingSemanticCartesianProductDomain[
    PredicateInstancesDomain,
    PredicatesDomain,
    PredicateDomain] {

  def factory(i: PredicateInstancesDomain, d: PredicatesDomain) =
    PredicateDomain(i, d)

  def _1 = instances

  def _1canHandle(id: Identifier) =
    !_2canHandle(id)

  def _2 = predicates

  def _2canHandle(id: Identifier) =
    id.isInstanceOf[PredicateIdentifier]

  override def toString =
    "Instances:\n" + ToStringUtilities.indent(instances.toString) + "\n" +
    "Predicates:\n" + ToStringUtilities.indent(predicates.toString)
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

/** When applying a condition without edge-local predicate instance
  * state labels to an edge, the predicate instance states would be set to
  * bottom, because the calls to createVariables initialize predicate states to top.
  * Thus, set them to bottom here so we don't lose permissions.
  */
object CustomGlbPreservingIdsStrategy extends GlbPreservingIdsStrategy {
  import PredicateDrivenHeapState._

  def apply[S <: SemanticDomain[S]](left: S, right: S): S = {
    // Create all non-predicate instance variables that are missing
    val newRightIds = (left.edgeLocalAndAccessPathIds diff right.ids).filterNot(_.typ == PredType)
    val newLeftIds = (right.edgeLocalAndAccessPathIds diff left.ids).filterNot(_.typ == PredType)

    // Problem: Just creating all missing predicate instance variables
    // and setting them to bottom does not work, because the resulting
    // predicate instances domain as a whole will be bottom as a result.

    var newLeft = left.createVariables(newLeftIds)
    var newRight = right.createVariables(newRightIds)

    // Temporarily set the default value of instance types to bottom
    // so we don't lose permission when applying a condition to an edge
    def setDefaultValue[I <: SemanticDomain[I]](
        state: EdgeStateDomain[I],
        defaultValue: PredicateInstanceDomain): EdgeStateDomain[I] = {
      state.transformPredInsts(insts => {
        // Also call functionalFactory so it ensures that isTop is not true anymore
        insts.copy(defaultValue = defaultValue).functionalFactory(insts.map, insts.isBottom, insts.isTop)
      })
    }

    newLeft = setDefaultValue(newLeft.asInstanceOf[EdgeStateDomain[ApronInterface.Default]],
      PredicateInstanceDomain().bottom()).asInstanceOf[S]
    newRight = setDefaultValue(newRight.asInstanceOf[EdgeStateDomain[ApronInterface.Default]],
      PredicateInstanceDomain().bottom()).asInstanceOf[S]

    val result = newLeft.glb(newRight)

    // Det the default value back to top.
    val newResult = setDefaultValue(result.asInstanceOf[EdgeStateDomain[ApronInterface.Default]],
      PredicateInstanceDomain().top()).asInstanceOf[S]

    newResult
  }
}