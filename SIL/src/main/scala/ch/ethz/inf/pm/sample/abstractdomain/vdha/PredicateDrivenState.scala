package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.ToStringUtilities
import ch.ethz.inf.pm.sample.oorepresentation.sil.{BoolType, PredType}
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PredicateDrivenHeapState.EdgeStateDomain
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import com.weiglewilczek.slf4s.Logging
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier

case class PredicateDrivenHeapState[S <: SemanticDomain[S]](
    abstractHeap: HeapGraph[EdgeStateDomain[S]],
    generalValState: EdgeStateDomain[S],
    expr: ExpressionSet,
    isTop: Boolean = false,
    // The subscribers are immutable like the rest of the state.
    // Subscribing results in a new state while the old state is unchanged.
    // A ghost operation collector is added by default.
    ghostOpSubscribers: Seq[GhostOpSubscriber[S]] = Seq(GhostOpCollector[S]()))
  extends PreciseValueDrivenHeapState[
    SemanticAndPredicateDomain[S],
    PredicateDrivenHeapState[S]]
  with Logging {

  // Shorthand for the self-type
  type T = PredicateDrivenHeapState[S]

  import PredicateInstanceState.{Folded, Unfolded}
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
    * given local variable vertex, with the same version number.
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
    * given type appears on all given, non-null edges, with the same version number.
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

  /** Returns the IDs of all predicates for which an instance
    * with any version number appears on every non-null edges of the
    * given local variable vertex.
    */
  def certainIds(
      localVarVertex: LocalVariableVertex,
      instState: PredicateInstanceState): Set[PredicateIdentifier] = {
    val recvEdges = abstractHeap.outEdges(localVarVertex)
    certainIds(recvEdges, instState)
  }

  /** Returns the IDs of all predicates for which an instance
    * with any version number appears on every given edges whose
    * target is not the null vertex.
    */
  def certainIds(
      edges: Set[Edge[EdgeStateDomain[S]]],
      instState: PredicateInstanceState): Set[PredicateIdentifier] = {
    val nonNullEdges = edges.filterNot(_.target == NullVertex)
    if (nonNullEdges.isEmpty) Set.empty
    else {
      val predIdsPerEdge = nonNullEdges.map(_.state.predInsts.ids(instState))
      predIdsPerEdge.reduceLeft(_ intersect _)
    }
  }

  /** Sets the state of a predicate instance for a given access path.
    * Leaves null edges untouched.
    */
  def assignPredicateInstanceState(
      path: List[Identifier],
      predId: PredicateInstanceIdentifier,
      state: PredicateInstanceState): T = {
    logger.debug(s"Assigning $state to $predId(${path.mkString(".")})")

    val accessPathId = AccessPathIdentifier(path, predId)

    evalExp(accessPathId, allowNullReceivers = true).mapCondHeaps(condHeap => {
      val takenPath = condHeap.takenPath(accessPathId.objPath)
      if (takenPath.target == NullVertex) Seq(condHeap)
      else condHeap.assignField(accessPathId, state)
    }).join
  }

  def removePredicateInstanceState(
      path: List[Identifier],
      predId: PredicateIdentifier,
      state: PredicateInstanceState): T = {
    logger.debug(s"Assuming $state for $predId(${path.mkString(".")})")

    val verticesToUpdate = abstractHeap.paths(path.map(_.getName)).map(_.target)
    mapEdges(edge => {
      if (verticesToUpdate.contains(edge.target)) {
        val instIds = edge.state.predInsts.instIds(state).filter(_.predId == predId)
        val edgeLocInstIds = instIds.map(EdgeLocalIdentifier(List(edge.field), _))
        edge.state.removeVariables(edgeLocInstIds)
      } else edge.state
    })
  }

  def assumePredicateInstanceState(
      path: List[Identifier],
      predId: PredicateInstanceIdentifier,
      state: PredicateInstanceState): T = {
    logger.debug(s"Assuming $state for $predId(${path.mkString(".")})")

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

    val preds = generalValState.predDefs
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

      logger.info(s"Unfolding $recvPredInstId($localVarVertex)")

      // Unfold
      result = result.assignPredicateInstanceState(
        List(localVarVertex.variable), recvPredInstId, Unfolded)

      // Add permission if necessary
      if (!hasPerm) {
        if (id.typ.isObject) {
          val nestedPredId = PredicateIdentifier.make()
          val nestedPredBody = PredicateBody().top()
          result = result.assignVariable(nestedPredId, nestedPredBody)
          recvPredBody = recvPredBody.add(field, NestedPredicatesDomain(Set(nestedPredId), isTop = false))
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
      val unfoldedIds = certainIds(localVarVertex, Unfolded)
      val unfoldedIdsWithPerm = unfoldedIds.filter(id => generalValState.predDefs.get(id).hasPerm(field))

      if (!unfoldedIdsWithPerm.isEmpty) {
        assert(unfoldedIdsWithPerm.size == 1,
          s"there must be at most one unfolded predicate instance " +
          s"for $localVarVertex with permission to field $field")

        val unfoldedId = unfoldedIdsWithPerm.head
        val unfoldedPredBody = generalValState.predDefs.get(unfoldedId)
        val existingNestedId = unfoldedPredBody.get(field).value.head

        val paths = result.abstractHeap.paths(left.stringPath).filter(_.target != NullVertex)
        val fieldEdges = paths.map(_.edges.last)
        val newNestedIds = certainIds(fieldEdges, Folded)

        if (!newNestedIds.isEmpty) {
          val merge = PredicateIdentifierMerge(Set(existingNestedId) ++ newNestedIds)
          result = result.mergePredicates(merge)
        }
      } else {
        logger.warn(s"Assigning field $left without any " +
          "predicate instance with permission")
      }
    }

    result
  }

  def tryToFoldLocalVar(localVarVertex: LocalVariableVertex, predId: PredicateIdentifier): T = {
    require(certainIds(localVarVertex, Unfolded).contains(predId),
      s"can only fold if there is an unfolded instance of $predId " +
      s"on every non-null edges going out of $localVarVertex")

    if (abstractHeap.outEdges(localVarVertex).count(_.target != NullVertex) > 1) {
      logger.info(s"Not folding $predId($localVarVertex) " +
        "since there are multiple non-null receiver edges")
      return this
    }

    var result = this
    val predBody = generalValState.predDefs.get(predId)

    // Create predicate instance identifier with fresh version
    val newPredInstId = PredicateInstanceIdentifier.make(predId)

    result = result.assignPredicateInstanceState(
      List(localVarVertex.variable), newPredInstId, Folded)

    for ((field, nestedPredId) <- predBody.nestedPredIdMap) {
      val path = List(localVarVertex.variable, field)
      val paths = result.abstractHeap.paths(path.map(_.getName)).filter(_.target != NullVertex)
      val fieldEdges = paths.map(_.edges.last)

      // No need for any folded labels if there are only null edges
      if (!fieldEdges.isEmpty) {
        val presentFoldedIds = certainIds(fieldEdges, Folded)

        if (presentFoldedIds.contains(nestedPredId)) {
          result = result.removePredicateInstanceState(path, nestedPredId, Folded)
        } else {
          logger.info(s"Not folding $predId($localVarVertex) " +
            s"since nested predicate instance $nestedPredId may be missing")
          return this
        }
      }
    }

    abstractHeap.localVarVertices.foreach(innerLocalVarVertex => {
      def hasPredInstOnEveryEdge(state: PredicateDrivenHeapState[S]): Boolean = {
        val heap = state.abstractHeap
        val nonNullLocalVarEdges = heap.outEdges(innerLocalVarVertex).filter(_.target != NullVertex)
        if (nonNullLocalVarEdges.isEmpty) true
        else {
          val state = Lattice.bigLub(nonNullLocalVarEdges.map(_.state))
          !state.predInsts.foldedIds.isEmpty
        }
      }

      if (hasPredInstOnEveryEdge(this)) {
        if (!hasPredInstOnEveryEdge(result)) {
          logger.info(s"Not folding $predId($localVarVertex) " +
            s"to avoid loss of permissions for $innerLocalVarVertex")
          return this
        }
      }
    })

    result = result.prunePredIds()

    logger.info(s"Folding $predId($localVarVertex)")

    // Let subscribers know about the fold operation
    result.publish(FoldGhostOpEvent(localVarVertex.variable, predId))

    result
  }

  def tryToFoldLocalVar(localVarVertex: LocalVariableVertex): T = {
    // Only fold local variables if it is possible to do so
    // on all local variable edges
    val unfoldedPredIds = certainIds(localVarVertex, Unfolded)
    val foldedPredIds = certainIds(localVarVertex, Folded)

    var result = this

    for (candidateUnfoldedPredId <- unfoldedPredIds -- foldedPredIds) {
      result = result.tryToFoldLocalVar(localVarVertex, candidateUnfoldedPredId)
    }
    result
  }

  def tryToFoldAllLocalVars(): T = {
    var result = prunePredIds()

    for (localVarVertex <- abstractHeap.localVarVertices) {
      result = result.tryToFoldLocalVar(localVarVertex)
    }
    result.prunePredIds()
  }

  def findPredicateIdMerge(other: T, localVarVertex: LocalVariableVertex): Option[PredicateIdentifierMerge] = {
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

      Some(PredicateIdentifierMerge(Set(thisFoldedId, otherFoldedId)))
    } else None
  }

  def mergePredicates(predIdMerge: PredicateIdentifierMerge): T = {
    if (predIdMerge.predIds.size == 1) return this // Nothing to do

    logger.info(s"Merging predicate IDs ${predIdMerge.predIds}")

    var result = map(_.transformPredDefs(_.merge(predIdMerge))).mapEdges(edge => {
      val edgeLocalRepl = new Replacement()
      val repl = predIdMerge.toReplacement

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

    // Recursively perform a merge for all nested predicate IDs
    result.generalValState.predDefs.requiredIdMergeOption match {
      case Some(requiredIdMerge) =>
        result = result.mergePredicates(requiredIdMerge)
      case None =>
    }

    result
  }

  override def lub(other: PredicateDrivenHeapState[S]): PredicateDrivenHeapState[S] = {
    if (isBottom || other.isTop)
      return other
    if (isTop || other.isBottom)
      return this

    // Fold as much as possible before joining
    var thisFolded = tryToFoldAllLocalVars()
    var otherFolded = other.tryToFoldAllLocalVars()

    thisFolded.abstractHeap.localVarVertices.foreach(localVarVertex => {
      thisFolded.findPredicateIdMerge(otherFolded, localVarVertex) match {
        case Some(predIdMerge) =>
          thisFolded = thisFolded.mergePredicates(predIdMerge)
          otherFolded = otherFolded.mergePredicates(predIdMerge)

          thisFolded.publish(PredicateIdentifierMergeEvent(predIdMerge))
        case None =>
      }
    })

    val iso = thisFolded.abstractHeap.mcs(otherFolded.abstractHeap).vertexMap
    var (resultAH, renameMap) = thisFolded.abstractHeap.minCommonSuperGraphBeforeJoin(otherFolded.abstractHeap, iso)

    val newEdges = resultAH.weakEdgeEquivalenceSets.flatMap(edges => {
      if (edges.size == 1) {
        edges
      } else {
        assert(edges.size == 2, "there should not be more than two weakly-equal edges")
        var edge = edges.head
        var otherEdge = edges.tail.head

        val instIds = edge.state.predInsts.foldedInstIds
        val otherInstIds = otherEdge.state.predInsts.foldedInstIds

        assert(instIds.size <= 1, "cannot handle more than one folded pred inst id")
        assert(otherInstIds.size <= 1, "cannot handle more than one folded pred inst id")

        if (instIds.size == 1 && otherInstIds.size == 1) {
          val instId = instIds.head
          val otherInstId = otherInstIds.head

          if (instId != otherInstId && instId.predId == otherInstId.predId) {
            val edgeLocalInstId = EdgeLocalIdentifier(List(edge.field), instId)
            val otherEdgeLocalInstId = EdgeLocalIdentifier(List(otherEdge.field), otherInstId)
            val repl = new Replacement()

            if (instId.version > otherInstId.version) {
              repl.value += (Set[Identifier](otherEdgeLocalInstId) -> Set[Identifier](edgeLocalInstId))
              otherEdge = otherEdge.copy(state = otherEdge.state.merge(repl))
            } else {
              repl.value += (Set[Identifier](edgeLocalInstId) -> Set[Identifier](otherEdgeLocalInstId))
              edge = edge.copy(state = edge.state.merge(repl))
            }

            logger.info(s"Merging predicate instance IDs $repl for ${edge.source}")

            Set(edge, otherEdge)
          } else edges
        } else edges
      }
    })

    resultAH = resultAH.copy(edges = newEdges)

    resultAH = resultAH.joinCommonEdges()

    val valueRenameMap = Vertex.vertexMapToValueHeapIdMap(renameMap)
    val resGeneralState = thisFolded.generalValState.lub(otherFolded.generalValState.rename(valueRenameMap.toMap))

    val result = factory(resultAH, resGeneralState, ExpressionSet())

    result
  }

  /** @todo this is only an experimental implementation
    * for simple cases such as `firstNaturals`. */
  def findPredInstIdIsomorphism(other: T, predId: PredicateIdentifier):
    Map[PredicateInstanceIdentifier, PredicateInstanceIdentifier] = {
    var result = Map.empty[PredicateInstanceIdentifier, PredicateInstanceIdentifier]
    for (edge <- abstractHeap.edges) {
      val otherEdge = other.abstractHeap.edges.find(e => e.weakEquals(edge)).get
      val insts = edge.state.predInsts
      val otherInsts = otherEdge.state.predInsts

      val folded = insts.foldedInstIds
      val unfolded = insts.unfoldedInstIds

      val otherFolded = otherInsts.foldedInstIds
      val otherUnfolded = otherInsts.unfoldedInstIds

      if (!folded.isEmpty && !otherFolded.isEmpty) {
        result += otherFolded.head -> folded.head
      }

      if (!unfolded.isEmpty && !otherUnfolded.isEmpty) {
        result += otherUnfolded.head -> unfolded.head
      }
    }
    result
  }

  def findPredInstIdIsomorphism(other: T): Map[PredicateInstanceIdentifier, PredicateInstanceIdentifier] = {
    if (generalValState.predDefs.ids == other.generalValState.predDefs.ids) {
      generalValState.predDefs.map.keySet.map(findPredInstIdIsomorphism(other, _)).flatten.toMap
    } else {
      sys.error("cannot find predicate instance ID isomorphism " +
        "if the predicate IDs do not match")
    }
  }

  def applyPredInstIdIsomorphism(iso: Map[PredicateInstanceIdentifier, PredicateInstanceIdentifier]): T = {
    mapEdges(edge => {
      val foldedAndUnfoldedPredInstIds = edge.state.predInsts.foldedAndUnfoldedInstIds
      val filteredIso = iso.filterKeys(foldedAndUnfoldedPredInstIds.contains)
      val repl = new Replacement()
      for ((from, to) <- filteredIso) {
        val fromEdgeLocId = EdgeLocalIdentifier(List(edge.field), from)
        val toEdgeLocId = EdgeLocalIdentifier(List(edge.field), to)
        repl.value += (Set[Identifier](fromEdgeLocId) -> Set[Identifier](toEdgeLocId))
      }
      edge.state.merge(repl)
    })
  }

  def pruneTopPredInstStates(): T = {
    mapEdges(edge => {
      edge.state.transformPredInsts(insts => {
        insts.copy(map = insts.map.filterNot(_._2.isTop))
      })
    })
  }

  override def widening(right: T): T = {
    // Consistent naming
    val left = this

    val mergedLeft = left.mergePointedNodes().pruneTopPredInstStates()
    val mergedRight = right.mergePointedNodes().pruneTopPredInstStates()

    val joined = mergedLeft.lub(mergedRight)

    val predInstIso = mergedLeft.findPredInstIdIsomorphism(joined)
    val joinedWithIsoApplied = joined.applyPredInstIdIsomorphism(predInstIso)
    // val joinedWithIsoApplied = joined

    if (mergedLeft.hasGraphIdenticalTo(joinedWithIsoApplied)) {
      val newAbstractHeap = mergedLeft.abstractHeap.wideningAfterMerge(joinedWithIsoApplied.abstractHeap)
      val newGeneralValState = mergedLeft.generalValState.widening(joinedWithIsoApplied.generalValState)
      factory(newAbstractHeap, newGeneralValState, ExpressionSet())
    } else {
      // Widening is not possible yet
      joined
    }
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
  // Shorthand for the type of states on the edges of abstract heaps.
  type EdgeStateDomain[S <: SemanticDomain[S]] =
  PreciseValueDrivenHeapState.EdgeStateDomain[SemanticAndPredicateDomain[S]]

  def makeTopEdgeState[S <: SemanticDomain[S]](s: S): EdgeStateDomain[S] = {
    PreciseValueDrivenHeapState.makeTopEdgeState(
      SemanticAndPredicateDomain(s, PredicateDomain()).top())
  }

  /** Implicit methods for edge states that make it more convenient to access
    * and transform the predicate definitions and instances states
    * in an edge state.
    *
    * For example, it is possible to get the predicates of an `edge` using
    * `edge.state.preds` instead of `edge.state.valueState.predicateState.definitions`.
    */
  implicit class ExtendedEdgeStateDomain[S <: SemanticDomain[S]](state: EdgeStateDomain[S]) {
    def predHeapIds: Set[ValueHeapIdentifier] =
      state.valueHeapIds.filter(_.typ == PredType)

    def predHeapIds(vertex: Vertex): Set[ValueHeapIdentifier] =
      state.valueHeapIds(vertex).filter(_.typ == PredType)

    /** Returns the predicate instances state in this edge state. */
    def predInsts: PredicateInstancesDomain =
      state.valueState.predicateState.instances

    /** Returns the predicate definitions state in this edge state. */
    def predDefs: PredicateDefinitionsDomain =
      state.valueState.predicateState.definitions

    /** Applies a transformation to the predicate state
      * in this edge state and returns the resulting edge state.
      * @param f the function to apply
      * @return the transformed edge state.
      */
    def transformPredState(f: PredicateDomain => PredicateDomain): EdgeStateDomain[S] = {
      state.copy(
        valueState = state.valueState.copy(
          predicateState = f(state.valueState.predicateState)))
    }

    /** Applies a transformation to the predicate instances state
      * in this edge state and returns the resulting edge state.
      * @param f the function to apply
      * @return the transformed edge state.
      */
    def transformPredInsts(f: PredicateInstancesDomain => PredicateInstancesDomain): EdgeStateDomain[S] =
      transformPredState(predState => {
        predState.copy(instances = f(predState.instances))
      })

    /** Applies a transformation to the predicate definitions state
      * in this edge state and returns the resulting edge state.
      * @param f the function to apply
      * @return the transformed edge state.
      */
    def transformPredDefs(f: PredicateDefinitionsDomain => PredicateDefinitionsDomain): EdgeStateDomain[S] =
      transformPredState(predState => {
        predState.copy(definitions = f(predState.definitions))
      })
  }
}

/** Cartesian product domain combining the predicate definitions
  * and predicate instances state.
  */
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
    !_2canHandle(id)

  def _2 = definitions

  def _2canHandle(id: Identifier) =
    id.isInstanceOf[PredicateIdentifier]

  override def toString =
    "Instances:\n" + ToStringUtilities.indent(instances.toString) + "\n" +
    "Definitions:\n" + ToStringUtilities.indent(definitions.toString)
}

/** Cartesian product domain that combines an arbitrary `SemanticDomain`
  * with a `PredicateDomain`.
  * It routes all method calls involving predicate identifiers to the
  * predicate domain and all other calls to the other domain.
  */
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

/** Overrides the behavior of `glbPreservingIds` such that the predicate
  * instance state of an edge is not set to top when applying a condition
  * without predicate instance state.
  *
  * The problem is that `DefaultGlbPreservingIdsStrategy` calls
  * `createVariable` for all edge-local predicate instance identifiers
  * that do not exist in the condition to be applied. These identifiers
  * are initialized to top. Then, when taking the meet with the
  * edge state, the resulting predicate instance state would always be top
  * (because of the must semantics of the predicate instance domain).
  * To combat this, this class temporarily sets the default value of
  * predicate instance identifiers to bottom in the states to meet.
  *
  * @todo get rid of this ugly hack
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