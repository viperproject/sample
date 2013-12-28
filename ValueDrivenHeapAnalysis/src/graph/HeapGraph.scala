package graph

import ch.ethz.inf.pm.sample.abstractdomain._
import scala.collection.immutable.{Set, TreeSet}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.Type
import scala.collection.mutable

case class HeapGraph[S <: SemanticDomain[S]](
    vertices: Set[Vertex] = TreeSet.empty[Vertex],
    edges: Set[EdgeWithState[S]] = Set.empty[EdgeWithState[S]]) {

  // This check should be carried out only in the debug mode.
  // checkConsistency()

  // private var mcsCounter = 0

  private def getNewVersionNumber: Int = {
    val usedVersionNumbers = getCurrentVersionNumbers
    for (i <- 0 until Int.MaxValue) {
      if (!usedVersionNumbers.contains(i))
        return i
    }
    throw new Exception("This point should never be reached!!!")
  }

  private def getCurrentVersionNumbers: Set[Int] =
    vertices collect {case hv : HeapVertex => hv.version}

  /** Returns all edges going out of a given vertex. */
  def outEdges(source: Vertex): Set[EdgeWithState[S]] =
    edges.filter(_.source == source)

  /** Returns all edges going out of a given vertex for a given field. */
  def outEdges(source: Vertex, field: Option[String]): Set[EdgeWithState[S]] =
    outEdges(source).filter(_.field == field)

  def createVariablesInAllStates(ids: Set[Identifier]): HeapGraph[S] =
    mapEdgeStates(_.createVariables(ids))

  def getPathsToBeAssigned(expr: AccessPathExpression): Set[Path[S]] =
    getPaths(expr.path.dropRight(1))

  def getPaths(path: List[String]): Set[Path[S]] = {
    assert(path.size > 0, "The path must be non-empty.")
    val startingVertices = vertices.filter(_.name == path.head)
    assert(startingVertices.size == 1, "The start of the path is not uniquely determined. This should not happen, " + "as the start should be always a variable.")
    val startingVertex = startingVertices.head
    assert(startingVertex.isInstanceOf[LocalVariableVertex], "The starting node should always represent a local variable.")
    paths(List.empty[EdgeWithState[S]], startingVertex, path)
  }

  def paths(prefix: Path[S], currentVertex : Vertex, path: List[String]): Set[Path[S]] = {
    assert(path.size > 0, "The path should never be empty.")
    var possibleNextEdges: Set[EdgeWithState[S]] = null
    if (currentVertex.isInstanceOf[LocalVariableVertex]) {
      possibleNextEdges = outEdges(currentVertex, None)
    } else {
      possibleNextEdges = outEdges(currentVertex, Some(path.head))
    }
    path match {
      case x :: Nil => {
        val result: Set[Path[S]] = possibleNextEdges.map(e => prefix :+ e)
        return result
      }
      case x :: xs => {
        assert(xs.size > 0, "This should never happen, should be caught by the previous case.")
        var result = Set.empty[Path[S]]
        for (e <- possibleNextEdges) {
          result = result.union(paths(prefix :+ e, e.target, path.tail))
        }
        return result
      }
    }
  }

  def assignAllValStates(leftId: Identifier, rightExp: Expression): HeapGraph[S] = {
    assert(leftId.isInstanceOf[VariableIdentifier] || leftId.isInstanceOf[ValueHeapIdentifier], "The other kinds of identifiers are not supported.")
    rightExp match {
      case _: Constant | _: VariableIdentifier =>
        mapEdgeStates(_.assign(leftId, rightExp))
    }
  }

  def meetStateOnAllEdges(state: S): HeapGraph[S] = {
    // The given state may AccessPathIdentifiers. These need to be added to the edge states.
    val apIDs = state.getIds().filter(_.isInstanceOf[AccessPathIdentifier])
    mapEdgeStates(edgeState => {
      // Edges may contain edge local identifiers that are not
      // in the given state. They need to be added.
      val elIDs = edgeState.getIds().filter(_.isInstanceOf[EdgeLocalIdentifier])
      val newState = state.createVariables(elIDs)
      val newEdgeState = edgeState.createVariables(apIDs)
      newState.glb(newEdgeState)
    })
  }

  def assignValueVariable(id: VariableIdentifier, right: Expression): HeapGraph[S] =
    mapEdgeStates(_.assign(id, right))

  /**
   *
   * @param label - Tells us whether this is a null, summary, definite or variable node (See VariableConstants for more info)
   * @return the heap graph that contains that new node and the newly created node itself.
   */
  def addNewVertex(label : String, typ: Type): (HeapGraph[S], Vertex) = {
    var newVertex : Vertex = null
    label match {
      case VertexConstants.NULL =>
        newVertex = new NullVertex
      case VertexConstants.SUMMARY =>
        newVertex = new SummaryHeapVertex(getNewVersionNumber, typ)
      case VertexConstants.DEFINITE =>
        newVertex = new DefiniteHeapVertex(getNewVersionNumber, typ)
      case _ =>
        newVertex = new LocalVariableVertex(label, typ)
    }
    (copy(vertices = vertices + newVertex), newVertex)
  }

  def addVertices(vs: Set[Vertex]): HeapGraph[S] =
    copy(vertices = vertices ++ vs)

  /**
   * This method removes all given vertices and all edges that have vertices
   * from vs as a source or target from the graph.
   *
   * @param vs set of vertices to be removed from the graph
   * @return graph without vertices vs and without edges containing vertices from vs
   */
  def removeVertices(vs: Set[Vertex]): HeapGraph[S] =
    HeapGraph(vertices -- vs, edges -- edges.filter(_.vertices.intersect(vs).isEmpty))

  def addEdges(es: Set[EdgeWithState[S]]): HeapGraph[S] =
    copy(edges = edges ++ es)

  def removeEdges(es: Set[EdgeWithState[S]]): HeapGraph[S] =
    copy(edges = edges -- es)

  def getVerticesWithLabel(label: String): Set[Vertex] =
    vertices.filter(_.label == label)

  /**
   * Helper function that initializes the map of maximal possible correspondence between edges of <code>this</code> and
   * <code>other</code>.
   *
   * @param other
   * @return
   */
  def initialMaxEdges(other: HeapGraph[S]): Map[EdgeWithState[S],Set[EdgeWithState[S]]] = {
    var maxEdges = Map.empty[EdgeWithState[S],Set[EdgeWithState[S]]]
    for (edge <- this.edges) {
      val possibleMatches = other.edges.filter(e => e.field.equals(edge.field) && e.source.label.equals(edge.source.label) && e.target.label.equals(edge.target.label))
      if (!possibleMatches.isEmpty)
        maxEdges += (edge -> possibleMatches)
    }
    return maxEdges
  }

  /**
   * Helper function that removes from the given <code>maxEdges</code> map those edges that are not possible under the
   * partial isomorphism <code>from</code> -> <code>to</code>.
   *
   * @param maxEdges
   * @param from
   * @param to
   * @return
   */
  def refineMaxEdges(maxEdges: Map[EdgeWithState[S],Set[EdgeWithState[S]]], from: Vertex, to: Vertex) : Map[EdgeWithState[S],Set[EdgeWithState[S]]] = {
    assert(from.label.equals(to.label), "The labels of " + from.toString + " and " + to.toString + " are not the same.")
    var newMaxEdges = Map.empty[EdgeWithState[S],Set[EdgeWithState[S]]]
    for ((edge, edgeSet) <- maxEdges) {
      var newEdgeSet = edgeSet
      if (edge.source.equals(from))
        newEdgeSet = newEdgeSet.filter(_.source == to)
//      newEdgeSet = newEdgeSet.filter(e => e.source.equals(to) && edge.source.equals(from))
      if (edge.target.equals(from))
        newEdgeSet = newEdgeSet.filter(_.target == to)
//      newEdgeSet = newEdgeSet.filter(e => e.target.equals(to)  && edge.source.equals(from))
      if (!edge.source.equals(from) && !edge.target.equals(from))
        newEdgeSet = newEdgeSet.filter(e => !e.source.equals(to) && !e.target.equals(to))
      if (!newEdgeSet.isEmpty)
        newMaxEdges += (edge -> newEdgeSet)
    }
    return newMaxEdges
  }

  /**
   * This method is used to calculate the maximum common subgraph (mcs) of <code>this</code> and <code>other</code>.
   * The resulting graph that is composed of vertices in keys of <code>I</code> and edges that are keys of <code>E</code>
   * is a proper subgraph of <code>this</code> meaning that the vertices (keys of <code>I</code>) and edge (keys of
   * <code>E</code>) subsets of vertices and edges of <code>this</this> (respectively).
   *
   * Furthermore, <code>I</code> is the subgraph isomorphism from the resulting mcs to <code>other</code>.
   *
   * <a href="http://onlinelibrary.wiley.com/doi/10.1002/spe.4380120103/abstract">McGregor's algorithms</a> is used to compute the mcs.
   *
   * @param other - graph for which mcs(<code>this, other</code>) should be calculated
   * @return a pair (<code>I, E</code>) where <code>I</code> is subgraph isomorphism form mcs(<code> this, other </code>)
   *         to <code>other</code> and <code>E</code> is the edge correspondence between the resulting mcs and edges of
   *         <code>other</code>
   */
  def mcs(other: HeapGraph[S]): (Map[Vertex, Vertex], Map[EdgeWithState[S], EdgeWithState[S]]) = {

    var isomorphism =  Map.empty[Vertex, Vertex]
    var possibleEdges = initialMaxEdges(other)
    val sureSet = vertices.intersect(other.vertices).filter(!_.isInstanceOf[HeapVertex])
    for(v <- sureSet) {
      isomorphism = isomorphism + (v -> v)
      possibleEdges = refineMaxEdges(possibleEdges, v, v)
    }
    //    mcsCounter = 0
    val (i, edgeMap) = mcsRecursive(vertices.filter(_.isInstanceOf[HeapVertex]),
      other.vertices.filter(_.isInstanceOf[HeapVertex]),
      isomorphism,
      possibleEdges,
      Map.empty[Vertex, Vertex],
      Map.empty[EdgeWithState[S],Set[EdgeWithState[S]]])

    //    //**println("When computing mcs " + mcsCounter + " nodes of the search tree were explored")
    var resEdgeMap =  Map.empty[EdgeWithState[S], EdgeWithState[S]]
    for ((from,to) <- edgeMap) {
      assert(to.size <= 1, "This should be always the case if the isomorphism is valid.")
      if (to.size > 0)
        resEdgeMap = resEdgeMap + (from -> to.head)
    }
    // val resEdgeMap: Map[EdgeWithState[S], EdgeWithState[S]] = edgeMap.map(entry => (entry._1 -> entry._2.head))
    return (i, resEdgeMap)
  }

  /**
   * @param other
   * @return maximum common subgraph of left and right (with names from left)
   *         set of identifiers to be removed from right general value state
   *         variables to be renamed in the right graph
   *         variables to which the above should be renamed in the right graph
   */
  def glb(other: HeapGraph[S]): (HeapGraph[S], Set[Identifier], List[Identifier], List[Identifier])= {
    val (iso, edgeMap) = mcs(other)
    var resultingGraph = new HeapGraph[S]()
    resultingGraph = resultingGraph.addVertices(iso.values.toSet[Vertex])
    resultingGraph = resultingGraph.addEdges(edgeMap.keySet)
    var renameFrom = List.empty[Identifier]
    var renameTo = List.empty[Identifier]
    for ((from, to) <- iso) {
      assert(from.typ.equals(to.typ))
      if (from.isInstanceOf[HeapVertex]) {
        for (valField <- from.typ.nonObjectFields) {
          renameFrom = renameFrom :+ ValueHeapIdentifier(from.asInstanceOf[HeapVertex], valField)
          renameTo = renameTo :+ ValueHeapIdentifier(to.asInstanceOf[HeapVertex], valField)
        }
      }
    }
    val verticesToRemove = (other.vertices.filter(_.isInstanceOf[HeapVertex]) -- iso.keySet).asInstanceOf[Set[HeapVertex]]
    var idsToRemove = Set.empty[Identifier]
    for (v <- verticesToRemove) {
      val removeForV: Set[ValueHeapIdentifier] = v.typ.getPossibleFields().map(ValueHeapIdentifier(v, _))
      idsToRemove = idsToRemove ++ removeForV
    }
    for (edgeRight <- edgeMap.values) {
      var newState = edgeRight.state.removeVariables(idsToRemove.asInstanceOf[Set[Identifier]])
      newState = newState.rename(renameFrom, renameTo)
      val edgeToAdd = EdgeWithState(iso.apply(edgeRight.source), newState, edgeRight.field, iso.apply(edgeRight.target))
      resultingGraph = resultingGraph.addEdges(Set(edgeToAdd))
    }
    resultingGraph = resultingGraph.meetCommonEdges()
    (resultingGraph, idsToRemove, renameFrom, renameTo)
  }

  def isBottom(): Boolean = {
    var result = false
    for (locVar <- vertices.filter(_.isInstanceOf[LocalVariableVertex])) {
      val localVarEdges = outEdges(locVar)
      result = result || localVarEdges.isEmpty
    }
    for (heapVertex <- vertices.filter(_.isInstanceOf[HeapVertex])) {
      for (refField <- heapVertex.typ.objectFields) {
        var presentEdges = Set.empty[EdgeWithState[S]]
        for (outEdge <- outEdges(heapVertex)) {
          outEdge.field match {
            case None =>
            case Some(f) => {
              if (refField.getName.equals(f))
                presentEdges = presentEdges + outEdge
            }
          }
        }
        result = result || presentEdges.isEmpty
      }
    }
    return result
  }

  private def minCommonSuperGraphBeforeJoin (other: HeapGraph[S], iso: Map[Vertex, Vertex]): (HeapGraph[S], List[Identifier], List[Identifier]) = {
    var resultingGraph = addVertices(other.vertices.filter(!_.isInstanceOf[HeapVertex]))
    var renameFrom = List.empty[Identifier]
    var renameTo = List.empty[Identifier]
    var edgesToAdd = other.edges
    //    var edgesToAdd: Set[EdgeWithState[S]] =
    //      if (!(right.vertices.filter(_.isInstanceOf[NullVertex]) -- left.vertices.filter(_.isInstanceOf[NullVertex])).isEmpty)
    //        right.edges.filter(_.target.isInstanceOf[NullVertex])
    //      else
    //        Set.empty[EdgeWithState[S]]
    var renaming = iso
    for (v <- other.vertices -- iso.keySet) {
      val (rg, newV) = resultingGraph.addNewVertex(v.label, v.typ)
      resultingGraph = rg
      edgesToAdd = edgesToAdd ++ other.edges.filter(e => e.source.equals(v) || e.target.equals(v))
      renaming = renaming + (v -> newV)
    }
    for ((from, to) <- renaming) {
      assert(from.typ.equals(to.typ))
      (from, to) match {
        case (from: HeapVertex, to: HeapVertex) =>
          for (valField <- from.typ.nonObjectFields) {
            renameFrom = renameFrom :+ ValueHeapIdentifier(from, valField)
            renameTo = renameTo :+ ValueHeapIdentifier(to, valField)
          }
        case _ => // Ignore all non-heap vertices
      }
    }
    for (e <- edgesToAdd) {
      val newSrc = if (renaming.keySet.contains(e.source)) renaming.apply(e.source) else e.source
      val newTrg = if (renaming.keySet.contains(e.target)) renaming.apply(e.target) else e.target
      resultingGraph = resultingGraph.addEdges(Set(EdgeWithState(newSrc, e.state.rename(renameFrom, renameTo), e.field, newTrg)))
    }
    resultingGraph.checkConsistency()
    (resultingGraph, renameFrom, renameTo)
  }

  private def mcsRecursive(V1: Set[Vertex],
                           V2: Set[Vertex],
                           isomorphism: Map[Vertex, Vertex],
                           possibleEdges: Map[EdgeWithState[S],Set[EdgeWithState[S]]],
                           bestIsomorphism: Map[Vertex, Vertex],
                           bestEdges: Map[EdgeWithState[S],Set[EdgeWithState[S]]]): (Map[Vertex, Vertex], Map[EdgeWithState[S],Set[EdgeWithState[S]]]) =
  {
    //    mcsCounter = mcsCounter + 1
    // Checking whether it is possible to get better results, if not, prune the part of the search tree rooted at this node
    if (math.min(V1.size,V2.size) + isomorphism.size < bestIsomorphism.size
      || (math.min(V1.size,V2.size) + isomorphism.size == bestIsomorphism.size && bestEdges.size >= possibleEdges.size))
      return (bestIsomorphism, bestEdges)
    // We reached the leaf of the search tree
    if (V1.isEmpty || V2.isEmpty) {
      if (isomorphism.size >= bestIsomorphism.size && possibleEdges.size > bestEdges.size)
        return (isomorphism, possibleEdges)
      else
        return (bestIsomorphism, bestEdges)
    }

    // Checking all possible parings for the next node from the left graph
    var currentIsomorphism = bestIsomorphism
    var currentEdges = bestEdges
    val v1 = V1.min
    for (v2 <- V2) {
      if (v2.label.equals(v1.label)) {
        val (i, e) = (isomorphism + (v2 -> v1), refineMaxEdges(possibleEdges, v1, v2));
        val (resIsomorphism, resEdge) = mcsRecursive(V1 - v1, V2 - v2, i, e, currentIsomorphism, currentEdges)
        if (resIsomorphism.size >= currentIsomorphism.size && resEdge.size > currentEdges.size) {
          currentIsomorphism = resIsomorphism
          currentEdges = resEdge
        }
      }
    }

    // The next node in the left graph might stay unpaired (not part of isomorphism). We check that here.
    val (resIsomorphism, resEdge) = mcsRecursive(V1 - v1, V2, isomorphism, possibleEdges, currentIsomorphism, currentEdges)
    if (resIsomorphism.size >= currentIsomorphism.size && resEdge.size > currentEdges.size) {
      currentIsomorphism = resIsomorphism
      currentEdges = resEdge
    }

    (currentIsomorphism, currentEdges)
  }

  override def toString = edges.mkString("\n")

  def prune(): (HeapGraph[S], Set[Identifier]) = {
    var currentEdges = edges
    var resultingEdgeSet = Set.empty[EdgeWithState[S]]
    var resultingVertices = vertices.filter(_.isInstanceOf[LocalVariableVertex])
    var changed = true
    while (changed) {
      val addEdges = currentEdges.filter(e => resultingVertices.contains(e.source) && !e.state.lessEqual(e.state.bottom()))
      currentEdges = currentEdges -- addEdges
      changed = !addEdges.isEmpty
      resultingVertices = resultingVertices ++ addEdges.map(e => e.target)
      resultingEdgeSet = resultingEdgeSet ++ addEdges
    }
    // It may be necessary to prune unreachable vertices even though the set
    // of edges stays the same. This can happen if edges were removed before
    // `prune` is called.
    if (resultingEdgeSet.size == edges.size && resultingVertices.size == vertices.size)
      return (this, Set.empty[Identifier])
    val verticesToRemove = (vertices -- resultingVertices).filter(_.isInstanceOf[HeapVertex])
    var idsToRemove = Set.empty[Identifier]
    for (v <- verticesToRemove) {
      for (valField <- v.typ.nonObjectFields) {
        val idToRemove = ValueHeapIdentifier(v.asInstanceOf[HeapVertex], valField)
        idsToRemove = idsToRemove + idToRemove
      }
    }
    val finalEdges = resultingEdgeSet.map(e => e.copy(state = e.state.removeVariables(idsToRemove)))
    val result = HeapGraph(resultingVertices, finalEdges)
    result.checkConsistency()
    (result, idsToRemove)
  }

  /**
   * Returns the partition of edges into sets of edges that are weakly
   * equivalent, identical except for the state.
   */
  def weakEdgeEquivalenceSets: Set[Set[EdgeWithState[S]]] =
    edges.map(e => edges.filter(e.weakEquals))

  /**
   * Creates a copy of the heap where each set of weakly equivalent edges
   * is combined into a single edge according to a given transformation
   * function.
   *
   * @param f the state transformation function
   * @return the transformed heap graph
   */
  def mapWeaklyEqualEdges(f: Set[S] => S) =
    copy(edges = weakEdgeEquivalenceSets.map(eqSet => {
      val someEdge = eqSet.iterator.next()
      someEdge.copy(state = f(eqSet.map(_.state)))
    }))

  def joinCommonEdges(): HeapGraph[S] =
    mapWeaklyEqualEdges(Lattice.bigLub)

  def meetCommonEdges(): HeapGraph[S] =
    mapWeaklyEqualEdges(Lattice.bigGlb)

  def widenCommonEdges(): HeapGraph[S] =
    mapWeaklyEqualEdges(Lattice.bigWidening)

  def lub(other: HeapGraph[S]): (HeapGraph[S], List[Identifier], List[Identifier]) = {
//    val (minCSBefore, renameFrom, renameTo) = minCommonSuperGraphBeforeJoin(left, right, left.mcs(left, right))
//    val (minCSBefore, nameMap) = minCommonSuperGraphBeforeJoin(left, right, left.mcs(right))
    val (resultingGraph, renameFrom, renameTo) = minCommonSuperGraphBeforeJoin(other, mcs(other)._1)
    val resultAH = resultingGraph.joinCommonEdges()
    resultAH.checkConsistency()
    (resultAH, renameFrom, renameTo)
  }

  /**
   * BFS algorithm for computing from which local variables heap nodes are reachable.
   *
   * @return the map that maps HeapVertices to set of LocalVariableVertices from which the HeapVertex is reachable
   *
   * @author Milos Novacek
   */
  private def reachableFromLocalVariable() : Map[HeapVertex, Set[LocalVariableVertex]] = {
    val queue = mutable.Queue.empty[HeapVertex]
    var result = mutable.Map.empty[HeapVertex, Set[LocalVariableVertex]]
    for (v <- vertices.collect({ case v: HeapVertex => v })) {
      val initSet : Set[LocalVariableVertex] = edges.filter(e =>
        e.target.equals(v) && e.source.isInstanceOf[LocalVariableVertex])
        .map(_.source).asInstanceOf[Set[LocalVariableVertex]]
      result += (v -> initSet)
      if (!initSet.isEmpty)
        queue.enqueue(v)
    }
    while (!queue.isEmpty) {
      val current = queue.dequeue()
      for (succ <- edges.filter(e => e.source.equals(current) && e.target.isInstanceOf[HeapVertex]).map(_.target).asInstanceOf[Set[HeapVertex]]) {
        if (!(result.apply(current) subsetOf result.apply(succ))) {
          queue.enqueue(succ)
          result.update(succ, result.apply(current) union result.apply(succ))
        }
      }
    }
    result.toMap[HeapVertex, Set[LocalVariableVertex]]
  }

  private def partition() : Map[(Set[LocalVariableVertex], Set[LocalVariableVertex]), Set[HeapVertex]] = {
    // TODO: Extend to access paths of any kind, not just local variables
    val reachabilityMap = reachableFromLocalVariable()
    val partitions = mutable.Map.empty[(Set[LocalVariableVertex], Set[LocalVariableVertex]), Set[HeapVertex]]
    val pointedByMap = mutable.Map.empty[HeapVertex, (Set[LocalVariableVertex], Set[LocalVariableVertex])]
    for (v <- reachabilityMap.keySet) {
      val pointingVars = edges.filter(e => e.target.equals(v) && e.source.isInstanceOf[LocalVariableVertex]).map(_.source).asInstanceOf[Set[LocalVariableVertex]]
      pointedByMap.update(v,(pointingVars, reachabilityMap.apply(v)))
    }
    for (valCond <- pointedByMap.values.toSet[(Set[LocalVariableVertex], Set[LocalVariableVertex])]) {
      val partition = pointedByMap.keySet.filter(pointedByMap.apply(_).equals(valCond))
      partitions.update(valCond, partition.toSet[HeapVertex])
    }
    partitions.toMap[(Set[LocalVariableVertex], Set[LocalVariableVertex]), Set[HeapVertex]]
  }

  def mergePointedNodes(): (HeapGraph[S], Replacement) = {
    //checkConsistancy(this)
    val partitions = partition()
    var newVertices = vertices.filter(!_.isInstanceOf[HeapVertex])
    val mergeMap = mutable.Map.empty[Vertex, Vertex]
    for (v <- newVertices)
      mergeMap.update(v,v)
    val repl = new Replacement()
    for((k,v) <- partitions.filter(!_._2.isEmpty)) {
      if (v.size > 1) {
        var newType = v.head.typ.bottom()
        for (vrtx <- v)
          newType = newType.lub(vrtx.typ)
        // If there is a summary vertex among the vertices to be merged,
        // reuse its version for the new summary vertex.
        // See issue #22.
        val newVersion = v.find(_.isInstanceOf[SummaryHeapVertex]).getOrElse(v.head).version
        val newVertex = new SummaryHeapVertex(newVersion, newType)
        newVertices = newVertices + newVertex
        for (vrtx <- v)
          mergeMap.update(vrtx, newVertex)
        for (valField <- newType.nonObjectFields) {
          val fromIds = mutable.Set.empty[ValueHeapIdentifier]
          for (vrtx <- v)
            fromIds += ValueHeapIdentifier(vrtx, valField)
          repl.value.update(fromIds.toSet, Set(ValueHeapIdentifier(newVertex, valField)))
        }
      } else {
        newVertices = newVertices + v.head
        mergeMap.update(v.head, v.head)
      }
    }
    val newEdges = mutable.Set.empty[EdgeWithState[S]]
    for (e <- edges)
      newEdges += EdgeWithState(mergeMap.apply(e.source), e.state.merge(repl), e.field, mergeMap.apply(e.target))
    (HeapGraph(newVertices, newEdges.toSet[EdgeWithState[S]]).joinCommonEdges(), repl)

    // See version control history for the original code
  }

  def wideningAfterMerge(other: HeapGraph[S]): HeapGraph[S] = {
    assert(vertices.size == other.vertices.size)
    val resGraph = HeapGraph(vertices ++ other.vertices, edges ++ other.edges)
    resGraph.checkConsistency()
    resGraph.widenCommonEdges()
  }

  private def checkConsistency() = {
    for (edge <- edges)
      if (!vertices.contains(edge.source) || !vertices.contains(edge.target)) {
        throw new Exception("Source and target vertices should be present.")
      }
  }

  /** Returns true iff there is no weakly equal pair of edges in the graph. */
  def isNormalized: Boolean =
    weakEdgeEquivalenceSets.forall(_.size == 1)

  def valueAssignOnEachEdge(variable : Option[VariableIdentifier],
                       pathsToConds : Map[Path[S],S],
                       field : Option[String],
                       rightExp : Expression,
                       condsForExp : Set[S]) : HeapGraph[S] = {
    // Assume that each value state in pathsToConds.values does not have EdgeLocalIdentifiers (Precondition)
    var resultingEdges = mutable.Set.empty[EdgeWithState[S]]
    variable match {
      case Some(v) => {
        for (edge <- edges) {
          var resultingState = edge.state.bottom()
          for (cond <- Utilities.applyConditions(Set(edge.state), condsForExp))
            resultingState = resultingState.lub(cond.assign(v, rightExp))
          resultingState = Utilities.removeAccessPathIdentifiers(resultingState)
          if (!resultingState.lessEqual(resultingState.bottom()))
            resultingEdges += edge.copy(state = resultingState)
        }
      }
      case None =>
    }
    field match {
      case Some(f) => {
        val nodesToUpdate = pathsToConds.keySet.map(_.last.target).asInstanceOf[Set[HeapVertex]]
        for (edge <- edges) {
          // This is for weak updates
          var resultingState = edge.state
          if ((nodesToUpdate.size == 1 && nodesToUpdate.head.isInstanceOf[DefiniteHeapVertex]) || // Strong update
              (pathsToConds.keySet.size == 1
                && pathsToConds.keySet.head.last.target.equals(edge.target)
                && (pathsToConds.keySet.head.last.source.isInstanceOf[DefiniteHeapVertex] || pathsToConds.keySet.head.last.source.isInstanceOf[LocalVariableVertex]))) { // Weak update with strong update of the target EdgeLocalIdentifier
            resultingState = edge.state.bottom()
          }
          val conditions = Utilities.applyConditions(Set(edge.state), Utilities.applyConditions(pathsToConds.values.toSet[S], condsForExp))
          for (nodeToUpdate <- nodesToUpdate) {
            val valueHeapIdToAssign = ValueHeapIdentifier(nodeToUpdate, f, rightExp.getType, rightExp.getProgramPoint)
            for (cond <- conditions) {
              var tempEdgeState = cond.assign(valueHeapIdToAssign, rightExp)
              if (edge.source.equals(nodeToUpdate)) {
                val edgeLocId = EdgeLocalIdentifier(List.empty[String], f, rightExp.getType)(rightExp.getProgramPoint)
                tempEdgeState = tempEdgeState.assign(edgeLocId, rightExp)
              }
              if (edge.target.equals(nodeToUpdate)) {
                val path = edge.field match {
                  case Some(g) => List(g)
                  case None => List.empty[String]
                }
                val edgeLocId = EdgeLocalIdentifier(path, f, rightExp.getType)(rightExp.getProgramPoint)
                tempEdgeState = tempEdgeState.assign(edgeLocId, rightExp)
              }
              resultingState = resultingState.lub(Utilities.removeAccessPathIdentifiers(tempEdgeState))
            }
          }
          resultingEdges = resultingEdges + edge.copy(state = resultingState)
        }
      }
      case None =>
    }
    copy(edges = resultingEdges.toSet)
  }

  def applyReplacement(repl: Replacement): HeapGraph[S] =
    mapEdgeStates(_.merge(repl))

  def valueAssumeOnEachEdge(exp: Expression, conds: Set[S]): HeapGraph[S] = {
    mapEdgeStates(state => {
      val edgeStateAndConds = Utilities.applyConditions(Set(state), conds)
      var resultingEdgeCond = state.bottom()
      for (c <- edgeStateAndConds) {
        resultingEdgeCond = resultingEdgeCond.lub(Utilities.removeAccessPathIdentifiers(c.assume(exp)))
      }
      resultingEdgeCond
    })
  }

  /**
   * Creates a copy of the heap where the state of all edges has been
   * transformed with the given function.
   *
   * @param f the state transformation function
   * @return the transformed heap graph
   */
  def mapEdgeStates(f: S => S) =
    copy(edges = edges.map(e => e.copy(state = f(e.state))))
}
