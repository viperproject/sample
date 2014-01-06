package graph

import ch.ethz.inf.pm.sample.abstractdomain._
import scala.collection.immutable.{Set, TreeSet}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.Type
import scala.collection.mutable
import graph.ValueHeapIdentifier

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

  /** Returns all heap vertices in the heap graph. */
  def heapVertices: Set[HeapVertex] =
    vertices.collect({ case v: HeapVertex => v })

  /** Returns all local variable vertices in the heap graph. */
  def localVarVertices: Set[LocalVariableVertex] =
    vertices.collect({ case v: LocalVariableVertex => v })

  /** Returns all edges going out of a given vertex. */
  def outEdges(source: Vertex): Set[EdgeWithState[S]] =
    edges.filter(_.source == source)

  /** Returns all edges going out of a given vertex for a given field. */
  def outEdges(source: Vertex, field: Option[String]): Set[EdgeWithState[S]] =
    outEdges(source).filter(_.field == field)

  /** Returns the local variable vertex with the given name. */
  def localVarVertex(name: String): LocalVariableVertex = {
    val results = vertices.collect({
      case v: LocalVariableVertex if v.name == name => v
    })
    require(!results.isEmpty, s"no local variable vertex named '$name'")
    // TODO: Could check consistency when instantiating the heap graph
    assert(results.size == 1, s"there may only be one vertex named '$name'")
    results.head
  }

  def createVariablesInAllStates(ids: Set[Identifier]): HeapGraph[S] =
    mapEdgeStates(_.createVariables(ids))

  def getPathsToBeAssigned(accPathId: AccessPathIdentifier): Set[Path[S]] =
    paths(accPathId.path.dropRight(1))

  /**
   * Returns the possible set of paths in the heap graph (lists of edges)
   * corresponding to a given an access path of variable and field names.
   *
   * The first identifier in the access path must be a valid variable.
   * For example, given the simple access path `List("a")`, the method
   * will return the set of all edges (as singleton lists) going out of the
   * local variable vertex 'a'.
   *
   * @param path the non-empty access path
   * @return an empty set if there is no list of edges corresponding
   *         to the given access path
   */
  def paths(path: List[String]): Set[Path[S]] = {
    require(!path.isEmpty, "path must not be empty")

    def paths(path: List[String], vertex: Vertex): Set[Path[S]] = {
      val field = vertex match {
        case v: LocalVariableVertex => None
        case _ => Some(path.head)
      }
      val nextEdges = outEdges(vertex, field)
      path match {
        case head :: Nil =>
          nextEdges.map(List(_))
        case head :: tail =>
          nextEdges.map(e => paths(tail, e.target).map(e :: _)).flatten
      }
    }

    paths(path, localVarVertex(path.head))
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
  def glb(other: HeapGraph[S]): (HeapGraph[S], Set[Identifier], Map[Identifier, Identifier])= {
    val (iso, edgeMap) = mcs(other)
    var resultingGraph = HeapGraph(vertices = iso.values.toSet, edges = edgeMap.keySet)
    val renameMap = vertexToValueMap(iso)

    val verticesToRemove = (other.vertices.filter(_.isInstanceOf[HeapVertex]) -- iso.keySet).asInstanceOf[Set[HeapVertex]]
    var idsToRemove = Set.empty[Identifier]
    for (v <- verticesToRemove) {
      val removeForV: Set[ValueHeapIdentifier] = v.typ.getPossibleFields().map(ValueHeapIdentifier(v, _))
      idsToRemove = idsToRemove ++ removeForV
    }
    for (edgeRight <- edgeMap.values) {
      var newState = edgeRight.state.removeVariables(idsToRemove.asInstanceOf[Set[Identifier]])
      newState = newState.rename(renameMap)
      val edgeToAdd = EdgeWithState(iso.apply(edgeRight.source), newState, edgeRight.field, iso.apply(edgeRight.target))
      resultingGraph = resultingGraph.addEdges(Set(edgeToAdd))
    }
    resultingGraph = resultingGraph.meetCommonEdges()
    (resultingGraph, idsToRemove, renameMap)
  }

  /**
   * Builds a value field map from a vertex map.
   *
   * For example, given a key-value pair of heap vertices `n0` -> `n1` with
   * field `val` the resulting value field map will map `n0.val` to `n1.val`.
   *
   * @param vertexMap the mapping of vertices
   * @return the mapping of `ValueHeapIdentifier`
   */
  private def vertexToValueMap(vertexMap: Map[Vertex, Vertex]):
      Map[Identifier, Identifier] = {
    vertexMap.collect({
      case (from: HeapVertex, to: HeapVertex) =>
        assert(from.typ == to.typ)
        from.typ.nonObjectFields.map(field =>
          ValueHeapIdentifier(from, field) -> ValueHeapIdentifier(to, field))
    }).flatten.toMap
  }

  def isBottom(): Boolean = {
    var result = false
    for (locVar <- localVarVertices) {
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

  private def minCommonSuperGraphBeforeJoin (other: HeapGraph[S], iso: Map[Vertex, Vertex]):
      (HeapGraph[S], Map[Identifier, Identifier]) = {
    var resultingGraph = addVertices(other.vertices.filter(!_.isInstanceOf[HeapVertex]))
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
    val renameMap = vertexToValueMap(renaming)
    for (e <- edgesToAdd) {
      val newSrc = if (renaming.keySet.contains(e.source)) renaming.apply(e.source) else e.source
      val newTrg = if (renaming.keySet.contains(e.target)) renaming.apply(e.target) else e.target
      resultingGraph = resultingGraph.addEdges(Set(EdgeWithState(newSrc, e.state.rename(renameMap), e.field, newTrg)))
    }
    resultingGraph.checkConsistency()
    (resultingGraph, renameMap)
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
    var resultingVertices = localVarVertices.toSet[Vertex]
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
    mapWeaklyEqualEdges(Lattice.bigLub(_))

  def meetCommonEdges(): HeapGraph[S] =
    mapWeaklyEqualEdges(Lattice.bigGlb(_))

  def widenCommonEdges(): HeapGraph[S] =
    mapWeaklyEqualEdges(Lattice.bigWidening(_))

  def lub(other: HeapGraph[S]): (HeapGraph[S], Map[Identifier, Identifier]) = {
    val (resultingGraph, renameMap) = minCommonSuperGraphBeforeJoin(other, mcs(other)._1)
    val resultAH = resultingGraph.joinCommonEdges()
    resultAH.checkConsistency()
    (resultAH, renameMap)
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
    partitions.toMap
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

  def assignFieldOnEachEdge(
      field: String,
      pathsToConds: Map[Path[S], S],
      rightExp: Expression,
      condsForExp: Set[S]): HeapGraph[S] = {
    require(rightExp.getType.isNumericalType(),
      "can only assign numerical variables")
    // Assume that each value state in pathsToConds.values does not have EdgeLocalIdentifiers (Precondition)
    var resultingEdges = mutable.Set.empty[EdgeWithState[S]]
    val nodesToUpdate = pathsToConds.keySet.map(_.last.target).asInstanceOf[Set[HeapVertex]]
    for (edge <- edges) {
      // This is for weak updates
      var resultingState = edge.state
      if ((nodesToUpdate.size == 1 && nodesToUpdate.head.isInstanceOf[DefiniteHeapVertex]) || // Strong update
        (pathsToConds.keySet.size == 1
          && pathsToConds.keySet.head.last.target.equals(edge.target)
          && (pathsToConds.keySet.head.last.source.isInstanceOf[DefiniteHeapVertex] || pathsToConds.keySet.head.last.source.isInstanceOf[LocalVariableVertex]))) {
        // Weak update with strong update of the target EdgeLocalIdentifier
        resultingState = edge.state.bottom()
      }
      val conditions = Utilities.applyConditions(Set(edge.state), Utilities.applyConditions(pathsToConds.values.toSet[S], condsForExp))
      for (nodeToUpdate <- nodesToUpdate) {
        val valueHeapIdToAssign = ValueHeapIdentifier(nodeToUpdate, field, rightExp.getType, rightExp.getProgramPoint)
        for (cond <- conditions) {
          var tempEdgeState = cond.assign(valueHeapIdToAssign, rightExp)
          if (edge.source.equals(nodeToUpdate)) {
            val edgeLocId = EdgeLocalIdentifier(List.empty[String], field, rightExp.getType)(rightExp.getProgramPoint)
            tempEdgeState = tempEdgeState.assign(edgeLocId, rightExp)
          }
          if (edge.target.equals(nodeToUpdate)) {
            val path = edge.field match {
              case Some(g) => List(g)
              case None => List.empty[String]
            }
            val edgeLocId = EdgeLocalIdentifier(path, field, rightExp.getType)(rightExp.getProgramPoint)
            tempEdgeState = tempEdgeState.assign(edgeLocId, rightExp)
          }
          resultingState = resultingState.lub(Utilities.removeAccessPathIdentifiers(tempEdgeState))
        }
      }
      resultingEdges = resultingEdges + edge.copy(state = resultingState)
    }
    copy(edges = resultingEdges.toSet)
  }

  def applyReplacement(repl: Replacement): HeapGraph[S] =
    mapEdgeStates(_.merge(repl))

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

object HeapGraph {
  /**
   * Computes the condition that is satisfied by the given path.
   *
   * @param path for which the condition should be computed
   * @return abstract value condition that is satisfied by the given path
   */
  def pathCondition[S <: SemanticDomain[S]](path: Path[S]): S = {
    require(!path.isEmpty, "path cannot be empty")
    require(path.head.source.isInstanceOf[LocalVariableVertex],
      "first edge source is not a local variable vertex")
    require(path.tail.forall(_.source.isInstanceOf[HeapVertex]),
      "all edges (except the first) must have a heap vertex source")
    require(path.zip(path.tail).forall(t => t._1.target == t._2.source),
      "path is not consistent (edge target must equal source of next edge")

    /**
     * Inner helper method for computing the condition recursively.
     *
     * @param path to be processed
     * @param state starting state where are only the edge-local identifiers
     *              with empty sequence of field access that represent targets
     */
    def pathConditionRecursive(path: Path[S], state: S): S = {
      val stateEdgeLocalIds = edgeLocalIds(state)

      // Only the edge-local identifiers that refer to target are present in
      // the given state (i.e. the once with empty sequence of field accesses)
      assert(stateEdgeLocalIds.forall(_.accPath.isEmpty))

      // Base case is when the path is empty. (Termination)
      if (path.isEmpty) {
        return state
      }

      // If the path is non-empty, the head of it must refer to a field
      // (i.e. the first node must be a HeapVertex).
      val edge = path.head

      // Field should not be None here
      val field = edge.field.get

      // Originally, the edge local identifiers of the given state with the
      // empty sequence of fields refer to the target and no other edge-local
      // identifiers are present in the given state. We need to add them
      // so that the edge-local identifiers of the currently processed edge
      // do not get lost.
      val edgeLocalIdsToAdd = edgeLocalIds(edge.state).filter(!_.accPath.isEmpty)
      var newState: S = state.createVariables(edgeLocalIdsToAdd.toSet[Identifier])
      newState = newState.glb(edge.state)

      // Now, we need to rename source-edge local identifiers to the ones
      // that are target of this edge and remove any others.
      val originalSourceIds = edgeLocalIds(newState).filter(_.accPath.isEmpty)
      newState = newState.removeVariables(originalSourceIds)

      // Renaming
      val idsToRenameToSource = edgeLocalIds(newState).filter(_.accPath == List(field))

      // Building lists for renaming
      var renameFrom = List.empty[EdgeLocalIdentifier]
      var renameTo = List.empty[EdgeLocalIdentifier]
      for (elId <- idsToRenameToSource) {
        renameFrom = elId :: renameFrom
        renameTo = elId.copy(accPath = List.empty)(elId.getProgramPoint) :: renameTo
      }
      newState = newState.rename(renameFrom, renameTo)

      // Now we remove all edge-local identifiers that can not be the targets.
      val elIdsToRemove = newState.getIds().filter(_.isInstanceOf[EdgeLocalIdentifier]) -- renameTo
      newState = newState.removeVariables(elIdsToRemove.toSet[Identifier])

      pathConditionRecursive(path.tail, newState)
    }

    // The head of the path (edge sequence) is starting from a variable.
    // Therefore, the edge local variables that represent the target edge-local
    // variables have an empty sequence of fields. However, we need to remove
    // all other edge-local identifier that might be possibly present.
    val elIdsToRemove = edgeLocalIds(path.head.state).filter(!_.accPath.isEmpty)
    pathConditionRecursive(path.tail, path.head.state.removeVariables(elIdsToRemove))
  }

  /** Returns the set of all edge-local identifiers in the given state. */
  def edgeLocalIds[S <: SemanticDomain[S]](state: S): Set[EdgeLocalIdentifier] =
    state.getIds().collect({ case id: EdgeLocalIdentifier => id })
}

/**
 * Combines a heap graph with a condition that the heap graph must satisfy.
 *
 * It is currently only used temporarily for expression evaluation. Such an
 * object is basically a `ValueDrivenHeapState` without an expression and
 * without the gigantic set of `State`-specific methods.
 *
 * In the future, it might be desirable to move the general value state to
 * `HeapGraph` itself and merge `HeapGraph` and `CondHeapGraph`.
 *
 * The condition is (currently) not applied eagerly to the edges.
 */
case class CondHeapGraph[S <: SemanticDomain[S]](heap: HeapGraph[S], cond: S) {

  import Utilities._
  import CondHeapGraph._

  require(edgeLocalIds(cond).isEmpty,
    "condition must not contain edge-local identifiers")

  /**
   * Intersects this and another heap graph as well as their associated
   * conditions.
   *
   * This cheap intersection requires that the two heap graphs are sub-graphs
   * of some normalized heap graph.
   *
   * In the resulting heap sub-graph, an edge only occurs if
   * it occurred in both heap sub-graphs (strong equality).
   */
  def intersect(other: CondHeapGraph[S]): CondHeapGraph[S] = {
    require(heap.vertices == other.heap.vertices,
      "the vertices of the two heap graphs do not match")
    // TODO: Check the full precondition (the edges must no have been modified)

    val newEdges = heap.edges intersect other.heap.edges
    val newHeap = heap.copy(edges = newEdges)
    val newCond = glbPreserveIds(cond, other.cond)
    CondHeapGraph(newHeap, newCond)
  }

  /** Applies the condition to each edge state. */
  def apply(): CondHeapGraph[S] = {
    copy(heap = heap.mapEdgeStates(glbPreserveIds(_, cond)))
  }

  /**
   * Applies the condition to each edge state and then applies a function
   * to both the condition and all edge states.
   */
  def apply(f: S => S): CondHeapGraph[S] = {
    val newCondHeap = apply()
    val newCond = f(newCondHeap.cond)
    val newSubHeap = newCondHeap.heap.mapEdgeStates(f)
    copy(cond = newCond, heap = newSubHeap)
  }

  /**
   * Returns whether either the heap or its condition are certainly bottom.
   * @todo this method is not precise, as the condition is not applied eagerly
   */
  def isBottom: Boolean =
    heap.isBottom() || cond.lessEqual(cond.bottom())

  /**
   * Recursively evaluates the given expression.
   *
   * For each access path expression in the given expression, the method
   * enumerates the corresponding possible paths through the heap graph and
   * for each such path, materializes the heap sub-graph where all edges
   * are removed that are certainly not taken. Each such sub-graph is returned
   * together with the corresponding path condition.
   *
   * @todo support reference expressions
   */
  def evalExp(expr: Expression): CondHeapGraphSeq[S] = {
    require(!expr.getType.isObject(), "can only evaluate value expressions")

    expr match {
      case v: VariableIdentifier => this
      case c: Constant => this
      case ap: AccessPathIdentifier =>
        // Get path to the non-null receiver of the field access
        val paths = heap.paths(ap.objPath)
          .filter(_.last.target.isInstanceOf[HeapVertex])

        var result = List.empty[CondHeapGraph[S]]
        for (path <- paths) {
          val field = ap.path.last
          val targetVertex = path.last.target.asInstanceOf[HeapVertex]
          var cond = HeapGraph.pathCondition(path)

          // Rename edge local identifier that corresponds to the access path
          val renameFrom = HeapGraph.edgeLocalIds(cond).filter(_.field == field).toList
          assert(renameFrom.size == 1, "there should be exactly one identifier to rename")
          cond = cond.rename(renameFrom, List(ap))

          // AccessPathIdentifier must agree also with the ValueHeapIdentifier
          val resId = ValueHeapIdentifier(targetVertex, field, ap.getType, ap.getProgramPoint)
          cond = cond.assume(new BinaryArithmeticExpression(resId, ap, ArithmeticOperator.==, null))

          // Remove all edge local identifiers
          cond = cond.removeVariables(HeapGraph.edgeLocalIds(cond))

          // Remove all edges that were NOT taken on this access path
          // Never remove edges going out of a summary node.
          var edgesToRemove = path.map(edge => {
            val outEdges = heap.outEdges(edge.source, edge.field)
            val otherOutEdges = outEdges - edge
            otherOutEdges
          }).flatten.toSet
          edgesToRemove = edgesToRemove.filter(!_.source.isInstanceOf[SummaryHeapVertex])

          cond = Utilities.glbPreserveIds(this.cond, cond)

          val prunedHeap = heap.removeEdges(edgesToRemove)
          result = CondHeapGraph(prunedHeap, cond) :: result
        }
        val lattice = this.cond.bottom()
        CondHeapGraphSeq(result)(lattice)
      case BinaryArithmeticExpression(left, right, _, _) =>
        val evalLeft = evalExp(left)
        val evalRight = evalExp(right)
        evalLeft.intersect(evalRight)
      case BinaryBooleanExpression(left, right, _, _) =>
        val evalLeft = evalExp(left)
        val evalRight = evalExp(right)
        evalLeft.intersect(evalRight)
      case NegatedBooleanExpression(e) =>
        evalExp(e)
      case _ => ???
    }
  }
}

object CondHeapGraph {
  /** Converts a `ValueDrivenHeapState` to a `CondHeapGraph`. */
  def apply[S <: SemanticDomain[S]]
      (state: ValueDrivenHeapState[S]): CondHeapGraph[S] =
    CondHeapGraph(state.abstractHeap, state.generalValState)

  /**
   * Implicitly converts a conditional heap graph to a singleton conditional
   * heap graph sequence.
   */
  implicit def CondHeapGraphToCondHeapGraphSeq[S <: SemanticDomain[S]]
  (condHeap: CondHeapGraph[S]): CondHeapGraphSeq[S] =
    CondHeapGraphSeq(Seq(condHeap))(condHeap.cond.bottom())

  /**
   * Implicitly converts a conditional heap graph to a state
   * with an empty expression. It also automatically prunes the state.
   *
   * @todo Pruning should happen before the conversion
   */
  implicit def CondHeapGraphToValueDrivenHeapState[S <: SemanticDomain[S]]
  (condHeap: CondHeapGraph[S]): ValueDrivenHeapState[S] =
    ValueDrivenHeapState(condHeap.heap, condHeap.cond, ExpressionSet()).prune()
}

/**
 * Wraps a sequence of `CondHeapGraph`s and provides convenience methods.
 * It can be empty, representing bottom.
 *
 * @param lattice used to get access to the bottom element of the value lattice
 */
case class CondHeapGraphSeq[S <: SemanticDomain[S]]
    (condHeaps: Seq[CondHeapGraph[S]])(implicit lattice: S) {

  import Utilities._

  /**
   * Intersects all conditional heap graphs in this and another
   * given sequence pair-wise.
   *
   * This cheap intersection requires that the all heap graphs are sub-graphs
   * of some normalized heap graph.
   */
  def intersect(other: CondHeapGraphSeq[S]): CondHeapGraphSeq[S] =
    condHeaps.map(l => other.condHeaps.map(r => l.intersect(r))).flatten

  /** Applies `apply(f)` to each conditional heap graph. */
  def apply(f: S => S): CondHeapGraphSeq[S] =
    condHeaps.map(_.apply(f))

  /**
   * Joins all conditional heap graphs in this sequence and returns
   * the resulting conditional heap graph.
   *
   * Before joining, it removes the access path identifiers from the heaps.
   */
  def join: CondHeapGraph[S] = {
    val newVertices = condHeaps.map(_.heap.vertices).flatten.toSet
    val newEdges = condHeaps.map(_.heap.mapEdgeStates(removeAccessPathIdentifiers)).map(_.edges).flatten.toSet
    val newHeap = HeapGraph(newVertices, newEdges).joinCommonEdges()
    val newCond = removeAccessPathIdentifiers(Lattice.bigLub(condHeaps.map(_.cond), lattice))
    CondHeapGraph(newHeap, newCond)
  }

  private implicit def CondHeapGraphSeqToCondHeapGraphSeq
      (condHeaps: Seq[CondHeapGraph[S]]): CondHeapGraphSeq[S] =
    CondHeapGraphSeq(condHeaps)
}