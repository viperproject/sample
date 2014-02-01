package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import scala.collection.immutable.{Set, TreeSet}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.Type
import scala.collection.mutable

case class HeapGraph[S <: SemanticDomain[S]](
    vertices: Set[Vertex] = TreeSet.empty[Vertex],
    edges: Set[EdgeWithState[S]] = Set.empty[EdgeWithState[S]]) {

  // TODO: Should only check during debugging for the sake of performance
  require(edges.forall(_.vertices.subsetOf(vertices)),
    "graph contains edges with unknown source or target vertices")

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

  def getPathsToBeAssigned(accPathId: AccessPathIdentifier): Set[RootedHeapGraphPath[S]] =
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
  def paths(path: List[String]): Set[RootedHeapGraphPath[S]] = {
    require(!path.isEmpty, "path must not be empty")

    def paths(path: List[String], vertex: Vertex): Set[PartialHeapGraphPath[S]] = {
      val field = vertex match {
        case v: LocalVariableVertex => None
        case _ => Some(path.head)
      }
      val nextEdges = outEdges(vertex, field)
      path match {
        case head :: Nil =>
          nextEdges.map(e => PartialHeapGraphPath(List(e)))
        case head :: tail =>
          nextEdges.map(e => paths(tail, e.target).map(
            path => PartialHeapGraphPath(e :: path.edges))).flatten
      }
    }

    paths(path, localVarVertex(path.head)).map(path =>
      RootedHeapGraphPath[S](path.edges))
  }

  /**
   *
   * @param label - Tells us whether this is a null, summary, definite or variable node (See VariableConstants for more info)
   * @return the heap graph that contains that new node and the newly created node itself.
   */
  def addNewVertex(label: String, typ: Type): (HeapGraph[S], Vertex) = {
    var newVertex : Vertex = null
    label match {
      case VertexConstants.NULL =>
        newVertex = NullVertex
      case VertexConstants.SUMMARY =>
        newVertex = SummaryHeapVertex(getNewVersionNumber)(typ)
      case VertexConstants.DEFINITE =>
        newVertex = DefiniteHeapVertex(getNewVersionNumber)(typ)
      case _ =>
        newVertex = LocalVariableVertex(label)(typ)
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
      val possibleMatches = other.edges.filter(e => {
        e.field == edge.field &&
          e.source.label == edge.source.label &&
          e.target.label == edge.target.label &&
          // It's not possible to match a self-loop edge to a regular edge
          // or vice-versa. Hence, already remove such mappings here
          e.isSelfLoop == edge.isSelfLoop
      })
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
      val removeForV: Set[ValueHeapIdentifier] = v.typ.possibleFields.map(ValueHeapIdentifier(v, _))
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

  override def toString = edges.toList.sorted.mkString("\n")

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
        val newVertex = SummaryHeapVertex(newVersion)(newType)
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
    resGraph.widenCommonEdges()
  }

  /** Returns true iff there is no weakly equal pair of edges in the graph. */
  def isNormalized: Boolean =
    weakEdgeEquivalenceSets.forall(_.size == 1)

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

/**
 * Combines a heap graph with a condition that the heap graph must satisfy.
 *
 * It is currently only used temporarily for expression evaluation. Such an
 * object is basically a `ValueDrivenHeapState` without an expression and
 * without the gigantic set of `State`-specific methods.
 *
 * The condition is (currently) not applied eagerly to the edges.
 */
case class CondHeapGraph[S <: SemanticDomain[S]](
    heap: HeapGraph[S],
    cond: S,
    takenPaths: Set[RootedHeapGraphPath[S]] = Set.empty[RootedHeapGraphPath[S]]) {

  import Utilities._
  import CondHeapGraph._

  require(edgeLocalIds(cond).isEmpty,
    "condition must not contain edge-local identifiers")

  require(accPathIds(cond).map(_.objPath).forall(objPath =>
    takenPaths.exists(_.accPath.startsWith(objPath))),
    "condition must only contain access path identifiers for taken paths")

  def takenPath(path: List[String]): RootedHeapGraphPath[S] =
    takenPaths.find(_.accPath == path).get

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
    val newPaths = takenPaths union other.takenPaths
    CondHeapGraph(newHeap, newCond, newPaths)
  }

  /** Applies the condition to each edge state. */
  def apply(): CondHeapGraph[S] =
    copy(heap = heap.mapEdgeStates(glbPreserveIds(_, cond)))

  /** Applies a function to both the condition and all edge states. */
  def map(f: S => S): CondHeapGraph[S] =
    copy(cond = f(cond), heap = heap.mapEdgeStates(f))

  /** Applies a function to each edge. */
  def mapEdges(f: EdgeWithState[S] => S) =
    copy(heap = heap.copy(edges = heap.edges.map(e => e.copy(state = f(e)))))

  /**
   * Returns whether either the heap or its condition are certainly bottom.
   * @todo this method is not precise, as the condition is not applied eagerly
   * @todo also return true if the taken paths are contradicting
   */
  def isBottom: Boolean =
    heap.isBottom() || cond.lessEqual(cond.bottom())

  /**
   * Prunes the conditional heap-graph, e.g., removes edges with a bottom state
   * as well as unreachable vertices.
   */
  def prune: CondHeapGraph[S] = {
    val (newHeap, idsToRemove) = heap.prune()
    val newCond = cond.removeVariables(idsToRemove)
    copy(heap = newHeap, cond = newCond)
  }

  /**
   * Finds all `AccessPathIdentifier`s contained within the given expression
   * and returns a conditional heap sub-graph for every possible combination
   * of corresponding paths taken through the heap.
   *
   * That is, in each returned conditional heap sub-graph, the taken path
   * is fixed for each `AccessPathIdentifer` in the given expression.
   *
   * @param expr the expression whose `AccessPathIdentifier`s to consider
   * @return the sequence of conditional heap sub-graphs
   */
  def evalExp(expr: Expression): CondHeapGraphSeq[S] = {
    // Translate non-numeric VariableIdentifiers to AccessPathIdentifiers
    val accessPathIds = expr.getIdentifiers.collect {
      case v: VariableIdentifier if !v.getType.isNumericalType =>
        AccessPathIdentifier(v)
      case apId: AccessPathIdentifier => apId
    }

    // If there are no AccessPathIdentifers, just return this
    accessPathIds.foldLeft[CondHeapGraphSeq[S]](this)((condHeaps, apId) => {
      condHeaps.intersect(evalAccessPathId(apId))
    })
  }

  /**
   * Builds a conditional heap sub-graph for every possible path that could be
   * taken in the heap for the given `AccessPathIdentifier`.
   *
   * Edges that certainly don't exist in a heap where a particular path is
   * taken are removed.
   *
   * Note that the path conditions are not applied to the edges of the
   * conditional heaps yet.
   *
   * @param ap the access path identifier to consider heap graph paths for
   * @return a conditional heap sub-graph for every path that could be taken
   */
  def evalAccessPathId(ap: AccessPathIdentifier): CondHeapGraphSeq[S] = {
    // Get path to the non-null receiver of the field access
    var paths = heap.paths(ap.objPath)
    if (ap.getType.isNumericalType) {
      paths = paths.filter(_.target.isInstanceOf[HeapVertex])
    }

    var result = List.empty[CondHeapGraph[S]]
    for (path <- paths) {
      var cond = path.condition

      if (ap.getType.isNumericalType) {
        val field = ap.path.last
        val targetVertex = path.target.asInstanceOf[HeapVertex]

        // Rename edge local identifier that corresponds to the access path
        val renameFrom = edgeLocalIds(cond).filter(_.field == field).toList
        assert(renameFrom.size == 1, "there should be exactly one identifier to rename")
        cond = cond.rename(renameFrom, List(ap))

        // AccessPathIdentifier must agree also with the ValueHeapIdentifier
        val resId = ValueHeapIdentifier(targetVertex, field, ap.getType, ap.pp)
        cond = cond.assume(new BinaryArithmeticExpression(resId, ap, ArithmeticOperator.==, null))
      }

      // Remove all edge local identifiers
      cond = cond.removeVariables(edgeLocalIds(cond))

      // Remove all edges that were NOT taken on this access path
      // Never remove edges going out of a summary node.
      var edgesToRemove = path.edges.map(edge => {
        val outEdges = heap.outEdges(edge.source, edge.field)
        val otherOutEdges = outEdges - edge
        otherOutEdges
      }).flatten.toSet
      edgesToRemove = edgesToRemove.filter(!_.source.isInstanceOf[SummaryHeapVertex])

      cond = Utilities.glbPreserveIds(this.cond, cond)

      val prunedHeap = heap.removeEdges(edgesToRemove)
      result = CondHeapGraph(prunedHeap, cond, Set(path)) :: result
    }
    CondHeapGraphSeq(result)(lattice)
  }

  /** Returns a sequence of heap sub-graphs on which the given expression
    * has been assumed.
    */
  def assume(cond: Expression): CondHeapGraphSeq[S] = {
    val result: CondHeapGraphSeq[S] = cond match {
      case Constant("false", _, _) => CondHeapGraphSeq(Seq())(lattice)
      case Constant("true", _, _) => this
      case VariableIdentifier(_, _, _, _)
           | NegatedBooleanExpression(VariableIdentifier(_, _, _, _))
           | BinaryArithmeticExpression(_, _, _, _) =>
        evalExp(cond).apply().map(_.assume(cond))
      case NegatedBooleanExpression(e) =>
        assume(negateExpression(e))
      case BinaryBooleanExpression(l,r,o,t) => {
        val result: CondHeapGraphSeq[S] = o match {
          case BooleanOperator.&& =>
            assume(l).assume(r)
          case BooleanOperator.|| =>
            // Delay joining, just return all heap graphs
            CondHeapGraphSeq(assume(l).condHeaps ++ assume(r).condHeaps)(lattice)
        }
        result
      }
      case ReferenceComparisonExpression(left, right, op, returnTyp) => {
        import ArithmeticOperator._

        evalExp(left).intersect(evalExp(right)).apply().mapCondHeaps(condHeap => {
          def targetVertex(exp: Expression): Vertex = exp match {
            case (Constant("null", _, _)) => NullVertex
            case AccessPathIdentifier(path) => condHeap.takenPath(path).target
          }

          val leftTarget = targetVertex(left)
          val rightTarget = targetVertex(right)

          op match {
            case `==` =>
              if (leftTarget == rightTarget) Seq(condHeap) else Seq()
            case `!=` =>
              if (leftTarget != rightTarget || leftTarget.isInstanceOf[SummaryHeapVertex]) Seq(condHeap) else Seq()
          }
        })
      }
      case _ =>
        println(s"CondHeapGraph.assume: $cond is not supported.")
        this
    }
    result.prune
  }

  def lattice = cond.bottom()
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

  implicit def CondHeapGraphSeqToCondHeapGraphSeq[S <: SemanticDomain[S]](
      condHeapGraphSeq: CondHeapGraphSeq[S]): Seq[CondHeapGraph[S]] =
    condHeapGraphSeq.condHeaps

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
 *
 * @todo prune bottom conditional heap graphs
 * @todo could model as set domain
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

  /** Applies the condition to each heap graph. */
  def apply(): CondHeapGraphSeq[S] =
    condHeaps.map(_.apply())

  /** Maps each condition and the state of each edge. */
  def map(f: S => S): CondHeapGraphSeq[S] =
    condHeaps.map(condHeap => condHeap.map(f))

  /** Assumes the given expression on each heap graph. */
  def assume(cond: Expression): CondHeapGraphSeq[S] =
    condHeaps.map(condHeap => condHeap.assume(cond).condHeaps).flatten

  /**
   * Maps conditional heap graphs with the given function.
   * The function may return multiple conditional heap graph.
   * They will be joined.
   */
  def mapCondHeaps(f: CondHeapGraph[S] => Seq[CondHeapGraph[S]]): CondHeapGraphSeq[S] =
    condHeaps.map(f).flatten

  /** Prunes all conditional heap graphs and removes the ones that are bottom. */
  def prune: CondHeapGraphSeq[S] =
    condHeaps.map(_.prune).filter(!_.isBottom)

  def isBottom: Boolean =
    condHeaps.isEmpty

  /**
   * Prunes and then joins all conditional heap graphs in this sequence
   * and returns the resulting conditional heap graph.
   *
   * Before joining, it removes the access path identifiers from the heaps.
   */
  def join: CondHeapGraph[S] = {
    val prunedHeaps = prune.condHeaps
    val newVertices = prunedHeaps.map(_.heap.vertices).flatten.toSet
    val newEdges = prunedHeaps.map(_.heap.mapEdgeStates(removeAccessPathIdentifiers)).map(_.edges).flatten.toSet
    val newHeap = HeapGraph(newVertices, newEdges).joinCommonEdges()
    val newCond = removeAccessPathIdentifiers(Lattice.bigLub(prunedHeaps.map(_.cond), lattice))
    CondHeapGraph(newHeap, newCond).prune
  }

  private implicit def CondHeapGraphSeqToCondHeapGraphSeq
      (condHeaps: Seq[CondHeapGraph[S]]): CondHeapGraphSeq[S] =
    CondHeapGraphSeq(condHeaps)
}