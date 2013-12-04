package graph

import ch.ethz.inf.pm.sample.abstractdomain._
import scala.collection.immutable.{Queue, Set, TreeSet}
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.Type

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 4/23/13
 * Time: 4:59 PM
 * To change this template use File | Settings | File Templates.
 */
class HeapGraph[S <: SemanticDomain[S]](val vertices: TreeSet[Vertex], val edges: Set[EdgeWithState[S]]) {

  checkConsistancy(this)

//  for (edge <- edges)
//    if (!vertices.contains(edge.source) || !vertices.contains(edge.target)) {
//      throw new Exception("Source and target vertices should be present.")
//    }

  def this() = this(TreeSet.empty[Vertex], Set.empty[EdgeWithState[S]])

//  private var mcsCounter = 0

  private def getNewVersionNumber(): Int = {
    val usedVersionNumbers = getCurrentVersionNumbers()
    for (i <- 0 until Int.MaxValue) {
      if (!usedVersionNumbers.contains(i))
        return i
    }
    throw new Exception("This point should never be reached!!!")
  }

  private def getCurrentVersionNumbers(): Set[Int] = {
    return vertices collect {case hv : HeapVertex => hv.version}
  }

//  /**
//   * This method returns a copy of <code>this</code> in which all the states are produced by applying <code>repl</code>
//   * to the corresponding states in <code>this</code>.
//   *
//   * @param repl - replacement that should be applied to value states on all edges
//   * @return copy of <code>this</code> after applying <code>repl</code> to value states on each edge
//   */
//  def applyReplacementToAllStates(repl: Replacement): HeapGraph[S] = {
//    var resEdges = Set.empty[EdgeWithState[S]]
//    for (edge <- edges) {
//      resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.merge(repl), edge.field, edge.target)
//    }
//    return new HeapGraph[S](this.vertices, resEdges)
//  }

  def createVariableInAllStates(id: Identifier): HeapGraph[S] = {
    var resEdges = Set.empty[EdgeWithState[S]]
    for (edge <- edges) {
      resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.createVariable(id, id.getType()), edge.field, edge.target)
    }
    return new HeapGraph[S](this.vertices, resEdges)
  }

  def createVariablesInAllStates(ids: Set[Identifier]): HeapGraph[S] = {
    var result = this
    for (id <- ids)
      result = result.createVariableInAllStates(id)
    return result
  }

  def getPathsToBeAssigned(expr: AccessPathExpression): Set[List[EdgeWithState[S]]] = {
    getPaths(expr.path.dropRight(1))
  }

  def getPaths(path: List[String]): Set[List[EdgeWithState[S]]] = {
    assert(path.size > 0, "The path must be non-empty.")
    val startingVertices = vertices.filter(v => v.name == path.head)
    assert(startingVertices.size == 1, "The start of the path is not uniquely determined. This should not happen, " + "as the start should be always a variable.")
    val startingVertex = startingVertices.head
    assert(startingVertex.isInstanceOf[LocalVariableVertex], "The starting node should always represent a local variable.")
    paths(List.empty[EdgeWithState[S]], startingVertex, path)
  }

  def paths(prefix: List[EdgeWithState[S]], currentVertex : Vertex, path: List[String]): Set[List[EdgeWithState[S]]] = {
    assert(path.size > 0, "The path should never be empty.")
    var possibleNextEdges: Set[EdgeWithState[S]] = null
    if (currentVertex.isInstanceOf[LocalVariableVertex]) {
      possibleNextEdges = edges.filter(e => e.source.equals(currentVertex) && e.field == None)
    } else {
      possibleNextEdges = edges.filter(e => e.source.equals(currentVertex) && e.field.equals(Some(path.head)))
    }
    path match {
      case x :: Nil => {
        val result: Set[List[EdgeWithState[S]]] = possibleNextEdges.map(e => prefix :+ e)
        return result
      }
      case x :: xs => {
        assert(xs.size > 0, "This should never happen, should be caught by the previous case.")
        var result = Set.empty[List[EdgeWithState[S]]]
        for (e <- possibleNextEdges) {
          result = result.union(paths(prefix :+ e, e.target, path.tail))
        }
        return result
      }
      case _ =>
        throw new Exception("This should never happen.")
    }
  }

  def assignAllValStates(leftId: Identifier, rightExp: Expression): HeapGraph[S] = {
    assert(leftId.isInstanceOf[VariableIdentifier] || leftId.isInstanceOf[ValueHeapIdentifier], "The other kinds of identifiers are not supported.")
    rightExp match {
      case c: Constant => {
        var resEdges = Set.empty[EdgeWithState[S]]
        for (edge <- edges) {
          resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(leftId, rightExp), edge.field, edge.target)
        }
        return new HeapGraph[S](this.vertices, resEdges)
      }
      case v: VariableIdentifier => {
        var resEdges = Set.empty[EdgeWithState[S]]
        for (edge <- edges) {
          resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(leftId, rightExp), edge.field, edge.target)
        }
        return new HeapGraph[S](this.vertices, resEdges)
      }

    }
    return null
  }

  def meetStateOnAllEdges(state: S): HeapGraph[S] = {
    // The given state may AccessPathIdentifiers. These need to be added to the edge states.
    val apIDs = state.getIds().filter(_.isInstanceOf[AccessPathIdentifier]).toSet
    var newEdges = Set.empty[EdgeWithState[S]]
    for (e <- edges) {
      // Edges may contain edge local identifiers that are not in the given state. They need to be added.
      val elIDs = e.state.getIds().filter(_.isInstanceOf[EdgeLocalIdentifier]).toSet
      val newState = Utilities.createVariablesForState(state, elIDs)
      val newEdgeState = Utilities.createVariablesForState(e.state, apIDs)
      newEdges += new EdgeWithState[S](e.source, e.state.glb(newState, newEdgeState), e.field, e.target)
    }
    new HeapGraph[S](vertices, newEdges)



//    val valueFields = vertices.map(v => v.typ.getPossibleFields().filter(!_.getType().isObject())).flatten
//    var allEdgeLocalIds = valueFields.map(f => new EdgeLocalIdentifier(List.empty[String], f.getName(), f.getType, f.getProgramPoint()))
//    var referenceFields = Set.empty[String]
//    for (e <- edges) {
//      e.field match {
//        case None =>
//        case Some(f) => referenceFields = referenceFields + f
//      }
//    }
//    for (rf <- referenceFields) {
//      allEdgeLocalIds = allEdgeLocalIds ++ valueFields.map(f => new EdgeLocalIdentifier(List(rf), f.getName(), f.getType, f.getProgramPoint()))
//    }
//
//    val newEdges = edges.map(e => new EdgeWithState[S](e.source, e.state.glb(e.state, Utilities.createVariablesForState(state, allEdgeLocalIds.asInstanceOf[Set[Identifier]])), e.field, e.target))
//    new HeapGraph[S](vertices, newEdges)
  }


  /**
   *
   * @param variable
   * @param right - the right hand side should be already evaluated
   * @return
   */
  def assignValueVariable(variable: VariableIdentifier, right: Expression): HeapGraph[S] = {
    var resEdges = Set.empty[EdgeWithState[S]]
    for (edge <- edges) {
      resEdges = resEdges + new EdgeWithState[S](edge.source, edge.state.assign(variable, right), edge.field, edge.target)
    }
    return new HeapGraph[S](this.vertices, resEdges)
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
        newVertex = new SummaryHeapVertex(getNewVersionNumber(), typ)
      case VertexConstants.DEFINITE =>
        newVertex = new DefiniteHeapVertex(getNewVersionNumber(), typ)
      case _ =>
        newVertex = new LocalVariableVertex(label, typ)
    }
    return (new HeapGraph[S](vertices + newVertex,edges), newVertex)
  }

  def addVertices(vs: Set[Vertex]): HeapGraph[S] = {
    return new HeapGraph[S](vertices ++ vs,edges)
  }

  /**
   * This method removes all given vertices vs and all edges that have vertices from vs as a source or target from the graph.
   *
   * @param vs - set of vertices to be removed from the graph
   * @return graph without vertices vs and without edges containing vertices from vs
   */
  def removeVertices(vs: Set[Vertex]): HeapGraph[S] = {
    return new HeapGraph[S](vertices -- vs, edges -- edges.filter(x => !(vs.contains(x.source) || vs.contains(x.target))))
  }

  /**
   * Only the edges that have source and target in the set of vertices are added
   *
   * @param es - set of edges to be added
   * @return
   *
   */
  def addEdges(es: Set[EdgeWithState[S]]): HeapGraph[S] = {
    for (e <- es) {
      assert(containsVertex(this, e.source), "Trying to add edge that does not have source in the set of vertices")
      assert(containsVertex(this, e.target), "Trying to add edge that does not have target in the set of vertices")
    }
    return new HeapGraph[S](vertices,edges ++ es.filter(e => (vertices.contains(e.source) && vertices.contains(e.target))))
  }

  def removeEdges(es: Set[EdgeWithState[S]]): HeapGraph[S] = {
    return new HeapGraph[S](vertices, edges -- es)
  }

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[HeapGraph[S]])
      return false
    return vertices.equals(obj.asInstanceOf[HeapGraph[S]].vertices) && edges.equals(obj.asInstanceOf[HeapGraph[S]])
  }

  override def hashCode(): Int = {
    return (vertices.toString + edges.toString).hashCode
  }



  def getVerticesWithLabel(label: String): TreeSet[Vertex] = {
    return vertices.filter(v => v.label.equals(label))
  }

  /**
   * Helper function that initializes the map of maximal possible correspondence between edges of <code>this</code> and
   * <code>other</code>.
   *
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
        newEdgeSet = newEdgeSet.filter(e => e.source.equals(to))
//      newEdgeSet = newEdgeSet.filter(e => e.source.equals(to) && edge.source.equals(from))
      if (edge.target.equals(from))
        newEdgeSet = newEdgeSet.filter(e => e.target.equals(to))
//      newEdgeSet = newEdgeSet.filter(e => e.target.equals(to)  && edge.source.equals(from))
      if (!edge.source.equals(from) && !edge.target.equals(from))
        newEdgeSet = newEdgeSet.filter(e => !e.source.equals(to) && !e.target.equals(to))
      if (!newEdgeSet.isEmpty)
        newMaxEdges += (edge -> newEdgeSet)
    }
    return newMaxEdges
  }


  /**
   * This method calculates minimum common supergraph (MCS) of <code>this</code> and <code>other</code>.
   * <code>this</code> is the proper subgraph of <code>result</code>; i.e. vertices and edges of <code>this</code> are
   * subsets of <code>result</code>. Furthermore, <code>other</code> is subgraph isomorphic to <code>result</code>. The
   * method also returns the subgraph isomorphism <code>I</code> from <code>other</code> to <code>result</code>.
   *
   * MCS is calculated using <code>maximumCommonSubgraph</code> and gluing uncommon parts to it.
   * See <a href="http://dl.acm.org/citation.cfm?id=349979">this article</a> for more information on the algorithm.
   *
   * @param other - graph for which MCS(<code>this, other</code>) should be calculated
   * @return a pair (<code>G ,I</code>) where <code>G</code> = MCS(<code> this, other </code>) and <code>I</code> is the
   *         subgraph isomorphism from <code>other</code> to <code>G</code>
   */
  def minimumCommonSupergraph(other: HeapGraph[S]): (HeapGraph[S], Map[Vertex,Vertex]) = {
    return (null,null)
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

    //    var isomorphism =  Map.empty[Vertex, Vertex]
    //    var possibleEdges = initialMaxEdges(other)
    //    val sureSet = vertices.intersect(other.vertices).filter(v => !v.label.equals("def") && !v.label.equals("sum"))
    //    for(v <- sureSet) {
    //      isomorphism = isomorphism + (v -> v)
    //      possibleEdges = refineMaxEdges(possibleEdges, v, v)
    //    }
    //    mcsCounter = 0
    //    val result = mcsRecursive(vertices.filter(v => v.label.equals(VertexConstants.SUMMARY) || v.label.equals(VertexConstants.DEFINITE)),
    //      other.vertices.filter(v => v.label.equals(VertexConstants.SUMMARY) || v.label.equals(VertexConstants.DEFINITE)),
    //      isomorphism,
    //      possibleEdges,
    //      Map.empty[Vertex, Vertex],
    //      Map.empty[EdgeWithState[S],Set[EdgeWithState[S]]])
    //    //**println("When computing mcs " + mcsCounter + " nodes of the search tree were explored")
    //    return result
  }


//  private def mcsRecursive(V1: TreeSet[Vertex],
//          V2: TreeSet[Vertex],
//          isomorphism: Map[Vertex, Vertex],
//          possibleEdges: Map[EdgeWithState[S],Set[EdgeWithState[S]]],
//          bestIsomorphismAndEdges: Set[(Map[Vertex, Vertex], Map[EdgeWithState[S],Set[EdgeWithState[S]]])]): (Map[Vertex, Vertex], Set[Map[EdgeWithState[S],Set[EdgeWithState[S]]]]) =
//  {
////    mcsCounter = mcsCounter + 1
//    // Checking whether it is possible to get better results, if not, prune the part of the search tree rooted at this node
//    if (math.min(V1.size,V2.size) + isomorphism.size < bestIsomorphism.size
//        || (math.min(V1.size,V2.size) + isomorphism.size == bestIsomorphism.size && bestEdges.head.size > possibleEdges.size))
//      return (bestIsomorphism, bestEdges)
//    if (math.min(V1.size,V2.size) + isomorphism.size < bestIsomorphism.size
//      || (math.min(V1.size,V2.size) + isomorphism.size == bestIsomorphism.size && bestEdges.head.size == possibleEdges.size))
//      return (bestIsomorphism, bestEdges + possibleEdges)
//    // We reached the leaf of the search tree
//    if (V1.isEmpty || V2.isEmpty) {
//      if (isomorphism.size >= bestIsomorphism.size && possibleEdges.size > bestEdges.head.size)
//        return (isomorphism, Set(possibleEdges))
//      else if (isomorphism.size >= bestIsomorphism.size && possibleEdges.size == bestEdges.head.size)
//        return (isomorphism, bestEdges + possibleEdges)
//      else
//        return (bestIsomorphism, bestEdges)
//    }
//
//    // Checking all possible parings for the next node from the left graph
//    var currentIsomorphism = bestIsomorphism
//    var currentEdges = bestEdges
//    val v1 = V1.min
//    for (v2 <- V2) {
//      if (v2.label.equals(v1.label)) {
//        val (i, e) = (isomorphism + (v2 -> v1), refineMaxEdges(possibleEdges, v1, v2));
//        val (resIsomorphism, resEdge) = mcsRecursive(V1 - v1, V2 - v2, i, e, currentIsomorphism, currentEdges)
//        if (resIsomorphism.size >= currentIsomorphism.size && resEdge.head.size > currentEdges.head.size) {
//          currentIsomorphism = resIsomorphism
//          currentEdges = resEdge
//        } else if (resIsomorphism.size >= currentIsomorphism.size && resEdge.head.size == currentEdges.head.size) {
//          currentIsomorphism = resIsomorphism
//          currentEdges = resEdge ++ currentEdges
//        }
//      }
//    }
//
//    // The next node in the left graph might stay unpaired (not part of isomorphism). We check that here.
//    val (resIsomorphism, resEdge) = mcsRecursive(V1 - v1, V2, isomorphism, possibleEdges, currentIsomorphism, currentEdges)
//    if (resIsomorphism.size >= currentIsomorphism.size && resEdge.size > currentEdges.size) {
//      currentIsomorphism = resIsomorphism
//      currentEdges = resEdge
//    }
//    if (resIsomorphism.size >= currentIsomorphism.size && resEdge.size == currentEdges.size) {
//      currentIsomorphism = resIsomorphism
//      currentEdges = resEdge
//    }
//
//    return (currentIsomorphism, currentEdges)
//
//////    mcsCounter = mcsCounter + 1
////    // Checking whether it is possible to get better results, if not, prune the part of the search tree rooted at this node
////    if (math.min(V1.size,V2.size) + isomorphism.size < bestIsomorphism.size
////      || (math.min(V1.size,V2.size) + isomorphism.size == bestIsomorphism.size && bestEdges.size >= possibleEdges.size))
////      return (bestIsomorphism, bestEdges)
////    // We reached the leaf of the search tree
////    if (V1.isEmpty || V2.isEmpty) {
////      if (isomorphism.size >= bestIsomorphism.size && possibleEdges.size > bestEdges.size)
////        return (isomorphism, possibleEdges)
////      else
////        return (bestIsomorphism, bestEdges)
////    }
////
////    // Checking all possible parings for the next node from the left graph
////    var currentIsomorphism = bestIsomorphism
////    var currentEdges = bestEdges
////    val v1 = V1.min
////    for (v2 <- V2) {
////      if (v2.label.equals(v1.label)) {
////        val (i, e) = (isomorphism + (v1 -> v2), refineMaxEdges(possibleEdges, v1, v2));
////        val (resIsomorphism, resEdge) = mcsRecursive(V1 - v1, V2 - v2, i, e, currentIsomorphism, currentEdges)
////        if (resIsomorphism.size >= currentIsomorphism.size && resEdge.size > currentEdges.size) {
////          currentIsomorphism = resIsomorphism
////          currentEdges = resEdge
////        }
////      }
////    }
////
////    // The next node in the left graph might stay unpaired (not part of isomorphism). We check that here.
////    val (resIsomorphism, resEdge) = mcsRecursive(V1 - v1, V2, isomorphism, possibleEdges, currentIsomorphism, currentEdges)
////    if (resIsomorphism.size >= currentIsomorphism.size && resEdge.size > currentEdges.size) {
////      currentIsomorphism = resIsomorphism
////      currentEdges = resEdge
////    }
////
////    return (currentIsomorphism, currentEdges)
//  }

  /**
   *
   * @param left
   * @param right
   * @return maximum common subgraph of left and right (with names from left)
   *         set of identifiers to be removed from right general value state
   *         variables to be renamed in the right graph
   *         variables to which the above should be renamed in the right graph
   */
  def glb(left: HeapGraph[S], right: HeapGraph[S]): (HeapGraph[S], Set[Identifier], List[Identifier], List[Identifier])= {
    val (iso, edgeMap) = left.mcs(right)
    var resultingGraph = new HeapGraph[S]()
    resultingGraph = resultingGraph.addVertices(iso.values.toSet[Vertex])
    resultingGraph = resultingGraph.addEdges(edgeMap.keySet)
    var renameFrom = List.empty[Identifier]
    var renameTo = List.empty[Identifier]
    for ((from, to) <- iso) {
      assert(from.typ.equals(to.typ))
      if (from.isInstanceOf[HeapVertex]) {
        for (valField <- from.typ.getPossibleFields().filter(!_.getType().isObject())) {
          renameFrom = renameFrom :+ new ValueHeapIdentifier(from.asInstanceOf[HeapVertex], valField.getName(), valField.getType(), valField.getProgramPoint())
          renameTo = renameTo :+ new ValueHeapIdentifier(to.asInstanceOf[HeapVertex], valField.getName(), valField.getType(), valField.getProgramPoint())
        }
      }
    }
    val verticesToRemove = (right.vertices.filter(_.isInstanceOf[HeapVertex]) -- iso.keySet).asInstanceOf[Set[HeapVertex]]
    var idsToRemove = Set.empty[ValueHeapIdentifier]
    for (v <- verticesToRemove) {
      val removeForV: Set[ValueHeapIdentifier] = v.typ.getPossibleFields().map(f => new ValueHeapIdentifier(v, f.getName(), f.getType(), f.getProgramPoint()))
      idsToRemove = idsToRemove ++ removeForV
    }
    for (edgeRight <- edgeMap.values) {
      var newState = Utilities.removeVariablesFromState(edgeRight.state, idsToRemove.asInstanceOf[Set[Identifier]])
      newState = newState.rename(renameFrom, renameTo)
      val edgeToAdd = new EdgeWithState[S](iso.apply(edgeRight.source), newState, edgeRight.field, iso.apply(edgeRight.target))
      resultingGraph = resultingGraph.addEdges(Set(edgeToAdd))
    }
    resultingGraph = meetCommonEdges(resultingGraph)
    return (resultingGraph, idsToRemove.asInstanceOf[Set[Identifier]], renameFrom, renameTo)
  }

  def isBottom(): Boolean = {
    var result = false
    for (locVar <- vertices.filter(_.isInstanceOf[LocalVariableVertex])) {
      val localVarEdges = edges.filter(_.source.equals(locVar))
      result = result || localVarEdges.isEmpty
    }
    for (heapVertex <- vertices.filter(_.isInstanceOf[HeapVertex])) {
      for (refField <- heapVertex.typ.getPossibleFields().filter(f => f.getType().isObject())) {
        val outEdges = edges.filter(e => e.source.equals(heapVertex))
        var presentEdges = Set.empty[EdgeWithState[S]]
        for (oe <- outEdges) {
          oe.field match {
            case None =>
            case Some(f) => {
              if (refField.getName().equals(f))
                presentEdges = presentEdges + oe
            }
          }
        }
        result = result || presentEdges.isEmpty
      }
    }
    return result
  }


  private def minCommonSuperGraphBeforeJoin (left: HeapGraph[S], right: HeapGraph[S], iso: Map[Vertex, Vertex]): (HeapGraph[S], List[Identifier], List[Identifier]) = {
    var resultingGraph = left.addVertices(right.vertices.filter(!_.isInstanceOf[HeapVertex]))
    var renameFrom = List.empty[Identifier]
    var renameTo = List.empty[Identifier]
    var edgesToAdd = right.edges
    //    var edgesToAdd: Set[EdgeWithState[S]] =
    //      if (!(right.vertices.filter(_.isInstanceOf[NullVertex]) -- left.vertices.filter(_.isInstanceOf[NullVertex])).isEmpty)
    //        right.edges.filter(_.target.isInstanceOf[NullVertex])
    //      else
    //        Set.empty[EdgeWithState[S]]
    var renaming = iso
    for (v <- right.vertices -- iso.keySet) {
      val (rg, newV) = resultingGraph.addNewVertex(v.label, v.typ)
      resultingGraph = rg
      edgesToAdd = edgesToAdd ++ right.edges.filter(e => e.source.equals(v) || e.target.equals(v))
      renaming = renaming + (v -> newV)
    }
    for ((from, to) <- renaming) {
      assert(from.typ.equals(to.typ))
      if (from.isInstanceOf[HeapVertex]) {
        for (valField <- from.typ.getPossibleFields().filter(!_.getType().isObject())) {
          renameFrom = renameFrom :+ new ValueHeapIdentifier(from.asInstanceOf[HeapVertex], valField.getName(), valField.getType(), valField.getProgramPoint())
          renameTo = renameTo :+ new ValueHeapIdentifier(to.asInstanceOf[HeapVertex], valField.getName(), valField.getType(), valField.getProgramPoint())
        }
      }
    }
    for (e <- edgesToAdd) {
      val newSrc = if (renaming.keySet.contains(e.source)) renaming.apply(e.source) else e.source
      val newTrg = if (renaming.keySet.contains(e.target)) renaming.apply(e.target) else e.target
      resultingGraph = resultingGraph.addEdges(Set(new EdgeWithState[S](newSrc, e.state.rename(renameFrom, renameTo), e.field, newTrg)))
    }
    checkConsistancy(resultingGraph)
    return (resultingGraph, renameFrom, renameTo)
  }

//
//  private def mcsRecursive(left: HeapGraph[S],
//                           right: HeapGraph[S],
//                           V1: TreeSet[Vertex],
//                           V2: TreeSet[Vertex],
//                           isomorphism: Map[Vertex, Vertex],
//                           bestIsomorphism: Map[Vertex, Vertex]): Map[Vertex, Vertex] =
//  {
//    //    mcsCounter = mcsCounter + 1
//    // Checking whether it is possible to get better results, if not, prune the part of the search tree rooted at this node
//    if (math.min(V1.size,V2.size) + isomorphism.size < bestIsomorphism.size)
//      return bestIsomorphism
//    // We reached the leaf of the search tree
//    if (V1.isEmpty || V2.isEmpty) {
//      if (isomorphism.size > bestIsomorphism.size)
//        return isomorphism
//      if (isomorphism.size == bestIsomorphism.size) {
//        if (minCommSuperEdgeSize(left, right, isomorphism) < minCommSuperEdgeSize(left, right, bestIsomorphism))
//          return isomorphism
//      }
//      return bestIsomorphism
//    }
//
//    // Checking all possible parings for the next node from the left graph
//    var currentIsomorphism = bestIsomorphism
//    val v1 = V1.min
//    for (v2 <- V2) {
//      if (v2.label.equals(v1.label)) {
//        val i = isomorphism + (v2 -> v1)
//        val resIsomorphism = mcsRecursive(left, right, V1 - v1, V2 - v2, i, currentIsomorphism)
//        if (resIsomorphism.size > currentIsomorphism.size)
//          currentIsomorphism = resIsomorphism
//        if (resIsomorphism.size == currentIsomorphism.size)
//          if (minCommSuperEdgeSize(left, right, resIsomorphism) < minCommSuperEdgeSize(left, right, currentIsomorphism))
//            currentIsomorphism = resIsomorphism
//      }
//    }
//
//    // The next node in the left graph might stay unpaired (not part of isomorphism). We check that here.
//    val resIsomorphism = mcsRecursive(left, right, V1 - v1, V2, isomorphism, currentIsomorphism)
//    if (resIsomorphism.size > currentIsomorphism.size)
//      currentIsomorphism = resIsomorphism
//    if (resIsomorphism.size == currentIsomorphism.size)
//      if (minCommSuperEdgeSize(left, right, resIsomorphism) < minCommSuperEdgeSize(left, right, currentIsomorphism))
//        currentIsomorphism = resIsomorphism
//
//    return currentIsomorphism
//
//    ////    mcsCounter = mcsCounter + 1
//    //    // Checking whether it is possible to get better results, if not, prune the part of the search tree rooted at this node
//    //    if (math.min(V1.size,V2.size) + isomorphism.size < bestIsomorphism.size
//    //      || (math.min(V1.size,V2.size) + isomorphism.size == bestIsomorphism.size && bestEdges.size >= possibleEdges.size))
//    //      return (bestIsomorphism, bestEdges)
//    //    // We reached the leaf of the search tree
//    //    if (V1.isEmpty || V2.isEmpty) {
//    //      if (isomorphism.size >= bestIsomorphism.size && possibleEdges.size > bestEdges.size)
//    //        return (isomorphism, possibleEdges)
//    //      else
//    //        return (bestIsomorphism, bestEdges)
//    //    }
//    //
//    //    // Checking all possible parings for the next node from the left graph
//    //    var currentIsomorphism = bestIsomorphism
//    //    var currentEdges = bestEdges
//    //    val v1 = V1.min
//    //    for (v2 <- V2) {
//    //      if (v2.label.equals(v1.label)) {
//    //        val (i, e) = (isomorphism + (v1 -> v2), refineMaxEdges(possibleEdges, v1, v2));
//    //        val (resIsomorphism, resEdge) = mcsRecursive(V1 - v1, V2 - v2, i, e, currentIsomorphism, currentEdges)
//    //        if (resIsomorphism.size >= currentIsomorphism.size && resEdge.size > currentEdges.size) {
//    //          currentIsomorphism = resIsomorphism
//    //          currentEdges = resEdge
//    //        }
//    //      }
//    //    }
//    //
//    //    // The next node in the left graph might stay unpaired (not part of isomorphism). We check that here.
//    //    val (resIsomorphism, resEdge) = mcsRecursive(V1 - v1, V2, isomorphism, possibleEdges, currentIsomorphism, currentEdges)
//    //    if (resIsomorphism.size >= currentIsomorphism.size && resEdge.size > currentEdges.size) {
//    //      currentIsomorphism = resIsomorphism
//    //      currentEdges = resEdge
//    //    }
//    //
//    //    return (currentIsomorphism, currentEdges)
//  }


  private def mcsRecursive(V1: TreeSet[Vertex],
                           V2: TreeSet[Vertex],
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

    return (currentIsomorphism, currentEdges)

    ////    mcsCounter = mcsCounter + 1
    //    // Checking whether it is possible to get better results, if not, prune the part of the search tree rooted at this node
    //    if (math.min(V1.size,V2.size) + isomorphism.size < bestIsomorphism.size
    //      || (math.min(V1.size,V2.size) + isomorphism.size == bestIsomorphism.size && bestEdges.size >= possibleEdges.size))
    //      return (bestIsomorphism, bestEdges)
    //    // We reached the leaf of the search tree
    //    if (V1.isEmpty || V2.isEmpty) {
    //      if (isomorphism.size >= bestIsomorphism.size && possibleEdges.size > bestEdges.size)
    //        return (isomorphism, possibleEdges)
    //      else
    //        return (bestIsomorphism, bestEdges)
    //    }
    //
    //    // Checking all possible parings for the next node from the left graph
    //    var currentIsomorphism = bestIsomorphism
    //    var currentEdges = bestEdges
    //    val v1 = V1.min
    //    for (v2 <- V2) {
    //      if (v2.label.equals(v1.label)) {
    //        val (i, e) = (isomorphism + (v1 -> v2), refineMaxEdges(possibleEdges, v1, v2));
    //        val (resIsomorphism, resEdge) = mcsRecursive(V1 - v1, V2 - v2, i, e, currentIsomorphism, currentEdges)
    //        if (resIsomorphism.size >= currentIsomorphism.size && resEdge.size > currentEdges.size) {
    //          currentIsomorphism = resIsomorphism
    //          currentEdges = resEdge
    //        }
    //      }
    //    }
    //
    //    // The next node in the left graph might stay unpaired (not part of isomorphism). We check that here.
    //    val (resIsomorphism, resEdge) = mcsRecursive(V1 - v1, V2, isomorphism, possibleEdges, currentIsomorphism, currentEdges)
    //    if (resIsomorphism.size >= currentIsomorphism.size && resEdge.size > currentEdges.size) {
    //      currentIsomorphism = resIsomorphism
    //      currentEdges = resEdge
    //    }
    //
    //    return (currentIsomorphism, currentEdges)
  }

  override def toString(): String = {
    var result = ""
    for (edge <- edges) {
      result = result + edge.toString() + "\n"
    }
    return result
  }

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
    if (resultingEdgeSet.size == edges.size)
      return (this, Set.empty[Identifier])
    val verticesToRemove = (vertices -- resultingVertices).filter(_.isInstanceOf[HeapVertex])
    var idsToRemove = Set.empty[ValueHeapIdentifier]
    for (v <- verticesToRemove) {
      for (valField <- v.typ.getPossibleFields().filter(!_.getType().isObject())) {
        val idToRemove = new ValueHeapIdentifier(v.asInstanceOf[HeapVertex], valField.getName(), valField.getType(), valField.getProgramPoint())
        idsToRemove = idsToRemove + idToRemove
      }
    }
    val finalEdges = resultingEdgeSet.map(e => new EdgeWithState[S](e.source, Utilities.removeVariablesFromState[S](e.state, idsToRemove.asInstanceOf[Set[Identifier]]), e.field, e.target))
    val result = new HeapGraph[S](resultingVertices, finalEdges)
    checkConsistancy(result)
    return (result, idsToRemove.asInstanceOf[Set[Identifier]])
  }

  def joinCommonEdges() : HeapGraph[S] = {
    var resultEdges = Set.empty[EdgeWithState[S]]
    for (edge <- this.edges) {
      val weakEqualsSet = resultEdges.filter(_.weakEquals(edge))
      assert(weakEqualsSet.size <= 1)
      if (weakEqualsSet.isEmpty)
        resultEdges = resultEdges + edge
      else
        resultEdges = (resultEdges - weakEqualsSet.head) + new EdgeWithState[S](edge.source, edge.state.lub(weakEqualsSet.head.state, edge.state), edge.field, edge.target)
    }
    return new HeapGraph[S](this.vertices, resultEdges)
  }

  private def meetCommonEdges(graph: HeapGraph[S]): HeapGraph[S] = {
    var resultEdges = Set.empty[EdgeWithState[S]]
    for (edge <- graph.edges) {
      val weakEqualsSet = resultEdges.filter(_.weakEquals(edge))
      assert(weakEqualsSet.size <= 1)
      if (weakEqualsSet.isEmpty)
        resultEdges = resultEdges + edge
      else
        resultEdges = (resultEdges - weakEqualsSet.head) + new EdgeWithState[S](edge.source, edge.state.glb(weakEqualsSet.head.state, edge.state), edge.field, edge.target)
    }
    return new HeapGraph[S](graph.vertices, resultEdges)
  }

  private def widenCommonEdges(graph: HeapGraph[S]): HeapGraph[S] = {
    var resultEdges = Set.empty[EdgeWithState[S]]
    for (edge <- graph.edges) {
      val weakEqualsSet = resultEdges.filter(_.weakEquals(edge))
      assert(weakEqualsSet.size <= 1)
      if (weakEqualsSet.isEmpty)
        resultEdges = resultEdges + edge
      else
        resultEdges = (resultEdges - weakEqualsSet.head) + new EdgeWithState[S](edge.source, edge.state.widening(weakEqualsSet.head.state, edge.state), edge.field, edge.target)
    }
    return new HeapGraph[S](graph.vertices, resultEdges)
  }

  def lub(left: HeapGraph[S], right: HeapGraph[S]): (HeapGraph[S], List[Identifier], List[Identifier]) = {
//    val (minCSBefore, renameFrom, renameTo) = minCommonSuperGraphBeforeJoin(left, right, left.mcs(left, right))
//    val (minCSBefore, nameMap) = minCommonSuperGraphBeforeJoin(left, right, left.mcs(right))
    val (resultingGraph, renameFrom, renameTo) = minCommonSuperGraphBeforeJoin(left, right, left.mcs(right)._1)
    val resultAH = resultingGraph.joinCommonEdges()
    checkConsistancy(resultAH)
    return (resultAH, renameFrom, renameTo)
  }

  /**
   * BFS algorithm for computing from which local variables nodes are reachable.
   *
   * @return the map that maps HeapVertices to set of LocalVariableVertices from which the HeapVertex is reachable
   *
   * @author Milos Novacek
   */
  private def reachableFromLocalVariable() : Map[HeapVertex, Set[LocalVariableVertex]] = {
    val queue = scala.collection.mutable.Queue.empty[HeapVertex]
    var result = scala.collection.mutable.Map.empty[HeapVertex, Set[LocalVariableVertex]]
    for (v <- vertices.filter(_.isInstanceOf[HeapVertex]).asInstanceOf[Set[HeapVertex]]) {
      val initSet : Set[LocalVariableVertex] = edges.filter(e => e.target.equals(v) && e.source.isInstanceOf[LocalVariableVertex]).map(_.source).asInstanceOf[Set[LocalVariableVertex]]
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
    result
  }

  def mergePointedNodes(): (HeapGraph[S], Replacement) = {
    //checkConsistancy(this)
    val skuska = reachableFromLocalVariable()
    var resultGraph = new HeapGraph[S](vertices.filter(!_.isInstanceOf[HeapVertex]), Set.empty[EdgeWithState[S]])
    var pointedByMap = Map.empty[Set[Vertex], Set[Vertex]]
    for (v <- vertices.filter(_.isInstanceOf[HeapVertex])) {
      val pointedBySet = edges.filter(e => e.source.isInstanceOf[LocalVariableVertex] && e.target.equals(v)).map(_.source)
      if (pointedByMap.keySet.contains(pointedBySet))
        pointedByMap = pointedByMap.updated(pointedBySet, pointedByMap.apply(pointedBySet) + v)
      else
        pointedByMap = pointedByMap + (pointedBySet-> Set(v))
    }
    var replacementVertexMap = Map.empty[Set[Vertex], Vertex]
    val replacement = new Replacement()
    for ((pointedBy, vs) <- pointedByMap) {
      // merge vertices in vs
      var addedVertex: Vertex = null
      var verType = vs.head.typ
      if (vs.size == 1 && vs.head.isInstanceOf[DefiniteHeapVertex]) {
        val (newResGraph, newVertex) = resultGraph.addNewVertex(VertexConstants.DEFINITE, verType)
        resultGraph = newResGraph
        addedVertex = newVertex
      } else {
        for (t <- vs.map(_.typ)) verType = verType.lub(verType, t)
        val (newResGraph, newVertex) = resultGraph.addNewVertex(VertexConstants.SUMMARY, verType)
        resultGraph = newResGraph
        addedVertex = newVertex
      }
      for (valField <- verType.getPossibleFields().filter(!_.getType().isObject())) {
        val repFrom: Set[Identifier] = vs.map(v => new ValueHeapIdentifier(v.asInstanceOf[HeapVertex], valField.getName, valField.getType(), valField.getProgramPoint()))
        val repTo:Set[Identifier] = Set(new ValueHeapIdentifier(addedVertex.asInstanceOf[HeapVertex], valField.getName, valField.getType(), valField.getProgramPoint()))
        replacement.value += (repFrom -> repTo)
      }
      replacementVertexMap = replacementVertexMap + (vs -> addedVertex)
    }
    // now we add all edges
    for (edge <- edges) {
      val newSrcVertex: Vertex =
        if (edge.source.isInstanceOf[HeapVertex]) {
          val repKeySet = replacementVertexMap.keySet.filter(_.contains(edge.source))
          if (repKeySet.isEmpty) {
            checkConsistancy(this)
            throw new Exception("repKeySet should never be empty.")
          }
          val key = replacementVertexMap.keySet.filter(_.contains(edge.source)).head
          assert(key.size > 0, "The source vertex should be present in exactly one set.")
          replacementVertexMap.apply(key)
        } else
          edge.source
      val newTrgVertex: Vertex =
        if (edge.target.isInstanceOf[HeapVertex]) {
          val key = replacementVertexMap.keySet.filter(_.contains(edge.target)).head
          assert(key.size > 0, "The target vertex should be present in exactly one set.")
          replacementVertexMap.apply(key)
        } else
          edge.target
      // TODO: Check weather to do merge first and then LUB or the other way around
      resultGraph = resultGraph.addEdges(Set(new EdgeWithState[S](newSrcVertex, edge.state.merge(replacement), edge.field, newTrgVertex)))
    }
    var result = resultGraph
    checkConsistancy(resultGraph)
    result = result.joinCommonEdges()
    return (result, replacement)
  }

  def wideningAfterMerge(left: HeapGraph[S], right: HeapGraph[S]): HeapGraph[S] = {
    assert(left.vertices.size == right.vertices.size)
    val resGraph = new HeapGraph[S](left.vertices ++ right.vertices, left.edges ++ right.edges)
    checkConsistancy(resGraph)
    return widenCommonEdges(resGraph)
  }

  // def getAllEdgesFromVertexViaField(vertex: Vertex, field: String): Set[EdgeWithState[S]] = edges.filter(e => e.source.equals(vertex) && e.field.equals(field))

  private def checkConsistancy(graph: HeapGraph[S]) = {
    for (edge <- graph.edges)
      if (!containsVertex(graph, edge.source) || !containsVertex(graph, edge.target)) {
        throw new Exception("Source and target vertices should be present.")
      }

  }

  private def containsVertex(graph: HeapGraph[S], v: Vertex): Boolean =  {
    for (ver <- graph.vertices) {
      if (v.equals(ver))
        return true
    }
    return false
  }

  def isNormalized(): Boolean = {
    for (e <- edges) {
      val weaklyEqualEdges = edges.filter(_.weakEquals(e))
      if (weaklyEqualEdges.size > 1)
        return false
    }
    return true
  }


//  def materializeAccessPath(a)
//
//  def materializeVertex(v : HeapVertex): (HeapGraph[S], HeapVertex) = {
//    assert(vertices.contains(v), "The vertex to be materialized is not present.")
//    if (v.isInstanceOf[DefiniteHeapVertex])
//      return (this, v)
//    var (resultingGraph, newVertex) = addNewVertex(VertexConstants.DEFINITE, v.typ)
//    // compute the edges that need to be added
//    // 1. add all the incoming edges
//    val incomingEdges = edges.filter(_.target.equals(v))
//    for (e <- incomingEdges) {
//      if (e.source.equals(e.target)) {
//
//      }
//    }
//    null
//  }
}
