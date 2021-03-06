/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import scala.collection.immutable.{Set, TreeSet}
import ch.ethz.inf.pm.sample.oorepresentation.Type
import scala.collection.mutable

case class HeapGraph[S <: SemanticDomain[S]](
    vertices: Set[Vertex] = TreeSet.empty[Vertex],
    edges: Set[Edge[S]] = Set.empty[Edge[S]]) {

  require(edges.forall(_.vertices.subsetOf(vertices)),
    "graph contains edges with unknown source or target vertices")

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

  /** Returns all definite heap vertices in the heap graph. */
  def definiteHeapVertices: Set[DefiniteHeapVertex] =
    vertices.collect({ case v: DefiniteHeapVertex => v })

  /** Returns all summary heap vertices in the heap graph. */
  def summaryHeapVertices: Set[SummaryHeapVertex] =
    vertices.collect({ case v: SummaryHeapVertex => v })

  /** Returns all local variable vertices in the heap graph. */
  def localVarVertices: Set[LocalVariableVertex] =
    vertices.collect({ case v: LocalVariableVertex => v })

  /** Returns the local variable vertex with the given name. */
  def localVarVertex(name: String): LocalVariableVertex = {
    val results = vertices.collect({
      case v: LocalVariableVertex if v.name == name => v
    })
    require(results.nonEmpty, s"no local variable vertex named '$name'")
    // TODO: Could check consistency when instantiating the heap graph
    assert(results.size == 1, s"there may only be one vertex named '$name'")
    results.head
  }

  /** Returns all vertices that are possible sources of edges. */
  def possibleSourceVertices: Set[Vertex] =
    vertices.filterNot(_ == NullVertex)

  /** Returns all vertices that are possible targets of edges. */
  def possibleTargetVertices: Set[Vertex] =
    vertices.filterNot(_.isInstanceOf[LocalVariableVertex])

  /** Returns all vertices that are possible targets of an edge
    * for a field of a given type.
    */
  def possibleTargetVertices(fieldType: Type): Set[Vertex] =
    possibleTargetVertices.filter(_.typ.lessEqual(fieldType))

  /** Returns all edges going out of a given vertex. */
  def outEdges(source: Vertex): Set[Edge[S]] =
    edges.filter(_.source == source)

  /** Returns all edges going out of a given vertex for a given field. */
  def outEdges(source: Vertex, field: Option[String]): Set[Edge[S]] =
    outEdges(source).filter(_.field == field)

  /** Returns all edges going out of local variable vertices. */
  def localVarEdges: Set[Edge[S]] =
    edges.filter(_.source.isInstanceOf[LocalVariableVertex])

  def createVariables(ids: Set[Identifier]): HeapGraph[S] =
    mapEdgeStates(_.createVariables(ids))

  def removeVariable(id: Identifier): HeapGraph[S] =
    mapEdgeStates(_.removeVariable(id))

  def removeVariables(ids: Set[Identifier]): HeapGraph[S] =
    mapEdgeStates(_.removeVariables(ids))

  def getPathsToBeAssigned(accPathId: AccessPathIdentifier): Set[RootedPath[S]] =
    paths(accPathId.stringPath.dropRight(1))

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
  def paths(path: List[String]): Set[RootedPath[S]] = {
    require(path.nonEmpty, "path must not be empty")

    def paths(path: List[String], vertex: Vertex): Set[PartialPath[S]] = {
      val field = vertex match {
        case v: LocalVariableVertex => None
        case _ => Some(path.head)
      }
      val nextEdges = outEdges(vertex, field)
      path match {
        case head :: Nil =>
          nextEdges.map(e => PartialPath(List(e)))
        case head :: tail =>
          nextEdges.map(e => paths(tail, e.target).map(
            path => PartialPath(e :: path.edges))).flatten
      }
    }

    paths(path, localVarVertex(path.head)).map(path =>
      RootedPath[S](path.edges))
  }

  def addNonHeapVertices(vs: Set[Vertex]): HeapGraph[S] = {
    require(!vs.exists(_.isInstanceOf[HeapVertex]))
    copy(vertices = vertices ++ vs)
  }

  def addNonHeapVertex(v: Vertex): HeapGraph[S] = {
    require(!v.isInstanceOf[HeapVertex])
    copy(vertices = vertices + v)
  }

  /** Creates a heap vertex for the given type and returns the graph. */
  def addHeapVertex(label: String, typ: Type): (HeapGraph[S], HeapVertex) = {
    val newVertex = label match {
      case VertexConstants.SUMMARY =>
        SummaryHeapVertex(getNewVersionNumber)(typ)
      case VertexConstants.DEFINITE =>
        DefiniteHeapVertex(getNewVersionNumber)(typ)
    }
    (copy(vertices = vertices + newVertex), newVertex)
  }

  /** Creates a new definite heap vertex and returns the resulting graph. */
  def addDefiniteHeapVertex(typ: Type): (HeapGraph[S], DefiniteHeapVertex) = {
    val newVertex = DefiniteHeapVertex(getNewVersionNumber)(typ)
    (copy(vertices = vertices + newVertex), newVertex)
  }

  /** Creates a heap vertex for each given type and returns the graph. */
  def addHeapVertices(label: String, types: Set[Type]): HeapGraph[S] =
    types.foldLeft(this)(_.addHeapVertex(label, _)._1)

  /**
   * This method removes all given vertices and all edges that have vertices
   * from vs as a source or target from the graph.
   *
   * @param verticesToRemove set of vertices to be removed from the graph
   * @return graph without vertices vs and without edges containing vertices from vs
   */
  def removeVertices(verticesToRemove: Set[Vertex]): HeapGraph[S] = {
    val edgesToRemove = edges.filterNot(_.vertices.intersect(verticesToRemove).isEmpty)
    HeapGraph(vertices -- verticesToRemove, edges -- edgesToRemove)
  }

  def addEdges(es: Set[Edge[S]]): HeapGraph[S] =
    copy(edges = edges ++ es)

  def addEdge(e: Edge[S]): HeapGraph[S] =
    copy(edges = edges + e)

  def removeEdges(es: Set[Edge[S]]): HeapGraph[S] =
    copy(edges = edges -- es)

  def mcs(other: HeapGraph[S]): CommonSubGraphIso[S] =
    CommonSubGraphIso.firstMax(from = other, to = this)

  /**
   * @param other
   * @return maximum common subgraph of left and right (with names from left)
   *         set of identifiers to be removed from right general value state
   *         variables to be renamed in the right graph
   *         variables to which the above should be renamed in the right graph
   */
  def glb(other: HeapGraph[S]): (HeapGraph[S], Set[Identifier], Map[Identifier, Identifier])= {
    val maxCommonSubGraph = mcs(other)
    val iso = maxCommonSubGraph.vertexMap
    val edgeMap = maxCommonSubGraph.edgeMap
    var resultingGraph = HeapGraph(vertices = iso.values.toSet, edges = edgeMap.keySet)
    val renameMap = Vertex.vertexMapToValueHeapIdMap(iso)

    val verticesToRemove = (other.vertices.filter(_.isInstanceOf[HeapVertex]) -- iso.keySet).asInstanceOf[Set[HeapVertex]]
    var idsToRemove = Set.empty[Identifier]
    for (v <- verticesToRemove) {
      val removeForV: Set[ValueHeapIdentifier] = v.typ.possibleFields.map(ValueHeapIdentifier(v, _))
      idsToRemove = idsToRemove ++ removeForV
    }
    for (edgeRight <- edgeMap.values) {
      var newState = edgeRight.state.removeVariables(idsToRemove)
      newState = newState.rename(renameMap)
      val edgeToAdd = Edge(iso.apply(edgeRight.source), newState, edgeRight.field, iso.apply(edgeRight.target))
      resultingGraph = resultingGraph.addEdge(edgeToAdd)
    }
    resultingGraph = resultingGraph.meetCommonEdges()
    (resultingGraph, idsToRemove, renameMap.toMap)
  }

  /** Returns true only if there exist certainly no concretizations
    * of this abstract heap.
    *
    * That is, it returns true only if there is any vertex without any
    * out-going edges for one of its objects fields (or no edge at all
    * in case of local variable vertices).
    *
    * The complexity of this method is O(V * F + E), where F is the number of
    * maximum number of object fields that a object type may have.
    * The old complexity was O(V * F * E).
    */
  def isBottom: Boolean = {
    val presentEdgeFieldsPerVertex = edges
      .groupBy(_.source)
      .mapValues(_.map(_.field).toSet)
      .withDefaultValue(Set.empty)

    vertices.exists(vertex => {
      vertex.neededEdgeFields.size > presentEdgeFieldsPerVertex(vertex).size
    })
  }

  def minCommonSuperGraphBeforeJoin (other: HeapGraph[S], iso: Map[Vertex, Vertex]):
      (HeapGraph[S], Map[Vertex, Vertex]) = {
    var resultingGraph = addNonHeapVertices(other.vertices.filter(!_.isInstanceOf[HeapVertex]))
    var edgesToAdd = other.edges
    var renaming = iso
    for (v <- other.vertices -- iso.keySet) {
      val (rg, newV) = v match {
        // Recreate heap vertices (they may require a new version number)
        case v: HeapVertex => resultingGraph.addHeapVertex(v.label, v.typ)
        // Reuse all other heap vertices
        case _ => (resultingGraph.addNonHeapVertex(v), v)
      }
      resultingGraph = rg
      edgesToAdd = edgesToAdd ++ other.edges.filter(e => e.source.equals(v) || e.target.equals(v))
      renaming = renaming + (v -> newV)
    }
    val renameMap = Vertex.vertexMapToValueHeapIdMap(renaming)
    for (e <- edgesToAdd) {
      val newSrc = renaming.getOrElse(e.source, e.source)
      val newTrg = renaming.getOrElse(e.target, e.target)
      resultingGraph = resultingGraph.addEdge(Edge(newSrc, e.state.rename(renameMap), e.field, newTrg))
    }
    (resultingGraph, renaming)
  }

  override def toString = edges.toList.sorted.mkString("\n")

  def prune(): (HeapGraph[S], Set[Identifier]) = {
    var currentEdges = edges
    var resultingEdgeSet = Set.empty[Edge[S]]
    var resultingVertices = localVarVertices.toSet[Vertex]
    var changed = true
    while (changed) {
      val addEdges = currentEdges.filter(e => resultingVertices.contains(e.source) && !e.state.lessEqual(e.state.bottom()))
      currentEdges = currentEdges -- addEdges
      changed = addEdges.nonEmpty
      resultingVertices = resultingVertices ++ addEdges.map(e => e.target)
      resultingEdgeSet = resultingEdgeSet ++ addEdges
    }
    // It may be necessary to prune unreachable vertices even though the set
    // of edges stays the same. This can happen if edges were removed before
    // `prune` is called.
    if (resultingEdgeSet.size == edges.size && resultingVertices.size == vertices.size)
      return (this, Set.empty[Identifier])

    val verticesToRemove = (vertices -- resultingVertices).collect({ case v: HeapVertex => v })
    val idsToRemove = verticesToRemove.flatMap(_.valueHeapIds)
    val finalEdges = resultingEdgeSet.map(e => e.copy(state = e.state.removeVariables(idsToRemove)))
    val result = HeapGraph(resultingVertices, finalEdges)
    (result, idsToRemove.toSet)
  }

  /**
   * Returns the partition of edges into sets of edges that are weakly
   * equivalent, identical except for the state.
   */
  def weakEdgeEquivalenceSets: Set[Set[Edge[S]]] =
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

  def lub(other: HeapGraph[S]): (HeapGraph[S], Map[Vertex, Vertex]) = {
    val iso = mcs(other).vertexMap
    val (resultingGraph, renameMap) = minCommonSuperGraphBeforeJoin(other, iso)
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
      if (initSet.nonEmpty)
        queue.enqueue(v)
    }
    while (queue.nonEmpty) {
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
    val mergeMap = mutable.Map.empty[Vertex, Vertex]
    for (v <- vertices.filter(!_.isInstanceOf[HeapVertex]))
      mergeMap.update(v,v)
    val repl = new Replacement()
    for((k,v) <- partitions.filter(_._2.nonEmpty)) {
      if (v.size > 1) {
        var newType = v.head.typ.bottom()
        for (vrtx <- v)
          newType = newType.lub(vrtx.typ)
        // If there is a summary vertex among the vertices to be merged,
        // reuse its version for the new summary vertex.
        // See issue #22.
        val newVersion = v.find(_.isInstanceOf[SummaryHeapVertex]).getOrElse(v.head).version
        val newVertex = SummaryHeapVertex(newVersion)(newType)
        for (vrtx <- v)
          mergeMap.update(vrtx, newVertex)
        for (valField <- newType.nonObjectFields) {
          val fromIds = mutable.Set.empty[ValueHeapIdentifier]
          for (vrtx <- v)
            fromIds += ValueHeapIdentifier(vrtx, valField)
          repl.value.update(fromIds.toSet, Set(ValueHeapIdentifier(newVertex, valField)))
        }
      } else {
        mergeMap.update(v.head, v.head)
      }
    }
    val newVertices = mergeMap.values.toSet
    val newEdges = mutable.Set.empty[Edge[S]]
    for (e <- edges)
      newEdges += Edge(mergeMap.apply(e.source), e.state.merge(repl), e.field, mergeMap.apply(e.target))
    (HeapGraph(newVertices, newEdges.toSet[Edge[S]]).joinCommonEdges(), repl)
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