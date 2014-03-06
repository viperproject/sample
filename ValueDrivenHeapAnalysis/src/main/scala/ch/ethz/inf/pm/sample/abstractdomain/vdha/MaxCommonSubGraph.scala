package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.SemanticDomain

/** An isomorphism between two sets of vertices.
  * It requires that any two corresponding vertices have the same label.
  */
trait VertexIso {
  require(verticesFrom.size == verticesTo.size,
    "vertex map must be an isomorphism")

  require(vertexMap.forall({ case (from, to) => from.label == to.label }),
    "vertex labels must match")

  /** Isomorphism between two sets of vertices. */
  val vertexMap: Map[Vertex, Vertex]

  /** Vertices mapped from. */
  def verticesFrom: Set[Vertex] = vertexMap.keySet

  /** Vertices mapped to. */
  def verticesTo: Set[Vertex] = vertexMap.values.toSet

  /** Size of the vertex isomorphism. */
  def size: Int = vertexMap.size
}

/** Isomorphism between two heap sub-graphs.
  * @todo add further consistency checks
  */
case class CommonSubGraphIso[S <: SemanticDomain[S]](
    vertexMap: Map[Vertex, Vertex],
    edgeMap: Map[Edge[S], Edge[S]])
  extends VertexIso {

  require(edgeMap.values.toSet.size == edgeMap.size,
    "edge map must be an isomorphism")

  /** Common sub-graph in the heap *from* which vertices
    * and edges have been mapped.
    */
  def maxCommonSubGraphFrom: HeapGraph[S] =
    HeapGraph[S](vertices = verticesFrom, edges = edgeMap.keySet)

  /** Common sub-graph in the heap *to* which vertices
    * and edges have been mapped.
    */
  def maxCommonSubGraphTo: HeapGraph[S] =
    HeapGraph[S](vertices = verticesTo, edges = edgeMap.values.toSet)
}

object CommonSubGraphIso {
  /** Finds the maximum common sub-graph of two given heap graphs and
    * returns the corresponding isomorphim between the two sub-graphs.
    *
    * In the resulting `CommonSubGraph` object, the methods
    * `commonSubGraphTo` and `commonSubGraphTo` yield a sub-graph of
    * `from` and `to`, respectively.
    *
    * See <a href="http://onlinelibrary.wiley.com/doi/10.1002/spe.4380120103/abstract">
    * McGregor's algorithms</a>.
    */
  def firstMax[S <: SemanticDomain[S]](from: HeapGraph[S], to: HeapGraph[S]):
      CommonSubGraphIso[S] = {
    val result = PartialCommonSubGraphIso.sure[S](from, to).findMax()
    result.toCommonSubGraph
  }
}

/**
 * Isomorphism between two heap sub-graphs during the computation.
 *
 * @param remainingVerticesFrom vertices we can still map from
 *                              (not part of the vertex isomorphism yet)
 * @param remainingVerticesTo vertices we can still map to
 *                            (not part of the vertex isomorphism yet)
 * @param vertexMap current vertex isomorphism
 * @param possibleEdgeMap stores for each edge what edges in the other heap graph
 *                        are still candidates for the edge isomorphism
 */
case class PartialCommonSubGraphIso[S <: SemanticDomain[S]](
    remainingVerticesFrom: Set[Vertex],
    remainingVerticesTo: Set[Vertex],
    vertexMap: Map[Vertex, Vertex],
    possibleEdgeMap: Map[Edge[S], Set[Edge[S]]])
  extends VertexIso {

  require(possibleEdgeMap.values.forall(!_.isEmpty),
    "each edge in the edge map must map to a non-empty set")

  require((remainingVerticesFrom intersect verticesFrom).isEmpty,
    "remaining and already mapped vertices must be disjoint")

  require((remainingVerticesTo intersect verticesTo).isEmpty,
    "remaining and already mapped vertices must be disjoint")

  def findMax(best: PartialCommonSubGraphIso[S] = PartialCommonSubGraphIso.empty[S]):
      PartialCommonSubGraphIso[S] = {
    if (isComplete) {
      // We reached the leaf of the search tree
      if (isBetterThan(best)) this else best
    } else if (!couldBeBetterThan(best)) {
      // Prune the part of the search tree rooted at this node
      best
    } else {
      // Checking all possible parings for the next node from the left graph
      var result = best
      // Find the best possible vertex to map from
      val to = remainingVerticesTo.min
      for (from <- remainingVerticesFrom) {
        if (from.label == to.label) {
          result = refine(from, to).findMax(result)
        }
      }

      // The next node in the left graph might stay unpaired
      // (not part of isomorphism). We check that here.
      val thisWithoutTo = copy[S](
        remainingVerticesTo = remainingVerticesTo - to)
      result = thisWithoutTo.findMax(result)
      result
    }
  }

  /** Convert computation result to a `MaxCommonSubGraphIsomorphism`. */
  def toCommonSubGraph: CommonSubGraphIso[S] = {
    CommonSubGraphIso(vertexMap, possibleEdgeMap.map({
      case (fromEdge, toEdges) =>
        assert(toEdges.size == 1, s"$fromEdge maps to multiple possible edges")
        fromEdge -> toEdges.head
      })
    )
  }

  /** Returns whether there are no remaining vertices to map. */
  def isComplete: Boolean =
    remainingVerticesFrom.isEmpty || remainingVerticesTo.isEmpty

  /** Returns the maximum number of vertices that could still be mapped. */
  def maxRemainingVertices: Int =
    Math.min(remainingVerticesFrom.size, remainingVerticesTo.size)

  /** Adds a new mapping to the vertex map and filters all entries
    * from the possible edge map that have become impossible.
    * Also removes the given vertices from the sets of remaining vertices.
    */
  def refine(from: Vertex, to: Vertex): PartialCommonSubGraphIso[S] = {
    require(remainingVerticesFrom.contains(from),
      s"$from is not a remaining vertex")
    require(remainingVerticesTo.contains(to),
      s"$to is not a remaining vertex")

    // Parameter validity will be checked in constructor of CommonSubGraphIso
    val newPossibleEdgeMap = possibleEdgeMap.map({
      case (fromEdge, toEdges) =>
        var newToEdges = toEdges
        if (fromEdge.source == from)
          newToEdges = newToEdges.filter(_.source == to)
        if (fromEdge.target == from)
          newToEdges = newToEdges.filter(_.target == to)
        if (fromEdge.source != from && fromEdge.target != from)
          newToEdges = newToEdges.filter(e => e.source != to && e.target != to)
        fromEdge -> newToEdges
    }).toMap.filterNot(_._2.isEmpty)

    PartialCommonSubGraphIso(
      remainingVerticesFrom = remainingVerticesFrom - from,
      remainingVerticesTo = remainingVerticesTo - to,
      vertexMap = vertexMap + (from -> to),
      newPossibleEdgeMap)
  }

  def refine(vertexMap: Map[Vertex, Vertex]): PartialCommonSubGraphIso[S] = {
    vertexMap.foldLeft(this)({
      case (commonSubGraph, (from, to)) => commonSubGraph.refine(from, to)
    })
  }

  def refine(vertices: Set[Vertex]): PartialCommonSubGraphIso[S] =
    refine(vertices.map(v => v -> v).toMap)

  /** Returns whether future refinements of this common sub-graph isomorphism,
    * could possibly lead of a complete common sub-graph isomorphism that is
    * better than the other given complete common sub-graph isomorphism.
    */
  def couldBeBetterThan(other: PartialCommonSubGraphIso[S]): Boolean = {
    require(other.isComplete, "the common sub-graph to compare to must be complete")
    (size + maxRemainingVertices > other.size) ||
    (size + maxRemainingVertices >= other.size && possibleEdgeMap.size > other.possibleEdgeMap.size)
  }

  /** Returns whether this complete common sub-graph isomorphism is better
    * than a given other one.
    */
  def isBetterThan(other: PartialCommonSubGraphIso[S]): Boolean = {
    require(isComplete, "only compare completed common sub-graphs")
    require(other.isComplete, "only compare completed common sub-graphs")
    size >= other.size && possibleEdgeMap.size > other.possibleEdgeMap.size
  }
}

object PartialCommonSubGraphIso {
  def empty[S <: SemanticDomain[S]] =
    PartialCommonSubGraphIso[S](Set.empty, Set.empty, Map.empty, Map.empty)

  /**
   * Returns the initial common sub-graph isomorphism with trivial mappings.
   *
   * It initializes the sets of remaining vertices and possible edges map.
   * The returned common sub-graph isomorphism is already refined w.r.t.
   * to local variables found in both heap graphs.
   *
   * @param from the heap graph to map vertices and edges from
   * @param to the heap graph to map vertices and edges to
   */
  def sure[S <: SemanticDomain[S]](
      from: HeapGraph[S], to: HeapGraph[S]): PartialCommonSubGraphIso[S] = {
    val possibleEdgeMap = from.edges.map(fromEdge =>
      fromEdge -> to.edges.filter(toEdge =>
        fromEdge.field == toEdge.field &&
          fromEdge.source.label == toEdge.source.label &&
          fromEdge.target.label == toEdge.target.label &&
          // It's not possible to match a self-loop edge to a regular edge
          // or vice-versa. Hence, already remove such mappings here
          fromEdge.isSelfLoop == toEdge.isSelfLoop
      )
    ).toMap.filterNot(_._2.isEmpty)

    val sureVertices = from.vertices.intersect(to.vertices).filter(!_.isInstanceOf[HeapVertex])
    val remainingVerticesFrom = sureVertices ++ from.heapVertices
    val remainingVerticesTo = sureVertices ++ to.heapVertices
    val initial = PartialCommonSubGraphIso[S](
      remainingVerticesFrom,
      remainingVerticesTo,
      Map.empty,
      possibleEdgeMap)

    initial.refine(sureVertices)
  }
}