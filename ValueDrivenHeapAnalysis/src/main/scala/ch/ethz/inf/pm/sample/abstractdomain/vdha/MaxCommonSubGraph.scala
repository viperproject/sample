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
    val empty = MaxCommonSubGraphIsoResult.First.empty[S]
    val result = PartialCommonSubGraphIso.sure[S](from, to).findMax(result = empty)
    result.best
  }

  /** Finds all maximum common sub-graphs of two given heap graphs. */
  def allMax[S <: SemanticDomain[S]](from: HeapGraph[S], to: HeapGraph[S]):
      Seq[CommonSubGraphIso[S]] = {
    val empty = MaxCommonSubGraphIsoResult.All.empty[S]
    val result = PartialCommonSubGraphIso.sure[S](from, to).findMax(result = empty)
    result.allBest
  }

  /** Returns an empty common sub-graph isomorphism. */
  def empty[S <: SemanticDomain[S]] = CommonSubGraphIso[S](Map.empty, Map.empty)
}

/** Result of a maximum common sub-graph isomorphism computation.
  *
  * Concrete implementation can choose whether to just keep one max common
  * sub-graph isomorphism or more than one.
  */
trait MaxCommonSubGraphIsoResult[
    S <: SemanticDomain[S],
    T <: MaxCommonSubGraphIsoResult[S, T]] { self: T =>

  /** Registers a new candidate isomorphism. */
  def add(candidate: PartialCommonSubGraphIso[S]): T

  /** Returns whether future refinements of the given common sub-graph iso,
    * could possibly lead of a common sub-graph isomorphism that could be
    * part of this result.
    */
  def couldContain(candidate: PartialCommonSubGraphIso[S]): Boolean

  /** Returns whether the given common sub-graph isomorphism is better
    * than a given other one.
    */
  def isBetter(
      candidate: PartialCommonSubGraphIso[S],
      best: CommonSubGraphIso[S]): Boolean = {
    require(candidate.isComplete, "only compare completed common sub-graphs")
    candidate.size >= best.size && candidate.possibleEdgeMap.size > best.edgeMap.size
  }
}

object MaxCommonSubGraphIsoResult {
  /** Just keeps the first maximum common sub-graph isomorphism. */
  case class First[S <: SemanticDomain[S]](best: CommonSubGraphIso[S])
    extends MaxCommonSubGraphIsoResult[S, First[S]] {

    def add(candidate: PartialCommonSubGraphIso[S]): First[S] = {
      require(candidate.isComplete,
        "can only add complete common sub-graphs to the result")

      if (isBetter(candidate, best))
        First[S](candidate.toCommonSubGraph)
      else
        this
    }

    def couldContain(candidate: PartialCommonSubGraphIso[S]) = {
      (candidate.size + candidate.maxRemainingVertices > best.size) ||
        (candidate.size + candidate.maxRemainingVertices >= best.size &&
          candidate.possibleEdgeMap.size > best.edgeMap.size)
    }
  }

  object First {
    /** Returns an empty result. */
    def empty[S <: SemanticDomain[S]]: First[S] =
      First[S](CommonSubGraphIso.empty[S])
  }

  /** Keeps all maximum common sub-graph isomorphisms.
    *
    * @todo share code with `First`
    */
  case class All[S <: SemanticDomain[S]](allBest: Seq[CommonSubGraphIso[S]])
    extends MaxCommonSubGraphIsoResult[S, All[S]] {

    require(allBest.map(_.size).toSet.size == 1,
      "all common sub-graphs isomorphisms must have the same size")

    require(allBest.map(_.edgeMap.size).toSet.size == 1,
      "all common sub-graphs isomorphisms must have the same size")

    def add(candidate: PartialCommonSubGraphIso[S]) = {
      require(candidate.isComplete,
        "can only add complete common sub-graphs to the result")

      if (isBetter(candidate, allBest.head))
        All[S](Seq(candidate.toCommonSubGraph))
      else if (isEqual(candidate, allBest.head))
        All[S](allBest :+ candidate.toCommonSubGraph)
      else
        this
    }

    def couldContain(candidate: PartialCommonSubGraphIso[S]) = {
      // Since all isomorphism in the result have the same size, we can just
      // compare to the first one
      (candidate.size + candidate.maxRemainingVertices > allBest.head.size) ||
        (candidate.size + candidate.maxRemainingVertices >= allBest.head.size &&
          candidate.possibleEdgeMap.size >= allBest.head.edgeMap.size)
    }

    /** Returns whether the given common sub-graph isomorphism is better
      * than a given other one.
      */
    def isEqual(
        candidate: PartialCommonSubGraphIso[S],
        best: CommonSubGraphIso[S]): Boolean = {
      require(candidate.isComplete, "only compare completed common sub-graphs")
      candidate.size == best.size && candidate.possibleEdgeMap.size == best.edgeMap.size
    }
  }

  object All {
    /** Returns an empty result. */
    def empty[S <: SemanticDomain[S]]: All[S] =
      All[S](Seq(CommonSubGraphIso.empty[S]))
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

  /** Finds the maximum common sub-graph isomorphism.
    *
    * @param result the best result found so far
    * @return the new best result
    */
  def findMax[T <: MaxCommonSubGraphIsoResult[S, T]](result: T): T = {
    if (isComplete) {
      // We reached the leaf of the search tree
      result.add(this)
    } else if (!result.couldContain(this)) {
      // Prune the part of the search tree rooted at this node
      result
    } else {
      // Checking all possible parings for the next node from the left graph
      var newResult = result
      // Find the result possible vertex to map from
      val to = remainingVerticesTo.min
      for (from <- remainingVerticesFrom) {
        if (from.label == to.label) {
          newResult = refine(from, to).findMax(newResult)
        }
      }

      // The next node in the left graph might stay unpaired
      // (not part of isomorphism). We check that here.
      val thisWithoutTo = copy[S](
        remainingVerticesTo = remainingVerticesTo - to)
      newResult = thisWithoutTo.findMax(newResult)

      // TODO: Should keep looking. It's possible that the first two or more
      // vertices remain unpaired

      newResult
    }
  }

  /** Convert computation result to a `CommonSubGraphIso`. */
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
}

object PartialCommonSubGraphIso {
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