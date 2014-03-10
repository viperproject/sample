package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.{AccessPathIdentifier, SemanticDomain}

/** Represents a path of edges in a heap graph. */
trait Path[S <: SemanticDomain[S]] {
  require(!edges.isEmpty, "path cannot be empty")

  require(edges.tail.forall(_.source.isInstanceOf[HeapVertex]),
    "all edges (except the first) must have a heap vertex source")

  require(edges.zip(edges.tail).forall(t => t._1.target == t._2.source),
    "path is not consistent (edge target must equal source of next edge)")

  val edges: List[Edge[S]]

  /** Returns the source vertex of the first edge on the path. */
  def source: Vertex = edges.head.source

  /** Returns the target vertex of the last edge on the path. */
  def target: Vertex = edges.last.target
}

/** Represents a heap graph path that may start with any vertex. */
case class PartialPath[S <: SemanticDomain[S]](edges: List[Edge[S]])
  extends Path[S] {}

/** Represents a heap graph path that starts with a local variable vertex. */
case class RootedPath[S <: SemanticDomain[S]](edges: List[Edge[S]])
  extends Path[S] {

  require(source.isInstanceOf[LocalVariableVertex],
    "first edge source is not a local variable vertex")

  /** Returns the local variable vertex from which the path starts. */
  def localVarVertex: LocalVariableVertex =
    source.asInstanceOf[LocalVariableVertex]

  /**
   * Returns the access path of strings.
   *
   * For example, for a path `this ----> (n0) --next--> (n1)`, the result
   * is `List("this", "next")`.
   */
  def accPath: List[String] =
    localVarVertex.name :: edges.tail.map(_.field.get)

  /** Returns the access path identifier corresponding to this path. */
  def accPathId: AccessPathIdentifier =
    AccessPathIdentifier(accPath)(target.typ)

  /** The condition satisfied by this path.
    *
    * The method basically takes the greatest lower bound of the states
    * of all edges along the path.
    *
    * The resulting condition contains source edge-local identifiers that
    * refer to value fields of the the last vertex on the path.
    *
    * @todo Could turn this method into a lazy value so the condition is never
    *       computed twice. However, for the moment not caching the condition
    *       is helpful for debugging
    */
  def condition: S = {
    /**
     * Inner helper method for computing the condition recursively.
     *
     * @param path still to be processed
     * @param currentState state satisfied by the path processed so far.
     *                     All of its edge-local identifiers have an empty
     *                     access path, i.e., refer to value fields of the
     *                     first vertex on the path still to be processed
     */
    def recurse(path: List[Edge[S]], currentState: S): S = {
      assert(currentState.targetEdgeLocalIds.isEmpty,
        "current state must not contain target edge-local identifier")

      // Terminate when the full path has been processed
      if (path.isEmpty) {
        return currentState
      }

      // The next edge to be processed
      val edge = path.head

      // The current state does not contain any target edge-local identifiers
      // of the new edge to be processed. Hence, we add them so
      // they don't get lost when taking the greatest lower bound.
      var newState: S = currentState.glbPreserveIds(edge.state)

      // Now, we need to rename source edge-local identifiers to the ones
      // that are target of this edge and remove any others.
      newState = newState.removeVariables(newState.sourceEdgeLocalIds)
      val renameMap = newState.targetEdgeLocalIds(edge.field).map(id => {
        id -> id.copy(accPath = List.empty)
      }).toMap
      newState = newState.rename(renameMap)

      // Now we remove all edge-local identifiers that can not be the targets.
      val elIdsToRemove = newState.edgeLocalIds -- renameMap.values
      newState = newState.removeVariables(elIdsToRemove)

      recurse(path.tail, newState)
    }

    // We can remove any edge-local ids from the local variable edge.
    // The first recursion step will rename the target edge-local identifiers
    // of that edge (with access path `List(None)`) to
    // source edge-local identifiers (with access path `List()`).
    val state = edges.head.state
    recurse(edges, state.removeVariables(state.edgeLocalIds))
  } ensuring(
    _.targetEdgeLocalIds.isEmpty,
    "resulting state must not contain edge-local target identifier")
}