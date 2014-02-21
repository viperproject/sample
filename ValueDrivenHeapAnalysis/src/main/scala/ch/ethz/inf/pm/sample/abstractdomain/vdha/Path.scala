package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, AccessPathIdentifier, SemanticDomain}

/** Represents a path of edges in a heap graph. */
trait Path[S <: SemanticDomain[S]] {
  require(!edges.isEmpty, "path cannot be empty")

  require(edges.tail.forall(_.source.isInstanceOf[HeapVertex]),
    "all edges (except the first) must have a heap vertex source")

  require(edges.zip(edges.tail).forall(t => t._1.target == t._2.source),
    "path is not consistent (edge target must equal source of next edge")

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

  import Utilities._

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

  /** The condition satisfied by this path. */
  lazy val condition: S = {
    /**
     * Inner helper method for computing the condition recursively.
     *
     * @param path to be processed
     * @param state starting state where are only the edge-local identifiers
     *              with empty sequence of field access that represent targets
     */
    def recurse(path: List[Edge[S]], state: S): S = {
      // Only the edge-local identifiers that refer to target are present in
      // the given state (i.e. the once with empty sequence of field accesses)
      assert(state.edgeLocalIds.forall(_.accPath.isEmpty))

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
      var newState: S = state.createVariables(edge.state.edgeLocalIdsWithNonEmptyAccPath)
      newState = newState.glb(edge.state)

      // Now, we need to rename source-edge local identifiers to the ones
      // that are target of this edge and remove any others.
      newState = newState.removeVariables(newState.edgeLocalIdsWithEmptyAccPath)

      // Renaming
      val idsToRenameToSource = newState.edgeLocalIds.filter(_.accPath == List(field))

      // Building lists for renaming
      val renameMap = idsToRenameToSource.map(id => {
        id -> id.copy(accPath = List.empty)(id.pp)
      }).toMap
      newState = newState.rename(renameMap)

      // Now we remove all edge-local identifiers that can not be the targets.
      val elIdsToRemove = newState.edgeLocalIds -- renameMap.values
      newState = newState.removeVariables(elIdsToRemove)

      recurse(path.tail, newState)
    }

    // The head of the path (edge sequence) is starting from a variable.
    // Therefore, the edge local variables that represent the target edge-local
    // variables have an empty sequence of fields. However, we need to remove
    // all other edge-local identifier that might be possibly present.
    val state = edges.head.state
    recurse(edges.tail, state.removeVariables(state.edgeLocalIdsWithNonEmptyAccPath))
  }
}