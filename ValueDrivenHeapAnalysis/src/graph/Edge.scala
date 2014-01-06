package graph

import ch.ethz.inf.pm.sample.abstractdomain.{AccessPathIdentifier, SemanticDomain}
import ch.ethz.inf.pm.sample.oorepresentation.DummyProgramPoint

case class EdgeWithState[S <: SemanticDomain[S]](
    source: Vertex,
    state: S,
    field: Option[String],
    target: Vertex) {

  override def toString: String = {
    val stateString = if (state != null) state.toString else "null"
    s"$source --> $target\nState = $stateString\nField = $field"
  }

  /**
   * Checks for equivalence of two edges, ignoring their state.
   * @param other the other edge to compare with
   */
  def weakEquals(other: EdgeWithState[S]): Boolean = {
    source.equals(other.source) && target.equals(other.target) && field.equals(other.field)
  }

  /** The set of vertices incident to the edge. */
  def vertices: Set[Vertex] = Set(source, target)
}

/** Represents a path of edges in a heap graph. */
trait HeapGraphPath[S <: SemanticDomain[S]] {
  require(!edges.isEmpty, "path cannot be empty")
  require(edges.tail.forall(_.source.isInstanceOf[HeapVertex]),
    "all edges (except the first) must have a heap vertex source")
  require(edges.zip(edges.tail).forall(t => t._1.target == t._2.source),
    "path is not consistent (edge target must equal source of next edge")

  val edges: List[EdgeWithState[S]]

  /** Returns the target vertex of the last edge on the path. */
  def target: Vertex =
    edges.last.target
}

/** Represents a heap graph path that may start with any vertex. */
case class PartialHeapGraphPath[S <: SemanticDomain[S]]
    (edges: List[EdgeWithState[S]]) extends HeapGraphPath[S] { }

/** Represents a heap graph path that starts with a local variable vertex. */
case class RootedHeapGraphPath[S <: SemanticDomain[S]]
    (edges: List[EdgeWithState[S]]) extends HeapGraphPath[S] {

  require(edges.head.source.isInstanceOf[LocalVariableVertex],
    "first edge source is not a local variable vertex")

  /** Returns the local variable vertex from which the path starts. */
  def localVarVertex: LocalVariableVertex =
    edges.head.source.asInstanceOf[LocalVariableVertex]

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
    AccessPathIdentifier(accPath)(edges.last.target.typ, DummyProgramPoint)
}