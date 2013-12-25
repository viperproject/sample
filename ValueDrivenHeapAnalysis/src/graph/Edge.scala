package graph

import ch.ethz.inf.pm.sample.abstractdomain.SemanticDomain

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


