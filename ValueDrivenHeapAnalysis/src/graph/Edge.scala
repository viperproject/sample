package graph

import ch.ethz.inf.pm.sample.abstractdomain.SemanticDomain

case class EdgeWithState[S <: SemanticDomain[S]](source: Vertex, state: S, field: Option[String], target: Vertex) {

  override def toString: String = {
    var stateString = "null"
    if (state != null)
      stateString = state.toString
    source.toString + " --> " + target.toString + "\nState = " + stateString + "\nfield = " + field.toString
  }

  /**
   * Equivalence up to state.
   * @param other the other edge to compare with
   */
  def weakEquals(other: EdgeWithState[S]): Boolean = {
    source.equals(other.source) && target.equals(other.target) && field.equals(other.field)
  }
}


