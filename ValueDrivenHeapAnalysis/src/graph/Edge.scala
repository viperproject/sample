package graph

import ch.ethz.inf.pm.sample.abstractdomain.{Lattice, SemanticDomain}

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 4/23/13
 * Time: 2:17 PM
 * To change this template use File | Settings | File Templates.
 */
class EdgeWithState[S <: SemanticDomain[S]] (val source: Vertex, val state: S, val field : Option[String], val target: Vertex) {

  override def toString(): String = {
    var stateString = "null"
    if (state != null)
      stateString = state.toString
    return source.toString + " --> " + target.toString + "\nState = " + stateString + "\nfield = " + field.toString
  }

  override def hashCode(): Int = {
    var stateString = "null"
    if (state != null)
//      stateString = state.toString
      stateString = state.hashCode().toString
    return (source.toString + target.toString + stateString + field.toString).hashCode
  }

  override def equals(obj : Any) : Boolean = {
    if (!obj.isInstanceOf[EdgeWithState[S]])
      return false
    val other = obj.asInstanceOf[EdgeWithState[S]]
    val srcEq = source.equals(other.source)
    val trgEq = target.equals(other.target)
    val fieldEq = field.equals(other.field)
//    val stateEq = state.equals(other.state)
    val stateEq = state == other.state
    return srcEq && trgEq && fieldEq && stateEq
  }

  /**
   * Equivalence up to state.
   * @param other
   */
  def weakEquals(other: EdgeWithState[S]): Boolean =  {
    return source.equals(other.source) && target.equals(other.target) && field.equals(other.field)
  }
}


