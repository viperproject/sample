package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{Type, DummyProgramPoint}
import ch.ethz.inf.pm.sample.util.Predef._

case class Edge[S <: SemanticDomain[S]](
    source: Vertex,
    state: S,
    field: Option[String],
    target: Vertex) extends Ordered[Edge[S]] {

  require(field.isDefined implies
    source.typ.objectFields.exists(_.getName == field.get),
    s"source vertex has no field named '${field.get}'")

  require(target.typ.lessEqual(edgeType),
    s"target type ${target.typ} must be a subtype of the edge type $edgeType")

  require(field.isEmpty == source.isInstanceOf[LocalVariableVertex],
    "field must be empty iff the source is a local variable vertex")

  require(!target.isInstanceOf[LocalVariableVertex],
    "edge target must not be a local variable vertex")

  require(source != NullVertex,
    "edge source must not be a null vertex")

  override def toString: String = {
    val stateString = if (state != null) state.toString else "null"
    s"$source --> $target\nState = $stateString\nField = $field"
  }

  /**
   * Checks for equivalence of two edges, ignoring their state.
   * @param other the other edge to compare with
   */
  def weakEquals(other: Edge[S]): Boolean =
    source == other.source && target == other.target && field == other.field

  /** The set of vertices incident to the edge. */
  def vertices: Set[Vertex] = Set(source, target)

  /** Compare lexicographically by source, field and target. */
  override def compare(that: Edge[S]): Int = {
    implicitly[Ordering[(Vertex, Option[String], Vertex)]].compare(
      (source, field, target),
      (that.source, that.field, that.target))
  }

  /** Returns whether this edge points from a vertex to that vertex itself. */
  def isSelfLoop: Boolean =
    source == target

  /** Returns the identifier representing the field (if the field is defined). */
  def fieldId: Option[Identifier] = field match {
    // Its existence is guaranteed with the constructor precondition
    case Some(name) => source.typ.objectFields.find(_.getName == name)
    case None => None
  }

  /** Returns the static type of the reference represented by this edge.
    * If the source is a local variable vertex, the result is the variable type.
    * If the source is an heap vertex, the result is the field type.
    */
  def edgeType: Type = fieldId match {
    case Some(fieldId) => fieldId.getType
    case None => source.typ
  }

  /** Creates an `EdgeLocalIdentifier` in the edge state for a given value field
    * of the target vertex.
    *
    * If this edge is corresponds to a field, the identifier looks like
    * "eLocId.field.valField". Otherwise, it looks like "eLocId.valField".
    *
    * The method also assumes "eLocId.field.valField = target.valField" on the
    * edge state. However, that assumption only takes effect if the target
    * is a `DefiniteHeapVertex`.
    *
    * @param valueField the identifier of the value field of the target vertex
    * @return the resulting edge
   */
  def createTargetEdgeLocalId(valueField: Identifier): Edge[S] = {
    require(target.typ.nonObjectFields.contains(valueField),
       s"target vertex has no value field $valueField")

    val edgeLocalId = EdgeLocalIdentifier(List(field).flatten, valueField)
    var newState = state.createVariable(edgeLocalId)

    target match {
      case obj: HeapVertex =>
        val valueHeapId = ValueHeapIdentifier(obj, valueField)

        require(state.ids.contains(valueHeapId),
          s"edge state must already contain value heap identifier $valueHeapId")

        newState = state
          .createVariable(edgeLocalId)
          .assume(new BinaryArithmeticExpression(valueHeapId, edgeLocalId, ArithmeticOperator.==, null))
      case _ =>
        // The target vertex may also be a NullVertex, in which case we cannot
        // make the equality assumption anyway
    }

    copy(state = newState)
  }

  /** Create an `EdgeLocalIdentifier` in the edge state for each value field
    * of the target vertex.
    */
  def createTargetEdgeLocalIds(): Edge[S] = {
    require(target.isInstanceOf[HeapVertex],
      "target vertex must be a heap vertex")
    target.typ.nonObjectFields.foldLeft(this)(_.createTargetEdgeLocalId(_))
  }

  /** Create an `EdgeLocalIdentifier` in the edge state for
    * the given value field of the source vertex.
    */
  def createSourceEdgeLocalId(valueField: Identifier): Edge[S] = {
    require(source.isInstanceOf[HeapVertex],
      "source vertex must be a heap vertex")
    require(source.typ.nonObjectFields.contains(valueField),
      s"source vertex has no value field $valueField")

    copy(state = source.asInstanceOf[HeapVertex]
      .createEdgeLocalIdInState(state, valueField))
  }

  /** Create an `EdgeLocalIdentifier` in the edge state for each value field
    * of the source vertex.
    */
  def createSourceEdgeLocalIds(): Edge[S] = {
    require(source.isInstanceOf[HeapVertex],
      "source vertex must be a heap vertex")

    copy(state = source.asInstanceOf[HeapVertex]
      .createEdgeLocalIdsInState(state))
  }

  def createEdgeLocalIds(): Edge[S] = {
    var result = this
    if (source.isInstanceOf[HeapVertex])
      result = result.createSourceEdgeLocalIds()
    if (target.isInstanceOf[HeapVertex])
      result = result.createTargetEdgeLocalIds()
    result
  }
}