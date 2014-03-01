package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
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

  require(source.isInstanceOf[LocalVariableVertex] implies
    state.sourceEdgeLocalIds.isEmpty,
    "local variable edges must not have edge-local source identifiers")

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

  /** Returns the source or target vertex given a relative access path
    * as used by edge-local identifiers associated with this edge.
    */
  def accPathToVertex(accPath: List[Option[String]]): Vertex = accPath match {
    case Nil => source
    case List(f) if f == field => target
  }

  /** Compare lexicographically by source, field and target. */
  override def compare(that: Edge[S]): Int = {
    implicitly[Ordering[(Vertex, Option[String], Vertex)]].compare(
      (source, field, target),
      (that.source, that.field, that.target))
  }

  /** Returns whether this edge points from a vertex to that vertex itself. */
  def isSelfLoop: Boolean = source == target

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
    case Some(fieldId) => fieldId.typ
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
    * @note The method used to require that the given value field identifier is
    *       contained in `target.typ.nonObjectFields`. To allow tracking the
    *       value of ghost fields that are not in `target.typ.nonObjectFields`,
    *       the requirement has been dropped.
   */
  def createTargetEdgeLocalId(valueField: Identifier): Edge[S] = {
    val edgeLocalId = EdgeLocalIdentifier(List(field), valueField)
    copy(state = state.createVariable(edgeLocalId))
      .assumeEdgeLocalIdEquality(edgeLocalId)
  }

  /** Create an `EdgeLocalIdentifier` in the edge state for each value field
    * of the target vertex.
    */
  def createTargetEdgeLocalIds(): Edge[S] =
    target.typ.nonObjectFields.foldLeft(this)(_.createTargetEdgeLocalId(_))

  /** Creates an `EdgeLocalIdentifier` in the edge state for a given value field
    * of the source vertex.
    *
    * Assumes "eLocId.valueField == source.valueField" on the state.
    * Note that this assumption only takes effect on definite heap vertices.
    *
    * @param valueField the identifier of the value field of the source vertex
    * @return the resulting edge
    * @note The method used to require that the given value field identifier is
    *       contained in `source.typ.nonObjectFields`. To allow tracking the
    *       value of ghost fields that are not in `source.typ.nonObjectFields`,
    *       the requirement has been dropped.
    */
  def createSourceEdgeLocalId(valueField: Identifier): Edge[S] = {
    source match {
      case obj: HeapVertex =>
        val edgeLocalId = EdgeLocalIdentifier(valueField)
        copy(state = state.createVariable(edgeLocalId))
          .assumeEdgeLocalIdEquality(edgeLocalId)
      case _ => this
        // Do not create source edge-local ids for local variable vertices
    }
  }

  /** Create an `EdgeLocalIdentifier` in the edge state for each value field
    * of the source vertex.
    */
  def createSourceEdgeLocalIds(): Edge[S] = {
    source.typ.nonObjectFields.foldLeft(this)(_.createSourceEdgeLocalId(_))
  }

  def createEdgeLocalIds(): Edge[S] = {
    this
      .createSourceEdgeLocalIds()
      .createTargetEdgeLocalIds()
  }

  /** Assume that the value of the given `EdgeLocalIdentifier` is the same
    * as the value of the corresponding `ValueHeapIdentifier`.
    *
    * If there is no such `ValueHeapIdentifier` present in the edge state,
    * the state of the edge remains unchanged.
    */
  def assumeEdgeLocalIdEquality(edgeLocalId: EdgeLocalIdentifier): Edge[S] = {
    require(state.edgeLocalIds.contains(edgeLocalId),
      "edge local identifier must be part of the edge state")

    accPathToVertex(edgeLocalId.accPath) match {
      case vertex: HeapVertex =>
        val valueHeapId = ValueHeapIdentifier(vertex, edgeLocalId.field)
        if (state.ids.contains(valueHeapId))
          copy(state = state.assume(BinaryArithmeticExpression(valueHeapId,
            edgeLocalId, ArithmeticOperator.==)))
        else this
      case _ => this
        // Support edge-local identifiers on null edges
    }
  }

  /** Assume that the value of each `EdgeLocalIdentifier` in the edge state
    * is the same as the value of the corresponding `ValueHeapIdentifier`.
    */
  def assumeEdgeLocalIdEqualities(): Edge[S] =
    state.edgeLocalIds.foldLeft(this)(_.assumeEdgeLocalIdEquality(_))
}