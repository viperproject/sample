package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{Type, DummyProgramPoint}
import ch.ethz.inf.pm.sample.util.Predef._

case class EdgeWithState[S <: SemanticDomain[S]](
    source: Vertex,
    state: S,
    field: Option[String],
    target: Vertex) extends Ordered[EdgeWithState[S]] {

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
  def weakEquals(other: EdgeWithState[S]): Boolean =
    source == other.source && target == other.target && field == other.field

  /** The set of vertices incident to the edge. */
  def vertices: Set[Vertex] = Set(source, target)

  /** Compare lexicographically by source, field and target. */
  override def compare(that: EdgeWithState[S]): Int = {
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
  def createTargetEdgeLocalId(valueField: Identifier): EdgeWithState[S] = {
    require(target.isInstanceOf[HeapVertex],
      "target vertex must be a heap vertex")
    require(target.typ.nonObjectFields.contains(valueField),
      s"target vertex has no value field $valueField")

    val edgeLocalId = EdgeLocalIdentifier(List(field).flatten, valueField)
    val valueHeapId = ValueHeapIdentifier(target.asInstanceOf[HeapVertex], valueField)
    val newState = state
      .createVariable(edgeLocalId, edgeLocalId.getType)
      .assume(new BinaryArithmeticExpression(valueHeapId, edgeLocalId, ArithmeticOperator.==, null))

    copy(state = newState)
  }

  /** Create an `EdgeLocalIdentifier` in the edge state for each value field
    * of the target vertex.
    */
  def createTargetEdgeLocalIds(): EdgeWithState[S] = {
    require(target.isInstanceOf[HeapVertex],
      "target vertex must be a heap vertex")
    target.typ.nonObjectFields.foldLeft(this)(_.createTargetEdgeLocalId(_))
  }

  /** Create an `EdgeLocalIdentifier` in the edge state for
    * the given value field of the source vertex.
    */
  def createSourceEdgeLocalId(valueField: Identifier): EdgeWithState[S] = {
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
  def createSourceEdgeLocalIds(): EdgeWithState[S] = {
    require(source.isInstanceOf[HeapVertex],
      "source vertex must be a heap vertex")

    copy(state = source.asInstanceOf[HeapVertex]
      .createEdgeLocalIdsInState(state))
  }

  def createEdgeLocalIds(): EdgeWithState[S] = {
    var result = this
    if (source.isInstanceOf[HeapVertex])
      result = result.createSourceEdgeLocalIds()
    if (target.isInstanceOf[HeapVertex])
      result = result.createTargetEdgeLocalIds()
    result
  }
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

  import Utilities._

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

  /** The condition satisfied by this path. */
  lazy val condition: S = {
    /**
     * Inner helper method for computing the condition recursively.
     *
     * @param path to be processed
     * @param state starting state where are only the edge-local identifiers
     *              with empty sequence of field access that represent targets
     */
    def recurse(path: List[EdgeWithState[S]], state: S): S = {
      val stateEdgeLocalIds = edgeLocalIds(state)

      // Only the edge-local identifiers that refer to target are present in
      // the given state (i.e. the once with empty sequence of field accesses)
      assert(stateEdgeLocalIds.forall(_.accPath.isEmpty))

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
      val edgeLocalIdsToAdd = edgeLocalIds(edge.state).filter(!_.accPath.isEmpty)
      var newState: S = state.createVariables(edgeLocalIdsToAdd.toSet[Identifier])
      newState = newState.glb(edge.state)

      // Now, we need to rename source-edge local identifiers to the ones
      // that are target of this edge and remove any others.
      val originalSourceIds = edgeLocalIds(newState).filter(_.accPath.isEmpty)
      newState = newState.removeVariables(originalSourceIds)

      // Renaming
      val idsToRenameToSource = edgeLocalIds(newState).filter(_.accPath == List(field))

      // Building lists for renaming
      var renameFrom = List.empty[EdgeLocalIdentifier]
      var renameTo = List.empty[EdgeLocalIdentifier]
      for (elId <- idsToRenameToSource) {
        renameFrom = elId :: renameFrom
        renameTo = elId.copy(accPath = List.empty)(elId.pp) :: renameTo
      }
      newState = newState.rename(renameFrom, renameTo)

      // Now we remove all edge-local identifiers that can not be the targets.
      val elIdsToRemove = newState.getIds().filter(_.isInstanceOf[EdgeLocalIdentifier]) -- renameTo
      newState = newState.removeVariables(elIdsToRemove.toSet[Identifier])

      recurse(path.tail, newState)
    }

    // The head of the path (edge sequence) is starting from a variable.
    // Therefore, the edge local variables that represent the target edge-local
    // variables have an empty sequence of fields. However, we need to remove
    // all other edge-local identifier that might be possibly present.
    val elIdsToRemove = edgeLocalIds(edges.head.state).filter(!_.accPath.isEmpty)
    recurse(edges.tail, edges.head.state.removeVariables(elIdsToRemove))
  }
}