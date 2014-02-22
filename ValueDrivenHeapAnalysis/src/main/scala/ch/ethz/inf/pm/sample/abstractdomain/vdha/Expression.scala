package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.{Expression, Identifier}

case class ValueHeapIdentifier
    (obj: HeapVertex, field: String)
    (val typ: Type, val pp: ProgramPoint)
  extends Identifier {

  def getName = s"${obj.name}.$field"

  def getField = Some(field)

  def representsSingleVariable() = obj.isInstanceOf[DefiniteHeapVertex]
}

object ValueHeapIdentifier {
  /** Creates a value heap identifier from a heap vertex and a field identifier. */
  def apply(obj: HeapVertex, field: Identifier): ValueHeapIdentifier =
    ValueHeapIdentifier(obj, field.getName)(field.getType, field.pp)
}

/** Represents a value field of the object pointed from or to by the edge
  * which this edge-local identifier belongs to.
  *
  * An empty access path means that the edge-local identifier
  * refers to a value field of the source object of the corresponding edge.
  *
  * Analogously, a non-empty access path means that the edge-local identifier
  * refers to a value field of the target object of the corresponding edge.
  *
  * Note that edges going out of a local variable vertex are treated in exactly
  * the same way. Such edges have the field `None` and so will the edge-local
  * identifier.
  */
case class EdgeLocalIdentifier
    (accPath: List[Option[String]], field: String, typ: Type)
    (val pp: ProgramPoint)
  extends Identifier {

  require(!typ.isObject, "EdgeLocalIdentifier should represent value information.")
  require(accPath.size <= 1, "For now, we allow at most single step look-ahead.")

  import ValueDrivenHeapStateConstants._

  def getName: String = {
    val fullPath = List(edgeLocalIdentifier) ++ accPath.flatten ++ List(field)
    fullPath.mkString(".")
  }

  def getField: Option[String] = Some(field)

  /** An edge-local identifier always represents a field of a single object. */
  def representsSingleVariable(): Boolean = true

  /** Whether the edge-local identifier refers to a field of the source. */
  def isForSource: Boolean = accPath.isEmpty

  /** Whether the edge-local identifier refers to a field of the target. */
  def isForTarget: Boolean = !accPath.isEmpty
}

object EdgeLocalIdentifier {
  /** Creates an edge-local identifier from an access path and a field identifier. */
  def apply(accPath: List[Option[String]], field: Identifier): EdgeLocalIdentifier =
    EdgeLocalIdentifier(accPath, field.getName, field.getType)(field.pp)

  /** Creates an edge-local identifier with an empty access path from a field identifier. */
  def apply(field: Identifier): EdgeLocalIdentifier =
    apply(List.empty, field)
}

case class VertexExpression(typ: Type, vertex: Vertex)(val pp: ProgramPoint) extends Expression {
  def getType: Type = typ

  def getIdentifiers: Set[Identifier] =
    throw new Exception("getIdentifiers() should never be called!")

  override def toString: String = vertex.name

  def transform(f: (Expression) => Expression): Expression = f(this)
}