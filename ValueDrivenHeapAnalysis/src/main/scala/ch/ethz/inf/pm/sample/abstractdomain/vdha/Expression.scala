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

case class EdgeLocalIdentifier
    (accPath: List[String], field: String, typ: Type)
    (val pp: ProgramPoint)
  extends Identifier {

  require(!typ.isObject, "EdgeLocalIdentifier should represent value information.")
  require(accPath.size <= 1, "For now, we allow at most single step look-ahead.")

  def getName: String = {
    val fullPath = List(ValueDrivenHeapStateConstants.edgeLocalIdentifier) ++ accPath ++ List(field)
    fullPath.mkString(".")
  }

  def getField: Option[String] = Some(field)

  /** An edge-local identifier always represents a field of a single object. */
  def representsSingleVariable(): Boolean = true
}

object EdgeLocalIdentifier {
  /** Creates an edge-local identifier from an access path and a field identifier. */
  def apply(accPath: List[String], field: Identifier): EdgeLocalIdentifier =
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