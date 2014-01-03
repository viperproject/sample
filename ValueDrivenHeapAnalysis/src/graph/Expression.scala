package graph

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.{Expression, Identifier}

case class ValueHeapIdentifier(
    obj: HeapVertex,
    field: String,
    typ: Type,
    pp: ProgramPoint) extends Identifier(typ, pp) {

  /**
   * Returns the name of the identifier. We suppose that if two identifiers
   * return the same name if and only if they are the same identifier.
   * @return The name of the identifier
   */
  def getName: String = s"${obj.name}.$field"

  /**
   * Returns the name of the field that is represented by this identifier
   * if it is a heap identifier.
   * @return The name of the field pointed by this identifier
   */
  def getField: Option[String] = Some(field)

  /**
   * Since an abstract identifier can be an abstract node of the heap,
   * it can represent more than one concrete identifier.
   * This function tells if a node is a summary node.
   * @return true iff this identifier represents exactly one variable
   */
  def representsSingleVariable(): Boolean = obj.isInstanceOf[DefiniteHeapVertex]

  override def hashCode(): Int = toString.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case x: ValueHeapIdentifier =>
      this.toString().equals(x.toString())
    case _ => false
  }
}

object ValueHeapIdentifier {
  /** Creates a value heap identifier from a heap vertex and a field identifier. */
  def apply(obj: HeapVertex, field: Identifier): ValueHeapIdentifier =
    ValueHeapIdentifier(obj, field.getName, field.getType, field.getProgramPoint)
}

case class EdgeLocalIdentifier(
    accPath: List[String],
    field: String,
    typ: Type)(
    pp: ProgramPoint) extends Identifier(typ, pp) {

  require(!typ.isObject(), "EdgeLocalIdentifier should represent value information.")
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
    EdgeLocalIdentifier(accPath, field.getName, field.getType)(field.getProgramPoint)

  /** Creates an edge-local identifier with an empty access path from a field identifier. */
  def apply(field: Identifier): EdgeLocalIdentifier =
    apply(List.empty, field)
}

case class VertexExpression(typ: Type, vertex: Vertex)(pp: ProgramPoint) extends Expression {

  def getProgramPoint = pp

  def getType: Type = typ

  def getIdentifiers: Set[Identifier] =
    throw new Exception("getIdentifiers() should never be called!")

  override def toString: String = vertex.name

  def transform(f: (Expression) => Expression): Expression = f(this)
}