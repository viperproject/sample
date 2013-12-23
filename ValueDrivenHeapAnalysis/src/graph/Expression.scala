package graph

import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.{Expression, Identifier}

case class ValueHeapIdentifier(obj: HeapVertex, field: String, typ1 : Type, pp : ProgramPoint) extends Identifier(typ1, pp) {
  /**
  Returns the name of the identifier. We suppose that if two identifiers return the same name if and only
   if they are the same identifier
   @return The name of the identifier
    */
  def getName: String = obj.name + "." + field

  /**
  Returns the name of the field that is represented by this identifier if it is a heap identifier.

   @return The name of the field pointed by this identifier
    */
  def getField(): Option[String] = Some(field)

  /**
  Since an abstract identifier can be an abstract node of the heap, it can represent more than one concrete
   identifier. This function tells if a node is a summary node.

   @return true iff this identifier represents exactly one variable
    */
  def representsSingleVariable(): Boolean = obj.isInstanceOf[DefiniteHeapVertex]

  override def toString(): String = getName

  override def hashCode(): Int = {
    return toString.hashCode()
  }

  override def equals(obj : Any): Boolean = obj match {
    case x : ValueHeapIdentifier =>
      this.toString().equals(x.toString())
    case _ => false
  }
}

object ValueHeapIdentifier {
  /** Creates a value heap identifier from a heap vertex and a field identifier. */
  def apply(obj: HeapVertex, field: Identifier): ValueHeapIdentifier =
    ValueHeapIdentifier(obj, field.getName, field.getType, field.getProgramPoint)
}

case class EdgeLocalIdentifier(accPath: List[String],val field: String, typ1: Type, pp: ProgramPoint) extends Identifier(typ1, pp) {

  assert(!typ1.isObject(), "EdgeLocalIdentifier should represent value information.")
  assert(accPath.size <= 1, "For now, we allow at most single step look-ahead.")

  /**
  Returns the name of the identifier. We suppose that if two identifiers return the same name if and only
   if they are the same identifier
   @return The name of the identifier
    */
  def getName: String = {
    var result = ValeDrivenHeapStateConstants.edgeLocalIdentifier
    for (ap <- accPath)
      result = result + "." + ap
    result = result + "." + field
    return result
  }

  /**
  Returns the name of the field that is represented by this identifier if it is a heap identifier.

   @return The name of the field pointed by this identifier
    */
  def getField(): Option[String] = Some(field)

  /**
  Edge-local identifier always represents a field of a single object. Hence, this method always returns true.

   @return true
    */
  def representsSingleVariable(): Boolean = true

  override def toString(): String = getName

  override def hashCode(): Int = {
    return toString.hashCode()
  }

  override def equals(obj : Any): Boolean = obj match {
    case x: EdgeLocalIdentifier =>
      this.toString().equals(x.toString())
    case _ => false
  }
}

object EdgeLocalIdentifier {
  /** Creates an edge-local identifier from an access path and a field identifier. */
  def apply(accPath: List[String], field: Identifier): EdgeLocalIdentifier =
    EdgeLocalIdentifier(accPath, field.getName, field.getType, field.getProgramPoint)
}

case class VertexExpression(pp : ProgramPoint, typ: Type, vertex: Vertex) extends Expression {

  def getProgramPoint = pp
  def getType: Type = typ
  def getIdentifiers: Set[Identifier] = throw new Exception("getIdentifiers() should never be called!")

  override def toString(): String = vertex.name

  override def hashCode(): Int = {
    return toString.hashCode()
  }

  override def equals(obj : Any): Boolean = obj match {
    case VertexExpression(_, objTyp, objVertex) =>
      this.typ.equals(objTyp) && this.vertex.equals(objVertex)
    case _ => false
  }

  /**
   * Runs f on the expression and all sub-expressions
   *
   * This also replaces identifiers inside heap ID sets.
   *
   * @param f the transformer
   * @return the transformed expression
   */
  def transform(f: (Expression) => Expression): Expression = ???
}