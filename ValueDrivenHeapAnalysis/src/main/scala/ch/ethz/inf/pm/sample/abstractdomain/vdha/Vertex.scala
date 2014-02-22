package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier

object VertexConstants {
  val SUMMARY = "sum"
  val DEFINITE = "def"
  val NULL = "null"
}

trait Vertex extends Ordered[Vertex] {
  def name: String

  // TODO: It seems odd that local variable vertices and null vertices
  // also need a label, as it is identical to their name
  def label: String

  def typ: Type

  override def toString = s"($name, $label)"

  /** Orders vertices such that local variable vertices come first,
    * heap vertices after and null vertices at the end. Two vertices
    * of the same type are ordered lexicographically by their name.*/
  override def compare(that: Vertex): Int = {
    def vertexGroup(v: Vertex): Int = v match {
      case _: LocalVariableVertex => 0
      case _: HeapVertex => 1
      case NullVertex => 2
    }

    val thisGroup = vertexGroup(this)
    val thatGroup = vertexGroup(that)
    if (thisGroup == thatGroup)
      name.compareTo(that.name)
    else
      thisGroup.compareTo(thatGroup)
  }

  /** Returns all fields and corresponding types for which
    * this vertex needs to have an out-going edge for.
    */
  def neededEdgeFieldsAndTypes: Set[(Option[String], Type)]

  /** Returns all fields for which this vertex needs to have
    * an out-going edge for.
    */
  def neededEdgeFields: Set[Option[String]] =
    neededEdgeFieldsAndTypes.map(_._1)
}

object Vertex {
  /**
   * Builds a map of `ValueHeapIdentifier`s from the given `Vertex` map.
   *
   * For example, when the vertex map contains `n0` -> `n1` and the vertices
   * have the value field 'val', then the resulting map contains
   * `n0.val` -> `n1.val`.
   */
  def vertexMapToValueHeapIdMap(vertexMap: Map[Vertex, Vertex]):
      Map[ValueHeapIdentifier, ValueHeapIdentifier] = {
    vertexMap.collect({
      case (from: HeapVertex, to: HeapVertex) =>
        assert(from.typ == to.typ)
        from.typ.nonObjectFields.map(field =>
          ValueHeapIdentifier(from, field) -> ValueHeapIdentifier(to, field))
    }).flatten.toMap
  }
}

case class LocalVariableVertex(name: String)(val typ: Type) extends Vertex {
  def label = name

  override def toString = name

  def neededEdgeFieldsAndTypes = Set((None, typ))
}

object LocalVariableVertex {
  /** Creates a new local variable vertex from a local variable. */
  def apply(localVar: VariableIdentifier): LocalVariableVertex =
    LocalVariableVertex(localVar.name)(localVar.typ)
}

object NullVertex extends Vertex {
  def name = VertexConstants.NULL

  def label = VertexConstants.NULL

  // Make it possible to set the type to something other than bottom.
  // There is actually a dedicated Null type in Scala and other type systems.
  var typ = SystemParameters.getType().bottom()

  override def toString = name

  def neededEdgeFieldsAndTypes = Set.empty
}

trait HeapVertex extends Vertex {
  require(version >= 0)

  val version: Int
  def name = s"n$version"

  /** Returns the set of all value heap identifiers of this heap vertex. */
  def valueHeapIds[I >: Identifier]: Set[I] =
    typ.nonObjectFields.map(ValueHeapIdentifier(this, _))

  def neededEdgeFieldsAndTypes =
    typ.objectFields.map(f => (Some(f.getName), f.typ))
}

case class SummaryHeapVertex(version: Int)(val typ: Type) extends HeapVertex {
  def label = VertexConstants.SUMMARY
}

case class DefiniteHeapVertex(version: Int)(val typ: Type) extends HeapVertex {
  def label = VertexConstants.DEFINITE
}