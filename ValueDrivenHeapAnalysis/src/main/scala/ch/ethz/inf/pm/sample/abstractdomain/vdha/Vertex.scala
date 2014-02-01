package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.SystemParameters
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
  override def compare(that: Vertex): Int = toString.compareTo(that.toString)
}

case class LocalVariableVertex(name: String)(val typ: Type) extends Vertex {
  def label = name
  override def toString = name
}

object LocalVariableVertex {
  /** Creates a new local variable vertex from a local variable. */
  def apply(localVar: VariableIdentifier): LocalVariableVertex =
    LocalVariableVertex(localVar.name)(localVar.typ)
}

object NullVertex extends Vertex {
  def name = VertexConstants.NULL
  def label = VertexConstants.NULL
  def typ = SystemParameters.getType().bottom()
  override def toString = name
}

trait HeapVertex extends Vertex {
  require(version >= 0)

  val version: Int
  def name = s"n$version"
}

case class SummaryHeapVertex(version: Int)(val typ: Type) extends HeapVertex {
  def label = VertexConstants.SUMMARY
}

case class DefiniteHeapVertex(version: Int)(val typ: Type) extends HeapVertex {
  def label = VertexConstants.DEFINITE
}