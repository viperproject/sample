package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.SystemParameters

object VertexConstants {
  val SUMMARY = "sum"
  val DEFINITE = "def"
  val NULL = "null"
}

abstract class Vertex(val name: String, val label: String, val typ: Type) extends Ordered[Vertex]{

  override def toString(): String = {
    return "(" + name.toString + ", " + label + ")"
  }

  // For now, the order is lexicographical but can be overriden to other orders
  override def compare(that: Vertex): Int = this.toString.compareTo(that.toString)

  override def hashCode(): Int = {
    return toString.hashCode()
  }

  override def equals(obj : Any): Boolean = {
    if (!obj.isInstanceOf[Vertex])
      return false
    return name.equals(obj.asInstanceOf[Vertex].name) && label.equals(obj.asInstanceOf[Vertex].label)
  }
}

class LocalVariableVertex(name: String, typ: Type) extends Vertex(name, name, typ) {
  override def toString(): String = name
}

//class NullVertex extends Vertex(VertexConstants.NULL, VertexConstants.NULL, null) {
class NullVertex extends Vertex(VertexConstants.NULL, VertexConstants.NULL, SystemParameters.getType().top()) {
  override def toString(): String = name
}

abstract class HeapVertex(label: String, val version: Int, typ: Type) extends Vertex("n"+version, label, typ: Type) {

  assert(version >= 0)

  def getVersion() : Int = version
}

class SummaryHeapVertex(version: Int, typ: Type) extends HeapVertex(VertexConstants.SUMMARY, version, typ)

class DefiniteHeapVertex(version: Int, typ: Type) extends HeapVertex(VertexConstants.DEFINITE, version, typ)

