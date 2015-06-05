package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis


import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.abstractdomain.{SemanticException, Expression, Assignable}
;


class FieldHeapIdentifier(val obj: Type, val field: String, value: Type, pp: ProgramPoint) extends ClassHeapIdentifier(value, pp) {
  override def getLabel() = "Class"

  override def getField = throw new Exception("Not supported")

  override def isNormalized(): Boolean = false

  override def toString: String = obj.name + "." + field

  override def clone(): Object = new FieldHeapIdentifier(obj, field, value, pp)

  override def hashCode(): Int = field.hashCode()

  override def equals(a: Any): Boolean = a match {
    case a1: FieldHeapIdentifier => this.obj.equals(a1.obj) && this.field.equals(a1.field);
    case _ => false;
  }
}

class ClassNullNodeHeapIdentifier(typ: Type, pp: ProgramPoint) extends ClassHeapIdentifier(typ, pp) {
  override def toString: String = "null"
}

class ClassHeapIdentifier(val value: Type, val pp: ProgramPoint)
  extends NonRelationalHeapIdentifier[ClassHeapIdentifier] {

  val typ = value

  override def getLabel() = "Class"

  override def getNullNode(p: ProgramPoint) = new ClassNullNodeHeapIdentifier(value.top(), p)

  override def getField = throw new Exception("Not supported")

  override def isNormalized(): Boolean = true

  override def factory(): ClassHeapIdentifier = new ClassHeapIdentifier(this.value, this.pp)

  override def extractField(h: ClassHeapIdentifier, s: String, t: Type): ClassHeapIdentifier = new FieldHeapIdentifier(h.value, s, t, h.pp)

  override def accessStaticObject(t: Type, p: ProgramPoint): ClassHeapIdentifier = new ClassHeapIdentifier(t, p)

  override def createAddress(t: Type, p: ProgramPoint): ClassHeapIdentifier = new ClassHeapIdentifier(t, p)

  override def createAddressForArgument(t: Type, p: ProgramPoint): ClassHeapIdentifier = new ClassHeapIdentifier(t, p)

  override def representsSingleVariable: Boolean = false

  override def getName: String = value.name

  override def clone(): Object = new ClassHeapIdentifier(this.value, this.pp)

  override def equals(a: Any): Boolean = a match {
    case a1: FieldHeapIdentifier => false;
    case a1: ClassHeapIdentifier => this.value.equals(a1.value);
    case _ => false;
  }

  override def getArrayCell(array: Assignable, index: Expression) = throw new SemanticException("Not yet supported")

  override def getArrayLength(array: Assignable) = throw new SemanticException("Not yet supported")

  override def hashCode(): Int = 1

  override def toString: String = this.getName

  override def toSummaryNode: ClassHeapIdentifier = this

  override def toNonSummaryNode: ClassHeapIdentifier = this

  override def getReachableFromId: Option[ClassHeapIdentifier] = None

  override def getCounter = 0

  override def setCounter(c: Int) = this

  def createNonDeterminismSource(typ: Type, pp: ProgramPoint, multiple: Boolean): ClassHeapIdentifier = ???
}