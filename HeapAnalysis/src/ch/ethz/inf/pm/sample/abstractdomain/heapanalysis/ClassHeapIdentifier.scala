package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.abstractdomain.HeapIdentifier;
import ch.ethz.inf.pm.sample.oorepresentation.Type;
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint;


class FieldHeapIdentifier(val obj : Type, val field : String, value : Type, pp : ProgramPoint) extends ClassHeapIdentifier(value, pp) {
  override def getLabel() = "Class";
  override def getField() = throw new Exception("Not supported")
	override def isNormalized() : Boolean = false;
    override def toString() : String = obj.getName()+"."+field;
    override def clone() : Object = new FieldHeapIdentifier(obj, field, value, pp);
    override def hashCode() : Int = field.hashCode();
    override def equals(a : Any) : Boolean = a match {
      case a1 : FieldHeapIdentifier => this.obj.equals(a1.obj) && this.field.equals(a1.field);
      case _ => false;
    }
}

class ClassNullNodeHeapIdentifier(typ : Type, pp : ProgramPoint) extends ClassHeapIdentifier(typ, pp) {
    override def toString() : String = "null"
}

class ClassHeapIdentifier(val value : Type, pp : ProgramPoint) extends NonRelationalHeapIdentifier[ClassHeapIdentifier](value, pp) {
  override def getLabel() = "Class";
	override def getNullNode(p : ProgramPoint) = new ClassNullNodeHeapIdentifier(value.top(), p);
	override def getField() = throw new Exception("Not supported")
	override def isNormalized() : Boolean = true;
    override def factory() : ClassHeapIdentifier=new ClassHeapIdentifier(this.value, this.getProgramPoint);
    override def extractField(h : ClassHeapIdentifier, s : String, t : Type) : ClassHeapIdentifier=new FieldHeapIdentifier(h.value, s, t, h.getProgramPoint);
    override def accessStaticObject(t : Type, p : ProgramPoint) : ClassHeapIdentifier=new ClassHeapIdentifier(t, p);
    override def createAddress(t : Type, p : ProgramPoint) : ClassHeapIdentifier=new ClassHeapIdentifier(t, p);
    override def createAddressForParameter(t : Type, p : ProgramPoint) : ClassHeapIdentifier=
    	new ClassHeapIdentifier(t, p);
    override def representSingleVariable() : Boolean=false;
    override def getName() : String=value.getName();
    override def clone() : Object =new ClassHeapIdentifier(this.value, this.getProgramPoint);
    override def equals(a : Any) :  Boolean = a match {
      case a1 : FieldHeapIdentifier => false;
      case a1 : ClassHeapIdentifier => this.value.equals(a1.value);
      case _ => false;
    }
    override def hashCode() : Int = 1;
    override def toString() : String = return this.getName();
}
 