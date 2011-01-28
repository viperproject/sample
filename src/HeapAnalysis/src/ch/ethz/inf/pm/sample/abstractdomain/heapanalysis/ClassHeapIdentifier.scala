package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.abstractdomain.HeapIdentifier;
import ch.ethz.inf.pm.sample.oorepresentation.Type;
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint;


class FieldHeapIdentifier(val obj : Type, val field : String, value : Type) extends ClassHeapIdentifier(value) {
  override def getField() = throw new Exception("Not supported")
	override def isNormalized() : Boolean = false;
    override def toString() : String = obj.getName()+"."+field;
    override def clone() : Object = new FieldHeapIdentifier(obj, field, value);
    override def hashCode() : Int = field.hashCode();
    override def equals(a : Any) : Boolean = a match {
      case a1 : FieldHeapIdentifier => this.obj.equals(a1.obj) && this.field.equals(a1.field);
      case _ => false;
    }
}

class ClassNullNodeHeapIdentifier(typ : Type) extends ClassHeapIdentifier(typ) {
    override def toString() : String = "null"
}

class ClassHeapIdentifier(val value : Type) extends NonRelationalHeapIdentifier[ClassHeapIdentifier](value) {
	override def getNullNode() = new ClassNullNodeHeapIdentifier(value.top()); 
	override def getField() = throw new Exception("Not supported")
	override def isNormalized() : Boolean = true;
    override def factory() : ClassHeapIdentifier=new ClassHeapIdentifier(this.value);
    override def extractField(h : ClassHeapIdentifier, s : String, t : Type) : ClassHeapIdentifier=new FieldHeapIdentifier(h.value, s, t);
    override def accessStaticObject(t : Type) : ClassHeapIdentifier=new ClassHeapIdentifier(t);
    override def createAddress(t : Type, p : ProgramPoint) : ClassHeapIdentifier=new ClassHeapIdentifier(t);
    override def createAddressForParameter(t : Type) : ClassHeapIdentifier=
    	new ClassHeapIdentifier(t);
    override def representSingleVariable() : Boolean=false;
    override def getName() : String=value.getName();
    override def clone() : Object =new ClassHeapIdentifier(this.value);
    override def equals(a : Any) :  Boolean = a match {
      case a1 : FieldHeapIdentifier => false;
      case a1 : ClassHeapIdentifier => this.value.equals(a1.value);
      case _ => false;
    }
    override def hashCode() : Int = 1;
    override def toString() : String = return this.getName();
}
 