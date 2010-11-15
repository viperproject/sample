package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

sealed abstract class ProgramPointHeapIdentifier(val t : Type) extends HeapIdentifier[ProgramPointHeapIdentifier](t) {
  override def extractField(h : ProgramPointHeapIdentifier, s : String, t : Type) : ProgramPointHeapIdentifier=h match {
    case x : SimpleProgramPointHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : ParameterHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : StaticProgramPointHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case _ => throw new Exception("Not allowed");
  }
  override def accessStaticObject(t : Type) : ProgramPointHeapIdentifier=new StaticProgramPointHeapIdentifier(t);
  override def createAddress(t : Type, p : ProgramPoint) : ProgramPointHeapIdentifier=new SimpleProgramPointHeapIdentifier(p, t);
  override def createAddressForParameter(t : Type) : ProgramPointHeapIdentifier=new ParameterHeapIdentifier(t);
  override def hashCode() : Int = 1;  
  override def getNullNode() = new NullProgramPointHeapIdentifier(t.top());
  override def getName() : String=this.toString();
  override def isNormalized() : Boolean = true;
}

case class NullProgramPointHeapIdentifier(t2 : Type) extends ProgramPointHeapIdentifier(t2) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case NullProgramPointHeapIdentifier(t3) => return true
    case _ => return false
  }
  override def toString() : String = "null"
  
  override def factory() : ProgramPointHeapIdentifier=new NullProgramPointHeapIdentifier(t2.top());
  override def representSingleVariable() : Boolean=true;//TODO: Improve the precision here!
  override def clone() : Object =new NullProgramPointHeapIdentifier(t2);
}

case class SimpleProgramPointHeapIdentifier(val pp : ProgramPoint, t2 : Type) extends ProgramPointHeapIdentifier(t2) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case SimpleProgramPointHeapIdentifier(pp, t3) => return this.pp.equals(pp)
    case _ => return false
  }
  override def toString() : String = pp.toString()
  
  override def factory() : ProgramPointHeapIdentifier=new SimpleProgramPointHeapIdentifier(this.pp, this.t);
  override def representSingleVariable() : Boolean=false;//TODO: Improve the precision here!
  override def clone() : Object =new SimpleProgramPointHeapIdentifier(pp, t);
}

case class ParameterHeapIdentifier(t2 : Type) extends ProgramPointHeapIdentifier(t2) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case ParameterHeapIdentifier(t3) => return this.t2.equals(t3);
    case _ => return false
  }
  override def representSingleVariable() : Boolean=false//TODO: Improve the precision here
  override def factory() : ProgramPointHeapIdentifier=new ParameterHeapIdentifier(this.t);
  override def toString() : String = "Parameter of type "+t.toString()+""
}


case class StaticProgramPointHeapIdentifier(t2 : Type) extends ProgramPointHeapIdentifier(t2) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case StaticProgramPointHeapIdentifier(t2) => return this.t.equals(t2);
    case _ => return false
  }
  override def representSingleVariable() : Boolean=true
  override def factory() : ProgramPointHeapIdentifier=new StaticProgramPointHeapIdentifier(this.t);
  override def toString() : String = "Static "+t.toString()+""
}

case class FieldAndProgramPoint(val pp : ProgramPointHeapIdentifier, val field : String, t2 : Type) extends ProgramPointHeapIdentifier(t2) {
  def getField() : Option[String] = Some(field);
  override def isNormalized() : Boolean = false;
  override def equals(x : Any) : Boolean = x match {
    case FieldAndProgramPoint(pp1, field1, t2) => return this.pp.equals(pp1) && this.field.equals(field1);
    case _ => return false
    }
  override def representSingleVariable() : Boolean=pp.representSingleVariable();
  override def factory() : ProgramPointHeapIdentifier=new FieldAndProgramPoint(this.pp, this.field, this.t);
  override def toString() : String = "("+pp.toString()+", "+field+")"
}