package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.SystemParameters
import scala.Some

object ParameterIds {
  var map = Map.empty[String, Int];
  def get(method : String) : Int = map.get(method) match {
    case None => map=map+((method, 1)); return 0;
    case Some(x) => map=map+((method, x+1)); return x;
  }
  def reset() : Unit = map=Map.empty[String, Int];
}

sealed abstract class ProgramPointHeapIdentifier(t : Type, pp1 : ProgramPoint) extends NonRelationalHeapIdentifier[ProgramPointHeapIdentifier](t, pp1) {


  override def getLabel() = "Program point";
  override def extractField(h : ProgramPointHeapIdentifier, s : String, t : Type) : ProgramPointHeapIdentifier=h match {
    case x : SimpleProgramPointHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : ParameterHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : StaticProgramPointHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : UnsoundParameterHeapIdentifier => new FieldAndProgramPoint(x, s, t);
    case _ => throw new Exception("Not allowed");
  }
  override def accessStaticObject(t : Type, pp : ProgramPoint) : ProgramPointHeapIdentifier=new StaticProgramPointHeapIdentifier(t, pp);
  override def createAddress(t : Type, p : ProgramPoint) : ProgramPointHeapIdentifier=new SimpleProgramPointHeapIdentifier(p, t);
  override def createAddressForParameter(t : Type, p : ProgramPoint) : ProgramPointHeapIdentifier=
	  if(NonRelationalHeapDomainSettings.unsoundEntryState)
	 	  new UnsoundParameterHeapIdentifier(t, Math.min(ParameterIds.get(SystemParameters.currentMethod), NonRelationalHeapDomainSettings.maxInitialNodes), p);
	  else new ParameterHeapIdentifier(t, p);
  override def hashCode() : Int = 1;  
  override def getNullNode(p : ProgramPoint) = new NullProgramPointHeapIdentifier(t.top(), p);
  override def getName() : String=this.toString();
  override def isNormalized() : Boolean = true;
}

case class NullProgramPointHeapIdentifier(t2 : Type, pp1 : ProgramPoint) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case NullProgramPointHeapIdentifier(t3, pp) => return true
    case _ => return false
  }
  override def toString() : String = "null"
  
  override def factory() : ProgramPointHeapIdentifier=new NullProgramPointHeapIdentifier(getType().top(), this.getProgramPoint);
  override def representSingleVariable() : Boolean=true;
  override def clone() : Object =new NullProgramPointHeapIdentifier(t2, this.getProgramPoint);
}

case class SimpleProgramPointHeapIdentifier(val pp1 : ProgramPoint, t2 : Type) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case SimpleProgramPointHeapIdentifier(pp, t3) => return this.pp.equals(pp)
    case _ => return false
  }
  override def toString() : String = pp.toString()
  
  override def factory() : ProgramPointHeapIdentifier=new SimpleProgramPointHeapIdentifier(this.pp, this.getType());
  override def representSingleVariable() : Boolean=false;//TODO: Improve the precision here!
  override def clone() : Object =new SimpleProgramPointHeapIdentifier(pp, this.getType());
}

case class ParameterHeapIdentifier(t2 : Type, pp1 : ProgramPoint) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case ParameterHeapIdentifier(t3, pp) => return this.t2.equals(t3);
    case _ => return false
  }
  override def representSingleVariable() : Boolean=false//TODO: Improve the precision here
  override def factory() : ProgramPointHeapIdentifier=new ParameterHeapIdentifier(this.getType(), this.getProgramPoint);
  override def toString() : String = "Parameter of type "+this.getType()+""
}

case class UnsoundParameterHeapIdentifier(t2 : Type, n : Int, pp1 : ProgramPoint) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case UnsoundParameterHeapIdentifier(t3, n2, pp) => return this.t2.equals(t3) && this.n ==n2;
    case _ => return false
  }
  override def representSingleVariable() : Boolean= this.n!=NonRelationalHeapDomainSettings.maxInitialNodes
  override def factory() : ProgramPointHeapIdentifier=new UnsoundParameterHeapIdentifier(this.getType(), this.n, this.getProgramPoint);
  override def toString() : String = "Unsound parameter of type "+this.getType()+" number "+this.n;
}

case class StaticProgramPointHeapIdentifier(t2 : Type, pp1 : ProgramPoint) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case StaticProgramPointHeapIdentifier(t2, pp) => return this.getType().equals(t2);
    case _ => return false
  }
  override def representSingleVariable() : Boolean=true
  override def factory() : ProgramPointHeapIdentifier=new StaticProgramPointHeapIdentifier(this.getType(), this.getProgramPoint);
  override def toString() : String = "Static "+this.getType()+""
}

case class FieldAndProgramPoint(val p1 : ProgramPointHeapIdentifier, val field : String, t2 : Type) extends ProgramPointHeapIdentifier(t2, p1.getProgramPoint) {
  def getField() : Option[String] = Some(field);
  override def isNormalized() : Boolean = false;
  override def equals(x : Any) : Boolean = x match {
    case FieldAndProgramPoint(pp1, field1, t2) => return this.p1.equals(pp1) && this.field.equals(field1);
    case _ => return false
    }
  override def representSingleVariable() : Boolean=p1.representSingleVariable();
  override def factory() : ProgramPointHeapIdentifier=new FieldAndProgramPoint(this.p1, this.field, this.getType());
  override def toString() : String = "("+p1.toString()+", "+field+")"
}