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
    case x : CollectionIdentifier => new FieldAndProgramPoint(x, s, t);
    case x : CollectionSummaryIdentifier => new FieldAndProgramPoint(x, s, t);
    case _ => throw new SemanticException("Not allowed");
  }
  override def accessStaticObject(t : Type, pp : ProgramPoint) : ProgramPointHeapIdentifier=new StaticProgramPointHeapIdentifier(t, pp);
  override def createAddress(t : Type, p : ProgramPoint) : ProgramPointHeapIdentifier=new SimpleProgramPointHeapIdentifier(p, t);
  override def createAddressForArgument(t : Type, p : ProgramPoint) : ProgramPointHeapIdentifier=
	  if(NonRelationalHeapDomainSettings.unsoundEntryState)
	 	  new UnsoundParameterHeapIdentifier(t, Math.min(ParameterIds.get(SystemParameters.currentMethod), NonRelationalHeapDomainSettings.maxInitialNodes), p);
	  else new ParameterHeapIdentifier(t, p);
  override def hashCode() : Int = 1;  
  override def getNullNode(p : ProgramPoint) = new NullProgramPointHeapIdentifier(t.top(), p);
  override def getName() : String=this.toString();
  override def isNormalized() : Boolean = true;
  override def getArrayCell(array : Assignable, index : Expression) = new ArrayTopIdentifier();
  override def getArrayLength(array : Assignable) = new ArrayTopIdentifier();

  override def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, pp:ProgramPoint) =
    new CollectionIdentifier(pp,collTyp,keyTyp,valueTyp,lengthTyp)

  override def getCollectionCell(collection: Assignable, index:Expression) = collection match {
    case x:CollectionIdentifier => new CollectionSummaryIdentifier(x)
    case _ => throw new SemanticException("This is not a collection: "+collection.toString)
  }

  override def getCollectionLength(collection: Assignable) = collection match {
      case x:CollectionIdentifier => new CollectionLengthIdentifier(x)
      case _ => throw new SemanticException("This is not a collection: "+collection.toString)
  }

}

case class NullProgramPointHeapIdentifier(t2 : Type, pp1 : ProgramPoint) extends ProgramPointHeapIdentifier(t2, pp1) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case NullProgramPointHeapIdentifier(t3, pp) => return true
    case _ => return false
  }
  override def toString() : String = "null"
  
  override def factory() : ProgramPointHeapIdentifier=new NullProgramPointHeapIdentifier(getType(), this.getProgramPoint);
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

case class CollectionIdentifier(override val pp:ProgramPoint,collTyp:Type,keyTyp:Type,valueTyp:Type,lengthTyp:Type) extends ProgramPointHeapIdentifier(collTyp, pp) {
  def getField() : Option[String] = None
  override def isNormalized() : Boolean = true
  override def equals(x : Any) : Boolean = x match {
    case CollectionIdentifier(ppX,collTypX,keyTypX,valueTypX,lengthTypX) => (pp==ppX && collTyp==collTypX
      && keyTyp==keyTypX && valueTyp==valueTypX && lengthTyp == lengthTypX)
    case _ => false
  }
  override def toString() : String = "Collection("+collTyp.toString+","+pp.toString+")"
  override def factory() : ProgramPointHeapIdentifier = new CollectionIdentifier(pp,collTyp,keyTyp,valueTyp,lengthTyp)
  override def representSingleVariable() : Boolean=false
  override def clone() : Object = new CollectionIdentifier(pp,collTyp,keyTyp,valueTyp,lengthTyp)
}

case class CollectionLengthIdentifier(coll:CollectionIdentifier) extends ProgramPointHeapIdentifier(coll.lengthTyp, coll.pp) {
  def getField() : Option[String] = None
  override def isNormalized() : Boolean = true
  override def equals(x : Any) : Boolean = x match {
    case CollectionLengthIdentifier(collX) => (coll.equals(collX))
    case _ => false
  }
  override def toString() : String = "Length("+coll.pp+")"
  override def factory() : ProgramPointHeapIdentifier=new CollectionLengthIdentifier(coll)
  override def representSingleVariable():Boolean=false
  override def clone() : Object = new CollectionLengthIdentifier(coll)
}

case class CollectionSummaryIdentifier(coll:CollectionIdentifier) extends ProgramPointHeapIdentifier(coll.valueTyp, coll.pp) {
  def getField() : Option[String] = None
  override def isNormalized() : Boolean = false
  override def equals(x : Any) : Boolean = x match {
    case CollectionSummaryIdentifier(collX) => (coll.equals(collX))
    case _ => false
  }
  override def toString() : String = "Summary("+coll.pp+")"
  override def factory() : ProgramPointHeapIdentifier=new CollectionSummaryIdentifier(coll)
  override def representSingleVariable() : Boolean=false
  override def clone() : Object = new CollectionSummaryIdentifier(coll)
}

case class ArrayTopIdentifier() extends ProgramPointHeapIdentifier(null, null) {
  def getField() : Option[String] = None;
  override def isNormalized() : Boolean = true;
  override def equals(x : Any) : Boolean = x match {
    case ArrayTopIdentifier() => return true
    case _ => return false
  }
  override def toString() : String = "Array ID"

  override def factory() : ProgramPointHeapIdentifier=new ArrayTopIdentifier();
  override def representSingleVariable() : Boolean=false;
  override def clone() : Object =new ArrayTopIdentifier();
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