package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property.Property
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph

abstract sealed class TopHeapIdentifier(typ : Type, pp : ProgramPoint) extends NonRelationalHeapIdentifier[TopHeapIdentifier](typ, pp) {
    override def getLabel() = "Top";
	  override def getNullNode(pp : ProgramPoint) = new NullHeapIdentifier(typ.top(), pp);
}

case class NullHeapIdentifier(typ2 : Type, pp1 : ProgramPoint) extends TopHeapIdentifier(typ2, pp1) {
	  override def getField() : Option[String] = None;
	  override def isNormalized() : Boolean = true;
	  override def representSingleVariable()=true;
	  override def getName() = "null"
	  override def equals(o : Any) = o match {
	    case x : NullHeapIdentifier => true 
	    case _ => false
	  }
	  override def factory() = this;
      override def createAddress(typ : Type, pp : ProgramPoint)=new SingleHeapIdentifier(typ, pp);
      override def createAddressForParameter(typ : Type, pp : ProgramPoint)=new SingleHeapIdentifier(typ, pp);
      override def extractField(obj : TopHeapIdentifier, field : String, typ : Type)=new SingleHeapIdentifier(typ, obj.getProgramPoint);
      override def accessStaticObject(typ : Type, pp : ProgramPoint)=new StaticHeapIdentifier(typ, pp);
	  override def hashCode() : Int = 0;
}

case class SingleHeapIdentifier(typ2 : Type, pp1 : ProgramPoint) extends TopHeapIdentifier(typ2, pp1) {
	  override def getField() : Option[String] = None;
	  override def isNormalized() : Boolean = true;
	  override def representSingleVariable()=false;
	  override def getName() = "#abstractReference#"
	  override def equals(o : Any) = o match {
	    case x : SingleHeapIdentifier => true 
	    case _ => false
	  }
	  override def factory() = this;
      override def createAddress(typ : Type, pp : ProgramPoint)=this;
      override def createAddressForParameter(typ : Type, pp : ProgramPoint)=this;
      override def extractField(obj : TopHeapIdentifier, field : String, typ : Type)=this;
      override def accessStaticObject(typ : Type, pp : ProgramPoint)=this;
	  override def hashCode() : Int = 0;
}

case class StaticHeapIdentifier(typ2 : Type, pp1 : ProgramPoint) extends TopHeapIdentifier(typ2, pp1) {
	  override def getField() : Option[String] = None;
	  override def isNormalized() : Boolean = true;
	  override def getName() = if(typ.equals(typ.top())) "#abstractReference#" else "#"+typ.getName+"#";
	  override def representSingleVariable()=typ.isStatic
	  override def equals(o : Any) = o match {
	    case x : StaticHeapIdentifier => typ.equals(x.typ)
	    case _ => false
	  }
	  override def factory() = new StaticHeapIdentifier(typ.top(), this.getProgramPoint);
    override def createAddress(typ : Type, pp : ProgramPoint)=new StaticHeapIdentifier(typ.top(), pp);
      override def createAddressForParameter(typ : Type, pp : ProgramPoint)=new StaticHeapIdentifier(typ.top(), pp);
      override def accessStaticObject(typ : Type, pp : ProgramPoint)=StaticHeapIdentifier(typ, pp);
      override def extractField(obj : TopHeapIdentifier, field : String, typ : Type)=new StaticHeapIdentifier(typ.top(), obj.getProgramPoint);
	  override def hashCode() : Int = 0;
	  override def getType() = typ;
}

//Approximates all the concrete references with just one abstract element
class ReallyApproximatedHeapDomain extends HeapDomain[ReallyApproximatedHeapDomain, TopHeapIdentifier] {
  override def reset() : Unit = Unit;
  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = Nil;
  override def getLabel() : String = "Top domain"
  override def parameters() : List[(String, Object)] = Nil
  override def setParameter(label : String, value : Any) : Unit = Unit;
  override def getInitialState() = new ReallyApproximatedHeapDomain();
  override def getProperties() : Set[Property] = Set.empty+ShowGraph;

  private var isBottom = false;
  final override def factory() = top();
  def getStringOfId(id : Identifier) : String = this.toString()
  override def access(field : Identifier)=this;
  override def backwardAccess(field : Identifier)=this;
  override def assign(variable : Identifier, expr : Expression) : ReallyApproximatedHeapDomain = this;
  override def backwardAssign(variable : Identifier, expr : Expression) : ReallyApproximatedHeapDomain = this;
  override def setParameter(variable : Identifier, expr : Expression) : ReallyApproximatedHeapDomain = assign(variable, expr);                                                                                                               
  override def setToTop(variable : Identifier) : ReallyApproximatedHeapDomain = this;
  override def removeVariable(variable : Identifier) : ReallyApproximatedHeapDomain = this;
  override def top() : ReallyApproximatedHeapDomain = this
  override def bottom() : ReallyApproximatedHeapDomain = {
    val result=new ReallyApproximatedHeapDomain();
    result.isBottom=true;
    result;
  }
  override def lub(left : ReallyApproximatedHeapDomain, right : ReallyApproximatedHeapDomain) : ReallyApproximatedHeapDomain = {
    if(left.isBottom && right.isBottom) return bottom();
    else return new ReallyApproximatedHeapDomain();
  }
  override def glb(left : ReallyApproximatedHeapDomain, right : ReallyApproximatedHeapDomain) : ReallyApproximatedHeapDomain = {
    if(left.isBottom || right.isBottom) return bottom();
    else left;
  }
  override def widening(left : ReallyApproximatedHeapDomain, right : ReallyApproximatedHeapDomain) : ReallyApproximatedHeapDomain = lub(left, right)
  override def lessEqual(r : ReallyApproximatedHeapDomain) : Boolean = true
  
  override def createVariable(id : Identifier, typ : Type)=this
  override def createVariableForParameter(id : Identifier, typ : Type, path : List[String])={
    var result = Map.empty[Identifier, List[String]];
    result=result+((id, path ::: id.toString() :: Nil));
    (this, result);
  } 
  override def createObject(typ : Type, pp : ProgramPoint) : (SingleHeapIdentifier, ReallyApproximatedHeapDomain)=(new SingleHeapIdentifier(typ, pp), this);
  override def getFieldIdentifier(heapIdentifier : Expression, name : String, typ : Type) : (TopHeapIdentifier, ReallyApproximatedHeapDomain)=(new SingleHeapIdentifier(heapIdentifier.getType(), heapIdentifier.getProgramPoint), this);
  
  override def assume(expr : Expression) = this //TODO: for now there is nothing about the heap structure
  
  override def toString() : String = "#abstractReference#"
  
  override def equals(a : Any)= a match{
    case x : ReallyApproximatedHeapDomain => this.isBottom==x.isBottom
    case _ => false;
  }
}

//Approximates all the concrete references with just one abstract element. In addition, it tracks references to static objects
class OnlyStaticReferenceHeapDomain extends ReallyApproximatedHeapDomain {
  override def reset() : Unit = Unit;
  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = Nil;
  override def getLabel() : String = "Only static references"
  private var isBottom = false;
   
  override def getFieldIdentifier(heapIdentifier : Expression, name : String, typ : Type) : (TopHeapIdentifier, ReallyApproximatedHeapDomain) = {
    if(typ.isStatic())
       return (new StaticHeapIdentifier(typ, heapIdentifier.getProgramPoint), this)
    //if(heapIdentifier.isTop())
    //  return new StaticHeapIdentifier(typ.top()); 
    return (new SingleHeapIdentifier(typ, heapIdentifier.getProgramPoint), this);
  }
  
  override def access(field : Identifier)=this;
  
  override def createVariable(id : Identifier, typ : Type)=this;
  
  override def toString() : String = "#abstractReference#"
  
  override def equals(a : Any)= a match{
    case x : OnlyStaticReferenceHeapDomain => this.isBottom==x.isBottom
    case _ => false;
  }
}