package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample._
import abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.userinterfaces.ShowGraph
import property.Property

object NonRelationalHeapDomainSettings {
	var unsoundEntryState : Boolean = true;
	var maxInitialNodes : Int = 5;
}

class HeapEnv[I <: NonRelationalHeapIdentifier[I]](var typ : Type, val dom : HeapIdSetDomain[I]) extends FunctionalDomain[I, HeapIdSetDomain[I], HeapEnv[I]] {

  def getIds = this.getAddresses;
  override def factory() = new HeapEnv(typ, dom)
  private def getAddresses : Set[I] = {
    var result : Set[I] = Set.empty[I] ++ value.keySet;
    val it : Iterator[HeapIdSetDomain[I]] = value.values.iterator;
    for(v <- it) {
      result++=v.value;
    }
    return result;
  }
  def get(key : I) : HeapIdSetDomain[I] = this.value.get(key) match {
    case None => dom.bottom(); //TODO: This is not sound!!!
    case Some(x) => x
  }
} 

class VariableEnv[I <: NonRelationalHeapIdentifier[I]](var typ : Type, val dom : HeapIdSetDomain[I]) extends FunctionalDomain[VariableIdentifier, HeapIdSetDomain[I], VariableEnv[I]] {
  def getIds : Set[Identifier] = (this.getVariables++this.getAddresses).asInstanceOf[Set[Identifier]];
  override def factory() = new VariableEnv(typ, dom)
  private def getVariables=value.keySet;
  private def getAddresses : Set[I]={
    var result : Set[I] = Set.empty[I];
    val it : Iterator[HeapIdSetDomain[I]] = value.values.iterator;
    for(v <- it) {
      result++=v.value;
    }
    return result;
  }
  def get(key : VariableIdentifier) : HeapIdSetDomain[I] = this.value.get(key) match {
    case None => dom.bottom(); //TODO: This is not sound!!!
    case Some(x) => x
  }
  
}
/*
final class MaybeHeapIdSetDomain[I <: NonRelationalHeapIdentifier[I]](id : I) extends NonRelationalHeapIdentifier[MaybeHeapIdSetDomain[I]](id.getType, id.getProgramPoint) with SetDomain[I, MaybeHeapIdSetDomain[I]] {
  def getField() : Option[String] = if(value.size==1) return value.elements.next.getField() else return None;
  override def getLabel() = id.getLabel;

  override def equals(x : Any) : Boolean = x match {
	  case x : I => if(value.size==1) return x.equals(value.elements.next); else return false;
	  case _ => return super.equals(x);
  }
  
  def getNullNode(p : ProgramPoint) : MaybeHeapIdSetDomain[I] = new MaybeHeapIdSetDomain(id.getNullNode(p));
  
  def convert(add : I) : MaybeHeapIdSetDomain[I] = new MaybeHeapIdSetDomain(add).add(add);
  override def getType() : Type = {
    var res=typ.bottom();
    for(a <- this.value)
      res=res.lub(res, a.getType());
    return res;
  }
  
  def isNormalized() : Boolean = {
    for(add <- this.value)
      if(! add.isNormalized) return false;
    return true;
  }
  
  def add(id : MaybeHeapIdSetDomain[I]) : MaybeHeapIdSetDomain[I]= {
    var result=this;
    for(add <- id.value)
      result=result.add(add);
    return result;
  }
  def createAddressForParameter(typ : Type, pp : ProgramPoint) : MaybeHeapIdSetDomain[I] = this.factory().add(id.createAddressForParameter(typ, pp))
  def createAddress(typ : Type, pp : ProgramPoint) : MaybeHeapIdSetDomain[I] = this.factory().add(id.createAddress(typ, pp))
  def extractField(obj : MaybeHeapIdSetDomain[I], field : String, typ : Type) : MaybeHeapIdSetDomain[I] = {
      var result=this.factory();
      for(add <- obj.value) {
        result=result.add(id.extractField(add, field, typ));
      }
      return result;
    }
  def accessStaticObject(typ : Type, pp : ProgramPoint) : MaybeHeapIdSetDomain[I] = this.factory().add(id.accessStaticObject(typ, pp));
  
  def factory() : MaybeHeapIdSetDomain[I]=new MaybeHeapIdSetDomain[I](id);
  
  def representSingleVariable() : Boolean = {
    if(this.value.size==1)
      return this.value.elements.next.representSingleVariable();
    else return false;
  }
  def getName() : String = this.toString();
} 
*/

abstract class NonRelationalHeapIdentifier[I <: NonRelationalHeapIdentifier[I]](typ1 : Type, pp : ProgramPoint) extends HeapIdentifier[I](typ1, pp) {
  def getLabel() : String;
  def createAddress(typ : Type, pp : ProgramPoint) : I;
  def createAddressForParameter(typ : Type, p : ProgramPoint) : I;
  def extractField(obj : I, field : String, typ : Type) : I;
  def accessStaticObject(typ : Type, p : ProgramPoint) : I;
  def getNullNode(p : ProgramPoint) : I;
  def isNormalized() : Boolean;
  def factory() : I;
}


//Approximates all the concrete references created at the same point of the program with a unique abstract reference
class NonRelationalHeapDomain[I <: NonRelationalHeapIdentifier[I]](env : VariableEnv[I], heap : HeapEnv[I], val cod : HeapIdSetDomain[I], dom : I)
    extends CartesianProductDomain[VariableEnv[I], HeapEnv[I], NonRelationalHeapDomain[I]](env, heap)
    with HeapDomain[NonRelationalHeapDomain[I], I]
    with HeapAnalysis[NonRelationalHeapDomain[I], I] {

  override def endOfAssignment() = (this, new Replacement());

  override def getIds : Set[Identifier] = (this._1.getIds++this._2.getIds).asInstanceOf[Set[Identifier]];
  override def lubWithReplacement(left : NonRelationalHeapDomain[I], right : NonRelationalHeapDomain[I]) = (this.lub(left, right), new Replacement)
  override def glbWithReplacement(left : NonRelationalHeapDomain[I], right : NonRelationalHeapDomain[I]) = (this.glb(left, right), new Replacement)
  override def wideningWithReplacement(left : NonRelationalHeapDomain[I], right : NonRelationalHeapDomain[I]) = (this.widening(left, right), new Replacement)
  override def reset() : Unit = {
     if(NonRelationalHeapDomainSettings.unsoundEntryState)
       ParameterIds.reset()
  }
  def setType(t : Type) = {
    env.typ=t;
    heap.typ=t;
    dom.typ=t;
    //cod.typ=t;
  }
  override def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier : Expression, index : Expression, state : S, typ : Type)
  = throw new SemanticException("Non relational heap domains do not support arrays");
  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = Nil;
  override def getLabel() : String = "Heap Domain:"+dom.getLabel();
  override def parameters() : List[(String, Any)] = List((("UnsoundEntryState"), true), (("MaxEntryNodes"), 10))
  override def setParameter(label : String, value : Any) : Unit = label match {
    case "UnsoundEntryState" => NonRelationalHeapDomainSettings.unsoundEntryState=value.asInstanceOf[Boolean];
    case "MaxEntryNodes" => NonRelationalHeapDomainSettings.maxInitialNodes=value.asInstanceOf[Int];
  };
  override def getInitialState() = new NonRelationalHeapDomain(new VariableEnv(env.typ, env.dom), new HeapEnv(heap.typ, heap.dom), cod, dom);
  override def getProperties() : Set[Property] = Set.empty+ShowGraph;

  def this(typ : Type, cod : HeapIdSetDomain[I], dom : I) {
    this(new VariableEnv(typ, cod), new HeapEnv(typ, cod), cod, dom)
  }
  def getStringOfId(id : Identifier) : String = id match {
    case x : VariableIdentifier => this.get(x).toString();
    case x : HeapIdSetDomain[I] => this.get(x).toString();
  }

  def factory() = new NonRelationalHeapDomain(d1.factory(), d2.factory(), cod.factory(), dom.factory())
  
  def get(key : VariableIdentifier) : HeapIdSetDomain[I] = this._1.value.get(key) match {
    case None => cod.top();
    case Some(x) => x
  }

  def get(key : HeapIdSetDomain[I]) : HeapIdSetDomain[I] = {
    var result = cod.bottom();
    for(addr <- key.value) 
      this._2.value.get(addr) match {
      	case None => return cod.top();
      	case Some(x) => result=result.lub(result, x)
      }
    result
  }

  def get(key : I) : HeapIdSetDomain[I] = this._2.get(key);

  override def createVariable(variable : Identifier, typ : Type) =  variable match {
    case x : VariableIdentifier => (new NonRelationalHeapDomain(this._1.add(x, cod.bottom()), this._2, cod, dom), new Replacement);
    case x : HeapIdentifier[I] => (this, new Replacement)
  }

   override def createVariableForParameter(variable : Identifier, typ : Type, path : List[String])  =  variable match {
    case x : VariableIdentifier =>
      if(typ.isObject) {
	    var (result, r)=this.createVariable(variable, typ); //r will be always empty, so I ignore it
	    var ids : Map[Identifier, List[String]] = Map.empty[Identifier, List[String]];
	    alreadyInitialized = Set.empty[I];
	    this.initializeObject(x, dom.createAddressForParameter(typ, x.getProgramPoint), typ, result, path ::: variable.toString() :: Nil);
      }
      else {
        var result = Map.empty[Identifier, List[String]];
        result=result+((variable, variable.toString() :: Nil ))
        (new NonRelationalHeapDomain(this._1.add(x.asInstanceOf[VariableIdentifier], cod.bottom()), this._2, cod, dom), result, new Replacement);
        }
     case x : HeapIdentifier[I] => {throw new Exception("This should not happen!");}
  }
  
  private var alreadyInitialized : Set[I] = Set.empty[I];
  private var fieldsInitialized : Set[I] = Set.empty[I];
  
  private def initializeObject(x : Identifier, obj : I, typ : Type, heap : NonRelationalHeapDomain[I], path : List[String]) : (NonRelationalHeapDomain[I], Map[Identifier, List[String]], Replacement) = {
	  if(/*typ.isObject && */! alreadyInitialized.contains(obj)) {
	  	var result=heap;
	    var ids : Map[Identifier, List[String]] = Map.empty[Identifier, List[String]];
	    ids=ids+((obj, path ));
	    val newAdd=cod.convert(obj);
	    if(! NonRelationalHeapDomainSettings.unsoundEntryState)
		    for(add <- result.getIds)
          if(add.isInstanceOf[I] && add.getType().lessEqual(typ))
		        newAdd.add(add.asInstanceOf[I]);
	    if(x.isInstanceOf[VariableIdentifier])
	    	result=new NonRelationalHeapDomain(result._1.add(x.asInstanceOf[VariableIdentifier], newAdd), result._2, cod, dom);
     
	    alreadyInitialized=alreadyInitialized+obj;
	    val c = typ.getPossibleFields;
	    for(field <- c) {
	      val adds = cod.convert(dom.createAddressForParameter(field.getType(), x.getProgramPoint));
        //I can ignore newHeap since it's equal to result as it is not changed by getFieldIdentifier
        //in the same way I ignore rep
	      val (fieldAdd, newHeap, rep)=result.getFieldIdentifier(cod.convert(obj), field.getName(), field.getType(), field.getProgramPoint());
	      for(id : I <- fieldAdd.value) {
	    	  result=new NonRelationalHeapDomain(result._1, result._2.add(id, adds), cod, dom);
	    	  ids=ids+((id, path ::: (field.getName()) :: Nil));
	    	  val r=initializeObject(id, id, id.getType, result, path ::: (field.getName()) :: Nil)
	    	  alreadyInitialized=alreadyInitialized+id;
	    	  result=r._1;
	    	  ids=r._2++ids;//This order is quite important: in this way we keep the shortest path to arrive to an abstract node!
	      }
	    }
	    (result, ids, new Replacement)
        }
	  else (heap, Map.empty[Identifier, List[String]], new Replacement);
  }

  override def setParameter(variable : Identifier, expr : Expression) = this.assign(variable, expr, null);
  
  override def backwardAssign(variable : Identifier, expr : Expression) = (this, new Replacement)

  override def assignField(variable : Identifier, s : String, expr : Expression) : (NonRelationalHeapDomain[I], Replacement) = {
    var result=this.bottom();
    val ids = this.getFieldIdentifier(variable, s, expr.getType, variable.getProgramPoint())._1;
    for(id <- ids.value)
      result=result.lub(result, this.assign(id, expr, null)._1);
    (result, new Replacement);
  }
  //We ignore the other parts since getting a field does not modify a non relational heap domain

  override def assign[S <: SemanticDomain[S]](variable : Identifier, expr : Expression, state : S) : (NonRelationalHeapDomain[I], Replacement) = {
    if(! variable.getType.isObject) return (this, new Replacement);//It does not modify the heap
    variable match {
	    case x : VariableIdentifier => 
	      try {
	        expr match {
	          case value : HeapIdSetDomain[I] =>
	            var result=this._1;
	            for(heapid <- this.normalize(value).value)
	            	if(value.value.size==1)
	            		result=result.add(x, this.normalize(value));
	            	else result=result.add(x, value.lub(this.normalize(value), this._1.get(x)));
	            (new NonRelationalHeapDomain(result, this._2, cod, dom), new Replacement);
	          case _ =>
	            val value=this.eval(expr);
	            (new NonRelationalHeapDomain(this._1.add(x, value.lub(this._1.get(x), this.normalize(value))), this._2, cod, dom), new Replacement)
	        }
	      }
	      catch {
	      	case _ => this.setToTop(variable)
	      }

	    case x : I =>
	      val value=this.eval(expr)
	      var result=this._2.add(x, this.normalize(value))
	      return (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);

	    case x : HeapIdSetDomain[I] =>
	      if(x.isTop)
	        return (this.top(), new Replacement);
	      var result=this._2;
	      val value=this.eval(expr)
        //TODO:Distinguish between definite and maybe
	      for(addr <- x.value/*this.normalize(x).value*/)
	        result=result.add(addr, value.lub(this.normalize(value), this._2.get(addr)))
	      return (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);
	    case x : ArrayAccess => return (this, new Replacement);//We skip array access, because of AVP2010 project
    }
  }
  
  override def setToTop(variable : Identifier) = variable match  {
    case x : VariableIdentifier => (new NonRelationalHeapDomain(this._1.add(x, cod.top()), this._2, cod, dom), new Replacement)
    case x : HeapIdSetDomain[I] =>;
      var result=this._2;
      for(addr <- x.value)
        result=result.add(addr, cod.top())
      (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);
    case x : I =>
      (new NonRelationalHeapDomain(this._1, this._2.add(x, cod.top()), cod, dom), new Replacement)
  }  
  
  override def removeVariable(variable : Identifier) = variable match  {
    case x : VariableIdentifier => (new NonRelationalHeapDomain(this._1.remove(x), this._2, cod, dom), new Replacement)
    case x : HeapIdSetDomain[I] =>;
      var result=this._2;
      for(addr <- x.value)
        result=result.remove(addr)
      (new NonRelationalHeapDomain(this._1, result, cod, dom), new Replacement);
  }  
  
  override def createObject(typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], NonRelationalHeapDomain[I], Replacement) = (cod.convert(dom.createAddress(typ, pp)), this, new Replacement);
  
  override def getFieldIdentifier(heapIdentifier : Expression, name : String, typ : Type, pp : ProgramPoint) : (HeapIdSetDomain[I], NonRelationalHeapDomain[I], Replacement) = (this.evalFieldAccess(heapIdentifier, name, typ), this, new Replacement);
  
  private def evalFieldAccess[S <: State[S]](expr : Expression, field : String, typ : Type) : HeapIdSetDomain[I] = expr match {
    case obj : VariableIdentifier => return extractField(this.get(obj), field, typ)
    
    case obj : HeapIdSetDomain[I] => {
      var result : HeapIdSetDomain[I] = cod.bottom();
      for(simplepp : I <- obj.value)
            result=result.lub(result, extractField(simplepp, field, typ));

      //If I don't have any information, I return a top identifier
      if(result.isBottom)
        return result.top();
      return result;
    }
    //case obj : I => return extractField(this.get(obj.asInstanceOf[I]), field, typ)
      case obj : I => return extractField(obj.asInstanceOf[I], field, typ)

    //case obj : SimpleProgramPointHeapIdentifier => return extractField(obj.asInstanceOf[I], field, typ)

    //case _ => return throw new Exception();//cod.top();
  }
  
  private def extractField(obj : I, field : String, typ : Type) : HeapIdSetDomain[I] = {
    var result : HeapIdSetDomain[I] = cod.bottom();
    for(id <- this.normalize(cod.convert(obj)).value)
      result=result.add(dom.extractField(id, field, typ));
    if(result.isBottom)
      return result.top();
    return result;
  }
  
  
  private def normalize(id : HeapIdSetDomain[I]) : HeapIdSetDomain[I] = {
    var result=id.factory();
    for(add <- id.value)
      if(add.isNormalized)
        result=result.add(add);
      else result=result.add(this._2.get(add));
    return result;
  }
  
  private def extractField(obj : HeapIdSetDomain[I], field : String, typ : Type) : HeapIdSetDomain[I] = {
    var result : HeapIdSetDomain[I] = cod.bottom();
    if(obj.isBottom) return result.bottom();
    if(obj.isTop) {//We manage the case in which we use the object to access a static object -> useful in Scala
      if(typ != null && typ.isStatic) {
    	  typ.isStatic;
        return result.add(dom.accessStaticObject(typ, obj.getProgramPoint));
      }
    }
    val accessed = this.normalize(obj);
    for(node <- accessed.value)
      result=result.add(dom.extractField(node, field, typ));
    //If I don't have any information, I return a top identifier
    if(result.isBottom)
      return result.top();
    return result;
  }
  
  private def eval[S <: State[S]](expr : Expression) : HeapIdSetDomain[I] = expr match {
    case x : VariableIdentifier => this.get(x);
    case x : HeapIdSetDomain[I] => x;                /*
      var result=cod.bottom();
      for(addr <- x.value)
        result=result.lub(result, this._2.get(addr));
      return result;                               */
    case Constant("null", typ, pp) => return cod.convert(dom.getNullNode(pp));
    
    case x => return cod.top();
  }
  
  override def assume(expr : Expression) = 
    (this, new Replacement) //TODO: for now there is nothing about the heap structure

}

case class TopHeapIdentifier(typ2 : Type, pp2 : ProgramPoint) extends NonRelationalHeapIdentifier[TopHeapIdentifier](typ2, pp2) {
    override def getLabel() = "Top";
	  override def getNullNode(pp : ProgramPoint) = this
	  override def getField() : Option[String] = None;
	  override def isNormalized() : Boolean = true;
	  override def representSingleVariable()=false;
	  override def getName() = "#abstractReference#"
	  override def equals(o : Any) = o match {
	    case x : TopHeapIdentifier => true
	    case _ => false
	  }
	  override def factory() = this;
      override def createAddress(typ : Type, pp : ProgramPoint)=this;
      override def createAddressForParameter(typ : Type, pp : ProgramPoint)=this;
      override def extractField(obj : TopHeapIdentifier, field : String, typ : Type)=this;
      override def accessStaticObject(typ : Type, pp : ProgramPoint)=this;
	  override def hashCode() : Int = 0;
}
