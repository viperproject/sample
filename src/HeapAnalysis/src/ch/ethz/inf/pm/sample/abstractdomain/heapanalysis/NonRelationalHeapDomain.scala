package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

object NonRelationalHeapDomainSettings {
	var unsoundEntryState : Boolean = true;
	var maxInitialNodes : Int = 5;
}

class HeapEnv[I <: NonRelationalHeapIdentifier[I]](val typ : Type, val dom : HeapIdAndSetDomain[I]) extends FunctionalDomain[I, HeapIdAndSetDomain[I], HeapEnv[I]] {
  override def factory() = new HeapEnv(typ, dom)
  def getAddresses : Set[I] = {
    var result : Set[I] = Set.empty[I] ++ value.keySet;
    val it : Iterator[HeapIdAndSetDomain[I]] = value.values.iterator;
    for(v <- it) {
      result++=v.value;
    }
    return result;
  }
  def get(key : I) : HeapIdAndSetDomain[I] = this.value.get(key) match {
    case None => dom.bottom(); //TODO: This is not sound!!!
    case Some(x) => x
  }
} 

class VariableEnv[I <: NonRelationalHeapIdentifier[I]](val typ : Type, val dom : HeapIdAndSetDomain[I]) extends FunctionalDomain[VariableIdentifier, HeapIdAndSetDomain[I], VariableEnv[I]] {
  override def factory() = new VariableEnv(typ, dom)
  def getVariables=value.keySet; 
  def getAddresses : Set[I]={
    var result : Set[I] = Set.empty[I];
    val it : Iterator[HeapIdAndSetDomain[I]] = value.values.iterator;
    for(v <- it) {
      result++=v.value;
    }
    return result;
  }
  def get(key : VariableIdentifier) : HeapIdAndSetDomain[I] = this.value.get(key) match {
    case None => dom.bottom(); //TODO: This is not sound!!!
    case Some(x) => x
  }
  
}

final class HeapIdAndSetDomain[I <: NonRelationalHeapIdentifier[I]](id : I) extends NonRelationalHeapIdentifier[HeapIdAndSetDomain[I]](id.getType) with SetDomain[I, HeapIdAndSetDomain[I]] {
  def getField() : Option[String] = if(value.size==1) return value.elements.next.getField() else return None;

  override def equals(x : Any) : Boolean = x match {
	  case x : I => if(value.size==1) return x.equals(value.elements.next); else return false;
	  case _ => return super.equals(x);
  }
  
  def getNullNode() : HeapIdAndSetDomain[I] = new HeapIdAndSetDomain(id.getNullNode());
  
  def convert(add : I) : HeapIdAndSetDomain[I] = new HeapIdAndSetDomain(add).add(add);
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
  
  def add(id : HeapIdAndSetDomain[I]) : HeapIdAndSetDomain[I]= {
    var result=this;
    for(add <- id.value)
      result=result.add(add);
    return result;
  }
  def createAddressForParameter(typ : Type) : HeapIdAndSetDomain[I] = this.factory().add(id.createAddressForParameter(typ))
  def createAddress(typ : Type, pp : ProgramPoint) : HeapIdAndSetDomain[I] = this.factory().add(id.createAddress(typ, pp))
  def extractField(obj : HeapIdAndSetDomain[I], field : String, typ : Type) : HeapIdAndSetDomain[I] = {
      var result=this.factory();
      for(add <- obj.value) {
        result=result.add(id.extractField(add, field, typ));
      }
      return result;
    }
  def accessStaticObject(typ : Type) : HeapIdAndSetDomain[I] = this.factory().add(id.accessStaticObject(typ));
  
  def factory() : HeapIdAndSetDomain[I]=new HeapIdAndSetDomain[I](id);
  
  def representSingleVariable() : Boolean = {
    if(this.value.size==1)
      return this.value.elements.next.representSingleVariable();
    else return false;
  }
  def getName() : String = this.toString();
  
} 


abstract class NonRelationalHeapIdentifier[I <: NonRelationalHeapIdentifier[I]](typ1 : Type) extends HeapIdentifier[I](typ1) {
  def createAddress(typ : Type, pp : ProgramPoint) : I;
  def createAddressForParameter(typ : Type) : I;
  def extractField(obj : I, field : String, typ : Type) : I;
  def accessStaticObject(typ : Type) : I;
  def getNullNode() : I;
  def isNormalized() : Boolean;
  def factory() : I;
}


//Approximates all the concrete references created at the same point of the program with a unique abstract reference
class NonRelationalHeapDomain[I <: NonRelationalHeapIdentifier[I]](env : VariableEnv[I], heap : HeapEnv[I], val cod : HeapIdAndSetDomain[I], dom : I) extends CartesianProductDomain[VariableEnv[I], HeapEnv[I], NonRelationalHeapDomain[I]](env, heap) with HeapDomain[NonRelationalHeapDomain[I], HeapIdAndSetDomain[I]] {
  def this(typ : Type, cod : HeapIdAndSetDomain[I], dom : I) {
    this(new VariableEnv(typ, cod), new HeapEnv(typ, cod), cod, dom)
  }
  def getStringOfId(id : Identifier) : String = id match {
    case x : VariableIdentifier => this.get(x).toString();
    case x : HeapIdAndSetDomain[I] => this.get(x).toString();
  }
  override def access(field : Identifier)=this;
  
  def getAddresses : Set[I] = d1.getAddresses++d2.getAddresses;
  
  override def backwardAccess(field : Identifier)=this;
  
  def getVariables()=d1.getVariables;
  
  def factory() = new NonRelationalHeapDomain(d1.factory(), d2.factory(), cod.factory(), dom.factory())
  
  def get(key : VariableIdentifier) : HeapIdAndSetDomain[I] = this._1.value.get(key) match {
    case None => cod.top();
    case Some(x) => x
  }

  def get(key : HeapIdAndSetDomain[I]) : HeapIdAndSetDomain[I] = {
    var result = cod.bottom();
    for(addr <- key.value) 
      this._2.value.get(addr) match {
      	case None => return cod.top();
      	case Some(x) => result=result.lub(result, x)
      }
    result
  }
  
  override def createVariable(variable : Identifier, typ : Type) =  variable match {
    case x : VariableIdentifier => new NonRelationalHeapDomain(this._1.add(x, cod.bottom()), this._2, cod, dom);
    case x : HeapIdentifier[I] => this
  }
  
   override def createVariableForParameter(variable : Identifier, typ : Type, path : List[String])  =  variable match {
    case x : VariableIdentifier =>
      if(typ.isObject) {
	    var result=this.createVariable(variable, typ);
	    var ids : Map[Identifier, List[String]] = Map.empty[Identifier, List[String]];
	    alreadyInitialized = Set.empty[I];
	    this.initializeObject(x, dom.createAddressForParameter(typ), typ, result, path ::: variable.toString() :: Nil);
      }
      else {
        var result = Map.empty[Identifier, List[String]];
        result=result+((variable, variable.toString() :: Nil ))
        (this, result);
        }
     case x : HeapIdentifier[I] => {throw new Exception("This should not happen!");}
  }
  
  private var alreadyInitialized : Set[I] = Set.empty[I];
  private var fieldsInitialized : Set[I] = Set.empty[I];
  
  private def initializeObject(x : Identifier, obj : I, typ : Type, heap : NonRelationalHeapDomain[I], path : List[String]) : (NonRelationalHeapDomain[I], Map[Identifier, List[String]]) = {
	  if(/*typ.isObject && */! alreadyInitialized.contains(obj)) {
	  	var result=heap;
	    var ids : Map[Identifier, List[String]] = Map.empty[Identifier, List[String]];
	    ids=ids+((obj, path ));
	    val newAdd=cod.convert(obj);
	    if(! NonRelationalHeapDomainSettings.unsoundEntryState)
		    for(add <- result.getAddresses)
		      if(add.getType().lessEqual(typ))
		        newAdd.add(add);
	    if(x.isInstanceOf[VariableIdentifier])
	    	result=new NonRelationalHeapDomain(result._1.add(x.asInstanceOf[VariableIdentifier], newAdd), result._2, cod, dom);
     
	    alreadyInitialized=alreadyInitialized+obj;
	    val c = typ.getPossibleFields;
	    for((field, typ2) <- c) {
	      val adds = cod.convert(dom.createAddressForParameter(typ2));
	      val fieldAdd=result.getFieldIdentifier(cod.convert(obj), field, typ2);
	      for(id : I <- fieldAdd.value) {
	    	  result=new NonRelationalHeapDomain(result._1, result._2.add(id, adds), cod, dom);
	    	  ids=ids+((id, path ::: field :: Nil));
	    	  val r=initializeObject(id, id, id.getType, result, path ::: field :: Nil)
	    	  alreadyInitialized=alreadyInitialized+id;
	    	  result=r._1;
	    	  ids=r._2++ids;//This order is quite important: in this way we keep the shortest path to arrive to an abstract node!
	      }
	    }
	    (result, ids)
        }
	  else (heap, Map.empty[Identifier, List[String]]);
  }
  
 /* override def createVariableForParameter(variable : Identifier, typ : Type, path : List[String])  =  variable match {
    case x : VariableIdentifier =>
      if(typ.isObject) {
	    var result=this.createVariable(variable, typ);
	    var ids : Map[Identifier, List[String]] = Map.empty[Identifier, List[String]];
	    val add : I = dom.createAddressForParameter(typ)
	    ids=ids+((add, path ::: variable.toString()::Nil));
	    val newAdd=cod.convert(add);
	    for(add <- this.getAddresses)
	      if(add.getType().lessEqual(typ))
	        newAdd.add(add);
	    result=new NonRelationalHeapDomain(result._1.add(x, newAdd), result._2, cod, dom);
	    //if(typ.isObject) {
	      for((field, typ2) <- typ.getPossibleFields) {
	    	  val adds = cod.convert(dom.createAddressForParameter(typ2));
	    	  val fieldAdd=this.getFieldIdentifier(cod.convert(add), field, typ2);
	    	  for(id : I <- fieldAdd.value) {
	    		  result=new NonRelationalHeapDomain(result._1, result._2.add(id, adds), cod, dom);
	    		  ids=ids+((id, path ::: variable.toString() :: field :: Nil));
	    		  alreadyInitialized = Map.empty[I, List[String]];
	    		  val r=initializeObject(id, id.getType, result, path ::: variable.toString() :: field :: Nil)
	    		  result=r._1;
	    		  ids=ids++r._2;
	    		  ids=ids++alreadyInitialized;
	    	  }
	      }
	    //}
	    (result, ids)
      }
      else {
        var result = Map.empty[Identifier, List[String]];
        result=result+((variable, variable.toString() :: Nil ))
        (this, result);
        }
     case x : HeapIdentifier[I] => {
    	 throw new Exception("This should not happen!");
    	 /*var result = Map.empty[Identifier, List[String]];
    	 result=result+((x, x.toString() :: Nil ))
    	 (this, result);*/
        }
  }
  
  
  private var alreadyInitialized : Map[I, List[String]] = Map.empty[I, List[String]];
  private def initializeObject(obj : I, typ : Type, heap : NonRelationalHeapDomain[I], path : List[String]) : (NonRelationalHeapDomain[I], Map[Identifier, List[String]]) = {
	    var ids : Map[Identifier, List[String]] = Map.empty[Identifier, List[String]];
	    if(typ.isObject && ! alreadyInitialized.contains(obj)) {
          alreadyInitialized=alreadyInitialized+((obj, path)); //To avoid infinite loops during the initialization
		  var result=heap;
   	      for((field, typ2) <- typ.getPossibleFields) {
	    	  val adds = cod.convert(dom.createAddressForParameter(typ2));
	    	  val fieldAdd=result.getFieldIdentifier(result.get(cod.convert(obj)), field, typ2);
	    	  for(id : I <- fieldAdd.value) {
	    		  result=new NonRelationalHeapDomain(result._1, result._2.add(id, adds), cod, dom);
	    		  ids=ids+((id, path));
	    		  val r =initializeObject(id, id.getType, result, path ::: field :: Nil);
	    		  ids=ids++r._2;
	    		  result=r._1;
	    	  	}
	      }
          (result, ids);
        }
	    /*else if(! alreadyInitialized.contains(obj)) {
	    		  ids=ids+((id, path ::: field :: Nil));
	      
	    } */ 
	    else (heap, ids);
  }*/
  
  override def setParameter(variable : Identifier, expr : Expression) : NonRelationalHeapDomain[I] = this.assign(variable, expr);
  
  override def backwardAssign(variable : Identifier, expr : Expression) : NonRelationalHeapDomain[I] = this
  
  override def assign(variable : Identifier, expr : Expression) : NonRelationalHeapDomain[I] = {
    if(! variable.getType.isObject) return this;//It does not modify the heap
    variable match {
	    case x : VariableIdentifier => 
	      try {
	        expr match {
	          case value : HeapIdAndSetDomain[I] =>
	            var result=this._1;
	            for(heapid <- this.normalize(value).value)
	            	if(value.value.size==1)
	            		result=result.add(x, this.normalize(value));
	            	else result=result.add(x, value.lub(this.normalize(value), this._1.get(x)));
	            new NonRelationalHeapDomain(result, this._2, cod, dom);
	          case _ =>
	            val value=this.eval(expr);
	            new NonRelationalHeapDomain(this._1.add(x, value.lub(this._1.get(x), this.normalize(value))), this._2, cod, dom)
	        }
	      }
	      catch {
	      	case _ => this.setToTop(variable)
	      }
	    case x : HeapIdAndSetDomain[I] =>
	      if(x.isTop)
	        return this.top();
	      var result=this._2;
	      val value=this.eval(expr)
	      for(addr <- x.value/*this.normalize(x).value*/)
	        result=result.add(addr, value.lub(this.normalize(value), this._2.get(addr)))
	      return new NonRelationalHeapDomain(this._1, result, cod, dom);
	    case x : ArrayAccess => return this;//We skip array access, because of AVP2010 project
    }
  }
  
  override def setToTop(variable : Identifier) : NonRelationalHeapDomain[I] = variable match  {
    case x : VariableIdentifier => new NonRelationalHeapDomain(this._1.add(x, cod.top()), this._2, cod, dom)
    case x : HeapIdAndSetDomain[I] =>;
      var result=this._2;
      for(addr <- x.value)
        result=result.add(addr, cod.top())
      return new NonRelationalHeapDomain(this._1, result, cod, dom);
  }  
  
  override def removeVariable(variable : Identifier) : NonRelationalHeapDomain[I] = variable match  {
    case x : VariableIdentifier => new NonRelationalHeapDomain(this._1.remove(x), this._2, cod, dom)
    case x : HeapIdAndSetDomain[I] =>;
      var result=this._2;
      for(addr <- x.value)
        result=result.remove(addr)
      return new NonRelationalHeapDomain(this._1, result, cod, dom);
  }  
  
  override def createObject(typ : Type, pp : ProgramPoint) : HeapIdAndSetDomain[I] = cod.convert(dom.createAddress(typ, pp));
  
  override def getFieldIdentifier(heapIdentifier : Expression, name : String, typ : Type) : HeapIdAndSetDomain[I] = this.evalFieldAccess(heapIdentifier, name, typ);
  
  private def evalFieldAccess[S <: State[S]](expr : Expression, field : String, typ : Type) : HeapIdAndSetDomain[I] = expr match {
    case obj : VariableIdentifier => return extractField(this.get(obj), field, typ)
    
    case obj : HeapIdAndSetDomain[I] => {
      var result : HeapIdAndSetDomain[I] = cod.bottom();  
      for(simplepp : I <- obj.value)
            result=result.lub(result, extractField(simplepp, field, typ));

      //If I don't have any information, I return a top identifier
      if(result.isBottom)
        return result.top();
      return result;
    }
    
    case _ => return cod.top();//TODO: It should never happen!!!
  }
  
  private def extractField(obj : I, field : String, typ : Type) : HeapIdAndSetDomain[I] = {
    var result : HeapIdAndSetDomain[I] = cod.bottom();
    for(id <- this.normalize(cod.convert(obj)).value)
      result=result.add(dom.extractField(id, field, typ));
    if(result.isBottom)
      return result.top();
    return result;
  }
  
  
  private def normalize(id : HeapIdAndSetDomain[I]) : HeapIdAndSetDomain[I] = {
    var result=id.factory();
    for(add <- id.value)
      if(add.isNormalized)
        result=result.add(add);
      else result=result.add(this._2.get(add));
    return result;
  }
  
  private def extractField(obj : HeapIdAndSetDomain[I], field : String, typ : Type) : HeapIdAndSetDomain[I] = {
    var result : HeapIdAndSetDomain[I] = cod.bottom();
    if(obj.isBottom) return result.bottom();
    if(obj.isTop) {//We manage the case in which we use the object to access a static object -> useful in Scala
      if(typ != null && typ.isStatic) {
    	  typ.isStatic;
        return result.add(dom.accessStaticObject(typ));
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
  
  private def eval[S <: State[S]](expr : Expression) : HeapIdAndSetDomain[I] = expr match {
    case x : VariableIdentifier => this.get(x);
    case x : HeapIdAndSetDomain[I] => 
      var result=cod.bottom();
      for(addr <- x.value)
        result=result.lub(result, this._2.get(addr));
      return result;
    case Constant("null", typ) => return cod.convert(dom.getNullNode());
    
    case x => return cod.top();
  }
  
  override def assume(expr : Expression) = 
    this //TODO: for now there is nothing about the heap structure
  
}