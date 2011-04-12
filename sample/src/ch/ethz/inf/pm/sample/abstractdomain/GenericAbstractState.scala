package ch.ethz.inf.pm.sample.abstractdomain


import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._

/**
 * An abstract semantic domain that combines and heap and another semantic domain.
 * The intuition is that the heap domain takes care of approximating the heap structure, while the 
 * semantic domain has to manage the information of its interest without taking care of field accesses
 * and object creation, but dealing only with identifiers (of variables or of heap nodes).
 *
 * @param <N> The semantic domain
 * @param <H> The heap analysis
 * @param <I> The heap identifiers
 * @author Pietro Ferrara
 * @since 0.1
 */
class HeapAndAnotherDomain[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](n1 : N, h1 : H) extends SemanticCartesianProductDomain[N, H, HeapAndAnotherDomain[N, H, I]](n1, h1) {
  
  def getStringOfId(id : Identifier) : String = d1.getStringOfId(id)
  
  def getHeap() : H = return d2;
  def getSemanticDomain() : N = return d1;
  
  def factory() = new HeapAndAnotherDomain(d1.factory(), d2.factory());
  
  override def createVariableForParameter(variable : Identifier, typ : Type, path : List[String]) = {
    SystemParameters.domainTimer.start();
    val (s2, ids) = this._2.createVariableForParameter(variable, typ, path);
    SystemParameters.domainTimer.stop();
    var s1 = this._1;
    val i = null;
    s1=s1.createVariableForParameter(variable, typ, path)._1
    //We recursively create the entry state for all the entry abstract nodes.
    SystemParameters.heapTimer.start();
    for(id <- ids.keys)
      s1=s1.createVariableForParameter(id, typ, ids.apply(id))._1;
    SystemParameters.heapTimer.stop();
    (new HeapAndAnotherDomain[N, H, I](s1, s2), ids)
  }


  //---------------------------------------------------
  //The following part is the same of SemanticCartesianProduct. It is there only for profiling purposes.

  type T = HeapAndAnotherDomain[N, H, I];

  override def setToTop(variable : Identifier) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.setToTop(variable)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.setToTop(variable)
    SystemParameters.domainTimer.stop();
    result
  }
 override def assign(variable : Identifier, expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.assign(variable, expr)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.assign(variable, expr)
    SystemParameters.domainTimer.stop();
    result
  }
 override def setParameter(variable : Identifier, expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.setParameter(variable, expr)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.setParameter(variable, expr)
    SystemParameters.domainTimer.stop();
    result
  }
 override def assume(expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.assume(expr)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.assume(expr)
    SystemParameters.domainTimer.stop();
    result
  }
 override def createVariable(variable : Identifier, typ : Type) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.createVariable(variable, typ)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.createVariable(variable, typ)
    SystemParameters.domainTimer.stop();
    result
  }
 override def removeVariable(variable : Identifier) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.removeVariable(variable)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.removeVariable(variable)
    SystemParameters.domainTimer.stop();
    result
  }
 override def access(field : Identifier) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.access(field)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.access(field)
    SystemParameters.domainTimer.stop();
    result
  }
 override def backwardAccess(field : Identifier) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.backwardAccess(field)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.backwardAccess(field)
    SystemParameters.domainTimer.stop();
    result
  }
 override def backwardAssign(variable : Identifier, expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.backwardAssign(variable, expr)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.backwardAssign(variable, expr)
    SystemParameters.domainTimer.stop();
    result
  }

 override def top() : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.top()
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.top()
    SystemParameters.domainTimer.stop();
    result
  }

 override def bottom() : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.bottom()
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.bottom()
    SystemParameters.domainTimer.stop();
    result
  }

 override def lub(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.lub(l._1, r._1)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.lub(l._2, r._2)
    SystemParameters.domainTimer.stop();
    result
  }

 override def glb(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.glb(l._1, r._1)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.glb(l._2, r._2)
    SystemParameters.domainTimer.stop();
    result
  }

 override def widening(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    result.d1=d1.widening(l._1, r._1)
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d2=d2.widening(l._2, r._2)
    SystemParameters.domainTimer.stop();
    result
  }

 override def lessEqual(r : T) : Boolean = {
    SystemParameters.heapTimer.start();
    var b : Boolean = d1.lessEqual(r._1);
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    b = b && d2.lessEqual(r._2)
    SystemParameters.domainTimer.stop();
    return b;
  }

}

/** 
 * A generic abstract state combines a HeapAndAnotherDomain and a SymbolicAbstractValue. It is the
 * engine of our analysis: it takes care of all the complexities related to our approach, e.g., if
 * we have to deal with several possible expressions, it passes one expression after the other to
 * the abstract domain computing the upper bound between the results of the evaluation of all the 
 * possible expressions.
 *
 * @param <N> The semantic domain
 * @param <H> The heap analysis
 * @param <I> The heap identifiers
 * @param n1 The abstract state of the heap and the semantic domain
 * @param r1 The expression
 * @author Pietro Ferrara
 * @since 0.1
 */
class GenericAbstractState[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](n1 : HeapAndAnotherDomain[N, H, I], r1 : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) extends
  CartesianProductDomain[HeapAndAnotherDomain[N, H, I], SymbolicAbstractValue[GenericAbstractState[N,H,I]], GenericAbstractState[N,H,I]](n1, r1) with State[GenericAbstractState[N,H,I]] with SingleLineRepresentation {
  
  def factory() = new GenericAbstractState(this._1.top(), this._2.top());
  def getHeap() : H = d1.getHeap();
  def getSemanticDomain() : N = d1.getSemanticDomain();
  def isBottom() : Boolean = this._1.equals(this._1.bottom()) || this._2.equals(this._2.bottom());
  def getStringOfId(id : Identifier) : String = this._1.getStringOfId(id)
  def createObject(typ : Type, pp : ProgramPoint) : GenericAbstractState[N,H,I] =  {
    if(this.isBottom) return this;
    //It discharges on the heap analysis the creation of the object and its fields
    var (createdLocation, newHeap)=this._1._2.createObject(typ, pp)
    var result=new HeapAndAnotherDomain[N, H, I](this._1._1, newHeap);
    result=result.createVariable(createdLocation, typ);
    for((field, typ2) <- typ.getPossibleFields()) {
     val (address, newHeap2) = result._2.getFieldIdentifier(createdLocation, field, typ2);
     result=new HeapAndAnotherDomain[N, H, I](result._1, newHeap2);
     //It asks the semantic domain to simply create the initial value for the given identifier
     result=result.createVariable(address, typ2);
    }
    this.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](createdLocation, new GenericAbstractState(result, this._2).removeExpression()));
  }
  
  def createObjectForParameter(typ : Type, pp : ProgramPoint, path : List[String]) : GenericAbstractState[N,H,I] =  {
    if(this.isBottom) return this;
    //It discharges on the heap analysis the creation of the object and its fields
    val (createdLocation, newHeap)=this._1._2.createObject(typ, pp)
    var result=new HeapAndAnotherDomain[N, H, I](this._1._1, newHeap);
     //It asks the semantic domain to simply create the initial value for the given identifier
    val (result1, ids)=result.createVariableForParameter(createdLocation, typ, path);
    this.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](createdLocation, new GenericAbstractState(result, this._2).removeExpression()));
  }
  
  def getExpression() : SymbolicAbstractValue[GenericAbstractState[N,H,I]] = getResult
  
  def removeExpression() : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    new GenericAbstractState(this._1, new SymbolicAbstractValue[GenericAbstractState[N,H,I]](Some(this), r1.typ))
  }
  
  def createVariable(x : SymbolicAbstractValue[GenericAbstractState[N,H,I]], typ : Type) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    if(x.value.size != 1 || x.value.elements.next._1.isInstanceOf[VariableIdentifier]==false) 
      throw new SymbolicSemanticException("Cannot declare multiple variables together");
    var result=this.bottom();
    for(el <- x.value) {
    	//For each variable that is potentially created, it computes its semantics and it considers the upper bound
	    el._1 match {
	      case variable : Identifier => {
	        for(assigned <- x.value) {
	        	val done=new GenericAbstractState[N,H,I](assigned._2._1.createVariable(variable, typ), this._2);
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom()), this.removeExpression()))
	        }
	      }
	      case _ => throw new SymbolicSemanticException("I can assign only variables")
       }
    }
    return result;
  }

  override def createVariableForParameter(x : SymbolicAbstractValue[GenericAbstractState[N,H,I]], typ : Type) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    if(x.value.size != 1 || x.value.elements.next._1.isInstanceOf[VariableIdentifier]==false) 
      throw new SymbolicSemanticException("Cannot declare multiple variables together");
    var result=this.bottom();
    for(el <- x.value) {
    	//For each variable that is potentially a parameter, it computes its semantics and it considers the upper bound
	    el._1 match {
	      case variable : Identifier => {
	        for(assigned <- x.value) {
	        	val done=new GenericAbstractState[N,H,I](assigned._2._1.createVariableForParameter(variable, typ, Nil)._1, this._2);
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom()), this.removeExpression()))
	        }
	      }
	      case _ => throw new SymbolicSemanticException("I can assign only variables")
       }
    }
    return result;
  }
  
  def assignVariable(x : SymbolicAbstractValue[GenericAbstractState[N,H,I]], right : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    if(right.isTop) return top();
    var result : GenericAbstractState[N,H,I] = this.bottom();
    for(el <- x.value) {
    	//For each variable that is potentially assigned, it computes its semantics and it considers the upper bound
	    el._1 match {
	      case variable : Identifier => {
	        for(assigned <- right.value) {
	        	val done=new GenericAbstractState[N,H,I](assigned._2._1.assign(variable, assigned._1), this._2);
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom()), this.removeExpression()))
	        }
	      }
	      case _ => throw new SymbolicSemanticException("I can assign only variables")
       }
    }
    result
  }
   
  def backwardAssignVariable(x : SymbolicAbstractValue[GenericAbstractState[N,H,I]], right : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    if(right.isTop) return top();
    var result : GenericAbstractState[N,H,I] = this.bottom();
    for(el <- x.value) {
    	//For each variable that is potentially assigned, it computes its semantics and it considers the upper bound
	    el._1 match {
	      case variable : Identifier => {
	        for(assigned <- right.value) {
	        	val done=new GenericAbstractState[N,H,I](assigned._2._1.backwardAssign(variable, assigned._1), this._2);
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom()), this.removeExpression()))
	        }
	      }
	      case _ => throw new SymbolicSemanticException("I can assign only variables")
       }
    }
    result
  }
  
  def setParameter(x : SymbolicAbstractValue[GenericAbstractState[N,H,I]], right : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    if(right.isTop) return top();
    var result : GenericAbstractState[N,H,I] = this.bottom();
    for(el <- x.value) {
    	
    	//For each parameter that is set, it computes its semantics and it considers the upper bound
	    el._1 match {
	      case variable : Identifier => {
	        for(assigned <- right.value) {
	        	val done=new GenericAbstractState[N,H,I](assigned._2._1.setParameter(variable, assigned._1), this._2);
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom()), this.removeExpression()))
	        }
	      }
	      case _ => throw new SymbolicSemanticException("I can assign only variables")
       }
    }
    result
  }
  
  def removeVariable(x : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    var result : GenericAbstractState[N,H,I] = this.bottom();
    for(el <- x.value) {
    	//For each variable that is potentially removed, it computes its semantics and it considers the upper bound
	    el._1 match {
	      case variable : Identifier => {
	        for(previousState <- x.value) {
	        	val done=new GenericAbstractState[N,H,I](previousState._2._1.removeVariable(variable), new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom()), this.removeExpression()));
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom()), this.removeExpression()))
	        }
	      }
	      case _ => throw new SymbolicSemanticException("I can remove only variables")
       }
    }
    result
  }
  
  def throws(throwed : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = this.bottom() //TODO: Support exceptions 
   
  def evalNumericalConstant(value : String, typ : Type) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    this.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new Constant(value, typ), this.removeExpression()))
  }
  
  def getVariableValue(id : Identifier) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    val state = new GenericAbstractState(this._1.access(id), this.removeExpression().getExpression());
    new GenericAbstractState(state._1, new SymbolicAbstractValue[GenericAbstractState[N,H,I]](id, state));
  }
  
  def backwardGetVariableValue(id : Identifier) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    val state = new GenericAbstractState(this._1.backwardAccess(id), this.removeExpression().getExpression());
    new GenericAbstractState(state._1, new SymbolicAbstractValue[GenericAbstractState[N,H,I]](id, state));
  }
  
  def getType(variable : Identifier) : Type = {//TODO: is this correct???
    return this._2.getType(variable);
  }
  
  def getFieldValue(objs : List[SymbolicAbstractValue[GenericAbstractState[N,H,I]]], field : String, typ : Type) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    var result : GenericAbstractState[N,H,I] = this.bottom();
    for(obj : SymbolicAbstractValue[GenericAbstractState[N,H,I]] <- objs) {
    	//For each object that is potentially accessed, it computes the semantics of the field access and it considers the upper bound
      	for(expr <- obj.getExpressions) {
     	  val (heapid, newHeap) = obj.get(expr)._1._2.getFieldIdentifier(expr, field, typ);
        var result2=new HeapAndAnotherDomain[N, H, I](obj.get(expr)._1._1, newHeap);
     	  val accessed=result2.access(heapid);
     	  val state=new GenericAbstractState(accessed, new SymbolicAbstractValue[GenericAbstractState[N,H,I]](heapid, new GenericAbstractState(accessed, this._2)));
     	  result=result.lub(result, state);
        }
    }
    result
  }
  
  def backwardGetFieldValue(objs : List[SymbolicAbstractValue[GenericAbstractState[N,H,I]]], field : String, typ : Type) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    var result : GenericAbstractState[N,H,I] = this.bottom();
    for(obj : SymbolicAbstractValue[GenericAbstractState[N,H,I]] <- objs) {
    	//For each object that is potentially accessed, it computes the backward semantics of the field access and it considers the upper bound
     	for(expr <- obj.getExpressions) {
     	  val (heapid, newHeap) = obj.get(expr)._1._2.getFieldIdentifier(expr, field, typ);
        var result2=new HeapAndAnotherDomain[N, H, I](obj.get(expr)._1._1, newHeap);
     	  val accessed=result2.backwardAccess(heapid);
     	  val state=new GenericAbstractState(accessed, new SymbolicAbstractValue[GenericAbstractState[N,H,I]](heapid, new GenericAbstractState(accessed, this._2)));
     	  result=result.lub(result, state);
        }
    }
    result
  }
  
  def setVariableToTop(x : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    var result : GenericAbstractState[N,H,I] = this.bottom();
    for(expr <- x.value) {
    	//For each variable that is forgotten, it computes the semantics and it considers the upper bound
    	if(! expr._1.isInstanceOf[Identifier]) throw new SymbolicSemanticException("Not a variable")
        val variable : Identifier = expr._1.asInstanceOf[Identifier] 
        result=result.lub(result, new GenericAbstractState(this._1.setToTop(variable), this._2));
    }
    result;
  }
  def assert(cond : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = this //TODO
  def assume(cond : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    var d1 = this._1;
    var isFirst = true;
    for(expr <- cond.value) {
    //For each expression that is assumed, it computes the semantics and it considers the upper bound
      if(isFirst) {
        d1=expr._2._1.assume(expr._1);
        isFirst=false;
      }
      else {
        d1=d1.lub(d1, expr._2._1.assume(expr._1))
      }
    }
    return new GenericAbstractState[N,H,I](d1, this._2)
  }
  
  def testTrue() : GenericAbstractState[N,H,I] = {
    var result=this.assume(this.getExpression());
    result.removeExpression();
  }
  
  def testFalse() : GenericAbstractState[N,H,I] = {
    var result=this.assume(this.getExpression().not());
    result.removeExpression();
  }
  
  override def setExpression(value : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    new GenericAbstractState(this._1, value)
  } 
  
  private def getResult() : SymbolicAbstractValue[GenericAbstractState[N,H,I]] = this._2

  override def toSingleLineString() : String = {
    if(isBottom) return "_|_";
    else this._1.toString+
         ";\nExpr.: "+this._2.toString
  }
  override def toString() : String = {
    if(isBottom) return "_|_";
    else this._1.toString+
         "------------------------\n Expression on the top of the stack:\n"+this._2.toString
  }
  
}