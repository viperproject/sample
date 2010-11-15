package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._


class HeapAndAnotherDomain[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](n1 : N, h1 : H) extends CartesianProductDomain[N, H, HeapAndAnotherDomain[N, H, I]](n1, h1) with SemanticDomain[HeapAndAnotherDomain[N, H, I]]{
  
  def getStringOfId(id : Identifier) : String = d1.getStringOfId(id)
  
  def getHeap() : H = return d2;
  def getSemanticDomain() : N = return d1;
  
  def factory() = new HeapAndAnotherDomain(d1.factory(), d2.factory());

  def setToTop(variable : Identifier) : HeapAndAnotherDomain[N, H, I] = new HeapAndAnotherDomain[N, H, I](this._1.setToTop(variable), this._2.setToTop(variable))
  
  def assign(variable : Identifier, expr : Expression) : HeapAndAnotherDomain[N, H, I] = new HeapAndAnotherDomain[N, H, I](this._1.assign(variable, expr), this._2.assign(variable, expr))
  
  def setParameter(variable : Identifier, expr : Expression) : HeapAndAnotherDomain[N, H, I] = new HeapAndAnotherDomain[N, H, I](this._1.setParameter(variable, expr), this._2.assign(variable, expr))
  
  def assume(expr : Expression) : HeapAndAnotherDomain[N, H, I] = new HeapAndAnotherDomain[N, H, I](this._1.assume(expr), this._2.assume(expr))
  
  def createVariable(variable : Identifier, typ : Type) : HeapAndAnotherDomain[N, H, I] = new HeapAndAnotherDomain[N, H, I](this._1.createVariable(variable, typ), this._2.createVariable(variable, typ))
  
  def createVariableForParameter(variable : Identifier, typ : Type, path : List[String]) = {
    var (s2, ids) = this._2.createVariableForParameter(variable, typ, path);
    var s1 = this._1;
    val i = null;
    s1=s1.createVariableForParameter(variable, typ, path)._1
    for(id <- ids.keys)
      s1=s1.createVariableForParameter(id, typ, ids.apply(id))._1;
    (new HeapAndAnotherDomain[N, H, I](s1, s2), ids)
  }
  
  def removeVariable(variable : Identifier) : HeapAndAnotherDomain[N, H, I] = new HeapAndAnotherDomain[N, H, I](this._1.removeVariable(variable), this._2.removeVariable(variable))
  
  def access(field : Identifier) : HeapAndAnotherDomain[N, H, I] = new HeapAndAnotherDomain[N, H, I](this._1.access(field), this._2.access(field))
  
  def backwardAccess(field : Identifier) : HeapAndAnotherDomain[N, H, I] = new HeapAndAnotherDomain[N, H, I](this._1.backwardAccess(field), this._2)
  
  def backwardAssign(variable : Identifier, expr : Expression) : HeapAndAnotherDomain[N, H, I] = new HeapAndAnotherDomain[N, H, I](this._1.backwardAssign(variable, expr), this._2)
}

class GenericAbstractState[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](n1 : HeapAndAnotherDomain[N, H, I], r1 : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) extends
  CartesianProductDomain[HeapAndAnotherDomain[N, H, I], SymbolicAbstractValue[GenericAbstractState[N,H,I]], GenericAbstractState[N,H,I]](n1, r1) with State[GenericAbstractState[N,H,I]] with SingleLineRepresentation {
  
  def factory() = new GenericAbstractState(this._1.top(), this._2.top());
  def getHeap() : H = d1.getHeap();
  def getSemanticDomain() : N = d1.getSemanticDomain();
  def isBottom() : Boolean = this._1.equals(this._1.bottom()) || this._2.equals(this._2.bottom());
  def getStringOfId(id : Identifier) : String = this._1.getStringOfId(id)
  def createAddress(typ : Type, pp : ProgramPoint) : GenericAbstractState[N,H,I] =  {
    if(this.isBottom) return this;
    val createdLocation=this._1._2.createAddress(typ, pp)
    var result=this._1.createVariable(createdLocation, typ);
    for((field, typ2) <- typ.getPossibleFields()) {
     val address = this._1._2.getFieldIdentifier(createdLocation, field, typ2);
     result=result.createVariable(address, typ2);
    }
    this.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](createdLocation, new GenericAbstractState(result, this._2).removeExpression()));
  }
  
  def createAddressForParameter(typ : Type, pp : ProgramPoint, path : List[String]) : GenericAbstractState[N,H,I] =  {
    if(this.isBottom) return this;
    val createdLocation=this._1._2.createAddress(typ, pp)
    val (result, ids)=this._1.createVariableForParameter(createdLocation, typ, path);
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
      	for(expr <- obj.getExpressions) {
     	  val heapid : I = obj.get(expr)._1._2.getFieldIdentifier(expr, field, typ);
     	  val accessed=obj.get(expr)._1.access(heapid);
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
     	for(expr <- obj.getExpressions) {
     	  val heapid : I = obj.get(expr)._1._2.getFieldIdentifier(expr, field, typ);
     	  val accessed=obj.get(expr)._1.backwardAccess(heapid);
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
    	if(! expr._1.isInstanceOf[Identifier]) throw new SymbolicSemanticException("Not a variable")
        val variable : Identifier = expr._1.asInstanceOf[Identifier]//new VariableIdentifier(expr._1.asInstanceOf[VariableIdentifier[T]].name, expr._1.getType); 
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