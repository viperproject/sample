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
class HeapAndAnotherDomain[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](private var d1 : N, private var d2 : H) extends Lattice[HeapAndAnotherDomain[N, H, I]]{


  override def toString() : String = "Heap state:\n"+d2.toString()+"\nSemantic state:\n"+d1.toString();

  type T = HeapAndAnotherDomain[N, H, I];

  def _1() = d1;
  def _2() = d2;

  def merge(r : Replacement) : T = if(r.isEmpty) return this; else throw new SemanticException("Merge not yet implemented");
  
  def getStringOfId(id : Identifier) : String = d1.getStringOfId(id)
  
  def getHeap() : H = return d2;
  def getSemanticDomain() : N = return d1;

  def getIds() = this._1.getIds()++this._2.getIds();

  def factory() = new HeapAndAnotherDomain[N, H, I](d1.factory(), d2.factory());
  
  def createVariableForParameter(variable : Assignable, typ : Type, path : List[String]) = {
    SystemParameters.heapTimer.start();
    val (s2, ids, r) = this.d2.createVariableForParameter(variable, typ, path);
    SystemParameters.heapTimer.stop();
    var s1 = this.d1;
    s1=applyToAssignable[N](variable, s1, _.createVariableForParameter(_, typ, path)._1);
    variable match {
      case x : VariableIdentifier =>
        s1=s1.createVariableForParameter(x, typ, path)._1
      case x : HeapIdSetDomain[I] =>
        var first : Boolean = true;
        for(singleid <- x.value)
          if(first) {
            first=false;
            s1=s1.createVariableForParameter(singleid, typ, path)._1;
          }
          else
            s1=x.combinator(s1, s1.createVariableForParameter(singleid, typ, path)._1);
    }
    //We recursively create the entry state for all the entry abstract nodes.
    SystemParameters.domainTimer.start();
    s1=s1.merge(r)
    for(id <- ids.keys)
      if(!id.equals(variable))
        s1=s1.createVariableForParameter(id, typ, ids.apply(id))._1;
    SystemParameters.domainTimer.stop();
    (new HeapAndAnotherDomain[N, H, I](s1, s2), ids)
  }


  def setToTop(variable : Assignable) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.setToTop(variable)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r)
    result.d1=applyToAssignable[N](variable, result.d1, _.setToTop(_));
    SystemParameters.domainTimer.stop();
    result
  }
 def assign(variable : Assignable, expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d,r)=d2.assign(variable, expr, d1)
    val (d3, r1) = d.endOfAssignment();
    result.d2=d3;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r).merge(r1)
    result.d1=applyToAssignable[N](variable, result.d1, _.assign(_, expr));
    SystemParameters.domainTimer.stop();
    result
  }

  def assignField(variable : Assignable, field : String, expr : Expression, typ : Type, pp : ProgramPoint) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (h2,r2)=d2.assignField(variable, field, expr)
    val (id, h, r1) = h2.getFieldIdentifier(variable, field, typ, pp)
    val (h3, r3)= h.endOfAssignment();
    result.d2=h3;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r2).merge(r1).merge(r3);
    var newd1 : Option[N]= None;
    if(id.isTop)
      newd1 = Some(result.d1.top());
    else
      for(singleheapid <- id.value) {
        if(newd1==None)
          newd1=Some(result.d1.assign(singleheapid, expr))
        else newd1=Some(id.combinator(newd1.get, result.d1.assign(singleheapid, expr)))
      }
    if(newd1!=None)
      result.d1=newd1.get; //throw new SemanticException("You should assign to something")
    else result.d1=result.d1;
    SystemParameters.domainTimer.stop();
    result
  }

 def setParameter(variable : Assignable, expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.setParameter(variable, expr)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r)
    result.d1=applyToAssignable[N](variable, result.d1, _.setParameter(_, expr));
    SystemParameters.domainTimer.stop();
    result
  }
 def assume(expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.assume(expr)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r).assume(expr)
    SystemParameters.domainTimer.stop();
    result
  }
 def createVariable(variable : Assignable, typ : Type) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.createVariable(variable, typ)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=applyToAssignable[N](variable, this.d1, _.createVariable(_, typ));
    SystemParameters.domainTimer.stop();
    result
  }
 def removeVariable(variable : Assignable) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.removeVariable(variable)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=applyToAssignable[N](variable, this.d1, _.removeVariable(_));
    SystemParameters.domainTimer.stop();
    result
  }
 def access(field : Assignable) : T= {
    val result : T = this.factory();
    SystemParameters.domainTimer.start();
    result.d1=applyToAssignable[N](field, this.d1, _.access(_));
    SystemParameters.domainTimer.stop();
    SystemParameters.heapTimer.start();
    result.d2=d2//Access does not change the state of the heap domain
    SystemParameters.heapTimer.stop();
    result
  }
 def backwardAccess(field : Assignable) : T= {
    val result : T = this.factory();
    SystemParameters.domainTimer.start();
    result.d1=applyToAssignable[N](field, this.d1, _.backwardAccess(_));
    SystemParameters.domainTimer.stop();
    SystemParameters.heapTimer.start();
    result.d2 =d2 //Backward access does not change the state of the heap domain
    SystemParameters.heapTimer.stop();
    result
  }
 def backwardAssign(variable : Assignable, expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.backwardAssign(variable, expr)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r);
    result.d1=applyToAssignable[N](variable, result.d1, _.backwardAssign(_, expr));
    SystemParameters.domainTimer.stop();
    result
  }

 override def top() : T = {
    val result : T = this.factory();
    SystemParameters.domainTimer.start();
    result.d1=d1.top()
    SystemParameters.domainTimer.stop();
    SystemParameters.heapTimer.start();
    result.d2=d2.top()
    SystemParameters.heapTimer.stop();
    result
  }

 override def bottom() : T = {
    val result : T = this.factory();
    SystemParameters.domainTimer.start();
    result.d1=d1.bottom()
    SystemParameters.domainTimer.stop();
    SystemParameters.heapTimer.start();
    result.d2=d2.bottom()
    SystemParameters.heapTimer.stop();
    result
  }

 override def lub(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, rep) =d2.lubWithReplacement(l.d2, r.d2)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(rep).lub(l.d1, r.d1)
    SystemParameters.domainTimer.stop();
    result
  }

 override def glb(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, rep) =d2.glbWithReplacement(l.d2, r.d2)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(rep).glb(l.d1, r.d1)
    SystemParameters.domainTimer.stop();
    result
  }

 override def widening(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, rep) =d2.wideningWithReplacement(l.d2, r.d2)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(rep).widening(l.d1, r.d1)
    SystemParameters.domainTimer.stop();
    result
  }

 override def lessEqual(r : T) : Boolean = {
    if(this.d1.lessEqual(this.d1.bottom()) || this.d2.lessEqual(this.d2.bottom())) return true;
    if(r.d1.lessEqual(r.d1.bottom()) || r.d2.lessEqual(r.d2.bottom())) return false;
    SystemParameters.domainTimer.start();
    var b : Boolean = d1.lessEqual(r.d1);
    SystemParameters.domainTimer.stop();
    SystemParameters.heapTimer.start();
    b = b && d2.lessEqual(r.d2)
    SystemParameters.heapTimer.stop();
    return b;
  }



  private def applyToAssignable[L <: Lattice[L]](variable : Assignable, state : L, functor : (L, Identifier) => L) : L = {
    variable match {
      case x : VariableIdentifier =>
        return functor(state, x)
      case x : HeapIdSetDomain[I] =>
        var result : L = state;
        var first : Boolean = true;
        for(singleid <- x.value)
          if(first) {
            first=false;
            result=functor(result, singleid)
          }
          else
            result=x.combinator(result, functor(result, singleid));
        return result;
    }
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

  def getSemanticDomainState() : N = n1._1();
  def factory() = new GenericAbstractState(this._1.top(), this._2.top());
  def getHeap() : H = d1.getHeap();
  def getSemanticDomain() : N = d1.getSemanticDomain();
  def isBottom() : Boolean = this._1.equals(this._1.bottom()) || this._2.equals(this._2.bottom());
  def getStringOfId(id : Identifier) : String = this._1.getStringOfId(id)
  def createObject(typ : Type, pp : ProgramPoint) : GenericAbstractState[N,H,I] =  {
    if(this.isBottom) return this;
    //It discharges on the heap analysis the creation of the object and its fields
    var (createdLocation, newHeap, rep)=this._1._2.createObject(typ, pp)
    var result=new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap);

    result=result.createVariable(createdLocation, typ)
    var result2 = result;
    for(field <- typ.getPossibleFields()) {
         val (address, newHeap2, rep1) = result2._2.getFieldIdentifier(createdLocation, field.getName(), field.getType(), field.getProgramPoint());
         result2=new HeapAndAnotherDomain[N, H, I](result2._1.merge(rep1), newHeap2).createVariable(address, field.getType());
    }
    val (h, rep2) = result2._2.endOfAssignment()
    result2 = new HeapAndAnotherDomain[N, H, I](result2._1.merge(rep2), h);
    //An object could have no fields, so that's acceptable to have result3==None here
    this.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](createdLocation, new GenericAbstractState(result2, this._2).removeExpression()));
  }
  
  def createObjectForParameter(typ : Type, pp : ProgramPoint, path : List[String]) : GenericAbstractState[N,H,I] =  {
    if(this.isBottom) return this;
    //It discharges on the heap analysis the creation of the object and its fields
    val (createdLocation, newHeap, rep)=this._1._2.createObject(typ, pp)
    var result=new HeapAndAnotherDomain[N, H, I](this._1._1.merge(rep), newHeap);
     //It asks the semantic domain to simply create the initial value for the given identifier
      val (result1, ids)=result.createVariableForParameter(createdLocation, typ, path);
      this.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](createdLocation, new GenericAbstractState(result1, this._2).removeExpression()));
  }
  
  def getExpression() : SymbolicAbstractValue[GenericAbstractState[N,H,I]] = getResult
  
  def removeExpression() : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    new GenericAbstractState(this._1, new SymbolicAbstractValue[GenericAbstractState[N,H,I]](Some(this), r1.typ))
  }
  
  def createVariable(x : SymbolicAbstractValue[GenericAbstractState[N,H,I]], typ : Type, pp : ProgramPoint) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    if(x.value.size != 1 || x.value.elements.next._1.isInstanceOf[VariableIdentifier]==false) 
      throw new SymbolicSemanticException("Cannot declare multiple variables together");
    var result=this.bottom();
    for(el <- x.value) {
    	//For each variable that is potentially created, it computes its semantics and it considers the upper bound
	    el._1 match {
	      case variable : Assignable => {
	        for(assigned <- x.value) {
	        	val done=new GenericAbstractState[N,H,I](assigned._2._1.createVariable(variable, typ), this._2);
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom(), pp), this.removeExpression()))
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
	      case variable : Assignable => {
	        for(assigned <- x.value) {
            val r = assigned._2._1.createVariableForParameter(variable, typ, Nil);
            val left = r._1;
	        	val done=new GenericAbstractState[N,H,I](left, this._2);
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom(), variable.getProgramPoint), this.removeExpression()))
	        }
	      }
	      case _ => throw new SymbolicSemanticException("I can assign only variables")
       }
    }
    return result;
  }
  
  def assignVariable(x : SymbolicAbstractValue[GenericAbstractState[N,H,I]], right : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    if(right.isTop)
      return this.setVariableToTop(x).removeExpression()
    if(right.isTop) return top();
    var result : GenericAbstractState[N,H,I] = this.bottom();
    for(el <- x.value) {
    	//For each variable that is potentially assigned, it computes its semantics and it considers the upper bound
	    el._1 match {
	      case variable : Assignable => {
	        for(assigned <- right.value) {
	        	val done=new GenericAbstractState[N,H,I](assigned._2._1.assign(variable, assigned._1), this._2);
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom(), variable.getProgramPoint), this.removeExpression()))
	        }
	      }
	      case _ => throw new SymbolicSemanticException("I can assign only variables here")
       }
    }
    result
  }

  def assignField(x : List[SymbolicAbstractValue[GenericAbstractState[N,H,I]]], field : String, right : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    var result : Option[GenericAbstractState[N,H,I]] = None;
    if(right.isTop) {
      var t : GenericAbstractState[N,H,I] = this.getFieldValue(x, field, right.getType(this));
      return t.setVariableToTop(t.getExpression).removeExpression()
    }
    for(obj <- x) {
      for(el <- obj.value) {
    	  //For each variable that is potentially assigned, it computes its semantics and it considers the upper bound
	      el._1 match {
	        case variable : Identifier => {
	          for(assigned <- right.value) {
	        	  val done=new GenericAbstractState[N,H,I](assigned._2._1.assignField(variable, field, assigned._1, right.getType(this), variable.getProgramPoint() ), this._2);
              if(result==None)
                result=Some(done)
	        	  else result=Some(done.lub(result.get, done));
		          //result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom(), variable.getProgramPoint), this.removeExpression()))
	          }
	        }
          case heapid : HeapIdSetDomain[I] => {
	          for(assigned <- right.value) {
              val done=new GenericAbstractState[N,H,I](assigned._2._1.assignField(heapid, field, assigned._1, right.getType(this), heapid.getProgramPoint() ), this._2);
              if(result==None)
                result=Some(done)
	        	  else result=Some(done.lub(result.get, done));
	          }
	        }

	        case _ => throw new SymbolicSemanticException("I can assign only variables and heap ids here")
        }
      }
    }
    if(result==None)
      throw new SymbolicSemanticException(("You should assign something to something"))
    result.get.removeExpression();
  }
   
  def backwardAssignVariable(x : SymbolicAbstractValue[GenericAbstractState[N,H,I]], right : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    if(right.isTop) return top();
    var result : GenericAbstractState[N,H,I] = this.bottom();
    for(el <- x.value) {
    	//For each variable that is potentially assigned, it computes its semantics and it considers the upper bound
	    el._1 match {
	      case variable : Assignable => {
	        for(assigned <- right.value) {
	        	val done=new GenericAbstractState[N,H,I](assigned._2._1.backwardAssign(variable, assigned._1), this._2);
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom(), variable.getProgramPoint), this.removeExpression()))
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
	      case variable : Assignable => {
	        for(assigned <- right.value) {
	        	val done=new GenericAbstractState[N,H,I](assigned._2._1.setParameter(variable, assigned._1), this._2);
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom(), variable.getProgramPoint), this.removeExpression()))
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
	      case variable : Assignable => {
	        for(previousState <- x.value) {
	        	val done=new GenericAbstractState[N,H,I](previousState._2._1.removeVariable(variable), new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom(), variable.getProgramPoint), this.removeExpression()));
	        	result=result.lub(result, done);
		        result=result.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new UnitExpression(variable.getType().bottom(), variable.getProgramPoint), this.removeExpression()))
	        }
	      }
	      case _ => throw new SymbolicSemanticException("I can remove only variables")
       }
    }
    result
  }
  
  def throws(throwed : SymbolicAbstractValue[GenericAbstractState[N,H,I]]) : GenericAbstractState[N,H,I] = this.bottom() //TODO: Support exceptions 
   
  def evalNumericalConstant(value : String, typ : Type, pp : ProgramPoint) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    this.setExpression(new SymbolicAbstractValue[GenericAbstractState[N,H,I]](new Constant(value, typ, pp), this.removeExpression()))
  }
  
  def getVariableValue(id : Assignable) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    val state = new GenericAbstractState(this._1.access(id), this.removeExpression().getExpression());
    new GenericAbstractState(state._1, new SymbolicAbstractValue[GenericAbstractState[N,H,I]](id.asInstanceOf[Expression], state));
  }
  
  def backwardGetVariableValue(id : Assignable) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    val state = new GenericAbstractState(this._1.backwardAccess(id), this.removeExpression().getExpression());
    new GenericAbstractState(state._1, new SymbolicAbstractValue[GenericAbstractState[N,H,I]](id.asInstanceOf[Expression], state));
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
        if(! expr.isInstanceOf[Assignable]) throw new SymbolicSemanticException("Only assignable objects should be here");
     	  val (heapid, newHeap, rep) = obj.get(expr)._1._2.getFieldIdentifier(expr.asInstanceOf[Assignable], field, typ, expr.getProgramPoint());
        var result2=new HeapAndAnotherDomain[N, H, I](obj.get(expr)._1._1.merge(rep), newHeap);
        val accessed=result2.access(heapid);
     	  val state=new GenericAbstractState(accessed, new SymbolicAbstractValue[GenericAbstractState[N,H,I]](heapid, new GenericAbstractState(accessed, this._2)));
     	  result=result.lub(result, state);
        }
    }
    result
  }

  def getArrayCell(obj : List[SymbolicAbstractValue[GenericAbstractState[N,H,I]]], index : List[SymbolicAbstractValue[GenericAbstractState[N,H,I]]], typ : Type) : GenericAbstractState[N,H,I] =
  throw new SemanticException("Arrays not yet implemented")
  
  def backwardGetFieldValue(objs : List[SymbolicAbstractValue[GenericAbstractState[N,H,I]]], field : String, typ : Type) : GenericAbstractState[N,H,I] = {
    if(this.isBottom) return this;
    var result : GenericAbstractState[N,H,I] = this.bottom();
    for(obj : SymbolicAbstractValue[GenericAbstractState[N,H,I]] <- objs) {
    	//For each object that is potentially accessed, it computes the backward semantics of the field access and it considers the upper bound
     	for(expr <- obj.getExpressions) {
        if(! expr.isInstanceOf[Assignable]) throw new SymbolicSemanticException("Only assignable objects should be here");
     	  val (heapid, newHeap, rep) = obj.get(expr)._1._2.getFieldIdentifier(expr.asInstanceOf[Assignable], field, typ, expr.getProgramPoint());
        var result2=new HeapAndAnotherDomain[N, H, I](obj.get(expr)._1._1.merge(rep), newHeap);
        var accessed = result2.backwardAccess(heapid)
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
    	if(expr._1.isInstanceOf[Assignable]) {
        val variable : Assignable = expr._1.asInstanceOf[Assignable]
        result=result.lub(result, new GenericAbstractState(this._1.setToTop(variable), this._2));
      }
      else throw new SymbolicSemanticException("Something assignable expected here")
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