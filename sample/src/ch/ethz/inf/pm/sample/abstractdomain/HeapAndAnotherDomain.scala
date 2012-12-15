package ch.ethz.inf.pm.sample.abstractdomain


import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._
import util.HeapIdSetFunctionalLifting

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


  override def toString() : String = "Heap state:\n"+ToStringUtilities.indent(d2.toString())+"\nSemantic state:\n"+ToStringUtilities.indent(d1.toString())

  type T = HeapAndAnotherDomain[N, H, I];

  def _1 = d1;
  def _2 = d2;

  def merge(r : Replacement) : T = if(r.isEmpty) return this; else throw new SemanticException("Merge not yet implemented");
  
  def getStringOfId(id : Identifier) : String = d1.getStringOfId(id)
  
  def getHeap() : H = return d2;
  def getSemanticDomain() : N = return d1;

  def getIds() = this._1.getIds()++this._2.getIds();

  def factory() = {
//    assert(d1 != null);
//    assert(d2 != null);
    new HeapAndAnotherDomain[N, H, I](d1.factory(), d2.factory());
  }
  
  def createVariableForArgument(variable : Assignable, typ : Type, path : List[String]) = {
    SystemParameters.heapTimer.start();
    val (s2, ids, r) = this.d2.createVariableForArgument(variable, typ, path);
    SystemParameters.heapTimer.stop();
    var s1 = this.d1;
    s1=applyToAssignable[N](variable, s1, _.createVariableForArgument(_, typ, path)._1);
    variable match {
      case x : VariableIdentifier =>
        s1=s1.createVariableForArgument(x, typ, path)._1
      case x : HeapIdSetDomain[I] =>
        var first : Boolean = true;
        for(singleid <- x.value)
          if(first) {
            first=false;
            s1=s1.createVariableForArgument(singleid, typ, path)._1;
          }
          else
            s1=x.combinator(s1, s1.createVariableForArgument(singleid, typ, path)._1);
    }
    //We recursively create the entry state for all the entry abstract nodes.
    SystemParameters.domainTimer.start();
    s1=s1.merge(r)
    for(id <- ids.keys)
      if(!id.equals(variable))
        s1=s1.createVariableForArgument(id, typ, ids.apply(id))._1;
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

  def assignArrayCell(variable : Assignable, index : Expression, expr : Expression, typ : Type) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (h2,r2)=d2.assignArrayCell(variable, index, expr, d1)
    val (id, h, r1) = h2.getArrayCell(variable, index, d1, typ)
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


  /**
  Assign a cell of an collection

  @param collection The object on which the collection assignment
  @param index The assigned index
  @param right The assigned expression
  @return The abstract state obtained after the collection cell assignment
    */
  def assignCollectionCell(collection: Assignable, index: Expression, right: Expression): T = {
    val result: T = this.factory()
    SystemParameters.heapTimer.start()
    val (h2, r2) = d2.assignCollectionCell(collection, index, right, d1)
    val (id, h, r1) = h2.getCollectionCell(collection, index, r2)
    val (h3, r3) = h.endOfAssignment()
    result.d2 = h3
    result.d1 = r2.merge(r3)
    SystemParameters.heapTimer.stop()
    SystemParameters.domainTimer.start()
    var newd1: Option[N] = None
    if (id.isTop)
      newd1 = Some(result.d1.top())
    else
      for (singleheapid <- id.value) {
        if (newd1 == None)
          newd1 = Some(result.d1.assign(singleheapid, right))
        else newd1 = Some(id.combinator(newd1.get, result.d1.assign(singleheapid, right)))
      }
    if (newd1 != None)
      result.d1 = newd1.get //throw new SemanticException("You should assign to something")
    else result.d1 = result.d1
    SystemParameters.domainTimer.stop()
    result
  }

  def setArgument(variable : Assignable, expr : Expression) : T= {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, r) =d2.setArgument(variable, expr)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    result.d1=d1.merge(r)
    result.d1=applyToAssignable[N](variable, result.d1, _.setArgument(_, expr));
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
    val (d, rep) =d2.lubWithReplacement(l.d2, r.d2, l.d1, r.d1)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    val s = d1.lub(l.d1, r.d1)
    result.d1 = s.merge(rep)
    SystemParameters.domainTimer.stop();
    result
  }

 override def glb(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, rep) =d2.glbWithReplacement(l.d2, r.d2, l.d1, r.d1)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    val s = d1.glb(l.d1, r.d1)
   result.d1= s.merge(rep)
    SystemParameters.domainTimer.stop();
    result
  }

 override def widening(l : T, r : T) : T = {
    val result : T = this.factory();
    SystemParameters.heapTimer.start();
    val (d, rep) =d2.wideningWithReplacement(l.d2, r.d2, l.d1, r.d1)
    result.d2=d;
    SystemParameters.heapTimer.stop();
    SystemParameters.domainTimer.start();
    val s = d1.widening(l.d1, r.d1)
    result.d1= s.merge(rep)
    SystemParameters.domainTimer.stop();
    result
  }

 override def lessEqual(r : T) : Boolean = {
    if(this.d1.lessEqual(this.d1.bottom()) || this.d2.lessEqualWithReplacement(this.d2.bottom(), this.d1, this.d1.bottom())._1) return true;
    if(r.d1.lessEqual(r.d1.bottom()) || r.d2.lessEqualWithReplacement(r.d2.bottom(), r.d1, r.d1.bottom())._1) return false;
    SystemParameters.heapTimer.start();
    var (b, rep) = d2.lessEqualWithReplacement(r.d2, this.d1, r.d1)
    SystemParameters.heapTimer.stop();
    if(! b) return false;
    SystemParameters.domainTimer.start();
    b = d1.merge(rep).lessEqual(r.d1.merge(rep));
    SystemParameters.domainTimer.stop();
    return b;
  }



  private def applyToAssignable[L <: Lattice[L]](variable : Assignable, state : L, functor : (L, Identifier) => L) : L = {
    variable match {
      case x : VariableIdentifier =>
        return functor(state, x)
      case x : I =>
        var result : L = functor(state, x)
        return result;
    }
  }


}
