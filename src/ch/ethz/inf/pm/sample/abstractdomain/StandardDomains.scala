package ch.ethz.inf.pm.sample.abstractdomain
import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._

trait FunctionalDomain[K, V <: Lattice[V], T <: FunctionalDomain[K, V, T]] extends Lattice[T] {
  override def factory() : T;
  
  var value : Map[K, V]=Map.empty[K, V]
  var isBottom : Boolean = false;

  def add(key : K, value : V) : T = {
    val result : T = this.factory();
    result.value=this.value+((key, value));
    result
  } 
  
  def get(key : K) : V;/* match {
    case None => leftDomain match {
      case Some(l) => l.top()
      case None => throw new StandardDomainException("Codomain not yet instantiated");
      }
    case Some(x) => x
  }*/
  
  def remove(key : K) : T = {
    val result : T = this.factory();
    result.value=this.value-(key);
    result
  } 
  
  def lub(left : T, right : T) : T = {
    if(left.equals(this.bottom())) return right;
    if(right.equals(this.bottom())) return left;
    if(left.equals(this.top()) || right.equals(this.top())) return this.top();
    val res : Map[K, V]= upperBoundFunctionalLifting(left, right);
    val result = this.factory();
    result.value=res
    result
  }
  
  def glb(left : T, right : T) : T =  {
    if(left.equals(this.bottom()) || right.equals(this.bottom())) return this.bottom();
    if(left.equals(this.top())) return right;
    if(right.equals(this.top())) return left;
    var result : Map[K, V]  = Map.empty[K, V] 
    for( el <- left.value.keySet++right.value.keySet ) {
      result=result+((el, left.get(el).glb(left.get(el), right.get(el)))) 
    }
    val domain=this.factory()
    domain.value=result
    domain
  }
  
  final override def widening(left : T, right : T) : T =  {
    val res : Map[K, V]= wideningFunctionalLifting(left, right);
    val result = this.factory();
    result.value=res
    result
  }
  
  final override def lessEqual(r : T) : Boolean = {
    //case bottom
    if(this.isBottom) return true;
    if(r.isBottom) return false;
    //dom(left)\supset dom(right) => ! (left \leq right)
    /*if(this.value.keySet.subsetOf(r.value.keySet)) {
      for(variable <- this.value.keySet)
        if(! this.value.get(variable).get.lessEqual(r.value.get(variable).get) )
          return false;
      return true;
    }
    else return false*/
    for(variable <- this.value.keySet)
      if(! this.get(variable).lessEqual(r.get(variable)) )
        return false;
    for(variable <- r.value.keySet)
      if(! this.get(variable).lessEqual(r.get(variable)) )
        return false;
    return true;
  }
  
  override def equals(a : Any) : Boolean = a match {
    case right : T => 
	    //case bottom
	    if(this.isBottom && right.isBottom) return true;
	    if(this.isBottom || right.isBottom) return false;
	    if(this.value.keySet.equals(right.value.keySet)) {
	      for(variable <- this.value.keySet)
	        if(! this.value.get(variable).get.equals(right.value.get(variable).get) )
	          return false;
	      return true;
	    }
	    else return false
    case _ => false
  }
  
  override def toString() : String = {
    if(isBottom) return "_|_";
    else return ToStringUtilities.mapToString(value);
  }
  
  def top() : T = {
    var result : T=this.factory();
    result.value=Map.empty[K, V]
    for(key <- this.value.keySet)
      result=result.add(key, this.get(key).top())
    result
  }
  
  final def bottom() : T = {
    val result : T = this.factory()
    result.isBottom=true;
    result
  }
  
  final def wideningFunctionalLifting(f1 : T, f2 : T) : Map[K, V] = {
    var result : Map[K, V]  = Map.empty[K, V] 
    for( el <- f1.value.keySet ) {
      f2.value.get(el) match {
        case Some(x) => result=result+((el, x.widening(f1.value.get(el).get, x))) 
        //case None => result=result+((el, f1.value.get(el).get.widening(f1.value.get(el).get, f2.get(el))));
        case None => result=result+((el, f1.value.get(el).get));
      } 
    }
    for( el <- f2.value.keySet ) {
      f1.value.get(el) match {
        case Some(x) => 
        case None => result=result+((el, f2.get(el)))
      } 
    }
    result
  }
  
  final def upperBoundFunctionalLifting(f1 : T, f2 : T) : Map[K, V] = {
    var result : Map[K, V]  = Map.empty[K, V] 
    for( el <- f1.value.keySet ) {
      f2.value.get(el) match {
        case Some(x) => result=result+((el, x.lub(f1.value.get(el).get, x))) 
        case None => result=result+((el, f1.value.get(el).get));
      } 
    }
    for( el <- f2.value.keySet ) {
      f1.value.get(el) match {
        case Some(x) => 
        case None => result=result+((el, f2.get(el)))
      } 
    }
    result
  }
}

trait BoxedDomain[V <: Lattice[V], T <: BoxedDomain[V, T]] extends FunctionalDomain[Identifier, V, T] {
  def getStringOfId(id : Identifier) : String = this.get(id).toString();
  
  def getAddresses[I <: HeapIdentifier[I]]() = {
    var result : Set[I] = Set.empty[I];
    for(id <- this.value.keySet) {
      if(id.isInstanceOf[HeapIdentifier[I]]) result=result+id.asInstanceOf[I];
    }
  }
  
}

trait SetDomain[V, T <: SetDomain[V, T]] extends Lattice[T] {
  override def factory() : T;
  var value : Set[V] = Set.empty[V]
  var isTop=false;
  var isBottom=false;
  
  final def top() : T = {
    val result : T = this.factory();
    result.isBottom=false;
    result.isTop=true;
    result
  }
  
  final def bottom() : T = {
    val result : T = this.factory();
    result.isBottom=true;
    result
  }
  
  final def remove(v : V) : T = {
    val newSet = this.value.-(v);
    val result = this.factory();
    result.value=newSet;
    return result;
  }
  
  def add(el : V) : T = {
    if(this.isTop) return this.top();
    var result = factory();
    result.value=value+el;
    result;
  }
  
  def lub(left : T, right : T) : T = {
    if(left.isTop || right.isTop) return top();
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    val result : T = this.factory()
    result.value=left.value++right.value;
    result
  }
  
  def glb(left : T, right : T) : T = {
    if(left.isBottom || right.isBottom) return bottom();
    if(left.isTop) return right;
    if(right.isTop) return left;
    val result : T = this.factory()
    result.value=left.value**right.value;
    result
  }
  
  def widening(left : T, right : T) : T=this.lub(left, right)
  
  def lessEqual(right : T) : Boolean = {
    if(this.isBottom) return true;
    if(right.isTop) return true;
    this.value.subsetOf(right.value)
  }
  
  override def toString() : String = {
    if(this.isBottom) return "_|_";
    if(this.isTop) return "T";
    return ToStringUtilities.setToString(value);
  }
  
  override def equals(a : Any) : Boolean = a match {
    case x : SetDomain[V, T] => 
      if(this.isBottom && x.isBottom) return true;
      if(this.isTop && x.isTop) return true;
      if(this.isBottom || x.isBottom || this.isTop || x.isTop) return false;
      if(this.value.size!=x.value.size) return false;
      for(el <- this.value)
        if(! x.value.contains(el)) return false;
      return true;
    case _ => return false;
  }
  
  override def hashCode() : Int = {
    var result : Int = 0;
    for(el <- this.value)
      result=result+el.hashCode();
    return result;
  }
  
}

trait InverseSetDomain[V, T <: SetDomain[V, T]] extends SetDomain[V, T] {
  override def factory() : T;

  override def add(el : V) : T = {
    //if(this.isTop) return this.top();
    var result = factory();
    result.isTop=false;
    result.isBottom=false;
    result.value=value+el;
    result;
  }
  
  override def lub(left : T, right : T) : T = {
    if(left.isTop || right.isTop) return top();
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    val result : T = this.factory()
    result.value=left.value**right.value;
    result
  }
  
  override def glb(left : T, right : T) : T = {
    if(left.isBottom || right.isBottom) return bottom();
    if(left.isTop) return right;
    if(right.isTop) return left;
    val result : T = this.factory()
    result.value=left.value++right.value;
    result
  }
  
  override def widening(left : T, right : T) : T=this.lub(left, right)
  
  override def lessEqual(right : T) : Boolean = {
    if(this.isBottom) return true;
    if(right.isTop) return true;
    right.value.subsetOf(this.value)
  }  
}



abstract class ReducedProductDomain[T1 <: Lattice[T1], T2 <: Lattice[T2], T <: ReducedProductDomain[T1, T2, T]](d1 : T1, d2 : T2) extends CartesianProductDomain[T1, T2, T](d1, d2) {
  def reduce() : ReducedProductDomain[T1, T2, T];
}

abstract class CartesianProductDomain[T1 <: Lattice[T1], T2 <: Lattice[T2], T <: CartesianProductDomain[T1, T2, T]](protected var d1 : T1, protected var d2 : T2) extends Lattice[T] {
  
  if(d1.equals(d1.bottom()) || d2.equals(d2.bottom())) {
    d1=d1.bottom();
    d2=d2.bottom();
  }
  
  def _1 : T1 = d1
  def _2 : T2 = d2
  
  override def factory() : T;
  
  
  def top() : T = {
    val result : T = this.factory();
    result.d1=d1.top()
    result.d2=d2.top()
    result
  }
  
  def bottom() : T = {
    val result : T = this.factory();
    result.d1=d1.bottom()
    result.d2=d2.bottom()
    result
  }
  
  def lub(l : T, r : T) : T = {
    val result : T = this.factory();
    result.d1=d1.lub(l._1, r._1)
    result.d2=d2.lub(l._2, r._2)
    result
  }
  
  def glb(l : T, r : T) : T = {
    val result : T = this.factory();
    result.d1=d1.glb(l._1, r._1)
    result.d2=d2.glb(l._2, r._2)
    result
  }
  
  final def widening(l : T, r : T) : T = {
    val result : T = this.factory();
    result.d1=d1.widening(l._1, r._1)
    result.d2=d2.widening(l._2, r._2)
    result
  }
  
  final def lessEqual(r : T) : Boolean = d1.lessEqual(r._1) && d2.lessEqual(r._2)
 
  override def equals(a : Any) : Boolean = a match {
    case right : T => 
	    this._1.equals(right._1) &&
	    this._2.equals(right._2)
    case _ => false
  }
  
  override def toString() = "Domain 1:\n"+this._1.toString+"\nDomain 2:\n"+this._2.toString;
  
}

abstract class SemanticCartesianProductDomain[T1 <: SemanticDomain[T1], T2 <: SemanticDomain[T2], T <: SemanticCartesianProductDomain[T1, T2, T]](a1 : T1, a2 : T2) extends CartesianProductDomain[T1, T2, T](a1, a2) with SemanticDomain[T] {
 def setToTop(variable : Identifier) : T = {
    val result : T = this.factory();
    result.d1=d1.setToTop(variable)
    result.d2=d2.setToTop(variable)
    result
  }
 def assign(variable : Identifier, expr : Expression) : T= {
    val result : T = this.factory();
    result.d1=d1.assign(variable, expr)
    result.d2=d2.assign(variable, expr)
    result
  }
 def setParameter(variable : Identifier, expr : Expression) : T= {
    val result : T = this.factory();
    result.d1=d1.setParameter(variable, expr)
    result.d2=d2.setParameter(variable, expr)
    result
  }
 def assume(expr : Expression) : T= {
    val result : T = this.factory();
    result.d1=d1.assume(expr)
    result.d2=d2.assume(expr)
    result
  }
 def createVariable(variable : Identifier, typ : Type) : T= {
    val result : T = this.factory();
    result.d1=d1.createVariable(variable, typ)
    result.d2=d2.createVariable(variable, typ)
    result
  }
 def createVariableForParameter(variable : Identifier, typ : Type, path : List[String]) = {
    val result : T = this.factory();
    var (a1, b1)=d1.createVariableForParameter(variable, typ, path)
    var (a2, b2)=d2.createVariableForParameter(variable, typ, path)
    result.d1=a1;
    result.d2=a2;
    (result, b1++b2)
  }
 def removeVariable(variable : Identifier) : T= {
    val result : T = this.factory();
    result.d1=d1.removeVariable(variable)
    result.d2=d2.removeVariable(variable)
    result
  }
 def access(field : Identifier) : T= {
    val result : T = this.factory();
    result.d1=d1.access(field)
    result.d2=d2.access(field)
    result
  }
 def backwardAccess(field : Identifier) : T= {
    val result : T = this.factory();
    result.d1=d1.backwardAccess(field)
    result.d2=d2.backwardAccess(field)
    result
  }
 def backwardAssign(variable : Identifier, expr : Expression) : T= {
    val result : T = this.factory();
    result.d1=d1.backwardAssign(variable, expr)
    result.d2=d2.backwardAssign(variable, expr)
    result
  }

  
}

abstract class ReducedSemanticProductDomain[T1 <: SemanticDomain[T1], T2 <: SemanticDomain[T2], T <: ReducedSemanticProductDomain[T1, T2, T]](d1 : T1, d2 : T2) extends SemanticCartesianProductDomain[T1, T2, T](d1, d2) {
 def reduce() : T;
 override def lub(l : T, r : T) : T = super.lub(l, r).reduce();
 override def glb(l : T, r : T) : T = super.glb(l, r).reduce();
 override def setToTop(variable : Identifier) : T = super.setToTop(variable).reduce();
 override def assign(variable : Identifier, expr : Expression) : T = super.assign(variable, expr).reduce();
 override def setParameter(variable : Identifier, expr : Expression) : T = super.setParameter(variable, expr).reduce();
 override def assume(expr : Expression) : T = super.assume(expr).reduce();
 override def createVariable(variable : Identifier, typ : Type) : T = super.createVariable(variable, typ).reduce();
 override def createVariableForParameter(variable : Identifier, typ : Type, path : List[String]) = {
   val (result, i)=super.createVariableForParameter(variable, typ, path);
   (result.reduce(), i);
 }
 override def removeVariable(variable : Identifier) : T = super.removeVariable(variable).reduce();
 override def access(field : Identifier) : T = super.access(field).reduce();
 override def backwardAccess(field : Identifier) : T = super.backwardAccess(field).reduce();
 override def backwardAssign(variable : Identifier, expr : Expression) : T = super.backwardAssign(variable, expr).reduce();
  
}


class StandardDomainException(message : String) extends Exception(message)