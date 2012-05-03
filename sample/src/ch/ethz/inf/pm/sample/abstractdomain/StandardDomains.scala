//This file contains several implementations of some standard domains. These standard domains are quite useful to develop
//new analyses without rewriting some standard code.

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._
import com.sun.corba.se.spi.ior.IORFactories

/**
 * The representation of a functional domain, that is, a domain that is represented by a function whose
 * codomain is a lattice. The lattice operators are the functional extensions of the lattice operators of
 * the codomain.
 *
 * @param <K> The type of the keys
 * @param <V> The type of the codomain that has to be a lattice
 * @param <T> The type of the current functional domain
 * @author Pietro Ferrara
 * @since 0.1
 */
trait FunctionalDomain[K, V <: Lattice[V], T <: FunctionalDomain[K, V, T]] extends Lattice[T] {
  override def factory() : T;
  
  var value : Map[K, V]=Map.empty[K, V]
  var isBottom : Boolean = false;

  /**
   * Adds [key->value] to the domain 
   * @param key The key
   * @param value The value 
   * @return The state of the domain after the assignment
   */
  def add(key : K, value : V) : T = {
    val result : T = this.factory();
    result.value=this.value+((key, value));
    result
  } 
  
  /**
   * Returns the value of key. It is not implemented since in some domains if the domain is not defined on the given
   * key we have a top value, in some others we have a bottom value. So we decided to let to the particular instance
   * of the domain the opportunity to define it in order to have a total function (that is more convenient when
   * defining the other lattice operators).
   *  
   * @param key The key
   * @return The value related to the given key
   */
  def get(key : K) : V;
  
  /**
   * Removes the key from the domain. 
   * @param key The key to be removed
   * @return The state of the domain after the kays has been removed
   */
  def remove(key : K) : T = {
    val result : T = this.factory();
    result.value=this.value-(key);
    result
  } 
  
  /**
   * Computes the upper bound between two states. It is defined by:
   * left \sqcup right = [k -> left(k) \sqcup right(k) : k \in dom(left) \cup dom(right)]   
   * @param left One of the two operands
   * @param right The other operand
   * @return The upper bound of left and right
   */
  def lub(left : T, right : T) : T = {
    if(left.equals(this.bottom())) return right;
    if(right.equals(this.bottom())) return left;
    //if(left.equals(this.top()) || right.equals(this.top())) return this.top();
    if(left.equals(right)) return left;
    val res : Map[K, V]= upperBoundFunctionalLifting(left, right);
    val result = this.factory();
    result.value=res
    result
  }
  
  
  /**
   * Computes the lower bound between two states. It is defined by:
   * left \sqcap right = [k -> left(k) \sqcap right(k) : k \in dom(left) \cap dom(right)]   
   * @param left One of the two operands
   * @param right The other operand
   * @return The lower bound of left and right
   */
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
  
  /**
   * Computes the widening between two states. It is defined by:
   * left \nable right = [k -> left(k) \nabla right(k) : k \in dom(left) \cup dom(right)]   
   * @param left The left operand
   * @param right The right operand
   * @return The upper bound of left and right
   */
  override def widening(left : T, right : T) : T =  {
	if(left.isBottom && right.isBottom) return this.bottom();
	if(left.isBottom) return right;
	if(right.isBottom) return left;
    val res : Map[K, V]= wideningFunctionalLifting(left, right);
    val result = this.factory();
    result.value=res
    result
  }
  
  /**
   * Implements the partial ordering between two states of functional domains. It is defined by: 
   * this \leq r <==> \forall k \in dom(this) : this(k) \leq r(k)   
   * @param r The right operand
   * @return true iff this is less or equal than t
   */
  override def lessEqual(r : T) : Boolean = {
    //case bottom
    if(this.isBottom) return true;
    if(r.isBottom) return false;
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
  
  def bottom() : T = {
    val result : T = this.factory()
    result.isBottom=true;
    result
  }
  
  private def wideningFunctionalLifting(f1 : T, f2 : T) : Map[K, V] = {
    var result : Map[K, V]  = Map.empty[K, V] 
    for( el <- f1.value.keySet ) {
      f2.value.get(el) match {
        case Some(x) => result=result+((el, x.widening(f1.value.get(el).get, x))) 
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
  
  private def upperBoundFunctionalLifting(f1 : T, f2 : T) : Map[K, V] = {
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

/** 
 * The representation of a boxed domain, that is, a domain that is a functional domain whose keys are (variable
 * or heap) identifiers
 *
 * @param <V> The type of the codomain that has to be a lattice
 * @param <T> The type of the current boxed domain
 * @author Pietro Ferrara
 * @since 0.1
 */
trait BoxedDomain[V <: Lattice[V], T <: BoxedDomain[V, T]] extends FunctionalDomain[Identifier, V, T] {
  def merge(r : Replacement) : T = {
    if(r.isEmpty) return this.asInstanceOf[T];
    var result : T = this.clone;
    val removedVariables : scala.collection.Set[Identifier]= flatten(r.keySet);
    //We remove the variables from the result state
    for(v <- removedVariables)
      result=result.remove(v);
    for(s <- r.keySet) {
      var value : V = this.get(s.iterator.next).bottom();
      //We compute the value that should be assigned to all other ids
      for(v <- s)
        value=value.lub(value, this.get(v));
      //We assign the value to all other ids
      for(v <- r.apply(s))
        result=result.merge(v, value);
    }
    return result;
  };
  override def clone() : T = {
    val result = this.factory();
    for(k <- this.value.keySet)
      result.value=result.value+((k, this.value.apply(k)));
    return result;
  }
  private def merge(id : Identifier, v : V) : T = {
    if(this.value.keySet.contains(id))
      return this.add(id, v.lub(v, this.get(id)));
    else return this.add(id, v);
  }
  private def flatten[A](s : scala.collection.Set[Set[A]]) : scala.collection.Set[A] = {
    var result : scala.collection.Set[A] = Set.empty[A];
    for(el <- s) {
      result=result.union(el);
    }
    return result;
  }



  def getIds = this.value.keySet;

  def getStringOfId(id : Identifier) : String = this.get(id).toString();
  
  def getAddresses[I <: HeapIdentifier[I]]() = {
    var result : Set[I] = Set.empty[I];
    for(id <- this.value.keySet) {
      if(id.isInstanceOf[HeapIdentifier[I]]) result=result+id.asInstanceOf[I];
    }
  }
  
}

/** 
 * The representation of a set domain, that is, a domain that is represented by a set. The lattice operators 
 * are the common ones of sets, that is, the upper bound is the union, the lower bound the intersection, and so on.
 *
 * @param <V> The type of the values contained in the set
 * @param <T> The type of the current set domain
 * @author Pietro Ferrara
 * @since 0.1
 */
trait SetDomain[V, T <: SetDomain[V, T]] extends Lattice[T] {

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
    result.isTop = false;
    result.isBottom=true;
    result
  }
  
  /**
   * Removes an element from the set. Formally, return = old(this)\setminus {v}
   * @param v The element to be removed
   * @return The abstract state without the given element.
   */
  def remove(v : V) : T = {
    val newSet = this.value.-(v);
    val result = this.factory();
    result.value=newSet;
    return result;
  }
  
  /**
   * Adds an element to the set. Formally, return = old(this) \cup {v}
   * @param v The element to be added
   * @return The abstract state with the given element as well.
   */
  def add(v : V) : T = {
    if(this.isTop) return this.top();
    var result = factory();
    result.value=value+v;
    result;
  }


  /**
   * Adds an element to the set. Formally, return = old(this) \cup {v}
   * @param v The element to be added
   * @return The abstract state with the given element as well.
   */
  def add(v : T) : T = {
    if(this.isTop) return this.top();
    var result = factory();
    result.value=value++v.value;
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

/** 
 * The representation of an inverse set domain, that is, a domain that is represented by a set, and whose lattice
 * operators are the inversed one. Formally, the upper bound is the intersection, the lower bound the union, and 
 * so on.
 *
 * @param <V> The type of the values contained in the set
 * @param <T> The type of the current set domain
 * @author Pietro Ferrara
 * @since 0.1
 */
trait InverseSetDomain[V, T <: SetDomain[V, T]] extends SetDomain[V, T] {
  override def factory() : T;

  override def add(el : V) : T = {
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

trait FunctionalDomainWithReplacement[K, V <: LatticeWithReplacement[V], T <: FunctionalDomainWithReplacement[K, V, T]] extends LatticeWithReplacement[T] {
  override def factory() : T;

  var value : Map[K, V]=Map.empty[K, V]
  var isBottom : Boolean = false;

  /**
   * Adds [key->value] to the domain
   * @param key The key
   * @param value The value
   * @return The state of the domain after the assignment
   */
  def add(key : K, value : V) : T = {
    val result : T = this.factory();
    result.value=this.value+((key, value));
    result
  }

  /**
   * Returns the value of key. It is not implemented since in some domains if the domain is not defined on the given
   * key we have a top value, in some others we have a bottom value. So we decided to let to the particular instance
   * of the domain the opportunity to define it in order to have a total function (that is more convenient when
   * defining the other lattice operators).
   *
   * @param key The key
   * @return The value related to the given key
   */
  def get(key : K) : V;

  /**
   * Removes the key from the domain.
   * @param key The key to be removed
   * @return The state of the domain after the kays has been removed
   */
  def remove(key : K) : T = {
    val result : T = this.factory();
    result.value=this.value-(key);
    result
  }

  /**
   * Computes the upper bound between two states. It is defined by:
   * left \sqcup right = [k -> left(k) \sqcup right(k) : k \in dom(left) \cup dom(right)]
   * @param left One of the two operands
   * @param right The other operand
   * @return The upper bound of left and right
   */
  def lubWithReplacement(left : T, right : T) : (T, Replacement) = {
    if(left.equals(this.bottom())) return (right, new Replacement);
    if(right.equals(this.bottom())) return (left, new Replacement);
    if(left.equals(this.top()) || right.equals(this.top())) return (this.top(), new Replacement);
    val (res, replacement)= upperBoundFunctionalLifting(left, right);
    val result = this.factory();
    result.value=res
    (result, replacement)
  }


  /**
   * Computes the lower bound between two states. It is defined by:
   * left \sqcap right = [k -> left(k) \sqcap right(k) : k \in dom(left) \cap dom(right)]
   * @param left One of the two operands
   * @param right The other operand
   * @return The lower bound of left and right
   */
  def glbWithReplacement(left : T, right : T) : (T, Replacement) =  {
    if(left.equals(this.bottom()) || right.equals(this.bottom())) return (this.bottom(), new Replacement);
    if(left.equals(this.top())) return (right, new Replacement);
    if(right.equals(this.top())) return (left, new Replacement);
    var result : Map[K, V]  = Map.empty[K, V]
    var replacement : Replacement = null;
    for( el <- left.value.keySet++right.value.keySet ) {
      val (r, repl) = left.get(el).glbWithReplacement(left.get(el), right.get(el));
      result=result+((el, r));
      if(replacement==null)
        replacement=repl;
      else replacement = replacement.glb(replacement, repl);
    }
    val domain=this.factory()
    domain.value=result
    (domain, replacement)
  }

  /**
   * Computes the widening between two states. It is defined by:
   * left \nable right = [k -> left(k) \nabla right(k) : k \in dom(left) \cup dom(right)]
   * @param left The left operand
   * @param right The right operand
   * @return The upper bound of left and right
   */
  override def wideningWithReplacement(left : T, right : T) : (T, Replacement) =  {
	if(left.isBottom && right.isBottom) return (this.bottom(), new Replacement);
	if(left.isBottom) return (right, new Replacement);
	if(right.isBottom) return (left, new Replacement);
    val (res, replacement)= wideningFunctionalLifting(left, right);
    val result = this.factory();
    result.value=res
    (result, replacement)
  }

  /**
   * Implements the partial ordering between two states of functional domains. It is defined by:
   * this \leq r <==> \forall k \in dom(this) : this(k) \leq r(k)
   * @param r The right operand
   * @return true iff this is less or equal than t
   */
  override def lessEqualWithReplacement(r : T) : (Boolean, Replacement) = {
    //case bottom
    if(this.isBottom) return (true, new Replacement);
    if(r.isBottom) return (false, new Replacement);
    var rep : Replacement = new Replacement();
    for(variable <- this.value.keySet) {
      val (b1, r1) = this.get(variable).lessEqualWithReplacement(r.get(variable));
      rep=rep.lub(rep, r1)
      if(! b1) return (false, rep)
    }
    for(variable <- r.value.keySet) {
      val (b1, r1) = this.get(variable).lessEqualWithReplacement(r.get(variable));
      rep=rep.lub(rep, r1)
      if(! b1) return (false, rep)
    }
    return (true, rep);
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

  private def wideningFunctionalLifting(f1 : T, f2 : T) : (Map[K, V], Replacement) = {
    var result : Map[K, V]  = Map.empty[K, V]
    var replacement : Replacement = new Replacement();
    for( el <- f1.value.keySet ) {
      f2.value.get(el) match {
        case Some(x) =>
          val (r, r2) : (V, Replacement) = x.wideningWithReplacement(f1.value.get(el).get, x);
          result=result+((el, r));
          replacement=r2.lub(r2, replacement);
        case None => result=result+((el, f1.value.get(el).get));
      }
    }
    for( el <- f2.value.keySet ) {
      f1.value.get(el) match {
        case Some(x) =>
        case None => result=result+((el, f2.get(el)))
      }
    }
    (result, replacement)
  }

  private def upperBoundFunctionalLifting(f1 : T, f2 : T) : (Map[K, V], Replacement) = {
    var result : Map[K, V]  = Map.empty[K, V]
    var replacement : Replacement = new Replacement();
    for( el <- f1.value.keySet ) {
      f2.value.get(el) match {
        case Some(x) =>
          val (r, repl) = x.lubWithReplacement(f1.value.get(el).get, x);
          result=result+((el, r))
          replacement=repl.lub(repl, replacement);
        case None => result=result+((el, f1.value.get(el).get));
      }
    }
    for( el <- f2.value.keySet ) {
      f1.value.get(el) match {
        case Some(x) =>
        case None => result=result+((el, f2.get(el)))
      }
    }
    (result, replacement)
  }
}


/** 
 * The representation of a Cartesian product, that is, a lattice domain that combines two other lattices without
 * passing information from one to the other. Each domain could track a different type of information, so their
 * combination could lead to more precise results than each domain separately.
 *
 * @param <T1> The type of the first domain
 * @param <T2> The type of the second domain
 * @param <T> The type of the current domain
 * @author Pietro Ferrara
 * @since 0.1
 */
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
  
  def widening(l : T, r : T) : T = {
    val result : T = this.factory();
    result.d1=d1.widening(l._1, r._1)
    result.d2=d2.widening(l._2, r._2)
    result
  }
  
  def lessEqual(r : T) : Boolean = {
      if(this._1.lessEqual(this._1.bottom()) || this._2.lessEqual(this._2.bottom())) return true;
      if(r._1.lessEqual(r._1.bottom()) || r._2.lessEqual(r._2.bottom())) return false;
    d1.lessEqual(r._1) && d2.lessEqual(r._2)
  }

  override def equals(a : Any) : Boolean = a match {
    case right : T =>
      if(this._1.equals(this._1.bottom()) || this._2.equals(this._2.bottom())) {
        if(right._1.equals(right._1.bottom()) || right._2.equals(right._2.bottom()))
          return true;
        else return false;
      }
	    this._1.equals(right._1) &&
	    this._2.equals(right._2)
    case _ => false
  }
  
  override def toString() = "Domain 1:\n"+this._1.toString+"\nDomain 2:\n"+this._2.toString;
  
}

/** 
 * The representation of a reduced Cartesian product, that is, a Cartesian product that could pass information
 * from one domain to the other.
 *
 * @param <T1> The type of the first domain
 * @param <T2> The type of the second domain
 * @param <T> The type of the current domain
 * @author Pietro Ferrara
 * @since 0.1
 */
abstract class ReducedProductDomain[T1 <: Lattice[T1], T2 <: Lattice[T2], T <: ReducedProductDomain[T1, T2, T]](d1 : T1, d2 : T2) extends CartesianProductDomain[T1, T2, T](d1, d2) {
	
  /**
   * Reduce the information contained in the two domains. The returned value has to be less or equal
   * (that is, more precise) than the initial state.
   * @return The reduced abstract state
   */
  def reduce() : T;
}

/** 
 * The representation of a Cartesian product supporting the operations of the semantic domain. 
 *
 * @param <T1> The type of the first domain
 * @param <T2> The type of the second domain
 * @param <T> The type of the current domain
 * @author Pietro Ferrara
 * @since 0.1
 */
abstract class SemanticCartesianProductDomain[T1 <: SemanticDomain[T1], T2 <: SemanticDomain[T2], T <: SemanticCartesianProductDomain[T1, T2, T]](a1 : T1, a2 : T2) extends CartesianProductDomain[T1, T2, T](a1, a2) with SemanticDomain[T] {

 def getIds = this._1.getIds()++this._2.getIds();

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
 def setArgument(variable : Identifier, expr : Expression) : T= {
    val result : T = this.factory();
    result.d1=d1.setArgument(variable, expr)
    result.d2=d2.setArgument(variable, expr)
    result
  }
 def assume(expr : Expression) : T= {
    val result : T = this.factory();
    result.d1=d1.assume(expr)
    result.d2=d2.assume(expr)
    result
  }

 def merge(r : Replacement) : T= {
    val result : T = this.factory();
    result.d1=d1.merge(r)
    result.d2=d2.merge(r)
    result
  }
 def createVariable(variable : Identifier, typ : Type) : T= {
    val result : T = this.factory();
    result.d1=d1.createVariable(variable, typ)
    result.d2=d2.createVariable(variable, typ)
    result
  }
 def createVariableForArgument(variable : Identifier, typ : Type, path : List[String]) = {
    val result : T = this.factory();
    var (a1, b1)=d1.createVariableForArgument(variable, typ, path)
    var (a2, b2)=d2.createVariableForArgument(variable, typ, path)
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
  def getStringOfId(id : Identifier) : String = "( "+d1.getStringOfId(id)+", "+d2.getStringOfId(id)+")"

  
}

/** 
 * The representation of reduced a Cartesian product supporting the operations of the semantic domain. After each
 * semantic operation the reduction is applied. Note that this implementation is not particularly performant. 
 *
 * @param <T1> The type of the first domain
 * @param <T2> The type of the second domain
 * @param <T> The type of the current domain
 * @author Pietro Ferrara
 * @since 0.1
 */
abstract class ReducedSemanticProductDomain[T1 <: SemanticDomain[T1], T2 <: SemanticDomain[T2], T <: ReducedSemanticProductDomain[T1, T2, T]](d1 : T1, d2 : T2) extends SemanticCartesianProductDomain[T1, T2, T](d1, d2) {
 def reduce() : T;
 override def lub(l : T, r : T) : T = super.lub(l, r).reduce();
 override def glb(l : T, r : T) : T = super.glb(l, r).reduce();
 override def setToTop(variable : Identifier) : T = super.setToTop(variable).reduce();
 override def assign(variable : Identifier, expr : Expression) : T = super.assign(variable, expr).reduce();
 override def setArgument(variable : Identifier, expr : Expression) : T = super.setArgument(variable, expr).reduce();
 override def assume(expr : Expression) : T = super.assume(expr).reduce();
 override def createVariable(variable : Identifier, typ : Type) : T = super.createVariable(variable, typ).reduce();
 override def createVariableForArgument(variable : Identifier, typ : Type, path : List[String]) = {
   val (result, i)=super.createVariableForArgument(variable, typ, path);
   (result.reduce(), i);
 }
 override def removeVariable(variable : Identifier) : T = super.removeVariable(variable).reduce();
 override def access(field : Identifier) : T = super.access(field).reduce();
 override def backwardAccess(field : Identifier) : T = super.backwardAccess(field).reduce();
 override def backwardAssign(variable : Identifier, expr : Expression) : T = super.backwardAssign(variable, expr).reduce();
 override def merge(r : Replacement) : T = super.merge(r).reduce();
  
}


class StandardDomainException(message : String) extends Exception(message)