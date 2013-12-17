//This file contains several implementations of some standard domains. These standard domains are quite useful to develop
//new analyses without rewriting some standard code.

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._

/**
 * The representation of a functional domain, that is, a domain that is represented by a function whose
 * codomain is a lattice. The lattice operators are the functional extensions of the lattice operators of
 * the codomain.
 *
 * @tparam K The type of the keys
 * @tparam V The type of the codomain that has to be a lattice
 * @tparam T The type of the current functional domain
 * @author Pietro Ferrara, Lucas Brutschy
 * @since 0.1
 */
abstract class FunctionalDomain[K, V <: Lattice[V], T <: FunctionalDomain[K, V, T]]
  (val value:Map[K, V] = Map.empty[K, V],val isBottom:Boolean = false,val isTop:Boolean = false)
  extends Lattice[T] {

  override def factory():T = functionalFactory()

  /**
   * Creates a new instance of the functional domain with the given contents
   *
   * @param value The map of values, empty if bottom or top
   * @param isBottom Domain is bottom
   * @param isTop Domain is top for all keys
   * @return A fresh instance
   */
  def functionalFactory(value:Map[K, V] = Map.empty[K, V],isBottom:Boolean = false,isTop:Boolean = false):T

  /**
   * Adds [key->value] to the domain 
   * @param key The key
   * @param v The value
   * @return The state of the domain after the assignment
   */
  def add(key: K, v: V): T = functionalFactory(value + ((key, v)))

  /**
   * Returns the value of key. It is not implemented since in some domains if the domain is not defined on the given
   * key we have a top value, in some others we have a bottom value. So we decided to let to the particular instance
   * of the domain the opportunity to define it in order to have a total function (that is more convenient when
   * defining the other lattice operators).
   *
   * @param key The key
   * @return The value related to the given key
   */
  def get(key: K): V

  /**
   * Removes the key from the domain. 
   * @param key The key to be removed
   * @return The state of the domain after the kays has been removed
   */
  def remove(key: K): T = functionalFactory(value - key)

  /**
   * Computes the upper bound between two states. It is defined by:
   * left \sqcup right = [k -> left(k) \sqcup right(k) : k \in dom(left) \cup dom(right)]   
   * @param left One of the two operands
   * @param right The other operand
   * @return The upper bound of left and right
   */
  def lub(left: T, right: T): T = {
    if (left.isBottom) return right
    if (right.isBottom) return left
    if (left.isTop) return left
    if (right.isTop) return right
    functionalFactory(upperBoundFunctionalLifting(left, right))
  }


  /**
   * Computes the lower bound between two states. It is defined by:
   * left \sqcap right = [k -> left(k) \sqcap right(k) : k \in dom(left) \cap dom(right)]   
   * @param left One of the two operands
   * @param right The other operand
   * @return The lower bound of left and right
   */
  def glb(left: T, right: T): T = {
    if (left.isBottom) return left
    if (right.isBottom) return right
    if (left.isTop) return right
    if (right.isTop) return left
    this.functionalFactory(lowerBoundFunctionalLifting(left,right))
  }

  /**
   * Computes the widening between two states. It is defined by:
   * left \nable right = [k -> left(k) \nabla right(k) : k \in dom(left) \cup dom(right)]   
   * @param left The left operand
   * @param right The right operand
   * @return The upper bound of left and right
   */
  override def widening(left: T, right: T): T = {
    if (left.isBottom) return right
    if (right.isBottom) return left
    if (left.isTop) return left
    if (right.isTop) return right
    functionalFactory(wideningFunctionalLifting(left, right))
  }

  /**
   * Implements the partial ordering between two states of functional domains. It is defined by: 
   * this \leq r <==> \forall k \in dom(this) : this(k) \leq r(k)   
   * @param r The right operand
   * @return true iff this is less or equal than t
   */
  override def lessEqual(r: T): Boolean = {

    // Case we are bottom
    if (this.isBottom || r.isTop) return true
    if (r.isBottom || this.isTop) return false

    for (variable <- this.value.keySet)
      if (!this.get(variable).lessEqual(r.get(variable)))
        return false
    for (variable <- r.value.keySet)
      if (!this.get(variable).lessEqual(r.get(variable)))
        return false

    true
  }

  override def equals(a: Any): Boolean = a match {
    case right: T =>
      if (this.isBottom && right.isBottom) return true
      if (this.isBottom || right.isBottom) return false
      if (this.isTop && right.isTop) return true
      if (this.isTop || right.isTop) return false
      if (this.value.keySet.equals(right.value.keySet)) {
        for (variable <- this.value.keySet)
          if (!this.value.get(variable).get.equals(right.value.get(variable).get))
            return false
        true
      }
      else false
    case _ => false
  }

  override def toString: String = {
    if (isBottom) "_|_"
    else if (isTop) "T"
    else ToStringUtilities.mapToString(value)
  }

  def top(): T = functionalFactory(isTop = true)

  def bottom(): T = functionalFactory(isBottom = true)

  private def lowerBoundFunctionalLifting(f1: T, f2: T): Map[K, V] = {
    var result: Map[K, V] = Map.empty[K, V]
    for (el <- f1.value.keySet ++ f2.value.keySet) {
      result = result + ((el, f1.get(el).glb(f1.get(el), f2.get(el))))
    }
    result
  }

  private def wideningFunctionalLifting(f1: T, f2: T): Map[K, V] = {
    var result: Map[K, V] = Map.empty[K, V]
    for (el <- f1.value.keySet) {
      f2.value.get(el) match {
        case Some(x) => result = result + ((el, x.widening(f1.value.get(el).get, x)))
        case None => result = result + ((el, f1.value.get(el).get))
      }
    }
    for (el <- f2.value.keySet) {
      f1.value.get(el) match {
        case Some(x) =>
        case None => result = result + ((el, f2.get(el)))
      }
    }
    result
  }

  private def upperBoundFunctionalLifting(f1: T, f2: T): Map[K, V] = {
    var result: Map[K, V] = Map.empty[K, V]
    for (el <- f1.value.keySet) {
      f2.value.get(el) match {
        case Some(x) => result = result + ((el, x.lub(f1.value.get(el).get, x)))
        case None => result = result + ((el, f1.value.get(el).get))
      }
    }
    for (el <- f2.value.keySet) {
      f1.value.get(el) match {
        case Some(x) =>
        case None => result = result + ((el, f2.get(el)))
      }
    }
    result
  }
}

/**
 * The representation of a boxed domain, that is, a domain that is a functional domain whose keys are (variable
 * or heap) identifiers
 *
 * @tparam V The type of the codomain that has to be a lattice
 * @tparam T The type of the current boxed domain
 * @author Pietro Ferrara, Lucas Brutschy
 * @since 0.1
 */
abstract class BoxedDomain[V <: Lattice[V], T <: BoxedDomain[V, T]]
  (_value:Map[Identifier, V] = Map.empty[Identifier, V],_isBottom:Boolean = false,_isTop:Boolean = false)
  extends FunctionalDomain[Identifier, V, T](_value,_isBottom,_isTop) {

  def merge(r: Replacement): T = {

    if (r.isEmpty()) return this.asInstanceOf[T]
    var result: T = this.asInstanceOf[T]
    val removedVariables = r.keySet().flatten

    // We remove the variables from the result state
    for (v <- removedVariables)
      result = result.remove(v)

    for (s <- r.keySet()) {
      var value: V = this.get(s.head).bottom()

      // We compute the value that should be assigned to all other ids
      for (v <- s) value = value.lub(value, this.get(v))

      // We assign the value to all other ids
      for (v <- r.apply(s)) result = result.merge(v, value)
    }

    result
  }

  private def merge(id: Identifier, v: V): T = {
    if (this.value.keySet.contains(id))
      this.add(id, v.lub(v, this.get(id)))
    else this.add(id, v)
  }

  def getStringOfId(id: Identifier): String = this.get(id).toString

  def getIds = value.keySet

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

  var value: Set[V] = Set.empty[V]
  var isTop = false
  var isBottom = false

  final def top(): T = {
    val result: T = this.factory()
    result.isBottom = false
    result.isTop = true
    result
  }

  def bottom(): T = {
    val result: T = this.factory()
    result.isTop = false
    result.isBottom = true
    result
  }

  /**
   * Removes an element from the set. Formally, return = old(this)\setminus {v}
   * @param v The element to be removed
   * @return The abstract state without the given element.
   */
  def remove(v: V): T = {
    if (this.isTop) return this.top()
    if (this.isBottom) return this.bottom()
    val newSet = this.value.-(v)
    val result = this.factory()
    result.value = newSet
    result.isBottom = (result.value.isEmpty)
    result
  }

  /**
   * Removes a set from the set. Formally, return = old(this) \setminus v
   * @param v The element to be removed
   * @return The abstract state without the given set of elements.
   */
  def remove(v: SetDomain[V, T]): T = {
    if (this.isBottom) return bottom()
    if (v.isTop) return bottom()
    if (this.isTop) return top()
    val newSet = this.value -- v.value
    val result = this.factory()
    result.value = newSet
    result.isBottom = (result.value.isEmpty)
    result
  }

  /**
   * Adds an element to the set. Formally, return = old(this) \cup {v}
   * @param v The element to be added
   * @return The abstract state with the given element as well.
   */
  def add(v: V): T = {
    if (this.isTop) return this.top()
    var result = factory()
    result.value = value + v
    result
  }

  /**
   * Adds an element to the set. Formally, return = old(this) \cup V
   * @param v The element to be added
   * @return The abstract state with the given element as well.
   */
  def add(v: T): T = {
    if (this.isTop || v.isTop) return top()
    if (this.isBottom) return v
    val result: T = this.factory()
    result.value = this.value ++ v.value
    result
  }

  def lub(left: T, right: T): T = {
    if (left.isTop || right.isTop) return top()
    if (left.isBottom) return right
    if (right.isBottom) return left
    val result: T = this.factory()
    result.value = left.value ++ right.value
    result
  }

  def glb(left: T, right: T): T = {
    if (left.isBottom || right.isBottom) return bottom()
    if (left.isTop) return right
    if (right.isTop) return left
    val result: T = this.factory()
    result.value = left.value.intersect(right.value)
    result.isBottom = (result.value.isEmpty)
    result
  }

  def widening(left: T, right: T): T = this.lub(left, right)

  def lessEqual(right: T): Boolean = {
    if (this.isBottom) return true
    if (right.isTop) return true
    if (right.isBottom) return false
    if (this.isTop) return false
    this.value.subsetOf(right.value)
  }

  override def toString(): String = {
    if (this.isBottom) return "_|_"
    if (this.isTop) return "T"
    ToStringUtilities.setToString(value)
  }

  override def equals(a: Any): Boolean = a match {
    case x: SetDomain[V, T] =>
      if (this.isBottom && x.isBottom) return true
      if (this.isTop && x.isTop) return true
      if (this.isBottom || x.isBottom || this.isTop || x.isTop) return false
      if (this.value.size != x.value.size) return false
      for (el <- this.value)
        if (!x.value.contains(el)) return false
      true
    case _ => false
  }

  override def hashCode(): Int = {
    var result: Int = 0
    for (el <- this.value)
      result = result + el.hashCode()
    result
  }

}

/**
 *
 * Implements a set domain which is bounded by a given K
 *
 * @tparam V the values stored
 * @tparam T the type itself
 */
trait KSetDomain[V, T <: KSetDomain[V, T]] extends SetDomain[V,T] {

  /**
   * Overwrite this method to set K
   * @return the maximum number of represented elements
   */
  def getK:Int

  /**
   * Adds an element to the set. Formally, return = old(this) \cup {v}
   * Returns top if the cardinality of the result is > k
   * @param v The element to be added
   * @return The abstract state with the given element as well.
   */
  override def add(v: V): T = {
    if (this.isTop) return this.top()
    val result = factory()
    result.value = value + v
    if (result.value.size > getK) return this.top()
    result
  }

  /**
   * Adds an element to the set. Formally, return = old(this) \cup {v}
   * Returns top if the cardinality of the result is > k
   * @param v The set to be added
   * @return The abstract state with the given element as well.
   */
  override def add(v: T): T = {
    if (this.isTop || v.isTop) return top()
    if (this.isBottom) return v
    val result: T = this.factory()
    result.value = this.value ++ v.value
    if (result.value.size > getK) return this.top()
    result
  }

  override def lub(left: T, right: T): T = {
    if (left.isBottom) return right
    if (right.isBottom) return left
    if (left.isTop || right.isTop) return top()
    val result: T = this.factory()
    result.value = left.value ++ right.value
    if (result.value.size > getK) return this.top()
    result
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
  override def factory(): T;

  override def add(el: V): T = {
    var result = factory();
    result.isTop = false;
    result.isBottom = false;
    result.value = value + el;
    result;
  }

  override def lub(left: T, right: T): T = {
    if (left.isTop || right.isTop) return top();
    if (left.isBottom) return right;
    if (right.isBottom) return left;
    val result: T = this.factory()
    result.value = left.value.intersect(right.value);
    result
  }

  override def glb(left: T, right: T): T = {
    if (left.isBottom || right.isBottom) return bottom();
    if (left.isTop) return right;
    if (right.isTop) return left;
    val result: T = this.factory()
    result.value = left.value ++ right.value;
    result
  }

  override def widening(left: T, right: T): T = this.lub(left, right)

  override def lessEqual(right: T): Boolean = {
    if (this.isBottom) return true;
    if (right.isTop) return true;
    right.value.subsetOf(this.value)
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
abstract class CartesianProductDomain[T1 <: Lattice[T1], T2 <: Lattice[T2], T <: CartesianProductDomain[T1, T2, T]]
    (d1: T1, d2: T2) extends Lattice[T] {

  def _1: T1 = d1

  def _2: T2 = d2

  def factory(a :T1, b:T2):T

  def set_1(a:T1) = factory(a,_2)
  def set_2(b:T2) = factory(_1,b)

  override def factory(): T = factory(_1.factory(),_2.factory())

  def top(): T = factory(_1.top(),_2.top())

  def bottom(): T = factory(_1.bottom(),_2.bottom())

  def lub(l: T, r: T): T = factory(_1.lub(l._1, r._1),_2.lub(l._2, r._2))

  def glb(l: T, r: T): T = factory(_1.glb(l._1, r._1),_2.glb(l._2, r._2))

  def widening(l: T, r: T): T = factory(_1.widening(l._1, r._1),_2.widening(l._2, r._2))

  def lessEqual(r: T): Boolean = {
    if (this._1.lessEqual(this._1.bottom()) || this._2.lessEqual(this._2.bottom())) return true
    if (r._1.lessEqual(r._1.bottom()) || r._2.lessEqual(r._2.bottom())) return false
    d1.lessEqual(r._1) && d2.lessEqual(r._2)
  }

  override def equals(a: Any): Boolean = a match {
    case right: T =>
      if (this._1.equals(this._1.bottom()) || this._2.equals(this._2.bottom())) {
        if (right._1.equals(right._1.bottom()) || right._2.equals(right._2.bottom()))
          return true
        else return false
      }
      this._1.equals(right._1) && this._2.equals(right._2)
    case _ => false
  }

  override def toString =
    "Cartesian,Left:\n" + ToStringUtilities.indent(_1.toString) +
      "\nCartesian,Right:\n" + ToStringUtilities.indent(_2.toString)

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
abstract class ReducedProductDomain[T1 <: Lattice[T1], T2 <: Lattice[T2], T <: ReducedProductDomain[T1, T2, T]](d1: T1, d2: T2) extends CartesianProductDomain[T1, T2, T](d1, d2) {

  /**
   * Reduce the information contained in the two domains. The returned value has to be less or equal
   * (that is, more precise) than the initial state.
   * @return The reduced abstract state
   */
  def reduce(): T

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
abstract class SemanticCartesianProductDomain[T1 <: SemanticDomain[T1], T2 <: SemanticDomain[T2], T <: SemanticCartesianProductDomain[T1, T2, T]](a1: T1, a2: T2) extends CartesianProductDomain[T1, T2, T](a1, a2) with SemanticDomain[T] {

  def getIds() = _1.getIds() ++ _2.getIds()

  def setToTop(variable: Identifier): T = factory(_1.setToTop(variable),_2.setToTop(variable))

  def assign(variable: Identifier, expr: Expression): T = factory(_1.assign(variable, expr),_2.assign(variable, expr))

  def setArgument(variable: Identifier, expr: Expression): T = factory(_1.setArgument(variable, expr),_2.setArgument(variable, expr))

  def assume(expr: Expression): T = factory(_1.assume(expr),_2.assume(expr))

  def merge(r: Replacement): T = factory(_1.merge(r),_2.merge(r))

  def createVariable(variable: Identifier, typ: Type): T = factory(_1.createVariable(variable, typ),_2.createVariable(variable, typ))

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    val (a1, b1) = _1.createVariableForArgument(variable, typ, path)
    val (a2, b2) = _2.createVariableForArgument(variable, typ, path)
    (factory(a1,a2),b1 ++ b2)
  }

  def removeVariable(variable: Identifier): T = factory(_1.removeVariable(variable),_2.removeVariable(variable))

  def access(field: Identifier): T = factory(_1.access(field),_2.access(field))

  def backwardAccess(field: Identifier): T = factory(_1.backwardAccess(field),_2.backwardAccess(field))

  def backwardAssign(variable: Identifier, expr: Expression): T = factory(_1.backwardAssign(variable,expr),_2.backwardAssign(variable,expr))

  def getStringOfId(id: Identifier): String = "( " + _1.getStringOfId(id) + ", " + _2.getStringOfId(id) + ")"

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
abstract class ReducedSemanticProductDomain[T1 <: SemanticDomain[T1], T2 <: SemanticDomain[T2], T <: ReducedSemanticProductDomain[T1, T2, T]](d1: T1, d2: T2) extends SemanticCartesianProductDomain[T1, T2, T](d1, d2) {
  def reduce(): T;

  override def lub(l: T, r: T): T = super.lub(l, r).reduce();

  override def glb(l: T, r: T): T = super.glb(l, r).reduce();

  override def setToTop(variable: Identifier): T = super.setToTop(variable).reduce();

  override def assign(variable: Identifier, expr: Expression): T = super.assign(variable, expr).reduce();

  override def setArgument(variable: Identifier, expr: Expression): T = super.setArgument(variable, expr).reduce();

  override def assume(expr: Expression): T = super.assume(expr).reduce();

  override def createVariable(variable: Identifier, typ: Type): T = super.createVariable(variable, typ).reduce();

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    val (result, i) = super.createVariableForArgument(variable, typ, path);
    (result.reduce(), i);
  }

  override def removeVariable(variable: Identifier): T = super.removeVariable(variable).reduce();

  override def access(field: Identifier): T = super.access(field).reduce();

  override def backwardAccess(field: Identifier): T = super.backwardAccess(field).reduce();

  override def backwardAssign(variable: Identifier, expr: Expression): T = super.backwardAssign(variable, expr).reduce();

  override def merge(r: Replacement): T = super.merge(r).reduce();

}


class StandardDomainException(message: String) extends Exception(message)