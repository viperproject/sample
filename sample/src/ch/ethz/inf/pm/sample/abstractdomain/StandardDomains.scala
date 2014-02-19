//This file contains several implementations of some standard domains. These standard domains are quite useful to develop
//new analyses without rewriting some standard code.

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.oorepresentation._

/**
 * Domain that is represented by a function whose codomain is a lattice.
 * The lattice operators are the functional extensions of the lattice operators of
 * the codomain.
 *
 * @tparam K The type of the keys
 * @tparam V The type of the codomain that has to be a lattice
 * @tparam T The type of the current functional domain
 * @author Pietro Ferrara, Lucas Brutschy
 */
trait FunctionalDomain[K, V <: Lattice[V], T <: FunctionalDomain[K, V, T]]
  extends Lattice[T] { this: T =>

  def isBottom: Boolean

  def isTop: Boolean

  def map: Map[K, V]

  override def factory(): T = functionalFactory()

  /**
   * Creates a new instance of the functional domain with the given contents.
   *
   * @param map The map of values, empty if bottom or top
   * @param isBottom Domain is bottom
   * @param isTop Domain is top for all keys
   * @return A fresh instance
   */
  def functionalFactory(
      map: Map[K, V] = Map.empty[K, V],
      isBottom: Boolean = false,
      isTop: Boolean = false): T

  /**
   * Adds [key->value] to the domain 
   * @param key The key
   * @param value The value
   * @return The state of the domain after the assignment
   */
  def add(key: K, value: V): T = functionalFactory(map + ((key, value)))

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
   * @return The state of the domain after the key has been removed
   */
  def remove(key: K): T = functionalFactory(map - key)

  /**
   * Computes the upper bound between two states. It is defined by:
   * this \sqcup other = [k -> this(k) \sqcup other(k) : k \in dom(this) \cup dom(other)]
   * @param other The other operand
   * @return The upper bound of this and other
   */
  def lub(other: T): T = {
    if (isBottom) return other
    if (other.isBottom) return this
    if (isTop) return this
    if (other.isTop) return other
    lift(other, _ ++ _, _ lub _)
  }


  /**
   * Computes the lower bound between two states. It is defined by:
   * this \sqcap other = [k -> this(k) \sqcap other(k) : k \in dom(this) \cap dom(other)]
   * @param other The other operand
   * @return The lower bound of this and other
   */
  def glb(other: T): T = {
    if (isBottom) return this
    if (other.isBottom) return other
    if (isTop) return other
    if (other.isTop) return this
    lift(other, _ ++ _, _ glb _)
  }

  /**
   * Computes the widening between two states. It is defined by:
   * this \nable other = [k -> this(k) \nabla other(k) : k \in dom(this) \cup dom(other)]
   * @param other The other operand
   * @return The upper bound of this and other
   */
  override def widening(other: T): T = {
    if (isBottom) return other
    if (other.isBottom) return this
    if (isTop) return this
    if (other.isTop) return other
    lift(other, _ ++ _, _ widening _)
  }

  /**
   * Implements the partial ordering between two states of functional domains. It is defined by: 
   * this \leq r <==> \forall k \in dom(this) : this(k) \leq r(k)   
   * @param r The other operand
   * @return true iff this is less or equal than t
   */
  override def lessEqual(r: T): Boolean = {
    // Case we are bottom
    if (isBottom || r.isTop) return true
    if (r.isBottom || isTop) return false

    for (variable <- map.keySet)
      if (!get(variable).lessEqual(r.get(variable)))
        return false
    for (variable <- r.map.keySet)
      if (!get(variable).lessEqual(r.get(variable)))
        return false

    true
  }

  override def equals(a: Any): Boolean = a match {
    case right: T =>
      if (isBottom && right.isBottom) return true
      if (isBottom || right.isBottom) return false
      if (isTop && right.isTop) return true
      if (isTop || right.isTop) return false
      if (map.keySet.equals(right.map.keySet)) {
        for (variable <- map.keySet)
          if (!map.get(variable).get.equals(right.map.get(variable).get))
            return false
        true
      }
      else false
    case _ => false
  }

  override def toString: String = {
    if (isBottom) "⊥"
    else if (isTop) "⊤"
    else ToStringUtilities.mapToString(map)
  }

  def top(): T = functionalFactory(isTop = true)

  def bottom(): T = functionalFactory(isBottom = true)

  private def lift(other: T, keySetFunc: (Set[K], Set[K]) => Set[K], valueFunc: (V, V) => V): T = {
    val newMap = keySetFunc(map.keySet, other.map.keySet).map(k =>
      k -> valueFunc(get(k), other.get(k))).toMap
    functionalFactory(newMap)
  }
}

object FunctionalDomain {
  /** Simple implementation of `FunctionalDomain`. Cannot be extended.
    *
    * @param defaultValue value returned by `get` for unknown keys (usually
    *                     the top or bottom element of the value domain)
    */
  final case class Default[K, V <: Lattice[V]](
      map: Map[K, V] = Map.empty[K, V],
      isTop: Boolean = false,
      isBottom: Boolean = false,
      defaultValue: V)
    extends FunctionalDomain[K, V, Default[K, V]] {

    def get(key: K): V = map.getOrElse(key, defaultValue)

    def functionalFactory(value: Map[K, V], isBottom: Boolean, isTop: Boolean) =
      Default(value, isTop, isBottom, defaultValue)
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
trait BoxedDomain[V <: Lattice[V], T <: BoxedDomain[V, T]]
  extends FunctionalDomain[Identifier, V, T] { this: T =>

  def merge(r: Replacement): T = {
    if (r.isEmpty()) return this
    var result: T = this
    val removedVariables = r.keySet().flatten

    // We remove the variables from the result state
    for (v <- removedVariables)
      result = result.remove(v)

    for (s <- r.keySet()) {
      var value: V = this.get(s.head).bottom()

      // We compute the value that should be assigned to all other ids
      for (v <- s) value = value.lub(this.get(v))

      // We assign the value to all other ids
      for (v <- r.apply(s)) result = result.merge(v, value)
    }

    result
  }

  private def merge(id: Identifier, v: V): T = {
    if (this.map.keySet.contains(id))
      this.add(id, v.lub(this.get(id)))
    else this.add(id, v)
  }

  def getStringOfId(id: Identifier): String = this.get(id).toString

  def getIds = map.keySet

}

/**
 * A domain that is represented by a set.
 *
 * The lattice operators are the common ones of sets, that is, the upper bound
 * is the union, the lower bound the intersection, and so on.
 *
 * @tparam V The type of the values contained in the set
 * @tparam T The type of the current set domain
 * @author Pietro Ferrara, Lucas Brutschy
 *
 * @todo do not allow `value` to be empty while both `isTop` and `isBottom` are `false`
 * @todo ideally, one should not allow access to `value` when the object is top
 */
trait SetDomain[V, T <: SetDomain[V, T]] extends Lattice[T] { this: T =>
  def value: Set[V]

  def isTop: Boolean

  def isBottom: Boolean

  /**
   * Constructs a new set domain of the concrete type
   *
   * @return a fresh, empty instance of the set domain
   */
  def setFactory(
                  value: Set[V] = Set.empty[V],
                  isTop: Boolean = false,
                  isBottom: Boolean = false): T

  override def factory() = setFactory()

  final def top(): T = setFactory(isTop = true)

  final def bottom(): T = setFactory(isBottom = true)

  /**
   * Removes an element from the set. Formally, return = old(this)\setminus {v}
   * @param v The element to be removed
   * @return The abstract state without the given element.
   */
  def remove(v: V): T = {
    if (this.isTop) return this.top()
    if (this.isBottom) return this.bottom()
    val newSet = this.value.-(v)
    setFactory(newSet, isBottom = newSet.isEmpty)
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
    setFactory(newSet, isBottom = newSet.isEmpty)
  }

  /**
   * Adds an element to the set. Formally, return = old(this) \cup {v}
   * @param v The element to be added
   * @return The abstract state with the given element as well.
   */
  def add(v: V): T = {
    if (this.isTop) return this.top()
    setFactory(value + v)
  }

  /**
   * Adds an element to the set. Formally, return = old(this) \cup V
   * @param v The element to be added
   * @return The abstract state with the given element as well.
   */
  def add(v: T): T = {
    if (this.isTop || v.isTop) return top()
    if (this.isBottom) return v
    setFactory(this.value ++ v.value)
  }

  def lub(other: T): T = {
    if (this.isTop || other.isTop) return top()
    if (this.isBottom) return other
    if (other.isBottom) return this
    setFactory(this.value ++ other.value)
  }

  def glb(other: T): T = {
    if (this.isBottom || other.isBottom) return bottom()
    if (this.isTop) return other
    if (other.isTop) return this
    val newSet = this.value.intersect(other.value)
    setFactory(newSet, isBottom = newSet.isEmpty)
  }

  def widening(other: T): T = this.lub(other)

  def lessEqual(other: T): Boolean = {
    if (this.isBottom) return true
    if (other.isTop) return true
    if (other.isBottom) return false
    if (this.isTop) return false
    this.value.subsetOf(other.value)
  }

  override def toString: String = {
    if (this.isBottom) return "⊥"
    if (this.isTop) return "⊤"
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

object SetDomain {
  /** Simple implementation of `SetDomain`. Cannot be extended. */
  final case class Default[V](
      value: Set[V] = Set.empty[V],
      isTop: Boolean = false,
      isBottom: Boolean = false)
    extends SetDomain[V, Default[V]] {

    def setFactory(value: Set[V], isTop: Boolean, isBottom: Boolean) =
      Default(value, isTop, isBottom)
  }
}

/**
 * A set domain which is bounded by a given K.
 *
 * @tparam V the values stored
 * @tparam T the type itself
 * @author Lucas Brutschy
 */
trait KSetDomain[V, T <: KSetDomain[V, T]] extends SetDomain[V, T] {
  this: T =>

  /** The maximum number of represented elements. */
  def K: Int

  /**
   * Adds an element to the set. Formally, return = old(this) \cup {v}
   * Returns top if the cardinality of the result is > k
   * @param v The element to be added
   * @return The abstract state with the given element as well.
   */
  override def add(v: V): T = {
    if (this.isTop) return this.top()
    val result = setFactory(value + v)
    if (result.value.size > K) return this.top()
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
    val result: T = setFactory(this.value ++ v.value)
    if (result.value.size > K) return this.top()
    result
  }

  override def lub(other: T): T = {
    if (this.isBottom) return other
    if (other.isBottom) return this
    if (this.isTop || other.isTop) return top()
    val result: T = setFactory(this.value ++ other.value)
    if (result.value.size > K) return this.top()
    result
  }
}

object KSetDomain {
  /** Simple implementation of `KSetDomain`. Cannot be extended. */
  case class Default[V](
      K: Int,
      value: Set[V] = Set.empty[V],
      isTop: Boolean = false,
      isBottom: Boolean = false)
    extends KSetDomain[V, Default[V]] {

    def setFactory(value: Set[V], isTop: Boolean, isBottom: Boolean) =
      Default(K, value, isTop, isBottom)
  }
}

/**
 * A domain that is represented by a set, and whose lattice operators are
 * the inversed one. Formally, the upper bound is the intersection,
 * the lower bound the union, and so on.
 *
 * @tparam V The type of the values contained in the set
 * @tparam T The type of the current set domain
 * @author Pietro Ferrara, Lucas Brutschy
 */
trait InverseSetDomain[V, T <: SetDomain[V, T]] extends SetDomain[V, T] { this: T =>

  override def add(el: V): T = {
    setFactory(value + el)
  }

  override def lub(other: T): T = {
    if (this.isTop || other.isTop) return top()
    if (this.isBottom) return other
    if (other.isBottom) return this
    setFactory(this.value.intersect(other.value))
  }

  override def glb(other: T): T = {
    if (this.isBottom || other.isBottom) return bottom()
    if (this.isTop) return other
    if (other.isTop) return this
    setFactory(this.value ++ other.value)
  }

  override def widening(other: T): T = this.lub(other)

  override def lessEqual(other: T): Boolean = {
    if (this.isBottom) return true
    if (other.isTop) return true
    if (other.isBottom) return false
    if (this.isTop) return false
    other.value.subsetOf(this.value)
  }
}

object InverseSetDomain {
  /** Simple implementation of `InverseSetDomain`. Cannot be extended. */
  final case class Default[V](
      value: Set[V] = Set.empty[V],
      isTop: Boolean = false,
      isBottom: Boolean = false)
    extends InverseSetDomain[V, Default[V]] {

    def setFactory(value: Set[V], isTop: Boolean, isBottom: Boolean) =
      Default(value, isTop, isBottom)
  }

  /** Simple implementation of a must-`InverseSetDomain`. Cannot be extended. */
  final case class Must[V](
      value: Set[V] = Set.empty[V],
      isTop: Boolean = false,
      isBottom: Boolean = false)
    extends InverseSetDomain[V, Must[V]] with Lattice.Must[Must[V]] {

    def setFactory(value: Set[V], isTop: Boolean, isBottom: Boolean) =
      Must(value, isTop, isBottom)
  }
}

/**
 * A lattice domain that combines two other lattices without
 * passing information from one to the other. Each domain could track
 * a different type of information, so their combination could lead
 * to more precise results than each domain separately.
 *
 * @tparam T1 The type of the first domain
 * @tparam T2 The type of the second domain
 * @tparam T The type of the current domain
 * @author Pietro Ferrara, Lucas Brutschy
 */
trait CartesianProductDomain[
    T1 <: Lattice[T1],
    T2 <: Lattice[T2],
    T <: CartesianProductDomain[T1, T2, T]]
  extends Lattice[T] with Product2[T1, T2] { this: T =>

  def factory(a: T1, b: T2): T

  def set_1(a: T1) = factory(a, _2)

  def set_2(b: T2) = factory(_1, b)

  override def factory(): T = factory(_1.factory(), _2.factory())

  def top(): T = factory(_1.top(), _2.top())

  def bottom(): T = factory(_1.bottom(), _2.bottom())

  def lub(other: T): T = factory(_1.lub(other._1), _2.lub(other._2))

  def glb(other: T): T = factory(_1.glb(other._1), _2.glb(other._2))

  def widening(other: T): T = factory(_1.widening(other._1), _2.widening(other._2))

  def lessEqual(other: T): Boolean = {
    if (_1.lessEqual(_1.bottom()) || _2.lessEqual(_2.bottom())) return true
    if (other._1.lessEqual(other._1.bottom()) || other._2.lessEqual(other._2.bottom())) return false
    _1.lessEqual(other._1) && _2.lessEqual(other._2)
  }

  override def equals(a: Any): Boolean = a match {
    case other: T =>
      if (_1.equals(_1.bottom()) || _2.equals(_2.bottom())) {
        if (other._1.equals(other._1.bottom()) || other._2.equals(other._2.bottom()))
          return true
        else return false
      }
      _1.equals(other._1) && _2.equals(other._2)
    case _ => false
  }

  override def toString =
    "Cartesian,Left:\n" + ToStringUtilities.indent(_1.toString) +
      "\nCartesian,other:\n" + ToStringUtilities.indent(_2.toString)

}

object CartesianProductDomain {
  /** Simple implementation of `CartesianProductDomain`. Cannot be extended. */
  final case class Default[T1 <: Lattice[T1], T2 <: Lattice[T2]](_1: T1, _2: T2)
    extends CartesianProductDomain[T1, T2, Default[T1, T2]] {
    def factory(_1: T1, _2: T2): Default[T1, T2] = Default(_1, _2)
  }
}

/**
 * Cartesian product that could pass information from one domain to the other.
 *
 * @tparam T1 The type of the first domain
 * @tparam T2 The type of the second domain
 * @tparam T The type of the current domain
 * @author Pietro Ferrara, Lucas Brutschy
 */
trait ReducedProductDomain[
    T1 <: Lattice[T1],
    T2 <: Lattice[T2],
    T <: ReducedProductDomain[T1, T2, T]]
  extends CartesianProductDomain[T1, T2, T] { this: T =>

  /**
   * Reduce the information contained in the two domains. The returned value
   * has to be less or equal (that is, more precise) than the initial state.
   * @return The reduced abstract state
   */
  def reduce(): T
}

/**
 * Cartesian product supporting the operations of the semantic domain.
 *
 * @tparam T1 The type of the first domain
 * @tparam T2 The type of the second domain
 * @tparam T The type of the current domain
 * @author Pietro Ferrara
 */
trait SemanticCartesianProductDomain[
    T1 <: SemanticDomain[T1],
    T2 <: SemanticDomain[T2],
    T <: SemanticCartesianProductDomain[T1, T2, T]]
  extends CartesianProductDomain[T1, T2, T] with SemanticDomain[T] { this: T =>

  def getIds() = _1.getIds() ++ _2.getIds()

  def setToTop(variable: Identifier): T =
    factory(_1.setToTop(variable), _2.setToTop(variable))

  def assign(variable: Identifier, expr: Expression): T =
    factory(_1.assign(variable, expr), _2.assign(variable, expr))

  def setArgument(variable: Identifier, expr: Expression): T =
    factory(_1.setArgument(variable, expr), _2.setArgument(variable, expr))

  def assume(expr: Expression): T =
    factory(_1.assume(expr), _2.assume(expr))

  def merge(r: Replacement): T =
    factory(_1.merge(r), _2.merge(r))

  def createVariable(variable: Identifier, typ: Type): T =
    factory(_1.createVariable(variable, typ), _2.createVariable(variable, typ))

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    val (a1, b1) = _1.createVariableForArgument(variable, typ, path)
    val (a2, b2) = _2.createVariableForArgument(variable, typ, path)
    (factory(a1, a2), b1 ++ b2)
  }

  def removeVariable(variable: Identifier): T =
    factory(_1.removeVariable(variable), _2.removeVariable(variable))

  def access(field: Identifier): T =
    factory(_1.access(field), _2.access(field))

  def backwardAccess(field: Identifier): T =
    factory(_1.backwardAccess(field), _2.backwardAccess(field))

  def backwardAssign(oldPreState: T, variable: Identifier, expr: Expression): T =
    factory(_1.backwardAssign(oldPreState._1, variable, expr), _2.backwardAssign(oldPreState._2, variable, expr))

  def getStringOfId(id: Identifier): String =
    "( " + _1.getStringOfId(id) + ", " + _2.getStringOfId(id) + ")"
}

/**
 * Cartesian product of a `SemanticDomain` and an arbitrary other domain.
 * This trait implements all `SemanticDomain`-specific methods, such that
 * they forward calls to the wrapped `SemanticDomain` object,
 * while the other domain object remains unchanged.
 *
 * @tparam S type of the wrapped semantic domain
 * @tparam O type of the other domain
 * @tparam T type of the current domain
 */
trait HalfSemanticCartesianProductDomain[
    S <: SemanticDomain[S],
    O <: Lattice[O],
    T <: HalfSemanticCartesianProductDomain[S, O, T]]
  extends CartesianProductDomain[S, O, T]
  with SemanticDomain[T] { this: T =>

  def getIds() = _1.getIds()

  def copy(_1: S = _1, _2: O = _2): T =
    factory(_1, _2)

  def backwardAssign(oldPreState: T, variable : Identifier, expr : Expression) =
    copy(_1.backwardAssign(oldPreState._1, variable, expr))

  def backwardAccess(field: Identifier) =
    copy(_1.backwardAccess(field))

  def access(field: Identifier) =
    copy(_1.access(field))

  def removeVariable(variable: Identifier) =
    copy(_1.removeVariable(variable))

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    val (result, map) = _1.createVariableForArgument(variable, typ, path)
    (copy(result), map)
  }

  def createVariable(variable: Identifier, typ: Type) =
    copy(_1.createVariable(variable, typ))

  def assume(expr: Expression) =
    copy(_1.assume(expr))

  def setArgument(variable: Identifier, expr: Expression) =
    copy(_1.setArgument(variable, expr))

  def assign(variable: Identifier, expr: Expression) =
    copy(_1.assign(variable, expr))

  def setToTop(variable: Identifier) =
    copy(_1.setToTop(variable))

  def getStringOfId(id: Identifier) =
    _1.getStringOfId(id)

  def merge(f: Replacement) =
    copy(_1.merge(f))
}

object HalfSemanticCartesianProductDomain {
  /** Simple implementation of `HalfSemanticCartesianProductDomain`.
    * Cannot be extended.
    */
  final case class Default[S <: SemanticDomain[S], O <: Lattice[O]](_1: S, _2: O)
    extends HalfSemanticCartesianProductDomain[S, O, Default[S, O]] {
    def factory(a: S, b: O): Default[S, O] = Default(a, b)
  }
}

/**
 * Reduced Cartesian product supporting the operations of the semantic domain.
 * After each semantic operation the reduction is applied.
 * Note that this implementation is not particularly efficient.
 *
 * @tparam T1 The type of the first domain
 * @tparam T2 The type of the second domain
 * @tparam T The type of the current domain
 * @author Pietro Ferrara
 */
trait ReducedSemanticProductDomain[
    T1 <: SemanticDomain[T1],
    T2 <: SemanticDomain[T2],
    T <: ReducedSemanticProductDomain[T1, T2, T]]
  extends SemanticCartesianProductDomain[T1, T2, T] { this: T =>

  def reduce(): T

  override def lub(other: T): T =
    super.lub(other).reduce()

  override def glb(other: T): T =
    super.glb(other).reduce()

  override def setToTop(variable: Identifier): T =
    super.setToTop(variable).reduce()

  override def assign(variable: Identifier, expr: Expression): T =
    super.assign(variable, expr).reduce()

  override def setArgument(variable: Identifier, expr: Expression): T =
    super.setArgument(variable, expr).reduce()

  override def assume(expr: Expression): T =
    super.assume(expr).reduce()

  override def createVariable(variable: Identifier, typ: Type): T =
    super.createVariable(variable, typ).reduce()

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    val (result, i) = super.createVariableForArgument(variable, typ, path)
    (result.reduce(), i)
  }

  override def removeVariable(variable: Identifier): T =
    super.removeVariable(variable).reduce()

  override def access(field: Identifier): T =
    super.access(field).reduce()

  override def backwardAccess(field: Identifier): T =
    super.backwardAccess(field).reduce()

  override def backwardAssign(oldPreState: T, variable: Identifier, expr: Expression): T = super.backwardAssign(oldPreState, variable, expr).reduce()

  override def merge(r: Replacement): T =
    super.merge(r).reduce()

}