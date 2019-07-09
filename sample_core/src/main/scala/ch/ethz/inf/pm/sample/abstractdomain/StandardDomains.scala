/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

//This file contains several implementations of some standard domains. These standard domains are quite useful to develop
//new analyses without rewriting some standard code.

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain.SetDomain.Default
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.util.Relation

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
  extends Lattice[T] {
  this: T =>

  def isTop: Boolean

  def map: Map[K, V]

  override def factory(): T = functionalFactory()

  /**
    * Creates a new instance of the functional domain with the given contents.
    *
    * @param map      The map of values, empty if bottom or top
    * @param isBottom Domain is bottom
    * @param isTop    Domain is top for all keys
    * @return A fresh instance
    */
  def functionalFactory(
                         map: Map[K, V] = Map.empty[K, V],
                         isBottom: Boolean = false,
                         isTop: Boolean = false): T

  /**
    * Adds [key->value] to the domain
    *
    * @param key   The key
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
    *
    * @param key The key to be removed
    * @return The state of the domain after the key has been removed
    */
  def remove(key: K): T = functionalFactory(map - key)

  /**
    * Computes the upper bound between two states. It is defined by:
    * this \sqcup other = [k -> this(k) \sqcup other(k) : k \in dom(this) \cup dom(other)]
    *
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

  private def lift(other: T, keySetFunc: (Set[K], Set[K]) => Set[K], valueFunc: (V, V) => V): T = {
    val newMap = keySetFunc(map.keySet, other.map.keySet).map(k =>
      k -> valueFunc(get(k), other.get(k))).toMap
    functionalFactory(newMap)
  }

  /**
    * Computes the lower bound between two states. It is defined by:
    * this \sqcap other = [k -> this(k) \sqcap other(k) : k \in dom(this) \cap dom(other)]
    *
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

  override def strictGlb(other: T): T = {
    if (isBottom) return this
    if (other.isBottom) return other
    if (isTop) return other
    if (other.isTop) return this
    lift(other, _ intersect _, _ glb _)
  }

  /**
    * Computes the widening between two states. It is defined by:
    * this \nable other = [k -> this(k) \nabla other(k) : k \in dom(this) \cup dom(other)]
    *
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
    *
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
    case right: T@unchecked =>
      if (isBottom && right.isBottom) return true
      if (isBottom || right.isBottom) return false
      if (isTop && right.isTop) return true
      if (isTop || right.isTop) return false
      if (map.keySet.equals(right.map.keySet)) {
        for (variable <- map.keySet)
          if (!map(variable).equals(right.map(variable)))
            return false
        true
      }
      else false
    case _ => false
  }

  override def top(): T = functionalFactory(isTop = true)

  override def bottom(): T = functionalFactory(isBottom = true)

  override def toString: String = {
    if (isBottom) "⊥"
    else if (isTop) "⊤"
    else ToStringUtilities.mapToString(map)
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
                                                override val isBottom: Boolean = false,
                                                defaultValue: V)
    extends FunctionalDomain[K, V, Default[K, V]] {

    override def get(key: K): V = map.getOrElse(key, defaultValue)

    override def functionalFactory(value: Map[K, V], isBottom: Boolean, isTop: Boolean) =
      Default(value, isTop, isBottom, defaultValue)
  }

}

trait MergeDomain[T <: MergeDomain[T]] extends Lattice[T] {
  this: T =>

  /** For each set of identifiers in the domain of f, this method merges these identifiers into the given one.
    *
    * @param f The identifiers to merge
    * @return the state after the merge
    */
  def merge(f: Replacement): T

}

/**
  * Simplifies the implementation of the merge function by restricting the set of allowed replacements
  */
trait SimplifiedMergeDomain[T <: SimplifiedMergeDomain[T]] extends MergeDomain[T] {
  this: T =>

  /**
    * For each set of identifiers in the domain of f, this method merges these identifiers
    * into the given one.
    *
    * @param f The identifiers to merge
    * @return the state after the merge
    */
  override def merge(f: Replacement): T = {
    var cur = this
    for ((from, to) <- f.value) {
      if (from.size == 1 && to.size > 1) cur = cur.expand(from.head, to)
      else if (from.size > 1 && to.size == 1) cur = cur.fold(from, to.head)
      else if (from.size == 1 && to.size == 1) cur = cur.rename(from.head, to.head)
      else if (to.isEmpty) cur = cur.remove(from)
      else if (from.isEmpty) cur = cur.add(to)
      else new NotImplementedError("This domain only supports fold, expand, rename, remove and add; No general replacement support.")
    }
    cur
  }

  def expand(idA: Identifier, idsB: Set[Identifier]): T

  def rename(idA: Identifier, idB: Identifier): T

  def remove(ids: Set[Identifier]): T

  def fold(idsA: Set[Identifier], idB: Identifier): T

  def add(ids: Set[Identifier]): T

}

object SimplifiedMergeDomain {

  trait Bottom[T <: SimplifiedMergeDomain[T]] extends Lattice.Bottom[T] with SimplifiedMergeDomain[T] {
    this: T =>
    override def fold(idsA: Set[Identifier], idB: Identifier): T = this

    override def remove(ids: Set[Identifier]): T = this

    override def rename(idA: Identifier, idB: Identifier): T = this

    override def expand(idA: Identifier, idsB: Set[Identifier]): T = this
  }

  trait Top[T <: SimplifiedMergeDomain[T]] extends Lattice.Top[T] with SimplifiedMergeDomain[T] {
    this: T =>
    override def add(ids: Set[Identifier]): T = this

    override def fold(idsA: Set[Identifier], idB: Identifier): T = this

    override def remove(ids: Set[Identifier]): T = this

    override def rename(idA: Identifier, idB: Identifier): T = this

    override def expand(idA: Identifier, idsB: Set[Identifier]): T = this
  }

}

object RelationalDomain {

  trait Top[T <: RelationalDomain[T]] extends RelationalDomain[T] with SemanticDomain.Top[T] with SimplifiedMergeDomain.Top[T] {
    this: T =>
  }

  trait Bottom[T <: RelationalDomain[T]] extends RelationalDomain[T] with SemanticDomain.Bottom[T] with SimplifiedMergeDomain.Bottom[T] {
    this: T =>
  }

  trait Inner[T <: RelationalDomain[T], X <: RelationalDomain.Inner[T, X]] extends RelationalDomain[T] with SemanticDomain.Inner[T, X] {
    this: T =>

    val elements: Relation[Identifier]

    override def lessEqualInner(other: X): Boolean = other.elements.subSetOf(elements)

    override def wideningInner(other: X): T = lubInner(other)

    override def lubInner(other: X): T = factory(elements.intersect(other.elements))

    override def glbInner(other: X): T = factory(elements.union(other.elements))

    override def expand(idA: Identifier, idsB: Set[Identifier]): T = factory(elements.expand(idA, idsB))

    override def rename(idA: Identifier, idB: Identifier): T = factory(elements.rename(idA, idB))

    override def remove(ids: Set[Identifier]): T = factory(elements.remove(ids))

    override def fold(idsA: Set[Identifier], idB: Identifier): T = factory(elements.fold(idsA, idB))

    override def add(ids: Set[Identifier]): T = this

    override def setToTop(variable: Identifier): T = removeVariable(variable)

    override def removeVariable(id: Identifier): T = factory(elements.remove(id))

    override def ids: IdentifierSet = IdentifierSet.Inner(elements.getAll)

  }

}

trait RelationalDomain[T <: RelationalDomain[T]] extends SemanticDomain[T] with SimplifiedMergeDomain[T] {
  this: T =>

  override def factory(): T = factory(Relation.empty[Identifier])

  def factory(rel: Relation[Identifier]): T

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
  extends FunctionalDomain[Identifier, V, T]
    with SemanticDomain[T] {
  this: T =>

  override def merge(r: Replacement): T = {
    if (r.isEmpty()) return this
    var result: T = this
    val removedVariables = r.keySet().flatten

    // We remove the variables from the result state
    for (v <- removedVariables)
      result = result.remove(v)

    for (s <- r.keySet()) {
      if (!(ids glb IdentifierSet.Inner(s)).isBottom) {

        // We compute the value that should be assigned to all other ids
        val value = Lattice.bigLub(s.map(this.get))

        // We assign the value to all other ids
        for (v <- r.apply(s)) result = result.merge(v, value)

      }
    }

    result
  }

  private def merge(id: Identifier, v: V): T = {
    if (this.map.keySet.contains(id))
      this.add(id, v.lub(this.get(id)))
    else this.add(id, v)
  }

  override def ids: IdentifierSet =
    if (isBottom || map.isEmpty) IdentifierSet.Bottom
    else if (isTop) IdentifierSet.Top
    else IdentifierSet.Inner(map.keySet)

  override def getStringOfId(id: Identifier): String = this.get(id).toString

}


/** A domain that is represented by a set.
  *
  * The lattice operators are the common ones of sets, that is, the upper bound
  * is the union, the lower bound the intersection, and so on.
  *
  * @tparam V The type of the values contained in the set
  * @tparam T The type of the current set domain
  * @author Lucas Brutschy
  * @author Jerome Dohrau
  * @author Pietro Ferrara
  */
trait SetDomain[V, T <: SetDomain[V, T]] extends Lattice[T] {
  this: T =>

  /** Constructs a new set domain of the concrete type
    *
    * This has to make sure that the corresponding top or bottom element is returned
    *
    * @return a fresh, empty instance of the set domain
    */
  def factory(value: Set[V] = Set.empty[V]): T

  /** Removes an element from the set. Formally, return = old(this)\setminus {v}
    *
    * @param v The element to be removed
    * @return The abstract state without the given element.
    */
  def -(v: V): T

  /** Removes a set from the set. Formally, return = old(this) \setminus v
    *
    * @param v The element to be removed
    * @return The abstract state without the given set of elements.
    */
  def --(v: T): T

  /** Adds an element to the set. Formally, return = old(this) \cup {v}
    *
    * @param v The element to be added
    * @return The abstract state with the given element as well.
    */
  def +(v: V): T

  /** Adds an element to the set. Formally, return = old(this) \cup V
    *
    * @param v The element to be added
    * @return The abstract state with the given element as well.
    */
  def ++(v: T): T

  /** Checks whether an element is in the set
    *
    * @return A new instance of the current object
    */
  def contains(v: V): Boolean

  /**
    * Returns whether there is an element that satisfies the given predicate.
    *
    * @param predicate The predicate.
    * @return True if there is an element that satisfies the given predicate.
    */
  def exists(predicate: V => Boolean): Boolean

  /**
    * Returns whether all elements satisfy the given predicate.
    *
    * @param predicate The predicate.
    * @return True if all elements satisfy the given predicate.
    */
  def forall(predicate: V => Boolean): Boolean = !exists(!predicate(_))

  /** Converts a set domain into a set. Here, we assume that the domain element is
    * not top. If the domain element is top, this will fail.
    *
    * @return A representation of this domain as a set
    */
  def toSet: Set[V]

  /** Converts a set domain into a set. Here, we assume that the domain element is
    * not top. If the domain element is top, this will fail.
    *
    * @return A representation of this domain as a set
    */
  def map[B](f: V => B): SetDomain.Default[B]

  override def factory(): T = top()

  // Helpers
  def ++(v: Set[V]): T = ++(factory(v))

  def --(v: Set[V]): T = --(factory(v))

}

object SetDomain {

  /** Simple implementation of `SetDomain`. Cannot be extended. */
  sealed trait Default[V] extends SetDomain[V, Default[V]] {

    override def top() = Default.Top()

    override def factory(value: Set[V]): Default[V] =
      if (value.isEmpty) bottom() else Default.Inner(value)

    override def bottom() = Default.Bottom()

  }

  trait Top[V, T <: SetDomain[V, T]]
    extends SetDomain[V, T]
      with Lattice.Top[T] {
    this: T =>

    override def -(v: V): T = this

    override def --(v: T): T = if (v.isTop) bottom() else this

    override def +(v: V): T = this

    override def ++(v: T): T = this

    override def contains(v: V) = true

    override def exists(predicate: (V) => Boolean): Boolean = true

    override def toSet = throw new UnsupportedOperationException("Called toSetOrFail on a top value")

    override def map[B](f: V => B): SetDomain.Default[B] = SetDomain.Default.Top[B]()

  }

  trait Bottom[V, T <: SetDomain[V, T]]
    extends SetDomain[V, T]
      with Lattice.Bottom[T] {
    this: T =>

    override def -(v: V): T = this

    override def --(v: T): T = this

    override def +(v: V): T = factory(Set(v))

    override def ++(v: T): T = v

    override def contains(v: V) = false

    override def exists(predicate: (V) => Boolean): Boolean = false

    override def toSet: Set[V] = Set.empty

    override def map[B](f: V => B): SetDomain.Default[B] = SetDomain.Default.Bottom[B]()

  }

  trait Inner[V, T <: SetDomain[V, T], I <: Inner[V, T, I]]
    extends SetDomain[V, T]
      with Lattice.Inner[T, I] {
    this: T =>

    if (SystemParameters.DEBUG) {
      // This should be bottom
      assert {
        value.nonEmpty
      }
    }

    def value: Set[V]

    override def --(v: T): T = v match {
      case a: Bottom[V, T] => this
      case a: Top[V, T] => bottom()
      case a: Inner[V, T, I] => factory(value -- a.value)
    }

    override def -(v: V): T = factory(value - v)

    override def +(v: V): T = factory(value + v)

    override def ++(v: T): T = lub(v)

    override def contains(v: V): Boolean = value.contains(v)

    override def exists(predicate: (V) => Boolean): Boolean = value.exists(predicate)

    override def glbInner(other: I): T = factory(value intersect other.value)

    // using the least upper bound as widening is okay since we assume that
    // there are a finite number of elements.
    override def wideningInner(other: I): T = lubInner(other)

    override def lubInner(other: I): T = factory(value ++ other.value)

    override def lessEqualInner(other: I): Boolean = value subsetOf other.value

    override def toSet: Set[V] = value

    override def map[B](f: V => B): SetDomain.Default[B] = SetDomain.Default.Inner[B](value.map(f))

    override def toString: String = ToStringUtilities.setToString(value)

  }

  /**
    * A set domain which is bounded by a given function
    *
    * @tparam V the values stored
    * @tparam T the type itself
    * @author Lucas Brutschy
    */
  trait Bounded[V, T <: Bounded[V, T]]
    extends SetDomain[V, T] {
    this: T =>

    /**
      * Returns a version of this set which restricts the bounds
      */
    def cap: T

  }

  object Default {

    final case class Inner[V](value: Set[V])
      extends Default[V] with SetDomain.Inner[V, Default[V], Inner[V]]

    final case class Bottom[V]()
      extends Default[V] with SetDomain.Bottom[V, Default[V]]

    final case class Top[V]()
      extends Default[V] with SetDomain.Top[V, Default[V]]

  }

  object Bounded {

    trait Bottom[V, T <: Bounded[V, T]] extends Bounded[V, T] with SetDomain.Bottom[V, T] {
      this: T =>
      override def cap: T = this
    }

    trait Top[V, T <: Bounded[V, T]] extends Bounded[V, T] with SetDomain.Top[V, T] {
      this: T =>
      override def cap: T = this
    }

    trait Inner[V, T <: Bounded[V, T], I <: Inner[V, T, I]] extends Bounded[V, T] with SetDomain.Inner[V, T, I] {
      this: T =>

      override def lubInner(other: I): T = super.lubInner(other).cap

      override def wideningInner(other: I): T = super.wideningInner(other).cap

      override def ++(v: T): T = super.++(v).cap

      override def +(v: V): T = super.+(v).cap

    }

    trait Default[V] extends Bounded[V, Bounded.Default[V]] {

      /** The bound of the set */
      val k: Int

      override def bottom() = Bounded.Default.Bottom(k)

      override def top() = Bounded.Default.Top(k)

      override def factory(value: Set[V]) = Bounded.Default.Inner(k, value)

    }

    object Default {

      case class Inner[V](k: Int, value: Set[V]) extends Bounded.Default[V] with Bounded.Inner[V, Bounded.Default[V], Bounded.Default.Inner[V]] {

        override def cap: Default[V] = if (value.size > k) top() else this

      }

      case class Bottom[V](k: Int) extends Bounded.Default[V] with Bounded.Bottom[V, Bounded.Default[V]]

      case class Top[V](k: Int) extends Bounded.Default[V] with Bounded.Top[V, Bounded.Default[V]]

    }

  }

}

/**
  * A very commonly used set domain, which implements a merge function
  */
trait IdentifierSet extends SetDomain[Identifier, IdentifierSet] with SimplifiedMergeDomain[IdentifierSet] {

  override def factory(value: Set[Identifier]): IdentifierSet =
    if (value.isEmpty) IdentifierSet.Bottom
    else IdentifierSet.Inner(value)

  override def bottom(): IdentifierSet = IdentifierSet.Bottom

  override def top(): IdentifierSet = IdentifierSet.Top

  def getNonTop: Set[Identifier]

  def getNonTopUnsafe: Set[Identifier]

}

object IdentifierSet {

  case class Inner(value: Set[Identifier]) extends SetDomain.Inner[Identifier, IdentifierSet, Inner] with IdentifierSet {
    override def expand(idA: Identifier, idsB: Set[Identifier]): IdentifierSet = factory(value - idA ++ idsB)

    override def rename(idA: Identifier, idB: Identifier): IdentifierSet = factory(value - idA + idB)

    override def remove(ids: Set[Identifier]): IdentifierSet = factory(value -- ids)

    override def fold(idsA: Set[Identifier], idB: Identifier): IdentifierSet = factory(value -- idsA + idB)

    override def add(ids: Set[Identifier]): IdentifierSet = factory(value ++ ids)

    override def getNonTop: Set[Identifier] = value

    override def getNonTopUnsafe: Set[Identifier] = value
  }

  object Bottom extends SetDomain.Bottom[Identifier, IdentifierSet] with IdentifierSet {
    override def expand(idA: Identifier, idsB: Set[Identifier]): IdentifierSet = this

    override def rename(idA: Identifier, idB: Identifier): IdentifierSet = this

    override def remove(ids: Set[Identifier]): IdentifierSet = this

    override def fold(idsA: Set[Identifier], idB: Identifier): IdentifierSet = this

    override def add(ids: Set[Identifier]): IdentifierSet = factory(ids)

    override def getNonTop: Set[Identifier] = Set.empty

    override def getNonTopUnsafe: Set[Identifier] = Set.empty
  }

  object Top extends SetDomain.Top[Identifier, IdentifierSet] with IdentifierSet {
    override def expand(idA: Identifier, idsB: Set[Identifier]): IdentifierSet = this

    override def rename(idA: Identifier, idB: Identifier): IdentifierSet = this

    override def remove(ids: Set[Identifier]): IdentifierSet = this

    override def fold(idsA: Set[Identifier], idB: Identifier): IdentifierSet = this

    override def add(ids: Set[Identifier]): IdentifierSet = this

    override def getNonTop: Set[Identifier] = throw new UnsupportedOperationException("Invalid access")

    override def getNonTopUnsafe: Set[Identifier] = {
      println("trying to convert top lattice to set --- unsound")
      Set.empty
    }
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
  * @tparam T  The type of the current domain
  * @author Pietro Ferrara, Lucas Brutschy
  */
trait CartesianProductDomain[
T1 <: Lattice[T1],
T2 <: Lattice[T2],
T <: CartesianProductDomain[T1, T2, T]]
  extends Lattice[T] with Product2[T1, T2] {
  this: T =>

  def factory(a: T1, b: T2): T

  def set_1(a: T1): T = factory(a, _2)

  def set_2(b: T2): T = factory(_1, b)

  override def factory(): T = factory(_1.factory(), _2.factory())

  def top(): T = factory(_1.top(), _2.top())

  def bottom(): T = factory(_1.bottom(), _2.bottom())

  def lub(other: T): T = factory(_1.lub(other._1), _2.lub(other._2))

  def glb(other: T): T = factory(_1.glb(other._1), _2.glb(other._2))

  def isTop: Boolean = _1.isTop && _2.isTop

  override def strictGlb(other: T): T = factory(_1.strictGlb(other._1), _2.strictGlb(other._2))

  def widening(other: T): T = factory(_1.widening(other._1), _2.widening(other._2))

  def lessEqual(other: T): Boolean = {
    if (isBottom) return true
    if (other.isBottom) return false
    _1.lessEqual(other._1) && _2.lessEqual(other._2)
  }

  def isBottom: Boolean = _1.isBottom || _2.isBottom

  override def toString: String =
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
  * @tparam T  The type of the current domain
  * @author Pietro Ferrara, Lucas Brutschy
  */
trait ReducedProductDomain[
T1 <: Lattice[T1],
T2 <: Lattice[T2],
T <: ReducedProductDomain[T1, T2, T]]
  extends CartesianProductDomain[T1, T2, T] {
  this: T =>

  /**
    * Reduce the information contained in the two domains. The returned value
    * has to be less or equal (that is, more precise) than the initial state.
    *
    * @return The reduced abstract state
    */
  def reduce(): T
}

/**
  * Cartesian product supporting the operations of the semantic domain.
  *
  * It applies operations to neither, one or both domains depending on the
  * identifiers in the expressions that the operations operate upon.
  * Subclasses can define which domain can handle which identifiers.
  *
  * @tparam T1 The type of the first domain
  * @tparam T2 The type of the second domain
  * @tparam T  The type of the current domain
  */
trait RoutingSemanticCartesianProductDomain[
T1 <: SemanticDomain[T1],
T2 <: SemanticDomain[T2],
T <: RoutingSemanticCartesianProductDomain[T1, T2, T]]
  extends CartesianProductDomain[T1, T2, T] with SemanticDomain[T] {
  this: T =>

  /** Returns true if the first domain can handle the given identifier. */
  def _1canHandle(id: Identifier): Boolean

  /** Returns true if the second domain can handle the given identifier. */
  def _2canHandle(id: Identifier): Boolean

  override def ids: IdentifierSet = _1.ids ++ _2.ids

  override def setToTop(variable: Identifier): T =
    factory(variable, _1.setToTop, _2.setToTop)

  override def assign(variable: Identifier, expr: Expression): T =
    factory(variable, expr, _1.assign, _2.assign)

  override def setArgument(variable: Identifier, expr: Expression): T =
    factory(variable, expr, _1.setArgument, _2.setArgument)

  override def assume(expr: Expression): T =
    factory(expr, _1.assume, _2.assume)

  override def merge(r: Replacement): T = {

    if (r.isEmpty()) return this

    def filter(r: Replacement, f: Identifier => Boolean): Replacement = {
      val result = new Replacement(
        isPureExpanding = r.isPureExpanding,
        isPureRemoving = r.isPureRemoving,
        isPureRenaming = r.isPureRenaming)
      r.value.map {
        case (from, to) =>
          if (from.forall(f) && to.forall(f))
            result.value += from -> to
      }
      result
    }

    val firstReplacement = filter(r, _1canHandle)
    val secondReplacement = filter(r, _2canHandle)
    factory(_1.merge(firstReplacement), _2.merge(secondReplacement))
  }

  override def createVariable(variable: Identifier, typ: Type): T =
    factory[Identifier](variable, _1.createVariable(_, typ), _2.createVariable(_, typ))

  /**
    * Constructs a new state with `op_1` applied to the state of the first
    * domain only if it can handle the identifiers in the expression passed
    * as an argument. Analogous for `op_2`.
    *
    * @param exp  the expression to supply to the operations
    * @param op_1 to apply to the first domain if it can handle `exp`
    * @param op_2 to apply to the second domain if it can handle `exp`
    * @tparam E the type of the expression
    * @return the new state
    */
  def factory[E <: Expression](exp: E, op_1: E => T1, op_2: E => T2): T = {
    factory(
      if (_1canHandle(exp)) op_1(exp) else _1,
      if (_2canHandle(exp)) op_2(exp) else _2)
  }

  /** Returns true if the first domain can handle all identifiers
    * in the given expressions.
    */
  private def _1canHandle(exps: Expression*): Boolean =
    _1canHandle(Lattice.bigLub(exps.map(_.ids)))

  def _1canHandle(ids: IdentifierSet): Boolean =
    ids match {
      case IdentifierSet.Bottom => true
      case IdentifierSet.Top => true
      case IdentifierSet.Inner(v) => v.forall(_1canHandle)
    }

  /** Returns true if the second domain can handle all identifiers
    * in the given expressions.
    */
  private def _2canHandle(exps: Expression*): Boolean =
    _2canHandle(Lattice.bigLub(exps.map(_.ids)))

  def _2canHandle(ids: IdentifierSet): Boolean =
    ids match {
      case IdentifierSet.Bottom => true
      case IdentifierSet.Top => true
      case IdentifierSet.Inner(v) => v.forall(_2canHandle)
    }

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]): (T, Map[Identifier, List[String]]) = {
    val (a1, b1) = if (_1canHandle(variable)) _1.createVariableForArgument(variable, typ, path) else (_1, Map.empty[Identifier, List[String]])
    val (a2, b2) = if (_2canHandle(variable)) _2.createVariableForArgument(variable, typ, path) else (_2, Map.empty[Identifier, List[String]])
    (factory(a1, a2), b1 ++ b2)
  }

  def removeVariable(variable: Identifier): T =
    factory(variable, _1.removeVariable, _2.removeVariable)

  def backwardAssign(oldPreState: T, variable: Identifier, expr: Expression): T =
    factory[Identifier, Expression](variable, expr,
      _1.backwardAssign(oldPreState._1, _, _),
      _2.backwardAssign(oldPreState._2, _, _))

  /** Analogous to `factory`, but with two expressions passed as arguments. */
  def factory[E1 <: Expression, E2 <: Expression](
                                                   a: E1, b: E2, op_1: (E1, E2) => T1, op_2: (E1, E2) => T2): T = {
    factory(
      if (_1canHandle(a, b)) op_1(a, b) else _1,
      if (_2canHandle(a, b)) op_2(a, b) else _2)
  }

  def getStringOfId(id: Identifier): String = {
    if (_1canHandle(id) && _2canHandle(id))
      "(" + _1.getStringOfId(id) + ", " + _2.getStringOfId(id) + ")"
    else if (_1canHandle(id))
      _1.getStringOfId(id)
    else if (_2canHandle(id))
      _2.getStringOfId(id)
    else
      ""
  }

  override def explainError(expr: Expression): Set[(String, ProgramPoint)] = _1.explainError(expr) ++ _2.explainError(expr)

  override def getPossibleConstants(id: Identifier): Default[Constant] = _1.getPossibleConstants(id) glb _2.getPossibleConstants(id)

  /**
    * Given a possible set of constraints
    *
    * @param ids the list of identifiers which should be addressed
    * @return a set of expressions that express a statement about th ids. May return "true"
    */
  override def getConstraints(ids: Set[Identifier]): Set[Expression] = {
    _1.getConstraints(ids) ++ _2.getConstraints(ids)
  }
}

/**
  * Cartesian product supporting the operations of the semantic domain.
  *
  * @tparam T1 The type of the first domain
  * @tparam T2 The type of the second domain
  * @tparam T  The type of the current domain
  * @author Pietro Ferrara
  */
trait SemanticCartesianProductDomain[
T1 <: SemanticDomain[T1],
T2 <: SemanticDomain[T2],
T <: SemanticCartesianProductDomain[T1, T2, T]]
  extends RoutingSemanticCartesianProductDomain[T1, T2, T] {
  this: T =>

  def _1canHandle(id: Identifier) = true

  def _2canHandle(id: Identifier) = true
}

object SemanticCartesianProductDomain {

  /** Simple implementation of `SemanticCartesianProductDomain`. Cannot be extended. */
  final case class Default[T1 <: SemanticDomain[T1], T2 <: SemanticDomain[T2]](_1: T1, _2: T2)
    extends SemanticCartesianProductDomain[T1, T2, Default[T1, T2]] {
    def factory(_1: T1, _2: T2): Default[T1, T2] = Default(_1, _2)
  }

}


/**
  * Reduced Cartesian product supporting the operations of the semantic domain.
  * After each semantic operation the reduction is applied.
  * Note that this implementation is not particularly efficient.
  *
  * @tparam T1 The type of the first domain
  * @tparam T2 The type of the second domain
  * @tparam T  The type of the current domain
  * @author Pietro Ferrara
  */
trait ReducedSemanticProductDomain[
T1 <: SemanticDomain[T1],
T2 <: SemanticDomain[T2],
T <: ReducedSemanticProductDomain[T1, T2, T]]
  extends SelectiveReducedSemanticProductDomain[T1, T2, T] {
  this: T =>

  def reduce(ids: Set[Identifier]): T = reduce()
}

/**
  * Reduced Cartesian product supporting the operations of the semantic domain.
  * After each semantic operation the reduction is applied.
  *
  * This reduced product is selectively applied to the given identifiers
  *
  * @tparam T1 The type of the first domain
  * @tparam T2 The type of the second domain
  * @tparam T  The type of the current domain
  * @author Pietro Ferrara
  */
trait SelectiveReducedSemanticProductDomain[
T1 <: SemanticDomain[T1],
T2 <: SemanticDomain[T2],
T <: SelectiveReducedSemanticProductDomain[T1, T2, T]]
  extends SemanticCartesianProductDomain[T1, T2, T] {
  this: T =>

  def reduce(): T

  def reduce(ids: IdentifierSet): T

  override def lub(other: T): T =
    super.lub(other).reduce()

  override def glb(other: T): T =
    super.glb(other).reduce()

  override def setToTop(variable: Identifier): T =
    super.setToTop(variable).reduce(IdentifierSet.Inner(Set(variable)))

  override def assign(variable: Identifier, expr: Expression): T =
    super.assign(variable, expr).reduce(expr.ids + variable)

  override def setArgument(variable: Identifier, expr: Expression): T =
    super.setArgument(variable, expr).reduce(expr.ids + variable)

  override def assume(expr: Expression): T =
    super.assume(expr).reduce(expr.ids)

  override def createVariable(variable: Identifier, typ: Type): T =
    super.createVariable(variable, typ).reduce(IdentifierSet.Inner(Set(variable)))

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]): (T, Map[Identifier, List[String]]) = {
    val (result, i) = super.createVariableForArgument(variable, typ, path)
    (result.reduce(IdentifierSet.Inner(Set(variable))), i)
  }

  override def removeVariable(variable: Identifier): T =
    super.removeVariable(variable).reduce(IdentifierSet.Inner(Set(variable)))

  override def backwardAssign(oldPreState: T, variable: Identifier, expr: Expression): T =
    super.backwardAssign(oldPreState, variable, expr).reduce(expr.ids + variable)

  override def merge(r: Replacement): T = {
    if (r.isEmpty()) return this
    super.merge(r).reduce(r.ids)
  }

}