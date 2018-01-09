/*
 * This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not
 * distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain

import ch.ethz.inf.pm.sample.abstractdomain.Lattice

/**
  * A domain that lifts an existing domain to stacks.
  *
  * @tparam T The type of the elements in the stack.
  * @author Jerome Dohrau
  */
sealed trait StackDomain[T <: Lattice[T]]
  extends Lattice[StackDomain[T]] {

  def default: T

  def get(): T

  def update(f: T => T): StackDomain[T]

  def map(f: T => T): StackDomain[T]

  /**
    * Pushes a new element onto the stack.  The new top element is computed by the given function which gets the current
    * top element as its input. If no function is specified the default
    * element is pushed onto the stack.
    *
    * @param f The function used to compute the new top element.
    * @return The resulting stack.
    */
  def push(f: T => T = _ => default): StackDomain[T]

  def pop(f: (T, T) => T = (_, lower) => lower): StackDomain[T]

  override def factory(): StackDomain[T] = StackDomain(default)

  override def top(): StackDomain[T] = StackDomain.Top(default)

  override def bottom(): StackDomain[T] = StackDomain.Bottom(default)
}

object StackDomain {

  def apply[T <: Lattice[T]](default: T): StackDomain[T] =
    StackDomain.Inner(default :: Nil, default)

  case class Top[T <: Lattice[T]](default: T)
    extends StackDomain[T]
      with Lattice.Top[StackDomain[T]] {

    override def get(): T = default.top()

    override def update(f: (T) => T): StackDomain[T] = this

    override def map(f: (T) => T): StackDomain[T] = this

    override def push(f: (T) => T): StackDomain[T] = this

    override def pop(f: (T, T) => T): StackDomain[T] = this
  }

  case class Bottom[T <: Lattice[T]](default: T)
    extends StackDomain[T]
      with Lattice.Bottom[StackDomain[T]] {

    override def get(): T = default.bottom()

    override def update(f: (T) => T): StackDomain[T] = this

    override def map(f: (T) => T): StackDomain[T] = this

    override def push(f: (T) => T): StackDomain[T] = this

    override def pop(f: (T, T) => T): StackDomain[T] = this
  }

  case class Inner[T <: Lattice[T]](stack: List[T], default: T)
    extends StackDomain[T]
      with Lattice.Inner[StackDomain[T], Inner[T]] {

    override def get(): T = stack.head

    override def update(f: (T) => T): StackDomain[T] = StackDomain.Inner(f(stack.head) :: stack.tail, default)

    override def map(f: (T) => T): StackDomain[T] = StackDomain.Inner(stack.map(f), default)

    override def push(f: (T) => T): StackDomain[T] =
      StackDomain.Inner(f(stack.head) :: stack, default)

    override def pop(f: (T, T) => T): StackDomain[T] = {
      val first :: second :: rest = stack
      StackDomain.Inner(f(first, second) :: rest, default)
    }

    override def lubInner(other: Inner[T]): StackDomain[T] = mergeInner(other, _ lub _)

    override def glbInner(other: Inner[T]): StackDomain[T] = mergeInner(other, _ glb _)

    override def wideningInner(other: Inner[T]): StackDomain[T] = mergeInner(other, _ widening _)

    override def lessEqualInner(other: Inner[T]): Boolean = {
      assert(stack.size == other.stack.size)
      val zipped = stack zip other.stack
      zipped.forall { case (left, right) => left lessEqual right }
    }

    private def mergeInner(other: Inner[T], combine: (T, T) => T): StackDomain[T] = {
      assert(stack.size == other.stack.size)
      assert(default == other.default)
      val zipped = stack zip other.stack
      val newStack = zipped.map { case (left, right) => combine(left, right) }
      StackDomain.Inner(newStack, default)
    }
  }

}