/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.inv

import scala.collection.AbstractIterator

trait Enumerator[A] {
  self =>

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  /**
    * Returns a lower bound on the number of remaining elements.
    *
    * @return A lower bound on the number of remaining elements.
    */
  def size: Int = if (nonEmpty) 1 else 0

  /**
    * Returns the current element.
    *
    * @return The current element.
    */
  def get: A

  def getOption: Option[A] = if (isEmpty) None else Some(get)

  def next: Enumerator[A]

  def reset: Enumerator[A]

  def find(p: A => Boolean): Enumerator[A] =
    if (isEmpty || p(get)) this
    else next.find(p)

  def filtered(p: A => Boolean): Enumerator[A] = Enumerator.filtered(this, p)

  def map[B](f: A => B): Enumerator[B] = Enumerator.mapped(this, f)

  def flatMap[B](f: A => Enumerator[B]): Enumerator[B] = Enumerator.flatMapped(this, f)

  def suffix: Enumerator[A] = Enumerator.suffix(this)

  def suffixes: Enumerator[Enumerator[A]] = Enumerator.suffixes(this)

  def iterator: Iterator[A] = new AbstractIterator[A] {

    var current: Enumerator[A] = self

    override def hasNext: Boolean = current.nonEmpty

    override def next(): A = {
      val result = current.get
      current = current.next
      result
    }
  }
}

object Enumerator {

  def empty[A]: Enumerator[A] =
    Empty()

  def sequence[A](list: Seq[A]): Enumerator[A] =
    Sequence(list, list)

  def filtered[A](enumerator: Enumerator[A], p: A => Boolean): Enumerator[A] =
    Filtered(enumerator, p)

  def mapped[A, B](enumerator: Enumerator[A], f: A => B): Enumerator[B] =
    Mapped(enumerator, f)

  def flatMapped[A, B](enumerator: Enumerator[A], f: A => Enumerator[B]): Enumerator[B] =
    flattened(mapped(enumerator, f))

  def flattened[A](enumerator: Enumerator[Enumerator[A]]): Enumerator[A] =
    if (enumerator.isEmpty) Flattened(empty, enumerator)
    else Flattened(enumerator.get, enumerator.next)

  def joined[A](first: Enumerator[A], second: Enumerator[A]): Enumerator[A] =
    Joined(first, second)

  def product[A, B, C](first: Enumerator[A], second: Enumerator[B], f: (A, B) => C): Enumerator[C] =
    Product(first, second, f)

  def suffix[A](enumerator: Enumerator[A]): Enumerator[A] =
    Suffix(enumerator, enumerator)

  def suffixes[A](enumerator: Enumerator[A]): Enumerator[Enumerator[A]] =
    Suffixes(enumerator)

  def tuples[A](enumerator: Enumerator[A]): Enumerator[Seq[A]] = {
    val arities = sequence(0 to enumerator.size)
    arities.flatMap { arity => tuples(enumerator, arity) }
  }


  def tuples[A](enumerator: Enumerator[A], arity: Int): Enumerator[Seq[A]] =
    if (arity == 0) sequence(Seq(Seq.empty))
    else if (enumerator.isEmpty) empty
    else enumerator.suffixes.flatMap { suffix =>
      val head = suffix.get
      val tails = tuples(suffix.next, arity - 1)
      tails.map { tail => head +: tail }
    }

  private case class Empty[A]() extends Enumerator[A] {

    override def isEmpty: Boolean = true

    override def get: A = ???

    override def next: Enumerator[A] = ???

    override def reset: Enumerator[A] = this
  }

  private case class Sequence[A](list: Seq[A], original: Seq[A]) extends Enumerator[A] {

    override def isEmpty: Boolean = list.isEmpty

    override def size: Int = list.size

    override def get: A = list.head

    override def next: Enumerator[A] = Sequence(list.tail, original)

    override def reset: Enumerator[A] = sequence(original)
  }

  private case class Filtered[A](base: Enumerator[A], p: A => Boolean) extends Enumerator[A] {

    override def isEmpty: Boolean = base.isEmpty

    override def get: A = base.get

    override def next: Enumerator[A] = Filtered(base.next.find(p), p)

    override def reset: Enumerator[A] = Filtered(base.reset, p)
  }

  private case class Mapped[A, B](base: Enumerator[A], f: A => B) extends Enumerator[B] {

    override def isEmpty: Boolean = base.isEmpty

    override def size: Int = base.size

    override def get: B = f(base.get)

    override def next: Enumerator[B] = Mapped(base.next, f)

    override def reset: Enumerator[B] = Mapped(base.reset, f)
  }

  private case class Flattened[A](current: Enumerator[A], base: Enumerator[Enumerator[A]]) extends Enumerator[A] {

    override def isEmpty: Boolean = current.isEmpty

    override def get: A = current.get

    override def next: Enumerator[A] = {
      val updated = current.next
      if (updated.nonEmpty) Flattened(updated, base)
      else flattened(base.find(_.nonEmpty))
    }

    override def reset: Enumerator[A] = flattened(base.reset)
  }

  private case class Joined[A](first: Enumerator[A], second: Enumerator[A]) extends Enumerator[A] {

    override def isEmpty: Boolean = first.isEmpty && second.isEmpty

    override def size: Int = first.size + second.size

    override def get: A = first.getOption.getOrElse(second.get)

    override def next: Enumerator[A] = {
      val updated = if (first.nonEmpty) first.next else first
      if (updated.nonEmpty) Joined(updated, second)
      else Joined(updated, second.next)
    }

    override def reset: Enumerator[A] = Joined(first.reset, second.reset)

  }

  private case class Product[A, B, C](first: Enumerator[A], second: Enumerator[B], f: (A, B) => C) extends Enumerator[C] {

    override def isEmpty: Boolean = first.isEmpty

    override def size: Int = math.max((first.size - 1) * second.reset.size + second.size, super.size)

    override def get: C = f(first.get, second.get)

    override def next: Enumerator[C] = {
      val updated = second.next
      if (updated.nonEmpty) Product(first, updated, f)
      else Product(first.next, second.reset, f)
    }

    override def reset: Enumerator[C] = Product(first.reset, second.reset, f)
  }

  private case class Suffix[A](base: Enumerator[A], original: Enumerator[A]) extends Enumerator[A] {

    override def isEmpty: Boolean = base.isEmpty

    override def size: Int = super.size

    override def get: A = base.get

    override def next: Enumerator[A] = Suffix(base.next, original)

    override def reset: Enumerator[A] = original.suffix
  }

  private case class Suffixes[A](base: Enumerator[A]) extends Enumerator[Enumerator[A]] {

    override def isEmpty: Boolean = base.isEmpty

    override def size: Int = base.size

    override def get: Enumerator[A] = base.suffix

    override def next: Enumerator[Enumerator[A]] = base.next.suffixes

    override def reset: Enumerator[Enumerator[A]] = base.reset.suffixes
  }

}