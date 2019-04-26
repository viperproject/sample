/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.DummyBooleanType

/**
 * Domain aimed at tracking numerical information
 */
trait NumericalDomain[T <: NumericalDomain[T]] extends SemanticDomain[T] {
  this: T =>
}

object NumericalDomain {

  trait Bottom[T <: NumericalDomain[T]] extends SemanticDomain.Bottom[T] with NumericalDomain[T] {
    this:T =>

    override def getConstraints(ids: Set[Identifier]) = Set(Constant("false", SystemParameters.tm.Boolean)())

  }

  trait Top[T <: NumericalDomain[T]] extends SemanticDomain.Top[T] with NumericalDomain[T] {
    this:T =>

    override def getConstraints(ids: Set[Identifier]) = Set(Constant("true", SystemParameters.tm.Boolean)())

  }

  trait Inner[T <: NumericalDomain[T],X <: NumericalDomain.Inner[T,X]] extends SemanticDomain.Inner[T,X] with NumericalDomain[T] {
    this:T =>

  }


  trait Wrapper[X <: NumericalDomain[X], T <: Wrapper[X,T]]
    extends NumericalDomain[T]
    with SemanticDomainWrapper[X,T] {
    self:T =>

  }

  trait Relational[T <: Relational[T]] extends NumericalDomain[T] {
    this: T =>

    /**
     * Merge two relational domains with !disjoint! environments
     */
    def unify(other: T):T

    /**
     * Join two relational domains with the !same! environments
     */
    def lubSameEnv(other: T):T

    /**
     * Meet two relational domains with the !same! environments
     */
    def glbSameEnv(other: T):T

    /**
     * Widen two relational domains with the !same! environments
     */
    def wideningSameEnv(other: T):T

    /**
     * Compare two relational domains with the !same! environments
     */
    def lessEqualSameEnv(other: T):Boolean

    /**
     * Widen two relational domains with the !same! environments
     * TODO: Move somewhere else
     */
    def remove(other: IdentifierSet):T = other match {
      case IdentifierSet.Bottom =>   this
      case IdentifierSet.Top =>      factory() // "empty"
      case IdentifierSet.Inner(v) => this.removeVariables(v)
    }
  }

  object Relational {

    trait Wrapper[X <: Relational[X], T <: Wrapper[X,T]]
      extends Relational[T]
      with NumericalDomain.Wrapper[X,T] {
      self:T =>

      override def wideningSameEnv(other: T)  = wrapperFactory(wrapped.wideningSameEnv(other.wrapped))
      override def glbSameEnv(other: T)       = wrapperFactory(wrapped.glbSameEnv(other.wrapped))
      override def lubSameEnv(other: T)       = wrapperFactory(wrapped.lubSameEnv(other.wrapped))
      override def unify(other: T)            = wrapperFactory(wrapped.unify(other.wrapped))
      override def lessEqualSameEnv(other: T) = wrapped.lessEqualSameEnv(other.wrapped)

    }

    trait Bottom[T <: Relational[T]] extends NumericalDomain.Bottom[T] with Relational[T] {
      this:T =>
      override def wideningSameEnv(other: T)  = other
      override def glbSameEnv(other: T)       = this
      override def lubSameEnv(other: T)       = other
      override def unify(other: T)            = other
      override def lessEqualSameEnv(other: T) = true
    }

    trait Top[T <: Relational[T]] extends NumericalDomain.Top[T] with Relational[T] {
      this:T =>
      override def wideningSameEnv(other: T)  = this
      override def glbSameEnv(other: T)       = other
      override def lubSameEnv(other: T)       = this
      override def unify(other: T)            = this
      override def lessEqualSameEnv(other: T) = other.isTop
    }

    trait Inner[T <: Relational[T],X <: Relational.Inner[T,X]] extends Relational[T] with NumericalDomain.Inner[T,X]  {
      this:T =>

      override def lubInner(that: X) = {
        if (this.ids == that.ids) lubSameEnv(that.asInstanceOf[T])
        else {
          val common = that.ids glb this.ids
          val diffThis = this.remove(common)
          val diffThat = that.remove(common)
          val extendedThis = this.unify(diffThat)
          val extendedThat = that.unify(diffThis)
          extendedThis.lubSameEnv(extendedThat)
        }
      }

      override def lubSameEnv(other: T) = other match {
        case x: Relational.Top[T] => other
        case x: Relational.Bottom[T] => this
        case x: Relational.Inner[T, X] => lubSameEnvInner(x.asInstanceOf[X])
      }

      override def unify(other: T) = other match {
        case x: Relational.Top[T] => other
        case x: Relational.Bottom[T] => this
        case x: Relational.Inner[T, X] => unifyInner(x.asInstanceOf[X])
      }

      override def glbInner(that: X) = {
        if (this.ids == that.ids) glbSameEnv(that.asInstanceOf[T])
        else {
          val commonThis = this.remove(this.ids -- that.ids)
          val commonThat = that.remove(that.ids -- this.ids)
          commonThis.glbSameEnv(commonThat)
        }
      }

      override def glbSameEnv(other: T) = other match {
        case x: Relational.Top[T] => this
        case x: Relational.Bottom[T] => other
        case x: Relational.Inner[T, X] => glbSameEnvInner(x.asInstanceOf[X])
      }

      override def wideningInner(that: X) = {
        if (this.ids == that.ids) wideningSameEnv(that.asInstanceOf[T])
        else {
          val common = that.ids glb this.ids
          val diffThis = this.remove(common)
          val diffThat = that.remove(common)
          val extendedThis = this.unify(diffThat)
          val extendedThat = that.unify(diffThis)
          extendedThis.wideningSameEnv(extendedThat)
        }
      }

      override def wideningSameEnv(other: T) = other match {
        case x: Relational.Top[T] => other
        case x: Relational.Bottom[T] => this
        case x: Relational.Inner[T, X] => wideningSameEnvInner(x.asInstanceOf[X])
      }

      override def lessEqualInner(that: X) = {
        if (this.ids == that.ids) lessEqualSameEnv(that.asInstanceOf[T])
        else {
          val common = that.ids glb this.ids
          val diffThis = this.remove(common)
          val diffThat = that.remove(common)
          val extendedThis = this.unify(diffThat)
          val extendedThat = that.unify(diffThis)
          extendedThis.lessEqualSameEnv(extendedThat)
        }
      }

      override def lessEqualSameEnv(other: T) = other match {
        case x:Relational.Top[T]     => true
        case x:Relational.Bottom[T]  => false
        case x:Relational.Inner[T,X] => lessEqualSameEnvInner(x.asInstanceOf[X])
      }

      def wideningSameEnvInner(that: X): T
      def unifyInner(that: X): T
      def lubSameEnvInner(that: X): T
      def glbSameEnvInner(that: X): T
      def lessEqualSameEnvInner(that: X): Boolean

    }

  }

}