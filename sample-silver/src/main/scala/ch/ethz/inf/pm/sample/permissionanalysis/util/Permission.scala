/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis.util

import ch.ethz.inf.pm.sample.abstractdomain.Lattice


/** Represents a permission.
  *
  * @author Jerome Dohrau
  */
trait Permission
  extends Lattice[Permission] {
  override def factory(): Permission = top()

  override def top(): Permission = Permission.Top

  override def bottom(): Permission = Permission.Bottom

  override def lub(other: Permission): Permission =
    if (this lessEqual other) other
    else if (other lessEqual this) this
    else top()

  override def glb(other: Permission): Permission =
    if (this lessEqual other) this
    else if (other lessEqual this) other
    else bottom()

  override def widening(other: Permission): Permission =
    if (other lessEqual this) this
    else top()

  /** Returns the sum of this permission and the other permission.
    *
    * @param other The permission to be added.
    */
  def plus(other: Permission): Permission

  /** Returns the difference of this permission and the other permission.
    *
    * @param other The permission to be subtracted.
    */
  def minus(other: Permission): Permission

  /** Returns whether the amount of the permission is strictly greater than zero.
    */
  def isSome: Boolean

  /** Returns whether the amount of the permission is at most zero.
    */
  def isNone: Boolean

  /**
    * Returns true if the permission is infeasible, i.e., the amount is at most
    * one.
    *
    * @return True if the permission is infeasible.
    */
  def isInfeasible: Boolean
}

object Permission {
  /** Returns no permission.
    */
  def none: Permission = fractional(0, 1)

  /**
    * Returns a read permission.
    */
  def read: Permission = fractional(0, 1, 1)

  /** Returns a write permission.
    */
  def write: Permission = fractional(1, 1)

  /** Returns a permission that is the sum of a fractional permission and a read
    * permission
    *
    * @param numerator   The numerator of the fractional part.
    * @param denominator The denominator of the fractional part.
    * @param read        The counter for the read permission.
    */
  def fractional(numerator: Int, denominator: Int, read: Int = 0): Permission = {
    val div = gcd(numerator, denominator)
    Fractional(numerator / div, denominator / div, read)
  }

  case object Top extends Permission with Lattice.Top[Permission] {
    override def plus(other: Permission): Permission = top()

    override def minus(other: Permission): Permission = top()

    override def isSome: Boolean = true

    override def isNone: Boolean = false

    override def isInfeasible: Boolean = true

    override def toString: String = "⊤"
  }

  case object Bottom extends Permission with Lattice.Bottom[Permission] {
    override def plus(other: Permission): Permission =
      if (other.isTop) top()
      else bottom()

    override def minus(other: Permission): Permission =
      if (other.isBottom) top()
      else bottom()

    override def isSome: Boolean = false

    override def isNone: Boolean = true

    override def isInfeasible: Boolean = false

    override def toString: String = "⊥"
  }

  /** A permission that is the sum of a fractional permission and a read
    * permission
    *
    * @param numerator   The numerator of the fractional part.
    * @param denominator The denominator of the fractional part.
    * @param read        The counter for the read permission.
    */
  case class Fractional(numerator: Int, denominator: Int, read: Int)
    extends Permission {
    override def isBottom: Boolean = false

    override def isTop: Boolean = false

    override def lessEqual(other: Permission): Boolean = other match {
      case Top => true
      case Bottom => false
      case Fractional(oNumerator, oDenominator, oRead) =>
        val x = numerator * oDenominator
        val y = denominator * oNumerator
        x < y || (x == y && read <= oRead)
    }

    override def plus(other: Permission): Permission = other match {
      case Top => top()
      case Bottom => bottom()
      case Fractional(oNumerator, oDenominator, oRead) =>
        val newNumerator = numerator * oDenominator + denominator * oNumerator
        val newDenominator = denominator * oDenominator
        val newRead = read | oRead
        fractional(newNumerator, newDenominator, newRead)
    }

    override def minus(other: Permission): Permission = other match {
      case Top => bottom()
      case Bottom => top()
      case Fractional(oNumerator, oDenominator, oRead) =>
        val newNumerator = numerator * oDenominator - denominator * oNumerator
        val newDenominator = denominator * oDenominator
        val newRead = read - oRead
        fractional(newNumerator, newDenominator, newRead)
    }

    override def isSome: Boolean =
      amount > 0 || (amount == 0 && 0 < read)

    override def isNone: Boolean =
      amount < 0 || (amount == 0 && read <= 0)

    override def isInfeasible: Boolean =
      amount > 1 || (amount == 1 && read > 0)

    def amount: Double =
      numerator.toDouble / denominator

    override def toString: String = {
      if (read == 0) {
        if (numerator == 0) "none"
        else if (numerator == denominator) "write"
        else s"$numerator / $denominator"
      } else {
        val suffix = if (read == 1) "read" else s"$read * read"
        if (numerator == 0) suffix
        else if (numerator == denominator) s"write + $suffix"
        else s"$numerator / $denominator + $suffix"
      }
    }
  }

  /** Computes the greatest common divisor of the two specified integers.
    *
    * @param a The first integer.
    * @param b The second integer.
    */
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a.abs
    else gcd(b, a % b)
}