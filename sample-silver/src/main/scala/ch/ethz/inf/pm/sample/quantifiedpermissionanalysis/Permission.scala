/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Constant, Expression, Lattice}

/**
  * Represents a permission.
  *
  * @author Jerome Dohrau
  */
sealed trait Permission
  extends Lattice[Permission] {

  override def factory(): Permission = Permission.Fractional(0, 1)

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

  override def widening(other: Permission): Permission = ???
}

object Permission {

  def create(numerator: Expression, denominator: Expression): Permission = {
    val a = numerator.asInstanceOf[Constant].constant.toInt
    val b = denominator.asInstanceOf[Constant].constant.toInt
    Fractional(a, b)
  }

  case object Top
    extends Permission
      with Lattice.Top[Permission]

  case object Bottom
    extends Permission
      with Lattice.Bottom[Permission]

  case object Read
    extends Permission {

    override def isTop: Boolean = false

    override def isBottom: Boolean = false

    override def lessEqual(other: Permission): Boolean = other match {
      case Top => true
      case Bottom => false
      case Read => true
      case Fractional(numerator, _) => numerator <= 0
    }

    override def toString: String = "read"
  }

  case class Fractional(numerator: Int, denominator: Int)
    extends Permission {

    override def isTop: Boolean = false

    override def isBottom: Boolean = false

    override def lessEqual(other: Permission): Boolean = other match {
      case Top => true
      case Bottom => false
      case Read => 0 < numerator
      case other: Fractional => numerator * other.denominator <= other.numerator * denominator
    }

    override def toString: String =
      if (numerator == 0) "none"
      else if (numerator == denominator) "write"
      else s"$numerator/$denominator"
  }

}