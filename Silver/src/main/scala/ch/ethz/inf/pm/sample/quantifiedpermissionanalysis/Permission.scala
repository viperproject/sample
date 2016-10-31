package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.Lattice

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */
trait Permission extends Lattice[Permission] {
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


  override def widening(other: Permission): Permission = ???

  /**
    * Returns the sum of this permission and the other permission.
    *
    * @param other the permission to be added
    */
  def plus(other: Permission): Permission

  /**
    * Returns the difference of this permission and the other permission.
    *
    * @param other the permission to be subtracted
    */
  def minus(other: Permission): Permission

  /**
    * Returns whether the amount of the permission is at most one.
    */
  def isFeasible: Boolean

  /**
    * Returns whether the amount of the permission is greater or equal to one.
    */
  def isWrite: Boolean

  /**
    * Returns whether the amount of the permission is strictly greater than zero.
    */
  def isSome: Boolean

  /**
    * Returns whether the amount of the permission is at most zero.
    */
  def isNone: Boolean
}

object Permission {
  /**
    * Returns no permission.
    */
  def none: Permission = fractional(0, 1)

  /**
    * Returns a read permission.
    */
  def read: Permission = inner(0, 1, read = true)

  /**
    * Returns a write permission.
    */
  def write: Permission = fractional(1, 1)

  /**
    * Returns a fractional permission.
    *
    * @param numerator   the numerator of the fraction
    * @param denominator the denominator of the fraction
    */
  def fractional(numerator: Int, denominator: Int): Permission =
  inner(numerator, denominator, false)

  /**
    * Returns a permission that is the sum of a fractional permission and a read
    * permission
    *
    * @param numerator   the numerator of the fractional part
    * @param denominator the denominator of the fractional part
    * @param read        indicates whether ther is a read part
    */
  private def inner(numerator: Int, denominator: Int, read: Boolean): Permission = {
    val div = gcd(numerator, denominator)
    Inner(numerator / div, denominator / div, read)
  }

  case object Top extends Permission with Lattice.Top[Permission] {
    override def plus(other: Permission): Permission = top()

    override def minus(other: Permission): Permission = top()

    override def isFeasible: Boolean = false

    override def isWrite: Boolean = true

    override def isSome: Boolean = true

    override def isNone: Boolean = false
  }

  case object Bottom extends Permission with Lattice.Bottom[Permission] {
    override def plus(other: Permission): Permission =
      if (other.isTop) top()
      else bottom()

    override def minus(other: Permission): Permission =
      if (other.isBottom) top()
      else bottom()

    override def isFeasible: Boolean = true

    override def isWrite: Boolean = false

    override def isSome: Boolean = false

    override def isNone: Boolean = true
  }

  /**
    * A permission that is the sum of a fractional permission and a read
    * permission
    *
    * @param numerator   the numerator of the fractional part
    * @param denominator the denominator of the fractional part
    * @param read        indicates whether ther is a read part
    */
  case class Inner(numerator: Int, denominator: Int, read: Boolean) extends Permission {

    override def isBottom: Boolean = false

    override def isTop: Boolean = false

    override def lessEqual(other: Permission): Boolean = other match {
      case Top => true
      case Bottom => false
      case Inner(oNumerator, oDenominator, oRead) =>
        val x = numerator * oDenominator
        val y = denominator * oNumerator
        x < y || (x == y && (!read || oRead))
    }

    override def plus(other: Permission): Permission = other match {
      case Top => top()
      case Bottom => bottom()
      case Inner(oNumerator, oDenominator, oRead) =>
        val newNumerator = numerator * oDenominator + denominator * oNumerator
        val newDenominator = denominator * oDenominator
        val newRead = read | oRead
        inner(newNumerator, newDenominator, newRead)
    }

    override def minus(other: Permission): Permission = other match {
      case Top => bottom()
      case Bottom => top()
      case Inner(oNumerator, oDenominator, _) =>
        val newNumerator = numerator * oDenominator - denominator * oNumerator
        val newDenominator = denominator * oDenominator
        inner(newNumerator, newDenominator, read)
    }

    override def isFeasible: Boolean =
      amount < 1 || (amount == 1 && !read)

    override def isWrite: Boolean =
      amount >= 1

    override def isSome: Boolean =
      amount > 0 || (amount == 0 && read)

    override def isNone: Boolean =
      amount < 0 || (amount == 0 && !read)

    def amount: Double =
      numerator.toDouble / denominator
  }

  /**
    * Computes the greatest common divisor of the two specified integers.
    *
    * @param a the first integer
    * @param b the second integer
    */
  private def gcd(a: Int, b: Int): Int =
  if (b == 0) a.abs
  else gcd(b, a % b)
}