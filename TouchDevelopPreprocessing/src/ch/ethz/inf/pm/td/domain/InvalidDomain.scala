package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.ToStringUtilities
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}

/**
 *
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 10:50 AM
 *
 */

trait InvalidDomain[T <: InvalidDomain[T]] extends SemanticDomain[T] {
  this: T =>
}


trait NumericWithInvalidDomain[
N <: NumericalDomain[N],
I <: InvalidDomain[I],
T <: NumericWithInvalidDomain[N, I, T]]
  extends SemanticCartesianProductDomain[N, I, T]
  with NumericalDomain[T] {
  this: T =>

  override def assign(id:Identifier, expr:Expression) =
    if (containsValidInvalidExpression(expr))
      factory(_1.setToTop(id), _2.assign(id,expr))
    else factory(_1.assign(id,expr), _2.assign(id,expr))

  override def assume(expr:Expression) =
    if (containsValidInvalidExpression(expr))
      factory(_1, _2.assume(expr))
    else factory(_1.assume(expr), _2.assume(expr))

  def numericalDomain: N = _1

  def invalidDomain: I = _2

  def isValidInvalidExpression(expr:Expression):Boolean = expr match {
    case a:InvalidExpression => true
    case a:ValidExpression => true
    case _ => false
  }

  def containsValidInvalidExpression(expr:Expression) = expr contains isValidInvalidExpression

  override def toString = "Numeric:\n" + ToStringUtilities.indent(this._1.toString) + "\nInvalid:\n" + ToStringUtilities.indent(this._2.toString)

  override def getConstraints(ids: Set[Identifier]) = ???

}

/**
 * Represents TouchDevelops "Invalid" value
 * @param typ Type of the invalid value
 * @param explanation String describing the cause of the invalid value
 * @param pp Program Point of the invalid value
 */
case class InvalidExpression(typ: Type, explanation: String, pp: ProgramPoint) extends Expression {
  def ids = IdentifierSet.Bottom

  override def toString = "invalid(" + explanation + ")"

  def transform(f: (Expression) => Expression): Expression = f(this)

  def contains(f: (Expression => Boolean)): Boolean = f(this)
}

/**
 * Represent an expression that is valid (no other information given, so essentially top minus invalid)
 * @param typ Type of the valid value
 * @param pp Program Point of the valid value
 */
case class ValidExpression(typ: Type, pp: ProgramPoint) extends Expression {
  def ids = IdentifierSet.Bottom

  override def toString = "valid"

  def transform(f: (Expression) => Expression): Expression = f(this)

  def contains(f: (Expression => Boolean)): Boolean = f(this)
}