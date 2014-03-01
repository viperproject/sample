package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.ToStringUtilities
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression
import BooleanDomain._

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 10:50 AM
 * 
 */

trait InvalidDomain[T <: InvalidDomain[T]] extends SemanticDomain[T] { this: T => }


trait NumericWithInvalidDomain[
    N <: NumericalDomain[N],
    I <: InvalidDomain[I],
    T <: NumericWithInvalidDomain[N, I, T]]
  extends SemanticCartesianProductDomain[N, I, T]
  with NumericalDomain[T] { this: T =>

  override def _1canHandle(id: Identifier) = id.typ.isNumericalType

  def numericalDomain: N = _1

  def invalidDomain: I = _2

  override def toString = "Numeric:\n"+ToStringUtilities.indent(this._1.toString)+"\nInvalid:\n"+ToStringUtilities.indent(this._2.toString)

}

/**
 * Represents TouchDevelops "Invalid" value
 * @param typ Type of the invalid value
 * @param pp Program Point of the invalid value
 */
case class InvalidExpression(typ: Type, pp: ProgramPoint) extends Expression {
  def ids = Set.empty
  override def toString = "invalid"
  def transform(f: (Expression) => Expression): Expression = f(this)
}

/**
 * Represent an expression that is valid (no other information given, so essentially top minus invalid)
 * @param typ Type of the valid value
 * @param pp Program Point of the valid value
 */
case class ValidExpression(typ: Type, pp: ProgramPoint) extends Expression {
  def ids = Set.empty
  override def toString = "valid"
  def transform(f: (Expression) => Expression): Expression = f(this)
}