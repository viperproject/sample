/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._

trait RelationalNumericalDomainTest[T <: NumericalDomain[T]]
  extends NumericalDomainTest[T] {

}

class OctagonsTest
  extends RelationalNumericalDomainTest[IntegerOctagons] {

  var once = false

  def or(left: Expression, right: Expression): Expression =
    BinaryBooleanExpression(left, right, BooleanOperator.||)

  def plus(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.+)

  /**
    * Add more instances to the test suite
    */
  override def instances: Set[IntegerOctagons] =
    super.instances ++ Set(
      IntegerOctagons.Top,
      IntegerOctagons.Bottom,
      IntegerOctagons.Top
        .assign(variable("a"), constant(2)),
      IntegerOctagons.Top
        .assign(variable("b"), constant(3)),
      IntegerOctagons.Top
        .assign(variable("a"), constant(1))
        .assume(equal(variable("a"), variable("b"))),
      IntegerOctagons.Top
        .assume(inRange(variable("a"), constant(-10), constant(10)))
        .assume(inRange(variable("c"), constant(0), constant(5)))
    )

  def constant(value: Int): Constant =
    Constant(value.toString, SystemParameters.tm.Int)

  def variable(name: String): Identifier =
    VariableIdentifier(name)(SystemParameters.tm.Int)

  def equal(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.==)

  def inRange(expr: Expression, low: Expression, high: Expression): Expression = {
    and(greaterOrEqual(expr, low), lessOrEqual(expr, high))
  }

  def lessOrEqual(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.<=)

  def greaterOrEqual(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.>=)

  def and(left: Expression, right: Expression): Expression =
    BinaryBooleanExpression(left, right, BooleanOperator.&&)

  override def factory: IntegerOctagons = IntegerOctagons.Bottom
}
