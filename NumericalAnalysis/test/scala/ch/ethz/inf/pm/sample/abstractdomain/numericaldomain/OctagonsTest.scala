package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyBooleanType, DummyNumericalType}

trait RelationalNumericalDomainTest[T <: NumericalDomain[T]]
  extends NumericalDomainTest[T] {

}

class OctagonsTest
  extends RelationalNumericalDomainTest[Octagons] {

  def constant(value: Int): Constant =
    Constant(value.toString, DummyNumericalType)

  def variable(name: String): Identifier =
    VariableIdentifier(name)(DummyNumericalType)

  def equal(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.==, DummyBooleanType)

  def lessOrEqual(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.<=, DummyBooleanType)

  def greaterOrEqual(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.>=, DummyBooleanType)

  def and(left: Expression, right: Expression): Expression =
    BinaryBooleanExpression(left, right, BooleanOperator.&&, DummyBooleanType)

  def or(left: Expression, right: Expression): Expression =
    BinaryBooleanExpression(left, right, BooleanOperator.||, DummyBooleanType)

  def inRange(expr: Expression, low: Expression, high: Expression): Expression = {
    and(greaterOrEqual(expr, low), lessOrEqual(expr, high))
  }

  def plus(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.+, DummyNumericalType)

  var once = false

  /**
    * Add more instances to the test suite
    */
  override def instances: Set[Octagons] =
    super.instances ++ Set(
      Octagons.Top,
      Octagons.Bottom,
      Octagons.Top
        .assign(variable("a"), constant(2)),
      Octagons.Top
        .assign(variable("b"), constant(3)),
      Octagons.Top
        .assign(variable("a"), constant(1))
        .assume(equal(variable("a"), variable("b"))),
      Octagons.Top
        .assume(inRange(variable("a"), constant(-10), constant(10)))
        .assume(inRange(variable("c"), constant(0), constant(5)))
    )

  override def factory: Octagons = Octagons.Bottom
}
