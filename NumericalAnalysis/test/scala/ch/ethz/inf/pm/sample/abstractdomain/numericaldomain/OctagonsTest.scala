package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyBooleanType, DummyIntegerType}

trait RelationalNumericalDomainTest[T <: NumericalDomain[T]]
  extends NumericalDomainTest[T] {

}

class OctagonsTest
  extends RelationalNumericalDomainTest[IntegerOctagons] {

  var once = false

  def or(left: Expression, right: Expression): Expression =
    BinaryBooleanExpression(left, right, BooleanOperator.||, DummyBooleanType)

  def plus(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.+, DummyIntegerType)

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
    Constant(value.toString, DummyIntegerType)

  def variable(name: String): Identifier =
    VariableIdentifier(name)(DummyIntegerType)

  def equal(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.==, DummyBooleanType)

  def inRange(expr: Expression, low: Expression, high: Expression): Expression = {
    and(greaterOrEqual(expr, low), lessOrEqual(expr, high))
  }

  def lessOrEqual(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.<=, DummyBooleanType)

  def greaterOrEqual(left: Expression, right: Expression): Expression =
    BinaryArithmeticExpression(left, right, ArithmeticOperator.>=, DummyBooleanType)

  def and(left: Expression, right: Expression): Expression =
    BinaryBooleanExpression(left, right, BooleanOperator.&&, DummyBooleanType)

  override def factory: IntegerOctagons = IntegerOctagons.Bottom
}
