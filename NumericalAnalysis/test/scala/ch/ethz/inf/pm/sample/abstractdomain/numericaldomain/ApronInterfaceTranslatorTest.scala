package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import apron.Polka
import ch.ethz.inf.pm.sample.oorepresentation.{DummyNumericalType, DummyProgramPoint}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier

/** Tests ApronInterfaceTranslator. */
class ApronInterfaceTranslatorTest extends FunSuite with BeforeAndAfter with ShouldMatchers {
  val manager = new Polka(true)
  val dom = ApronInterface.Default(None, manager, env = Set.empty)
  val typ = DummyNumericalType

  def makeVarId(name: String) = VariableIdentifier(name)(typ)
  def makeConst(value: Int) = Constant(value.toString, typ, DummyProgramPoint)

  val idx = makeVarId("x")
  val idy = makeVarId("y")

  val constMinus1 = makeConst(-1)
  val const0 = makeConst(0)
  val const1 = makeConst(1)
  val const2 = makeConst(2)

  def translate(iFace: ApronInterface.Default): Set[String] =
    ApronInterfaceTranslator.translate(iFace).map(ExpPrettyPrinter).toSet

  test("Simple Translation") {
    var i: ApronInterface.Default = dom

    i = i.createVariable(idx).assign(idx, const1)
    translate(i) should equal (Set("x = 1"))

    i = i.createVariable(idy).assign(idy, const0)
    translate(i) should equal (Set("x = 1", "y = 0"))

    i = i.assign(idx, idy)
    translate(i) should equal (Set("x = 0", "y = 0"))

    i = i.lub(i.assign(idx, const2))
    translate(i) should equal (Set("x ≥ 0", "x ≤ 2", "y = 0"))

    i = i.assign(idy, idx)
    translate(i) should equal (Set("x ≥ 0", "x ≤ 2", "y = x"))

    i = i.assign(idx, BinaryArithmeticExpression(idx, const1, ArithmeticOperator.+, typ))
    translate(i) should equal (Set("x ≥ 1", "x ≤ 3", "1 + y = x"))

    // Should prefer 'x = -1' over 'x + 1 = 0' for readability
    // The unused variable 'y' is added on purpose
    i = dom.createVariable(idx).createVariable(idy).assign(idx, constMinus1)
    translate(i) should equal (Set("x = -1"))
  }
}