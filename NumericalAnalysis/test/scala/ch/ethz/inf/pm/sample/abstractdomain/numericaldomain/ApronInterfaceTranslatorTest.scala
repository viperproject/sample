package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import apron.Polka
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.abstractdomain.EmptyScopeIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier

/**
 * A dummy object that can be used as a numerical type.
 * @todo move it to a more general place where other unit tests can use it
 */
object DummyNumericType extends Type {
  def factory(): Type = this
  def top(): Type = this
  def bottom(): Type = this
  def lub(other: Type): Type = this
  def glb(other: Type): Type = this
  def widening(other: Type): Type = this
  def lessEqual(other: Type): Boolean = true
  def isObject: Boolean = false
  def isNumericalType: Boolean = true
  def isFloatingPointType: Boolean = true
  def isBooleanType: Boolean = false
  def isStringType: Boolean = false
  def isStatic: Boolean = false
  def name: String = "Dummy Numeric Type"
  def possibleFields: Set[Identifier] = Set.empty
  def arrayElementsType: Option[Type] = None
  def isBottomExcluding(types: Set[Type]): Boolean = true
}

/** Tests ApronInterfaceTranslator. */
class ApronInterfaceTranslatorTest extends FunSuite with BeforeAndAfter with ShouldMatchers {
  val manager = new Polka(true)
  val dom = ApronInterface.Default(None, manager, env = Set.empty)
  val typ = DummyNumericType

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