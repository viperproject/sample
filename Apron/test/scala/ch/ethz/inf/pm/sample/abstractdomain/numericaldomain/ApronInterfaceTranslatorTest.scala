/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{Constant, VariableIdentifier, _}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyBooleanType, DummyNumericalType, DummyProgramPoint, Type}
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

/** Tests ApronInterfaceTranslator. */
class ApronInterfaceTranslatorTest extends FunSuite with BeforeAndAfter with Matchers {
  val dom:Apron.Polyhedra = Apron.Polyhedra.Bottom
  val numType = DummyNumericalType
  val boolType = DummyBooleanType

  SystemParameters.typ = DummyNumericalType

  def makeVarId(name: String, typ: Type) = VariableIdentifier(name)(typ)
  def makeConst(value: String, typ: Type) = Constant(value.toString, typ, DummyProgramPoint)

  val idx = makeVarId("x", numType)
  val idy = makeVarId("y", numType)

  val idb1 = makeVarId("b1", boolType)
  val idb2 = makeVarId("b2", boolType)

  val constMinus1 = makeConst("-1", numType)
  val const0 = makeConst("0", numType)
  val const1 = makeConst("1", numType)
  val const2 = makeConst("2", numType)

  val constTrue = makeConst("true", boolType)
  val constFalse = makeConst("false", boolType)

  def translate(iFace: Apron.Polyhedra): Set[String] = {
    val translator = ApronInterfaceTranslator(boolType = boolType)(iFace.asInstanceOf[Apron.Polyhedra.Inner])
    translator.translateAll().map(ExpPrettyPrinter)
  }

  test("Numerical") {
    var i = dom

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

    i = i.assign(idx, BinaryArithmeticExpression(idx, const1, ArithmeticOperator.+, numType))
    translate(i) should equal (Set("x ≥ 1", "x ≤ 3", "1 + y = x"))

    // Should prefer 'x = -1' over 'x + 1 = 0' for readability
    // The unused variable 'y' is added on purpose
    i = dom.createVariable(idx).createVariable(idy).assign(idx, constMinus1)
    translate(i) should equal (Set("x = -1"))
  }

  test("Booleans") {
    var i = dom

    i = i.createVariable(idb1)
    translate(i) should equal (Set())

    i = i.assign(idb1, constTrue)
    translate(i) should equal (Set("b1"))

    i = i.createVariable(idb2).assign(idb2, constFalse)
    translate(i) should equal (Set("b1", "! b2"))

    i = i.assign(idb2, idb1)
    translate(i) should equal (Set("b1", "b2"))
  }
}