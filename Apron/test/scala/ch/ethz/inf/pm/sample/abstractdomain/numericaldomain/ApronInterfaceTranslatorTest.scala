/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{Constant, VariableIdentifier, _}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.test.SampleTest
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

/** Tests ApronInterfaceTranslator. */
class ApronInterfaceTranslatorTest extends FunSuite with BeforeAndAfter with Matchers with SampleTest {

  val dom:Apron.Polyhedra = Apron.Polyhedra.Bottom

  val idx: VariableIdentifier = makeVarId("x", SystemParameters.tm.Int)
  val idy: VariableIdentifier = makeVarId("y", SystemParameters.tm.Int)
  val idb1: VariableIdentifier = makeVarId("b1", SystemParameters.tm.Boolean)
  val idb2: VariableIdentifier = makeVarId("b2", SystemParameters.tm.Boolean)
  val constMinus1: Constant = makeConst("-1", SystemParameters.tm.Int)
  val const0: Constant = makeConst("0", SystemParameters.tm.Int)
  val const1: Constant = makeConst("1", SystemParameters.tm.Int)
  val const2: Constant = makeConst("2", SystemParameters.tm.Int)
  val constTrue: Constant = makeConst("true", SystemParameters.tm.Boolean)
  val constFalse: Constant = makeConst("false", SystemParameters.tm.Boolean)

  def makeVarId(name: String, typ: Type): VariableIdentifier = VariableIdentifier(name)(typ)

  def makeConst(value: String, typ: Type) = Constant(value.toString, typ, DummyProgramPoint)

  def translate(iFace: Apron.Polyhedra): Set[String] = {
    val translator = ApronInterfaceTranslator()(iFace.asInstanceOf[Apron.Polyhedra.Inner])
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

    i = i.assign(idx, BinaryArithmeticExpression(idx, const1, ArithmeticOperator.+))
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