/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation

import org.scalatest.FunSuite
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier

case object DummyObjectTypeA extends DummyObjectType {
  def name = "A"
  override def possibleFields = Set(
    VariableIdentifier("a")(DummyObjectTypeA),
    VariableIdentifier("b")(DummyObjectTypeB),
    VariableIdentifier("i")(DummyNumericalType))
}

case object DummyObjectTypeB extends DummyObjectType {
  def name = "B"
  override def possibleFields = Set(
    VariableIdentifier("a")(DummyObjectTypeA))
}

case object DummyObjectTypeC extends DummyObjectType {
  def name = "C"
  override def possibleFields = Set(
    VariableIdentifier("a")(DummyObjectTypeA))
}

class TypeTest extends FunSuite {
  test("Recursively reachable object types") {
    assert(DummyObjectTypeA.reachableObjectTypes ==
      Set(DummyObjectTypeA, DummyObjectTypeB))

    assert(DummyObjectTypeB.reachableObjectTypes ==
      Set(DummyObjectTypeA, DummyObjectTypeB))

    assert(DummyObjectTypeC.reachableObjectTypes ==
      Set(DummyObjectTypeA, DummyObjectTypeB, DummyObjectTypeC))
  }
}
