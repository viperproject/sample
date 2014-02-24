package ch.ethz.inf.pm.sample.oorepresentation

import org.scalatest.FunSuite
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier

case object DummyObjectTypeA extends DummyObjectType {
  def name = "A"
  def possibleFields = Set(
    VariableIdentifier("a")(DummyObjectTypeA),
    VariableIdentifier("b")(DummyObjectTypeB),
    VariableIdentifier("i")(DummyNumericalType))
}

case object DummyObjectTypeB extends DummyObjectType {
  def name = "B"
  def possibleFields = Set(
    VariableIdentifier("a")(DummyObjectTypeA))
}

case object DummyObjectTypeC extends DummyObjectType {
  def name = "C"
  def possibleFields = Set(
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