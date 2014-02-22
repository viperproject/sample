package ch.ethz.inf.pm.sample.oorepresentation

import org.scalatest.FunSuite
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier

/** A dummy type with no proper hierarchy. */
trait DummyType extends Type {
  def factory() = this
  def top() = this
  def bottom() = this
  def lub(other: Type) = this
  def glb(other: Type) = this
  def widening(other: Type) = this
  def lessEqual(other: Type) = true
  def isFloatingPointType = false
  def isBooleanType = false
  def isStringType = false
  def isStatic = false
  def arrayElementsType = None
  def isBottomExcluding(types: Set[Type]) = true
}

/** A dummy object type with no proper hierarchy. */
trait DummyObjectType extends DummyType {
  def isObject = true
  def isNumericalType = false
}

/** A dummy numerical type with no proper hierarchy. */
case object DummyNumericalType extends DummyType {
  def name = "Int"
  def isObject = false
  def isNumericalType = true
  def possibleFields = Set.empty
}

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
