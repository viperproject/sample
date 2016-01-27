package ch.ethz.inf.pm.sample.test

import ch.ethz.inf.pm.sample.abstractdomain.{Constant, Expression, SemanticDomain, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyBooleanType, Type}

/**
  * Implements generic, property-based testing for semantic domains. These check well-formedness properties of lattice
  * elements which have proven helpful to provide more robust implementations. In particular:
  *
  *  - Creating vars, removing vars, assignment have the expected value
  *  - Assuming false is bottom, assuming top is identity.
  *
  * To use this basic infrastructure, extend this trait, and implement factory to create an empty element.
  * Also extend the return value of instances to produce more interesting instances of the lattice than
  * just bottom/top. Furthermore, provide more values for assignment by overriding "value", and define
  * an expression type (e.g. numerical) that fits for the domain.
  *
  * @author Lucas Brutschy
  */
trait SemanticDomainTest[T <: SemanticDomain[T]] extends LatticeTest[T] {

  test("Creating Variables") {
    for (a <- instances) {
      val x = a.createVariable(v1).createVariable(v2)
      assert { x.ids.contains(v1) }
      assert { x.ids.contains(v2) }
    }
  }

  test("Removing Variables") {
    for (a <- instances) if (!a.isTop) {
      val x = a.createVariable(v1).createVariable(v2).removeVariable(v1).removeVariable(v2)
      assert { !x.ids.contains(v1) }
      assert { !x.ids.contains(v2) }
    }
  }

  test("Assuming false gives bottom") {
    for (a <- instances) {
      assert { a.assume(Constant("false",DummyBooleanType)).isBottom }
    }
  }

  test("Assuming true gives identity") {
    for (a <- instances) {
      assert { a.assume(Constant("true",DummyBooleanType)) == a }
    }
  }

  test("Assigning Variables") {
    for (a1 <- values; a2 <- values) {
      if (a1 != a2) {
        val x = factory.createVariable(v1).createVariable(v2).assign(v1, a1).assign(v2, a2)
        assert (!x.assume(v1 equal a1).isBottom)
        assert (!x.assume(v2 equal a2).isBottom)
        assert (!x.assume(v1 equal v1).isBottom)
        assert (!x.assume(v2 equal v2).isBottom)
      }
    }
  }

  override def instances = super.instances ++
    (for (a1 <- values) yield factory.createVariable(v1, typ).assign(v1, a1)) ++
    (for (a1 <- values; a2 <- values) yield factory.createVariable(v1, typ).createVariable(v2, typ).assign(v1, a1).assign(v2, a2))

  def values:Set[Expression] = Set.empty
  def typ:Type
  lazy val v1 = VariableIdentifier("v1")(typ)
  lazy val v2 = VariableIdentifier("v2")(typ)
}
