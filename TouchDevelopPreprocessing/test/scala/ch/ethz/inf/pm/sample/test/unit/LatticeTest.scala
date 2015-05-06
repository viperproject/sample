package ch.ethz.inf.pm.sample.test.unit

import apron._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.td.domain.{ValidExpression, InvalidExpression, BooleanInvalidDomainWithSource}
import org.scalatest.FunSuite

/**
 * @author Lucas Brutschy
 */
trait LatticeTest[T <: Lattice[T]] extends FunSuite {

  /**
   * Creates a bottom instance of T
   */
  def factory:T

  /**
   * Overwrite this to extend the test suite
   */
  def instances:Set[T] = Set(
    factory,factory.bottom(),factory.top()
  )

  test("There is only one top/bottom element") {
    for (a <- instances) {
      assert { a.isTop == (a == a.top) && a.isBottom == (a == a.bottom) }
    }
  }

  test("bottom lessEqual everything") {
    for (a <- instances) {
      assert { (a.bottom lessEqual a) && ((a lessEqual a.bottom) == a.isBottom) }
    }
  }

  test("everything lessEqual top") {
    for (a <- instances) {
      assert { (a lessEqual a.top) && ((a.top lessEqual a) == a.isTop) }
    }
  }

  test("glb lessEqual lub") {
    for (a <- instances; b <- instances) {
      assert { (a glb b) lessEqual (a lub b) }
    }
  }

  test("lub lessEqual widening") {
    for (a <- instances; b <- instances) {
      assert { (a lub b) lessEqual (a widening b) }
    }
  }

  test("glb on yourself is identity") {
    for (a <- instances) {
      assert { (a glb a) equivalent a }
    }
  }

  test("lub on yourself is identity") {
    for (a <- instances) {
      assert { (a lub a) equivalent a }
    }
  }

  test("widening on yourself is identity") {
    for (a <- instances) {
      assert { (a widening a) equivalent a }
    }
  }

  test("lub is commutative") {
    for (a <- instances; b <- instances) {
      assert { (a lub b) equivalent (b lub a) }
    }
  }

  test("glb is commutative") {
    for (a <- instances; b <- instances) {
      assert { (a glb b) equivalent (b glb a) }
    }
  }

}

trait SemanticDomainTest[T <: SemanticDomain[T]] extends LatticeTest[T] {

  test("Creating Variables") {
    for (a <- instances) {
      val x = a.createVariable(v1).createVariable(v2)
      assert { x.ids.contains(v1) }
      assert { x.ids.contains(v2) }
    }
  }

  test("Removing Variables") {
    for (a <- instances) {
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


trait NumericalDomainTest[T <: NumericalDomain[T]] extends SemanticDomainTest[T] {

  override lazy val typ = DummyNumericalType

  override def values = super.values ++ Set(
    Constant("0",typ),
    Constant("2",typ),
    Constant("-2",typ),
    Constant(Int.MaxValue.toString,typ),
    Constant(Int.MinValue.toString,typ)
  )

}

// Not necessarily satisfied by all domains, e.g. Sign
trait MostPreciseAssignment[T <: SemanticDomain[T]] extends SemanticDomainTest[T] {

  test("Most precise assignment of variables") {
    for (a1 <- values; a2 <- values) {
      if (a1 != a2) {
        val x = factory.createVariable(v1).createVariable(v2).assign(v1, a1).assign(v2, a2)
        assert (x.assume(v1 unequal a1).isBottom)
        assert (x.assume(v2 unequal a2).isBottom)
        assert (x.assume(v1 unequal v1).isBottom)
        assert (x.assume(v2 unequal v2).isBottom)
      }
    }
  }

}

trait NonRelationalNumericalDomainTest[T <: NonRelationalNumericalDomain[T]] extends LatticeTest[T] {

  lazy val two = instances.head.evalConstant(2)
  lazy val four = instances.head.evalConstant(4)


  test("2 + 2 = 4") {
    assert { !((two sum two) glb four).isBottom }
  }

  test("2 * 2 = 4") {
    assert { !((two multiply two) glb four).isBottom }
  }

}


class SetDomainTest extends LatticeTest[SetDomain.Default[Int]] {

  override lazy val factory = SetDomain.Default.Bottom[Int]()

  override lazy val instances: Set[SetDomain.Default[Int]] = super.instances ++ Set(
    SetDomain.Default.Inner(Set(1,2,9)),
    SetDomain.Default.Inner(Set(1,3,10)),
    SetDomain.Default.Inner(Set.empty[Int])
  )

}


class IntegerIntervalTest extends NonRelationalNumericalDomainTest[IntegerInterval] {

  override lazy val factory = IntegerInterval.Bottom

  override lazy val instances: Set[IntegerInterval] = super.instances ++ Set(
    IntegerInterval.Inner(-1,Int.MaxValue),
    IntegerInterval.Inner(Int.MinValue,1),
    IntegerInterval.Inner(0,0),
    IntegerInterval.Inner(1,0),
    IntegerInterval.Inner(Int.MinValue,Int.MaxValue)
  )

}


class DoubleIntervalTest extends NonRelationalNumericalDomainTest[DoubleInterval] {

  override def factory: DoubleInterval = DoubleInterval.Bottom

  override lazy val instances: Set[DoubleInterval] = super.instances ++ Set(
    DoubleInterval.Inner(-1,Double.PositiveInfinity),
    DoubleInterval.Inner(Double.NegativeInfinity,1),
    DoubleInterval.Inner(0,0),
    DoubleInterval.Inner(1,0),
    DoubleInterval.Inner(Int.MinValue,Int.MaxValue)
  )

}


class SignTest extends NonRelationalNumericalDomainTest[Sign] {

  override lazy val factory = Sign.Bottom

  override lazy val instances: Set[Sign] = super.instances ++ Set(
    Sign.Minus,
    Sign.Plus,
    Sign.Zero
  )

}

trait ApronInterfaceTest
  extends NumericalDomainTest[ApronInterface.Default]
  with MostPreciseAssignment[ApronInterface.Default] {

  override lazy val factory = ApronInterface.Default(None,domain,isPureBottom = false,Set.empty)

  def domain:Manager

}

trait BoxedNonRelationalNumericalDomainTest[T <: NonRelationalNumericalDomain[T]]
  extends NumericalDomainTest[BoxedNonRelationalNumericalDomain[T]] {

  override def factory = BoxedNonRelationalNumericalDomain[T](domain)

  def domain:T
  
}

class ApronOctagonTest extends ApronInterfaceTest {
  override lazy val domain: Manager = new Octagon
}

class ApronStrictPolkaTest extends ApronInterfaceTest {
  override lazy val domain: Manager = new Polka(true)
}

class ApronWeakPolkaTest extends ApronInterfaceTest {
  override lazy val domain: Manager = new Polka(false)
}

class ApronOptOctagonTest extends ApronInterfaceTest {
  override lazy val domain: Manager = new OptOctagon
}

class ApronPolkaEqTest extends ApronInterfaceTest {
  override lazy val domain: Manager = new PolkaEq
}

class ApronBoxTest extends ApronInterfaceTest {
  override lazy val domain: Manager = new Box
}

class BoxedIntegerIntervalTest
  extends BoxedNonRelationalNumericalDomainTest[IntegerInterval] {
  override lazy val domain: IntegerInterval = IntegerInterval.Bottom
}

class BoxedDoubleIntervalTest
  extends BoxedNonRelationalNumericalDomainTest[DoubleInterval]
  with MostPreciseAssignment[BoxedNonRelationalNumericalDomain[DoubleInterval]] {
  override lazy val domain: DoubleInterval = DoubleInterval.Bottom
}

class BoxedSignTest
  extends BoxedNonRelationalNumericalDomainTest[Sign] {
  override lazy val domain: Sign = Sign.Bottom
}

class BooleanInvalidDomainWithSourceTest
  extends SemanticDomainTest[BooleanInvalidDomainWithSource]
  with MostPreciseAssignment[BooleanInvalidDomainWithSource] {

  override lazy val values = super.values ++ Set(
    InvalidExpression(typ,"dummy1",DummyProgramPoint),
    ValidExpression(typ,DummyProgramPoint)
  )

  override lazy val typ = DummyNumericalType
  override lazy val factory = BooleanInvalidDomainWithSource()
}

trait StringDomainTest[T <: StringDomain[T]] extends SemanticDomainTest[T] {
  override def typ: Type = DummyStringType
  override def values = super.values ++ Set(
    Constant("a",typ),
    Constant("bbbbbbbbbbbbbbbbbbbbbb",typ),
    Constant("",typ)
  )
}

class PrefixTest extends StringDomainTest[Prefix] {
  override def factory: Prefix = new Prefix()
}
class SuffixTest extends StringDomainTest[Suffix] {
  override def factory: Suffix = new Suffix()
}
class BricksTest extends StringDomainTest[Bricks] {
  override def factory: Bricks = new Bricks()
}
class MaybeContainedCharactersTest extends StringDomainTest[MaybeContainedCharacters] {
  override def factory: MaybeContainedCharacters = new MaybeContainedCharacters()
}
class SurelyContainedCharactersTest extends StringDomainTest[SurelyContainedCharacters] {
  override def factory: SurelyContainedCharacters = new SurelyContainedCharacters()
}
trait NonrelationalStringDomainTest[T <: StringValueSetDomain[T]] extends StringDomainTest[NonrelationalStringDomain[T]]
class StringKSetDomainTest extends StringDomainTest[NonrelationalStringDomain[StringKSetDomain]] {
  override def factory: NonrelationalStringDomain[StringKSetDomain] = new NonrelationalStringDomain[StringKSetDomain](StringKSetDomain.Bottom(2))
}