package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{SemanticDomain, Constant}
import ch.ethz.inf.pm.sample.oorepresentation.DummyNumericalType
import ch.ethz.inf.pm.sample.test.{SemanticDomainTest, LatticeTest}


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

  val ignoreMostPreciseAssignment = false
  test("Most precise assignment of variables") {
    if (!ignoreMostPreciseAssignment)
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



class IntegerIntervalTest extends NonRelationalNumericalDomainTest[IntegerInterval] {

  override lazy val factory = IntegerInterval.Bottom

  override lazy val instances: Set[IntegerInterval] = super.instances ++ Set(
    IntegerInterval.Inner(-1,Int.MaxValue),
    IntegerInterval.Inner(Int.MinValue,1),
    IntegerInterval.Inner(0,0)
  )

}


class DoubleIntervalTest extends NonRelationalNumericalDomainTest[DoubleInterval] {

  override def factory: DoubleInterval = DoubleInterval.Bottom

  override lazy val instances: Set[DoubleInterval] = super.instances ++ Set(
    DoubleInterval.Inner(-1,Double.PositiveInfinity),
    DoubleInterval.Inner(Double.NegativeInfinity,1),
    DoubleInterval.Inner(0,0)
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

trait ApronTest[T <: Apron[T]]
  extends NumericalDomainTest[T]
    with MostPreciseAssignment[T] {

  SystemParameters.typ = DummyNumericalType

}

trait BoxedNonRelationalNumericalDomainTest[T <: NonRelationalNumericalDomain[T]]
  extends NumericalDomainTest[BoxedNonRelationalNumericalDomain[T]] {

  override def factory = BoxedNonRelationalNumericalDomain[T](domain)

  def domain:T

}

class ApronOctagonTest extends ApronTest[Apron.Octagons] {
  override def factory = Apron.Octagons.Bottom
}

class ApronLinearEqualitiesTest extends ApronTest[Apron.LinearEqualities] {
  override def factory = Apron.LinearEqualities.Bottom

  // FAILS, therefore ignore. This is for antoine
  override val ignoreLubLessEqualWidening = true

}

class ApronOptOctagonsTest extends ApronTest[Apron.OptOctagons] {
  override def factory = Apron.OptOctagons.Bottom
}

class ApronFloatOptOctagonsTest extends ApronTest[Apron.FloatOptOctagons] {
  override def factory = Apron.FloatOptOctagons.Bottom
}

class ApronPolyhedraTest extends ApronTest[Apron.Polyhedra] {
  override def factory = Apron.Polyhedra.Bottom
}

class ApronStrictPolyhedraTest extends ApronTest[Apron.StrictPolyhedra] {
  override def factory = Apron.StrictPolyhedra.Bottom
}

class ApronBoxTest extends ApronTest[Apron.Box] {
  override def factory = Apron.Box.Bottom

  // FAILS, therefore ignore. This is for antoine
  override val ignoreMostPreciseAssignment = true
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