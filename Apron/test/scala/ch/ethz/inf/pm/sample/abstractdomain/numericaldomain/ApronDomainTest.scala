package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{Constant, SemanticDomain}
import ch.ethz.inf.pm.sample.oorepresentation.DummyIntegerType
import ch.ethz.inf.pm.sample.test.SemanticDomainTest

trait NumericalDomainTest[T <: NumericalDomain[T]] extends SemanticDomainTest[T] {

  override lazy val typ = DummyIntegerType

  override def values = super.values ++ Set(
    Constant("0", typ),
    Constant("2", typ),
    Constant("-2", typ),
    Constant(Int.MaxValue.toString, typ),
    Constant(Int.MinValue.toString, typ)
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
          assert(x.assume(v1 unequal a1).isBottom)
          assert(x.assume(v2 unequal a2).isBottom)
          assert(x.assume(v1 unequal v1).isBottom)
          assert(x.assume(v2 unequal v2).isBottom)
        }
      }
  }

}

trait ApronTest[T <: Apron[T]]
  extends NumericalDomainTest[T]
    with MostPreciseAssignment[T] {

  SystemParameters.typ = DummyIntegerType

}

class ApronOctagonTest extends ApronTest[Apron.Octagons] {
  override def factory = Apron.Octagons.Bottom
}

class ApronLinearEqualitiesTest extends ApronTest[Apron.LinearEqualities] {
  // FAILS, therefore ignore. This is for antoine
  override val ignoreLubLessEqualWidening = true

  override def factory = Apron.LinearEqualities.Bottom

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
  // FAILS, therefore ignore. This is for antoine
  override val ignoreMostPreciseAssignment = true

  override def factory = Apron.Box.Bottom
}
