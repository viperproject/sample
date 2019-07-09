/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{Constant, SemanticDomain, TypeMap}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyIntegerType, DummyProgramPoint, DummyTypeMap}
import ch.ethz.inf.pm.sample.test.{LatticeTest, SemanticDomainTest}


trait NumericalDomainTest[T <: NumericalDomain[T]] extends SemanticDomainTest[T] {

  override lazy val typ = SystemParameters.tm.Int

  override def values = super.values ++ Set(
    Constant("0", typ)(),
    Constant("2", typ)(),
    Constant("-2", typ)(),
    Constant(Int.MaxValue.toString, typ)(),
    Constant(Int.MinValue.toString, typ)()
  )

}

// Not necessarily satisfied by all domains, e.g. Sign
trait MostPreciseAssignment[T <: SemanticDomain[T]] extends SemanticDomainTest[T] {

  val ignoreMostPreciseAssignment = false
  test("Most precise assignment of variables") {
    if (!ignoreMostPreciseAssignment)
      for (a1 <- values; a2 <- values) {
        if (a1 != a2) {
          import ch.ethz.inf.pm.sample.abstractdomain.ExpressionFactory._
          implicit val pp = DummyProgramPoint

          val x = factory.createVariable(v1).createVariable(v2).assign(v1, a1).assign(v2, a2)
          assert(x.assume(v1 unequal a1).isBottom)
          assert(x.assume(v2 unequal a2).isBottom)
          assert(x.assume(v1 unequal v1).isBottom)
          assert(x.assume(v2 unequal v2).isBottom)
        }
      }
  }

}

trait NonRelationalNumericalDomainTest[T <: NonRelationalNumericalDomain[T]] extends LatticeTest[T] {

  lazy val two = instances.head.evalConstant(2)
  lazy val four = instances.head.evalConstant(4)


  test("2 + 2 = 4") {
    assert {
      !((two sum two) glb four).isBottom
    }
  }

  test("2 * 2 = 4") {
    assert {
      !((two multiply two) glb four).isBottom
    }
  }

}


class IntegerIntervalTest extends NonRelationalNumericalDomainTest[IntegerInterval] {

  override lazy val factory = IntegerInterval.Bottom

  override lazy val instances: Set[IntegerInterval] = super.instances ++ Set(
    IntegerInterval.Inner(-1, Int.MaxValue),
    IntegerInterval.Inner(Int.MinValue, 1),
    IntegerInterval.Inner(0, 0)
  )

  test("issue88") {
    val left = IntegerInterval.Inner(-1, -1)
    val right = IntegerInterval.Inner(Int.MinValue, 0)
    val expected = IntegerInterval.Inner(0, Int.MaxValue)
    val result = left multiply right
    assert {
      result equivalent expected
    }
  }

  test("[10, 20] / [-2, 2] = [-20, 20]") {
    val a = IntegerInterval.Inner(10, 20)
    val b = IntegerInterval.Inner(-2, 2)
    val low = instances.head.evalConstant(-20)
    val high = instances.head.evalConstant(20)
    assert {
      !((a divide b) glb low).isBottom
      !((a divide b) glb high).isBottom
    }
  }
}


class DoubleIntervalTest extends NonRelationalNumericalDomainTest[DoubleInterval] {

  override lazy val instances: Set[DoubleInterval] = super.instances ++ Set(
    DoubleInterval.Inner(-1, Double.PositiveInfinity),
    DoubleInterval.Inner(Double.NegativeInfinity, 1),
    DoubleInterval.Inner(0, 0)
  )

  override def factory: DoubleInterval = DoubleInterval.Bottom

  test("[10, 20] / [-2, 2] = Top") {
    val a = DoubleInterval.Inner(10, 20)
    val b = DoubleInterval.Inner(-2, 2)
    assert {
      (a divide b).isTop
    }
  }
}


class SignTest extends NonRelationalNumericalDomainTest[Sign] {

  override lazy val factory = Sign.Bottom

  override lazy val instances: Set[Sign] = super.instances ++ Set(
    Sign.Minus,
    Sign.Plus,
    Sign.Zero
  )

}

trait BoxedNonRelationalNumericalDomainTest[T <: NonRelationalNumericalDomain[T]]
  extends NumericalDomainTest[BoxedNonRelationalNumericalDomain[T]] {

  override def factory = BoxedNonRelationalNumericalDomain[T](domain)

  def domain: T

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