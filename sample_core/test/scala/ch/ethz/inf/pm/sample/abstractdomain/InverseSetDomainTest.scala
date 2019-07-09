/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.test.SampleTest
import org.scalatest.{FunSuite, Matchers}

/** Tests `InverseSetDomain`. */
class InverseSetDomainTest extends FunSuite with Matchers with SampleTest {
  val dom = InvertedSetDomain.Default[Int]()
  val bottom = dom.bottom()
  val top = dom.top()
  val one = top.+(1)
  val two = top.+(2)
  val oneTwo = top.+(1).+(2)

  test("lessEqual") {
    assert(bottom.lessEqual(bottom))
    assert(bottom.lessEqual(top))
    assert(top.lessEqual(top))
    assert(!top.lessEqual(bottom))

    assert(!one.lessEqual(two))
    assert(!one.lessEqual(oneTwo))
    assert(oneTwo.lessEqual(one))
  }

  test("add") {
    assert(top.+(1) == one)
    assert(one.+(2) == oneTwo)
    assert(bottom.+(1) == bottom)
  }

  test("remove") {
    assert(one.-(1) == top)
    assert(oneTwo.-(1) == two)
    assert(bottom.-(1) == bottom)
    assert(top.-(1) == top)
  }
}