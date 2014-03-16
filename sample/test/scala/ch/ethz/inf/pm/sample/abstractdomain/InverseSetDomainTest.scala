package ch.ethz.inf.pm.sample.abstractdomain

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/** Tests `InverseSetDomain`. */
class InverseSetDomainTest extends FunSuite with ShouldMatchers {
  val dom = InverseSetDomain.Default[Int]()
  val bottom = dom.bottom()
  val top = dom.top()
  val one = top.add(1)
  val two = top.add(2)
  val oneTwo = top.add(1).add(2)

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
    assert(top.add(1) == one)
    assert(one.add(2) == oneTwo)
    assert(bottom.add(1) == bottom)
  }

  test("remove") {
    assert(one.remove(1) == top)
    assert(oneTwo.remove(1) == two)
    assert(bottom.remove(1) == bottom)
    assert(top.remove(1) == top)
  }
}