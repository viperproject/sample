package ch.ethz.inf.pm.sample.abstractdomain

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers


/** Tests `InverseSetDomain`. */
class InverseSetDomainTest extends FunSuite with ShouldMatchers {
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