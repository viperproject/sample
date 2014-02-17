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
}