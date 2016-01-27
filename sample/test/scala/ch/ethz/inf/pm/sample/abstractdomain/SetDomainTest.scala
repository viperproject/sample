package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.test.LatticeTest

/**
  * An example of our test infrastructure, implemented for the set domain.
  */
class SetDomainTest extends LatticeTest[SetDomain.Default[Int]] {

  override lazy val factory = SetDomain.Default.Bottom[Int]()

  override lazy val instances: Set[SetDomain.Default[Int]] = super.instances ++ Set(
    SetDomain.Default.Inner(Set(1,2,9)),
    SetDomain.Default.Inner(Set(1,3,10))
  )

}
