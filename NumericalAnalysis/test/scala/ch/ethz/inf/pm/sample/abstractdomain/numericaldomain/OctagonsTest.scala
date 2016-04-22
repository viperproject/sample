package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.test.LatticeTest

trait RelationalNumericalDomainTest[T <: NumericalDomain[T]]
  extends LatticeTest[T] {

}

class OctagonsTest
  extends RelationalNumericalDomainTest[Octagons] {

  override def factory: Octagons = Octagons.Bottom
}
