package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain._
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

/**
 *
 * Defines the abstract domains specific to TouchDevelops
 *
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 10:54 AM
 * 
 */

/**
 * A concrete variant of a domain for touch develop: Numerical with Invalid values.
 */
class InvalidAnd[T <: NumericalDomain[T]](a1:T, a2:BooleanInvalidDomainWithSource = new BooleanInvalidDomainWithSource())
  extends NumericWithInvalidDomain[T,BooleanInvalidDomainWithSource,InvalidAnd[T]](a1,a2) {
  override def factory(a:T,b:BooleanInvalidDomainWithSource) = new InvalidAnd(a,b)
}

class StringsAnd[T <: NumericalDomain[T],V <: StringValueDomain[V],S <: StringDomain[V,S]](a1:T, a2:S = new NonrelationalStringDomain[StringKSetDomain](new StringKSetDomain(TouchAnalysisParameters.stringRepresentationBound)).asInstanceOf[S])
  extends NumericWithStringDomain[T,V,S,StringsAnd[T,V,S]](a1,a2) {
  override def factory(a:T,b:S) = new StringsAnd(a,b)
}
