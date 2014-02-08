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
case class InvalidAnd[T <: NumericalDomain[T]](
    _1: T,
    _2: BooleanInvalidDomainWithSource = new BooleanInvalidDomainWithSource())
  extends NumericWithInvalidDomain[T, BooleanInvalidDomainWithSource, InvalidAnd[T]] {
  override def factory(a:T,b:BooleanInvalidDomainWithSource) = new InvalidAnd(a,b)
}

case class StringsAnd[
    T <: NumericalDomain[T],
    V <: StringValueDomain[V],
    S <: StringDomain[V,S]](
    _1: T,
    _2: S = new NonrelationalStringDomain[StringKSetDomain](
      new StringKSetDomain(TouchAnalysisParameters.stringRepresentationBound)).asInstanceOf[S])
  extends NumericWithStringDomain[T, V, S, StringsAnd[T, V, S]] {
  override def factory(a:T,b:S) = new StringsAnd(a,b)
}
