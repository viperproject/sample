package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain

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
class InvalidAnd[T <: NumericalDomain[T]](a1:T)
  extends NumericWithInvalidDomain[T,BooleanInvalidDomainWithSource,InvalidAnd[T]](a1,new BooleanInvalidDomainWithSource()) {
  override def factory() = new InvalidAnd(a1.factory())
}

/**
 * A concrete variant of a domain for touch develop: Numerical + Strings-K-Sets with Invalid values.
 */
class StringsAnd[T <: NumericalDomain[T]](a1:T)
  extends NumericWithStringDomain[T,StringKSetDomain,NonrelationalStringDomain[StringKSetDomain],StringsAnd[T]](a1,new NonrelationalStringDomain[StringKSetDomain](new StringKSetDomain())) {
  override def factory() = new StringsAnd(a1.factory())
}