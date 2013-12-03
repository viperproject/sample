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
class InvalidAnd[T <: NumericalDomain[T]](a1:T, a2:BooleanInvalidDomain = new BooleanInvalidDomain())
  extends NumericWithInvalidDomain[T,BooleanInvalidDomain,InvalidAnd[T]](a1,a2) {
  override def factory(a:T,b:BooleanInvalidDomain) = new InvalidAnd(a,b)
}

/**
 * A concrete variant of a domain for touch develop: Numerical + Strings-K-Sets with Invalid values.
 */
class StringsAnd[T <: NumericalDomain[T]](a1:T, a2:NonrelationalStringDomain[StringKSetDomain] = new NonrelationalStringDomain[StringKSetDomain](new StringKSetDomain()))
  extends NumericWithStringDomain[T,StringKSetDomain,NonrelationalStringDomain[StringKSetDomain],StringsAnd[T]](a1,a2) {
  override def factory(a:T,b:NonrelationalStringDomain[StringKSetDomain]) = new StringsAnd(a,b)
}