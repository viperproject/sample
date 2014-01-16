package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.abstractdomain.stringdomain._

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

class StringsAnd[T <: NumericalDomain[T],V <: StringValueDomain[V],S <: StringDomain[V,S]](a1:T, a2:S = new NonrelationalStringDomain[StringKSetDomain](new StringKSetDomain()).asInstanceOf[S])
  extends NumericWithStringDomain[T,V,S,StringsAnd[T,V,S]](a1,a2) {
  override def factory(a:T,b:S) = new StringsAnd(a,b)
}

/*
abstract class TouchStringsAnd[T <: NumericalDomain[T], V <: StringValueDomain[V], S <: StringDomain[V,S]] (a1:T, a2: S)
  extends NumericWithStringDomain[T,V,S,TouchStringsAnd[T,V,S]](a1,a2) {

}

/**
 * A concrete variant of a domain for touch develop: Numerical + Strings-K-Sets with Invalid values.
 */
class StringsAnd[T <: NumericalDomain[T]](a1:T, a2:NonrelationalStringDomain[StringKSetDomain] = new NonrelationalStringDomain[StringKSetDomain](new StringKSetDomain()))
  extends TouchStringsAnd[T,StringKSetDomain,NonrelationalStringDomain[StringKSetDomain]](a1,a2) {
  override def factory(a:T,b:NonrelationalStringDomain[StringKSetDomain]) = new StringsAnd(a,b)
}

/**
 * A concrete variant of a domain for touch develop: Numerical + Strings-Bricks with Invalid values.
 */
class BricksAnd[T <: NumericalDomain[T]] (a1:T, a2:Bricks = new Bricks())
  extends TouchStringsAnd[T,BricksDomain,Bricks](a1,a2) {
  override def factory(a:T,b:Bricks) = new BricksAnd(a,b)
}                           */
