package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.oorepresentation.Type

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
 * A concrete variant of a domain for touch develop: Environment + Intervals with Invalid values.
 */
class TouchDomain[T <: NumericalDomain[T]](a1:T)
  extends NumericWithInvalidDomain[T,BooleanInvalidDomain,TouchDomain[T]](a1,new BooleanInvalidDomain()) {
  override def factory() = new TouchDomain(a1.factory())
}