package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{NumericalDomain, BoxedNonRelationalNumericalDomain, NonRelationalNumericalDomain, Interval}
import ch.ethz.inf.pm.sample.abstractdomain.{VariableIdentifier, Identifier}
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.{ToStringUtilities, SystemParameters}

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

/**
 * Our standard identifiers for the environment
 *
 * THIS is not supposed to be a variable identifier, but it simplifies things
 */
class EnvironmentIdentifier(name:String,typ:Type) extends VariableIdentifier(name,typ,null) {
  //override def getName() = name
  //override def getField() : Option[String] = None
  //override def representSingleVariable() = true
  override def toString() = "Env."+name
}
