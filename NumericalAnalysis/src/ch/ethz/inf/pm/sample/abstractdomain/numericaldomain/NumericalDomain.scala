package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain
import ch.ethz.inf.pm.sample.abstractdomain._

/** Domain aimed at tracking numerical information */
trait NumericalDomain[T <: NumericalDomain[T]] extends SemanticDomain[T] { this: T =>

}
