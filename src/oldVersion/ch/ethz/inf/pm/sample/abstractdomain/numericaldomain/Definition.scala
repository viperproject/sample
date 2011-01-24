package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
/** 
 * A <code>NumericalDomain</code> is a domain aimed at tracking
 * numerical information
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
trait NumericalDomain[T <: NumericalDomain[T]] extends SimplifiedSemanticDomain[T]
