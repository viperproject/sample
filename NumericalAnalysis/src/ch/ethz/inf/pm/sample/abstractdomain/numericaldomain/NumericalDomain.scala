package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._

/** Domain aimed at tracking numerical information */
trait NumericalDomain[T <: NumericalDomain[T]] extends SemanticDomain[T] {
  this: T =>

  /** Returns all the knowledge we have on the given identifiers as an expression */
  def getConstraints(ids: Set[Identifier]): Set[Expression] = ???

}
