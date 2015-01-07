package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._

/** Domain aimed at tracking numerical information */
trait NumericalDomain[T <: NumericalDomain[T]] extends SemanticDomain[T] {
  this: T =>

  /** Returns all the knowledge we have on the given identifiers as an expression */
  def getConstraints(ids: Set[Identifier]): Set[Expression] = ???

}

trait RelationalNumericalDomain[T <: RelationalNumericalDomain[T]]
  extends SimplifiedSemanticDomain[T]
  with NumericalDomain[T] {
  this: T =>

}

trait NumericalDomainWrapper[X <: NumericalDomain[X], T <: NumericalDomainWrapper[X,T]]
  extends NumericalDomain[T]
  with SemanticDomainWrapper[X,T] {
  self:T =>

}

trait RelationalNumericalDomainWrapper[X <: RelationalNumericalDomain[X], T <: RelationalNumericalDomainWrapper[X,T]]
  extends RelationalNumericalDomain[T]
  with NumericalDomainWrapper[X,T] {
  self:T =>

}