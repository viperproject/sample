package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._

trait RelationalNumericalDomain[T <: RelationalNumericalDomain[T]] extends NumericalDomain[T] {
	def getStringOfId (id : Identifer) : String ;
	def setToTop(variable : Identifer) : T;
	def assign (variable : Identifer, expr : Expression) : T;
	def assume(expr : Expression) : T;
	def createVariable (variable : Identifer, typ : Type) : T = this.setToTop(variable);
	def removeVariable(variable : Identifer) : T = this.setToTop(variable);
}