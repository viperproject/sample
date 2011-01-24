package ch.ethz.inf.pm.sample.abstractdomain.typedomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

trait TypeDomain[TD <: TypeDomain[TD]] extends Lattice[TD] {
  def isObject(variable : Identifier) : Boolean;
  def isNumericalVariable(variable : Identifier) : Boolean;
  def assume(expr : Expression) : TD;
  def assign(variable : Identifier, expr : Expression) : TD;
  def declareVariable(variable : Identifier, typ : Type) : TD;
  def removeVariable(variable : Identifier) : TD;
  def getType(variable : Identifier) : Type;
}