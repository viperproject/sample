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

/*
sealed abstract class ArithmeticOperator
case object + extends ArithmeticOperator
case object - extends ArithmeticOperator
case object * extends ArithmeticOperator
case object / extends ArithmeticOperator

sealed abstract class ConditionalOperator
case object >= extends ConditionalOperator
case object <= extends ConditionalOperator
case object > extends ConditionalOperator
case object < extends ConditionalOperator
case object == extends ConditionalOperator
case object != extends ConditionalOperator

sealed abstract class ArithmeticExpression
case class BinaryArithmeticExpression(left : ArithmeticExpression, right : ArithmeticExpression, op : ArithmeticOperator) extends ArithmeticExpression
case class Variable(id : Identifier) extends ArithmeticExpression
case class Integer(value : Int)  extends ArithmeticExpression

sealed abstract class ConditionalExpression
case class Comparison(left : ArithmeticExpression, right : ArithmeticExpression, op : ConditionalOperator) extends ConditionalExpression
case class Negate(c : ConditionalExpression) extends ConditionalExpression
case class Bool(value : Boolean)  extends ConditionalExpression
*/
/*class RelationalNumericalDomain[T <: RelationalNumericalDomain[T]] extends SimplifiedSemanticDomain[T] {
def getStringOfId ( id : Identifer ) : String ;
def setToTop(variable : Identifer) : T;
def assign ( variable : Identifer, expr : Expression ) : T;
def assume(expr : Expression ) : T;
def createVariable ( variable : Identifer , typ : Type) : T;
def removeVariable( variable : Identifer ) : T;
}*/