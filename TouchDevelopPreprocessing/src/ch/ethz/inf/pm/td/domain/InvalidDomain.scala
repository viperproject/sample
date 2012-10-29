package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.ToStringUtilities
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 10:50 AM
 * 
 */

trait InvalidDomain[T <: InvalidDomain[T]] extends SimplifiedSemanticDomain[T] {

  def setInvalid(variable:Identifier): T
  def canBeInvalid(variable:Identifier):Boolean

}

class BooleanInvalidDomain
  extends BoxedDomain[BooleanDomain,BooleanInvalidDomain]
  with InvalidDomain[BooleanInvalidDomain] {

  // Helper values

  private val domBottom = (new BooleanDomain()).bottom()
  private val domTrue = (new BooleanDomain()).add(true)
  private val domFalse = (new BooleanDomain()).add(false)
  private val domTop = (new BooleanDomain()).top()

  override def factory() = new BooleanInvalidDomain

  def setInvalid(variable:Identifier): BooleanInvalidDomain = {
    this.remove(variable).add(variable,domTrue)
  }

  def canBeInvalid(variable:Identifier):Boolean = {
    get(variable).canBeTrue
  }

  def get(key : Identifier) : BooleanDomain = value.get(key) match {
    case None => domBottom
    case Some(x) => x
  }

  override def createVariable(variable: Identifier, typ: Type): BooleanInvalidDomain = {
    return this.add(variable, domBottom)
  }

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    var result = Map.empty[Identifier, List[String]]
    result = result + ((variable, path ::: variable.toString() :: Nil))
    (this.add(variable, domTop), result)
  }

  override def setToTop(variable: Identifier): BooleanInvalidDomain = {
    this.add(variable, domTop)
  }

  override def removeVariable(variable: Identifier): BooleanInvalidDomain = {
    this.remove(variable)
  }

  override def setArgument(variable: Identifier, expr: Expression): BooleanInvalidDomain = this.assign(variable, expr)

  override def assign(variable: Identifier, expr: Expression): BooleanInvalidDomain = {
    if (variable.representSingleVariable)
      this.add(variable, eval(expr))
    else this.add(variable, this.get(variable).lub(this.get(variable), eval(expr)))
  }

  override def backwardAssign(variable: Identifier, expr: Expression): BooleanInvalidDomain = this

  override def access(field: Identifier) = this

  override def backwardAccess(field: Identifier) = this

  /**
   * Evaluation for invalid values. The following rules apply:
   *
   * 1) For every arithmetic operation, the expression may be invalid if one of the two operands may be invalid.
   *    The expression must be invalid, if one of the two operands must be invalid.
   * 2) Invalid constant = invalid, all other constants = valid
   *
   */
  private def eval(expr: Expression): BooleanDomain = expr match {
    case BinaryArithmeticExpression(left, right, _, typ) =>
      val l = eval(left)
      val r = eval(right)
      if (l.canBeTrue || r.canBeTrue) {
        if (l.canBeFalse && r.canBeFalse) domTop else domTrue
      } else {
        if (l.canBeFalse && r.canBeFalse) domFalse else domBottom
      }
    case Constant(constant, typ, pp) =>
      if (constant == "invalid") domTrue else domFalse
    case x: Identifier =>
      this.get(x)
    case xs: HeapIdSetDomain[_] =>
      var result = domBottom
      for (x <- xs.value) result = result.lub(result, this.get(x))
      result
    case x: Expression => domTop
  }

  /**
   * We only implement the most necessary part of this (completing this should be quite hard)
   * E ::= x == invalid | x != invalid | E and E | E or E
   *
   * TODO: We would have to convert this to some normal Form
   * TODO: Case x == y, x != y
   */
  override def assume(expr: Expression): BooleanInvalidDomain = expr match {
    case BinaryArithmeticExpression(a:Expression, b:Expression, ArithmeticOperator.==, _) =>
      val left = eval(a)
      val right = eval(b)

      // Does it evaluate to false? then set everything to bottom
      if( (left == domTrue && right == domFalse) || (left == domFalse && right == domTrue)) bottom()
      else (a,b) match {
        // Cases x = y, x = something, something = x
        case (x:Identifier, y:Identifier) =>
          this.add(x,domBottom.intersect(left,right)).add(y,domBottom.intersect(left,right))
        case (x:Identifier, y:Expression) =>
          this.add(x,domBottom.intersect(left,eval(y)))
        case (y:Expression, x:Identifier) =>
          this.add(x,domBottom.intersect(right,eval(y)))
        case _ => this
      }

    case NegatedBooleanExpression(BinaryArithmeticExpression(x:Identifier, Constant("invalid",_,_), ArithmeticOperator.==, _)) =>
      // Case x != invalid
      this.add(x,domBottom.intersect(domFalse,eval(x)))
    case NegatedBooleanExpression(BinaryArithmeticExpression(Constant("invalid",_,_), x:Identifier, ArithmeticOperator.==, _)) =>
      // Case invalid != x
      this.add(x,domBottom.intersect(domTrue,eval(x)))
    case BinaryBooleanExpression(left,right,op,typ) => op match {
      case BooleanOperator.&& => this.assume(left).assume(right)
      case BooleanOperator.|| => this.lub(this.assume(left),this.assume(right))
    }
    case _ => this
  }

}

abstract class NumericWithInvalidDomain[N <: NumericalDomain[N], I <: InvalidDomain[I], T <: NumericWithInvalidDomain[N,I,T]](var num:N,var inv:I)
  extends SemanticCartesianProductDomain[N,I,T](num,inv)
  with InvalidDomain[T]
  with NumericalDomain[T] {

  override def setInvalid(variable:Identifier):T = {
    val result : T = this.factory()
    result.d2=result.d2.setInvalid(variable)
    result
  }

  override def canBeInvalid(variable:Identifier): Boolean = {
    d2.canBeInvalid(variable)
  }

  override def toString() = "Numeric:\n"+ToStringUtilities.indent(d1.toString())+"\nInvalid:\n"+ToStringUtilities.indent(d2.toString)
}