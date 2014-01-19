package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.ToStringUtilities
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression
import BooleanDomain._

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 10:50 AM
 * 
 */

trait InvalidDomain[T <: InvalidDomain[T]] extends SimplifiedSemanticDomain[T] { this: T => }

class BooleanInvalidDomain (val value:Map[Identifier, BooleanDomain] = Map.empty[Identifier, BooleanDomain],
                            val isBottom:Boolean = false,
                            val isTop:Boolean = false)
  extends BoxedDomain[BooleanDomain,BooleanInvalidDomain]
  with InvalidDomain[BooleanInvalidDomain] {

  def functionalFactory(_value:Map[Identifier, BooleanDomain] = Map.empty[Identifier, BooleanDomain],
                        _isBottom:Boolean = false,
                        _isTop:Boolean = false) : BooleanInvalidDomain =
    new BooleanInvalidDomain(_value,_isBottom,_isTop)

  def get(key : Identifier) : BooleanDomain = value.get(key) match {
    case None => domBottom
    case Some(x) => x
  }

  override def createVariable(variable: Identifier, typ: Type): BooleanInvalidDomain = return this

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
    val res = eval(expr)
    if (res.isBottom) bottom()
    else if (variable.representsSingleVariable) this.add(variable, res)
    else this.add(variable, get(variable).lub(res))
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
        if (l.canBeFalse && r.canBeFalse) domTop else domInvalid
      } else {
        if (l.canBeFalse && r.canBeFalse) domValid else domBottom
      }
    case BinaryNondeterministicExpression(left, right, _, typ) =>
      val l = eval(left)
      val r = eval(right)
      if (l.canBeTrue || r.canBeTrue) {
        if (l.canBeFalse && r.canBeFalse) domTop else domInvalid
      } else {
        if (l.canBeFalse && r.canBeFalse) domValid else domBottom
    }
    case AbstractOperator(left,List(right),Nil,AbstractOperatorIdentifiers.stringConcatenation,_) =>
      val l = eval(left)
      val r = eval(right)
      if (l.canBeTrue || r.canBeTrue) {
        if (l.canBeFalse && r.canBeFalse) domTop else domInvalid
      } else {
        if (l.canBeFalse && r.canBeFalse) domValid else domBottom
      }
    case InvalidExpression(typ, pp) => domInvalid
    case ValidExpression(typ, pp) => domValid
    case i: HeapIdentifier[_] =>
      this.get(i)
    case x: Identifier =>
      this.get(x)
    case xs: HeapIdSetDomain[_] =>
      var result = domBottom
      for (x <- xs.value) result = result.lub(this.get(x))
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
      if( (left == domInvalid && right == domValid) || (left == domValid && right == domInvalid)) bottom()
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
    case NegatedBooleanExpression(BinaryArithmeticExpression(xs:HeapIdSetDomain[_], InvalidExpression(_,_), ArithmeticOperator.==, _)) =>
      val res = domBottom.intersect(domValid,eval(xs))
      if (res.isBottom) bottom()
      else {
        var result = bottom()
        for (x <- xs.value) result = result.lub(add(x,res))
        result
      }
    case NegatedBooleanExpression(BinaryArithmeticExpression(x:Identifier, InvalidExpression(_,_), ArithmeticOperator.==, _)) =>
      val res = domBottom.intersect(domValid,eval(x))
      if (res.isBottom) bottom()
      else this.add(x,res)
    case NegatedBooleanExpression(BinaryArithmeticExpression(InvalidExpression(_,_), xs:HeapIdSetDomain[_], ArithmeticOperator.==, _)) =>
      val res = domBottom.intersect(domValid,eval(xs))
      if (res.isBottom) bottom()
      else {
        var result = bottom()
        for (x <- xs.value) result = result.lub(add(x,res))
        result
      }
    case NegatedBooleanExpression(BinaryArithmeticExpression(InvalidExpression(_,_), x:Identifier, ArithmeticOperator.==, _)) =>
      val res = domBottom.intersect(domValid,eval(x))
      if (res.isBottom) bottom()
      else this.add(x,res)
    case NegatedBooleanExpression(BinaryArithmeticExpression(InvalidExpression(_,_), InvalidExpression(_,_), ArithmeticOperator.==, _)) =>
      bottom()
    case BinaryBooleanExpression(left,right,op,typ) => op match {
      case BooleanOperator.&& => assume(left).assume(right)
      case BooleanOperator.|| => assume(left).lub(assume(right))
    }
    case _ => this
  }

  override def toString():String = {
    if (isBottom) return "_|_"
    var result : String = ""
    value.foreach { case (k,v) =>
      if(v.canBeTrue)
        if(v.canBeFalse)
          result += k.toString+" may be invalid\n"
        else
          result += k.toString+" is invalid\n"
      else
        if(v.canBeFalse)
          result += k.toString+" is valid\n"
        else
          result += k.toString+" is BOTTOM\n"
    }
    result
  }

}

abstract class NumericWithInvalidDomain[N <: NumericalDomain[N], I <: InvalidDomain[I], T <: NumericWithInvalidDomain[N,I,T]](_initialNumerical:N,_initialInvalid:I)
  extends SemanticCartesianProductDomain[N,I,T](_initialNumerical,_initialInvalid)
  with NumericalDomain[T] { this: T =>

  def numericalDomain : N = _1

  def invalidDomain : I = _2

  override def toString() = "Numeric:\n"+ToStringUtilities.indent(this._1.toString)+"\nInvalid:\n"+ToStringUtilities.indent(this._2.toString)

}

/**
 * Represents TouchDevelops "Invalid" value
 * @param typ Type of the invalid value
 * @param pp Program Point of the invalid value
 */
case class InvalidExpression(typ:Type, pp:ProgramPoint) extends Expression {
  def getType: Type = typ
  def getProgramPoint: ProgramPoint = pp
  def getIdentifiers: Set[Identifier] = Set.empty
  override def toString = "invalid"
  def transform(f: (Expression) => Expression): Expression = f(this)
}

/**
 * Represent an expression that is valid (no other information given, so essentially top minus invalid)
 * @param typ Type of the valid value
 * @param pp Program Point of the valid value
 */
case class ValidExpression(typ:Type, pp:ProgramPoint) extends Expression {
  def getType: Type = typ
  def getProgramPoint: ProgramPoint = pp
  def getIdentifiers: Set[Identifier] = Set.empty
  override def toString = "valid"
  def transform(f: (Expression) => Expression): Expression = f(this)
}