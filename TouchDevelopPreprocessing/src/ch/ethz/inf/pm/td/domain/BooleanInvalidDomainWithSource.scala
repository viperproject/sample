package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import scala.Some
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression
import PositionedInvalidValueDomain._

/**
 * 
 * Lucas Brutschy
 * Date: 10/18/12
 * Time: 10:50 AM
 * 
 */
class BooleanInvalidDomainWithSource
  extends BoxedDomain[PositionedInvalidValueDomain,BooleanInvalidDomainWithSource]
  with InvalidDomain[BooleanInvalidDomainWithSource] {

  override def factory() = new BooleanInvalidDomainWithSource

  def get(key : Identifier) : PositionedInvalidValueDomain = value.get(key) match {
    case None => domBottom
    case Some(x) => x
  }

  override def createVariable(variable: Identifier, typ: Type): BooleanInvalidDomainWithSource = {
    this
  }

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    var result = Map.empty[Identifier, List[String]]
    result = result + ((variable, path ::: variable.toString :: Nil))
    (this.add(variable, domTop), result)
  }

  override def setToTop(variable: Identifier): BooleanInvalidDomainWithSource = {
    this.add(variable, domTop)
  }

  override def removeVariable(variable: Identifier): BooleanInvalidDomainWithSource = {
    this.remove(variable)
  }

  override def setArgument(variable: Identifier, expr: Expression): BooleanInvalidDomainWithSource = {
    this.assign(variable, expr)
  }

  override def assign(variable: Identifier, expr: Expression): BooleanInvalidDomainWithSource = {
    val res = eval(expr)
    if (!res.isTop && (res.isBottom || res.value.isEmpty))
      bottom()
    else if (variable.representSingleVariable()) this.add(variable, res)
    else this.add(variable, domBottom.lub(this.get(variable), res))
  }

  override def backwardAssign(variable: Identifier, expr: Expression): BooleanInvalidDomainWithSource = this

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
  private def eval(expr: Expression): PositionedInvalidValueDomain = expr match {
    case BinaryArithmeticExpression(left, right, _, typ) => domBottom.lub(eval(left),eval(right))
    case BinaryNondeterministicExpression(left, right, _, typ) => domBottom.lub(eval(left),eval(right))
    case Constant("invalid", typ, pp) => domInvalid(pp)
    case Constant(_, _, _) => domValid
    case x: Identifier => this.get(x)
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
  override def assume(expr: Expression): BooleanInvalidDomainWithSource = {
    val res = expr match {
      case BinaryArithmeticExpression(a:Expression, b:Expression, ArithmeticOperator.==, _) =>

        val left = eval(a)
        val right = eval(b)
        if ((left.mustBeValid && right.mustBeInvalid) || (left.mustBeInvalid && right.mustBeValid))
          return bottom()
        var cur = this
        a match {
          case x:Identifier => cur = cur.add(x,left.onlyIf(right))
          case xs: HeapIdSetDomain[_] => for (x <- xs.value) cur = cur.add(x, left.onlyIf(right))
          case _ => ()
        }
        b match {
          case x:Identifier => cur = cur.add(x,right.onlyIf(left))
          case xs: HeapIdSetDomain[_] => for (x <- xs.value) cur = cur.add(x, right.onlyIf(left))
          case _ => ()
        }
        cur

      case NegatedBooleanExpression(BinaryArithmeticExpression(a,b, ArithmeticOperator.==, _)) =>

        val left = eval(a)
        val right = eval(b)
        if (left.mustBeInvalid && right.mustBeInvalid)
          return bottom()
        var cur = this
        if (right.mustBeInvalid && left.canBeInvalid) {
          a match {
            case x:Identifier => cur = cur.add(x,left.onlyIf(domValid))
            case xs: HeapIdSetDomain[_] => for (x <- xs.value) cur = cur.add(x, left.onlyIf(domValid))
            case _ => ()
          }
        }
        if (left.mustBeInvalid && right.canBeInvalid) {
          b match {
            case x:Identifier => cur = cur.add(x,right.onlyIf(domValid))
            case xs: HeapIdSetDomain[_] => for (x <- xs.value) cur = cur.add(x, right.onlyIf(domValid))
            case _ => ()
          }
        }
        cur

      case BinaryBooleanExpression(left,right,op,typ) => op match {
        case BooleanOperator.&& => this.assume(left).assume(right)
        case BooleanOperator.|| => this.lub(this.assume(left),this.assume(right))
      }
      case _ => this
    }

    res
  }

  override def toString():String = {
    if (isBottom) return "_|_"
    var result : String = ""
    value.foreach { case (k,v) =>
      if(v.canBeInvalid)
        if(v.canBeValid)
          result += k.toString+" may be invalid\n"
        else
          result += k.toString+" is invalid\n"
      else
        if(v.canBeValid)
          result += k.toString+" is valid\n"
        else
          result += k.toString+" is BOTTOM\n"
    }
    result
  }

}


object PositionedInvalidValueDomain {

  // Helper values
  def domBottom = new PositionedInvalidValueDomain().bottom()
  def domInvalid(pp:ProgramPoint) = new PositionedInvalidValueDomain().add(Invalid(pp))
  def domValid = new PositionedInvalidValueDomain().add(Valid())
  def domTop = new PositionedInvalidValueDomain().top()

}

/**
 *
 * Attaches a position to every invalid value
 *
 * Lucas Brutschy
 * Date: 06/09/13
 * Time: 10:49 AM
 *
 */
class PositionedInvalidValueDomain extends SetDomain[InvalidValue,PositionedInvalidValueDomain] {

  override def factory() = new PositionedInvalidValueDomain()
  def canBeInvalid = isTop || value.exists { case Invalid(_) => true; case _ => false }
  def canBeValid = isTop || value.exists { case Valid() => true; case _ => false }
  def mustBeInvalid = canBeInvalid && !canBeValid
  def mustBeValid = canBeValid && !canBeInvalid

  // intersection corresponds to glb
  def onlyIf(x : PositionedInvalidValueDomain):PositionedInvalidValueDomain = {
    if (x.isTop) return this
    if (this.isTop) return this
    val res = new PositionedInvalidValueDomain()
    if (x.canBeValid) res.value = res.value ++ this.value.collect { case x@Valid() => x }
    if (x.canBeInvalid) res.value = res.value ++ this.value.collect { case x@Invalid(src) => x }
    if (res.value.isEmpty) return bottom()
    res
  }

}

trait InvalidValue
case class Valid() extends InvalidValue
case class Invalid(source:ProgramPoint) extends InvalidValue