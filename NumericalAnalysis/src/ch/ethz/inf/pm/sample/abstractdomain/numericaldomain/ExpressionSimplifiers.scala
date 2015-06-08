package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Normalizer.Monomial

/**
 *
 * Extend this if you are too lazy to handle all kinds of weird expressions and
 * instead want to handle only good old monomials
 *
 * @author Lucas Brutschy
 */
trait BooleanExpressionSimplifier[T <: SemanticDomain[T]] extends SemanticDomain[T] {
  this:T =>

  def assumeSimplified(expression: Expression): T

  /**
   * This method assumes that a given expression hold
   *
   * @param expr the expression to be assumed
   * @return the state after this action
   */
  override def assume(expr: Expression): T = expr match {

    // This must be first -- Shortcut in simplified version
    case b@BinaryArithmeticExpression(left, right, op, typ) if !left.typ.isBooleanType && !right.typ.isBooleanType =>
      assumeSimplified(b)

    // Boolean constants
    case Constant("true",_,_) => this
    case Constant("false",_,_) => this.bottom()
    case NegatedBooleanExpression(Constant("true",_,_)) => this.bottom()
    case NegatedBooleanExpression(Constant("false",_,_)) => this
    case BinaryArithmeticExpression(Constant(a,_,_),Constant(b,_,_),ArithmeticOperator.==,_) if a == b =>
      this
    case BinaryArithmeticExpression(Constant(a,_,_),Constant(b,_,_),ArithmeticOperator.!=,_) if a == b =>
      bottom()
    case BinaryArithmeticExpression(Constant("true",_,_),Constant("false",_,_),ArithmeticOperator.==,_)=>
      bottom()
    case BinaryArithmeticExpression(Constant("false",_,_),Constant("true",_,_),ArithmeticOperator.==,_) =>
      bottom()
    case BinaryArithmeticExpression(Constant("true",_,_),Constant("false",_,_),ArithmeticOperator.!=,_) =>
      this
    case BinaryArithmeticExpression(Constant("false",_,_),Constant("true",_,_),ArithmeticOperator.!=,_) =>
      this

    // Boolean variables
    case x: Identifier =>
      if (SystemParameters.DEBUG) assert(x.typ.isBooleanType)
      val res = assume(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.!=))
      res

    case NegatedBooleanExpression(x: Identifier) =>
      if (SystemParameters.DEBUG) assert(x.typ.isBooleanType)
      val res = assume(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.==))
      res

    // And and Or
    case BinaryBooleanExpression(left, right, op, _) => op match {
      case BooleanOperator.&& => assume(left).assume(right)
      case BooleanOperator.|| =>
        val l = assume(left)
        val r = assume(right)
        val res = l lub r
        res
    }

    // Double-Negation + De-Morgan
    case NegatedBooleanExpression(NegatedBooleanExpression(x)) =>
      assume(x)

    case NegatedBooleanExpression(BinaryBooleanExpression(left, right, op, typ)) =>
      val nl = NegatedBooleanExpression(left)
      val nr = NegatedBooleanExpression(right)
      val nop = op match {
        case BooleanOperator.&& => BooleanOperator.||
        case BooleanOperator.|| => BooleanOperator.&&
      }
      assume(BinaryBooleanExpression(nl, nr, nop, typ))

    // Inverting of operators
    case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
      val res = assume(BinaryArithmeticExpression(left,right,ArithmeticOperator.negate(op),typ))
      res

    // Handling of monomes
    case _ => assumeSimplified(expr)

  }

}

  /**
 *
 * Extend this if you are too lazy to handle all kinds of weird expressions and
 * instead want to handle only good old monomials
 *
 * @author Lucas Brutschy
 */
trait MonomialExpressionSimplifier[T <: SemanticDomain[T]] extends BooleanExpressionSimplifier[T] {
    this: T =>

    def assumeMonomes(tuple: Monomial): T

    def assumeNonMonomes(expression: Expression): T

    /**
     * This method assumes that a given expression hold
     *
     * @param expr the expression to be assumed
     * @return the state after this action
     */
    override def assumeSimplified(expr: Expression): T = expr match {

      // Handling of monomes
      case _ => Normalizer.conditionalExpressionToMonomial(expr) match {
        case Some(x) => assumeMonomes(x)
        case None => assumeNonMonomes(expr)
      }

    }

  }