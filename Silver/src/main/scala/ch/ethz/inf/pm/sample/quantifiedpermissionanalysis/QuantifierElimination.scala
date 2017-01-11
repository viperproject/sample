/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{BoolType, IntType}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.ExpressionBuilder._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils._

/**
  * @author Severin Münger
  *         Added on 10.01.17.
  */
object QuantifierElimination {

  def eliminate(variable: VariableIdentifier, expr: Expression): Expression = {
    println(expr)
    val formulaNNF = toNNF(expr)
    println(formulaNNF)
    val tzEquivalentFormula = toTzEquivalentFormula(formulaNNF)
    println(tzEquivalentFormula)
    val collected = collectVariable(variable, tzEquivalentFormula)
    println(collected)
    val (lcmReplaced, freshVariable) = replaceLCM(variable, collected)
    println(lcmReplaced)
    val equivalentFormula = constructEquivalence(freshVariable, lcmReplaced)
    println(equivalentFormula)
    equivalentFormula
  }

  // Step 1
  private def toNNF(expr: Expression): Expression = Utils.toNNF(expr)

  // Step 2
  private def toTzEquivalentFormula(expr: Expression): Expression = expr.transform {
    case BinaryArithmeticExpression(BinaryArithmeticExpression(left: VariableIdentifier, Constant(const, IntType, _), ArithmeticOperator.%), `zeroConst`, ArithmeticOperator.==) => Divides(const.toInt, left)
    case BinaryArithmeticExpression(`zeroConst`, BinaryArithmeticExpression(left: VariableIdentifier, Constant(const, IntType, _), ArithmeticOperator.%), ArithmeticOperator.==) => Divides(const.toInt, left)
    case BinaryArithmeticExpression(BinaryArithmeticExpression(left: VariableIdentifier, Constant(const, IntType, _), ArithmeticOperator.%), `zeroConst`, ArithmeticOperator.!=) => NotDivides(const.toInt, left)
    case BinaryArithmeticExpression(`zeroConst`, BinaryArithmeticExpression(left: VariableIdentifier, Constant(const, IntType, _), ArithmeticOperator.%), ArithmeticOperator.!=) => NotDivides(const.toInt, left)
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.==) => and(lt(left, plusOne(right)), lt(right, plusOne(left)))
    case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, ArithmeticOperator.!=)) => and(lt(left, plusOne(right)), lt(right, plusOne(left)))
    case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, ArithmeticOperator.==)) => or(lt(left, right), lt(right, left))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.!=) => or(lt(left, right), lt(right, left))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.<=) => lt(left, plusOne(right))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.>=) => lt(right, plusOne(left))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.>) => lt(right, left)
    case other => other
  } match {
    case transformed if transformed == expr => transformed
    case transformed => toTzEquivalentFormula(transformed)
  }

  // Step 3
  private def collectVariable(variable: VariableIdentifier, expr: Expression): Expression = expr match {
    case BinaryBooleanExpression(left, right, op) => BinaryBooleanExpression(collectVariable(variable, left), collectVariable(variable, right), op)
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.<) =>
      val collectedVariables = binOp(collect(left), collect(right), _ - _)
      val mapping: ((Any, Int)) => Expression = {
        case (_, 0) => const(0)
        case (key: VariableIdentifier, value) => VariableIdentifierWithFactor(value, key)
        case (ConstPlaceholder, value) => const(value)
      }
      val varFactor = collectedVariables.getOrElse(variable, 0)
      if (varFactor >= 0)
        LessThanWithVariableLeft(VariableIdentifierWithFactor(varFactor, variable), unOp(collectedVariables - variable, - _).map(mapping).reduce(plus))
      else
        LessThanWithVariableRight((collectedVariables - variable).map(mapping).reduce(plus), VariableIdentifierWithFactor(-varFactor, variable))
  }

  private def collect(expr: Expression): Map[Any, Int] = expr match {
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.+) => binOp(collect(left), collect(right), _ + _)
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.-) => binOp(collect(left), collect(right), _ - _)
    case BinaryArithmeticExpression(Constant(const, IntType, _), other, ArithmeticOperator.*) => unOp(collect(other), const.toInt * _)
    case BinaryArithmeticExpression(other, Constant(const, IntType, _), ArithmeticOperator.*) => unOp(collect(other), const.toInt * _)
    case UnaryArithmeticExpression(left, ArithmeticOperator.-, _) => unOp(collect(left), - _)
    case UnaryArithmeticExpression(left, ArithmeticOperator.+, _) => collect(left)
    case Constant(const, IntType, _) => Map((ConstPlaceholder, const.toInt))
    case v: VariableIdentifier => Map((v, 1))
  }

  private def binOp(a: Map[Any, Int], b: Map[Any, Int], op: (Int, Int) => Int): Map[Any, Int] = a ++ b.transform((key, value) => op(a.getOrElse(key, 0), value))

  private def unOp(a: Map[Any, Int], op: (Int) => Int): Map[Any, Int] = a.transform((_, value) => op(value))

  private case object ConstPlaceholder

  // Step 4
  private def replaceLCM(variable: VariableIdentifier, expr: Expression): (Expression, VariableIdentifier) = {
    var numbers: Set[Int] = Set()
    expr.foreach {
      case VariableIdentifierWithFactor(_, `variable`) => throw new IllegalStateException()
      case LessThanWithVariableLeft(VariableIdentifierWithFactor(factor, `variable`), _) => numbers += factor.abs
      case LessThanWithVariableRight(_, VariableIdentifierWithFactor(factor, `variable`)) => numbers += factor.abs
      case _ =>
    }
    val leastCommonMultiple = lcm(numbers)
    val coefficientMultiplier: (Int) => ((Expression) => Expression) = (hPrime) => {
      case VariableIdentifierWithFactor(h, variableIdentifier) => VariableIdentifierWithFactor(h * hPrime, variableIdentifier)
      case Constant(const, IntType, pp) => Constant((const.toInt * hPrime).toString, IntType, pp)
      case other => other
    }
    val freshVariable = VariableIdentifier(Context.createNewUniqueVarIdentifier("fresh", markAsTaken = false))(IntType)
    val replacedLCM = expr.transform {
      case VariableIdentifierWithFactor(_, `variable`) => throw new IllegalStateException()
      case LessThanWithVariableLeft(VariableIdentifierWithFactor(factor, `variable`), right) =>
        val hPrime = leastCommonMultiple / factor
        LessThanWithVariableLeft(VariableIdentifierWithFactor(1, freshVariable), right.transform(coefficientMultiplier(hPrime)))
      case LessThanWithVariableRight(left, VariableIdentifierWithFactor(factor, `variable`)) =>
        val h = leastCommonMultiple / factor
        LessThanWithVariableRight(left.transform(coefficientMultiplier(h)), VariableIdentifierWithFactor(1, freshVariable))
      case other => other
    }
    if (leastCommonMultiple != 1) (and(replacedLCM, Divides(leastCommonMultiple, freshVariable)), freshVariable)
    else (replacedLCM, freshVariable)
  }

  // Step 5
  private def constructEquivalence(freshVariable: VariableIdentifier, expr: Expression): Expression = {
    val leftProjection = simplifyExpression(leftInfiniteProjection(freshVariable, expr))
    val d = delta(freshVariable, expr)
    val B = getBs(freshVariable, expr)
    simplifyExpression(((1 to d).map(j => leftProjection.transform {
      case LessThanWithVariableRight(_, VariableIdentifierWithFactor(1, `freshVariable`)) |
           LessThanWithVariableLeft(VariableIdentifierWithFactor(1, `freshVariable`), _) => throw new IllegalStateException()
      case Divides(n, `freshVariable`) => Divides(n, const(j))
      case NotDivides(n, `freshVariable`) => NotDivides(n, const(j))
      case other => other
    }) ++ (1 to d).flatMap(j => B.map(b => expr.transform {
      case LessThanWithVariableRight(left, VariableIdentifierWithFactor(1, `freshVariable`)) => lt(left, plus(b, const(j)))
      case LessThanWithVariableLeft(VariableIdentifierWithFactor(1, `freshVariable`), right) => lt(plus(b, const(j)), right)
      case Divides(n, `freshVariable`) => Divides(n, plus(b, const(j)))
      case NotDivides(n, `freshVariable`) => NotDivides(n, plus(b, const(j)))
      case other => other
    }))).reduce(or))
  }

  private def delta(freshVariable: VariableIdentifier, expr: Expression): Int = {
    var numbers: Set[Int] = Set()
    expr.foreach {
      case div: DivideExpression => numbers += div.left
      case _ =>
    }
    if (numbers.isEmpty) 1 else lcm(numbers)
  }

  private def getBs(freshVariable: VariableIdentifier, expr: Expression): Set[Expression] = {
    var bs: Set[Expression] = Set()
    expr.foreach {
      case LessThanWithVariableRight(left, VariableIdentifierWithFactor(1, `freshVariable`)) => bs += left
      case LessThanWithVariableRight(VariableIdentifierWithFactor(1, `freshVariable`), right) => bs += right
      case _ =>
    }
    bs
  }

  private def leftInfiniteProjection(variable: VariableIdentifier, expr: Expression): Expression = expr.transform {
    case LessThanWithVariableLeft(VariableIdentifierWithFactor(1, `variable`), _) => trueConst
    case LessThanWithVariableRight(_, VariableIdentifierWithFactor(1, `variable`)) => falseConst
    case other => other
  }

}

case class VariableIdentifierWithFactor(factor: Int, variableIdentifier: VariableIdentifier) extends Expression {
  override def typ: Type = variableIdentifier.typ
  override def pp: ProgramPoint = variableIdentifier.pp
  override def ids: IdentifierSet = variableIdentifier.ids
  override def transform(f: (Expression) => Expression): Expression = f(this)
  override def contains(f: (Expression) => Boolean): Boolean = f(this) || f(variableIdentifier)
  override def toString: String = factor match {
    case 0 => "0"
    case 1 => variableIdentifier.toString
    case -1 => "-" + variableIdentifier.toString
    case other => other + variableIdentifier.toString
  }
}

case class LessThanWithVariableLeft(left: VariableIdentifierWithFactor, right: Expression) extends BinaryExpression {
  override def typ: Type = BoolType
  override def transform(f: (Expression) => Expression): Expression = f(LessThanWithVariableLeft(left, right.transform(f)))
  override def toString: String = left.toString + ArithmeticOperator.<.toString + right.toString
}

case class LessThanWithVariableRight(left: Expression, right: VariableIdentifierWithFactor) extends BinaryExpression {
  override def typ: Type = BoolType
  override def transform(f: (Expression) => Expression): Expression = f(LessThanWithVariableRight(left.transform(f), right))
  override def toString: String = left.toString + ArithmeticOperator.<.toString + right.toString
}

sealed trait DivideExpression extends Expression {
  def left: Int
  def right: Expression
  override def typ: Type = BoolType
  override def pp: ProgramPoint = right.pp
  override def ids: IdentifierSet = right.ids
  override def transform(f: (Expression) => Expression): Expression = f(this)
  override def contains(f: (Expression) => Boolean): Boolean = f(this) || f(right)
  override def toString: String = toModuloExpr.toString
  def toModuloExpr: BinaryArithmeticExpression
}

case class Divides(left: Int, right: Expression) extends DivideExpression {
  def toModuloExpr: BinaryArithmeticExpression = equ(modulo(right, Constant(left.toString, IntType)), const(0))
}

case class NotDivides(left: Int, right: Expression) extends DivideExpression {
  def toModuloExpr: BinaryArithmeticExpression = neq(modulo(right, Constant(left.toString, IntType)), const(0))
}

object ExpressionBuilder {

  def const(c: Int): Constant = Constant(c.toString, IntType)

  def plus(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.+)

  def plusOne(expr: Expression): BinaryArithmeticExpression = plus(expr, oneConst)

  def neg(arg: Expression): UnaryArithmeticExpression = UnaryArithmeticExpression(arg, ArithmeticOperator.-, IntType)

  def minus(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.-)

  def mult(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.*)

  def modulo(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.%)

  def lt(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.<)

  def leq(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.<=)

  def and(left: Expression, right: Expression): BinaryBooleanExpression = BinaryBooleanExpression(left, right, BooleanOperator.&&)

  def or(left: Expression, right: Expression): BinaryBooleanExpression = BinaryBooleanExpression(left, right, BooleanOperator.||)

  def equ(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.==)

  def neq(left: Expression, right: Expression): BinaryArithmeticExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.!=)

  def not(arg: Expression): NegatedBooleanExpression = NegatedBooleanExpression(arg)
}

object Main3 {
  def main(args: Array[String]): Unit = {
    val a = VariableIdentifier("A")(IntType)
    val b = VariableIdentifier("B")(IntType)
    val c = VariableIdentifier("C")(IntType)
    QuantifierElimination.eliminate(b, and(equ(a, b), not(and(leq(const(0), b), leq(b, c)))))
  }
}
