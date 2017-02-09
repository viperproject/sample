/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.IntType
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils.ExpressionBuilder._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils._

/**
  * @author Severin Münger
  *         Added on 10.01.17.
  */
object QuantifierElimination {

  def eliminate(variable: VariableIdentifier, conjuncts: Set[Expression]): Set[Expression] = toCNFConjuncts(eliminate(Set(variable), conjuncts.reduce(and)))

  def eliminate(variables: Set[VariableIdentifier], conjuncts: Set[Expression]): Set[Expression] = toCNFConjuncts(eliminate(variables, conjuncts.reduce(and)))

  def eliminate(variables: Set[VariableIdentifier], expr: Expression): Expression = variables.foldLeft(expr)((expr, variable) => eliminate(variable, expr))

  def eliminate(variable: VariableIdentifier, expr: Expression): Expression = {
    println("original: " + expr)
    val formulaNNF = toNNF(expr)
    println(s"F1[$variable] (NNF): " + formulaNNF)
    val tzEquivalentFormula = toTzEquivalentFormula(formulaNNF)
    println(s"F2[$variable] (tzEquivalentFormula): " + tzEquivalentFormula)
    val collected = collectVariable(variable, tzEquivalentFormula)
    println(s"F3[$variable] (collected): " + collected)
    val (lcmReplaced, freshVariable) = replaceLCM(variable, collected)
    println(s"F4[$variable] (lcmReplaced): " + lcmReplaced)
    val equivalentFormula = constructEquivalence(freshVariable, lcmReplaced)
    println("RESULT: " + equivalentFormula)
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
      if (varFactor > 0)
        LessThanWithVariableLeft(varFactor, variable, unOp(collectedVariables - variable, - _).map(mapping).reduce(plus))
      else if (varFactor < 0)
        LessThanWithVariableRight((collectedVariables - variable).map(mapping).reduce(plus), -varFactor, variable)
      else
        expr
  }

  // Step 4
  private def replaceLCM(variable: VariableIdentifier, expr: Expression): (Expression, VariableIdentifier) = {
    var numbers: Set[Int] = Set()
    expr.foreach {
      case LessThanWithVariableLeft(factor, `variable`, _) => numbers += factor.abs
      case LessThanWithVariableRight(_, factor, `variable`) => numbers += factor.abs
      case _ =>
    }
    val leastCommonMultiple = if (numbers.isEmpty) 1 else lcm(numbers)
    val coefficientMultiplier: (Int) => ((Expression) => Expression) = (hPrime) => {
      case VariableIdentifierWithFactor(h, variableIdentifier) => VariableIdentifierWithFactor(h * hPrime, variableIdentifier)
      case Constant(const, IntType, pp) => Constant((const.toInt * hPrime).toString, IntType, pp)
      case other => other
    }
    val freshVariable = VariableIdentifier(Context.createNewUniqueVarIdentifier("fresh", markAsTaken = false))(IntType)
    val replacedLCM = expr.transform {
      case LessThanWithVariableLeft(factor, `variable`, right) =>
        val hPrime = leastCommonMultiple / factor
        LessThanWithVariableLeft(1, freshVariable, right.transform(coefficientMultiplier(hPrime)))
      case LessThanWithVariableRight(left, factor, `variable`) =>
        val h = leastCommonMultiple / factor
        LessThanWithVariableRight(left.transform(coefficientMultiplier(h)), 1, freshVariable)
      case other => other
    }
    if (leastCommonMultiple != 1) (and(replacedLCM, Divides(leastCommonMultiple, freshVariable)), freshVariable)
    else (replacedLCM, freshVariable)
  }

  // Step 5
  private def constructEquivalence(freshVariable: VariableIdentifier, expr: Expression): Expression = {
    val leftProjection = leftInfiniteProjection(freshVariable, expr)
    val d = delta(freshVariable, expr)
    val B = getBs(freshVariable, expr)
    println(s"F-∞[.] (left infinite projection): "+ leftProjection)
    ((1 to d).map(j => leftProjection.transform {
      case LessThanWithVariableRight(_, 1, `freshVariable`) |
           LessThanWithVariableLeft(1, `freshVariable`, _) => throw new IllegalStateException()
      case Divides(n, `freshVariable`) => Divides(n, const(j))
      case NotDivides(n, `freshVariable`) => NotDivides(n, const(j))
      case other => other
    }) ++ (1 to d).flatMap(j => B.map(b => expr.transform {
      case LessThanWithVariableRight(left, 1, `freshVariable`) => lt(left, plus(b, const(j)))
      case LessThanWithVariableLeft(1, `freshVariable`, right) => lt(plus(b, const(j)), right)
      case Divides(n, `freshVariable`) => Divides(n, plus(b, const(j)))
      case NotDivides(n, `freshVariable`) => NotDivides(n, plus(b, const(j)))
      case other => other
    }))).reduce(or) match {
      case result if QuantifiedPermissionsParameters.useQESimplifications => simplifyExpression(result)
      case result => result
    }
  }

  private def delta(freshVariable: VariableIdentifier, expr: Expression): Int = {
    var numbers: Set[Int] = Set()
    expr.foreach {
      case DivideExpression(left, _) => numbers += left
      case _ =>
    }
    if (numbers.isEmpty) 1 else lcm(numbers)
  }

  private def getBs(freshVariable: VariableIdentifier, expr: Expression): Set[Expression] = {
    var bs: Set[Expression] = Set()
    expr.foreach {
      case LessThanWithVariableRight(left, 1, `freshVariable`) => bs += left
      case _ =>
    }
    bs
  }

  private def leftInfiniteProjection(variable: VariableIdentifier, expr: Expression): Expression = expr.transform {
    case LessThanWithVariableLeft(1, `variable`, _) => trueConst
    case LessThanWithVariableRight(_, 1, `variable`) => falseConst
    case other => other
  }

}

object VariableIdentifierWithFactor {
  def apply(factor: Int, variable: VariableIdentifier): Expression = mult(const(factor), variable)
  def unapply(expr: Expression): Option[(Int, VariableIdentifier)] = expr match {
    case BinaryArithmeticExpression(Constant(const, IntType, _), variable: VariableIdentifier, ArithmeticOperator.*) => Some(const.toInt, variable)
    case BinaryArithmeticExpression(variable: VariableIdentifier, Constant(const, IntType, _), ArithmeticOperator.*) => Some(const.toInt, variable)
    case _ => None
  }
}

object LessThanWithVariableLeft {
  def apply(factor: Int, variable: VariableIdentifier, right: Expression): Expression = lt(VariableIdentifierWithFactor(factor, variable), right)
  def unapply(expr: Expression): Option[(Int, VariableIdentifier, Expression)] = expr match {
    case BinaryArithmeticExpression(VariableIdentifierWithFactor(factor, variable), right, ArithmeticOperator.<) => Some(factor, variable, right)
    case _ => None
  }
}

object LessThanWithVariableRight {
  def apply(left: Expression, factor: Int, variable: VariableIdentifier): Expression = lt(left, VariableIdentifierWithFactor(factor, variable))
  def unapply(expr: Expression): Option[(Expression, Int, VariableIdentifier)] = expr match {
    case BinaryArithmeticExpression(left, VariableIdentifierWithFactor(factor, variable), ArithmeticOperator.<) => Some(left, factor, variable)
    case _ => None
  }
}

object DivideExpression {
  def unapply(divide: Expression): Option[(Int, Expression)] = (divide: Expression) match {
    case Divides(divisor, expr) => Some(divisor, expr)
    case NotDivides(divisor, expr) => Some(divisor, expr)
    case _ => None
  }
}

object Divides {
  def apply(divisor: Int, expr: Expression): Expression = equ(modulo(expr, const(divisor)), zeroConst)
  def unapply(divides: Expression): Option[(Int, Expression)] = divides match {
    case BinaryArithmeticExpression(BinaryArithmeticExpression(expr, Constant(divisor, IntType, _), ArithmeticOperator.%), `zeroConst`, ArithmeticOperator.==) => Some(divisor.toInt, expr)
    case BinaryArithmeticExpression(`zeroConst`, BinaryArithmeticExpression(expr, Constant(divisor, IntType, _), ArithmeticOperator.%), ArithmeticOperator.==) => Some(divisor.toInt, expr)
    case _ => None
  }
}

object NotDivides {
  def apply(divisor: Int, expr: Expression): Expression = neq(modulo(expr, const(divisor)), zeroConst)
  def unapply(notDivides: Expression): Option[(Int, Expression)] = notDivides match {
    case BinaryArithmeticExpression(BinaryArithmeticExpression(expr, Constant(divisor, IntType, _), ArithmeticOperator.%), `zeroConst`, ArithmeticOperator.!=) => Some(divisor.toInt, expr)
    case BinaryArithmeticExpression(`zeroConst`, BinaryArithmeticExpression(expr, Constant(divisor, IntType, _), ArithmeticOperator.%), ArithmeticOperator.!=) => Some(divisor.toInt, expr)
    case _ => None
  }
}

object Main3 {
  def main(args: Array[String]): Unit = {
    val a = VariableIdentifier("A")(IntType)
    val b = VariableIdentifier("B")(IntType)
    val c = VariableIdentifier("C")(IntType)
    val d = VariableIdentifier("D")(IntType)
    //    QuantifierElimination.eliminate(Set(d), and(equ(a, plus(b, c)), and(and(leq(const(0), b), leq(b, const(10))), and(leq(const(0), c), leq(c, const(10))))))
    val elim = QuantifierElimination.eliminate(Set(a), (and(equ(b, a), and(leq(const(0), a), leq(a, const(10))))))
    println(toNNF(NegatedBooleanExpression(elim)))
  }
}
