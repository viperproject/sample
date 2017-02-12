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

  def eliminate(variable: VariableIdentifier, conjuncts: Set[Expression]): Option[Set[Expression]] = eliminate(Set(variable), conjuncts.reduce(and)) match {
    case Some(eliminated) => Some(toCNFConjuncts(eliminated))
    case None => None
  }

  def eliminate(variables: Set[VariableIdentifier], conjuncts: Set[Expression]): Option[Set[Expression]] = eliminate(variables, conjuncts.reduce(and)) match {
    case Some(eliminated) => Some(toCNFConjuncts(eliminated))
    case None => None
  }

  def eliminate(variables: Set[VariableIdentifier], expr: Expression): Option[Expression] = variables.foldLeft[Option[Expression]](Some(expr))((expr, variable) => expr match {
    case Some(exp) => eliminate(variable, exp)
    case None => None
  })

  def eliminate(variable: VariableIdentifier, expr: Expression): Option[Expression] = try {
    println(s"original to eliminate $variable: $expr")
    val formulaNNF = toNNF(expr)
    println(s"F1[$variable] (NNF): " + formulaNNF)
//    val tzEquivalentFormula = toTzEquivalentFormula(formulaNNF)
//    println(s"F2[$variable] (tzEquivalentFormula): " + tzEquivalentFormula)
    val collected = collectVariable(variable, formulaNNF)
    println(s"F3[$variable] (collected): " + collected)
    val (lcmReplaced, freshVariable) = replaceLCM(variable, collected)
    println(s"F4[$variable] (lcmReplaced): " + lcmReplaced)
    val equivalentFormula = constructEquivalence(freshVariable, lcmReplaced)
    println("RESULT: " + equivalentFormula)
    Some(equivalentFormula)
  } catch { case exception: Exception =>
    println(s"Something went wrong: $exception")
    None
  }

  private def rewriteExpression(placeholder: VariableIdentifier, expr: Expression): Expression = expr.transform {
    case BinaryArithmeticExpression(`placeholder`, MaxFunction(left, right), ArithmeticOperator.==) => or(and(equ(placeholder, left), geq(left, right)), and(equ(placeholder, right), geq(right, left)))
  }

  // Step 1
  private def toNNF(expr: Expression): Expression = Utils.toNNF(expr)

  // Step 3
  private def collectVariable(variable: VariableIdentifier, expr: Expression): Expression = expr.transform {
    case binExp@BinaryArithmeticExpression(left, right, op) if ArithmeticOperator.isComparison(op) && (binExp.contains(_ == variable) || !binExp.contains {
      case DivideExpression(_, _) => true
      case _ => false
    }) =>
      val collectedVariables = binOp(collect(left), collect(right), _ - _)
      val mapping: ((Any, Int)) => Expression = {
        case (_, 0) => const(0)
        case (key: VariableIdentifier, value) => VariableIdentifierWithFactor(value, key)
        case (ConstPlaceholder, value) => const(value)
      }
      val varFactor = collectedVariables.getOrElse(variable, 0)
      if (varFactor > 0) comp(VariableIdentifierWithFactor(varFactor, variable), unOp(collectedVariables - variable, - _).map(mapping).reduce(plus), op)
      else if (varFactor < 0) comp((collectedVariables - variable).map(mapping).reduce(plus), VariableIdentifierWithFactor(-varFactor, variable), op)
      else binExp
    case other => other
  }

  // Step 4
  private def replaceLCM(variable: VariableIdentifier, expr: Expression): (Expression, VariableIdentifier) = {
    var numbers: Set[Int] = Set()
    expr.foreach {
      case VariableIdentifierWithFactor(factor, `variable`) => numbers += factor.abs
      case VariableIdentifierWithFactor(factor, `variable`) => numbers += factor.abs
      case _ =>
    }
    val leastCommonMultiple = if (numbers.isEmpty) 1 else lcm(numbers)
    val coefficientMultiplier: (Int) => ((Expression) => Expression) = (hPrime) => {
      case Constant(const, IntType, pp) => Constant((const.toInt * hPrime).toString, IntType, pp)
      case other => other
    }
    val freshVariable = VariableIdentifier(Context.createNewUniqueVarIdentifier("fresh", markAsTaken = false))(IntType)
    val replacedLCM = expr.transform {
      case ComparisonWithVariableLeft(factor, `variable`, right, op) => ComparisonWithVariableLeft(1, freshVariable, right.transform(coefficientMultiplier(leastCommonMultiple / factor)), op)
      case ComparisonWithVariableRight(left, factor, `variable`, op) => ComparisonWithVariableRight(left.transform(coefficientMultiplier(leastCommonMultiple / factor)), 1, freshVariable, op)
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
      case ComparisonWithVariableRight(_, 1, `freshVariable`, _) | ComparisonWithVariableLeft(1, `freshVariable`, _, _) => throw new IllegalStateException()
      case Divides(n, `freshVariable`) => Divides(n, const(j))
      case NotDivides(n, `freshVariable`) => NotDivides(n, const(j))
      case other => other
    }) ++ (1 to d).flatMap(j => B.map(b => {
      val x = plus(b, const(j))
      expr.transform {
        case ComparisonWithVariableRight(left, 1, `freshVariable`, op) => comp(left, x, op)
        case ComparisonWithVariableLeft(1, `freshVariable`, right, op) => comp(x, right, op)
        case Divides(n, `freshVariable`) => Divides(n, x)
        case NotDivides(n, `freshVariable`) => NotDivides(n, x)
        case other => other
      }
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
      case ComparisonWithVariableRight(left, 1, `freshVariable`, ArithmeticOperator.< | ArithmeticOperator.!=) => bs += left
      case ComparisonWithVariableRight(left, 1, `freshVariable`, ArithmeticOperator.<= | ArithmeticOperator.==) => bs += minus(left, oneConst)
      case ComparisonWithVariableLeft(1, `freshVariable`, right, ArithmeticOperator.> | ArithmeticOperator.!=) => bs += right
      case ComparisonWithVariableLeft(1, `freshVariable`, right, ArithmeticOperator.>= | ArithmeticOperator.==) => bs += minus(right, oneConst)
      case _ =>
    }
    bs
  }

  private def leftInfiniteProjection(variable: VariableIdentifier, expr: Expression): Expression = expr.transform {
    case ComparisonWithVariableLeft(1, `variable`, _, ArithmeticOperator.< | ArithmeticOperator.<= | ArithmeticOperator.!=) | ComparisonWithVariableRight(_, 1, `variable`, ArithmeticOperator.> | ArithmeticOperator.>= | ArithmeticOperator.!=) => trueConst
    case ComparisonWithVariableRight(_, 1, `variable`, ArithmeticOperator.< | ArithmeticOperator.<= | ArithmeticOperator.==) | ComparisonWithVariableLeft(1, `variable`, _, ArithmeticOperator.> | ArithmeticOperator.>= | ArithmeticOperator.==) => falseConst
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

object ComparisonWithVariableLeft {
  def apply(factor: Int, variable: VariableIdentifier, right: Expression, op: ArithmeticOperator.Value): Expression = BinaryArithmeticExpression(VariableIdentifierWithFactor(factor, variable), right, op)
  def unapply(expr: Expression): Option[(Int, VariableIdentifier, Expression, ArithmeticOperator.Value)] = expr match {
    case BinaryArithmeticExpression(VariableIdentifierWithFactor(factor, variable), right, op) if ArithmeticOperator.isComparison(op) => Some(factor, variable, right, op)
    case _ => None
  }
}

object ComparisonWithVariableRight {
  def apply(left: Expression, factor: Int, variable: VariableIdentifier, op: ArithmeticOperator.Value): Expression = BinaryArithmeticExpression(left, VariableIdentifierWithFactor(factor, variable), op)
  def unapply(expr: Expression): Option[(Expression, Int, VariableIdentifier, ArithmeticOperator.Value)] = expr match {
    case BinaryArithmeticExpression(left, VariableIdentifierWithFactor(factor, variable), op) if ArithmeticOperator.isComparison(op) => Some(left, factor, variable, op)
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

object MaxFunction {
  def unapply(expr: Expression): Option[(Expression, Expression)] = expr match {
    case FunctionCallExpression(name, left :: right :: Nil, _, _) if name == Context.getMaxFunction.name => Some(left, right)
    case _ => None
  }
}

object Main3 {
  def main(args: Array[String]): Unit = {
    val a = VariableIdentifier("A")(IntType)
    val b = VariableIdentifier("B")(IntType)
    //    QuantifierElimination.eliminate(Set(d), and(equ(a, plus(b, c)), and(and(leq(const(0), b), leq(b, const(10))), and(leq(const(0), c), leq(c, const(10))))))
    QuantifierElimination.eliminate(Set(a), and(equ(b, a), and(leq(const(0), a), leq(a, const(10)))))
    QuantifierElimination.eliminate(Set(a), equ(mult(const(2), a), b))
  }
}
