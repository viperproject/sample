/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils._
import com.typesafe.scalalogging.LazyLogging

/**
  * @author Severin Münger
  *         Added on 10.01.17.
  */
object QuantifierElimination extends LazyLogging {

  def eliminate(variable: Identifier, conjuncts: Set[Expression]): Set[Expression] = eliminate(Set(variable), conjuncts)

  def eliminate(variables: Set[_ <: Identifier], conjuncts: Set[Expression]): Set[Expression] = toCNFConjuncts(eliminate(variables, conjuncts.reduce(and)))

  def eliminate(variables: Set[_ <: Identifier], expr: Expression): Expression = variables.foldLeft[Expression](expr)((expr, variable) => eliminate(variable, expr))

  def eliminate(variable: Identifier, expr: Expression): Expression =  if (!expr.ids.contains(variable)) expr else {
    println(s"original to eliminate $variable (containing ${countLiterals(expr)} literals): $expr")
    val formulaNNF = toNNF(expr)
    println(s"F1[$variable] (NNF) (containing ${countLiterals(formulaNNF)}) literals): $formulaNNF")
    val collected = collectVariable(variable, formulaNNF)
    println(s"F3[$variable] (collected) (containing ${countLiterals(collected)} literals): $collected")
    val (lcmReplaced, freshVariable) = replaceLCM(variable, collected)
    println(s"F4[$variable] (lcmReplaced) (containing ${countLiterals(lcmReplaced)} literals): $lcmReplaced")
    val equivalentFormula = constructEquivalence(freshVariable, lcmReplaced)
    println(s"RESULT (containing ${countLiterals(equivalentFormula)} literals): $equivalentFormula\n")
    equivalentFormula
  }
  // Step 1
  private def toNNF(expr: Expression): Expression = Utils.toNNF(expr)

  // Step 3
  private def collectVariable(variable: Identifier, expr: Expression): Expression = expr.transform {
    case binExp@BinaryArithmeticExpression(left, right, op) if ArithmeticOperator.isComparison(op) && (binExp.contains(_ == variable) || !binExp.contains {
      case DivideExpression(_, _, _) => true
      case _ => false
    }) =>
      val collectedVariables = binOp(collect(left), collect(right), _ - _)
      val mapping: ((Any, Int)) => Expression = {
        case (_, 0) => intToConst(0, left.typ)
        case (key: Identifier, value) => IdentifierWithFactor(value, key)
        case (ConstPlaceholder, value) => intToConst(value, left.typ)
      }
      val varFactor = collectedVariables.getOrElse(variable, 0)
      if (varFactor > 0) comp(IdentifierWithFactor(varFactor, variable), unOp(collectedVariables - variable, - _).map(mapping).reduce(plus), op)
      else if (varFactor < 0) comp((collectedVariables - variable).map(mapping).reduce(plus), IdentifierWithFactor(-varFactor, variable), op)
      else binExp
    case binExp: BinaryArithmeticExpression => getCollected(binExp) // We still want to collect the variable to eliminate for divide expressions
    case other => other
  }

  // Step 4
  private def replaceLCM(variable: Identifier, expr: Expression): (Expression, Identifier) = {
    var numbers: Set[Int] = Set()
    expr.foreach {
      case IdentifierWithFactor(factor, `variable`) => numbers += factor.abs
      case _ =>
    }
    val leastCommonMultiple = lcm(numbers)
    val coefficientMultiplier: (Int) => ((Expression) => Expression) = (hPrime) => {
      case Constant(const, typ, _) => intToConst(const.toInt * hPrime, typ)
      case other => other
    }
    val freshVariable = VariableIdentifier(Context.createNewUniqueVarIdentifier("fresh", markAsTaken = false))(variable.typ)
    val replacedLCM = expr.transform {
      case ComparisonWithVariableLeft(factor, `variable`, right, op) => ComparisonWithVariableLeft(1, freshVariable, right.transform(coefficientMultiplier(leastCommonMultiple / factor)), op)
      case ComparisonWithVariableRight(left, factor, `variable`, op) => ComparisonWithVariableRight(left.transform(coefficientMultiplier(leastCommonMultiple / factor)), 1, freshVariable, op)
      case DivideExpression(divisor, arg, op) if arg.contains(_ == variable) =>
        var fact: Option[Int] = None
        arg.foreach {
          case IdentifierWithFactor(factor, `variable`) => assert(fact.isEmpty); fact = Some(factor)
          case _ =>
        }
        val hPrime = leastCommonMultiple / fact.get
        DivideExpression(divisor * hPrime, arg.transform {
          case IdentifierWithFactor(_, `variable`) => freshVariable
          case other => other
        }.transform(coefficientMultiplier(hPrime)), op)
      case other => other
    }
    if (leastCommonMultiple != 1) (and(replacedLCM, Divides(leastCommonMultiple, freshVariable)), freshVariable)
    else (replacedLCM, freshVariable)
  }

  // Step 5
  private def constructEquivalence(freshVariable: Identifier, expr: Expression): Expression = {
    val leftProjection = leftInfiniteProjection(freshVariable, expr)
    val d = delta(freshVariable, expr)
    val B = getBs(freshVariable, expr)
    println(s"F-∞[.] (left infinite projection): "+ leftProjection)
    println(s"F-∞[.] (left infinite projection simplified): "+ simplifyExpression(leftProjection))
    ((1 to d).map(j => leftProjection.transform {
      case ComparisonWithVariableRight(_, 1, `freshVariable`, _) | ComparisonWithVariableLeft(1, `freshVariable`, _, _) => throw new IllegalStateException()
      case `freshVariable` => intToConst(j, freshVariable.typ)
      case other => other
    }) ++ (1 to d).flatMap(j => B.map(b => {
      expr.transform {
        case `freshVariable` => plus(b, intToConst(j, freshVariable.typ))
        case other => other
      }
    }))).reduce(or) match {
      case result if QuantifiedPermissionsParameters.useQESimplifications => simplifyExpression(result)
      case result => result
    }
  }

  private def delta(freshVariable: Identifier, expr: Expression): Int = {
    var numbers: Set[Int] = Set()
    expr.foreach {
      case DivideExpression(left, _, _) => numbers += left
      case _ =>
    }
    lcm(numbers)
  }

  private def getBs(freshVariable: Identifier, expr: Expression): Set[Expression] = {
    var bs: Set[Expression] = Set()
    expr.foreach {
      case ComparisonWithVariableRight(left, 1, `freshVariable`, ArithmeticOperator.< | ArithmeticOperator.!=) => bs += left
      case ComparisonWithVariableRight(left, 1, `freshVariable`, ArithmeticOperator.<= | ArithmeticOperator.==) => bs += minus(left, intToConst(1, left.typ))
      case ComparisonWithVariableLeft(1, `freshVariable`, right, ArithmeticOperator.> | ArithmeticOperator.!=) => bs += right
      case ComparisonWithVariableLeft(1, `freshVariable`, right, ArithmeticOperator.>= | ArithmeticOperator.==) => bs += minus(right, intToConst(1, right.typ))
      case _ =>
    }
    bs
  }

  private def leftInfiniteProjection(variable: Identifier, expr: Expression): Expression = expr.transform {
    case ComparisonWithVariableLeft(1, `variable`, _, ArithmeticOperator.< | ArithmeticOperator.<= | ArithmeticOperator.!=) | ComparisonWithVariableRight(_, 1, `variable`, ArithmeticOperator.> | ArithmeticOperator.>= | ArithmeticOperator.!=) => trueConst
    case ComparisonWithVariableRight(_, 1, `variable`, ArithmeticOperator.< | ArithmeticOperator.<= | ArithmeticOperator.==) | ComparisonWithVariableLeft(1, `variable`, _, ArithmeticOperator.> | ArithmeticOperator.>= | ArithmeticOperator.==) => falseConst
    case other => other
  }

}

object IdentifierWithFactor {
  def apply(factor: Int, variable: Identifier): Expression = mult(intToConst(factor, variable.typ), variable)
  def unapply(expr: Expression): Option[(Int, Identifier)] = expr match {
    case BinaryArithmeticExpression(Constant(const, _, _), variable: Identifier, ArithmeticOperator.*) => Some(const.toInt, variable)
    case BinaryArithmeticExpression(variable: Identifier, Constant(const, _, _), ArithmeticOperator.*) => Some(const.toInt, variable)
    case _ => None
  }
}

object ComparisonWithVariableLeft {
  def apply(factor: Int, variable: Identifier, right: Expression, op: ArithmeticOperator.Value): Expression = BinaryArithmeticExpression(IdentifierWithFactor(factor, variable), right, op)
  def unapply(expr: Expression): Option[(Int, Identifier, Expression, ArithmeticOperator.Value)] = expr match {
    case BinaryArithmeticExpression(IdentifierWithFactor(factor, variable), right, op) if ArithmeticOperator.isComparison(op) => Some(factor, variable, right, op)
    case _ => None
  }
}

object ComparisonWithVariableRight {
  def apply(left: Expression, factor: Int, variable: Identifier, op: ArithmeticOperator.Value): Expression = BinaryArithmeticExpression(left, IdentifierWithFactor(factor, variable), op)
  def unapply(expr: Expression): Option[(Expression, Int, Identifier, ArithmeticOperator.Value)] = expr match {
    case BinaryArithmeticExpression(left, IdentifierWithFactor(factor, variable), op) if ArithmeticOperator.isComparison(op) => Some(left, factor, variable, op)
    case _ => None
  }
}

object DivideExpression {
  def apply(divisor: Int, arg: Expression, op: ArithmeticOperator.Value): Expression = comp(modulo(arg, intToConst(divisor, arg.typ)), intToConst(0, arg.typ), op)
  def unapply(divide: Expression): Option[(Int, Expression, ArithmeticOperator.Value)] = (divide: Expression) match {
    case Divides(divisor, expr) => Some(divisor, expr, ArithmeticOperator.==)
    case NotDivides(divisor, expr) => Some(divisor, expr, ArithmeticOperator.!=)
    case _ => None
  }
}

object Divides {
  def apply(divisor: Int, expr: Expression): Expression = equ(modulo(expr, intToConst(divisor, expr.typ)), intToConst(0, expr.typ))
  def unapply(divides: Expression): Option[(Int, Expression)] = divides match {
    case BinaryArithmeticExpression(BinaryArithmeticExpression(expr, Constant(divisor, _, _), ArithmeticOperator.%), Constant("0", _, _), ArithmeticOperator.==) => Some(divisor.toInt, expr)
    case BinaryArithmeticExpression(Constant("0", _, _), BinaryArithmeticExpression(expr, Constant(divisor, _, _), ArithmeticOperator.%), ArithmeticOperator.==) => Some(divisor.toInt, expr)
    case _ => None
  }
}

object NotDivides {
  def apply(divisor: Int, expr: Expression): Expression = neq(modulo(expr, intToConst(divisor, expr.typ)), intToConst(0, expr.typ))
  def unapply(notDivides: Expression): Option[(Int, Expression)] = notDivides match {
    case BinaryArithmeticExpression(BinaryArithmeticExpression(expr, Constant(divisor, _, _), ArithmeticOperator.%), Constant("0", _, _), ArithmeticOperator.!=) => Some(divisor.toInt, expr)
    case BinaryArithmeticExpression(Constant("0", _, _), BinaryArithmeticExpression(expr, Constant(divisor, _, _), ArithmeticOperator.%), ArithmeticOperator.!=) => Some(divisor.toInt, expr)
    case _ => None
  }
}