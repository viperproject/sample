/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{IntType, PermType}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils.ExpressionBuilder._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.EvaluationUtils._
import com.typesafe.scalalogging.LazyLogging

/**
  * @author Severin Münger
  *         Added on 10.01.17.
  */
object QuantifierElimination extends LazyLogging {

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
    println(s"F1[$variable] (NNF) (containing " + countLiterals(formulaNNF) + s" literals): $formulaNNF")
    val collected = collectVariable(variable, formulaNNF)
    println(s"F3[$variable] (collected) (containing " + countLiterals(collected) + s" literals): $collected")
    val (lcmReplaced, freshVariable) = replaceLCM(variable, collected)
    println(s"F4[$variable] (lcmReplaced) (containing " + countLiterals(lcmReplaced) + s" literals): $lcmReplaced")
    val equivalentFormula = constructEquivalence(freshVariable, lcmReplaced)
    println(s"RESULT (containing " + countLiterals(equivalentFormula) + s" literals): $equivalentFormula")
    Some(equivalentFormula)
  } catch { case exception: Exception =>
    println(s"Something went wrong: $exception")
    None
  }

  def rewriteExpression(placeholder: VariableIdentifier, expr: Expression): Expression = {
    val exprWithPlaceholder = equ(placeholder, expr)
    var placeholderMap: Map[Expression, VariableIdentifier] = Map()
    def replaceExpression(expr: Expression): Expression =
      if (placeholderMap.contains(expr)) placeholderMap(expr)
      else {
        val newFreeVar = VariableIdentifier(Context.createNewUniqueVarIdentifier("p"))(expr.typ)
        placeholderMap += expr -> newFreeVar
        newFreeVar
      }
    val rewritten = exprWithPlaceholder.transform {
      case max@MaxFunction(_, _) => replaceExpression(max)
      case bound@BoundaryFunction(_) => replaceExpression(bound)
      case cond: ConditionalExpression => replaceExpression(cond)
      case other => other
    }
    val rewrittenWithPlaceholders = placeholderMap.foldLeft(rewritten) {
      case (existing, (replaced, placeholderVar)) => and(existing, replaced match {
        case MaxFunction(left, right) => rewriteEquMax(placeholderVar, left, right)
        case BoundaryFunction(arg) => rewriteEquMax(placeholderVar, arg, zeroConst)
        case ConditionalExpression(cond, left, right, _) => rewriteEquCond(placeholderVar, cond, left, right)
      })
    }
    var denominators: Set[Int] = Set()
    rewrittenWithPlaceholders.foreach {
      case BinaryArithmeticExpression(_, Constant(const, _, _), ArithmeticOperator./) => denominators += const.toInt
      case _ =>
    }
    val leastCommonMultiple = lcm(denominators)
    def divisionMultiplier(expr: Expression): (Expression, Boolean) = expr match {
      case BinaryArithmeticExpression(left, Constant(c, _, _), ArithmeticOperator./) => (if (leastCommonMultiple / c.toInt == 1) left else BinaryArithmeticExpression(leastCommonMultiple / c.toInt, left, ArithmeticOperator.*), true)
      case binExp@BinaryArithmeticExpression(left, right, ArithmeticOperator.*) => (divisionMultiplier(left), divisionMultiplier(right)) match {
        case ((expr1, true), (expr2, false)) => (BinaryArithmeticExpression(expr1, expr2, ArithmeticOperator.*), true)
        case ((expr1, false), (expr2, true)) => (BinaryArithmeticExpression(expr1, expr2, ArithmeticOperator.*), true)
        case ((_, false), (_, false)) => (binExp, false)
      }
      case BinaryArithmeticExpression(left, right, op@(ArithmeticOperator.+ | ArithmeticOperator.-)) => (divisionMultiplier(left), divisionMultiplier(right)) match {
        case ((expr1, true), (expr2, true)) => (BinaryArithmeticExpression(expr1, expr2, op), true)
        case ((expr1, false), (expr2, false)) => (BinaryArithmeticExpression(expr1, expr2, op), false)
        case ((expr1, true), (expr2, false)) => (BinaryArithmeticExpression(expr1, mult(leastCommonMultiple, expr2), op), true)
        case ((expr1, false), (expr2, true)) => (BinaryArithmeticExpression(mult(leastCommonMultiple, expr1), expr2, op), true)
      }
      case binExp@BinaryArithmeticExpression(_, _, ArithmeticOperator.%) => (mult(leastCommonMultiple, binExp), true)
      case other => (other, false)
    }
    def coefficientMultiplier(expr: Expression): Expression = divisionMultiplier(expr) match {
      case (multiplied, true) => multiplied
      case (original, false) => mult(leastCommonMultiple, original)
    }
    val rewrittenWithoutFractions = rewrittenWithPlaceholders.transform {
      case BinaryArithmeticExpression(left, right, op) if ArithmeticOperator.isComparison(op) => comp(coefficientMultiplier(left), coefficientMultiplier(right), op)
      case other => other
    }
    eliminate(placeholderMap.values.toSet, rewrittenWithoutFractions).get
  }

  private def rewriteEquMax(expr: Expression, maxLeft: Expression, maxRight: Expression): Expression = or(and(equ(expr, maxLeft), geq(maxLeft, maxRight)), and(equ(expr, maxRight), geq(maxRight, maxLeft)))

  private def rewriteEquCond(expr: Expression, cond: Expression, left: Expression, right: Expression) = or(and(equ(expr, left), cond), and(equ(expr, right), NegatedBooleanExpression(cond)))

  private def rewriteWithStateInfo(expr: Expression, quantifiedVariable: VariableIdentifier, state: QuantifiedPermissionsState): Expression = expr.transform {
    case BinaryArithmeticExpression(left, ExpressionDescription(pp, intExpr), op) if ArithmeticOperator.isComparison(op) =>
      val constraints = Context.preNumericalInfo(pp).numDom.getConstraints(intExpr.ids.toSetOrFail)
      constraints.foldLeft[Expression](comp(left, quantifiedVariable, op)) {
        case (existing, constraint) => and(existing, constraint)
      }
    case BinaryArithmeticExpression(ExpressionDescription(pp, intExpr), right, op) if ArithmeticOperator.isComparison(op) =>
      val constraints = Context.preNumericalInfo(pp).numDom.getConstraints(intExpr.ids.toSetOrFail)
      constraints.foldLeft[Expression](comp(right, quantifiedVariable, op)) {
        case (existing, constraint) => and(existing, constraint)
      }
    case other => other
  }

  // Step 1
  private def toNNF(expr: Expression): Expression = Utils.toNNF(expr)

  // Step 3
  private def collectVariable(variable: VariableIdentifier, expr: Expression): Expression = expr.transform {
    case binExp@BinaryArithmeticExpression(left, right, op) if ArithmeticOperator.isComparison(op) && (binExp.contains(_ == variable) || !binExp.contains {
      case DivideExpression(_, _, _) => true
      case _ => false
    }) =>
      val collectedVariables = binOp(collect(left), collect(right), _ - _)
      val mapping: ((Any, Int)) => Expression = {
        case (_, 0) => 0
        case (key: VariableIdentifier, value) => VariableIdentifierWithFactor(value, key)
        case (ConstPlaceholder, value) => value
      }
      val varFactor = collectedVariables.getOrElse(variable, 0)
      if (varFactor > 0) comp(VariableIdentifierWithFactor(varFactor, variable), unOp(collectedVariables - variable, - _).map(mapping).reduce(plus), op)
      else if (varFactor < 0) comp((collectedVariables - variable).map(mapping).reduce(plus), VariableIdentifierWithFactor(-varFactor, variable), op)
      else binExp
    case binExp: BinaryArithmeticExpression => getCollected(binExp) // We still want to collect the variable to eliminate for divide expressions
    case other => other
  }

  // Step 4
  private def replaceLCM(variable: VariableIdentifier, expr: Expression): (Expression, VariableIdentifier) = {
    var numbers: Set[Int] = Set()
    expr.foreach {
      case VariableIdentifierWithFactor(factor, `variable`) => numbers += factor.abs
      case _ =>
    }
    val leastCommonMultiple = lcm(numbers)
    val coefficientMultiplier: (Int) => ((Expression) => Expression) = (hPrime) => {
      case Constant(const, IntType, pp) => Constant((const.toInt * hPrime).toString, IntType, pp)
      case other => other
    }
    val freshVariable = VariableIdentifier(Context.createNewUniqueVarIdentifier("fresh", markAsTaken = false))(IntType)
    val replacedLCM = expr.transform {
      case ComparisonWithVariableLeft(factor, `variable`, right, op) => ComparisonWithVariableLeft(1, freshVariable, right.transform(coefficientMultiplier(leastCommonMultiple / factor)), op)
      case ComparisonWithVariableRight(left, factor, `variable`, op) => ComparisonWithVariableRight(left.transform(coefficientMultiplier(leastCommonMultiple / factor)), 1, freshVariable, op)
      case DivideExpression(divisor, arg, op) if arg.contains(_ == variable) =>
        var fact: Option[Int] = None
        arg.foreach {
          case VariableIdentifierWithFactor(factor, `variable`) => assert(fact.isEmpty); fact = Some(factor)
          case _ =>
        }
        val hPrime = leastCommonMultiple / fact.get
        DivideExpression(divisor * hPrime, arg.transform {
          case VariableIdentifierWithFactor(_, `variable`) => freshVariable
          case other => other
        }.transform(coefficientMultiplier(hPrime)), op)
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
      case `freshVariable` => j
      case other => other
    }) ++ (1 to d).flatMap(j => B.map(b => {
      expr.transform {
        case `freshVariable` => plus(b, j)
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
      case DivideExpression(left, _, _) => numbers += left
      case _ =>
    }
    lcm(numbers)
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
  def apply(factor: Int, variable: VariableIdentifier): Expression = mult(factor, variable)
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
  def apply(divisor: Int, arg: Expression, op: ArithmeticOperator.Value): Expression = comp(modulo(arg, divisor), 0, op)
  def unapply(divide: Expression): Option[(Int, Expression, ArithmeticOperator.Value)] = (divide: Expression) match {
    case Divides(divisor, expr) => Some(divisor, expr, ArithmeticOperator.==)
    case NotDivides(divisor, expr) => Some(divisor, expr, ArithmeticOperator.!=)
    case _ => None
  }
}

object Divides {
  def apply(divisor: Int, expr: Expression): Expression = equ(modulo(expr, divisor), zeroConst)
  def unapply(divides: Expression): Option[(Int, Expression)] = divides match {
    case BinaryArithmeticExpression(BinaryArithmeticExpression(expr, Constant(divisor, IntType, _), ArithmeticOperator.%), `zeroConst`, ArithmeticOperator.==) => Some(divisor.toInt, expr)
    case BinaryArithmeticExpression(`zeroConst`, BinaryArithmeticExpression(expr, Constant(divisor, IntType, _), ArithmeticOperator.%), ArithmeticOperator.==) => Some(divisor.toInt, expr)
    case _ => None
  }
}

object NotDivides {
  def apply(divisor: Int, expr: Expression): Expression = neq(modulo(expr, divisor), zeroConst)
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

object BoundaryFunction {
  def unapply(expr: Expression): Option[(Expression)] = expr match {
    case FunctionCallExpression(name, arg :: Nil, _, _) if name == Context.getBoundaryFunction.name => Some(arg)
    case _ => None
  }
}

object Main3 {
  def main(args: Array[String]): Unit = {
    val a = VariableIdentifier("A")(IntType)
    val b = VariableIdentifier("B")(IntType)
    val c = VariableIdentifier("C")(IntType)
    val d = VariableIdentifier("D")(IntType)
    // QuantifierElimination.eliminate(Set(d), and(equ(a, plus(b, c)), and(and(leq(const(0), b), leq(b, const(10))), and(leq(const(0), c), leq(c, const(10))))))
    QuantifierElimination.eliminate(Set(a), and(equ(b, a), and(leq(0, a), leq(a, 10))))
    QuantifierElimination.eliminate(Set(a), equ(mult(2, a), b))
    QuantifierElimination.rewriteExpression(a, max(cond(equ(b, c), 1, 0), cond(equ(b, d), div(1, 2), 0)))
  }

  def max(left: Expression, right: Expression): FunctionCallExpression = FunctionCallExpression(Context.getMaxFunction.name, Seq(left, right), PermType)

  def cond(condition: Expression, left: Expression, right: Expression) = ConditionalExpression(condition, left, right, PermType)
}
