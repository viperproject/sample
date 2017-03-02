/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.oorepresentation.silver.{IntType, PermType}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.EvaluationUtils._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils.ExpressionBuilder._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils._
import com.typesafe.scalalogging.LazyLogging

/**
  * @author Severin Münger
  *         Added on 10.01.17.
  */
object QuantifierElimination extends LazyLogging {

  def eliminate(variable: Identifier, conjuncts: Set[Expression]): Option[Set[Expression]] = eliminate(Set(variable), conjuncts.reduce(and)) match {
    case Some(eliminated) => Some(toCNFConjuncts(eliminated))
    case None => None
  }

  def eliminate(variables: Set[_ <: Identifier], conjuncts: Set[Expression]): Option[Set[Expression]] = eliminate(variables, conjuncts.reduce(and)) match {
    case Some(eliminated) => Some(toCNFConjuncts(eliminated))
    case None => None
  }

  def eliminate(variables: Set[_ <: Identifier], expr: Expression): Option[Expression] = variables.foldLeft[Option[Expression]](Some(expr))((expr, variable) => expr match {
    case Some(exp) => eliminate(variable, exp)
    case None => None
  })

  def eliminate(variable: Identifier, expr: Expression): Option[Expression] =  if (!expr.ids.contains(variable)) Some(expr) else {
    println(s"original to eliminate $variable (containing ${countLiterals(expr)} literals): $expr")
    val formulaNNF = toNNF(expr)
    println(s"F1[$variable] (NNF) (containing ${countLiterals(formulaNNF)}) literals): $formulaNNF")
    val collected = collectVariable(variable, formulaNNF)
    println(s"F3[$variable] (collected) (containing ${countLiterals(collected)} literals): $collected")
    val (lcmReplaced, freshVariable) = replaceLCM(variable, collected)
    println(s"F4[$variable] (lcmReplaced) (containing ${countLiterals(lcmReplaced)} literals): $lcmReplaced")
    val equivalentFormula = constructEquivalence(freshVariable, lcmReplaced)
    println(s"RESULT (containing ${countLiterals(equivalentFormula)} literals): $equivalentFormula\n")
    Some(equivalentFormula)
  }

  def rewriteExpression(placeholder: VariableIdentifier, quantifiedVariable: VariableIdentifier, state: QuantifiedPermissionsState, expr: Expression): Expression = {
    val exprWithoutFunctionsAndConditions = rewriteConditions(equ(placeholder, expr), quantifiedVariable, state)
    println(s"EXPR without functions/conditions: $exprWithoutFunctionsAndConditions")
    var denominators: Set[Int] = Set()
    exprWithoutFunctionsAndConditions.foreach {
      case BinaryArithmeticExpression(_, Constant(const, _, _), ArithmeticOperator./) => denominators += const.toInt
      case _ =>
    }
    val leastCommonMultiple = lcm(denominators)
    def divisionMultiplier(expr: Expression): (Expression, Boolean) = expr match {
      case BinaryArithmeticExpression(left, Constant(c, typ, _), ArithmeticOperator./) => (if (leastCommonMultiple / c.toInt == 1) left else mult(intToConst(leastCommonMultiple / c.toInt, typ), left), true)
      case binExp@BinaryArithmeticExpression(left, right, ArithmeticOperator.*) => (divisionMultiplier(left), divisionMultiplier(right)) match {
        case ((expr1, true), (expr2, false)) => (mult(expr1, expr2), true)
        case ((expr1, false), (expr2, true)) => (mult(expr1, expr2), true)
        case ((_, false), (_, false)) => (binExp, false)
      }
      case BinaryArithmeticExpression(left, right, op@(ArithmeticOperator.+ | ArithmeticOperator.-)) => (divisionMultiplier(left), divisionMultiplier(right)) match {
        case ((expr1, true), (expr2, true)) => (BinaryArithmeticExpression(expr1, expr2, op), true)
        case ((expr1, false), (expr2, false)) => (BinaryArithmeticExpression(expr1, expr2, op), false)
        case ((expr1, true), (expr2, false)) => (BinaryArithmeticExpression(expr1, mult(intToConst(leastCommonMultiple, expr2.typ), expr2), op), true)
        case ((expr1, false), (expr2, true)) => (BinaryArithmeticExpression(mult(intToConst(leastCommonMultiple, expr1.typ), expr1), expr2, op), true)
      }
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.%) => (modulo(mult(left, intToConst(leastCommonMultiple, left.typ)), mult(right, intToConst(leastCommonMultiple, right.typ))), true)
      case other => (other, false)
    }
    def coefficientMultiplier(expr: Expression): Expression = divisionMultiplier(expr) match {
      case (multiplied, true) => multiplied
      case (original, false) => mult(intToConst(leastCommonMultiple, original.typ), original)
    }
    simplifyExpression(exprWithoutFunctionsAndConditions.transform {
      case BinaryArithmeticExpression(left, right, op) if ArithmeticOperator.isComparison(op) => comp(coefficientMultiplier(left), coefficientMultiplier(right), op)
      case other => other
    })
  }

  private def rewriteConditions(expr: Expression, quantifiedVariable: VariableIdentifier, state: QuantifiedPermissionsState): Expression = {
    var freeVarMap: Map[VariableIdentifier, Set[(Expression, Expression)]] = Map()
    var freeVarOrder: Seq[VariableIdentifier] = Seq()
    val exprWithoutConditions = expr.transform {
      case ConditionalExpression(cond, left, right, typ) =>
        val freeVariable = VariableIdentifier(Context.createNewUniqueVarIdentifier("y"))(typ)
        freeVarOrder +:= freeVariable
        cond match {
          case ExpressionDescription(pp, intExpr) => simplifyExpression(intExpr) match {
            case const: Constant => freeVarMap += freeVariable -> Set((left, equ(quantifiedVariable, const)), (right, neq(quantifiedVariable, const)))
            case other =>
              val constraints = Context.preNumericalInfo(pp).numDom.getConstraints(intExpr.ids.toSetOrFail).filter(x => intExpr.ids.toSetOrFail.forall(id => x.contains {
                case `id` => true
                case _ => false
              })).reduceOption(and) match {
                case None => trueConst
                case Some(exp) => exp
              }
              val quantifiedCond = equ(quantifiedVariable, other)
              val condTrue = and(quantifiedCond, constraints)
              val condFalse = and(quantifiedCond, not(constraints))
              freeVarMap += freeVariable -> Set((left, condTrue), (right, condFalse))
          }
          case _ => freeVarMap += freeVariable -> Set((left, cond), (right, NegatedBooleanExpression(cond)))
        }
        freeVariable
      case MaxExpression(args, typ, _) =>
        val argSet = args.toSet
        val freeVariable = VariableIdentifier(Context.createNewUniqueVarIdentifier("y"))(typ)
        freeVarOrder +:= freeVariable
        freeVarMap += freeVariable -> argSet.map(arg => (arg, (argSet - arg).map(otherArg => geq(arg, otherArg)).reduce(and)))
        freeVariable
      case other => other
    }
    var exprSet: Set[Expression] = Set(exprWithoutConditions)
    for (x <- freeVarOrder) {
      var newExprSet: Set[Expression] = Set()
      for (existing <- exprSet) {
        for ((expr, conditionsTrue) <- freeVarMap(x)) {
          val newExpr = existing.transform {
            case `x` => expr
            case other => other
          }
          newExprSet += and(newExpr, conditionsTrue)
        }
      }
      exprSet = newExprSet
    }
    Context.clearIdentifiers(freeVarOrder.map(_.name))
    exprSet.reduce(or)
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

object Main3 {

  def main(args: Array[String]): Unit = {
    val q = VariableIdentifier("q")(IntType)
    val i = VariableIdentifier("i")(IntType)
    val j = VariableIdentifier("j")(IntType)
    val p = VariableIdentifier("p")(PermType)
    val inv = and(leq(intToConst(0, IntType), i), leq(i, intToConst(10, IntType)))
    println(simplifyExpression(or(
      QuantifierElimination.eliminate(i, and(and(equ(q, i), inv), equ(p, intToConst(1, PermType)))).get,
        and(not(QuantifierElimination.eliminate(i, and(equ(q, i), inv)).get), equ(p, intToConst(0, PermType))))))

    QuantifierElimination.eliminate(i, and(equ(i, q), equ(i, p)))
    QuantifierElimination.eliminate(i, and(equ(i, q), and(and(Divides(2, i), leq(intToConst(0, IntType), i)), leq(i, intToConst(9, IntType)))))
    QuantifierElimination.eliminate(i, and(equ(i, q), equ(i, intToConst(3, IntType))))
    val exp = simplifyExpression(not(QuantifierElimination.eliminate(i, not(and(or(iff(inv, equ(p, writeConst)), neq(i, q)), or(iff(not(inv), equ(p, noneConst)), neq(i, q))))).get))
    println(s"Disjuncts:")
    splitToDisjuncts(exp).foreach(println)
    println(s"\nConjuncts:")
    splitToConjuncts(exp).foreach(println)
  }
  implicit def intToIntConst(i: Int): Constant = intToConst(i, IntType)
}