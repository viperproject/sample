/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.qp

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.IntType
import ch.ethz.inf.pm.sample.qp.QpMath.Collected
import ch.ethz.inf.pm.sample.util.Math.lcm
import ch.ethz.inf.pm.sample.util.SampleExpressions._
import com.typesafe.scalalogging.LazyLogging

object QpElimination extends LazyLogging {

  type Tuples = Set[(Expression, Expression)]

  def main(arguments: Array[String]): Unit = {
    var variable = Variable("x", IntType)
    val body = ConditionalExpression(LessEqual(variable, Zero), Permission(1, 2), Permission(1, 3))
    val formula = BigMax(variable, body)

    val result = eliminate(formula)
    println(formula)
    println(result)
  }

  def foo(variable: VariableIdentifier, expression: Expression, context: Expression): Expression = expression match {
    case Max(left, right) =>
      // TODO: Add "l>r" and "l<=r" to condition and check how this affects the result.
      val newLeft = foo(variable, left, context)
      val newRIght = foo(variable, right, context)
      Max(newLeft, newRIght)
    case ConditionalExpression(condition, left, right) =>
      if (condition.contains(_ == variable)) {
        // TODO: Do we have to compute NNF of negated condition?
        val newLeft = foo(variable, left, And(context, condition))
        val newRight = foo(variable, right, And(context, Not(condition)))
        Max(newLeft, newRight)
      } else {
        val newLeft = foo(variable, left, context)
        val newRight = foo(variable, right, context)
        ConditionalExpression(condition, newLeft, newRight)
      }
    case _ => ???
  }

  def rewrite(expression: Expression): Expression = expression.transform {
    case Plus(left, right) => rewritePlus(left, right)
    case Minus(left, right) => rewriteMinus(left, right)
    case Min(left, right) => rewriteMin(left, right)
    case other => other
  }

  /**
    * Returns an expression that is equivalent to the addition of the given left and right expression where the maximum
    * is recursively distributed over all maxima and conditionals appearing at the top level.
    *
    * @param left  The left argument to the addition.
    * @param right The right argument to the addition.
    * @return An expression that is equivalent to the addition of the given expressions.
    */
  def rewritePlus(left: Expression, right: Expression): Expression = (left, right) match {
    // distribute addition over maxima
    case (Max(l, r), e) => Max(rewritePlus(l, e), rewritePlus(r, e))
    case (e, Max(l, r)) => Max(rewritePlus(e, l), rewritePlus(e, r))
    // distribute addition over conditionals
    case (NonLeaf(b, l, r), e) => ConditionalExpression(b, rewritePlus(l, e), rewritePlus(r, e))
    case (e, NonLeaf(b, l, r)) => ConditionalExpression(b, rewritePlus(l, e), rewritePlus(r, e))
    // default action: do nothing
    case _ => Plus(left, right)
  }

  /**
    * Returns an expression that is equivalent to the subtraction of the given right expression from the left expression
    * where the subtraction is recursively distributed over all maxima and conditionals appearing at the top level and
    * ensures that the right-hand side of subtractions are only leaf expressions.
    *
    * @param left  The left argument to the subtraction.
    * @param right The right argument to the subtraction.
    * @return An expression that is equivalent to the subtraction of the given expressions.
    */
  def rewriteMinus(left: Expression, right: Expression): Expression = (left, right) match {
    // distribute subtraction over maxima and flip extremum if it appears in negative position
    case (Max(l, r), e) => Max(rewriteMinus(l, e), rewriteMinus(r, e))
    case (e, Max(l, r)) => rewriteMinus(rewriteMinus(e, l), rewriteMinus(r, e))
    // distribute subtraction over conditionals
    case (NonLeaf(b, l, r), e) => ConditionalExpression(b, rewriteMinus(l, e), rewriteMinus(r, e))
    case (e, NonLeaf(b, l, r)) => ConditionalExpression(b, rewriteMinus(e, l), rewriteMinus(e, r))
    //
    case (e, Plus(l, r)) => rewriteMinus(rewriteMinus(e, l), r)
    case (e, Minus(l, r)) => rewritePlus(rewriteMinus(e, l), r)
    // default action: do nothing
    case _ => Minus(left, right)
  }

  /**
    * Returns an expression that is equivalent to the minimum of the given left and right expression where the minimum
    * is recursively distributed over all maxima and conditionals appearing at the top level.
    *
    * @param left  The left argument to the minimum.
    * @param right The right argument to the minimum.
    * @return An expression that is equivalent to the minimum of the given expressions.
    */
  private def rewriteMin(left: Expression, right: Expression): Expression = (left, right) match {
    // distribute minimum over maxima
    case (Max(l, r), e) => Max(rewriteMin(l, e), rewriteMin(r, e))
    case (e, Max(l, r)) => Max(rewriteMin(e, l), rewriteMin(r, e))
    // distribute minimum over conditionals
    case (NonLeaf(b, l, r), e) => ConditionalExpression(b, rewriteMin(l, e), rewriteMin(r, e))
    case (e, NonLeaf(b, l, r)) => ConditionalExpression(b, rewriteMin(e, l), rewriteMin(e, r))
    // default action: do nothing
    case _ => Min(left, right)
  }

  def eliminate(expression: Expression): Expression = {
    val simplified = QpMath.simplify(expression)
    simplified.transform {
      case Exists(variables, body) => variables.foldLeft(body) {
        case (eliminated, variable) => eliminateExistential(variable, eliminated)
      }
      case BigMin(variables, body) => variables.foldLeft(body) {
        case (eliminated, variable) => eliminateExtremum(variable, eliminated, maximum = false)
      }
      case BigMax(variables, body) => variables.foldLeft(body) {
        case (eliminated, variable) => eliminateExtremum(variable, eliminated, maximum = true)
      }
      case other => other
    }
  }

  private def eliminateExistential(variable: VariableIdentifier, expression: Expression): Expression =
    if (!expression.contains(_ == variable)) expression
    else {
      // normalize expression
      val normalized = normalize(variable, expression)
      // compute boundary expressions, projection, and delta
      val (expressions1, projection1, delta1) = analyzeBoolean(variable, normalized, smallest = true)
      val (expressions2, projection2, delta2) = analyzeBoolean(variable, normalized, smallest = false)
      // check whether there is a trivial unbounded solution
      if (projection1 == True || projection2 == True) True
      else {
        // pick smaller set of boundary expressions
        val smallest = expressions1.size <= expressions2.size
        val (expressions, projection, delta) =
          if (smallest) (expressions1, projection1, delta1)
          else (expressions2, projection2, delta2)
        // compute disjunction corresponding to unbounded solutions
        val unbounded = for (i <- 0 until delta) yield projection.transform {
          case `variable` => Literal(i)
          case other => other
        }
        // compute disjunction corresponding to bounded solutions
        val bounded = for (e <- expressions;
                           i <- 0 until delta) yield normalized.transform {
          case `variable` => if (smallest) Plus(e, Literal(i)) else Minus(e, Literal(i))
          case other => other
        }
        // build and simplify final expression
        val disjunction = OrList(unbounded ++ bounded)
        QpMath.simplify(disjunction)
      }
    }

  private def eliminateExtremum(variable: VariableIdentifier, expression: Expression, maximum: Boolean): Expression =
    if (!expression.contains(_ == variable)) expression
    else {
      // normalize expression
      val normalized = normalize(variable, expression)
      // compute boundary expressions, projection, and delta
      val (expressions, projection, delta) = analyzeArithmetic(variable, normalized, maximum)
      // compute unbounded solutions
      val unbounded = for (i <- 0 until delta) yield projection.transform {
        case `variable` => Literal(i)
        case other => other
      }
      // compute bounded solutions
      val bounded = for ((boundary, constraint) <- expressions;
                         i <- 0 until delta) yield {
        // construct candidate solution
        val candidate = normalized.transform {
          case `variable` => Plus(boundary, Literal(i))
          case other => other
        }
        // simplify candidate and check whether conditions contradict the collected constraint
        val simplified = QpMath.simplify(candidate)
        simplified.transform {
          case original@ConditionalExpression(condition, term, No) =>
            // TODO: Add constraints?
            val body = And(condition, constraint)
            val variables = body.ids.toSet.collect {
              case variable: VariableIdentifier if variable.typ.isNumericalType => variable
            }
            val formula = Exists(variables.toSeq, body)
            val eliminated = QpElimination.eliminate(formula)
            eliminated match {
              case False => No
              case _ => original
            }
          case other => other
        }
      }
      // compute final extremum
      val terms = unbounded ++ bounded
      val extremum = if (maximum) MaxList(terms) else MinList(terms)
      val result = QpMath.simplify(extremum)

      logger.trace(s"${
        if (maximum) "max" else "min"
      } $variable :: $expression")
      logger.trace(s"res = $result")

      result
    }


  private def normalize(variable: VariableIdentifier, expression: Expression): Expression = {
    val nnf = QpMath.toNnf(expression)
    val collected = collectVariable(variable, nnf)
    normalizeCoefficient(variable, collected)
  }

  def collectVariable(variable: VariableIdentifier, expression: Expression): Expression = expression.transform {
    case Divides(left, right) => ???
    case NotDivides(left, right) => ???
    case Comparison(left, right, operator) if left.contains(_ == variable) || right.contains(_ == variable) =>
      // TODO: Check whether the rest contains the collected variable and over/under approximate it
      val collected = Collected(Minus(left, right))
      if (collected.rest.contains(_ == variable)) ???
      else {
        val factor = collected.coefficients.getOrElse(variable, 0)
        val positive = if (factor >= 0) collected.negate() else collected
        val newLeft = QpMath.simplification(Times(Literal(math.abs(factor)), variable))
        val newRight = positive.drop(variable).toExpression
        val newOperator = if (factor >= 0) operator else ArithmeticOperator.flip(operator)
        QpMath.simplification(Comparison(newLeft, newRight, newOperator))
      }
    case other => other
  }

  def normalizeCoefficient(variable: VariableIdentifier, expression: Expression): Expression = {
    // collect all coefficients
    var coefficients = Set.empty[Int]
    expression.foreach {
      case Times(`variable`, Literal(value: Int)) => coefficients += value
      case Times(Literal(value: Int), `variable`) => coefficients += value
      case _ => // do nothing
    }
    // compute least common multiple
    val factor = lcm(coefficients)
    // normalize coefficients
    val transformed = expression.transform {
      case original@Comparison(left, right, operator) => left match {
        case `variable` =>
          Comparison(variable, Times(Literal(factor), right), operator)
        case Times(Literal(value: Int), `variable`) =>
          Comparison(variable, Times(Literal(factor / value), right), operator)
        case _ => original
      }
      case original => original
    }

    //
    val constraint = if (factor == 1) True else Divides(Literal(factor), variable)
    if (expression.typ.isBooleanType) QpMath.simplify(And(transformed, constraint))
    else ConditionalExpression(constraint, transformed, No)
  }

  /**
    * The first return value is the set of so-called interesting expressions.
    *
    * Depending on whether we are interested in a smallest or a largest solution, the second return value is the
    * negative or positive infinite projection of the expression. The negative infinite projection of an expression E
    * with respect to a variable x is an expression E' that is equivalent to E for sufficiently small values  of x.
    * Analogously, the positive infinite projection E' is equivalent to E for sufficiently large values of x.
    *
    * The third return value is the delta that indicates the least common multiple of all values appearing in
    * divisibility expressions.
    *
    * @param variable   The variable to eliminate.
    * @param expression The expression.
    * @return
    */
  private def analyzeArithmetic(variable: VariableIdentifier, expression: Expression, maximum: Boolean): (Tuples, Expression, Int) = expression match {
    // conditional expression
    case ConditionalExpression(condition, term, No) => condition match {
      case And(left, right) =>
        // rewrite to nested conditional
        val partial = ConditionalExpression(right, term, No)
        val rewritten = ConditionalExpression(left, partial, No)
        analyzeArithmetic(variable, rewritten, maximum)
      case Or(left, right) =>
        // TODO: Check whether this is a correct transformation
        // rewrite to maximum/minimum of two conditionals
        val conditional1 = ConditionalExpression(left, term, No)
        val conditional2 = ConditionalExpression(right, term, No)
        val rewritten = if (maximum) Max(conditional1, conditional2) else Min(conditional1, conditional2)
        analyzeArithmetic(variable, rewritten, maximum)
      case _ =>
        // analyze condition and term
        val (set, projection1, delta1) = analyzeBoolean(variable, condition, QpParameters.SMALLEST_SOLUTION)
        val (tuples2, projection2, delta2) = analyzeArithmetic(variable, term, maximum)

        // TODO: Is this also sound when we project the delta?
        val optimize = QpMath.simplify(projection2) match {
          case No => true
          case _ => false
        }

        // add conditions used to optimize
        val filter = if (optimize) {
          val negated = Not(condition)
          val parts = for ((expression, _) <- tuples2) yield negated.transform {
            case `variable` => expression
            case other => other
          }
          OrList(parts)
        } else True

        val tuples = toTuples(set, filter) ++ tuples2
        val projection = ConditionalExpression(projection1, projection2, No)
        (tuples, projection, lcm(delta1, delta2))
    }
    // maxima
    case Max(left, right) =>
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, left, maximum)
      val (tuples2, projection2, delta2) = analyzeArithmetic(variable, right, maximum)
      (tuples1 ++ tuples2, Max(projection1, projection2), lcm(delta1, delta2))
    // minima
    case Min(left, right) =>
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, left, maximum)
      val (tuples2, projection2, delta2) = analyzeArithmetic(variable, right, maximum)
      (tuples1 ++ tuples2, Min(projection1, projection2), lcm(delta1, delta2))
    // additions
    case Plus(left, right) =>
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, left, maximum)
      val (tuples2, projection2, delta2) = analyzeArithmetic(variable, right, maximum)
      (tuples1 ++ tuples2, Plus(projection1, projection2), lcm(delta1, delta2))
    // subtractions
    case Minus(left, right) =>
      // TODO: Add constraints for optimization
      // TODO: Is it correct to make the second call to analyzeArithmetic with !maximum?
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, left, maximum)
      val (tuples2, projection2, delta2) = analyzeArithmetic(variable, right, !maximum)
      (tuples1 ++ tuples2, Minus(projection1, projection2), lcm(delta1, delta2))
    // expressions not depending on the variable to eliminate.
    case _ =>
      if (expression.contains(_ == variable)) ???
      else (Set.empty, expression, 1)
  }

  private def analyzeBoolean(variable: VariableIdentifier, expression: Expression, smallest: Boolean): (Set[Expression], Expression, Int) = expression match {
    // divisibility expressions
    case Divides(Literal(value: Int), _) => (Set.empty, expression, value)
    case NotDivides(Literal(value: Int), _) => (Set.empty, expression, value)
    // comparison expressions
    case Comparison(`variable`, term, operator) =>
      if (smallest) operator match {
        case ArithmeticOperator.== => (Set(term), False, 1)
        case ArithmeticOperator.!= => (Set(Plus(term, One)), True, 1)
        case ArithmeticOperator.< => (Set.empty, True, 1)
        case ArithmeticOperator.<= => (Set.empty, True, 1)
        case ArithmeticOperator.> => (Set(Plus(term, One)), False, 1)
        case ArithmeticOperator.>= => (Set(term), False, 1)
      } else operator match {
        case ArithmeticOperator.== => (Set(term), False, 1)
        case ArithmeticOperator.!= => (Set(Minus(term, One)), True, 1)
        case ArithmeticOperator.< => (Set(Minus(term, One)), False, 1)
        case ArithmeticOperator.<= => (Set(term), False, 1)
        case ArithmeticOperator.> => (Set.empty, True, 1)
        case ArithmeticOperator.>= => (Set.empty, True, 1)
      }
    // conjunctions and disjunctions
    case BinaryBooleanExpression(left, right, operator) =>
      val (set1, projection1, delta1) = analyzeBoolean(variable, left, smallest)
      val (set2, projection2, delta2) = analyzeBoolean(variable, right, smallest)
      val set = set1 ++ set2
      val projection = BinaryBooleanExpression(projection1, projection2, operator)
      val delta = lcm(delta1, delta2)
      (set, projection, delta)
    // boolean variables
    case `variable` if variable.typ.isBooleanType => (Set(`variable`), False, 1)
    case Not(`variable`) if variable.typ.isBooleanType => (Set.empty, True, 1)
    // expressions not depending on the variable to eliminate.
    case _ =>
      if (expression.contains(_ == variable)) ???
      else (Set.empty, expression, 1)
  }

  @inline
  protected def toTuples(expressions: Set[Expression], condition: Expression = True): Tuples =
    for (expression <- expressions) yield (expression, condition)


}
