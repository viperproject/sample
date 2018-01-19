/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.qp

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.PermType
import ch.ethz.inf.pm.sample.qp.QpMath.Collected
import ch.ethz.inf.pm.sample.util.Math._
import ch.ethz.inf.pm.sample.util.SampleExpressions._
import com.typesafe.scalalogging.LazyLogging

object QpElimination extends LazyLogging {

  type Tuples = Set[(Expression, Expression)]

  def eliminate(expression: Expression, fact: Expression = True): Expression = expression.transform {
    // TODO: Do we need facts for quantifiers?
    case ForAll(variables, body) => Not(eliminateExistential(variables, Not(body)))
    case Exists(variables, body) => eliminateExistential(variables, body)
    case BigMin(variables, body) => Negate(eliminateMaximum(variables, Negate(body), fact))
    case BigMax(variables, body) => eliminateMaximum(variables, body, fact)
    case other => other
  }

  private def eliminateExistential(variables: Seq[VariableIdentifier], body: Expression): Expression =
    variables.foldLeft(body) { case (eliminated, variable) =>
      val simplified = QpMath.simplify(eliminated)
      eliminateExistential(variable, simplified)
    }

  private def eliminateExistential(variable: VariableIdentifier, body: Expression): Expression =
    if (!body.contains(_ == variable)) body
    else {
      // normalize expression
      val normalized = normalize(variable, body)
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

  private def eliminateMaximum(variables: Seq[VariableIdentifier], body: Expression, fact: Expression): Expression = {
    val simplified = QpMath.simplify(body)
    println(s"body: $body")
    println(s"simplified: $simplified")
    val result = variables.foldLeft(simplified) { case (current, variable) =>
      val rewritten = rewrite(current)
      println(s"eliminate $variable")
      println(s"rewritten: $rewritten")
      val eliminated = eliminateMaximum(variable, rewritten, True, fact)
      QpMath.simplify(eliminated)
    }
    QpMath.prettify(result)
  }

  private def eliminateMaximum(variable: VariableIdentifier, body: Expression, context: Expression, fact: Expression): Expression = body match {
    // rewrite maximum
    case Max(left, right) =>
      // TODO: Check whether adding the conditions "left>right" and "left<=right" helps.
      val newLeft = eliminateMaximum(variable, left, context, fact)
      val newRight = eliminateMaximum(variable, right, context, fact)
      Max(newLeft, newRight)
    // rewrite conditional
    case NonLeaf(condition, left, right) =>
      if (condition.contains(_ == variable)) {
        val newLeft = eliminateMaximum(variable, left, And(context, condition), fact)
        val newRight = eliminateMaximum(variable, right, And(context, Not(condition)), fact)
        Max(newLeft, newRight)
      } else {
        val newLeft = eliminateMaximum(variable, left, context, fact)
        val newRight = eliminateMaximum(variable, right, context, fact)
        ConditionalExpression(condition, newLeft, newRight)
      }
    case _ =>
      // normalize expression and compute tuples, projection, and delta
      var x = QpMath.simplify( NonLeaf(context, body, No))
      val normalized = normalize(variable, x)
      val (tuples, projection, delta) = analyzeArithmetic(variable, normalized)
      // compute unbounded solutions
      val unbounded = for (i <- 0 until delta) yield projection.transform {
        case `variable` => Literal(i)
        case other => other
      }
      // compute bounded solutions
      val bounded = for ((boundary, constraint) <- tuples; i <- 0 until delta) yield {
        // construct candidate solution
        val candidate = normalized.transform {
          case `variable` => Plus(boundary, Literal(i))
          case other => other
        }
        // simplify candidate
        val simplified = QpMath.simplify(candidate)
        simplified.transform {
          case original@ConditionalExpression(condition, term, No) =>
            // TODO: Add constraint?
            // check whether condition is satisfiable under collected constraint
            val body = And(And(condition, constraint), fact)
            val variables = body.ids.toSet.toSeq.collect { case v: VariableIdentifier if v.typ.isNumericalType => v }
            val eliminated = eliminate(Exists(variables, body))
            // ignore conditional if condition is not satisfiable
            eliminated match {
              case False => No
              case _ => original
            }
          case other => other
        }
      }
      // compute final expression
      val res = MaxList(unbounded ++ bounded)
      println("###")
      println(x)
      println(normalized)
      println(res)
      println(QpMath.simplify(res))
      println("###")
      res
  }

  /**
    * Returns an expression that is equivalent to the given expression where no maximum or conditional is nested inside
    * a minimum, no minimum is nested inside a maximum, and no addition is nested inside a subtraction, and the right-
    * hand side of every subtraction is a simple leaf expression.
    *
    * @param expression The expression.
    * @return An expression that is equivalent to the given expression.
    */
  private def rewrite(expression: Expression): Expression = expression.transform { original =>
    if (original.typ == PermType) original match {
      case Negate(argument) => rewriteMinus(No, argument)
      case Plus(left, right) => rewritePlus(left, right)
      case Minus(left, right) => rewriteMinus(left, right)
      case Min(left, right) => rewriteMin(left, right)
      case other => other
    } else original
  }

  /**
    * Returns an expression that is equivalent to the addition of the given left and right expression where the maximum
    * is recursively distributed over all maxima and conditionals appearing at the top level.
    *
    * @param left  The left argument to the addition.
    * @param right The right argument to the addition.
    * @return An expression that is equivalent to the addition of the given expressions.
    */
  private def rewritePlus(left: Expression, right: Expression): Expression = (left, right) match {
    // distribute addition over minimum
    case (Min(l, r), e) => Min(rewritePlus(l, e), rewritePlus(r, e))
    case (e, Min(l, r)) => Min(rewritePlus(e, l), rewritePlus(e, r))
    // distribute addition over maximum
    case (Max(l, r), e) => Max(rewritePlus(l, e), rewritePlus(r, e))
    case (e, Max(l, r)) => Max(rewritePlus(e, l), rewritePlus(e, r))
    // distribute addition over conditional
    case (NonLeaf(b, l, r), e) => ConditionalExpression(b, rewritePlus(l, e), rewritePlus(r, e))
    case (e, NonLeaf(b, l, r)) => ConditionalExpression(b, rewritePlus(l, e), rewritePlus(r, e))
    // add constants
    case (Permission(n1, d1), Permission(n2, d2)) =>
      val d = lcm(d1, d2)
      val n = n1 * d / d1 + n2 * d / d2
      val f = gcd(n, d)
      Permission(n / f, d / f)
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
  private def rewriteMinus(left: Expression, right: Expression): Expression = (left, right) match {
    // distribute subtraction over minimum and flip it to maximum if it appears on the rhs
    case (Min(l, r), e) => Min(rewriteMinus(l, e), rewriteMin(r, e))
    case (e, Min(l, r)) => Max(rewriteMin(e, l), rewriteMinus(e, r))
    // distribute subtraction over maxima and flip extremum if it appears in negative position
    case (Max(l, r), e) => Max(rewriteMinus(l, e), rewriteMinus(r, e))
    case (e, Max(l, r)) => Min(rewriteMinus(e, l), rewriteMinus(r, e))
    // distribute subtraction over conditional
    case (NonLeaf(b, l, r), e) => ConditionalExpression(b, rewriteMinus(l, e), rewriteMinus(r, e))
    case (e, NonLeaf(b, l, r)) => ConditionalExpression(b, rewriteMinus(e, l), rewriteMinus(e, r))
    // push addition inside subtraction
    case (e, Plus(l, r)) => rewriteMinus(rewriteMinus(e, l), r)
    // rewrite nested subtraction
    case (e, Minus(l, r)) => rewritePlus(rewriteMinus(e, l), r)
    // subtract constants
    case (Permission(n1, d1), Permission(n2, d2)) =>
      val d = lcm(d1, d2)
      val n = n1 * d / d1 - n2 * d / d2
      val f = gcd(n, d)
      Permission(n / f, d / f)
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
    // distribute minimum over maximum
    case (Max(l, r), e) => Max(rewriteMin(l, e), rewriteMin(r, e))
    case (e, Max(l, r)) => Max(rewriteMin(e, l), rewriteMin(e, r))
    // distribute minimum over conditional
    case (NonLeaf(b, l, r), e) => ConditionalExpression(b, rewriteMin(l, e), rewriteMin(r, e))
    case (e, NonLeaf(b, l, r)) => ConditionalExpression(b, rewriteMin(e, l), rewriteMin(e, r))
    // default action: do nothing
    case _ => Min(left, right)
  }

  def approximate(expression: Expression): Expression = {
    val simplified = QpMath.simplify(expression)
    println(s"sf $simplified")
    val rewritten = rewrite(simplified)
    println(s"rww: $rewritten")
    approximateArithmetic(rewritten)
  }

  private def approximateArithmetic(expression: Expression): Expression = expression match {
    case ConditionalExpression(condition, left, No) =>
      val approximated = approximateBoolean(condition, overapproximate = true)
      ConditionalExpression(approximated, approximateArithmetic(left), No)
    case ConditionalExpression(condition, left, right) =>
      val newLeft = approximateArithmetic(ConditionalExpression(condition, left, No))
      val newRight = approximateArithmetic(ConditionalExpression(Not(condition), left, No))
      Max(newLeft, newRight)
    case Minus(left, right) =>
      val newLeft = approximateArithmetic(left)
      val newRight = right match {
        case permission@Permission(_, _) => permission
        case ReadParameter(variable) => variable
        case Leaf(condition, permission) =>
          val approximated = approximateBoolean(condition, overapproximate = false)
          ConditionalExpression(approximated, permission, No)
      }
      Minus(newLeft, newRight)
    case permission@Permission(_, _) => permission
    case ReadParameter(variable) => variable
    case Plus(left, right) => Plus(approximateArithmetic(left), approximateArithmetic(right))
    case Min(left, right) => Min(approximateArithmetic(left), approximateArithmetic(right))
    case Max(left, right) => Max(approximateArithmetic(left), approximateArithmetic(right))
  }

  /**
    * Approximates the heap dependent parts of the given condition.
    *
    * @param expression      The condition to approximate.
    * @param overapproximate The flag indicating whether to over- or underapproximate.
    * @return The approximated condition.
    */
  private def approximateBoolean(expression: Expression, overapproximate: Boolean): Expression = {
    val nnf = QpMath.toNnf(expression)
    nnf.transform {
      case comparison@Comparison(_, _, _) =>
        val approximate = comparison.contains {
          case FieldAccessExpression(_, _) => true
          case _ => false
        }
        if (approximate) Literal(overapproximate)
        else comparison
      case other => other
    }
  }

  private def normalize(variable: VariableIdentifier, expression: Expression): Expression = {
    val nnf = QpMath.toNnf(expression)
    val collected = collectVariable(variable, nnf)
    normalizeCoefficient(variable, collected)
  }

  // TODO: Approximate the currently not implemented cases
  def collectVariable(variable: VariableIdentifier, expression: Expression): Expression = expression.transform {
    case Divides(left, right) =>
      val collected = Collected(right)
      if (left.contains(_ == variable) || collected.rest.contains(_ == variable)) ???
      else Divides(left, collected.toExpression(variable))
    case NotDivides(left, right) =>
      val collected = Collected(right)
      if (left.contains(_ == variable) || collected.rest.contains(_ == variable)) ???
      else NotDivides(left, collected.toExpression(variable))
    case Comparison(left, right, operator) if left.contains(_ == variable) || right.contains(_ == variable) =>
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
          if (value == 0) Zero
          else Comparison(variable, Times(Literal(factor / value), right), operator)
        case _ => original
      }
      case original => original
    }

    //
    val constraint = if (factor == 1) True else Divides(Literal(factor), variable)
    if (expression.typ.isBooleanType) And(transformed, constraint)
    else ConditionalExpression(constraint, transformed, No)
  }

  /**
    * Analyzes the given arithmetic expression and returns a triple containing a set, an expression, and an integer with
    * the following properties.
    *
    * The first value of the triple is a set of tuples. Each tuple contains a boundary value and a boolean condition
    * under which it is interesting to look at that boundary value.
    *
    * The second value of the triple is the negative (?) infinite projection of the expression. The negative infinite
    * projection of an expression E with respect to a variable x is an expression E' that is equivalent to E for
    * sufficiently small values of x.
    *
    * The third return value is the least common multiple of all values appearing in divisibility expressions.
    *
    * @param variable   The variable to eliminate.
    * @param expression The arithmetic expression.
    * @return A triple with the described properties.
    */
  private def analyzeArithmetic(variable: VariableIdentifier, expression: Expression): (Tuples, Expression, Int) = expression match {
    // rewrite conjunction to nested conditional
    case ConditionalExpression(And(left, right), term, No) =>
      analyzeArithmetic(variable, ConditionalExpression(left, ConditionalExpression(right, term, No), No))
    // rewrite disjunction to maximum over conditionals
    case ConditionalExpression(Or(left, right), term, No) =>
      analyzeArithmetic(variable, Max(ConditionalExpression(left, term, No), ConditionalExpression(right, term, No)))
    // analyze leaf expression
    case Leaf(condition, permission) =>
      val (set, projection, delta) = analyzeBoolean(variable, condition)
      (toTuples(set), Leaf(projection, permission), delta)
    // analyze conditional expression
    case NonLeaf(condition, term, No) =>
      // analyze term and condition
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, term)
      val (tuples2, projection2, delta2) = {
        val (set, projection, delta) = analyzeBoolean(variable, condition)
        // check whether it is sound to optimize
        val simplified = QpMath.simplify(projection1)
        val filter = simplified match {
          case No =>
            val negated = Not(condition)
            val disjuncts = for ((expression, _) <- tuples1) yield negated.transform {
              case `variable` => expression
              case other => other
            }
            OrList(disjuncts)
          case _ => True
        }
        (toTuples(set, filter), projection, delta)
      }
      (tuples1 ++ tuples2, NonLeaf(projection2, projection1, No), lcm(delta1, delta2))
    // analyze constant or variable
    case constant@Permission(_, _) => (Set.empty, constant, 1)
    case variable: VariableIdentifier => (Set.empty, variable, 1)
    // analyze addition
    case Plus(left, right) =>
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, left)
      val (tuples2, projection2, delta2) = analyzeArithmetic(variable, right)
      (tuples1 ++ tuples2, Plus(projection1, projection2), lcm(delta1, delta2))
    // case analyze subtraction
    case Minus(left, Leaf(condition, permission)) =>
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, left)
      val (tuples2, projection2, delta2) = {
        // TODO: Add optimization
        // the condition is negated since we are interested in making it false
        val negated = QpMath.toNnf(Not(condition))
        val (set, projection, delta) = analyzeBoolean(variable, negated)
        (toTuples(set), Leaf(Not(projection), permission), delta)
      }
      (tuples1 ++ tuples2, Minus(projection1, projection2), lcm(delta1, delta2))
    // analyze minimum
    case Min(left, right) =>
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, left)
      val (tuples2, projection2, delta2) = analyzeArithmetic(variable, right)
      (tuples1 ++ tuples2, Min(projection1, projection2), lcm(delta1, delta2))
    // analyze maximum
    case Max(left, right) =>
      val (tuples1, projection1, delta1) = analyzeArithmetic(variable, left)
      val (tuples2, projection2, delta2) = analyzeArithmetic(variable, right)
      (tuples1 ++ tuples2, Max(projection1, projection2), lcm(delta1, delta2))
  }

  /**
    * Analyzes the given boolean expression and returns a triple containing a set, an expression, and an integer with
    * the following properties.
    *
    * The first value of the triple is a set containing the so-called boundary values.
    *
    * Depending on whether we are interested in a smallest or a largest solution, the second value of the triple is the
    * negative or positive infinite projection of the given expression.
    *
    * The third vlaue of the triple is the least common multiple of all values appearing in divisibility expressions.
    *
    * @param variable   The variable to eliminate.
    * @param expression The boolean expression.
    * @param smallest   The flag indicating whether we are interested in a smallest or a largest solution.
    * @return A triple with the described properties.
    */
  private def analyzeBoolean(variable: VariableIdentifier, expression: Expression, smallest: Boolean = QpParameters.SMALLEST_SOLUTION): (Set[Expression], Expression, Int) = expression match {
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
