/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.qp

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.oorepresentation.silver.PermType
import ch.ethz.inf.pm.sample.qp.QpMath.Collected
import ch.ethz.inf.pm.sample.util.Math._
import ch.ethz.inf.pm.sample.util.SampleExpressions._
import com.typesafe.scalalogging.LazyLogging

object QpElimination extends LazyLogging {

  type Tuples = Set[(Expression, Expression)]

  def eliminate(expression: Expression, fact: Expression = True, over: Boolean): Expression = expression.transform {
    // TODO: Do we need facts for quantifiers?
    case ForAll(variables, body) => Not(eliminateExistential(variables, Not(body)))
    case Exists(variables, body) => eliminateExistential(variables, body)
    case BigMin(variables, body) => Negate(eliminateMaximum(variables, Negate(body), fact, over))
    case BigMax(variables, body) => eliminateMaximum(variables, body, fact, over)
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
      val approximated = approximateBoolean(normalized, true)
      // compute boundary expressions, projection, and delta
      val (expressions1, projection1, delta1) = analyzeBoolean(variable, approximated, smallest = true)
      val (expressions2, projection2, delta2) = analyzeBoolean(variable, approximated, smallest = false)
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

  private def eliminateMaximum(variables: Seq[VariableIdentifier], body: Expression, fact: Expression, over: Boolean): Expression = {
    val simplified = QpMath.simplify(body)
    val result = variables.foldLeft(simplified) { case (current, variable) =>
      val rewritten = rewrite(variable, current)
      val eliminated = eliminateMaximum(variable, rewritten, True, fact, over)
      QpMath.simplify(eliminated)
    }
    QpMath.prettify(result)
  }

  private def eliminateMaximum(variable: VariableIdentifier, body: Expression, context: Expression, fact: Expression, over: Boolean): Expression = {
    val rewrite = body.contains(_ == variable)
    body match {
      // rewrite maximum
      case Max(left, right) if rewrite =>
        // TODO: Check whether adding the conditions "left>right" and "left<=right" helps.
        val newLeft = eliminateMaximum(variable, left, context, fact, over)
        val newRight = eliminateMaximum(variable, right, context, fact, over)
        Max(newLeft, newRight)
      // rewrite conditional
      case NonLeaf(condition, left, right) if rewrite =>
        if (condition.contains(_ == variable)) {
          val newLeft = eliminateMaximum(variable, left, And(context, condition), fact, over)
          val newRight = eliminateMaximum(variable, right, And(context, Not(condition)), fact, over)
          Max(newLeft, newRight)
        } else {
          val newLeft = eliminateMaximum(variable, left, context, fact, over)
          val newRight = eliminateMaximum(variable, right, context, fact, over)
          ConditionalExpression(condition, newLeft, newRight)
        }
      case No => No
      case _ =>
        // normalize expression and compute tuples, projection, and delta
        val formula = NonLeaf(context, body, No)
        val normalized = normalize(variable, formula)
        val approximated = approximateArithmetic(normalized, over)
        val (tuples, projection, delta) = analyzeArithmetic(variable, approximated)
        // compute unbounded solutions
        val sp = QpMath.simplify(projection)

        val unbounded = for (i <- 0 until delta) yield sp.transform {
          case `variable` => Literal(i)
          case other => other
        }
        // compute bounded solutions
        val bounded = for ((boundary, constraint) <- tuples; i <- 0 until delta) yield {
          // construct candidate solution
          val candidate = approximated.transform {
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
              val eliminated = eliminate(Exists(variables, body), over = over) // TODO: Always overapproximate?
              // ignore conditional if condition is not satisfiable
              eliminated match {
                case False => No
                case _ => original
              }
            case other => other
          }
        }
        // compute final expression
        MaxList(unbounded ++ bounded)
    }
  }

  /**
    * Returns an expression that is equivalent to the given expression where no maximum or conditional is nested inside
    * a minimum, no minimum is nested inside a maximum, and no addition is nested inside a subtraction, and the right-
    * hand side of every subtraction is a simple leaf expression.
    *
    * @param variable
    * @param expression The expression.
    * @return An expression that is equivalent to the given expression.
    */
  private def rewrite(variable: VariableIdentifier, expression: Expression): Expression = expression.transform { original =>
    if (original.typ == PermType) original match {
      case Negate(argument) => rewriteNegation(variable, argument)
      case Plus(left, right) => rewritePlus(variable, left, right)
      case Minus(left, right) => rewriteMinus(variable, left, right)
      case Min(left, right) => rewriteMin(variable, left, right)
      case other => other
    } else original
  }

  private def rewriteNegation(variable: VariableIdentifier, argument: Expression): Expression =
    if (argument.contains(_ == variable)) argument match {
      case Min(e1, e2) => Max(rewriteNegation(variable, e1), rewriteNegation(variable, e2))
      case Max(e1, e2) => Min(rewriteNegation(variable, e1), rewriteNegation(variable, e2))
      case NonLeaf(b, e1, e2) => ConditionalExpression(b, rewriteNegation(variable, e1), rewriteNegation(variable, e2))
      case Plus(e1, e2) => rewriteMinus(variable, rewriteNegation(variable, e1), e2)
      case Minus(e1, e2) => rewriteMinus(variable, e2, e1)
      case Negate(a) => a
      case _ => Negate(argument)
    } else argument match {
      case Permission(n, d) => Permission(-n, d)
      case _ => Negate(argument)
    }

  private def rewritePlus(variable: VariableIdentifier, left: Expression, right: Expression): Expression = {
    // check whether variable appears in addition
    val rewriteLeft = left.contains(_ == variable)
    val rewriteRight = right.contains(_ == variable)
    if (rewriteLeft || rewriteRight) (left, right) match {
      // distribute addition over minimum
      case (Min(e1, e2), _) if rewriteLeft => Min(rewritePlus(variable, e1, right), rewritePlus(variable, e2, right))
      case (_, Min(e1, e2)) if rewriteRight => Min(rewritePlus(variable, left, e1), rewritePlus(variable, left, e2))
      // distribute addition over maximum
      case (Max(e1, e2), _) if rewriteLeft => Max(rewritePlus(variable, e1, right), rewritePlus(variable, e2, right))
      case (_, Max(e1, e2)) if rewriteRight => Max(rewritePlus(variable, left, e1), rewritePlus(variable, left, e2))
      // distribute addition over conditional
      case (NonLeaf(b, e1, e2), _) if rewriteLeft => ConditionalExpression(b, rewritePlus(variable, e1, right), rewritePlus(variable, e2, right))
      case (_, NonLeaf(b, e1, e2)) if rewriteRight => ConditionalExpression(b, rewritePlus(variable, left, e1), rewritePlus(variable, left, e2))
      // default: do nothing
      case _ => Plus(left, right)
    } else {
      // leave unchanged if variable does not appear in addition
      Plus(left, right)
    }
  }

  private def rewriteMinus(variable: VariableIdentifier, left: Expression, right: Expression): Expression = {
    // check whether variable appears in subtraction
    val rewriteLeft = left.contains(_ == variable)
    val rewriteRight = right.contains(_ == variable)
    if (rewriteLeft || rewriteRight) (left, right) match {
      // distribute subtraction over minimum
      case (Min(e1, e2), _) if rewriteLeft => Min(rewriteMinus(variable, e1, right), rewriteMinus(variable, e2, right))
      case (_, Min(e1, e2)) if rewriteRight => Max(rewriteMinus(variable, left, e1), rewriteMinus(variable, left, e2))
      // distribute subtraction over maximum
      case (Max(e1, e2), _) if rewriteLeft => Max(rewriteMinus(variable, e1, right), rewriteMinus(variable, e2, right))
      case (_, Max(e1, e2)) if rewriteRight => Min(rewriteMinus(variable, left, e1), rewriteMinus(variable, left, e2))
      // distribute subtraction over conditional
      case (NonLeaf(b, e1, e2), _) if rewriteLeft => ConditionalExpression(b, rewriteMinus(variable, e1, right), rewriteMinus(variable, e2, right))
      case (_, NonLeaf(b, e1, e2)) if rewriteRight => ConditionalExpression(b, rewriteMinus(variable, left, e1), rewriteMinus(variable, left, e2))
      // push addition inside subtraction
      case (_,Plus(e1, e2)) if rewriteRight => rewriteMinus(variable, rewriteMinus(variable, left, e1), e2)
      // rewrite nested subtraction
      case (_, Minus(e1, e2)) if rewriteRight => rewriteMinus(variable, rewritePlus(variable, left, e2), e1)
      case (_, Negate(argument)) if rewriteRight => rewritePlus(variable, left, argument)
      // default: do nothing
      case _ => Minus(left, right)
    } else {
      // leave unchanged if variable does not appear in subtraction
      Minus(left, right)
    }
  }

  private def rewriteMin(variable: VariableIdentifier, left: Expression, right: Expression): Expression = {
    // check whether variable appears in minimum
    val rewriteLeft = left.contains(_ == variable)
    val rewriteRight = right.contains(_ == variable)
    if (rewriteLeft || rewriteRight) (left, right) match {
      // distribute minimum over maximum
      case (Min(e1, e2), _) if rewriteLeft => Max(rewriteMin(variable, e1, right), rewriteMin(variable, e2, right))
      case (_, Min(e1, e2)) if rewriteRight => Max(rewriteMin(variable, left, e1), rewriteMin(variable, left, e2))
      // distribute minimum over conditional
      case (NonLeaf(b, e1, e2), _) if rewriteLeft => ConditionalExpression(b, rewriteMin(variable, e1, right), rewriteMin(variable, e2, right))
      case (_, NonLeaf(b, e1, e2)) if rewriteRight => ConditionalExpression(b, rewriteMin(variable, left, e1), rewriteMin(variable, right, e2))
      // default: do nothing
      case _ => Min(left, right)
    } else {
      // leave unchanged if variable does not appear in minimum
      Min(left, right)
    }
  }

  def havoc(expression: Expression, access: Expression, over: Boolean): Expression = expression match {
    // havoc arithmetic expressions
    case Permission(_, _) | ReadParameter(_) => expression
    case ConditionalExpression(condition, left, No) =>
      val newCondition = havoc(condition, access, over)
      val newLeft = havoc(left, access, over)
      ConditionalExpression(newCondition, newLeft, No)
    case ConditionalExpression(condition, left, right) =>
      val newLeft = ConditionalExpression(havoc(condition, access, over), havoc(left, access, over), No)
      val newRight = ConditionalExpression(havoc(Not(condition), access, over), havoc(right, access, over), No)
      Max(newLeft, newRight)
    case Plus(left, right) => Plus(havoc(left, access, over), havoc(right, access, over))
    case Minus(left, right) => Minus(havoc(left, access, over), havoc(right, access, over))
    case Negate(argument) => Negate(havoc(argument, access, !over))
    case Min(left, right) => Min(havoc(left, access, over), havoc(right, access, over))
    case Max(left, right) => Max(havoc(left, access, over), havoc(right, access, over))
    // havoc boolean expressions
    case True | False => expression
    case Not(argument) => Not(havoc(argument, access, !over))
    case And(left, right) => And(havoc(left, access, over), havoc(right, access, over))
    case Or(left, right) => Or(havoc(left, access, over), havoc(right, access, over))
    case comparison@Comparison(_, _, _) =>
      var conditions = List.empty[Expression]
      val FieldAccessExpression(FunctionCallExpression(name, arguments, _), field) = access
      comparison.foreach {
        case FieldAccessExpression(FunctionCallExpression(`name`, args, _), `field`) =>
            val zipped = args zip arguments
            val condition = AndList(zipped.map { case (a, b) => Equal(a, b) })
            conditions = condition :: conditions

        case _ => // do nothing
      }

      if (conditions.isEmpty) comparison
      else {
        val disjunction = OrList(conditions)
        ConditionalExpression(disjunction, Literal(over), comparison)
      }
  }

  def approximateArithmetic(expression: Expression, over: Boolean): Expression = expression match {
    case Permission(_, _) | ReadParameter(_) => expression
    case ConditionalExpression(condition, left, No) =>
      val newCondition = approximateBoolean(condition, over)
      val newLeft = approximateArithmetic(left, over)
      ConditionalExpression(newCondition, newLeft, No)
    case ConditionalExpression(condition, left, right) =>
      val newLeft = ConditionalExpression(approximateBoolean(condition, over), approximateArithmetic(left, over), No)
      val newRight = ConditionalExpression(approximateBoolean(Not(condition), over), approximateArithmetic(right, over), No)
      Max(newLeft, newRight)
    case Plus(left, right) => Plus(approximateArithmetic(left, over), approximateArithmetic(right, over))
    case Minus(left, right) => Minus(approximateArithmetic(left, over), approximateArithmetic(right, !over))
    case Negate(argument) => Negate(approximateArithmetic(argument, !over))
    case Min(left, right) => Min(approximateArithmetic(left, over), approximateArithmetic(right, over))
    case Max(left, right) => Max(approximateArithmetic(left, over), approximateArithmetic(right, over))
  }

  def approximateBoolean(expression: Expression, over: Boolean): Expression = expression match {
    case True | False | _: VariableIdentifier => expression
    case Not(argument) => Not(approximateBoolean(argument, !over))
    case And(left, right) => And(approximateBoolean(left, over), approximateBoolean(right, over))
    case Or(left, right) => Or(approximateBoolean(left, over), approximateBoolean(right, over))
    case comparison@Comparison(_, _, _) =>
      val approximate = comparison.contains {
        case FieldAccessExpression(_, _) => true
        case _ => false
      }
      if (approximate) Literal(over)
      else comparison
    case ApproximatedExpression(variable, wrapped) => wrapped match {
      case Comparison(left, Divide(`variable`, Literal(v: Int)), operator) if over =>
        val comparisons = for (i <- 0 until v) yield {
          val expression = Plus(Times(Literal(v), left), Literal(i))
          val flipped = ArithmeticOperator.flip(operator)
          Comparison(variable, expression, flipped)
        }
        OrList(comparisons)
      case _ => Literal(over)
    }
  }

  private def normalize(variable: VariableIdentifier, expression: Expression): Expression = {
    val nnf = QpMath.toNnf(expression)
    val collected = collectVariable(variable, nnf)
    normalizeCoefficient(variable, collected)
  }

  // TODO: Approximate the currently not implemented cases
  def collectVariable(variable: VariableIdentifier, expression: Expression): Expression = expression.transform {
    case original@Divides(left, right) =>
      val collected = Collected(right)
      if (left.contains(_ == variable) || collected.rest.contains(_ == variable)) ApproximatedExpression(variable, original)
      else Divides(left, collected.toExpression(variable))
    case original@NotDivides(left, right) =>
      val collected = Collected(right)
      if (left.contains(_ == variable) || collected.rest.contains(_ == variable)) ApproximatedExpression(variable, original)
      else NotDivides(left, collected.toExpression(variable))
    case original@Comparison(left, right, operator) if left.contains(_ == variable) || right.contains(_ == variable) =>
      val collected = Collected(Minus(left, right))
      if (collected.rest.contains(_ == variable)) ApproximatedExpression(variable, original)
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
    * @param variable   The variable to emaxte.
    * @param expression The arithmetic expression.
    * @return A triple with the described properties.
    */
  private def analyzeArithmetic(variable: VariableIdentifier, expression: Expression): (Tuples, Expression, Int) = expression match {
    case _ if !expression.contains(_ == variable) => (Set.empty, expression, 1)
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
        // the condition is negated since we are interested in making it false
        val negated = QpMath.toNnf(Not(condition))
        val (set, projection, delta) = analyzeBoolean(variable, negated)
        (toTuples(set), Leaf(Not(projection), permission), delta)
      }
      (tuples1 ++ tuples2, Minus(projection1, projection2), lcm(delta1, delta2))
    case Negate(leaf@Leaf(_, _)) => analyzeArithmetic(variable, Minus(No, leaf))
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
    * The third value of the triple is the least common multiple of all values appearing in divisibility expressions.
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

case class ApproximatedExpression(variable: VariableIdentifier, expression: Expression)
  extends Expression {

  override def typ: Type = expression.typ

  override def transform(f: (Expression) => Expression): Expression = f(ApproximatedExpression(variable, expression.transform(f)))

  override def ids: IdentifierSet = expression.ids

  override def pp: ProgramPoint = expression.pp

  override def contains(f: (Expression) => Boolean): Boolean = f(this) || expression.contains(f)
}
