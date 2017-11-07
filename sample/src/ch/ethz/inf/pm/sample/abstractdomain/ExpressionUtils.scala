/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

object ExpSimplifier {
  import ArithmeticOperator.{`+`, `-`, `*`}

  /**
   * Simplifies the given Sample expressions, for example by replacing 'x + 0' with 'x'.
   *
   * It may change the semantics of the program in the presence of expressions
   * with side-effects and may cause a non-terminating expression to be
   * simplified to a terminating expressions. Thus, the simplifier is mostly
   * meant for debugging (output) purposes.
   *
   * The transformation should be language-agnostic. Thus, it should try
   * to make as few assumptions on the semantics of front-end languages
   * as possible.
   *
   * Since there are no "official" boolean literals, it currently does not
   * simplify boolean expressions.
   *
   * Feel free to add new matchers to the transformation in the future.
   * @param exp the expression to simplify
   * @return the simplified expression
   */
  def simplify(exp: Expression): Expression =
    exp.transform({
      // Push negations inward if possible
      case e @ NegatedBooleanExpression(negExp) => negExp match {
        case NegatedBooleanExpression(innerExp) => innerExp
        case Constant("true", typ) => Constant("false", typ)()
        case Constant("false", typ) => Constant("true", typ)()
        case BinaryArithmeticExpression(l, r, o) =>
          BinaryArithmeticExpression(l, r, ArithmeticOperator.negate(o))
        case ReferenceComparisonExpression(l, r, o) =>
          ReferenceComparisonExpression(l, r, ReferenceOperator.negate(o))
        case BinaryBooleanExpression(l, r, o) =>
          BinaryBooleanExpression(
            NegatedBooleanExpression(l),
            NegatedBooleanExpression(r),
            BooleanOperator.negate(o))
        case _ => e
      }

      // Boolean expressions
      case b@BinaryArithmeticExpression(left, right, op)
        if left.typ.isBooleanType && right.typ.isBooleanType &&
          (op == ArithmeticOperator.== || op == ArithmeticOperator.!=) =>
        (left, right, op) match {
          case (_, Constant("1", _), ArithmeticOperator.==) => left
          case (Constant("1", _), _, ArithmeticOperator.==) => right
          case (_, Constant("0", _), ArithmeticOperator.==) => NegatedBooleanExpression(left)
          case (Constant("0", _), _, ArithmeticOperator.==) => NegatedBooleanExpression(right)

          case (_, Constant("1", _), ArithmeticOperator.!=) => NegatedBooleanExpression(left)
          case (Constant("1", _), _, ArithmeticOperator.!=) => NegatedBooleanExpression(right)
          case (_, Constant("0", _), ArithmeticOperator.!=) => left
          case (Constant("0", _), _, ArithmeticOperator.!=) => right
          case _ => b
        }

      // Binary arithmetic expressions
      case BinaryArithmeticExpression(Constant("0", _), right, `+`) => right
      case BinaryArithmeticExpression(left, Constant("0", _), `+`) => left
      case BinaryArithmeticExpression(left, Constant("0", _), `-`) => left
      case BinaryArithmeticExpression(Constant("1", _), right, `*`) => right
      case BinaryArithmeticExpression(left, Constant("1", _), `*`) => left
      case BinaryArithmeticExpression(c@Constant("0", _), _, `*`) => c
      case BinaryArithmeticExpression(_, c@Constant("0", _), `*`) => c

      // Everything else
      case e => e
    })
}

object ExpPrettyPrinter extends Function[Expression, String] {

  /**
   * Pretty-prints the given expression.
   *
   * Changing the toString methods of expressions themselves is a bit risky,
   * because some applications may rely on the format. By doing it externally,
   * we get maximum freedom.
   *
   * @param exp the expression to pretty-print
   * @return the pretty string representation
   */
  def apply(exp: Expression): String = exp match {
    case BinaryArithmeticExpression(left, right, op) =>
      this(left) + " " + this(op) + " " + this(right)
    case Constant(value, _) => value
    case _ => exp.toString // Fallback
  }

  def apply(op: ArithmeticOperator.Value): String = op match {
    case ArithmeticOperator.`>=` => "≥"
    case ArithmeticOperator.`<=` => "≤"
    case ArithmeticOperator.`==` => "="
    case ArithmeticOperator.`!=` => "≠"
    case _ => op.toString
  }
}