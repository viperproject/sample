package ch.ethz.inf.pm.sample.abstractdomain

object ExpSimplifier extends Function[Expression, Expression] {
  import ArithmeticOperator.{`+`, `-`, `*`}

  /**
   * Simplifies the tiven Sample expressions, for example by replacing 'x + 0' with 'x'.
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
  def apply(exp: Expression): Expression =
    exp.transform({
      case NegatedBooleanExpression(NegatedBooleanExpression(innerExp)) => innerExp

      // Binary arithmetic expressions
      case BinaryArithmeticExpression(Constant("0", _, _), right, `+`, typ) => right
      case BinaryArithmeticExpression(left, Constant("0", _, _), `+`, typ) => left
      case BinaryArithmeticExpression(left, Constant("0", _, _), `-`, typ) => left
      case BinaryArithmeticExpression(Constant("1", _, _), right, `*`, typ) => right
      case BinaryArithmeticExpression(left, Constant("1", _, _), `*`, typ) => left
      case BinaryArithmeticExpression(Constant("0", _, _), _, `*`, typ) =>
        Constant("0", typ, exp.getProgramPoint)
      case BinaryArithmeticExpression(_, Constant("0", _, _), `*`, typ) =>
        Constant("0", typ, exp.getProgramPoint)

      // Everything else
      case e => e
    })
}

object ExpPrettyPrinter extends Function[Expression, String] {
  import ArithmeticOperator._

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
    case BinaryArithmeticExpression(left, right, op, _) =>
      this(left) + " " + this(op) + " " + this(right)
    case Constant(value, _, _) => value
    case _ => exp.toString // Fallback
  }

  def apply(op: ArithmeticOperator.Value): String = op match {
    case `>=` => "≥"
    case `<=` => "≤"
    case `==` => "="
    case `!=` => "≠"
    case _ => op.toString
  }
}