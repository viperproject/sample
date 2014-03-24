package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron.{Coeff, DoubleScalar, Linterm1, Lincons1}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.{ArithmeticOperator => AOp}
import ch.ethz.inf.pm.sample.oorepresentation.{Type, DummyProgramPoint}
import scala.Some
import ch.ethz.inf.pm.sample.SystemParameters

/**
 * Translates Apron constraints to Sample expressions.
 *
 * @param apronInterface the Apron interface to translate
 * @param resultTransformer the function applied to the resulting expressions,
 *                          DefaultExpSimplifier by default.
 * @param boolType boolean type used to generate boolean expressions
 */
case class ApronInterfaceTranslator(
    resultTransformer: (Expression => Expression) = ExpSimplifier,
    boolType: Type = SystemParameters.typ.top())
    (val apronInterface: ApronInterface[_]) {

  /**
   * Translates all Apron constraints in the ApronInterface to a sequence
   * of Expressions.
   *
   * @return the equivalent expressions
   */
  def translateAll(): Set[Expression] = {
    apronInterface.state match {
      case Some(state) =>
        val linearConstraints = state.toLincons(apronInterface.domain)
        linearConstraints.map(translate).flatten.toSet
      case None => Set.empty
    }
  }

  /**
   * Translates a single linear Apron constraint to a Sample expression.
   * @param c the Apron constraint to translate
   * @return the constraint expressed as a Sample expression
   */
  def translate(c: Lincons1): Option[Expression] = {
    // Separate terms with positive and negative coefficients, such that we can
    // build a linear inequality whose terms only have positive coefficients.
    val nonZeroLinterms = c.getLinterms.filter(!_.getCoefficient.isZero)
    var (leftTerms, rightTerms) = nonZeroLinterms.partition(
      _.getCoefficient.cmp(new DoubleScalar(0)) > 0)

    // Negate all negative terms
    rightTerms = rightTerms.map(negateTerm)

    var leftExps = leftTerms.toList map translate
    var rightExps = rightTerms.toList map translate

    // Determine the type of expression. As a simple heuristic, just take the
    // type of the first expression
    val typ = (leftExps ++ rightExps).head.typ

    // If the left sequence of expressions is empty, flip the operator
    var op = translateOp(c.getKind)
    var const = c.getCst
    if (leftExps.isEmpty) {
      val tmpExps = leftExps
      leftExps = rightExps
      rightExps = tmpExps
      const = negateCoeff(const)
      op = ArithmeticOperator.flip(op)
    }

    // Add the constant to the LHS or RHS of the inequality,
    // depending on whether it is positive or negative
    val sampleConst = Constant(const.toString, typ, DummyProgramPoint)
    val sampleNegConst = Constant(negateCoeff(const).toString, typ, DummyProgramPoint)

    if (const.cmp(new DoubleScalar(0)) > 0) {
      if (rightExps.isEmpty) {
        // Prefer 'x = -1' over 'x + 1 = 0'
        rightExps ::= sampleNegConst
      } else {
        leftExps ::= sampleConst
      }
    } else if (const.cmp(new DoubleScalar(0)) < 0)
      rightExps ::= sampleNegConst

    val zero = Constant("0", typ, DummyProgramPoint)
    val result = BinaryArithmeticExpression(
      left = BinaryArithmeticExpression(leftExps, AOp.`+`, typ, zero),
      right = BinaryArithmeticExpression(rightExps, AOp.`+`, typ, zero),
      op = op,
      returntyp = boolType)

    if (typ.isBooleanType && (result.op != AOp.`==` && result.op != AOp.`!=`))
      None // Do not return boolean inequalities for the moment
    else
      Some(resultTransformer(result))
  }

  /**
   * Translates a single linear Apron term to a Sample expression.
   * @param t the Apron term to translate
   * @return the term expressed as a Sample expression
   */
  def translate(t: Linterm1): Expression = {
    val id = resolve(t.getVariable)
    val coeff = Constant(t.coeff.toString, id.typ, id.pp)
    BinaryArithmeticExpression(coeff, id, ArithmeticOperator.*, id.typ)
  }

  /** Translates an Apron operator to a Sample operator */
  private def translateOp(kind: Int): ArithmeticOperator.Value = kind match {
    case Lincons1.DISEQ => ArithmeticOperator.!=
    case Lincons1.EQ => ArithmeticOperator.==
    case Lincons1.SUP => ArithmeticOperator.>
    case Lincons1.SUPEQ => ArithmeticOperator.>=
  }

  /** Returns a negated version of the given coefficient */
  private def negateCoeff(c: Coeff): Coeff = {
    val negCoeff = c.copy
    negCoeff.neg()
    negCoeff
  }

  /** Returns the negated version of the given term */
  private def negateTerm(t: Linterm1): Linterm1 =
    new Linterm1(t.getVariable, negateCoeff(t.getCoefficient))

  /** Maps variable names used within Apron to Sample identifiers */
  private def resolve(variable: String): Identifier = idMap.get(variable) match {
    case Some(id) => id
    case None => sys.error(s"unknown variable $variable")
  }

  private val idMap = apronInterface.env.groupBy(_.getName).map { case (k,v) => {
      if (v.size > 1) sys.error(s"non-unique identifier name $k")
      else (k, v.toList(0))
    }
  }
}