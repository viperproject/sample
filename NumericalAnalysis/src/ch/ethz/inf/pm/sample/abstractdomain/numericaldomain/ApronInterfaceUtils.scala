package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import apron.{Coeff, DoubleScalar, Linterm1, Lincons1}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.DummyProgramPoint
import scala.Some

/**
 * Translates Apron constraints to Sample expressions.
 *
 * @param env the set of identifiers that Apron constraint are built from
 * @param resultTransformer the function applied to the resulting expressions,
 *                          DefaultExpSimplifier by default.
 * @todo the returned expressions should have a boolean type,
 *       which the translator currently does not have access to
 */
class ApronInterfaceTranslator(
    val env: Set[Identifier],
    val resultTransformer: (Expression => Expression) = ExpSimplifier) {

  /**
   * Translates a single linear Apron constraint to a Sample expression.
   * @param c the Apron constraint to translate
   * @return the constraint expressed as a Sample expression
   */
  def translate(c: Lincons1): Expression = {
    // Separate terms with positive and negative coefficients, such that we can
    // build a linear inequality whose terms only have positive coefficients.
    var (leftTerms, rightTerms) = c.getLinterms.partition(
      _.getCoefficient.cmp(new DoubleScalar(0)) > 0)

    // Negate all negative terms
    rightTerms = rightTerms.map(negateTerm)

    var leftExps = leftTerms.toList map translate
    var rightExps = rightTerms.toList map translate

    // Determine the type of expression. As a simple heuristic, just take the
    // type of the first expression
    val typ = (leftExps ++ rightExps).head.getType

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
    if (const.cmp(new DoubleScalar(0)) > 0)
      leftExps = Constant(const.toString, typ, DummyProgramPoint) :: leftExps.toList
    else if (const.cmp(new DoubleScalar(0)) < 0)
      rightExps = Constant(negateCoeff(const).toString, typ, DummyProgramPoint) ::
        rightExps.toList

    val zero = Constant("0", typ, DummyProgramPoint)
    val result = BinaryArithmeticExpression(
      left = BinaryArithmeticExpression(leftExps, ArithmeticOperator.+, typ, zero),
      right = BinaryArithmeticExpression(rightExps, ArithmeticOperator.+, typ, zero),
      op = op,
      returntyp = typ) // TODO: Use boolean type here

    resultTransformer(result)
  }

  /**
   * Translates a single linear Apron term to a Sample expression.
   * @param t the Apron term to translate
   * @return the term expressed as a Sample expression
   */
  def translate(t: Linterm1): Expression = {
    val id = resolve(t.getVariable)
    val coeff = Constant(t.coeff.toString, id.getType, id.getProgramPoint)
    BinaryArithmeticExpression(coeff, id, ArithmeticOperator.*, id.getType)
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

  private val idMap = env.groupBy(_.getName()).map { case (k,v) => {
      if (v.size > 1) sys.error(s"non-unique identifier name $k")
      else (k, v.toList(0))
    }
  }
}

object ApronInterfaceTranslator {
  /**
   * Translates all Apron constraints in an ApronInterface to a sequence
   * of Expressions.
   *
   * @param a the interface to the Apron state
   * @return the equivalent expressions
   */
  def translate(a: ApronInterface): Set[Expression] =
    if (a.state.isDefined) {
      val translator = this(a)
      val linearConstraints = a.state.get.toLincons(a.domain)
      linearConstraints.map(translator.translate).toSet
    } else Set.empty

  /**
   * Build an ApronInterfaceTranslator that can translate individual
   * Apron constraints in a given ApronInterface.
   */
  def apply(a: ApronInterface): ApronInterfaceTranslator =
    new ApronInterfaceTranslator(a.env)
}