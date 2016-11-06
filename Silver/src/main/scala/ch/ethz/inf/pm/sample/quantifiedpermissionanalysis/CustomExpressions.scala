package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, ExpressionSet, IdentifierSet, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}

/**
  * @author Severin Münger
  *         Added on 05.11.16.
  */
class CustomExpressions {

}

case class ForallExpression(leftCond: Expression, right: Expression, varId: VariableIdentifier) extends Expression {
  /** The type of this expression. */
  override def typ: Type = leftCond.typ

  /** Point in the program where this expression is located. */
  override def pp: ProgramPoint = leftCond.pp

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = leftCond.ids ++ right.ids

  /** Runs f on the expression and all sub-expressions
    *
    * This also replaces identifiers inside heap ID sets.
    *
    * @param f the transformer
    * @return the transformed expression
    */
  override def transform(f: (Expression) => Expression): Expression = ForallExpression(leftCond.transform(f), right.transform(f), varId)

  /** Checks if function f evaluates to true for any sub-expression. */
  override def contains(f: (Expression) => Boolean): Boolean = leftCond.contains(f) || right.contains(f)
}

case class FunctionCallExpression(typ: Type,
                                  pp: ProgramPoint = DummyProgramPoint,
                                  name: String,
                                  parameters: List[Expression] = List())
  extends Expression {

  /** All identifiers that are part of this expression. */
  override def ids: IdentifierSet = parameters.foldLeft[IdentifierSet](IdentifierSet.Bottom)((ids, param) => ids ++ param.ids)

  /** Runs f on the expression and all sub-expressions
    *
    * This also replaces identifiers inside heap ID sets.
    *
    * @param f the transformer
    * @return the transformed expression
    */
  override def transform(f: (Expression) => Expression): Expression =
  FunctionCallExpression(typ, pp, name, parameters.map(param => param.transform(f)))

  /** Checks if function f evaluates to true for any sub-expression. */
  override def contains(f: (Expression) => Boolean): Boolean = f(this) || parameters.exists(param => param.contains(f))
}