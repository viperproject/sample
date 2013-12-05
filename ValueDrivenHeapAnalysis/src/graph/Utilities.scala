package graph

import ch.ethz.inf.pm.sample.abstractdomain._

object Utilities {

  val BinaryBoolArithmeticOperators = Set(ArithmeticOperator.>,
                                    ArithmeticOperator.>=,
                                    ArithmeticOperator.<,
                                    ArithmeticOperator.!=,
                                    ArithmeticOperator.==,
                                    ArithmeticOperator.<=)
  val BinaryBoolOperators = Set(BooleanOperator.||, BooleanOperator.&&)

  def removeVariablesFromState[S <: SemanticDomain[S]](state: S, ids: Set[Identifier]): S = {
    var result = state
    for (id <- ids) {
      result = result.removeVariable(id)
    }
    return result
  }

  def createVariablesForState[S <: SemanticDomain[S]](state: S, ids: Set[Identifier]): S = {
    var result = state
    for (id <- ids -- state.getIds())
      result = result.createVariable(id, id.getType())
    return result
  }

  def negateExpression(exp: Expression): Expression = exp match {
    // TODO(severinh): Code is similar to ApronInterface.assume. Code sharing may be possible.
    case NegatedBooleanExpression(e) => e
    case BinaryArithmeticExpression(l,r,o,t) => {
      new BinaryArithmeticExpression(negateExpression(l),negateExpression(r), ArithmeticOperator.negate(o), t)
    }
    case BinaryBooleanExpression(l,r,o,t) => {
      new BinaryBooleanExpression(negateExpression(l), negateExpression(r), negateBoolOperator(o), t)
    }
    case TrueExpression(pp, t) => new FalseExpression(pp, t)
    case FalseExpression(pp, t) => new TrueExpression(pp, t)
    case ReferenceComparisonExpression(l,r,o,t) => new ReferenceComparisonExpression(l,r, ArithmeticOperator.negate(o), t)
    case x => x
  }

  private def negateBoolOperator(op: BooleanOperator.Value): BooleanOperator.Value = op match {
    case BooleanOperator.&& => BooleanOperator.||
    case BooleanOperator.|| => BooleanOperator.&&
    case _ => throw new Exception("Invalid boolean operator.")
  }

}
