package graph

import ch.ethz.inf.pm.sample.abstractdomain._

/**
 * Created with IntelliJ IDEA.
 * User: milos
 * Date: 6/13/13
 * Time: 7:51 PM
 * To change this template use File | Settings | File Templates.
 */
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
    case NegatedBooleanExpression(e) => e
    case BinaryArithmeticExpression(l,r,o,t) => {
      new BinaryArithmeticExpression(negateExpression(l),negateExpression(r), negateArithmeticOperator(o), t)
    }
    case BinaryBooleanExpression(l,r,o,t) => {
      new BinaryBooleanExpression(negateExpression(l), negateExpression(r), negateBoolOperator(o), t)
    }
    case TrueExpression(pp, t) => new FalseExpression(pp, t)
    case FalseExpression(pp, t) => new TrueExpression(pp, t)
    case ReferenceComparisonExpression(l,r,o,t) => new ReferenceComparisonExpression(l,r, negateArithmeticOperator(o), t)
    case x => x
  }


  private def negateArithmeticOperator(op: ArithmeticOperator.Value): ArithmeticOperator.Value = op match {
    case ArithmeticOperator.<= => ArithmeticOperator.>
    case ArithmeticOperator.< => ArithmeticOperator.>=
    case ArithmeticOperator.>= => ArithmeticOperator.<
    case ArithmeticOperator.== => ArithmeticOperator.!=
    case ArithmeticOperator.!= => ArithmeticOperator.==
    case ArithmeticOperator.> => ArithmeticOperator.<=
    case x => x
  }

  private def negateBoolOperator(op: BooleanOperator.Value): BooleanOperator.Value = op match {
    case BooleanOperator.&& => BooleanOperator.||
    case BooleanOperator.|| => BooleanOperator.&&
    case _ => throw new Exception("Invalid boolean operator.")
  }

}
