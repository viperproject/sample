package ch.ethz.inf.pm.sample.property

import ch.ethz.inf.pm.sample.oorepresentation.{FieldAccess, MethodCall, Statement}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.SystemParameters

/**
 * Created by IntelliJ IDEA.
 * User: Pietro
 * Date: 22/06/11
 * Time: 16.27
 * To change this template use File | Settings | File Templates.
 */

object DivisionByZero extends Visitor {
  override def getLabel = "DivisionByZero"

  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) : Unit = statement match {
    case MethodCall(pp, FieldAccess(pp1, x, "/", typ), parametricTypes, y :: Nil, returnedType) =>
      var state1 = x.forwardSemantics(state)
      state1 = y.forwardSemantics(state1)
      for(divisor <- state1.expr.getSetOfExpressions) {
        if(! state1.assume(
          new ExpressionSet(SystemParameters.getType().top()).add(
            new BinaryArithmeticExpression(divisor, new Constant("0"), ArithmeticOperator.==)
            )
           ).lessEqual(state.bottom())) {
          printer.add(new WarningProgramPoint(statement.getPC(), "Possible division by 0"))
          return
        }
      }
      printer.add(new ValidatedProgramPoint(statement.getPC(), "Safe division"))
    case _ =>
  }
}