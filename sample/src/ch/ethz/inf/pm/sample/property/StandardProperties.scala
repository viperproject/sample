package ch.ethz.inf.pm.sample.property

import ch.ethz.inf.pm.sample.oorepresentation.{FieldAccess, MethodCall, Statement}
import ch.ethz.inf.pm.sample.abstractdomain._

/**
 * Created by IntelliJ IDEA.
 * User: Pietro
 * Date: 22/06/11
 * Time: 16.27
 * To change this template use File | Settings | File Templates.
 */

object DivisionByZero extends Visitor {
  override def getLabel = "DivisionByZero";

  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) : Unit = statement match {
    case MethodCall(pp, FieldAccess(pp1, x :: Nil, "/", typ), parametricTypes, y :: Nil, returnedType) =>
      var state1 = x.forwardSemantics(state);
      state1 = y.forwardSemantics(state1);
      for(divisor <- state1.getExpression().getExpressions()) {
        if(! state1.assume(
          new SymbolicAbstractValue[S](
            new BinaryArithmeticExpression(divisor, new Constant("0", null, null), ArithmeticOperator.==, null),
            state1
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