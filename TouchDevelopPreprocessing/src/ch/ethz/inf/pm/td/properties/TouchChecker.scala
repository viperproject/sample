package ch.ethz.inf.pm.td.properties

import ch.ethz.inf.pm.sample.property.{ValidatedProgramPoint, WarningProgramPoint, OutputCollector, Visitor}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{FieldAccess, MethodCall, Statement}
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.oorepresentation.FieldAccess
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression
import ch.ethz.inf.pm.sample.oorepresentation.MethodCall
import scala._
import ch.ethz.inf.pm.sample.property.WarningProgramPoint
import ch.ethz.inf.pm.sample.abstractdomain.Constant
import ch.ethz.inf.pm.sample.property.ValidatedProgramPoint
import ch.ethz.inf.pm.sample.oorepresentation.FieldAccess
import ch.ethz.inf.pm.sample.abstractdomain.BinaryArithmeticExpression
import ch.ethz.inf.pm.sample.oorepresentation.MethodCall

/**
 * 
 * Lucas Brutschy
 * Date: 10/5/12
 * Time: 3:50 PM
 * 
 */
object TouchChecker extends Visitor {

  override def getLabel() = "TouchChecker"

  var checks:List[Visitor] = Nil

  def addVisitor(vis:Visitor) { checks = vis :: checks }

  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) {
    for(check <- checks) {
        check.checkSingleStatement(state,statement,printer)
    }
  }

}

class AssertChecker(label:String, method:String, property:Expression) extends Visitor {

  override def getLabel() = label

  def checkSingleStatement[S <: State[S]](state : S, statement : Statement, printer : OutputCollector) { statement match {

    case MethodCall(pp, FieldAccess(pp1, x :: Nil, method, typ), parametricTypes, y :: Nil, returnedType) =>
      var state1 = x.forwardSemantics(state);
      state1 = y.forwardSemantics(state1);
//      for(divisor <- state1.getExpression().getExpressions()) {
//        if(!state1.assume(new SymbolicAbstractValue[S](property)).lessEqual(state.bottom())) {
//          printer.add(new WarningProgramPoint(statement.getPC(), label+" might fail"))
//          return
//        }
//      }
      printer.add(new ValidatedProgramPoint(statement.getPC(), label+" is safe"))
    case _ =>
  }

}}