
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionFactory, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint}
import RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.sample.{SystemParameters}
import scala.Some
import ch.ethz.inf.pm.td.analysis.interpreter._
import ch.ethz.inf.pm.td.analysis.backward.CollectingState
import ch.ethz.inf.pm.sample.execution.{ForwardInterpreter, AbstractErrorInfo}

/**
 * Specifies the abstract semantics of Contract
 *
 * Correctness helpers
 *
 * @author Lucas Brutschy
 */ 

object SContract {

  val typName = "Contract"
  val typ = DefaultTouchType(typName,isSingleton = true)

}

class SContract extends AAny {

  def getTyp = SContract.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** Specifies a precondition contract for the action; if the condition is false, execution fails. Does nothing for published scripts. */
    case "requires" =>
      val List(condition,message) = parameters // Boolean,String
      if (TouchAnalysisParameters.printValuesInWarnings)
        Error[S](condition.not(), "requires", "Precondition "+condition+" does not hold!")
      else
        Error[S](condition.not(), "requires", "Precondition does not hold!")

    /** Checks for a condition; if the condition is false, execution fails. Does nothing for published scripts. */
    case "assert" =>
      val List(condition,message) = parameters // Boolean,String
      if (TouchAnalysisParameters.printValuesInWarnings)
        Error[S](condition.not(), "assert", "Assertion "+condition+" does not hold!")
      else
        Error[S](condition.not(), "assert", "Assertion does not hold!")

    case "backwards" =>
      val List(errorExpr) = parameters
      if(!state.isInstanceOf[CollectingState]) {
        MethodSummaries.currentForwardPos match {
          case Some(cfgPosition) =>
            val errorState = state.assume(errorExpr).setExpression(ExpressionFactory.unitExpr)

            val locationContext = SystemParameters.analysisUnitContext
            val backwardContext = SystemParameters.backwardContext
            val error = AbstractErrorInfo(pp, locationContext.method, errorExpr, state, errorState, cfgPosition)
            backwardContext.registerError(error)
          case None =>
            throw new Exception("Current position must be set by forward interpreter")
        }
      }
      UnitResult(state,pp)

    case "assertBackwards" =>
      Skip

    case "assumeBackwards" =>
      Skip

    case "assume" =>
      val List(condition) = parameters
      val r = Assume(condition)
      UnitResult(r,pp)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

  override def backwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet],
                                                returnedType: TouchType)(implicit pp: ProgramPoint, state: S, oldPreState: S): S = {
    method match {
      case "backwards" =>
        Skip(state, pp)

      case "assert" =>
        Skip(state, pp)

      /** Checks for a condition; if the condition is false, execution fails. Does nothing for published scripts. */
      case "assertBackwards" =>
        val List(condition,message) = parameters // Boolean,String
        val s = BackwardError[S](condition.not(), "Backward assertion " + condition + " does not hold")(state, pp)
        Skip(s,pp)

      case "assumeBackwards" =>
        val List(condition) = parameters
        val r = Assume(condition)(state, pp)
        UnitResult(r,pp)

      case "assume" =>
        Skip(state, pp)


      case _ => super.backwardSemantics(this0, method, parameters, returnedType)(pp, state, oldPreState)
    }
  }

  override def concreteSemantics(this0: TouchValue, method: String, params: List[TouchValue], interpreter: ConcreteInterpreter,
                        pp: ProgramPoint): TouchValue = {

    val state = interpreter.state

    method match {
      case "assert" =>
        params match {
          case List(BooleanV(cond), StringV(message)) =>
            interpreter.assertE(cond, detailedMessage = Some(message))(pp)
            UnitV
        }
      case "assertBackwards" => UnitV
      case "backwards" => UnitV
      case "assume" => UnitV

    }

  }
}
      
