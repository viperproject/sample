
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.{MethodSummaries, TouchAnalysisParameters}

/**
 * Specifies the abstract semantics of App
 *
 * Interact with the app runtime
 *
 * @author Lucas Brutschy
 */

object SApp {

  val typName = "App"
  val typ = new TouchType(typName, isSingleton = true)

}

class SApp extends AAny {

  def getTyp = SApp.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Gets the binding of the current handler if any. This can be used to delete a handler from itself. */
    case "current handler" =>
      New[S](TEvent_Binding.typ) // deleting a handler is ignored anyways.

    /** Aborts the execution if the condition is false. */
    case "fail if not" =>
      val List(condition) = parameters // Boolean
      if (TouchAnalysisParameters.printValuesInWarnings)
        Error[S](condition.not(), "fail if not", "fail if not " + condition + " might fail")
      else
        Error[S](condition.not(), "fail if not", "fail if not might fail")
      Skip

    /** Appends this message to the debug log. Does nothing when the script is published. */
    case "log" =>
      val List(message) = parameters // String
      Skip

    /** Stops the app. */
    case "stop" =>
      val List() = parameters
      MethodSummaries.collectExit[S](state)

    // FIELDS: field_current_handler

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
