
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichNativeSemantics, MethodSummaries, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.defsemantics.Default_SApp
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of App
 *
 * Interact with the app runtime
 *
 * @author Lucas Brutschy
 */

object SApp extends Default_SApp {

  /** Never used: Aborts the execution if the condition is false. */
  override def member_fail_if_not = new ApiMember("fail if not", List(ApiParam(TBoolean)), ApiParam(this), TNothing, new ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, member:ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      val List(condition) = parameters // Boolean
      if (TouchAnalysisParameters.get.printValuesInWarnings)
        Error[S](condition.not(), "fail if not", "fail if not " + condition + " might fail")
      else
        Error[S](condition.not(), "fail if not", "fail if not might fail")
      Skip
    }
  })

  //  lazy val field_server_response = ApiField("server response",TServer_Response)
  //  lazy val field_server_request = ApiField("server request",TServer_Request)
  //  lazy val field_env = ApiField("env",TApp_Env)
  //  lazy val field_editor = ApiField("editor",TEditor)
  //  lazy val field_current_handler = ApiField("current handler",TEvent_Binding)

  //  override lazy val possibleFields:Set[ApiField] = super.possibleFields ++ Set(
  //    field_server_response,
  //    field_server_request,
  //    field_env,
  //    field_editor,
  //    field_current_handler
  //  )

}
      
