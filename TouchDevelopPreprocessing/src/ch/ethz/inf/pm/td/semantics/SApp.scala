
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{RichNativeSemantics, MethodSummaries, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of App
 *
 * Interact with the app runtime
 *
 * @author Lucas Brutschy
 */

object SApp extends ASingleton {

  lazy val typeName = TypeName("App")

  /** Never used: [**dbg**] When exported client-side, adds the Cordova plugin to the client build */
  lazy val member_cordova_import = new ApiMember("cordova import", List(ApiParam(TString), ApiParam(TString)), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: Creates a specialized logger */
  lazy val member_create_logger = new ApiMember("create logger", List(ApiParam(TString)), ApiParam(this), TApp_Logger) with DefaultSemantics

  /** Never used: Gets the binding of the current handler if any. This can be used to delete a handler from itself. */
  lazy val member_current_handler = new ApiMember("current handler", List(), ApiParam(this), TEvent_Binding) with DefaultSemantics

  /** Never used: [**beta**] Get the Editor interface */
  lazy val member_editor = new ApiMember("editor", List(), ApiParam(this), TEditor) with DefaultSemantics

  /** Never used: [**beta**] Access various properties of application environment */
  lazy val member_env = new ApiMember("env", List(), ApiParam(this), TApp_Env) with DefaultSemantics

  /** Never used: Aborts the execution if the condition is false. */
  lazy val member_fail_if_not = new ApiMember("fail if not", List(ApiParam(TBoolean)), ApiParam(this), TNothing) {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      val List(condition) = parameters // Boolean
      if (TouchAnalysisParameters.printValuesInWarnings)
        Error[S](condition.not(), "fail if not", "fail if not " + condition + " might fail")
      else
        Error[S](condition.not(), "fail if not", "fail if not might fail")
      Skip
    }
  }

  /** Never used: [**dbg**] When exported server-side, run `script` instead of the body of the action */
  lazy val member_javascript_async = new ApiMember("javascript async", List(ApiParam(TString), ApiParam(TString)), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: [**dbg**] When exported server-side, make `module` available in `app->javascript` calls; if `version` is non-empty it will be installed with npm */
  lazy val member_javascript_import = new ApiMember("javascript import", List(ApiParam(TString), ApiParam(TString)), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: [**dbg**] When exported server-side, run `script` instead of the body of the action */
  lazy val member_javascript = new ApiMember("javascript", List(ApiParam(TString), ApiParam(TString)), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: Appends this message to the debug log. */
  lazy val member_log = new ApiMember("log", List(ApiParam(TString)), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: [**dbg**] Return runtime information about functions and types defined in script and its libraries */
  lazy val member_reflect = new ApiMember("reflect", List(ApiParam(TString)), ApiParam(this), TJson_Object) with DefaultSemantics

  /** Never used: Restarts the app and pops a restart dialog */
  lazy val member_restart = new ApiMember("restart", List(ApiParam(TString)), ApiParam(this), TNothing) with DefaultSemantics

  /** Never used: [**beta**] Get the current incomming HTTP web request */
  lazy val member_server_request = new ApiMember("server request", List(), ApiParam(this), TServer_Request) with DefaultSemantics

  /** Never used: [**beta**] Get the response corresponding to the current incomming HTTP web request */
  lazy val member_server_response = new ApiMember("server response", List(), ApiParam(this), TServer_Response) with DefaultSemantics

  /** Never used: [**beta**] When exported server-side, retreives the value of a setting stored on the server. If not optional, fails if missing. Returns invalid if missing. */
  lazy val member_server_setting = new ApiMember("server setting", List(ApiParam(TString), ApiParam(TBoolean)), ApiParam(this), TString) with DefaultSemantics

  /** Never used: Stops the app. */
  lazy val member_stop = new ApiMember("stop", List(), ApiParam(this), TNothing) {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      MethodSummaries.collectExit[S](state)
    }
  }

  override lazy val declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "cordova import" -> member_cordova_import,
    "create logger" -> member_create_logger,
    "current handler" -> member_current_handler,
    "editor" -> member_editor,
    "env" -> member_env,
    "fail if not" -> member_fail_if_not,
    "javascript async" -> member_javascript_async,
    "javascript import" -> member_javascript_import,
    "javascript" -> member_javascript,
    "log" -> member_log,
    "reflect" -> member_reflect,
    "restart" -> member_restart,
    "server request" -> member_server_request,
    "server response" -> member_server_response,
    "server setting" -> member_server_setting,
    "stop" -> member_stop
  )

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
      
