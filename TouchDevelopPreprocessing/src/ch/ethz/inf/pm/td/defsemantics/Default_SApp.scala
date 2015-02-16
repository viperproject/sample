
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of App
 *
 * Interact with the app runtime
 *
 * @author Lucas Brutschy
 */

trait Default_SApp extends ASingleton {

  lazy val typeName = TypeName("App")
          
  /** Never used: Creates a specialized logger */
  def member_create_logger = ApiMember(
    name = "create logger",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TApp_Logger,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the binding of the current handler if any. This can be used to delete a handler from itself. */
  def member_current_handler = ApiMember(
    name = "current handler",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Get the Editor interface */
  def member_editor = ApiMember(
    name = "editor",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TEditor,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Access various properties of application environment */
  def member_env = ApiMember(
    name = "env",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TApp_Env,
    semantics = DefaultSemantics
  )

  /** Never used: Aborts the execution if the condition is false. */
  def member_fail_if_not = ApiMember(
    name = "fail if not",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Imports a dependent package which may be versioned. Package managers may be Node.JS npm, Apache cordova, Python Pip and TouchDevelop plugins. */
  def member_import_ = ApiMember(
    name = "import",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: When exported, run `script` instead of the body of the action */
  def member_javascript_async = ApiMember(
    name = "javascript async",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: When exported, run `script` instead of the body of the action */
  def member_javascript = ApiMember(
    name = "javascript",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Appends this message to the debug log. */
  def member_log = ApiMember(
    name = "log",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Return runtime information about functions and types defined in script and its libraries */
  def member_reflect = ApiMember(
    name = "reflect",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: Restarts the app and pops a restart dialog */
  def member_restart = ApiMember(
    name = "restart",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Runs a shell command. This action is only available when the script is running from a local web server. */
  def member_run_command = ApiMember(
    name = "run command",
    paramTypes = List(ApiParam(TString), ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Get the current incomming HTTP web request */
  def member_server_request = ApiMember(
    name = "server request",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TServer_Request,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Get the response corresponding to the current incomming HTTP web request */
  def member_server_response = ApiMember(
    name = "server response",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TServer_Response,
    semantics = DefaultSemantics
  )

  /** Never used: When exported server-side, retreives the value of a setting stored on the server. If not optional, fails if missing. Returns invalid if missing. */
  def member_server_setting = ApiMember(
    name = "server setting",
    paramTypes = List(ApiParam(TString), ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Shows a dialog with the logs */
  def member_show_logs = ApiMember(
    name = "show logs",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Stops the app. */
  def member_stop = ApiMember(
    name = "stop",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "create logger" -> member_create_logger,
    "current handler" -> member_current_handler,
    "editor" -> member_editor,
    "env" -> member_env,
    "fail if not" -> member_fail_if_not,
    "import" -> member_import_,
    "javascript async" -> member_javascript_async,
    "javascript" -> member_javascript,
    "log" -> member_log,
    "reflect" -> member_reflect,
    "restart" -> member_restart,
    "run command" -> member_run_command,
    "server request" -> member_server_request,
    "server response" -> member_server_response,
    "server setting" -> member_server_setting,
    "show logs" -> member_show_logs,
    "stop" -> member_stop
  )
            

}
          
