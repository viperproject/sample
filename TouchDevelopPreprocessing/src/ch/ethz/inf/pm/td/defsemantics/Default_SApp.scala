/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

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

  lazy val typeName = TypeName("App", isSingleton = true)

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "allow other events" -> member_allow_other_events,
    "consume rendered comments" -> member_consume_rendered_comments,
    "create logger" -> member_create_logger,
    "current handler" -> member_current_handler,
    "editor" -> member_editor,
    "env" -> member_env,
    "fail if not" -> member_fail_if_not,
    "host exec" -> member_host_exec,
    "host subscribe" -> member_host_subscribe,
    "import" -> member_import_,
    "javascript async" -> member_javascript_async,
    "javascript" -> member_javascript,
    "log" -> member_log,
    "reflect" -> member_reflect,
    "restart" -> member_restart,
    "run command" -> member_run_command,
    "script id" -> member_script_id,
    "server request" -> member_server_request,
    "server response" -> member_server_response,
    "server setting" -> member_server_setting,
    "show logs" -> member_show_logs,
    "stop" -> member_stop,
    "thumb" -> member_thumb
  )

  /** Never used: Allow execution of other events, before the current event finishes. */
  def member_allow_other_events = ApiMember(
    name = "allow other events",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Get HTML-rendered content of all comments 'executed' since last call */
  def member_consume_rendered_comments = ApiMember(
    name = "consume rendered comments",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

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

  /** Never used: [**beta**] Invokes the host to execute a command described in the message and returns the response. There is no restriction on the format of the request and response. If not available or errored, returns invalid. */
  def member_host_exec = ApiMember(
    name = "host exec",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    pausesInterpreter = true,
    isAsync = true,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Invokes the host to register an event listener described in the message. */
  def member_host_subscribe = ApiMember(
    name = "host subscribe",
    paramTypes = List(ApiParam(TString), ApiParam(TText_Action)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    pausesInterpreter = true,
    isAsync = true,
    semantics = DefaultSemantics
  )

  /** Never used: Imports a dependent package which may be versioned. Package managers may be Node.JS npm, Bower, Apache cordova, Python Pip and TouchDevelop plugins. ``bower`` and ``client`` imports are not available within the touchdevelop.com domain. */
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
    pausesInterpreter = true,
    isAsync = true,
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
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Runs a shell command. This action is only available when the script is running from a local web server. */
  def member_run_command = ApiMember(
    name = "run command",
    paramTypes = List(ApiParam(TString), ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    pausesInterpreter = true,
    isAsync = true,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the app script id if any; invalid if not available */
  def member_script_id = ApiMember(
    name = "script id",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
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
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Stops the app. */
  def member_stop = ApiMember(
    name = "stop",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: When compiled to ARM Thumb, inline the body. */
  def member_thumb = ApiMember(
    name = "thumb",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )
            

}
          
