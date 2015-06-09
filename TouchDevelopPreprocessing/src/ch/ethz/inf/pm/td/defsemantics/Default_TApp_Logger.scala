
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of App Logger
 *
 * A custom logger
 *
 * @author Lucas Brutschy
 */

trait Default_TApp_Logger extends AAny {

  lazy val typeName = TypeName("App Logger")
          
  /** Never used: [**beta**] How long the current context has been executing for in milliseconds. */
  def member_context_duration = ApiMember(
    name = "context duration",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] The unique id of current context, or empty if in global scope. */
  def member_context_id = ApiMember(
    name = "context id",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Stop counting time in all current contexts */
  def member_context_pause = ApiMember(
    name = "context pause",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Start counting time again in all current contexts */
  def member_context_resume = ApiMember(
    name = "context resume",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Get the userid attached to the current context, or empty. */
  def member_context_user = ApiMember(
    name = "context user",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Log a custom event tick, including specified meta information, in any registered performance logger. */
  def member_custom_tick = ApiMember(
    name = "custom tick",
    paramTypes = List(ApiParam(TString), ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Logs a debug message */
  def member_debug = ApiMember(
    name = "debug",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Ends a time sub-logger and reports the time. */
  def member_end = ApiMember(
    name = "end",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Logs an error message */
  def member_error = ApiMember(
    name = "error",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Logs an informational message */
  def member_info = ApiMember(
    name = "info",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Logs a new message with optional metadata. The level follows the syslog convention. */
  def member_log = ApiMember(
    name = "log",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] How long the current logger has been executing for in milliseconds. */
  def member_logger_duration = ApiMember(
    name = "logger duration",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Log a measure in any registered performance logger. */
  def member_measure = ApiMember(
    name = "measure",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Start new logging context when you're starting a new task (eg, handling a request) */
  def member_new_context = ApiMember(
    name = "new context",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Get the userid attached to the current context, or empty. */
  def member_set_context_user = ApiMember(
    name = "set context user",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Set minimum logging level for this logger (defaults to "debug"). */
  def member_set_verbosity = ApiMember(
    name = "set verbosity",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Starts a timed sub-logger. The task name is concatenated to the current logger category. */
  def member_start = ApiMember(
    name = "start",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TApp_Logger,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Log a custom event tick in any registered performance logger. */
  def member_tick = ApiMember(
    name = "tick",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Get the current logging level for this logger (defaults to "debug"). */
  def member_verbosity = ApiMember(
    name = "verbosity",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Logs a warning message */
  def member_warning = ApiMember(
    name = "warning",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "context duration" -> member_context_duration,
    "context id" -> member_context_id,
    "context pause" -> member_context_pause,
    "context resume" -> member_context_resume,
    "context user" -> member_context_user,
    "custom tick" -> member_custom_tick,
    "debug" -> member_debug,
    "end" -> member_end,
    "error" -> member_error,
    "info" -> member_info,
    "log" -> member_log,
    "logger duration" -> member_logger_duration,
    "measure" -> member_measure,
    "new context" -> member_new_context,
    "set context user" -> member_set_context_user,
    "set verbosity" -> member_set_verbosity,
    "start" -> member_start,
    "tick" -> member_tick,
    "verbosity" -> member_verbosity,
    "warning" -> member_warning
  )
            

}
          
