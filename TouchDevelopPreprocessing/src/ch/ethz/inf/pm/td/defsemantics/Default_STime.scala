
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Time
 *
 * time and dates
 *
 * @author Lucas Brutschy
 */

trait Default_STime extends ASingleton {

  lazy val typeName = TypeName("Time", isSingleton = true)
          
  /** Sometimes used: Creates a new date instance */
  def member_create = ApiMember(
    name = "create",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] Use `app->fail_if_not` instead. */
  def member_fail_if_not = ApiMember(
    name = "fail if not",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Use `app->log` instead. */
  def member_log = ApiMember(
    name = "log",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Gets the current time */
  def member_now = ApiMember(
    name = "now",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches a handler to run on every time frame, roughly every 20ms. */
  def member_on_every_frame = ApiMember(
    name = "on every frame",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Starts a timer to run ``perform`` after ``seconds`` seconds. */
  def member_run_after = ApiMember(
    name = "run after",
    paramTypes = List(ApiParam(TNumber), ApiParam(TAction)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TTimer,
    semantics = DefaultSemantics
  )

  /** Never used: Starts a timer to run ``perform`` every ``seconds`` seconds. */
  def member_run_every = ApiMember(
    name = "run every",
    paramTypes = List(ApiParam(TNumber), ApiParam(TAction)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TTimer,
    semantics = DefaultSemantics
  )

  /** Frequently used: Waits for a specified amount of seconds */
  def member_sleep = ApiMember(
    name = "sleep",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Use `app->stop` instead. */
  def member_stop_and_close = ApiMember(
    name = "stop and close",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Use `app->stop` instead. */
  def member_stop = ApiMember(
    name = "stop",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets today's date without time */
  def member_today = ApiMember(
    name = "today",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets tomorrow's date without time */
  def member_tomorrow = ApiMember(
    name = "tomorrow",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "create" -> member_create,
    "fail if not" -> member_fail_if_not,
    "log" -> member_log,
    "now" -> member_now,
    "on every frame" -> member_on_every_frame,
    "run after" -> member_run_after,
    "run every" -> member_run_every,
    "sleep" -> member_sleep,
    "stop and close" -> member_stop_and_close,
    "stop" -> member_stop,
    "today" -> member_today,
    "tomorrow" -> member_tomorrow
  )
            

}
          
