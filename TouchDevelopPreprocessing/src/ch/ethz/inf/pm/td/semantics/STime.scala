package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_STime
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of time
 *
 * time and dates
 *
 * @author Lucas Brutschy
 */

object STime extends Default_STime {

  /** PRIVATE HANDLER FIELDS */
  lazy val field_every_frame_handler = ApiField("every frame handler", TAction)

  override def possibleFields = super.possibleFields ++ List(field_every_frame_handler)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {


    /** Attaches a handler to run on every time frame, roughly every 20ms. */
    case "on every frame" =>
      val List(perform) = parameters // Action
    val newState = AssignField[S](this0, STime.field_every_frame_handler, perform)
      New[S](TEvent_Binding)(newState, pp)

    /** Starts a timer to run ``perform`` after ``seconds`` seconds. */
    case "run after" =>
      val List(seconds, perform) = parameters // Number,Action
      New[S](TTimer, initials = Map(TTimer.field_is_active -> True, TTimer.field_trigger_handler -> perform))

    /** Starts a timer to run ``perform`` every ``seconds`` seconds. */
    case "run every" =>
      val List(seconds, perform) = parameters // Number,Action
      New[S](TTimer, initials = Map(TTimer.field_is_active -> True, TTimer.field_trigger_handler -> perform))

    /** Creates a new date instance */
    case "create" =>
      val List(year, month, day, hour, minute, second) = parameters // Number,Number,Number,Number,Number,Number
      New[S](TDateTime, Map(
        TDateTime.field_year -> year,
        TDateTime.field_month -> month,
        TDateTime.field_day -> day,
        TDateTime.field_hour -> hour,
        TDateTime.field_minute -> minute,
        TDateTime.field_second -> second
      ))

    /* [**obsolete**] Use `app->fail_if_not` instead. */
    case "fail if not" =>
      val List(condition) = parameters // Boolean
      if (TouchAnalysisParameters.printValuesInWarnings)
        Error[S](condition.not(), "fail if not", "fail if not " + condition + " might fail")
      else
        Error[S](condition.not(), "fail if not", "fail if not might fail")
      Skip

    /* Use `app->log` instead. */
    case "log" =>
      val List(message) = parameters // String
      Skip

    /** Gets the current time */
    case "now" =>
      Top[S](TDateTime)

    /** Waits for a specified amount of seconds */
    case "sleep" =>
      val List(seconds) = parameters // Number
      Skip

    /* Use `app->stop` instead. */
    case "stop" =>
      Exit[S]
      state.bottom()

    /** Stops the execution and leaves the wall. */
    case "stop and close" =>
      Exit[S]
      state.bottom()

    /** Gets today's date without time */
    case "today" =>
      Top[S](TDateTime)

    /** Gets tomorrow's date without time */
    case "tomorrow" =>
      Top[S](TDateTime)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}