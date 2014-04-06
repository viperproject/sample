package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.analysis.interpreter.{NumberV, ConcreteInterpreter, TouchValue}

/**
 * Specifies the abstract semantics of time
 *
 * time and dates
 *
 * @author Lucas Brutschy
 */

object STime {

  /** PRIVATE HANDLER FIELDS */
  val field_every_frame_handler = new TouchField("every frame handler", TAction.typName)

  val typName = "Time"
  val typ = DefaultTouchType(typName, isSingleton = true, fields = List(field_every_frame_handler))

}

class STime extends AAny {

  def getTyp = STime.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {


    /** Attaches a handler to run on every time frame, roughly every 20ms. */
    case "on every frame" =>
      val List(perform) = parameters // Action
    val newState = AssignField[S](this0, STime.field_every_frame_handler, perform)
      New[S](TEvent_Binding.typ)(newState, pp)

    /** Starts a timer to run ``perform`` after ``seconds`` seconds. */
    case "run after" =>
      val List(seconds, perform) = parameters // Number,Action
      New[S](TTimer.typ, initials = Map(TTimer.field_is_active -> True, TTimer.field_trigger_handler -> perform))

    /** Starts a timer to run ``perform`` every ``seconds`` seconds. */
    case "run every" =>
      val List(seconds, perform) = parameters // Number,Action
      New[S](TTimer.typ, initials = Map(TTimer.field_is_active -> True, TTimer.field_trigger_handler -> perform))

    /** Creates a new date instance */
    case "create" =>
      val List(year, month, day, hour, minute, second) = parameters // Number,Number,Number,Number,Number,Number
      New[S](TDateTime.typ, Map(
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
      Top[S](TDateTime.typ)

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
      Top[S](TDateTime.typ)

    /** Gets tomorrow's date without time */
    case "tomorrow" =>
      Top[S](TDateTime.typ)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

  override def concreteSemantics(this0: TouchValue,
                                 method: String,
                                 params: List[TouchValue],
                                 interpreter: ConcreteInterpreter,
                                 pp: ProgramPoint): TouchValue = method match {

    case "today" =>
      import com.github.nscala_time.time.Imports._

      val state = interpreter.state
      val now = DateTime.now

      state.createObjectWithTouchFields(TDateTime.typ, Map(
        TDateTime.field_day -> NumberV(now.getDayOfMonth),
        TDateTime.field_month -> NumberV(now.getMonthOfYear),
        TDateTime.field_year -> NumberV(now.getYear)
      ))

  }
}