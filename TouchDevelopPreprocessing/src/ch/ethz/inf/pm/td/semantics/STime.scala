/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics, TopInitializer, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiMemberSemantics, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_STime
import RichNativeSemantics._
import ch.ethz.inf.pm.td.semantics.AAction.EnableSemantics

/**
 * Specifies the abstract semantics of time
 *
 * time and dates
 *
 * @author Lucas Brutschy
 */

object STime extends Default_STime {

  /** PRIVATE HANDLER FIELDS */
  lazy val field_every_frame_handler = ApiField("every frame handler", TAction, TopInitializer)

  override def member_on_every_frame =
    super.member_on_every_frame.copy(semantics = AAction.EnableSemantics(STime.field_every_frame_handler))

  case object ActivateTimerSemantics extends ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember,
                                                 parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      var curState = state
      curState = New[S](TTimer, initials = Map(TTimer.field_is_active -> True))(curState,pp)
      val obj = curState.expr
      curState = EnableSemantics(TTimer.field_trigger_handler,1).forwardSemantics[S](
        obj,method,parameters
      )(pp,curState)
      curState = Return[S](obj)
      curState
    }
  }

  override def member_run_every =
    super.member_run_every.copy(semantics = ActivateTimerSemantics)

  override def member_run_after =
    super.member_run_after.copy(semantics = ActivateTimerSemantics)

  override def possibleFields = super.possibleFields ++ List(field_every_frame_handler)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

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
      if (TouchAnalysisParameters.get.printValuesInWarnings)
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