/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TTimer
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Timer
 *
 * A timer
 *
 * @author Lucas Brutschy
 */

object TTimer extends Default_TTimer {

  /** is the timer active */
  lazy val field_is_active = ApiField("is active", TBoolean)

  /** is this an interval timer that fires regularly */
  lazy val field_is_interval = ApiField("is interval", TBoolean)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_trigger_handler = ApiField("trigger handler", TAction)

  override def member_on_trigger =
    super.member_on_trigger.copy(semantics = AAction.EnableSemantics(TTimer.field_trigger_handler))

  override def possibleFields = super.possibleFields ++ List(field_is_active, field_is_interval, field_trigger_handler)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {


    /** Clears the handlers and pauses the timer */
    case "clear" =>
      Skip

    /** deactivates the timer */
    case "pause" =>
      AssignField[S](this0, TTimer.field_is_active, False)

    /** reactives the timer */
    case "resume" =>
      AssignField[S](this0, TTimer.field_is_active, True or False)

    /** set the regular interval in seconds at which this timer fires */
    case "set interval" =>
      val List(seconds) = parameters // Number
      AssignField[S](this0, TTimer.field_is_interval, True)

    /** set the time in seconds after which this timer fires once */
    case "set timeout" =>
      val List(seconds) = parameters // Number
      AssignField[S](this0, TTimer.field_is_interval, False)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
