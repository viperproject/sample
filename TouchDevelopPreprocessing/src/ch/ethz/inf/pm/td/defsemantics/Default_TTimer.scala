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
 * Specifies the abstract semantics of Timer
 *
 * A timer
 *
 * @author Lucas Brutschy
 */

trait Default_TTimer extends AAny {

  lazy val typeName = TypeName("Timer")
          
  /** Never used: Clears the handlers and pauses the timer */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: is the timer active */
  def member_is_active = ApiMember(
    name = "is active",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: is this an interval timer that fires regularly */
  def member_is_interval = ApiMember(
    name = "is interval",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: sets the action to perform when the timer fires */
  def member_on_trigger = ApiMember(
    name = "on trigger",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: deactivates the timer */
  def member_pause = ApiMember(
    name = "pause",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: reactives the timer */
  def member_resume = ApiMember(
    name = "resume",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: set the regular interval in seconds at which this timer fires */
  def member_set_interval = ApiMember(
    name = "set interval",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: set the time in seconds after which this timer fires once */
  def member_set_timeout = ApiMember(
    name = "set timeout",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "clear" -> member_clear,
    "is active" -> member_is_active,
    "is interval" -> member_is_interval,
    "on trigger" -> member_on_trigger,
    "pause" -> member_pause,
    "resume" -> member_resume,
    "set interval" -> member_set_interval,
    "set timeout" -> member_set_timeout
  )
            

}
          
