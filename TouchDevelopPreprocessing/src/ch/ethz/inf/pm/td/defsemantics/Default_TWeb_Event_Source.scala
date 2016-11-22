/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Web Event Source
 *
 * A Server-Sent-Events client
 *
 * @author Lucas Brutschy
 */

trait Default_TWeb_Event_Source extends AAny {

  lazy val typeName = TypeName("Web Event Source")
          
  /** Never used: Closes the EventSource. No further event will be raised. */
  def member_close = ApiMember(
    name = "close",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets an event to run when an error occurs */
  def member_on_error = ApiMember(
    name = "on error",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Sets an event to run when a message is received. Change name to receive custom events. */
  def member_on_message = ApiMember(
    name = "on message",
    paramTypes = List(ApiParam(TString), ApiParam(TText_Action)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Sets an event to run when the event source is opened */
  def member_on_open = ApiMember(
    name = "on open",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the current connection state (`connecting`, `open`, `closed`) */
  def member_state = ApiMember(
    name = "state",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "close" -> member_close,
    "on error" -> member_on_error,
    "on message" -> member_on_message,
    "on open" -> member_on_open,
    "state" -> member_state
  )
            

}
          
