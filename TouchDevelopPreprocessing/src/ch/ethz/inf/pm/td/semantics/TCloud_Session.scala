
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TCloud_Session
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Cloud Session
 *
 * A cloud data session
 *
 * @author Lucas Brutschy
 */ 

object TCloud_Session extends Default_TCloud_Session {

  /** Gets a value indicating if the session is connected */
  lazy val field_is_connected = ApiField("is connected", TBoolean)

  /** Gets the session id */
  lazy val field_id = ApiField("id", TString)

  /** Gets a string that describes this cloud session */
  lazy val field_title = ApiField("title", TString)

  /** [**dbg**] Query server about current state of this session. You must be the authenticated owner. */
  lazy val field_server_info = ApiField("server info", TJson_Object)

  /** Gets information about the user that owns this session */
  lazy val field_owner = ApiField("owner", TUser)

  /** Gets information about the user that owns this session */
  lazy val field_is_owned = ApiField("is owned", TBoolean)

  override def possibleFields = super.possibleFields ++ List(
    field_is_connected,
    field_id,
    field_title,
    field_server_info,
    field_owner,
    field_is_owned
  )

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
