
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
 * Specifies the abstract semantics of Cloud Data
 *
 * Cloud session management
 *
 * @author Lucas Brutschy
 */

trait Default_SCloud_Data extends ASingleton {

  lazy val typeName = TypeName("Cloud Data", isSingleton = true)
          
  /** Never used: [**dbg**] Authenticate against your deployed cloud library. Returns false if the authentication fails or the connection times out. */
  def member_authenticate = ApiMember(
    name = "authenticate",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Clear all data of the currently active session. */
  def member_clear_all_data = ApiMember(
    name = "clear all data",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets a string that describes the state of the cloud synchronization, and additional details if requested */
  def member_connection_status = ApiMember(
    name = "connection status",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a new cloud session owned by the current user. */
  def member_create_session = ApiMember(
    name = "create session",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TCloud_Session,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the currently active session. When the script starts, this is always the just-me session. */
  def member_current_session = ApiMember(
    name = "current session",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TCloud_Session,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the everyone-session, in which cloud data is shared by everyone running this script. */
  def member_everyone_session = ApiMember(
    name = "everyone session",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TCloud_Session,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Import a JSON representation of the cloud data */
  def member_from_json = ApiMember(
    name = "from json",
    paramTypes = List(ApiParam(TJson_Object)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Returns a boolean indicating whether cloud synchronization is enabled for the current session */
  def member_is_sync_enabled = ApiMember(
    name = "is sync enabled",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the just-me session, in which cloud data is shared between devices by the same user. */
  def member_just_me_session = ApiMember(
    name = "just me session",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TCloud_Session,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] Deprecated: always equal to current session. */
  def member_last_session = ApiMember(
    name = "last session",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TCloud_Session,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the participant number within the current session, or -1 if not known yet. Participant numbers are assigned by the server on first connect, starting with 0. */
  def member_participant_number = ApiMember(
    name = "participant number",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Clear the local cache of the current session (discarding unsynced changes) and get fresh data from server */
  def member_rebuild_cache = ApiMember(
    name = "rebuild cache",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TCloud_Session,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets a session from a session id */
  def member_session_of = ApiMember(
    name = "session of",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TCloud_Session,
    semantics = DefaultSemantics
  )

  /** Never used: Enable or disable cloud synchronization for the current session */
  def member_set_sync_enabled = ApiMember(
    name = "set sync enabled",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] `validator(token)` should return user id (eg, `"fb:123456"`) or `""` in case token is invalid */
  def member_set_token_validator = ApiMember(
    name = "set token validator",
    paramTypes = List(ApiParam(GString_Converter(TString))),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Asks the user to choose a session to switch to */
  def member_switch_sessions = ApiMember(
    name = "switch sessions",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Connect to the given session. The user may be asked to confirm. */
  def member_switch_to_session = ApiMember(
    name = "switch to session",
    paramTypes = List(ApiParam(TCloud_Session)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Export a JSON representation of all the cloud data */
  def member_to_json = ApiMember(
    name = "to json",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: Waits until the current server state has been received. Returns false if offline, or if time limit is exceeded. */
  def member_wait_for_server = ApiMember(
    name = "wait for server",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "authenticate" -> member_authenticate,
    "clear all data" -> member_clear_all_data,
    "connection status" -> member_connection_status,
    "create session" -> member_create_session,
    "current session" -> member_current_session,
    "everyone session" -> member_everyone_session,
    "from json" -> member_from_json,
    "is sync enabled" -> member_is_sync_enabled,
    "just me session" -> member_just_me_session,
    "last session" -> member_last_session,
    "participant number" -> member_participant_number,
    "rebuild cache" -> member_rebuild_cache,
    "session of" -> member_session_of,
    "set sync enabled" -> member_set_sync_enabled,
    "set token validator" -> member_set_token_validator,
    "switch sessions" -> member_switch_sessions,
    "switch to session" -> member_switch_to_session,
    "to json" -> member_to_json,
    "wait for server" -> member_wait_for_server
  )
            

}
          
