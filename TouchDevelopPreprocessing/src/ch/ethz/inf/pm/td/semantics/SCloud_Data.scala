
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{ValidPureSemantics, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_SCloud_Data
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Cloud Data
 *
 * Cloud session management
 *
 * @author Lucas Brutschy
 */

object SCloud_Data extends Default_SCloud_Data {

  override lazy val member_connection_status = super.member_connection_status.copy(semantics = ValidPureSemantics)

  /** Gets the just-me session, in which cloud data is shared between devices by the same user. */
  lazy val field_just_me_session = ApiField("just me session", TCloud_Session)

  /** Gets the everyone-session, in which cloud data is shared by everyone running this script. */
  lazy val field_everyone_session = ApiField("everyone session", TCloud_Session)

  /** Gets the currently active session. When the script starts, this is always the just-me session. */
  lazy val field_current_session = ApiField("current session", TCloud_Session)

  /* [**obsolete**] Deprecated: always equal to current session. */
  lazy val field_last_session = ApiField("last session", TCloud_Session)

  /** Returns the participant number within the current session, or -1 if not known yet. Participant numbers
    * are assigned by the server on first connect, starting with 0. */
  lazy val field_participant_number = ApiField("participant number", TNumber)

  /** Returns a boolean indicating whether cloud synchronization is enabled for the current session */
  lazy val field_sync_enabled = ApiField("sync enabled", TBoolean)

  override def possibleFields = super.possibleFields ++ List(field_current_session,
    field_everyone_session, field_sync_enabled, field_just_me_session, field_last_session, field_participant_number)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet],
                                               returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Clear all data of the currently active session. */
    case "clear all data" =>
      val List() = parameters //
      Skip

    /** Creates a new cloud session owned by the current user. */
    case "create session" =>
      val List(title, typ) = parameters // String,String
      New[S](TCloud_Session, initials = Map(TCloud_Session.field_title -> title))

    /** Returns a boolean indicating whether cloud synchronization is enabled for the current session */
    case "is sync enabled" =>
      Return[S](Field[S](this0, SCloud_Data.field_sync_enabled))

    /** [**dbg**] Clear the local cache of the current session (which discards unsynced changes) and get fresh data
      * from server */
    case "rebuild cache" =>
      val List() = parameters //
      Skip

    /** Gets a session from a session id */
    case "session of" =>
      val List(id, title) = parameters // String,String
      TopWithInvalid[S](TCloud_Session, "session id may be wrong")

    /** Asks the user to choose a session to switch to */
    case "switch sessions" =>
      val List() = parameters //
      Skip

    /** Connect to the given session. The user may be asked to confirm. */
    case "switch to session" =>
      val List(session) = parameters // Cloud_Session
      Skip

    /** Waits until the current server state has been received. Returns false if offline, or if time limit is
      * exceeded. */
    case "wait for server" =>
      val List(timeout_msec) = parameters // Number
      Top[S](TBoolean)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
