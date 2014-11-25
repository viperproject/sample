
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Cloud Sessions
 *
 * Cloud session management
 *
 * @author Lucas Brutschy
 */

object SCloud_Sessions extends ASingleton {

  /** Gets the current session. */
  lazy val field_current_session = new ApiField("current session", TCloud_Session.typeName)

  lazy val typeName = TypeName("Cloud Sessions")

  override def possibleFields = super.possibleFields ++ List(field_current_session)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Open the given session and closes any previous active session */
    case "open" =>
      val List(session) = parameters // Cloud_Session
      AssignField[S](this0, SCloud_Sessions.field_current_session, session)

    /** Gets a session from a session id for the current user */
    case "from id" =>
      val List(id) = parameters // String
      TopWithInvalid[S](TCloud_Session, "session id may be wrong")

    /** Chooses a session */
    case "choose session" =>
      TopWithInvalid[S](TCloud_Session, "use may cancel session choice dialog")

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
