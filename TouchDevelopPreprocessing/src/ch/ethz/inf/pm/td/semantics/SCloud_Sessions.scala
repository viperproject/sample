
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Cloud Sessions
 *
 * Cloud session management
 *
 * @author Lucas Brutschy
 */ 

object SCloud_Sessions {

  /** Gets the current session. */
  val field_current_session = new TouchField("current session",TCloud_Session.typ)

  val typName = "Cloud Sessions"
  val typ = new TouchType(typName,isSingleton = true,fields = List(field_current_session))

}

class SCloud_Sessions extends AAny {

  def getTyp = SCloud_Sessions.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Open the given session and closes any previous active session */
    case "open" =>
      val List(session) = parameters // Cloud_Session
      AssignField[S](this0,SCloud_Sessions.field_current_session,session)

    /** Gets a session from a session id for the current user */
    case "from id" =>
      val List(id) = parameters // String
      TopWithInvalid[S](TCloud_Session.typ)

    /** Chooses a session */
    case "choose session" =>
      TopWithInvalid[S](TCloud_Session.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
