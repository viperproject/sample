
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Cloud Session
 *
 * A cloud data session
 *
 * @author Lucas Brutschy
 */ 

object TCloud_Session {

  /** Gets a value indicating if the session is connected */
  val field_is_connected = new TouchField("is connected",TBoolean.typName)

  /** Gets the session id */
  val field_id = new TouchField("id",TString.typName)

  /** Gets a string that describes this cloud session */
  val field_title = new TouchField("title",TString.typName)

  /** [**dbg**] Query server about current state of this session. You must be the authenticated owner. */
  val field_server_info = new TouchField("server info",TJson_Object.typName)

  /** Gets information about the user that owns this session */
  val field_owner = new TouchField("owner",TUser.typName)

  val typName = "Cloud Session"
  val typ = DefaultTouchType(typName,isImmutable = true,fields = List(field_is_connected,field_id,field_title,field_server_info,field_owner))

}

class TCloud_Session extends AAny {

  def getTyp = TCloud_Session.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
