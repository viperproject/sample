
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
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
  val field_is_connected = new TouchField("is connected",TBoolean.typ)

  /** Gets the session id */
  val field_id = new TouchField("id",TString.typ)

  val typName = "Cloud Session"
  val typ = new TouchType(typName,isImmutable = true,fields = List(field_is_connected,field_id))

}

class TCloud_Session extends AAny {

  def getTyp = TCloud_Session.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
