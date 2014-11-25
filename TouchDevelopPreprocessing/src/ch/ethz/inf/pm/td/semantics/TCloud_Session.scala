
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Cloud Session
 *
 * A cloud data session
 *
 * @author Lucas Brutschy
 */ 

object TCloud_Session extends AAny {

  /** Gets a value indicating if the session is connected */
  lazy val field_is_connected = new ApiField("is connected",TBoolean.typeName)

  /** Gets the session id */
  lazy val field_id = new ApiField("id",TString.typeName)

  /** Gets a string that describes this cloud session */
  lazy val field_title = new ApiField("title",TString.typeName)

  /** [**dbg**] Query server about current state of this session. You must be the authenticated owner. */
  lazy val field_server_info = new ApiField("server info",TJson_Object.typeName)

  /** Gets information about the user that owns this session */
  lazy val field_owner = new ApiField("owner",TUser.typeName)

  lazy val typeName = TypeName("Cloud Session")

  override def possibleFields = super.possibleFields ++ List(field_is_connected,field_id,field_title,field_server_info,field_owner)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
