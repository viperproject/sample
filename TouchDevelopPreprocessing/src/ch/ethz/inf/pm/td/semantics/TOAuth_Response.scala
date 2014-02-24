
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of OAuth Response
 *
 * OAuth 2.0 Access Token or Error
 *
 * @author Lucas Brutschy
 */ 

object TOAuth_Response {

  /** The access token issued by the authorization server. */
  val field_access_token = new TouchField("access token",TString.typName)

  /** (Optional) The lifetime in seconds of the access token. */
  val field_expires_in = new TouchField("expires in",TNumber.typName)

  /** (Optional) Optional if if identical to the scope requested by the client; otherwise, the scope of the access token as described by Section 3.3. */
  val field_scope = new TouchField("scope",TString.typName)

  /** A single ASCII [USASCII] error code. */
  // TODO: Invalid iff is_error = false ?
  val field_error = new TouchField("error",TString.typName)

  /** (Optional) A human readable error code. */
  // TODO: Invalid iff is_error = false ?
  val field_error_description = new TouchField("error description",TString.typName)

  /** (Optional) A URI identifying a human-readable web page with information about the error, used to provide the client developer with additional information about the error. */
  val field_error_uri = new TouchField("error uri",TString.typName)

  /** (Optional) Additional key-value pairs not covered by the OAuth 2.0 specification. */
  val field_others = new TouchField("others",TString_Map.typName)

  /** Indicates if this response is an error. */
  val field_is_error = new TouchField("is error",TBoolean.typName)

  val typName = "OAuth Response"
  val typ = DefaultTouchType(typName,isSingleton = false,fields = List(field_access_token, field_expires_in, field_scope,
    field_error, field_error_description, field_error_uri, field_others, field_is_error))

}

class TOAuth_Response extends AAny {

  def getTyp = TOAuth_Response.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** (Optional) Indicates if the token might expire within the next seconds. */
    case "is expiring" =>
      val List(lookup) = parameters // Number
      Dummy[S](this0,method)
      Top[S](TBoolean.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
