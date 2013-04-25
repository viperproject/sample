
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of OAuth Response
 *
 * OAuth 2.0 Access Token or Error
 *
 * @author Lucas Brutschy
 */ 

object TOAuth_Response {

  val typName = "OAuth Response"
  val typ = new TouchType(typName,isSingleton = true)

}

class TOAuth_Response extends AAny {

  def getTyp = TOAuth_Response.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** The access token issued by the authorization server. */
    // case "access token" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** The access token issued by the authorization server. */
    //   val field_access_token = new TouchField("access token",TString.typ)

    /** (Optional) The lifetime in seconds of the access token. */
    // case "expires in" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TNumber.typ)
    // DECLARATION AS FIELD: 
    //   /** (Optional) The lifetime in seconds of the access token. */
    //   val field_expires_in = new TouchField("expires in",TNumber.typ)

    /** (Optional) Optional if if identical to the scope requested by the client; otherwise, the scope of the access token as described by Section 3.3. */
    // case "scope" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** (Optional) Optional if if identical to the scope requested by the client; otherwise, the scope of the access token as described by Section 3.3. */
    //   val field_scope = new TouchField("scope",TString.typ)

    /** A single ASCII [USASCII] error code. */
    // case "error" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** A single ASCII [USASCII] error code. */
    //   val field_error = new TouchField("error",TString.typ)

    /** (Optional) A human readable error code. */
    // case "error description" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** (Optional) A human readable error code. */
    //   val field_error_description = new TouchField("error description",TString.typ)

    /** (Optional) A URI identifying a human-readable web page with information about the error, used to provide the client developer with additional information about the error. */
    // case "error uri" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** (Optional) A URI identifying a human-readable web page with information about the error, used to provide the client developer with additional information about the error. */
    //   val field_error_uri = new TouchField("error uri",TString.typ)

    /** (Optional) Additional key-value pairs not covered by the OAuth 2.0 specification. */
    // case "others" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString_Map.typ)
    // DECLARATION AS FIELD: 
    //   /** (Optional) Additional key-value pairs not covered by the OAuth 2.0 specification. */
    //   val field_others = new TouchField("others",TString_Map.typ)

    /** (Optional) Indicates if the token might expire within the next seconds. */
    // case "is expiring" => 
    //   val List(lookup) = parameters // Number
    //   TopWithInvalid[S](TBoolean.typ)
    // DECLARATION AS FIELD: 
    //   /** (Optional) Indicates if the token might expire within the next seconds. */
    //   val field_is_expiring = new TouchField("is expiring",TBoolean.typ)

    /** Indicates if this response is an error. */
    // case "is error" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TBoolean.typ)
    // DECLARATION AS FIELD: 
    //   /** Indicates if this response is an error. */
    //   val field_is_error = new TouchField("is error",TBoolean.typ)

    // FIELDS: field_access_token, field_expires_in, field_scope, field_error, field_error_description, field_error_uri, field_others, field_is_expiring, field_is_error

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
