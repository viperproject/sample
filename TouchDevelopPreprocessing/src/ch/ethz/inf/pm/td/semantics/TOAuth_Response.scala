/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TOAuth_Response
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of OAuth Response
 *
 * OAuth 2.0 Access Token or Error
 *
 * @author Lucas Brutschy
 */ 

object TOAuth_Response extends Default_TOAuth_Response {

  /** The access token issued by the authorization server. */
  lazy val field_access_token = ApiField("access token", TString)

  /** (Optional) The lifetime in seconds of the access token. */
  lazy val field_expires_in = ApiField("expires in", TNumber)

  /** (Optional) Optional if if identical to the scope requested by the client; otherwise, the scope of the access token as described by Section 3.3. */
  lazy val field_scope = ApiField("scope", TString)

  /** A single ASCII [USASCII] error code. */
  // TODO: Invalid iff is_error = false ?
  lazy val field_error = ApiField("error", TString)

  /** (Optional) A human readable error code. */
  // TODO: Invalid iff is_error = false ?
  lazy val field_error_description = ApiField("error description", TString)

  /** (Optional) A URI identifying a human-readable web page with information about the error, used to provide the client developer with additional information about the error. */
  lazy val field_error_uri = ApiField("error uri", TString)

  /** (Optional) Additional key-value pairs not covered by the OAuth 2.0 specification. */
  lazy val field_others = ApiField("others", TString_Map)

  /** Indicates if this response is an error. */
  lazy val field_is_error = ApiField("is error", TBoolean)

  override def possibleFields = super.possibleFields ++ List(field_access_token, field_expires_in, field_scope,
    field_error, field_error_description, field_error_uri, field_others, field_is_error)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** (Optional) Indicates if the token might expire within the next seconds. */
    case "is expiring" =>
      val List(lookup) = parameters // Number
      Dummy[S](this0,method)
      Top[S](TBoolean)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
