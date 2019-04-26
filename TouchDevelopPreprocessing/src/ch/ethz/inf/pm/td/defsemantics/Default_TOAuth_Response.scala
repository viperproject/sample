/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of OAuth Response
 *
 * OAuth 2.0 Access Token or Error as described in http://tools.ietf.org/html/rfc6749.
 *
 * @author Lucas Brutschy
 */

trait Default_TOAuth_Response extends AAny {

  lazy val typeName = TypeName("OAuth Response")
          
  /** Never used: The access token issued by the authorization server. */
  def member_access_token = ApiMember(
    name = "access token",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: (Optional) A human readable error code. */
  def member_error_description = ApiMember(
    name = "error description",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: (Optional) A URI identifying a human-readable web page with information about the error, used to provide the client developer with additional information about the error. */
  def member_error_uri = ApiMember(
    name = "error uri",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: A single ASCII [USASCII] error code. */
  def member_error = ApiMember(
    name = "error",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: (Optional) The lifetime in seconds of the access token. */
  def member_expires_in = ApiMember(
    name = "expires in",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if this response is an error. */
  def member_is_error = ApiMember(
    name = "is error",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: (Optional) Indicates if the token might expire within the next seconds. */
  def member_is_expiring = ApiMember(
    name = "is expiring",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: (Optional) Additional key-value pairs not covered by the OAuth 2.0 specification. */
  def member_others = ApiMember(
    name = "others",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString_Map,
    semantics = DefaultSemantics
  )

  /** Never used: (Optional) Optional if identical to the scope requested by the client; otherwise, the scope of the access token as described by Section 3.3 of the OAuth 2.0 specification. */
  def member_scope = ApiMember(
    name = "scope",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "access token" -> member_access_token,
    "error description" -> member_error_description,
    "error uri" -> member_error_uri,
    "error" -> member_error,
    "expires in" -> member_expires_in,
    "is error" -> member_is_error,
    "is expiring" -> member_is_expiring,
    "others" -> member_others,
    "scope" -> member_scope
  )
            

}
          
