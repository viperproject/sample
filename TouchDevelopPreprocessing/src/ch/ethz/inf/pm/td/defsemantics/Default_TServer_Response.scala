
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Server Response
 *
 * An HTTP web response to be returned
 *
 * @author Lucas Brutschy
 */

trait Default_TServer_Response extends AAny {

  lazy val typeName = TypeName("Server Response")
          
  /** Never used: [**beta**] Gets the names of the headers */
  def member_header_names = ApiMember(
    name = "header names",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Gets the value of a given header */
  def member_header = ApiMember(
    name = "header",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Gets the request associated to this response */
  def member_request = ApiMember(
    name = "request",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TServer_Request,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Sets the content of the response as a binary buffer */
  def member_set_content_as_buffer = ApiMember(
    name = "set content as buffer",
    paramTypes = List(ApiParam(TBuffer)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Sets the content of the response as the JSON tree */
  def member_set_content_as_json = ApiMember(
    name = "set content as json",
    paramTypes = List(ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Sets the content of the response as the XML tree */
  def member_set_content_as_xml = ApiMember(
    name = "set content as xml",
    paramTypes = List(ApiParam(TXml_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Sets the 'Content-Type' HTTP header; call after `->set_content...` */
  def member_set_content_type = ApiMember(
    name = "set content type",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Sets the content of the response */
  def member_set_content = ApiMember(
    name = "set content",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Sets an HTTP header value. Empty string clears the value */
  def member_set_header = ApiMember(
    name = "set header",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Sets the HTTP Status code of the response (defaults to 200) */
  def member_set_status_code = ApiMember(
    name = "set status code",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Gets the HTTP Status code of the response (defaults to 200) */
  def member_status_code = ApiMember(
    name = "status code",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "header names" -> member_header_names,
    "header" -> member_header,
    "request" -> member_request,
    "set content as buffer" -> member_set_content_as_buffer,
    "set content as json" -> member_set_content_as_json,
    "set content as xml" -> member_set_content_as_xml,
    "set content type" -> member_set_content_type,
    "set content" -> member_set_content,
    "set header" -> member_set_header,
    "set status code" -> member_set_status_code,
    "status code" -> member_status_code
  )
            

}
          
