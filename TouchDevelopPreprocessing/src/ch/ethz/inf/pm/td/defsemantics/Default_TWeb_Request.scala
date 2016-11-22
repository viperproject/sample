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
 * Specifies the abstract semantics of Web Request
 *
 * An HTTP web request
 *
 * @author Lucas Brutschy
 */

trait Default_TWeb_Request extends AAny {

  lazy val typeName = TypeName("Web Request")
          
  /** Never used: Gets the names of the headers */
  def member_header_names = ApiMember(
    name = "header names",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Never used: Gets the value of a given header */
  def member_header = ApiMember(
    name = "header",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets whether it was a 'get' or a 'post'. */
  def member_method = ApiMember(
    name = "method",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Use ``send`` instead */
  def member_on_response_received = ApiMember(
    name = "on response received",
    paramTypes = List(ApiParam(TWeb_Response_Action)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: User ``send`` instead */
  def member_send_async = ApiMember(
    name = "send async",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Performs the request synchronously */
  def member_send = ApiMember(
    name = "send",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TWeb_Response,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the Accept header type ('text/xml' for xml, 'application/json' for json). */
  def member_set_accept = ApiMember(
    name = "set accept",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Compresses the request content with gzip and sets the Content-Encoding header */
  def member_set_compress = ApiMember(
    name = "set compress",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the content of a 'post' request as a binary buffer */
  def member_set_content_as_buffer = ApiMember(
    name = "set content as buffer",
    paramTypes = List(ApiParam(TBuffer)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the content as multipart/form-data. */
  def member_set_content_as_form = ApiMember(
    name = "set content as form",
    paramTypes = List(ApiParam(TForm_Builder)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the content of a 'post' request as the JSON tree */
  def member_set_content_as_json = ApiMember(
    name = "set content as json",
    paramTypes = List(ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the content of a 'post' request as a JPEG encoded image. Quality from 0 (worse) to 1 (best). */
  def member_set_content_as_picture = ApiMember(
    name = "set content as picture",
    paramTypes = List(ApiParam(TPicture), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the content of a 'post' request as the XML tree */
  def member_set_content_as_xml = ApiMember(
    name = "set content as xml",
    paramTypes = List(ApiParam(TXml_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the content of a 'post' request */
  def member_set_content = ApiMember(
    name = "set content",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the name and password for basic authentication. Requires an HTTPS URL, empty string clears. */
  def member_set_credentials = ApiMember(
    name = "set credentials",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets an HTML header value. Empty string clears the value */
  def member_set_header = ApiMember(
    name = "set header",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the method. Default value is 'get'. */
  def member_set_method = ApiMember(
    name = "set method",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the url of the request. Must be a valid internet address. */
  def member_set_url = ApiMember(
    name = "set url",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if program notifications should be shown to the user. Default is true. */
  def member_show_notifications = ApiMember(
    name = "show notifications",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the url of the request */
  def member_url = ApiMember(
    name = "url",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "header names" -> member_header_names,
    "header" -> member_header,
    "method" -> member_method,
    "on response received" -> member_on_response_received,
    "send async" -> member_send_async,
    "send" -> member_send,
    "set accept" -> member_set_accept,
    "set compress" -> member_set_compress,
    "set content as buffer" -> member_set_content_as_buffer,
    "set content as form" -> member_set_content_as_form,
    "set content as json" -> member_set_content_as_json,
    "set content as picture" -> member_set_content_as_picture,
    "set content as xml" -> member_set_content_as_xml,
    "set content" -> member_set_content,
    "set credentials" -> member_set_credentials,
    "set header" -> member_set_header,
    "set method" -> member_set_method,
    "set url" -> member_set_url,
    "show notifications" -> member_show_notifications,
    "url" -> member_url
  )
            

}
          
