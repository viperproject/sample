
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
 * Specifies the abstract semantics of Web Response
 *
 * An HTTP web response
 *
 * @author Lucas Brutschy
 */

trait Default_TWeb_Response extends AAny {

  lazy val typeName = TypeName("Web Response")
          
  /** Never used: Reads the response body as a Buffer. */
  def member_content_as_buffer = ApiMember(
    name = "content as buffer",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBuffer,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Reads the response body as a JSON tree */
  def member_content_as_json = ApiMember(
    name = "content as json",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Rarely used: Reads the response body as a picture */
  def member_content_as_picture = ApiMember(
    name = "content as picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] Reads the response body as a wave sound */
  def member_content_as_sound = ApiMember(
    name = "content as sound",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSound,
    semantics = DefaultSemantics
  )

  /** Rarely used: Reads the response body as a XML tree */
  def member_content_as_xml = ApiMember(
    name = "content as xml",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TXml_Object,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Reads the response body as a string */
  def member_content = ApiMember(
    name = "content",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the names of the headers */
  def member_header_names = ApiMember(
    name = "header names",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the value of a given header */
  def member_header = ApiMember(
    name = "header",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the request associated to this response */
  def member_request = ApiMember(
    name = "request",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TWeb_Request,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the HTTP Status code of the request if any */
  def member_status_code = ApiMember(
    name = "status code",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "content as buffer" -> member_content_as_buffer,
    "content as json" -> member_content_as_json,
    "content as picture" -> member_content_as_picture,
    "content as sound" -> member_content_as_sound,
    "content as xml" -> member_content_as_xml,
    "content" -> member_content,
    "header names" -> member_header_names,
    "header" -> member_header,
    "request" -> member_request,
    "status code" -> member_status_code
  )
            

}
          
