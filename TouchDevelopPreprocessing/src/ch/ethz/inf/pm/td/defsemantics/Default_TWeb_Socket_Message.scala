
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
 * Specifies the abstract semantics of Web Socket Message
 *
 * A web socket message
 *
 * @author Lucas Brutschy
 */

trait Default_TWeb_Socket_Message extends AAny {

  lazy val typeName = TypeName("Web Socket Message")
          
  /** Never used: Gets the message as a Buffer */
  def member_buffer = ApiMember(
    name = "buffer",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBuffer,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the error if any. */
  def member_error = ApiMember(
    name = "error",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if this message is an error */
  def member_is_error = ApiMember(
    name = "is error",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the message as a Json payload */
  def member_json = ApiMember(
    name = "json",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the message as a string */
  def member_string = ApiMember(
    name = "string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "buffer" -> member_buffer,
    "error" -> member_error,
    "is error" -> member_is_error,
    "json" -> member_json,
    "string" -> member_string
  )
            

}
          
