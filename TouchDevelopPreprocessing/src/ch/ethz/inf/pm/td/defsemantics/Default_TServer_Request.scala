
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Server Request
 *
 * An incomming HTTP web request
 *
 * @author Lucas Brutschy
 */

trait Default_TServer_Request extends AAny {

  lazy val typeName = TypeName("Server Request")
          
  /** Never used: [**beta**] Reads the request body as a binary buffer */
  def member_content_as_buffer = ApiMember(
    name = "content as buffer",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBuffer,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Reads the request body as a JSON tree */
  def member_content_as_json = ApiMember(
    name = "content as json",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Reads the request body as a string */
  def member_content = ApiMember(
    name = "content",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Indicates if both requests are the same instance. */
  def member_equals = ApiMember(
    name = "equals",
    paramTypes = List(ApiParam(TServer_Request)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Gets the names of the headers */
  def member_header_names = ApiMember(
    name = "header names",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString_Collection,
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

  /** Never used: [**beta**] Gets whether it was a 'get', 'post', 'put', 'delete', 'options', etc. */
  def member_method = ApiMember(
    name = "method",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Gets the names of the query string parameters */
  def member_query_names = ApiMember(
    name = "query names",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString_Collection,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Gets the value of a given query string parameter */
  def member_query = ApiMember(
    name = "query",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Gets the associated response */
  def member_response = ApiMember(
    name = "response",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TServer_Response,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Gets the url of the request */
  def member_url = ApiMember(
    name = "url",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Get the user who sent this request */
  def member_user = ApiMember(
    name = "user",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TUser,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "content as buffer" -> member_content_as_buffer,
    "content as json" -> member_content_as_json,
    "content" -> member_content,
    "equals" -> member_equals,
    "header names" -> member_header_names,
    "header" -> member_header,
    "method" -> member_method,
    "query names" -> member_query_names,
    "query" -> member_query,
    "response" -> member_response,
    "url" -> member_url,
    "user" -> member_user
  )
            

}
          
