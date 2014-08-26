
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Server Request
 *
 * An incomming HTTP web request
 *
 * @author Lucas Brutschy
 */ 

object TServer_Request {

  val typName = "Server Request"
  val typ = DefaultTouchType(typName)

}

class TServer_Request extends AAny {

  def getTyp = TServer_Request.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** [**beta**] Reads the request body as a binary buffer */
    case "content as buffer" =>
      val List() = parameters //
      Top[S](TBuffer.typ)

    /** [**beta**] Reads the request body as a JSON tree */
    case "content as json" =>
      val List() = parameters //
      TopWithInvalid[S](TJson_Object.typ,"request may not contain json")

    /** [**beta**] Reads the request body as a string */
    case "content" =>
      val List() = parameters //
      Top[S](TString.typ)

    /** [**beta**] Indicates if both requests are the same instance. */
    case "equals" =>
      val List(other) = parameters // Server_Request
      Top[S](TBoolean.typ)

    /** [**beta**] Gets the names of the headers */
    case "header names" =>
      val List() = parameters //
      Top[S](TString_Collection.typ)

    /** [**beta**] Gets the value of a given header */
    case "header" =>
      val List(name) = parameters // String
      TopWithInvalid[S](TString.typ,"header may not exist")

    /** [**beta**] Gets whether it was a 'get', 'post', 'put', 'delete', 'options', etc. */
    case "method" =>
      val List() = parameters //
      Top[S](TString.typ)

    /** [**beta**] Gets the names of the query string parameters */
    // case "query names" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString_Collection.typ)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Gets the names of the query string parameters */
    //   val field_query_names = new TouchField("query names",TString_Collection.typName)

    /** [**beta**] Gets the value of a given query string parameter */
    // case "query" => 
    //   val List(name) = parameters // String
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Gets the value of a given query string parameter */
    //   val field_query = new TouchField("query",TString.typName)

    /** [**beta**] Gets the associated response */
    // case "response" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TServer_Response.typ)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Gets the associated response */
    //   val field_response = new TouchField("response",TServer_Response.typName)

    /** [**beta**] Gets the url of the request */
    // case "url" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString.typ)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Gets the url of the request */
    //   val field_url = new TouchField("url",TString.typName)

    // FIELDS: field_content_as_buffer, field_content_as_json, field_content, field_equals, field_header_names, field_header, field_method, field_query_names, field_query, field_response, field_url

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
