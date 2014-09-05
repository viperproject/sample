
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Server Request
 *
 * An incomming HTTP web request
 *
 * @author Lucas Brutschy
 */ 

object TServer_Request extends AAny {

  lazy val typeName = TypeName("Server Request")

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
      
    /** [**beta**] Reads the request body as a binary buffer */
    case "content as buffer" =>
      val List() = parameters //
      Top[S](TBuffer)

    /** [**beta**] Reads the request body as a JSON tree */
    case "content as json" =>
      val List() = parameters //
      TopWithInvalid[S](TJson_Object,"request may not contain json")

    /** [**beta**] Reads the request body as a string */
    case "content" =>
      val List() = parameters //
      Top[S](TString)

    /** [**beta**] Indicates if both requests are the same instance. */
    case "equals" =>
      val List(other) = parameters // Server_Request
      Top[S](TBoolean)

    /** [**beta**] Gets the names of the headers */
    case "header names" =>
      val List() = parameters //
      Top[S](TString_Collection)

    /** [**beta**] Gets the value of a given header */
    case "header" =>
      val List(name) = parameters // String
      TopWithInvalid[S](TString,"header may not exist")

    /** [**beta**] Gets whether it was a 'get', 'post', 'put', 'delete', 'options', etc. */
    case "method" =>
      val List() = parameters //
      Top[S](TString)

    /** [**beta**] Gets the names of the query string parameters */
    // case "query names" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString_Collection)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Gets the names of the query string parameters */
    //   lazy val field_query_names = new TouchField("query names",TString_Collection.typeName)

    /** [**beta**] Gets the value of a given query string parameter */
    // case "query" => 
    //   val List(name) = parameters // String
    //   TopWithInvalid[S](TString)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Gets the value of a given query string parameter */
    //   lazy val field_query = new TouchField("query",TString.typeName)

    /** [**beta**] Gets the associated response */
    // case "response" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TServer_Response)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Gets the associated response */
    //   lazy val field_response = new TouchField("response",TServer_Response.typeName)

    /** [**beta**] Gets the url of the request */
    // case "url" => 
    //   val List() = parameters // 
    //   TopWithInvalid[S](TString)
    // DECLARATION AS FIELD: 
    //   /** [**beta**] Gets the url of the request */
    //   lazy val field_url = new TouchField("url",TString.typeName)

    // FIELDS: field_content_as_buffer, field_content_as_json, field_content, field_equals, field_header_names, field_header, field_method, field_query_names, field_query, field_response, field_url

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
