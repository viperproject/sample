
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Web Request
 *
 * An HTTP web request
 *
 * @author Lucas Brutschy
 */ 

object TWeb_Request {

  val typName = "Web Request"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TWeb_Request extends AAny {

  def getTyp = TWeb_Request.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Indicates if both requests are the same instance. */
    // case "equals" => 
    //   val List(other) = parameters // Web_Request
    //   Return[S](Valid(TBoolean.typ))

    /** Gets the value of a given header */
    // case "header" => 
    //   val List(name) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Gets the names of the headers */
    // case "header_names" => 
    //   Return[S](Valid(TString_Collection.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the names of the headers */
    //   val field_header_names = new TouchField("header_names",TString_Collection.typ)

    /** Gets whether it was a 'get' or a 'post'. */
    // case "method" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets whether it was a 'get' or a 'post'. */
    //   val field_method = new TouchField("method",TString.typ)

    /** Performs the request synchronously */
    // case "send" => 
    //   Return[S](Valid(TWeb_Response.typ))
    // DECLARATION AS FIELD: 
    //   /** Performs the request synchronously */
    //   val field_send = new TouchField("send",TWeb_Response.typ)

    /** Sets the Accept header type ('text/xml' for xml, 'application/json' for json). */
    // case "set_accept" => 
    //   val List(type) = parameters // String
    //   Skip;

    /** Compresses the request content with gzip and sets the Content-Encoding header */
    // case "set_compress" => 
    //   val List(value) = parameters // Boolean
    //   Skip;

    /** Sets the content of a 'post' request */
    // case "set_content" => 
    //   val List(content) = parameters // String
    //   Skip;

    /** Sets the content of a 'post' request as the JSON tree */
    // case "set_content_as_json" => 
    //   val List(json) = parameters // Json_Object
    //   Skip;

    /** Sets the content of a 'post' request as a JPEG encoded image. Quality from 0 (worse) to 1 (best). */
    // case "set_content_as_picture" => 
    //   val List(picture,quality) = parameters // Picture,Number
    //   Skip;

    /** Sets the content of a 'post' request as the XML tree */
    // case "set_content_as_xml" => 
    //   val List(xml) = parameters // Xml_Object
    //   Skip;

    /** Sets the name and password for basic authentication. Requires an HTTPS URL, empty string clears. */
    // case "set_credentials" => 
    //   val List(name,password) = parameters // String,String
    //   Skip;

    /** Sets an HTML header value. Empty string clears the value */
    // case "set_header" => 
    //   val List(name,value) = parameters // String,String
    //   Skip;

    /** Sets the method as 'get' or 'post'. Default value is 'get'. */
    // case "set_method" => 
    //   val List(method) = parameters // String
    //   Skip;

    /** Sets the url of the request. Must be a valid internet address. */
    // case "set_url" => 
    //   val List(url) = parameters // String
    //   Skip;

    /** Gets the url of the request */
    // case "url" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the url of the request */
    //   val field_url = new TouchField("url",TString.typ)

    // FIELDS: , field_header_names, field_method, field_send, field_url

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
