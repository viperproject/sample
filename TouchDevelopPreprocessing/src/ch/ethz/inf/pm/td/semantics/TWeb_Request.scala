
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

  /** Reads the response body as a string */
  val field_content = new TouchField("content",TString.typ)

  /** Reads the response body as a JSON tree */
  val field_content_as_json = new TouchField("content_as_json",TJson_Object.typ)

  /** Reads the response body as a picture */
  val field_content_as_picture = new TouchField("content_as_picture",TPicture.typ)

  /** Reads the response body as a XML tree */
  val field_content_as_xml = new TouchField("content_as_xml",TXml_Object.typ)

  /** Stores the headers. This is actually not publicly accessible */
  val field_header_storage = new TouchField("header_storage",TString_Map.typ)

  /** Gets whether it was a 'get' or a 'post'. */
  val field_method = new TouchField("method",TString.typ)

  /** Gets the url of the request */
  val field_url = new TouchField("url",TString.typ)

  val typName = "Web_Request"
  val typ = new TouchType(typName,isSingleton = false,List(field_header_storage, field_method, field_url,
    field_content, field_content_as_json, field_content_as_picture, field_content_as_xml))

}

class TWeb_Request extends AAny {

  def getTyp = TWeb_Request.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Indicates if both requests are the same instance. */
    // case "equals" => 
    //   val List(other) = parameters // Web_Request
    //   Top[S](TBoolean.typ)

    /** Gets the value of a given header */
    case "header" =>
      val List(name) = parameters // String
      Return[S](CollectionAt[S](Field[S](this0,TWeb_Request.field_header_storage),name))

    /** Gets the names of the headers */
    // case "header_names" =>
    //   Top[S](TString_Collection.typ)

    /** Performs the request synchronously */
    case "send" =>
      Top[S](TWeb_Response.typ,Map(TWeb_Response.field_request -> this0))

    /** Sets the Accept header type ('text/xml' for xml, 'application/json' for json). */
    case "set_accept" =>
      val List(typ) = parameters // String
      CollectionInsert(Field[S](this0,TWeb_Request.field_header_storage),String("Accept"),typ)

    /** Compresses the request content with gzip and sets the Content-Encoding header */
    // case "set_compress" => 
    //   val List(value) = parameters // Boolean
    //  CollectionInsert(Field[S](this0,TWeb_Request.field_header_storage),StringCst("Content-Encoding"),value)

    /** Sets the name and password for basic authentication. Requires an HTTPS URL, empty string clears. */
    // case "set_credentials" => 
    //   val List(name,password) = parameters // String,String
    //   Skip;

    /** Sets an HTML header value. Empty string clears the value */
    case "set_header" =>
      val List(name,value) = parameters // String,String
      CollectionInsert(Field[S](this0,TWeb_Request.field_header_storage),name,value)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
