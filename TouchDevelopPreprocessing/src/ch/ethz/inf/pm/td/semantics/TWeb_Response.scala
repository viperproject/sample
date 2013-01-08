
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Web Response
 *
 * An HTTP web response
 *
 * @author Lucas Brutschy
 */ 

object TWeb_Response {

  /** Reads the response body as a string */
  val field_content = new TouchField("content",TString.typ)

  /** Stores the headers. This is actually not publicly accessible */
  val field_header_storage = new TouchField("header_storage",TString_Map.typ)

  //   /** Gets the HTTP Status code of the request if any */
  //   val field_status_code = new TouchField("status_code",TNumber.typ)

  val typName = "Web Response"
  val typ = TouchType(typName,isSingleton = false,List())


}

class TWeb_Response extends AAny {

  def getTyp = TWeb_Response.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Reads the response body as a JSON tree */
    // case "content_as_json" =>
    //   Return[S](Valid(TJson_Object.typ))

    /** Reads the response body as a picture */
    // case "content_as_picture" => 
    //   Return[S](Valid(TPicture.typ))

    /** Reads the response body as a wave sound */
    // case "content_as_sound" => 
    //   Return[S](Valid(TSound.typ))

    /** Reads the response body as a XML tree */
    // case "content_as_xml" => 
    //   Return[S](Valid(TXml_Object.typ))

    /** Gets the value of a given header */
    // case "header" =>
    //   val List(name) = parameters // String
    //   TODO: Rewrite to header
    //   Return[S](Valid(TString.typ))

    /** Gets the names of the headers */
    // case "header_names" => 
    //   Return[S](Valid(TString_Collection.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the names of the headers */
    //   val field_header_names = new TouchField("header_names",TString_Collection.typ)

    /** Gets the request associated to this response */
    // case "request" => 
    //   Return[S](Valid(TWeb_Request.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the request associated to this response */
    //   val field_request = new TouchField("request",TWeb_Request.typ)

    // FIELDS: , field_content, field_content_as_json, field_content_as_picture, field_content_as_sound, field_content_as_xml, field_header_names, field_request, field_status_code

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
