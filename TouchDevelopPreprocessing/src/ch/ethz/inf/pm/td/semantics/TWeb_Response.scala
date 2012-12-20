
package ch.ethz.inf.pm.td.semantics

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

  val typName = "Web Response"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TWeb_Response extends AAny {

  def getTyp = TWeb_Response.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Reads the response body as a string */
    // case "content" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Reads the response body as a string */
    //   val field_content = new TouchField("content",TString.typ)

    /** Reads the response body as a JSON tree */
    // case "content_as_json" => 
    //   Return[S](Valid(TJson_Object.typ))
    // DECLARATION AS FIELD: 
    //   /** Reads the response body as a JSON tree */
    //   val field_content_as_json = new TouchField("content_as_json",TJson_Object.typ)

    /** Reads the response body as a picture */
    // case "content_as_picture" => 
    //   Return[S](Valid(TPicture.typ))
    // DECLARATION AS FIELD: 
    //   /** Reads the response body as a picture */
    //   val field_content_as_picture = new TouchField("content_as_picture",TPicture.typ)

    /** Reads the response body as a wave sound */
    // case "content_as_sound" => 
    //   Return[S](Valid(TSound.typ))
    // DECLARATION AS FIELD: 
    //   /** Reads the response body as a wave sound */
    //   val field_content_as_sound = new TouchField("content_as_sound",TSound.typ)

    /** Reads the response body as a XML tree */
    // case "content_as_xml" => 
    //   Return[S](Valid(TXml_Object.typ))
    // DECLARATION AS FIELD: 
    //   /** Reads the response body as a XML tree */
    //   val field_content_as_xml = new TouchField("content_as_xml",TXml_Object.typ)

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

    /** Gets the request associated to this response */
    // case "request" => 
    //   Return[S](Valid(TWeb_Request.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the request associated to this response */
    //   val field_request = new TouchField("request",TWeb_Request.typ)

    /** Gets the HTTP Status code of the request if any */
    // case "status_code" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the HTTP Status code of the request if any */
    //   val field_status_code = new TouchField("status_code",TNumber.typ)

    // FIELDS: , field_content, field_content_as_json, field_content_as_picture, field_content_as_sound, field_content_as_xml, field_header_names, field_request, field_status_code

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
