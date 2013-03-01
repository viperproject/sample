
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

  /** Reads the response body as a JSON tree */
  val field_content_as_json = new TouchField("content_as_json",TJson_Object.typ)

  /** Reads the response body as a picture */
  val field_content_as_picture = new TouchField("content_as_picture",TPicture.typ)

  /** Reads the response body as a wave sound */
  val field_content_as_sound = new TouchField("content_as_sound",TSound.typ)

  /** Reads the response body as a XML tree */
  val field_content_as_xml = new TouchField("content_as_xml",TXml_Object.typ)

  /** Stores the headers. This is actually not publicly accessible */
  val field_header_storage = new TouchField("header_storage",TString_Map.typ)

  /** Gets the request associated to this response */
  val field_request = new TouchField("request",TWeb_Request.typ)

  /** Gets the HTTP Status code of the request if any */
  val field_status_code = new TouchField("status_code",TNumber.typ)

  val typName = "Web_Response"
  val typ = new TouchType(typName,isSingleton = false,List(field_content, field_content_as_json, field_content_as_picture, field_content_as_sound, field_content_as_xml, field_request, field_status_code))


}

class TWeb_Response extends AAny {

  def getTyp = TWeb_Response.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the value of a given header */
    case "header" =>
      val List(name) = parameters // String
      Return[S](CollectionAt[S](Field[S](this0,TWeb_Request.field_header_storage),name))

    /** Gets the names of the headers */
    // case "header_names" => 
    //   Top[S](TString_Collection.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
