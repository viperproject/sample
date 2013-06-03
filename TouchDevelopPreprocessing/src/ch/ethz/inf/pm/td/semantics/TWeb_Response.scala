
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
  val field_content = new TouchField("content",TString.typName)

  /** Reads the response body as a JSON tree */
  val field_content_as_json = new TouchField("content as json",TJson_Object.typName)

  /** Reads the response body as a picture */
  val field_content_as_picture = new TouchField("content as picture",TPicture.typName)

  /** Reads the response body as a wave sound */
  val field_content_as_sound = new TouchField("content as sound",TSound.typName)

  /** Reads the response body as a XML tree */
  val field_content_as_xml = new TouchField("content as xml",TXml_Object.typName)

  /** Stores the headers. This is actually not publicly accessible */
  val field_header_storage = new TouchField("  header",TString_Map.typName)

  /** Gets the request associated to this response */
  val field_request = new TouchField("request",TWeb_Request.typName)

  /** Gets the HTTP Status code of the request if any */
  val field_status_code = new TouchField("status code",TNumber.typName)

  val typName = "Web Response"
  val typ = new TouchType(typName,isSingleton = false, isImmutable = true, fields = List(field_header_storage, field_content, field_content_as_json, field_content_as_picture, field_content_as_sound, field_content_as_xml, field_request, field_status_code))


}

class TWeb_Response extends AAny {

  def getTyp = TWeb_Response.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the value of a given header */
    case "header" =>
      val List(name) = parameters // String
      CallApi[S](Field[S](this0,TWeb_Request.field_header_storage),"at",List(name),TString.typ)

    /** Gets the names of the headers */
    case "header names" =>
      CallApi[S](Field[S](this0,TWeb_Request.field_header_storage),"keys",Nil,TString_Collection.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
