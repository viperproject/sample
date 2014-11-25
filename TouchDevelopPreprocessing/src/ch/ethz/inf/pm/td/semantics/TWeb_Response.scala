
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Web Response
 *
 * An HTTP web response
 *
 * @author Lucas Brutschy
 */ 

object TWeb_Response extends AAny {

  /** Reads the response body as a string */
  lazy val field_content = new ApiField("content",TString.typeName)

  /** Reads the response body as a JSON tree */
  lazy val field_content_as_json = new ApiField("content as json",TJson_Object.typeName)

  /** Reads the response body as a picture */
  lazy val field_content_as_picture = new ApiField("content as picture",TPicture.typeName)

  /** Reads the response body as a wave sound */
  lazy val field_content_as_sound = new ApiField("content as sound",TSound.typeName)

  /** Reads the response body as a XML tree */
  lazy val field_content_as_xml = new ApiField("content as xml",TXml_Object.typeName)

  /** Stores the headers. This is actually not publicly accessible */
  lazy val field_header_storage = new ApiField("header storage",TString_Map.typeName)

  /** Gets the request associated to this response */
  lazy val field_request = new ApiField("request",TWeb_Request.typeName)

  /** Gets the HTTP Status code of the request if any */
  lazy val field_status_code = new ApiField("status code",TNumber.typeName)

  val typeName = TypeName("Web Response")

  override def possibleFields = super.possibleFields ++ List(field_header_storage,
    field_content, field_content_as_json, field_content_as_picture, field_content_as_sound, field_content_as_xml,
    field_request, field_status_code)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Gets the value of a given header */
    case "header" =>
      val List(name) = parameters // String
      CallApi[S](Field[S](this0,TWeb_Request.field_header_storage),"at",List(name),TString)

    /** Gets the names of the headers */
    case "header names" =>
      CallApi[S](Field[S](this0,TWeb_Request.field_header_storage),"keys",Nil,TString_Collection)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
