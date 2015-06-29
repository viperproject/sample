
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.{DefaultSemantics, ApiParam, ApiMember, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TWeb_Request
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Web Request
 *
 * An HTTP web request
 *
 * @author Lucas Brutschy
 */

object TWeb_Request extends Default_TWeb_Request {

  /** Reads the response body as a string */
  lazy val field_content = ApiField("content", TString)

  /** Reads the response body as a JSON tree */
  lazy val field_content_as_json = ApiField("content as json", TJson_Object)

  /** Reads the response body as a picture */
  lazy val field_content_as_picture = ApiField("content as picture", TPicture)
  lazy val field_content_as_picture_quality = ApiField("content as picture", TPicture)

  /** Reads the response body as a picture */
  lazy val field_content_as_form = ApiField("content as form", TForm_Builder)

  /** Reads the response body as a XML tree */
  lazy val field_content_as_xml = ApiField("content as xml", TXml_Object)

  /** Stores the headers. This is actually not publicly accessible */
  lazy val field_header_storage = ApiField("header storage", TString_Map)

  /** Gets whether it was a 'get' or a 'post'. */
  lazy val field_method = ApiField("method", TString)

  /** Gets the url of the request */
  lazy val field_url = ApiField("url", TString)

  /** Credentials name */
  lazy val field_credentials_name = ApiField("credentials name", TString)

  /** Credentials password */
  lazy val field_credentials_password = ApiField("credentials password", TString)

  /** Async response handler */
  lazy val field_handler = ApiField("handler", TWeb_Response_Action)
  override def possibleFields = super.possibleFields ++ List(field_header_storage, field_method, field_url,
    field_content, field_content_as_json, field_content_as_picture, field_content_as_picture_quality,
    field_content_as_xml, field_content_as_form,
    field_credentials_name, field_credentials_password, field_handler)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Indicates if both requests are the same instance. */
    case "equals" =>
      val List(other) = parameters // Web_Request
      Top[S](TBoolean)

    /** Gets the value of a given header */
    case "header" =>
      val List(name) = parameters // String
      Return[S](TString_Map.At[S](Field[S](this0, TWeb_Request.field_header_storage), name))

    /** Gets the names of the headers */
    case "header names" =>
      CallApi[S](Field[S](this0, TWeb_Request.field_header_storage), "keys", Nil, returnedType)

    /** Set what happens whenever the response comes back from 'send async'. */
    case "on response received" =>
      val List(handler) = parameters // Web_Response_Action
    val newState = AssignField[S](this0, TWeb_Request.field_handler, handler)
      New[S](TEvent_Binding)(newState, pp)

    /** Sends the request asynchronously. Attach a handler to 'on response received' to receive the response. */
    case "send async" =>
      Skip

    /** Performs the request synchronously */
    case "send" =>
      if (TouchAnalysisParameters.get.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not(), "send",
          "Check if the device is connected to the internet before using the connection")
      Top[S](TWeb_Response, Map(TWeb_Response.field_request -> this0))

    /** Sets the Accept header type ('text/xml' for xml, 'application/json' for json). */
    case "set accept" =>
      val List(typ) = parameters // String
      TString_Map.Insert(Field[S](this0, TWeb_Request.field_header_storage), String("Accept"), typ)

    /** Compresses the request content with gzip and sets the Content-Encoding header */
    case "set compress" =>
      val List(value) = parameters // Boolean
      TString_Map.Insert(Field[S](this0, TWeb_Request.field_header_storage), String("Content-Encoding"), value)

    /** Sets the content of a 'post' request as a JPEG encoded image. Quality from 0 (worse) to 1 (best). */
    case "set content as picture" =>
      val List(pic, quality) = parameters // Picture,Number
    var curState = state
      curState = AssignField[S](this0, TWeb_Request.field_content_as_picture, pic)(curState, pp)
      curState = AssignField[S](this0, TWeb_Request.field_content_as_picture_quality, quality)(curState, pp)
      curState

    /** Sets the name and password for basic authentication. Requires an HTTPS URL, empty string clears. */
    case "set credentials" =>
      val List(name, password) = parameters // String,String
    var curState = state
      curState = AssignField[S](this0, TWeb_Request.field_credentials_name, name)(curState, pp)
      curState = AssignField[S](this0, TWeb_Request.field_credentials_password, password)(curState, pp)
      curState

    /** Sets an HTML header value. Empty string clears the value */
    case "set header" =>
      val List(name, value) = parameters // String,String
      TString_Map.Insert(Field[S](this0, TWeb_Request.field_header_storage), name, value)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
