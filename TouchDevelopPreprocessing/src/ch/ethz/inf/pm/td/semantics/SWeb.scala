
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of web
 *
 * Search and browse the web...
 *
 * @author Lucas Brutschy
 */

object SWeb extends ASingleton {

  /** Gets a name of the currently connected network servicing Internet requests */
  lazy val field_connection_name = new ApiField("connection name", TString.typeName)

  /** Gets the type of the network servicing Internet requests (unknown, none, ethernet, wifi, mobile) */
  lazy val field_connection_type = new ApiField("connection type", TString.typeName)

  /** Indicates whether any network connection is available */
  lazy val field_is_connected = new ApiField("is connected", TBoolean.typeName)

  lazy val typeName = TypeName("Web")

  override def possibleFields = super.possibleFields ++ List(field_connection_name, field_connection_type, field_is_connected)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Decodes a string that has been base64-encoded */
    case "base64 decode" =>
      val List(text) = parameters // String
      Top[S](TString)

    /** Converts a string into an base64-encoded string */
    case "base64 encode" =>
      val List(text) = parameters // String
      Top[S](TString)

    /** Opens a web browser to a url */
    case "browse" =>
      val List(url) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "browse",
          "Check if the device is connected to the internet before launching the browser")
      Skip

    /** Creates a web request */
    case "create request" =>
      val List(url) = parameters // String
      New[S](TWeb_Request)

    /** Downloads the content of an internet page (http get) */
    case "download" =>
      val List(url) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "download",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString, "server may be unreachable")

    /** Downloads a web service response as a JSON data structure (http get) */
    case "download json" =>
      val List(url) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "download json",
          "Check if the device is connected to the internet before using the connection")
      val newState = TopWithInvalid[S](TJson_Object, "server may be unreachable")
      newState

    /** Downloads a picture from internet */
    case "download picture" =>
      val List(url) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "download picture",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TPicture, "server may be unreachable")

    /** Create a streamed song file from internet (download happens when playing) */
    case "download song" =>
      val List(url, name) = parameters // String,String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "download song",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TSong, "server may be unreachable")

    /** Downloads a WAV sound file from internet */
    case "download sound" =>
      val List(url) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "download sound",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TSound, "server may be unreachable")

    /** Parses the newsfeed string (RSS 2.0 or Atom 1.0) into a message collection */
    case "feed" =>
      val List(value) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "feed",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TMessage_Collection, "server may be unreachable")

    /** Decodes a string that has been HTML-encoded */
    case "html decode" =>
      val List(html) = parameters // String
      Top[S](TString)

    /** Converts a text string into an HTML-encoded string */
    case "html encode" =>
      val List(text) = parameters // String
      Top[S](TString)

    /** Parses the string as a json object */
    case "json" =>
      val List(value) = parameters // String
      TopWithInvalid[S](TJson_Object, "JSON parsing may fail")

    /** Returns an empty json array */
    case "json array" =>
      New[S](TJson_Object, Map(TJson_Object.field_kind -> String("array")))

    /** Returns an empty json object */
    case "json object" =>
      New[S](TJson_Object, Map(TJson_Object.field_kind -> String("object")))

    /** Creates a multi-scale image from an image url */
    case "link deep zoom" =>
      val List(url) = parameters // String
      New[S](TLink, Map(
        TLink.field_kind -> String("image"),
        TLink.field_address -> toRichExpression(url)
      ))

    /** Creates a link to an internet image */
    case "link image" =>
      val List(url) = parameters // String
      New[S](TLink, Map(
        TLink.field_kind -> String("image"),
        TLink.field_address -> toRichExpression(url)
      ))

    /** Creates a link to an internet audio/video */
    case "link media" =>
      val List(url) = parameters // String
      New[S](TLink, Map(
        TLink.field_kind -> String("media"),
        TLink.field_address -> toRichExpression(url)
      ))

    /** Creates a link to an internet page */
    case "link url" =>
      val List(name, url) = parameters // String,String
      New[S](TLink, Map(
        TLink.field_address -> url,
        TLink.field_name -> name,
        TLink.field_kind -> String("hyperlink")
      ))

    /** Opens a connection settings page (airplanemode, bluetooth, wiki, cellular) */
    case "open connection settings" =>
      val List(page) = parameters // String
      //      Error[S](((page equal String("airplanemode"))
      //        || (page equal String("bluetooth"))
      //        || (page equal String("wiki"))
      //        || (page equal String("cellular"))).not,"open connection settings","Invalid page given")
      Skip

    /** Plays an internet audio/video in full screen */
    case "play media" =>
      val List(url) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not(), "play media",
          "Check if the device is connected to the internet before using the connection")
      Skip

    /** Parses the newsfeed string (RSS 2.0 or Atom 1.0) into a message collection */
    case "rss" =>
      val List(value) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "feed",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TMessage_Collection, "server may be unreachable")

    /** Searching the web using Bing */
    case "search" =>
      val List(terms) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "search",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection, "Bing may be unreachable")

    /** Searching images using Bing */
    case "search images" =>
      val List(terms) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "search images",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection, "Bing may be unreachable")

    /** Searching images near a location using Bing. Distance in meters, negative to ignore. */
    case "search images nearby" =>
      val List(terms, location, distance) = parameters // String,Location,Number
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "search images nearby",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection, "Bing may be unreachable")

    /** Searching the web near a location using Bing. Distance in meters, negative to ignore. */
    case "search nearby" =>
      val List(terms, location, distance) = parameters // String,Location,Number
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "search nearby",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection, "Bing may be unreachable")

    /** Searching news using Bing */
    case "search news" =>
      val List(terms) = parameters // String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "search news",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection, "Bing may be unreachable")

    /** Searching news near a location using Bing. Distance in meters, negative to ignore. */
    case "search news nearby" =>
      val List(terms, location, distance) = parameters // String,Location,Number
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "search news nearby",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection, "Bing may be unreachable")

    /** Search phone numbers near a location using Bing. Distance in meters, negative to ignore. */
    case "search phone numbers nearby" =>
      val List(query, location, distance) = parameters // String,Location,Number
      TopWithInvalid[S](TLink_Collection, "Bing may be unreachable")

    /** Uploads text to an internet page (http post) */
    case "upload" =>
      val List(url, body) = parameters // String,String
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "upload",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString, "server may be unreachable")

    /** Uploads a picture to an internet page (http post) */
    case "upload picture" =>
      val List(url, pic) = parameters // String,Picture
      if (TouchAnalysisParameters.reportPrematurelyOnInternetAccess)
        Error[S](Field[S](this0, SWeb.field_is_connected).not, "upload picture",
          "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString, "server may be unreachable") // TODO

    /** Decodes a string that has been url-encoded */
    case "url decode" =>
      val List(url) = parameters // String
      Top[S](TString)

    /** Converts a text string into an url-encoded string */
    case "url encode" =>
      val List(text) = parameters // String
      Top[S](TString)

    /** Parses the string as a xml element */
    case "xml" =>
      val List(value) = parameters // String
      Top[S](TXml_Object) // TODO

    /** Creates a json builder */
    case "create json builder" =>
      val List() = parameters //
      New[S](TJson_Builder)

    /** Parses a Command Separated Values document into a JsonObject where the `headers` is a string array of column names; `records` is an array of rows where each row is itself an array of strings. The delimiter is inferred if not specified. */
    case "csv" =>
      val List(text, delimiter) = parameters // String,String
      TopWithInvalid[S](TJson_Object, "CSV parsing may fail")

    /** Authenticate with OAuth 2.0 and receives the access token or error. See [](/oauthv2) for more information on which Redirect URI to choose. */
    case "oauth v2" =>
      val List(oauth_url) = parameters // String
      Top[S](TOAuth_Response)

    /** Create a form builder */
    case "create form builder" =>
      New[S](TForm_Builder)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
