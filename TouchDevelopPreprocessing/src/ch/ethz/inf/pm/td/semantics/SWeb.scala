
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of web
 *
 * Search and browse the web...
 *
 * @author Lucas Brutschy
 */ 

object SWeb {

  /** Gets a name of the currently connected network servicing Internet requests */
  val field_connection_name = new TouchField("connection_name",TString.typ)

  /** Gets the type of the network servicing Internet requests (unknown, none, ethernet, wifi, mobile) */
  val field_connection_type = new TouchField("connection_type",TString.typ)

  /** Indicates whether any network connection is available */
  val field_is_connected = new TouchField("is_connected",TBoolean.typ)

  val typName = "web"
  val typ = new TouchType(typName,isSingleton = true,List(field_connection_name,field_connection_type,field_is_connected))

}

class SWeb extends AAny {

  def getTyp = SWeb.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Decodes a string that has been base64-encoded */
    case "base64_decode" =>
      val List(text) = parameters // String
      Top[S](TString.typ)

    /** Converts a string into an base64-encoded string */
    case "base64_encode" =>
      val List(text) = parameters // String
      Top[S](TString.typ)

    /** Opens a web browser to a url */
    case "browse" =>
      val List(url) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"browse",
        "Check if the device is connected to the internet before using the connection")
      Skip

    /** Creates a web request */
    case "create_request" =>
      val List(url) = parameters // String
      New[S](TWeb_Request.typ)

    /** Downloads the content of an internet page (http get) */
    case "download" =>
      val List(url) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"download",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString.typ)

    /** Downloads a web service response as a JSON data structure (http get) */
    case "download_json" =>
      val List(url) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"download_json",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TJson_Object.typ)

    /** Downloads a picture from internet */
    case "download_picture" =>
      val List(url) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"download_picture",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TPicture.typ)

    /** Create a streamed song file from internet (download happens when playing) */
    case "download_song" =>
      val List(url,name) = parameters // String,String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"download_song",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TSong.typ)

    /** Downloads a WAV sound file from internet */
    case "download_sound" =>
      val List(url) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"download_sound",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TSound.typ)

    /** Parses the newsfeed string (RSS 2.0 or Atom 1.0) into a message collection */
    case "feed" =>
      val List(value) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"feed",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TMessage_Collection.typ)

    /** Decodes a string that has been HTML-encoded */
    case "html_decode" =>
      val List(html) = parameters // String
      Top[S](TString.typ)

    /** Converts a text string into an HTML-encoded string */
    case "html_encode" =>
      val List(text) = parameters // String
      Top[S](TString.typ)

    /** Parses the string as a json object */
    case "json" =>
      val List(value) = parameters // String
      TopWithInvalid[S](TJson_Object.typ)

    /** Returns an empty json array */
    case "json_array" =>
      // TODO: Enforce empty, array etc.
      New[S](TJson_Object.typ)

    /** Returns an empty json object */
    case "json_object" =>
      // TODO: Enforce empty, object etc.
      New[S](TJson_Object.typ)

    /** Creates a multi-scale image from an image url */
    case "link_deep_zoom" =>
      val List(url) = parameters // String
      New[S](TLink.typ,Map(
        TLink.field_kind -> String("image"),
        TLink.field_address -> toRichExpression(url)
      ))

    /** Creates a link to an internet image */
    case "link_image" =>
      val List(url) = parameters // String
      New[S](TLink.typ,Map(
        TLink.field_kind -> String("image"),
        TLink.field_address -> toRichExpression(url)
      ))

    /** Creates a link to an internet audio/video */
    case "link_media" =>
      val List(url) = parameters // String
      New[S](TLink.typ,Map(
        TLink.field_kind -> String("media"),
        TLink.field_address -> toRichExpression(url)
      ))

    /** Creates a link to an internet page */
    case "link_url" =>
       val List(name,url) = parameters // String,String
       New[S](TLink.typ,Map(
         TLink.field_address -> url,
         TLink.field_name -> name,
         TLink.field_kind -> String("hyperlink")
       ))

    /** Opens a connection settings page (airplanemode, bluetooth, wiki, cellular) */
    case "open_connection_settings" =>
      val List(page) = parameters // String
      Error[S](((page equal String("airplanemode"))
        || (page equal String("bluetooth"))
        || (page equal String("wiki"))
        || (page equal String("cellular"))).not,"open_connection_settings","Invalid page given")
      Skip

    /** Plays an internet audio/video in full screen */
    case "play_media" =>
      val List(url) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not(),"play_media",
        "Check if the device is connected to the internet before using the connection")
      Skip

    /** Parses the newsfeed string (RSS 2.0 or Atom 1.0) into a message collection */
    case "rss" =>
      val List(value) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"feed",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TMessage_Collection.typ)

    /** Searching the web using Bing */
    case "search" =>
      val List(terms) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"search",
        "Check if the device is connected to the internet before using the connection")
      Top[S](TLink_Collection.typ) // TODO

    /** Searching images using Bing */
    case "search_images" =>
      val List(terms) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"search_images",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection.typ) // TODO

    /** Searching images near a location using Bing. Distance in meters, negative to ignore. */
    case "search_images_nearby" =>
      val List(terms,location,distance) = parameters // String,Location,Number
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"search_images_nearby",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection.typ) // TODO

    /** Searching the web near a location using Bing. Distance in meters, negative to ignore. */
    case "search_nearby" =>
      val List(terms,location,distance) = parameters // String,Location,Number
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"search_nearby",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection.typ) // TODO

    /** Searching news using Bing */
    case "search_news" =>
      val List(terms) = parameters // String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"search_news",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection.typ) // TODO

    /** Searching news near a location using Bing. Distance in meters, negative to ignore. */
    case "search_news_nearby" =>
      val List(terms,location,distance) = parameters // String,Location,Number
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"search_news_nearby",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TLink_Collection.typ) // TODO

    /** Uploads text to an internet page (http post) */
    case "upload" =>
      val List(url,body) = parameters // String,String
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"upload",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString.typ) // TODO

    /** Uploads a picture to an internet page (http post) */
    case "upload_picture" =>
      val List(url,pic) = parameters // String,Picture
      Error[S](Field[S](this0,SWeb.field_is_connected).not,"upload_picture",
        "Check if the device is connected to the internet before using the connection")
      TopWithInvalid[S](TString.typ) // TODO

    /** Decodes a string that has been url-encoded */
    case "url_decode" =>
      val List(url) = parameters // String
      TopWithInvalid[S](TString.typ) // TODO

    /** Converts a text string into an url-encoded string */
    case "url_encode" =>
      val List(text) = parameters // String
      TopWithInvalid[S](TString.typ) // TODO

    /** Parses the string as a xml element */
    case "xml" =>
      val List(value) = parameters // String
      Top[S](TXml_Object.typ) // TODO

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
