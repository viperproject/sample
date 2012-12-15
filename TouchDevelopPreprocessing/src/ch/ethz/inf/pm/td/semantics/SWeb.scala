package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of web
 *
 * Search and browse the web...
 *
 * @author Lucas Brutschy
 */

object SWeb {

  val typName = "web"
  val typ = TouchType(typName,isSingleton = true,List())

}

class SWeb extends AAny {

  def getTyp = SWeb.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Decodes a string that has been base64-encoded */
    // case "base64_decode" => 
    //   val List(text) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Converts a string into an base64-encoded string */
    // case "base64_encode" => 
    //   val List(text) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Opens a web browser to a url */
    // case "browse" => 
    //   val List(url) = parameters // String
    //   Skip;

    /** Gets a name of the currently connected network servicing Internet requests */
    // case "connection_name" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets a name of the currently connected network servicing Internet requests */
    //   field_connection_name = new TouchField("connection_name",TString.typ)

    /** Gets the type of the network servicing Internet requests (unknown, none, ethernet, wifi, mobile) */
    // case "connection_type" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the type of the network servicing Internet requests (unknown, none, ethernet, wifi, mobile) */
    //   field_connection_type = new TouchField("connection_type",TString.typ)

    /** Creates a web request */
    // case "create_request" => 
    //   val List(url) = parameters // String
    //   Return[S](Valid(TWeb_Request.typ))

    /** Downloads the content of an internet page (http get) */
    // case "download" => 
    //   val List(url) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Downloads a web service response as a JSON data structure (http get) */
    // case "download_json" => 
    //   val List(url) = parameters // String
    //   Return[S](Valid(TJson_Object.typ))

    /** Downloads a picture from internet */
    // case "download_picture" => 
    //   val List(url) = parameters // String
    //   Return[S](Valid(TPicture.typ))

    /** Create a streamed song file from internet (download happens when playing) */
    // case "download_song" => 
    //   val List(url,name) = parameters // String,String
    //   Return[S](Valid(TSong.typ))

    /** Downloads a WAV sound file from internet */
    // case "download_sound" => 
    //   val List(url) = parameters // String
    //   Return[S](Valid(TSound.typ))

    /** Parses the newsfeed string (RSS 2.0 or Atom 1.0) into a message collection */
    // case "feed" => 
    //   val List(value) = parameters // String
    //   Return[S](Valid(TMessage_Collection.typ))

    /** Decodes a string that has been HTML-encoded */
    // case "html_decode" => 
    //   val List(html) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Converts a text string into an HTML-encoded string */
    // case "html_encode" => 
    //   val List(text) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Indicates whether any network connection is available */
    // case "is_connected" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates whether any network connection is available */
    //   field_is_connected = new TouchField("is_connected",TBoolean.typ)

    /** Parses the string as a json object */
    // case "json" => 
    //   val List(value) = parameters // String
    //   Return[S](Valid(TJson_Object.typ))

    /** Returns an empty json array */
    // case "json_array" => 
    //   Return[S](Valid(TJson_Object.typ))
    // DECLARATION AS FIELD: 
    //   /** Returns an empty json array */
    //   field_json_array = new TouchField("json_array",TJson_Object.typ)

    /** Returns an empty json object */
    // case "json_object" => 
    //   Return[S](Valid(TJson_Object.typ))
    // DECLARATION AS FIELD: 
    //   /** Returns an empty json object */
    //   field_json_object = new TouchField("json_object",TJson_Object.typ)

    /** Creates a link to an internet image */
    // case "link_image" => 
    //   val List(url) = parameters // String
    //   Return[S](Valid(TLink.typ))

    /** Creates a link to an internet audio/video */
    // case "link_media" => 
    //   val List(url) = parameters // String
    //   Return[S](Valid(TLink.typ))

    /** Creates a link to an internet page */
    // case "link_url" => 
    //   val List(name,url) = parameters // String,String
    //   Return[S](Valid(TLink.typ))

    /** Opens a connection settings page (airplanemode, bluetooth, wiki, cellular) */
    // case "open_connection_settings" => 
    //   val List(page) = parameters // String
    //   Skip;

    /** Plays an internet audio/video in full screen */
    // case "play_media" => 
    //   val List(url) = parameters // String
    //   Skip;

    /** Searching the web using Bing */
    // case "search" => 
    //   val List(terms) = parameters // String
    //   Return[S](Valid(TLink_Collection.typ))

    /** Searching images using Bing */
    // case "search_images" => 
    //   val List(terms) = parameters // String
    //   Return[S](Valid(TLink_Collection.typ))

    /** Searching images near a location using Bing. Distance in meters, negative to ignore. */
    // case "search_images_nearby" => 
    //   val List(terms,location,distance) = parameters // String,Location,Number
    //   Return[S](Valid(TLink_Collection.typ))

    /** Searching the web near a location using Bing. Distance in meters, negative to ignore. */
    // case "search_nearby" => 
    //   val List(terms,location,distance) = parameters // String,Location,Number
    //   Return[S](Valid(TLink_Collection.typ))

    /** Searching news using Bing */
    // case "search_news" => 
    //   val List(terms) = parameters // String
    //   Return[S](Valid(TLink_Collection.typ))

    /** Searching news near a location using Bing. Distance in meters, negative to ignore. */
    // case "search_news_nearby" => 
    //   val List(terms,location,distance) = parameters // String,Location,Number
    //   Return[S](Valid(TLink_Collection.typ))

    /** Uploads text to an internet page (http post) */
    // case "upload" => 
    //   val List(url,body) = parameters // String,String
    //   Return[S](Valid(TString.typ))

    /** Uploads a picture to an internet page (http post) */
    // case "upload_picture" => 
    //   val List(url,pic) = parameters // String,Picture
    //   Return[S](Valid(TString.typ))

    /** Decodes a string that has been url-encoded */
    // case "url_decode" => 
    //   val List(url) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Converts a text string into an url-encoded string */
    // case "url_encode" => 
    //   val List(text) = parameters // String
    //   Return[S](Valid(TString.typ))

    /** Parses the string as a xml element */
    // case "xml" => 
    //   val List(value) = parameters // String
    //   Return[S](Valid(TXml_Object.typ))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}