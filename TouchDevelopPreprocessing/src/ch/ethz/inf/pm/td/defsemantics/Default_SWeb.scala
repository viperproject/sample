
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Web
 *
 * Search and browse the web...
 *
 * @author Lucas Brutschy
 */

trait Default_SWeb extends ASingleton {

  lazy val typeName = TypeName("Web")
          
  /** Never used: Decodes a string that has been base64-encoded */
  def member_base64_decode = ApiMember(
    name = "base64 decode",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Converts a string into an base64-encoded string */
  def member_base64_encode = ApiMember(
    name = "base64 encode",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Opens a web browser to a url */
  def member_browse = ApiMember(
    name = "browse",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets a name of the currently connected network servicing Internet requests. Empty string if no connection. */
  def member_connection_name = ApiMember(
    name = "connection name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the type of the network servicing Internet requests (unknown, none, ethernet, wifi, mobile) */
  def member_connection_type = ApiMember(
    name = "connection type",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Opens an Server-Sent-Events client on the given URL. If not supported, returns invalid. The server must implement CORS to allow https://www.touchdevelop.com to receive messages. */
  def member_create_event_source = ApiMember(
    name = "create event source",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TWeb_Event_Source,
    semantics = DefaultSemantics
  )

  /** Never used: Create a form builder */
  def member_create_form_builder = ApiMember(
    name = "create form builder",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TForm_Builder,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a json builder */
  def member_create_json_builder = ApiMember(
    name = "create json builder",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Builder,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a web request */
  def member_create_request = ApiMember(
    name = "create request",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TWeb_Request,
    semantics = DefaultSemantics
  )

  /** Never used: Parses a Command Separated Values document into a JsonObject where the `headers` is a string array of column names; `records` is an array of rows where each row is itself an array of strings. The delimiter is inferred if not specified. */
  def member_csv = ApiMember(
    name = "csv",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: Decodes a URI component */
  def member_decode_uri_component = ApiMember(
    name = "decode uri component",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Decodes a URI component */
  def member_decode_uri = ApiMember(
    name = "decode uri",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Downloads a web service response as a JSON data structure (http get) */
  def member_download_json = ApiMember(
    name = "download json",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Downloads a picture from internet */
  def member_download_picture = ApiMember(
    name = "download picture",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Rarely used: Create a streamed song file from internet (download happens when playing) */
  def member_download_song = ApiMember(
    name = "download song",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TSong,
    semantics = DefaultSemantics
  )

  /** Rarely used: Downloads a WAV sound file from internet */
  def member_download_sound = ApiMember(
    name = "download sound",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TSound,
    semantics = DefaultSemantics
  )

  /** Never used: Downloads a web service response as a XML data structure (http get) */
  def member_download_xml = ApiMember(
    name = "download xml",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TXml_Object,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Downloads the content of an internet page (http get) */
  def member_download = ApiMember(
    name = "download",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Encodes a uri component */
  def member_encode_uri_component = ApiMember(
    name = "encode uri component",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Encodes a uri component */
  def member_encode_uri = ApiMember(
    name = "encode uri",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Parses the newsfeed string (RSS 2.0 or Atom 1.0) into a message collection */
  def member_feed = ApiMember(
    name = "feed",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TMessage_Collection,
    semantics = DefaultSemantics
  )

  /** Rarely used: Decodes a string that has been HTML-encoded */
  def member_html_decode = ApiMember(
    name = "html decode",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Converts a text string into an HTML-encoded string */
  def member_html_encode = ApiMember(
    name = "html encode",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates whether any network connection is available */
  def member_is_connected = ApiMember(
    name = "is connected",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Returns an empty json array */
  def member_json_array = ApiMember(
    name = "json array",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: Returns an empty json object */
  def member_json_object = ApiMember(
    name = "json object",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Parses the string as a json object */
  def member_json = ApiMember(
    name = "json",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates a multi-scale image from an image url */
  def member_link_deep_zoom = ApiMember(
    name = "link deep zoom",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a link to an internet image */
  def member_link_image = ApiMember(
    name = "link image",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates a link to an internet audio/video */
  def member_link_media = ApiMember(
    name = "link media",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a link to an internet page */
  def member_link_url = ApiMember(
    name = "link url",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Never used: Parses a OAuth v2.0 access token from a JSON payload as described in http://tools.ietf.org/html/rfc6749. Returns invalid if the payload is not an OAuth token. */
  def member_oauth_token_from_json = ApiMember(
    name = "oauth token from json",
    paramTypes = List(ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TOAuth_Response,
    semantics = DefaultSemantics
  )

  /** Never used: Parses a OAuth v2.0 access token from a redirect uri as described in http://tools.ietf.org/html/rfc6749. Returns invalid if the url does not contain an OAuth token. */
  def member_oauth_token_from_url = ApiMember(
    name = "oauth token from url",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TOAuth_Response,
    semantics = DefaultSemantics
  )

  /** Never used: Authenticate with OAuth 2.0 and receives the access token or error. See [](/oauthv2) for more information on which Redirect URI to choose. */
  def member_oauth_v2 = ApiMember(
    name = "oauth v2",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TOAuth_Response,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches code to run when a message is received. Only messages from the parent window and `origin` will be received. */
  def member_on_received_message_from_parent = ApiMember(
    name = "on received message from parent",
    paramTypes = List(ApiParam(TString), ApiParam(TJson_Action)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Opens a connection settings page (airplanemode, bluetooth, wifi, cellular) */
  def member_open_connection_settings = ApiMember(
    name = "open connection settings",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a picture from a web address. The resulting picture cannot be modified, use clone if you want to change it. */
  def member_picture = ApiMember(
    name = "picture",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Plays an internet audio/video in full screen */
  def member_play_media = ApiMember(
    name = "play media",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Posts a message to the parent window if any. The `target origin` must match the domain of the parent window, * is not accepted. */
  def member_post_message_to_parent = ApiMember(
    name = "post message to parent",
    paramTypes = List(ApiParam(TString), ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Redirects the browser to a url; only available when exporting */
  def member_redirect = ApiMember(
    name = "redirect",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Obsolete. Use 'feed' instead. */
  def member_rss = ApiMember(
    name = "rss",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TMessage_Collection,
    semantics = DefaultSemantics
  )

  /** Never used: Searching images near a location using Bing. Distance in meters, negative to ignore. */
  def member_search_images_nearby = ApiMember(
    name = "search images nearby",
    paramTypes = List(ApiParam(TString), ApiParam(TLocation), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TLink_Collection,
    semantics = DefaultSemantics
  )

  /** Rarely used: Searching images using Bing */
  def member_search_images = ApiMember(
    name = "search images",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLink_Collection,
    semantics = DefaultSemantics
  )

  /** Never used: Searching the web near a location using Bing. Distance in meters, negative to ignore. */
  def member_search_nearby = ApiMember(
    name = "search nearby",
    paramTypes = List(ApiParam(TString), ApiParam(TLocation), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TLink_Collection,
    semantics = DefaultSemantics
  )

  /** Never used: Searching news near a location using Bing. Distance in meters, negative to ignore. */
  def member_search_news_nearby = ApiMember(
    name = "search news nearby",
    paramTypes = List(ApiParam(TString), ApiParam(TLocation), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TLink_Collection,
    semantics = DefaultSemantics
  )

  /** Never used: Searching news using Bing */
  def member_search_news = ApiMember(
    name = "search news",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLink_Collection,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Search phone numbers near a location using Bing. Distance in meters, negative to ignore. */
  def member_search_phone_numbers_nearby = ApiMember(
    name = "search phone numbers nearby",
    paramTypes = List(ApiParam(TString), ApiParam(TLocation), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TLink_Collection,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Search phone numbers using Bing */
  def member_search_phone_numbers = ApiMember(
    name = "search phone numbers",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLink_Collection,
    semantics = DefaultSemantics
  )

  /** Rarely used: Searching the web using Bing */
  def member_search = ApiMember(
    name = "search",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLink_Collection,
    semantics = DefaultSemantics
  )

  /** Rarely used: Uploads a picture to an internet page (http post) */
  def member_upload_picture = ApiMember(
    name = "upload picture",
    paramTypes = List(ApiParam(TString), ApiParam(TPicture)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Uploads a sound to an internet page (http post). The sound must have been recorded from the microphone. */
  def member_upload_sound = ApiMember(
    name = "upload sound",
    paramTypes = List(ApiParam(TString), ApiParam(TSound)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Uploads text to an internet page (http post) */
  def member_upload = ApiMember(
    name = "upload",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Use `web->decode uri component` instead. */
  def member_url_decode = ApiMember(
    name = "url decode",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] Use `web->encode uri component` instead. */
  def member_url_encode = ApiMember(
    name = "url encode",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Waits for the next message from the parent window in `origin`. */
  def member_wait_for_message_from_parent = ApiMember(
    name = "wait for message from parent",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Parses the string as a xml element */
  def member_xml = ApiMember(
    name = "xml",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TXml_Object,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "base64 decode" -> member_base64_decode,
    "base64 encode" -> member_base64_encode,
    "browse" -> member_browse,
    "connection name" -> member_connection_name,
    "connection type" -> member_connection_type,
    "create event source" -> member_create_event_source,
    "create form builder" -> member_create_form_builder,
    "create json builder" -> member_create_json_builder,
    "create request" -> member_create_request,
    "csv" -> member_csv,
    "decode uri component" -> member_decode_uri_component,
    "decode uri" -> member_decode_uri,
    "download json" -> member_download_json,
    "download picture" -> member_download_picture,
    "download song" -> member_download_song,
    "download sound" -> member_download_sound,
    "download xml" -> member_download_xml,
    "download" -> member_download,
    "encode uri component" -> member_encode_uri_component,
    "encode uri" -> member_encode_uri,
    "feed" -> member_feed,
    "html decode" -> member_html_decode,
    "html encode" -> member_html_encode,
    "is connected" -> member_is_connected,
    "json array" -> member_json_array,
    "json object" -> member_json_object,
    "json" -> member_json,
    "link deep zoom" -> member_link_deep_zoom,
    "link image" -> member_link_image,
    "link media" -> member_link_media,
    "link url" -> member_link_url,
    "oauth token from json" -> member_oauth_token_from_json,
    "oauth token from url" -> member_oauth_token_from_url,
    "oauth v2" -> member_oauth_v2,
    "on received message from parent" -> member_on_received_message_from_parent,
    "open connection settings" -> member_open_connection_settings,
    "picture" -> member_picture,
    "play media" -> member_play_media,
    "post message to parent" -> member_post_message_to_parent,
    "redirect" -> member_redirect,
    "rss" -> member_rss,
    "search images nearby" -> member_search_images_nearby,
    "search images" -> member_search_images,
    "search nearby" -> member_search_nearby,
    "search news nearby" -> member_search_news_nearby,
    "search news" -> member_search_news,
    "search phone numbers nearby" -> member_search_phone_numbers_nearby,
    "search phone numbers" -> member_search_phone_numbers,
    "search" -> member_search,
    "upload picture" -> member_upload_picture,
    "upload sound" -> member_upload_sound,
    "upload" -> member_upload,
    "url decode" -> member_url_decode,
    "url encode" -> member_url_encode,
    "wait for message from parent" -> member_wait_for_message_from_parent,
    "xml" -> member_xml
  )
            

}
          
