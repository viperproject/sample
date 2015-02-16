
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Invalid
 *
 * Create invalid values
 *
 * @author Lucas Brutschy
 */

trait Default_SInvalid extends ASingleton {

  lazy val typeName = TypeName("Invalid")
          
  /** Never used: Creates an invalid Action instance */
  def member_action = ApiMember(
    name = "action",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TAction,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Appointment Collection instance */
  def member_appointment_collection = ApiMember(
    name = "appointment collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TAppointment),
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Appointment instance */
  def member_appointment = ApiMember(
    name = "appointment",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TAppointment,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid BlueTooth Device instance */
  def member_bluetooth_device = ApiMember(
    name = "bluetooth device",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBluetooth_Device,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an invalid Board instance */
  def member_board = ApiMember(
    name = "board",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoard,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Boolean instance */
  def member_boolean = ApiMember(
    name = "boolean",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Camera instance */
  def member_camera = ApiMember(
    name = "camera",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TCamera,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an invalid Color instance */
  def member_color = ApiMember(
    name = "color",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Contact Collection instance */
  def member_contact_collection = ApiMember(
    name = "contact collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TContact),
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Contact instance */
  def member_contact = ApiMember(
    name = "contact",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TContact,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid DateTime instance */
  def member_datetime = ApiMember(
    name = "datetime",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates an invalid Device Collection instance */
  def member_device_collection = ApiMember(
    name = "device collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TDevice),
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates an invalid Device instance */
  def member_device = ApiMember(
    name = "device",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDevice,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Form Builder instance */
  def member_form_builder = ApiMember(
    name = "form builder",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TForm_Builder,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Gamepad instance */
  def member_gamepad = ApiMember(
    name = "gamepad",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TGamepad,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Json Builder instance */
  def member_json_builder = ApiMember(
    name = "json builder",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Builder,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Json Object instance */
  def member_json_object = ApiMember(
    name = "json object",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Link Collection instance */
  def member_link_collection = ApiMember(
    name = "link collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TLink),
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Link instance */
  def member_link = ApiMember(
    name = "link",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Location Collection instance */
  def member_location_collection = ApiMember(
    name = "location collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TLocation),
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Location instance */
  def member_location = ApiMember(
    name = "location",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Map instance */
  def member_map = ApiMember(
    name = "map",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMap,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Matrix instance */
  def member_matrix = ApiMember(
    name = "matrix",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMatrix,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates an invalid Media Link Collection instance */
  def member_media_link_collection = ApiMember(
    name = "media link collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Link),
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates an invalid Media Link instance */
  def member_media_link = ApiMember(
    name = "media link",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMedia_Link,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates an invalid Media Player Collection instance */
  def member_media_player_collection = ApiMember(
    name = "media player collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Player),
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates an invalid Media Player instance */
  def member_media_player = ApiMember(
    name = "media player",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMedia_Player,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates an invalid Media Server Collection instance */
  def member_media_server_collection = ApiMember(
    name = "media server collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Server),
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates an invalid Media Server instance */
  def member_media_server = ApiMember(
    name = "media server",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMedia_Server,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Message Collection Action instance */
  def member_message_collection_action = ApiMember(
    name = "message collection action",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TAction,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Message Collection instance */
  def member_message_collection = ApiMember(
    name = "message collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TMessage),
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Message instance */
  def member_message = ApiMember(
    name = "message",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMessage,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Motion instance */
  def member_motion = ApiMember(
    name = "motion",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMotion,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Number Collection instance */
  def member_number_collection = ApiMember(
    name = "number collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TNumber),
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Number Map instance */
  def member_number_map = ApiMember(
    name = "number map",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber_Map,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an invalid Number instance */
  def member_number = ApiMember(
    name = "number",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid OAuth Response instance */
  def member_oauth_response = ApiMember(
    name = "oauth response",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TOAuth_Response,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Page Button instance */
  def member_page_button = ApiMember(
    name = "page button",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPage_Button,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Page Collection instance */
  def member_page_collection = ApiMember(
    name = "page collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TPage),
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Page instance */
  def member_page = ApiMember(
    name = "page",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPage,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Picture Album instance */
  def member_picture_album = ApiMember(
    name = "picture album",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture_Album,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Picture Albums instance */
  def member_picture_albums = ApiMember(
    name = "picture albums",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture_Albums,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an invalid Picture instance */
  def member_picture = ApiMember(
    name = "picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Pictures instance */
  def member_pictures = ApiMember(
    name = "pictures",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPictures,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Place Collection instance */
  def member_place_collection = ApiMember(
    name = "place collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TPlace),
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Place instance */
  def member_place = ApiMember(
    name = "place",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPlace,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Playlist instance */
  def member_playlist = ApiMember(
    name = "playlist",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPlaylist,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Playlists instance */
  def member_playlists = ApiMember(
    name = "playlists",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPlaylists,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Position Action instance */
  def member_position_action = ApiMember(
    name = "position action",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TAction,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates an invalid Printer Collection instance */
  def member_printer_collection = ApiMember(
    name = "printer collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TPrinter),
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Creates an invalid Printer instance */
  def member_printer = ApiMember(
    name = "printer",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPrinter,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Song Album instance */
  def member_song_album = ApiMember(
    name = "song album",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSong_Album,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Song Albums instance */
  def member_song_albums = ApiMember(
    name = "song albums",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSong_Albums,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an invalid Song instance */
  def member_song = ApiMember(
    name = "song",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSong,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Songs instance */
  def member_songs = ApiMember(
    name = "songs",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSongs,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Sound instance */
  def member_sound = ApiMember(
    name = "sound",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSound,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Sprite Action instance */
  def member_sprite_action = ApiMember(
    name = "sprite action",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TAction,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Sprite Animation instance */
  def member_sprite_animation = ApiMember(
    name = "sprite animation",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSprite_Animation,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Sprite Set Action instance */
  def member_sprite_set_action = ApiMember(
    name = "sprite set action",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TAction,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an invalid Sprite Set instance */
  def member_sprite_set = ApiMember(
    name = "sprite set",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSprite_Set,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an invalid Sprite instance */
  def member_sprite = ApiMember(
    name = "sprite",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSprite,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid String Collection instance */
  def member_string_collection = ApiMember(
    name = "string collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid String Map instance */
  def member_string_map = ApiMember(
    name = "string map",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString_Map,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an invalid String instance */
  def member_string = ApiMember(
    name = "string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Text Action instance */
  def member_text_action = ApiMember(
    name = "text action",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TAction,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid TextBox instance */
  def member_textbox = ApiMember(
    name = "textbox",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TTextBox,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Tile instance */
  def member_tile = ApiMember(
    name = "tile",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TTile,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid User instance */
  def member_user = ApiMember(
    name = "user",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TUser,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Vector Action instance */
  def member_vector_action = ApiMember(
    name = "vector action",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TAction,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an invalid Vector3 instance */
  def member_vector3 = ApiMember(
    name = "vector3",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Web Request instance */
  def member_web_request = ApiMember(
    name = "web request",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TWeb_Request,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid Web Response instance */
  def member_web_response = ApiMember(
    name = "web response",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TWeb_Response,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an invalid WebResponse Action instance */
  def member_webresponse_action = ApiMember(
    name = "webresponse action",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TAction,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an invalid Xml Object instance */
  def member_xml_object = ApiMember(
    name = "xml object",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TXml_Object,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "action" -> member_action,
    "appointment collection" -> member_appointment_collection,
    "appointment" -> member_appointment,
    "bluetooth device" -> member_bluetooth_device,
    "board" -> member_board,
    "boolean" -> member_boolean,
    "camera" -> member_camera,
    "color" -> member_color,
    "contact collection" -> member_contact_collection,
    "contact" -> member_contact,
    "datetime" -> member_datetime,
    "device collection" -> member_device_collection,
    "device" -> member_device,
    "form builder" -> member_form_builder,
    "gamepad" -> member_gamepad,
    "json builder" -> member_json_builder,
    "json object" -> member_json_object,
    "link collection" -> member_link_collection,
    "link" -> member_link,
    "location collection" -> member_location_collection,
    "location" -> member_location,
    "map" -> member_map,
    "matrix" -> member_matrix,
    "media link collection" -> member_media_link_collection,
    "media link" -> member_media_link,
    "media player collection" -> member_media_player_collection,
    "media player" -> member_media_player,
    "media server collection" -> member_media_server_collection,
    "media server" -> member_media_server,
    "message collection action" -> member_message_collection_action,
    "message collection" -> member_message_collection,
    "message" -> member_message,
    "motion" -> member_motion,
    "number collection" -> member_number_collection,
    "number map" -> member_number_map,
    "number" -> member_number,
    "oauth response" -> member_oauth_response,
    "page button" -> member_page_button,
    "page collection" -> member_page_collection,
    "page" -> member_page,
    "picture album" -> member_picture_album,
    "picture albums" -> member_picture_albums,
    "picture" -> member_picture,
    "pictures" -> member_pictures,
    "place collection" -> member_place_collection,
    "place" -> member_place,
    "playlist" -> member_playlist,
    "playlists" -> member_playlists,
    "position action" -> member_position_action,
    "printer collection" -> member_printer_collection,
    "printer" -> member_printer,
    "song album" -> member_song_album,
    "song albums" -> member_song_albums,
    "song" -> member_song,
    "songs" -> member_songs,
    "sound" -> member_sound,
    "sprite action" -> member_sprite_action,
    "sprite animation" -> member_sprite_animation,
    "sprite set action" -> member_sprite_set_action,
    "sprite set" -> member_sprite_set,
    "sprite" -> member_sprite,
    "string collection" -> member_string_collection,
    "string map" -> member_string_map,
    "string" -> member_string,
    "text action" -> member_text_action,
    "textbox" -> member_textbox,
    "tile" -> member_tile,
    "user" -> member_user,
    "vector action" -> member_vector_action,
    "vector3" -> member_vector3,
    "web request" -> member_web_request,
    "web response" -> member_web_response,
    "webresponse action" -> member_webresponse_action,
    "xml object" -> member_xml_object
  )
            

}
          
