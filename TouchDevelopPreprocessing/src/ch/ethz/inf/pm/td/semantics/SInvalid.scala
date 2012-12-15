package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SInvalid {

  val typName = "invalid"
  val typ = TouchType(typName, isSingleton = true)

}

class SInvalid extends AAny {

  def getTyp = SInvalid.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    //case "appointment" => Expr(invalid(Appointment.typ)) 	    	                                                                    // Creates an invalid Appointment instance
    //case "appointment_collection" => Expr(invalid(Appointment_Collection.typ)) 	     	                                              // Creates an invalid Appointment Collection instance
    case "board" => Return(Invalid(TBoard.typ)) 	     	                                                                                // Creates an invalid Board instance
    case "boolean" => Return(Invalid(TBoolean.typ)) 	     	                                                                            // Creates an invalid Boolean instance
    //case "camera" => Expr(invalid(Camera.typ)) 	     	                                                                              // Creates an invalid Camera instance
    case "color" => Return(Invalid(TColor.typ)) 	     	                                                                                // Creates an invalid Color instance
    //case "contact" => Expr(invalid(Contact.typ)) 	     	                                                                            // Creates an invalid Contact instance
    //case "contact_collection" => Expr(invalid(Contact_Collection.typ)) 	     	                                                      // Creates an invalid Contact Collection instance
    //case "datetime" => Expr(invalid(DateTime.typ)) 	     	                                                                          // Creates an invalid DateTime instance
    //case "device" => Expr(invalid(Device.typ)) 	     	                                                                              // Creates an invalid Device instance
    //case "device_collection" => Expr(invalid(Device_Collection.typ)) 	     	                                                        // Creates an invalid Device Collection instance
    //case "json_object" => Expr(invalid(Json_Object.typ)) 	     	                                                                    // Creates an invalid Json Object instance
    case "link" => Return(Invalid(TLink.typ)) 	     	                                                                                  // Creates an invalid Link instance
    //case "link_collection" => Expr(invalid(Link_Collection.typ)) 	     	                                                            // Creates an invalid Link Collection instance
    case "location" => Return(Invalid(TLocation.typ)) 	     	                                                                          // Creates an invalid Location instance
    //case "location_collection" => Expr(invalid(Location_Collection.typ)) 	     	                                                    // Creates an invalid Location Collection instance
    //case "map" => Expr(invalid(Map.typ)) 	     	                                                                                    // Creates an invalid Map instance
    //case "media_link" => Expr(invalid(Media_Link.typ)) 	     	                                                                      // Creates an invalid Media Link instance
    //case "media_link_collection" => Expr(invalid(Media_Link_Collection.typ)) 	     	                                                // Creates an invalid Media Link Collection instance
    //case "media_player" => Expr(invalid(Media_Player.typ)) 	     	                                                                  // Creates an invalid Media Player instance
    //case "media_player_collection" => Expr(invalid(Media_Player_Collection.typ)) 	     	                                            // Creates an invalid Media Player Collection instance
    //case "media_server" => Expr(invalid(Media_Server.typ)) 	     	                                                                  // Creates an invalid Media Server instance
    //case "media_server_collection" => Expr(invalid(Media_Server_Collection.typ)) 	     	                                            // Creates an invalid Media Server Collection instance
    //case "message" => Expr(invalid(Message.typ)) 	     	                                                                            // Creates an invalid Message instance
    //case "message_collection" => Expr(invalid(Message_Collection.typ)) 	     	                                                      // Creates an invalid Message Collection instance
    //case "motion" => Expr(invalid(Motion.typ)) 	     	                                                                              // Creates an invalid Motion instance
    case "number" => Return(Invalid(TNumber.typ)) 	     	                                                                              // Creates an invalid Number instance
    //case "number_collection" => Expr(invalid(Number_Collection.typ)) 	     	                                                        // Creates an invalid Number Collection instance
    //case "number_map" => Expr(invalid(Number_Map.typ)) 	     	                                                                      // Creates an invalid Number Map instance
    //case "page" => Expr(invalid(Page.typ)) 	     	                                                                                  // Creates an invalid Page instance
    //case "page_button" => Expr(invalid(Page_Button.typ)) 	     	                                                                    // Creates an invalid Page Button instance
    //case "page_collection" => Expr(invalid(Page_Collection.typ)) 	     	                                                            // Creates an invalid Page Collection instance
    case "picture" => Return(Invalid(TPicture.typ)) 	     	                                                                            // Creates an invalid Picture instance
    //case "picture_album" => Expr(invalid(Picture_Album.typ)) 	     	                                                                // Creates an invalid Picture Album instance
    //case "picture_albums" => Expr(invalid(Picture_Albums.typ)) 	     	                                                              // Creates an invalid Picture Albums instance
    //case "pictures" => Expr(invalid(Pictures.typ)) 	     	                                                                          // Creates an invalid Pictures instance
    //case "place" => Expr(invalid(Place.typ)) 	     	                                                                                // Creates an invalid Place instance
    //case "place_collection" => Expr(invalid(Place_Collection.typ)) 	     	                                                          // Creates an invalid Place Collection instance
    //case "playlist" => Expr(invalid(Playlist.typ)) 	     	                                                                          // Creates an invalid Playlist instance
    //case "playlists" => Expr(invalid(Playlists.typ)) 	     	                                                                        // Creates an invalid Playlists instance
    //case "printer" => Expr(invalid(Printer.typ)) 	     	                                                                            // Creates an invalid Printer instance
    //case "printer_collection" => Expr(invalid(Printer_Collection.typ)) 	     	                                                      // Creates an invalid Printer Collection instance
    //case "song" => Expr(invalid(Song.typ)) 	     	                                                                                  // Creates an invalid Song instance
    //case "song_album" => Expr(invalid(Song_Album.typ)) 	     	                                                                      // Creates an invalid Song Album instance
    //case "song_albums" => Expr(invalid(Song_Albums.typ)) 	     	                                                                    // Creates an invalid Song Albums instance
    //case "songs" => Expr(invalid(Songs.typ)) 	     	                                                                                // Creates an invalid Songs instance
    //case "sound" => Expr(invalid(Sound.typ)) 	     	                                                                                // Creates an invalid Sound instance
    //case "sprite" => Expr(invalid(Sprite.typ)) 	     	                                                                              // Creates an invalid Sprite instance
    //case "sprite_set" => Expr(invalid(Sprite_Set.typ)) 	     	                                                                      // Creates an invalid Sprite Set instance
    //case "string" => Expr(invalid(String.typ)) 	     	                                                                              // Creates an invalid String instance
    //case "string_collection" => Expr(invalid(String_Collection.typ)) 	     	                                                        // Creates an invalid String Collection instance
    //case "string_map" => Expr(invalid(String_Map.typ)) 	     	                                                                      // Creates an invalid String Map instance
    //case "textbox" => Expr(invalid(TextBox.typ)) 	     	                                                                            // Creates an invalid TextBox instance
    //case "tile" => Expr(invalid(Tile.typ)) 	     	                                                                                  // Creates an invalid Tile instance
    //case "vector3" => Expr(invalid(Vector3.typ)) 	     	                                                                            // Creates an invalid Vector3 instance
    //case "web_request" => Expr(invalid(Web_Request.typ)) 	                                                                          // Creates an invalid Web Request instance
    //case "web_response" => Expr(invalid(Web_Response.typ)) 	     	                                                                  // Creates an invalid Web Response instance
    //case "xml_object" => Expr(invalid(Xml_Object.typ)) 	    	                                                                      // Creates an invalid Xml Object instance

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}