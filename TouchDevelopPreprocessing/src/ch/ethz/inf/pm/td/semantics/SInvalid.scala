package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.td.analysis.interpreter._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SInvalid {

  val typName = "Invalid"
  val typ = DefaultTouchType(typName, isSingleton = true)

}

class SInvalid extends AAny {

  def getTyp = SInvalid.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {

    case "appointment" => Return(Invalid(TAppointment.typ)) // Creates an invalid Appointment instance
    case "appointment collection" => Return(Invalid(TAppointment_Collection.typ)) // Creates an invalid Appointment Collection instance
    case "bluetooth device" => Return(Invalid(TBluetooth_Device.typ)) // Creates an invalid Bluetooth Device instance
    case "board" => Return(Invalid(TBoard.typ)) // Creates an invalid Board instance
    case "boolean" => Return(Invalid(TBoolean.typ)) // Creates an invalid Boolean instance
    case "camera" => Return(Invalid(TCamera.typ)) // Creates an invalid Camera instance
    case "color" => Return(Invalid(TColor.typ)) // Creates an invalid Color instance
    case "contact" => Return(Invalid(TContact.typ)) // Creates an invalid Contact instance
    case "contact collection" => Return(Invalid(TContact_Collection.typ)) // Creates an invalid Contact Collection instance
    case "datetime" => Return(Invalid(TDateTime.typ)) // Creates an invalid DateTime instance
    case "device" => Return(Invalid(TDevice.typ)) // Creates an invalid Device instance
    case "device collection" => Return(Invalid(TDevice_Collection.typ)) // Creates an invalid Device Collection instance
    case "json object" => Return(Invalid(TJson_Object.typ)) // Creates an invalid Json Object instance
    case "link" => Return(Invalid(TLink.typ)) // Creates an invalid Link instance
    case "link collection" => Return(Invalid(TLink_Collection.typ)) // Creates an invalid Link Collection instance
    case "location" => Return(Invalid(TLocation.typ)) // Creates an invalid Location instance
    case "location collection" => Return(Invalid(TLocation_Collection.typ)) // Creates an invalid Location Collection instance
    case "map" => Return(Invalid(TMap.typ)) // Creates an invalid Map instance
    case "media link" => Return(Invalid(TMedia_Link.typ)) // Creates an invalid Media Link instance
    case "media link collection" => Return(Invalid(TMedia_Link_Collection.typ)) // Creates an invalid Media Link Collection instance
    case "media player" => Return(Invalid(TMedia_Player.typ)) // Creates an invalid Media Player instance
    case "media player collection" => Return(Invalid(TMedia_Player_Collection.typ)) // Creates an invalid Media Player Collection instance
    case "media server" => Return(Invalid(TMedia_Server.typ)) // Creates an invalid Media Server instance
    case "media server collection" => Return(Invalid(TMedia_Server_Collection.typ)) // Creates an invalid Media Server Collection instance
    case "message" => Return(Invalid(TMessage.typ)) // Creates an invalid Message instance
    case "message collection" => Return(Invalid(TMessage_Collection.typ)) // Creates an invalid Message Collection instance
    case "motion" => Return(Invalid(TMotion.typ)) // Creates an invalid Motion instance
    case "number" => Return(Invalid(TNumber.typ)) // Creates an invalid Number instance
    case "number collection" => Return(Invalid(TNumber_Collection.typ)) // Creates an invalid Number Collection instance
    case "number map" => Return(Invalid(TNumber_Map.typ)) // Creates an invalid Number Map instance
    case "oauth response" => Return(Invalid(TOAuth_Response.typ))
    case "page" => Return(Invalid(TPage.typ)) // Creates an invalid Page instance
    case "page button" => Return(Invalid(TPage_Button.typ)) // Creates an invalid Page Button instance
    case "page collection" => Return(Invalid(TPage_Collection.typ)) // Creates an invalid Page Collection instance
    case "picture" => Return(Invalid(TPicture.typ)) // Creates an invalid Picture instance
    case "picture album" => Return(Invalid(TPicture_Album.typ)) // Creates an invalid Picture Album instance
    case "picture albums" => Return(Invalid(TPicture_Albums.typ)) // Creates an invalid Picture Albums instance
    case "pictures" => Return(Invalid(TPictures.typ)) // Creates an invalid Pictures instance
    case "place" => Return(Invalid(TPlace.typ)) // Creates an invalid Place instance
    case "place collection" => Return(Invalid(TPlace_Collection.typ)) // Creates an invalid Place Collection instance
    case "playlist" => Return(Invalid(TPlaylist.typ)) // Creates an invalid Playlist instance
    case "playlists" => Return(Invalid(TPlaylists.typ)) // Creates an invalid Playlists instance
    case "printer" => Return(Invalid(TPrinter.typ)) // Creates an invalid Printer instance
    case "printer collection" => Return(Invalid(TPrinter_Collection.typ)) // Creates an invalid Printer Collection instance
    case "song" => Return(Invalid(TSong.typ)) // Creates an invalid Song instance
    case "song album" => Return(Invalid(TSong_Album.typ)) // Creates an invalid Song Album instance
    case "song albums" => Return(Invalid(TSong_Albums.typ)) // Creates an invalid Song Albums instance
    case "songs" => Return(Invalid(TSongs.typ)) // Creates an invalid Songs instance
    case "sound" => Return(Invalid(TSound.typ)) // Creates an invalid Sound instance
    case "sprite" => Return(Invalid(TSprite.typ)) // Creates an invalid Sprite instance
    case "sprite set" => Return(Invalid(TSprite_Set.typ)) // Creates an invalid Sprite Set instance
    case "string" => Return(Invalid(TString.typ)) // Creates an invalid String instance
    case "string collection" => Return(Invalid(TString_Collection.typ)) // Creates an invalid String Collection instance
    case "string map" => Return(Invalid(TString_Map.typ)) // Creates an invalid String Map instance
    case "textbox" => Return(Invalid(TTextBox.typ)) // Creates an invalid TextBox instance
    case "tile" => Return(Invalid(TTile.typ)) // Creates an invalid Tile instance
    case "user" => Return(Invalid(TUser.typ)) // Creates an invalid User instance
    case "vector3" => Return(Invalid(TVector3.typ)) // Creates an invalid Vector3 instance
    case "web request" => Return(Invalid(TWeb_Request.typ)) // Creates an invalid Web Request instance
    case "web response" => Return(Invalid(TWeb_Response.typ)) // Creates an invalid Web Response instance
    case "xml object" => Return(Invalid(TXml_Object.typ)) // Creates an invalid Xml Object instance

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

  override def concreteSemantics(this0: TouchValue, method: String, params: List[TouchValue],
                                 interpreter: ConcreteInterpreter, pp: ProgramPoint): TouchValue = {
    method match {
      case "appointment" => InvalidV(TAppointment.typ) 	    	                                                                    // Creates an invalid Appointment instance
      case "appointment collection" => InvalidV(TAppointment_Collection.typ) 	     	                                              // Creates an invalid Appointment Collection instance
      case "bluetooth device" => InvalidV(TBluetooth_Device.typ) 	     	                                                          // Creates an invalid Bluetooth Device instance
      case "board" => InvalidV(TBoard.typ) 	     	                                                                                // Creates an invalid Board instance
      case "boolean" => InvalidV(TBoolean.typ) 	     	                                                                            // Creates an invalid Boolean instance
      case "camera" => InvalidV(TCamera.typ) 	     	                                                                              // Creates an invalid Camera instance
      case "color" => InvalidV(TColor.typ) 	     	                                                                                // Creates an invalid Color instance
      case "contact" => InvalidV(TContact.typ) 	     	                                                                            // Creates an invalid Contact instance
      case "contact collection" => InvalidV(TContact_Collection.typ) 	     	                                                      // Creates an invalid Contact Collection instance
      case "datetime" => InvalidV(TDateTime.typ) 	     	                                                                          // Creates an invalid DateTime instance
      case "device" => InvalidV(TDevice.typ) 	     	                                                                              // Creates an invalid Device instance
      case "device collection" => InvalidV(TDevice_Collection.typ) 	     	                                                        // Creates an invalid Device Collection instance
      case "json object" => InvalidV(TJson_Object.typ) 	     	                                                                    // Creates an invalid Json Object instance
      case "link" => InvalidV(TLink.typ) 	     	                                                                                  // Creates an invalid Link instance
      case "link collection" => InvalidV(TLink_Collection.typ) 	     	                                                            // Creates an invalid Link Collection instance
      case "location" => InvalidV(TLocation.typ) 	     	                                                                          // Creates an invalid Location instance
      case "location collection" => InvalidV(TLocation_Collection.typ) 	     	                                                    // Creates an invalid Location Collection instance
      case "map" => InvalidV(TMap.typ) 	     	                                                                                    // Creates an invalid Map instance
      case "media link" => InvalidV(TMedia_Link.typ) 	     	                                                                      // Creates an invalid Media Link instance
      case "media link collection" => InvalidV(TMedia_Link_Collection.typ) 	     	                                                // Creates an invalid Media Link Collection instance
      case "media player" => InvalidV(TMedia_Player.typ) 	     	                                                                  // Creates an invalid Media Player instance
      case "media player collection" => InvalidV(TMedia_Player_Collection.typ) 	     	                                            // Creates an invalid Media Player Collection instance
      case "media server" => InvalidV(TMedia_Server.typ) 	     	                                                                  // Creates an invalid Media Server instance
      case "media server collection" => InvalidV(TMedia_Server_Collection.typ) 	     	                                            // Creates an invalid Media Server Collection instance
      case "message" => InvalidV(TMessage.typ) 	     	                                                                            // Creates an invalid Message instance
      case "message collection" => InvalidV(TMessage_Collection.typ) 	     	                                                      // Creates an invalid Message Collection instance
      case "motion" => InvalidV(TMotion.typ) 	     	                                                                              // Creates an invalid Motion instance
      case "number" => InvalidV(TNumber.typ) 	     	                                                                              // Creates an invalid Number instance
      case "number collection" => InvalidV(TNumber_Collection.typ) 	     	                                                        // Creates an invalid Number Collection instance
      case "number map" => InvalidV(TNumber_Map.typ) 	     	                                                                      // Creates an invalid Number Map instance
      case "oauth response" => InvalidV(TOAuth_Response.typ)
      case "page" => InvalidV(TPage.typ) 	     	                                                                                  // Creates an invalid Page instance
      case "page button" => InvalidV(TPage_Button.typ) 	     	                                                                    // Creates an invalid Page Button instance
      case "page collection" => InvalidV(TPage_Collection.typ) 	     	                                                            // Creates an invalid Page Collection instance
      case "picture" => InvalidV(TPicture.typ) 	     	                                                                            // Creates an invalid Picture instance
      case "picture album" => InvalidV(TPicture_Album.typ) 	     	                                                                // Creates an invalid Picture Album instance
      case "picture albums" => InvalidV(TPicture_Albums.typ) 	     	                                                              // Creates an invalid Picture Albums instance
      case "pictures" => InvalidV(TPictures.typ) 	     	                                                                          // Creates an invalid Pictures instance
      case "place" => InvalidV(TPlace.typ) 	     	                                                                                // Creates an invalid Place instance
      case "place collection" => InvalidV(TPlace_Collection.typ) 	     	                                                          // Creates an invalid Place Collection instance
      case "playlist" => InvalidV(TPlaylist.typ) 	     	                                                                          // Creates an invalid Playlist instance
      case "playlists" => InvalidV(TPlaylists.typ) 	     	                                                                        // Creates an invalid Playlists instance
      case "printer" => InvalidV(TPrinter.typ) 	     	                                                                            // Creates an invalid Printer instance
      case "printer collection" => InvalidV(TPrinter_Collection.typ) 	     	                                                      // Creates an invalid Printer Collection instance
      case "song" => InvalidV(TSong.typ) 	     	                                                                                  // Creates an invalid Song instance
      case "song album" => InvalidV(TSong_Album.typ) 	     	                                                                      // Creates an invalid Song Album instance
      case "song albums" => InvalidV(TSong_Albums.typ) 	     	                                                                    // Creates an invalid Song Albums instance
      case "songs" => InvalidV(TSongs.typ) 	     	                                                                                // Creates an invalid Songs instance
      case "sound" => InvalidV(TSound.typ) 	     	                                                                                // Creates an invalid Sound instance
      case "sprite" => InvalidV(TSprite.typ) 	     	                                                                              // Creates an invalid Sprite instance
      case "sprite set" => InvalidV(TSprite_Set.typ) 	     	                                                                      // Creates an invalid Sprite Set instance
      case "string" => InvalidV(TString.typ) 	     	                                                                              // Creates an invalid String instance
      case "string collection" => InvalidV(TString_Collection.typ) 	     	                                                        // Creates an invalid String Collection instance
      case "string map" => InvalidV(TString_Map.typ) 	     	                                                                      // Creates an invalid String Map instance
      case "textbox" => InvalidV(TTextBox.typ) 	     	                                                                            // Creates an invalid TextBox instance
      case "tile" => InvalidV(TTile.typ) 	     	                                                                                  // Creates an invalid Tile instance
      case "vector3" => InvalidV(TVector3.typ) 	     	                                                                            // Creates an invalid Vector3 instance
      case "web request" => InvalidV(TWeb_Request.typ) 	                                                                          // Creates an invalid Web Request instance
      case "web response" => InvalidV(TWeb_Response.typ) 	     	                                                                  // Creates an invalid Web Response instance
      case "xml object" => InvalidV(TXml_Object.typ) 	    	                                                                      // Creates an invalid Xml Object instance
    }
  }

}