package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SInvalid {

  val typName = "Invalid"
  val typ = new TouchType(typName, isSingleton = true)

}

class SInvalid extends AAny {

  def getTyp = SInvalid.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    case "appointment" => Return(Invalid(TAppointment.typ)) 	    	                                                                    // Creates an invalid Appointment instance
    case "appointment collection" => Return(Invalid(TAppointment_Collection.typ)) 	     	                                              // Creates an invalid Appointment Collection instance
    case "board" => Return(Invalid(TBoard.typ)) 	     	                                                                                // Creates an invalid Board instance
    case "boolean" => Return(Invalid(TBoolean.typ)) 	     	                                                                            // Creates an invalid Boolean instance
    case "camera" => Return(Invalid(TCamera.typ)) 	     	                                                                              // Creates an invalid Camera instance
    case "color" => Return(Invalid(TColor.typ)) 	     	                                                                                // Creates an invalid Color instance
    case "contact" => Return(Invalid(TContact.typ)) 	     	                                                                            // Creates an invalid Contact instance
    case "contact collection" => Return(Invalid(TContact_Collection.typ)) 	     	                                                      // Creates an invalid Contact Collection instance
    case "datetime" => Return(Invalid(TDateTime.typ)) 	     	                                                                          // Creates an invalid DateTime instance
    case "device" => Return(Invalid(TDevice.typ)) 	     	                                                                              // Creates an invalid Device instance
    case "device collection" => Return(Invalid(TDevice_Collection.typ)) 	     	                                                        // Creates an invalid Device Collection instance
    case "json object" => Return(Invalid(TJson_Object.typ)) 	     	                                                                    // Creates an invalid Json Object instance
    case "link" => Return(Invalid(TLink.typ)) 	     	                                                                                  // Creates an invalid Link instance
    case "link collection" => Return(Invalid(TLink_Collection.typ)) 	     	                                                            // Creates an invalid Link Collection instance
    case "location" => Return(Invalid(TLocation.typ)) 	     	                                                                          // Creates an invalid Location instance
    case "location collection" => Return(Invalid(TLocation_Collection.typ)) 	     	                                                    // Creates an invalid Location Collection instance
    case "map" => Return(Invalid(TMap.typ)) 	     	                                                                                    // Creates an invalid Map instance
    case "media link" => Return(Invalid(TMedia_Link.typ)) 	     	                                                                      // Creates an invalid Media Link instance
    case "media link collection" => Return(Invalid(TMedia_Link_Collection.typ)) 	     	                                                // Creates an invalid Media Link Collection instance
    case "media player" => Return(Invalid(TMedia_Player.typ)) 	     	                                                                  // Creates an invalid Media Player instance
    case "media player collection" => Return(Invalid(TMedia_Player_Collection.typ)) 	     	                                            // Creates an invalid Media Player Collection instance
    case "media server" => Return(Invalid(TMedia_Server.typ)) 	     	                                                                  // Creates an invalid Media Server instance
    case "media server collection" => Return(Invalid(TMedia_Server_Collection.typ)) 	     	                                            // Creates an invalid Media Server Collection instance
    case "message" => Return(Invalid(TMessage.typ)) 	     	                                                                            // Creates an invalid Message instance
    case "message collection" => Return(Invalid(TMessage_Collection.typ)) 	     	                                                      // Creates an invalid Message Collection instance
    case "motion" => Return(Invalid(TMotion.typ)) 	     	                                                                              // Creates an invalid Motion instance
    case "number" => Return(Invalid(TNumber.typ)) 	     	                                                                              // Creates an invalid Number instance
    case "number collection" => Return(Invalid(TNumber_Collection.typ)) 	     	                                                        // Creates an invalid Number Collection instance
    case "number map" => Return(Invalid(TNumber_Map.typ)) 	     	                                                                      // Creates an invalid Number Map instance
    case "page" => Return(Invalid(TPage.typ)) 	     	                                                                                  // Creates an invalid Page instance
    case "page button" => Return(Invalid(TPage_Button.typ)) 	     	                                                                    // Creates an invalid Page Button instance
    case "page collection" => Return(Invalid(TPage_Collection.typ)) 	     	                                                            // Creates an invalid Page Collection instance
    case "picture" => Return(Invalid(TPicture.typ)) 	     	                                                                            // Creates an invalid Picture instance
    case "picture album" => Return(Invalid(TPicture_Album.typ)) 	     	                                                                // Creates an invalid Picture Album instance
    case "picture albums" => Return(Invalid(TPicture_Albums.typ)) 	     	                                                              // Creates an invalid Picture Albums instance
    case "pictures" => Return(Invalid(TPictures.typ)) 	     	                                                                          // Creates an invalid Pictures instance
    case "place" => Return(Invalid(TPlace.typ)) 	     	                                                                                // Creates an invalid Place instance
    case "place collection" => Return(Invalid(TPlace_Collection.typ)) 	     	                                                          // Creates an invalid Place Collection instance
    case "playlist" => Return(Invalid(TPlaylist.typ)) 	     	                                                                          // Creates an invalid Playlist instance
    case "playlists" => Return(Invalid(TPlaylists.typ)) 	     	                                                                        // Creates an invalid Playlists instance
    case "printer" => Return(Invalid(TPrinter.typ)) 	     	                                                                            // Creates an invalid Printer instance
    case "printer collection" => Return(Invalid(TPrinter_Collection.typ)) 	     	                                                      // Creates an invalid Printer Collection instance
    case "song" => Return(Invalid(TSong.typ)) 	     	                                                                                  // Creates an invalid Song instance
    case "song album" => Return(Invalid(TSong_Album.typ)) 	     	                                                                      // Creates an invalid Song Album instance
    case "song albums" => Return(Invalid(TSong_Albums.typ)) 	     	                                                                    // Creates an invalid Song Albums instance
    case "songs" => Return(Invalid(TSongs.typ)) 	     	                                                                                // Creates an invalid Songs instance
    case "sound" => Return(Invalid(TSound.typ)) 	     	                                                                                // Creates an invalid Sound instance
    case "sprite" => Return(Invalid(TSprite.typ)) 	     	                                                                              // Creates an invalid Sprite instance
    case "sprite set" => Return(Invalid(TSprite_Set.typ)) 	     	                                                                      // Creates an invalid Sprite Set instance
    case "string" => Return(Invalid(TString.typ)) 	     	                                                                              // Creates an invalid String instance
    case "string collection" => Return(Invalid(TString_Collection.typ)) 	     	                                                        // Creates an invalid String Collection instance
    case "string map" => Return(Invalid(TString_Map.typ)) 	     	                                                                      // Creates an invalid String Map instance
    case "textbox" => Return(Invalid(TTextBox.typ)) 	     	                                                                            // Creates an invalid TextBox instance
    case "tile" => Return(Invalid(TTile.typ)) 	     	                                                                                  // Creates an invalid Tile instance
    case "vector3" => Return(Invalid(TVector3.typ)) 	     	                                                                            // Creates an invalid Vector3 instance
    case "web request" => Return(Invalid(TWeb_Request.typ)) 	                                                                          // Creates an invalid Web Request instance
    case "web response" => Return(Invalid(TWeb_Response.typ)) 	     	                                                                  // Creates an invalid Web Response instance
    case "xml object" => Return(Invalid(TXml_Object.typ)) 	    	                                                                      // Creates an invalid Xml Object instance

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}