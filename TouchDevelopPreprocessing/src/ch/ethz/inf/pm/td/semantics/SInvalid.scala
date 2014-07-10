package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._

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

    case "action" => Return(Invalid(TAction.typ, "value may have been initialized to invalid"))
    case "appointment" => Return(Invalid(TAppointment.typ, "value may have been initialized to invalid")) // Creates an invalid Appointment instance
    case "appointment collection" => Return(Invalid(TAppointment_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Appointment Collection instance
    case "bluetooth device" => Return(Invalid(TBluetooth_Device.typ, "value may have been initialized to invalid")) // Creates an invalid Bluetooth Device instance
    case "board" => Return(Invalid(TBoard.typ, "value may have been initialized to invalid")) // Creates an invalid Board instance
    case "boolean" => Return(Invalid(TBoolean.typ, "value may have been initialized to invalid")) // Creates an invalid Boolean instance
    case "camera" => Return(Invalid(TCamera.typ, "value may have been initialized to invalid")) // Creates an invalid Camera instance
    case "color" => Return(Invalid(TColor.typ, "value may have been initialized to invalid")) // Creates an invalid Color instance
    case "contact" => Return(Invalid(TContact.typ, "value may have been initialized to invalid")) // Creates an invalid Contact instance
    case "contact collection" => Return(Invalid(TContact_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Contact Collection instance
    case "datetime" => Return(Invalid(TDateTime.typ, "value may have been initialized to invalid")) // Creates an invalid DateTime instance
    case "device" => Return(Invalid(TDevice.typ, "value may have been initialized to invalid")) // Creates an invalid Device instance
    case "device collection" => Return(Invalid(TDevice_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Device Collection instance
    case "json object" => Return(Invalid(TJson_Object.typ, "value may have been initialized to invalid")) // Creates an invalid Json Object instance
    case "link" => Return(Invalid(TLink.typ, "value may have been initialized to invalid")) // Creates an invalid Link instance
    case "link collection" => Return(Invalid(TLink_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Link Collection instance
    case "location" => Return(Invalid(TLocation.typ, "value may have been initialized to invalid")) // Creates an invalid Location instance
    case "location collection" => Return(Invalid(TLocation_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Location Collection instance
    case "map" => Return(Invalid(TMap.typ, "value may have been initialized to invalid")) // Creates an invalid Map instance
    case "media link" => Return(Invalid(TMedia_Link.typ, "value may have been initialized to invalid")) // Creates an invalid Media Link instance
    case "media link collection" => Return(Invalid(TMedia_Link_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Media Link Collection instance
    case "media player" => Return(Invalid(TMedia_Player.typ, "value may have been initialized to invalid")) // Creates an invalid Media Player instance
    case "media player collection" => Return(Invalid(TMedia_Player_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Media Player Collection instance
    case "media server" => Return(Invalid(TMedia_Server.typ, "value may have been initialized to invalid")) // Creates an invalid Media Server instance
    case "media server collection" => Return(Invalid(TMedia_Server_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Media Server Collection instance
    case "message" => Return(Invalid(TMessage.typ, "value may have been initialized to invalid")) // Creates an invalid Message instance
    case "message collection" => Return(Invalid(TMessage_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Message Collection instance
    case "motion" => Return(Invalid(TMotion.typ, "value may have been initialized to invalid")) // Creates an invalid Motion instance
    case "number" => Return(Invalid(TNumber.typ, "value may have been initialized to invalid")) // Creates an invalid Number instance
    case "number collection" => Return(Invalid(TNumber_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Number Collection instance
    case "number map" => Return(Invalid(TNumber_Map.typ, "value may have been initialized to invalid")) // Creates an invalid Number Map instance
    case "oauth response" => Return(Invalid(TOAuth_Response.typ, "value may have been initialized to invalid"))
    case "page" => Return(Invalid(TPage.typ, "value may have been initialized to invalid")) // Creates an invalid Page instance
    case "page button" => Return(Invalid(TPage_Button.typ, "value may have been initialized to invalid")) // Creates an invalid Page Button instance
    case "page collection" => Return(Invalid(TPage_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Page Collection instance
    case "picture" => Return(Invalid(TPicture.typ, "value may have been initialized to invalid")) // Creates an invalid Picture instance
    case "picture album" => Return(Invalid(TPicture_Album.typ, "value may have been initialized to invalid")) // Creates an invalid Picture Album instance
    case "picture albums" => Return(Invalid(TPicture_Albums.typ, "value may have been initialized to invalid")) // Creates an invalid Picture Albums instance
    case "pictures" => Return(Invalid(TPictures.typ, "value may have been initialized to invalid")) // Creates an invalid Pictures instance
    case "place" => Return(Invalid(TPlace.typ, "value may have been initialized to invalid")) // Creates an invalid Place instance
    case "place collection" => Return(Invalid(TPlace_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Place Collection instance
    case "playlist" => Return(Invalid(TPlaylist.typ, "value may have been initialized to invalid")) // Creates an invalid Playlist instance
    case "playlists" => Return(Invalid(TPlaylists.typ, "value may have been initialized to invalid")) // Creates an invalid Playlists instance
    case "printer" => Return(Invalid(TPrinter.typ, "value may have been initialized to invalid")) // Creates an invalid Printer instance
    case "printer collection" => Return(Invalid(TPrinter_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid Printer Collection instance
    case "song" => Return(Invalid(TSong.typ, "value may have been initialized to invalid")) // Creates an invalid Song instance
    case "song album" => Return(Invalid(TSong_Album.typ, "value may have been initialized to invalid")) // Creates an invalid Song Album instance
    case "song albums" => Return(Invalid(TSong_Albums.typ, "value may have been initialized to invalid")) // Creates an invalid Song Albums instance
    case "songs" => Return(Invalid(TSongs.typ, "value may have been initialized to invalid")) // Creates an invalid Songs instance
    case "sound" => Return(Invalid(TSound.typ, "value may have been initialized to invalid")) // Creates an invalid Sound instance
    case "sprite" => Return(Invalid(TSprite.typ, "value may have been initialized to invalid")) // Creates an invalid Sprite instance
    case "sprite set" => Return(Invalid(TSprite_Set.typ, "value may have been initialized to invalid")) // Creates an invalid Sprite Set instance
    case "string" => Return(Invalid(TString.typ, "value may have been initialized to invalid")) // Creates an invalid String instance
    case "string collection" => Return(Invalid(TString_Collection.typ, "value may have been initialized to invalid")) // Creates an invalid String Collection instance
    case "string map" => Return(Invalid(TString_Map.typ, "value may have been initialized to invalid")) // Creates an invalid String Map instance
    case "textbox" => Return(Invalid(TTextBox.typ, "value may have been initialized to invalid")) // Creates an invalid TextBox instance
    case "tile" => Return(Invalid(TTile.typ, "value may have been initialized to invalid")) // Creates an invalid Tile instance
    case "user" => Return(Invalid(TUser.typ, "value may have been initialized to invalid")) // Creates an invalid User instance
    case "vector3" => Return(Invalid(TVector3.typ, "value may have been initialized to invalid")) // Creates an invalid Vector3 instance
    case "web request" => Return(Invalid(TWeb_Request.typ, "value may have been initialized to invalid")) // Creates an invalid Web Request instance
    case "web response" => Return(Invalid(TWeb_Response.typ, "value may have been initialized to invalid")) // Creates an invalid Web Response instance
    case "xml object" => Return(Invalid(TXml_Object.typ, "value may have been initialized to invalid")) // Creates an invalid Xml Object instance

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

}