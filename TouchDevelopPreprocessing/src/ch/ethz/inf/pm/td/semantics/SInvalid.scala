package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SInvalid
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SInvalid extends Default_SInvalid {

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {

    case "action" => Return(Invalid(TAction, "value may have been initialized to invalid"))
    case "appointment" => Return(Invalid(TAppointment, "value may have been initialized to invalid")) // Creates an invalid Appointment instance
    case "appointment collection" => Return(Invalid(TAppointment_Collection, "value may have been initialized to invalid")) // Creates an invalid Appointment Collection instance
    case "bluetooth device" => Return(Invalid(TBluetooth_Device, "value may have been initialized to invalid")) // Creates an invalid Bluetooth Device instance
    case "board" => Return(Invalid(TBoard, "value may have been initialized to invalid")) // Creates an invalid Board instance
    case "boolean" => Return(Invalid(TBoolean, "value may have been initialized to invalid")) // Creates an invalid Boolean instance
    case "camera" => Return(Invalid(TCamera, "value may have been initialized to invalid")) // Creates an invalid Camera instance
    case "color" => Return(Invalid(TColor, "value may have been initialized to invalid")) // Creates an invalid Color instance
    case "contact" => Return(Invalid(TContact, "value may have been initialized to invalid")) // Creates an invalid Contact instance
    case "contact collection" => Return(Invalid(TContact_Collection, "value may have been initialized to invalid")) // Creates an invalid Contact Collection instance
    case "datetime" => Return(Invalid(TDateTime, "value may have been initialized to invalid")) // Creates an invalid DateTime instance
    case "device" => Return(Invalid(TDevice, "value may have been initialized to invalid")) // Creates an invalid Device instance
    case "device collection" => Return(Invalid(TDevice_Collection, "value may have been initialized to invalid")) // Creates an invalid Device Collection instance
    case "json object" => Return(Invalid(TJson_Object, "value may have been initialized to invalid")) // Creates an invalid Json Object instance
    case "link" => Return(Invalid(TLink, "value may have been initialized to invalid")) // Creates an invalid Link instance
    case "link collection" => Return(Invalid(TLink_Collection, "value may have been initialized to invalid")) // Creates an invalid Link Collection instance
    case "location" => Return(Invalid(TLocation, "value may have been initialized to invalid")) // Creates an invalid Location instance
    case "location collection" => Return(Invalid(TLocation_Collection, "value may have been initialized to invalid")) // Creates an invalid Location Collection instance
    case "map" => Return(Invalid(TMap, "value may have been initialized to invalid")) // Creates an invalid Map instance
    case "media link" => Return(Invalid(TMedia_Link, "value may have been initialized to invalid")) // Creates an invalid Media Link instance
    case "media link collection" => Return(Invalid(TMedia_Link_Collection, "value may have been initialized to invalid")) // Creates an invalid Media Link Collection instance
    case "media player" => Return(Invalid(TMedia_Player, "value may have been initialized to invalid")) // Creates an invalid Media Player instance
    case "media player collection" => Return(Invalid(TMedia_Player_Collection, "value may have been initialized to invalid")) // Creates an invalid Media Player Collection instance
    case "media server" => Return(Invalid(TMedia_Server, "value may have been initialized to invalid")) // Creates an invalid Media Server instance
    case "media server collection" => Return(Invalid(TMedia_Server_Collection, "value may have been initialized to invalid")) // Creates an invalid Media Server Collection instance
    case "message" => Return(Invalid(TMessage, "value may have been initialized to invalid")) // Creates an invalid Message instance
    case "message collection" => Return(Invalid(TMessage_Collection, "value may have been initialized to invalid")) // Creates an invalid Message Collection instance
    case "motion" => Return(Invalid(TMotion, "value may have been initialized to invalid")) // Creates an invalid Motion instance
    case "number" => Return(Invalid(TNumber, "value may have been initialized to invalid")) // Creates an invalid Number instance
    case "number collection" => Return(Invalid(TNumber_Collection, "value may have been initialized to invalid")) // Creates an invalid Number Collection instance
    case "number map" => Return(Invalid(TNumber_Map, "value may have been initialized to invalid")) // Creates an invalid Number Map instance
    case "oauth response" => Return(Invalid(TOAuth_Response, "value may have been initialized to invalid"))
    case "page" => Return(Invalid(TPage, "value may have been initialized to invalid")) // Creates an invalid Page instance
    case "page button" => Return(Invalid(TPage_Button, "value may have been initialized to invalid")) // Creates an invalid Page Button instance
    case "page collection" => Return(Invalid(TPage_Collection, "value may have been initialized to invalid")) // Creates an invalid Page Collection instance
    case "picture" => Return(Invalid(TPicture, "value may have been initialized to invalid")) // Creates an invalid Picture instance
    case "picture album" => Return(Invalid(TPicture_Album, "value may have been initialized to invalid")) // Creates an invalid Picture Album instance
    case "picture albums" => Return(Invalid(TPicture_Albums, "value may have been initialized to invalid")) // Creates an invalid Picture Albums instance
    case "pictures" => Return(Invalid(TPictures, "value may have been initialized to invalid")) // Creates an invalid Pictures instance
    case "place" => Return(Invalid(TPlace, "value may have been initialized to invalid")) // Creates an invalid Place instance
    case "place collection" => Return(Invalid(TPlace_Collection, "value may have been initialized to invalid")) // Creates an invalid Place Collection instance
    case "playlist" => Return(Invalid(TPlaylist, "value may have been initialized to invalid")) // Creates an invalid Playlist instance
    case "playlists" => Return(Invalid(TPlaylists, "value may have been initialized to invalid")) // Creates an invalid Playlists instance
    case "printer" => Return(Invalid(TPrinter, "value may have been initialized to invalid")) // Creates an invalid Printer instance
    case "printer collection" => Return(Invalid(TPrinter_Collection, "value may have been initialized to invalid")) // Creates an invalid Printer Collection instance
    case "song" => Return(Invalid(TSong, "value may have been initialized to invalid")) // Creates an invalid Song instance
    case "song album" => Return(Invalid(TSong_Album, "value may have been initialized to invalid")) // Creates an invalid Song Album instance
    case "song albums" => Return(Invalid(TSong_Albums, "value may have been initialized to invalid")) // Creates an invalid Song Albums instance
    case "songs" => Return(Invalid(TSongs, "value may have been initialized to invalid")) // Creates an invalid Songs instance
    case "sound" => Return(Invalid(TSound, "value may have been initialized to invalid")) // Creates an invalid Sound instance
    case "sprite" => Return(Invalid(TSprite, "value may have been initialized to invalid")) // Creates an invalid Sprite instance
    case "sprite set" => Return(Invalid(TSprite_Set, "value may have been initialized to invalid")) // Creates an invalid Sprite Set instance
    case "string" => Return(Invalid(TString, "value may have been initialized to invalid")) // Creates an invalid String instance
    case "string collection" => Return(Invalid(TString_Collection, "value may have been initialized to invalid")) // Creates an invalid String Collection instance
    case "string map" => Return(Invalid(TString_Map, "value may have been initialized to invalid")) // Creates an invalid String Map instance
    case "textbox" => Return(Invalid(TTextBox, "value may have been initialized to invalid")) // Creates an invalid TextBox instance
    case "tile" => Return(Invalid(TTile, "value may have been initialized to invalid")) // Creates an invalid Tile instance
    case "user" => Return(Invalid(TUser, "value may have been initialized to invalid")) // Creates an invalid User instance
    case "vector3" => Return(Invalid(TVector3, "value may have been initialized to invalid")) // Creates an invalid Vector3 instance
    case "web request" => Return(Invalid(TWeb_Request, "value may have been initialized to invalid")) // Creates an invalid Web Request instance
    case "web response" => Return(Invalid(TWeb_Response, "value may have been initialized to invalid")) // Creates an invalid Web Response instance
    case "xml object" => Return(Invalid(TXml_Object, "value may have been initialized to invalid")) // Creates an invalid Xml Object instance

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

}