package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 *
 * AUTOGENERATED BY generate_type_list.scala
 *
 * Lucas Brutschy
 *
 */

object TypeList  {

  def reset() {
    userTypes = Map.empty
    records = Set.empty
  }

  var userTypes: Map[TypeName, AAny] = Map.empty
  var records: Set[ApiField] = Set.empty

  def addTouchType(semantics: AAny) {
    TypeList.userTypes += semantics.typeName -> semantics
  }

  def addRecord(name:String, typ:AAny) {
    TypeList.records += ApiField(name,typ)
  }
  
  def getSingletons = List(SBazaar,SBox,SCloud_Data,SCloud_Storage,
    SColors,SHome,SLanguages,SLocations,SMedia,SPlayer,SRadio,SRecords,SSenses,SSocial,STime,SWall,SWeb)

  def getType(xName: TypeName): AAny = {
    if (!CFGGenerator.isLibraryIdent(xName.ident)) {
      userTypes.get(xName) match {
        case Some(x) => x
        case None => types(xName)
      }
    } else new ASingleton {
      override def typeName: TypeName = xName
    }
  }

  def types(typ:TypeName):AAny = typ match {
    
    case SData.typeName => SData
    case SCode.typeName => SCode
    case SArt.typeName => SArt
    case SRecords.typeName => SRecords
    case SLibs.typeName => SLibs
    case SHelpers.typeName => SHelpers

    case TNothing.typeName => TNothing
    case TAction.typeName => TAction
    case TText_Action.typeName => TText_Action
    case TNumber_Action.typeName => TNumber_Action
    case TBoolean_Action.typeName => TBoolean_Action
    case TPosition_Action.typeName => TPosition_Action
    case TSprite_Action.typeName => TSprite_Action
    case TSprite_Set_Action.typeName => TSprite_Set_Action
    case TVector_Action.typeName => TVector_Action
    case TWeb_Response_Action.typeName => TWeb_Response_Action
    case TMessage_Collection_Action.typeName => TMessage_Collection_Action
    case TJson_Action.typeName => TJson_Action
    case SApp.typeName => SApp
    case TAppointment.typeName => TAppointment
    case TAppointment_Collection.typeName => TAppointment_Collection
    case SBazaar.typeName => SBazaar
    case SBits.typeName => SBits
    case TBluetooth_Device.typeName => TBluetooth_Device
    case TBluetooth_Le_Device.typeName => TBluetooth_Le_Device
    case TBoard.typeName => TBoard
    case TObstacle.typeName => TObstacle
    case TBoard_Background_Scene.typeName => TBoard_Background_Scene
    case TBoard_Background_Layer.typeName => TBoard_Background_Layer
    case TBoolean.typeName => TBoolean
    case SBox.typeName => SBox
    case TBuffer.typeName => TBuffer
    case TCamera.typeName => TCamera
    case TCloud_Picture.typeName => TCloud_Picture
    case TCloud_Session.typeName => TCloud_Session
    case SCloud_Data.typeName => SCloud_Data
    case SCollections.typeName => SCollections
    case TColor.typeName => TColor
    case SColors.typeName => SColors
    case TContact.typeName => TContact
    case TContact_Collection.typeName => TContact_Collection
    case SContract.typeName => SContract
    case SCreate.typeName => SCreate
    case TDateTime.typeName => TDateTime
    case TDevice.typeName => TDevice
    case TDevice_Collection.typeName => TDevice_Collection
    case TEditor.typeName => TEditor
    case TEnumerator.typeName => TEnumerator
    case TEvent_Binding.typeName => TEvent_Binding
    case TForm_Builder.typeName => TForm_Builder
    case SHome.typeName => SHome
    case SInvalid.typeName => SInvalid
    case TJson_Builder.typeName => TJson_Builder
    case TJson_Object.typeName => TJson_Object
    case SLanguages.typeName => SLanguages
    case TLink.typeName => TLink
    case TLink_Collection.typeName => TLink_Collection
    case SCloud_Storage.typeName => SCloud_Storage
    case TLocation.typeName => TLocation
    case TLocation_Collection.typeName => TLocation_Collection
    case SLocations.typeName => SLocations
    case TMap_Pushpin.typeName => TMap_Pushpin
    case TMap.typeName => TMap
    case SMaps.typeName => SMaps
    case SMath.typeName => SMath
    case TMatrix.typeName => TMatrix
    case SMedia.typeName => SMedia
    case TMedia_Link.typeName => TMedia_Link
    case TMedia_Link_Collection.typeName => TMedia_Link_Collection
    case TMedia_Player.typeName => TMedia_Player
    case TMedia_Player_Collection.typeName => TMedia_Player_Collection
    case TMedia_Server.typeName => TMedia_Server
    case TMedia_Server_Collection.typeName => TMedia_Server_Collection
    case TMessage.typeName => TMessage
    case TMessage_Collection.typeName => TMessage_Collection
    case TMotion.typeName => TMotion
    case TNumber.typeName => TNumber
    case TNumber_Collection.typeName => TNumber_Collection
    case TNumber_Map.typeName => TNumber_Map
    case TOAuth_Response.typeName => TOAuth_Response
    case TPage.typeName => TPage
    case TPage_Button.typeName => TPage_Button
    case TPage_Collection.typeName => TPage_Collection
    case SPhone.typeName => SPhone
    case TPicture.typeName => TPicture
    case TPicture_Album.typeName => TPicture_Album
    case TPicture_Albums.typeName => TPicture_Albums
    case TPictures.typeName => TPictures
    case TPlace.typeName => TPlace
    case TPlace_Collection.typeName => TPlace_Collection
    case SPlayer.typeName => SPlayer
    case TPlaylist.typeName => TPlaylist
    case TPlaylists.typeName => TPlaylists
    case TPrinter.typeName => TPrinter
    case TPrinter_Collection.typeName => TPrinter_Collection
    case SRadio.typeName => SRadio
    case SSenses.typeName => SSenses
    case TGamepad.typeName => TGamepad
    case TServer_Request.typeName => TServer_Request
    case TServer_Response.typeName => TServer_Response
    case SSocial.typeName => SSocial
    case TSong.typeName => TSong
    case TSong_Album.typeName => TSong_Album
    case TSong_Albums.typeName => TSong_Albums
    case TSongs.typeName => TSongs
    case TSound.typeName => TSound
    case TSpring.typeName => TSpring
    case TSprite.typeName => TSprite
    case TSprite_Animation.typeName => TSprite_Animation
    case TSprite_Set.typeName => TSprite_Set
    case TSprite_Sheet.typeName => TSprite_Sheet
    case TString.typeName => TString
    case TString_Collection.typeName => TString_Collection
    case TString_Map.typeName => TString_Map
    case STags.typeName => STags
    case TTextBox.typeName => TTextBox
    case TTile.typeName => TTile
    case STiles.typeName => STiles
    case STime.typeName => STime
    case TTimer.typeName => TTimer
    case TUser.typeName => TUser
    case TUnfinished_Type.typeName => TUnfinished_Type
    case TVector3.typeName => TVector3
    case SWall.typeName => SWall
    case TWeb_Event_Source.typeName => TWeb_Event_Source
    case SWeb.typeName => SWeb
    case TWeb_Request.typeName => TWeb_Request
    case TWeb_Response.typeName => TWeb_Response
    case TXml_Object.typeName => TXml_Object
    case TUnknown.typeName => TUnknown

    case TypeName("Collection", List(elt)) => GCollection(TypeList.types(elt))
    case TypeName("Entry", List(key,value)) => GEntry(TypeList.types(key),TypeList.types(value))
    case TypeName("Comparison", List(elt)) => GComparison(TypeList.types(elt))
    case TypeName("Predicate",List(elt)) => GPredicate(TypeList.types(elt))
    case TypeName("Number Converter",List(elt)) => GNumber_Converter(TypeList.types(elt))
    case TypeName("Collection",List(elt)) => GCollection(TypeList.types(elt))
    case TypeName("Ref",List(elt)) => GRef(TypeList.types(elt))

    case _ =>
      userTypes.get(typ) match {
        case Some(x) => x
        case None => throw TouchException("TypeList: Non-existing type detected: "+typ)
      }

  }

}
    
