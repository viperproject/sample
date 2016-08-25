/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.td.analysis.{TopInitializer, ApiField}
import ch.ethz.inf.pm.td.parser.{Parameter, TypeName}
import ch.ethz.inf.pm.td.semantics._

import scala.None

/**
 *
 * Partly generated by GenerateTypeList
 *
 * Lucas Brutschy
 *
 */
object TypeList  {

  def reset() {
    userTypes = Map.empty
  }

  var userTypes: Map[TypeName, AAny] = Map.empty

  def toTouchType(typeName: TypeName): AAny = {
    TypeList.getTypeOrFail(typeName)
  }

  def toTouchTypes(names: List[TypeName]): List[AAny] = {
    for (typeName <- names) yield {
      TypeList.getTypeOrFail(typeName)
    }
  }

  def toTouchField(field: Parameter): ApiField = {
    ApiField(field.ident, TypeList.getTypeOrFail(field.typeName))
  }

  def toTouchFields(fields: List[Parameter]): Set[ApiField] = {
    (for (field <- fields) yield {
      ApiField(field.ident, TypeList.getTypeOrFail(field.typeName))
    }).toSet
  }

  def addTouchType(semantics: AAny) {
    TypeList.userTypes += semantics.typeName -> semantics
  }
  
  def getSingletons:List[ASingleton] = List(SApp,SArt,SBazaar,SBits,SBox,SCloud_Data,SCloud_Storage,
    SCode,SCollections,SColors,SContract,SCreate,SData,SDom,SHelpers,SHome,SLanguages,SLibs,SLocations,
    SMaps,SMath,SMedia,SPlayer,SPhone,SRadio,SRecords,SSenses,SSocial,STags,STiles,STime,STutorial,SWall,SWeb)

  def getTypeOrFail(typ:TypeName):AAny = getType(typ) match {
    case Some(x) => x
    case None => throw TouchException("TypeList: Non-existing type detected: "+typ)
  }

  def getType(typ:TypeName):Option[AAny] = {

    // Apparently, usertypes can override library types
    userTypes.get(typ) match {

      case Some(x) => Some(x)
      case None =>

        typ match {

          case SData.typeName => Some(SData)
          case SCode.typeName => Some(SCode)
          case SArt.typeName => Some(SArt)
          case SRecords.typeName => Some(SRecords)
          case SLibs.typeName => Some(SLibs)
          case SHelpers.typeName => Some(SHelpers)

          case TUnfinished_Ref_Collection.typeName => Some(TUnfinished_Ref_Collection)
          case TUnfinished_Collection.typeName => Some(TUnfinished_Collection)
          case TUnfinished_Ref.typeName => Some(TUnfinished_Ref)

          case TAtomic_Action.typeName => Some(TAtomic_Action)
          case TNothing.typeName => Some(TNothing)
          case TAction.typeName => Some(TAction)
          case TText_Action.typeName => Some(TText_Action)
          case TNumber_Action.typeName => Some(TNumber_Action)
          case TBoolean_Action.typeName => Some(TBoolean_Action)
          case TPosition_Action.typeName => Some(TPosition_Action)
          case TSprite_Action.typeName => Some(TSprite_Action)
          case TSprite_Set_Action.typeName => Some(TSprite_Set_Action)
          case TVector_Action.typeName => Some(TVector_Action)
          case TWeb_Response_Action.typeName => Some(TWeb_Response_Action)
          case TCollection_Message_Action.typeName => Some(TCollection_Message_Action)
          case TJson_Action.typeName => Some(TJson_Action)
          case TApp_Logger.typeName => Some(TApp_Logger)
          case TApp_Env.typeName => Some(TApp_Env)
          case SApp.typeName => Some(SApp)
          case TAppointment.typeName => Some(TAppointment)
          case SBazaar.typeName => Some(SBazaar)
          case SBits.typeName => Some(SBits)
          case TBluetooth_Device.typeName => Some(TBluetooth_Device)
          case TBoard.typeName => Some(TBoard)
          case TObstacle.typeName => Some(TObstacle)
          case TBoard_Background_Scene.typeName => Some(TBoard_Background_Scene)
          case TBoard_Background_Layer.typeName => Some(TBoard_Background_Layer)
          case TBoolean.typeName => Some(TBoolean)
          case SBox.typeName => Some(SBox)
          case SDom.typeName => Some(SDom)
          case TBuffer.typeName => Some(TBuffer)
          case TCamera.typeName => Some(TCamera)
          case TCloud_Picture.typeName => Some(TCloud_Picture)
          case TCloud_Session.typeName => Some(TCloud_Session)
          case SCloud_Data.typeName => Some(SCloud_Data)
          case SCollections.typeName => Some(SCollections)
          case TColor.typeName => Some(TColor)
          case SColors.typeName => Some(SColors)
          case TContact.typeName => Some(TContact)
          case SContract.typeName => Some(SContract)
          case SCreate.typeName => Some(SCreate)
          case TDateTime.typeName => Some(TDateTime)
          case TDevice.typeName => Some(TDevice)
          case TDocument.typeName => Some(TDocument)
          case TEditor.typeName => Some(TEditor)
          case TEnumerator.typeName => Some(TEnumerator)
          case TEvent_Binding.typeName => Some(TEvent_Binding)
          case TForm_Builder.typeName => Some(TForm_Builder)
          case SHome.typeName => Some(SHome)
          case SInvalid.typeName => Some(SInvalid)
          case TJson_Builder.typeName => Some(TJson_Builder)
          case TJson_Object.typeName => Some(TJson_Object)
          case SLanguages.typeName => Some(SLanguages)
          case TLink.typeName => Some(TLink)
          case SCloud_Storage.typeName => Some(SCloud_Storage)
          case TLocation.typeName => Some(TLocation)
          case SLocations.typeName => Some(SLocations)
          case TMap_Pushpin.typeName => Some(TMap_Pushpin)
          case TMap.typeName => Some(TMap)
          case SMaps.typeName => Some(SMaps)
          case SMath.typeName => Some(SMath)
          case TMatrix.typeName => Some(TMatrix)
          case SMedia.typeName => Some(SMedia)
          case TMedia_Link.typeName => Some(TMedia_Link)
          case TMedia_Player.typeName => Some(TMedia_Player)
          case TMedia_Server.typeName => Some(TMedia_Server)
          case TMessage.typeName => Some(TMessage)
          case TMotion.typeName => Some(TMotion)
          case TNumber.typeName => Some(TNumber)
          case TNumber_Map.typeName => Some(TNumber_Map)
          case TOAuth_Response.typeName => Some(TOAuth_Response)
          case TPage.typeName => Some(TPage)
          case TPage_Button.typeName => Some(TPage_Button)
          case SPhone.typeName => Some(SPhone)
          case TPicture.typeName => Some(TPicture)
          case TPicture_Album.typeName => Some(TPicture_Album)
          case TPicture_Albums.typeName => Some(TPicture_Albums)
          case TPictures.typeName => Some(TPictures)
          case TPlace.typeName => Some(TPlace)
          case SPlayer.typeName => Some(SPlayer)
          case TPlaylist.typeName => Some(TPlaylist)
          case TPlaylists.typeName => Some(TPlaylists)
          case TPrinter.typeName => Some(TPrinter)
          case SRadio.typeName => Some(SRadio)
          case SSenses.typeName => Some(SSenses)
          case TGamepad.typeName => Some(TGamepad)
          case TServer_Request.typeName => Some(TServer_Request)
          case TServer_Response.typeName => Some(TServer_Response)
          case SSocial.typeName => Some(SSocial)
          case TSong.typeName => Some(TSong)
          case TSong_Album.typeName => Some(TSong_Album)
          case TSong_Albums.typeName => Some(TSong_Albums)
          case TSongs.typeName => Some(TSongs)
          case TSound.typeName => Some(TSound)
          case TSpring.typeName => Some(TSpring)
          case TSprite.typeName => Some(TSprite)
          case TSprite_Animation.typeName => Some(TSprite_Animation)
          case TSprite_Set.typeName => Some(TSprite_Set)
          case TSprite_Sheet.typeName => Some(TSprite_Sheet)
          case TString.typeName => Some(TString)
          case TString_Map.typeName => Some(TString_Map)
          case STags.typeName => Some(STags)
          case TTextBox.typeName => Some(TTextBox)
          case TTile.typeName => Some(TTile)
          case STiles.typeName => Some(STiles)
          case STime.typeName => Some(STime)
          case TTimer.typeName => Some(TTimer)
          case STutorial.typeName => Some(STutorial)
          case TUser.typeName => Some(TUser)
          case TUnfinished_Type.typeName => Some(TUnfinished_Type)
          case TVector3.typeName => Some(TVector3)
          case SWall.typeName => Some(SWall)
          case TWeb_Event_Source.typeName => Some(TWeb_Event_Source)
          case SWeb.typeName => Some(SWeb)
          case TWeb_Request.typeName => Some(TWeb_Request)
          case TWeb_Response.typeName => Some(TWeb_Response)
          case TWeb_Socket_Message.typeName => Some(TWeb_Socket_Message)
          case TWeb_Socket.typeName => Some(TWeb_Socket)
          case TXml_Object.typeName => Some(TXml_Object)
          case TUnknown.typeName => Some(TUnknown)

          case TypeName("Converter",List(from,to),false,false) => Some(GConverter(TypeList.getTypeOrFail(from),TypeList.getTypeOrFail(to)))
          case TypeName("Task",List(elt),false,false) => Some(GTask(TypeList.getTypeOrFail(elt)))
          case TypeName("Action1",List(elt),false,false) => Some(GAction1(TypeList.getTypeOrFail(elt)))
          case TypeName("Atomic Action1",List(elt),false,false) => Some(GAtomic_Action1(TypeList.getTypeOrFail(elt)))
          case TypeName("Collection", List(elt),false,false) => Some(GCollection(TypeList.getTypeOrFail(elt)))
          case TypeName("Entry", List(key,value),false,false) => Some(GEntry(TypeList.getTypeOrFail(key),TypeList.getTypeOrFail(value)))
          case TypeName("Comparison", List(elt),false,false) => Some(GComparison(TypeList.getTypeOrFail(elt)))
          case TypeName("Predicate",List(elt),false,false) => Some(GPredicate(TypeList.getTypeOrFail(elt)))
          case TypeName("Number Converter",List(elt),false,false) => Some(GNumber_Converter(TypeList.getTypeOrFail(elt)))
          case TypeName("String Converter",List(elt),false,false) => Some(GString_Converter(TypeList.getTypeOrFail(elt)))
          case TypeName("Ref",List(elt),false,false) => Some(GRef(TypeList.getTypeOrFail(elt)))

          case _ =>

            userTypes.get(typ.copy(isUserDefined = true)) match {

              case Some(x) => Some(x)

              case _ =>
                if (CFGGenerator.isLibraryIdent(typ.ident))
                  Some(new ASingleton {
                    override def typeName: TypeName = typ
                  })
                else
                  None

            }

        }

    }
  }

}
