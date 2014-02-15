package ch.ethz.inf.pm.td.compiler

import ch.ethz.inf.pm.td.semantics._

/**
 *
 * AUTOGENERATED BY generate_type_list.scala
 *
 * Lucas Brutschy
 *
 */

object TypeList  {

  var types : Map[String,AAny] = Map(
    SData.typName -> new SData(),
    SCode.typName -> new SCode(),
    SArt.typName -> new SArt(),
    SRecords.typName -> new SRecords(),
    SLibs.typName -> new SLibs(),
    SHelpers.typName -> new SHelpers(),

    TNothing.typName -> new TNothing(),
    TAction.typName -> new TAction(),
    TText_Action.typName -> new TText_Action(),
    TNumber_Action.typName -> new TNumber_Action(),
    TBoolean_Action.typName -> new TBoolean_Action(),
    TPosition_Action.typName -> new TPosition_Action(),
    TSprite_Action.typName -> new TSprite_Action(),
    TSprite_Set_Action.typName -> new TSprite_Set_Action(),
    TVector_Action.typName -> new TVector_Action(),
    TWeb_Response_Action.typName -> new TWeb_Response_Action(),
    TMessage_Collection_Action.typName -> new TMessage_Collection_Action(),
    SApp.typName -> new SApp(),
    TAppointment.typName -> new TAppointment(),
    TAppointment_Collection.typName -> new TAppointment_Collection(),
    SBazaar.typName -> new SBazaar(),
    SBits.typName -> new SBits(),
    TBluetooth_Device.typName -> new TBluetooth_Device(),
    TBoard.typName -> new TBoard(),
    TObstacle.typName -> new TObstacle(),
    TBoolean.typName -> new TBoolean(),
    SBox.typName -> new SBox(),
    TBuffer.typName -> new TBuffer(),
    TCamera.typName -> new TCamera(),
    TCloud_Session.typName -> new TCloud_Session(),
    SCloud_Data.typName -> new SCloud_Data(),
    SCollections.typName -> new SCollections(),
    TColor.typName -> new TColor(),
    SColors.typName -> new SColors(),
    TContact.typName -> new TContact(),
    TContact_Collection.typName -> new TContact_Collection(),
    SContract.typName -> new SContract(),
    TDateTime.typName -> new TDateTime(),
    TDevice.typName -> new TDevice(),
    TDevice_Collection.typName -> new TDevice_Collection(),
    TEnumerator.typName -> new TEnumerator(),
    TEvent_Binding.typName -> new TEvent_Binding(),
    TForm_Builder.typName -> new TForm_Builder(),
    SHome.typName -> new SHome(),
    SInvalid.typName -> new SInvalid(),
    TJson_Builder.typName -> new TJson_Builder(),
    TJson_Object.typName -> new TJson_Object(),
    SLanguages.typName -> new SLanguages(),
    TLink.typName -> new TLink(),
    TLink_Collection.typName -> new TLink_Collection(),
    TLocation.typName -> new TLocation(),
    TLocation_Collection.typName -> new TLocation_Collection(),
    SLocations.typName -> new SLocations(),
    TMap_Pushpin.typName -> new TMap_Pushpin(),
    TMap.typName -> new TMap(),
    SMaps.typName -> new SMaps(),
    SMath.typName -> new SMath(),
    TMatrix.typName -> new TMatrix(),
    SMedia.typName -> new SMedia(),
    TMedia_Link.typName -> new TMedia_Link(),
    TMedia_Link_Collection.typName -> new TMedia_Link_Collection(),
    TMedia_Player.typName -> new TMedia_Player(),
    TMedia_Player_Collection.typName -> new TMedia_Player_Collection(),
    TMedia_Server.typName -> new TMedia_Server(),
    TMedia_Server_Collection.typName -> new TMedia_Server_Collection(),
    TMessage.typName -> new TMessage(),
    TMessage_Collection.typName -> new TMessage_Collection(),
    TMotion.typName -> new TMotion(),
    TNumber.typName -> new TNumber(),
    TNumber_Collection.typName -> new TNumber_Collection(),
    TNumber_Map.typName -> new TNumber_Map(),
    TOAuth_Response.typName -> new TOAuth_Response(),
    TPage.typName -> new TPage(),
    TPage_Button.typName -> new TPage_Button(),
    TPage_Collection.typName -> new TPage_Collection(),
    SPhone.typName -> new SPhone(),
    TPicture.typName -> new TPicture(),
    TPicture_Album.typName -> new TPicture_Album(),
    TPicture_Albums.typName -> new TPicture_Albums(),
    TPictures.typName -> new TPictures(),
    TPlace.typName -> new TPlace(),
    TPlace_Collection.typName -> new TPlace_Collection(),
    SPlayer.typName -> new SPlayer(),
    TPlaylist.typName -> new TPlaylist(),
    TPlaylists.typName -> new TPlaylists(),
    TPrinter.typName -> new TPrinter(),
    TPrinter_Collection.typName -> new TPrinter_Collection(),
    SRadio.typName -> new SRadio(),
    SSenses.typName -> new SSenses(),
    SSocial.typName -> new SSocial(),
    TSong.typName -> new TSong(),
    TSong_Album.typName -> new TSong_Album(),
    TSong_Albums.typName -> new TSong_Albums(),
    TSongs.typName -> new TSongs(),
    TSound.typName -> new TSound(),
    TSpring.typName -> new TSpring(),
    TSprite.typName -> new TSprite(),
    TSprite_Animation.typName -> new TSprite_Animation(),
    TSprite_Set.typName -> new TSprite_Set(),
    TSprite_Sheet.typName -> new TSprite_Sheet(),
    TString.typName -> new TString(),
    TString_Collection.typName -> new TString_Collection(),
    TString_Map.typName -> new TString_Map(),
    STags.typName -> new STags(),
    TTask.typName -> new TTask(),
    TTextBox.typName -> new TTextBox(),
    TTile.typName -> new TTile(),
    STiles.typName -> new STiles(),
    STime.typName -> new STime(),
    TTimer.typName -> new TTimer(),
    TUser.typName -> new TUser(),
    TVector3.typName -> new TVector3(),
    SWall.typName -> new SWall(),
    SWeb.typName -> new SWeb(),
    TWeb_Request.typName -> new TWeb_Request(),
    TWeb_Response.typName -> new TWeb_Response(),
    TXml_Object.typName -> new TXml_Object(),
    TUnknown.typName -> new TUnknown(),

    GCollection.typName(TAction.typName) -> new GCollection(TAction.typName),
    GCollection.typName(TUser.typName) -> new GCollection(TUser.typName),
    GCollection.typName(TPicture.typName) -> new GCollection(TPicture.typName),
    GCollection.typName(TSound.typName) -> new GCollection(TSound.typName),
    GCollection.typName(TBluetooth_Device.typName) -> new GCollection(TBluetooth_Device.typName)

  )

}
    
