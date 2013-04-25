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

object SMedia {

  /** Gets the list of built-in 48x48 icon names. You can see the icon list in the script settings. */
  val field_icon_names = new TouchField("icon names",TString_Collection.typ)

  /** Gets the picture albums */
  val field_picture_albums = new TouchField("picture albums",TPicture_Albums.typ)

  /** Gets the pictures on the phone */
  val field_pictures = new TouchField("pictures",TPictures.typ)

  /** Gets the playlists on the phone */
  val field_playlists = new TouchField("playlists",TPlaylists.typ)

  /** Gets the saved pictures on the phone */
  val field_saved_pictures = new TouchField("saved pictures",TPictures.typ)

  /** Gets the song albums on the phone */
  val field_song_albums = new TouchField("song albums",TSong_Albums.typ)

  /** Gets the songs on the phone */
  val field_songs = new TouchField("songs",TSongs.typ)

  val typName = "Media"
  val typ = new TouchType(typName, isSingleton = true, fields = List(field_icon_names, field_picture_albums, field_pictures,
    field_playlists, field_saved_pictures, field_song_albums, field_songs))

}

class SMedia extends AAny {

  def getTyp = SMedia.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Chooses a picture from the media library */
    case "choose picture" =>
      val mediaPictures = Field[S](Singleton(SMedia.typ),SMedia.field_pictures)
      If[S](CollectionSize[S](mediaPictures) equal 0, Then = {
        Return[S](Invalid(TPicture.typ))(_,pp)
      }, Else = {
        Return[S](CollectionSummary[S](mediaPictures),Invalid(TPicture.typ))(_,pp)
      })

    /** Creates a new game board */
    case "create board" =>
      val List(height) = parameters // Number
      Error( height < 0 , "create board", "Parameter height ("+height+") might be negative" )(state,pp)
      New[S](TBoard.typ,Map(
        TBoard.field_width -> 480,
        TBoard.field_height -> height
      )) // According to Windows Phone Spec.

    /** Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
    case "create full board" =>
      New[S](TBoard.typ,Map(
        TBoard.field_width -> 480,
        TBoard.field_height -> 800
      )) // According to Windows Phone Spec.

    /** Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
    case "create landscape board" =>
      val List(width,height) = parameters // Number,Number
      Error( width < 0 , "create landscape board", "Width ("+width+") might be negative" )(state,pp)
      Error( height < 0 , "create landscape board", "Height ("+height+") might be negative" )(state,pp)
      New[S](TBoard.typ,Map(
        TBoard.field_width -> width,
        TBoard.field_height -> height
      )) // According to Windows Phone Spec.
      // TODO: Landscape??

    case "create picture" =>
      val List(width,height) = parameters // Number,Number
      Error( width < 0 , "create picture", "Picture width ("+width+") might be negative" )(state,pp)
      Error( height < 0 , "create picture", "Picture height ("+height+") might be negative" )(state,pp)
      New[S](TPicture.typ,Map(
        TPicture.field_width -> width,
        TPicture.field_height -> height
      ))

    /** Creates a new game board in portrait mode. On rotatable devices it will take the entire screen when posted. */
    case "create portrait board" =>
      val List(width,height) = parameters // Number,Number
      Error( width < 0 , "create portrait board", "Width ("+width+") might be negative" )(state,pp)
      Error( height < 0 , "create portrait board", "Height ("+height+") might be negative" )(state,pp)
      New[S](TBoard.typ,Map(
        TBoard.field_width -> width,
        TBoard.field_height -> height
      ))
      // TODO: Portrait??

    /** Gets a 48x48 icon picture. Use 'media->icon names' to retrieve the list of names available. */
    case "icon" =>
      val List(name) = parameters // String
      New[S](TPicture.typ,Map(
        TPicture.field_width -> 48,
        TPicture.field_height -> 48
      ))

    /** Gets a 96x96 icon picture. Use 'media->icon names' to retrieve the list of names available. */
    case "large icon" =>
      val List(name) = parameters // String
      New[S](TPicture.typ,Map(
        TPicture.field_width -> 96,
        TPicture.field_height -> 96
      ))

    /** Searches the Windows Phone Marketplace (type in applications or music) */
    case "search marketplace" =>
      val List(terms,typ) = parameters // String,String
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not,"search marketplace",
        "Check if the device is connected to the internet before using the connection")
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}