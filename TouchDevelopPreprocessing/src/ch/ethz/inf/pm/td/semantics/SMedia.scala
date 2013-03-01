package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{Identifier, ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import RichExpression._
import ch.ethz.inf.pm.td.semantics

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SMedia {

  /** Gets the list of built-in 48x48 icon names. You can see the icon list in the script settings. */
  val field_icon_names = new TouchField("icon_names",TString_Collection.typ)

  /** Gets the picture albums */
  val field_picture_albums = new TouchField("picture_albums",TPicture_Albums.typ)

  /** Gets the pictures on the phone */
  val field_pictures = new TouchField("pictures",TPictures.typ)

  /** Gets the playlists on the phone */
  val field_playlists = new TouchField("playlists",TPlaylists.typ)

  /** Gets the saved pictures on the phone */
  val field_saved_pictures = new TouchField("saved_pictures",TPictures.typ)

  /** Gets the song albums on the phone */
  val field_song_albums = new TouchField("song_albums",TSong_Albums.typ)

  /** Gets the songs on the phone */
  val field_songs = new TouchField("songs",TSongs.typ)

  val typName = "media"
  val typ = new TouchType(typName, isSingleton = true, List(field_icon_names, field_picture_albums, field_pictures,
    field_playlists, field_saved_pictures, field_song_albums, field_songs))

}

class SMedia extends AAny {

  def getTyp = SMedia.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Chooses a picture from the media library */
    case "choose_picture" =>
       //Return[S](CollectionSummary[S](Field[S](this0,SMedia.field_pictures)) or Invalid(TPicture.typ))
       Skip

    /** Creates a new game board */
    case "create_board" =>
      val List(height) = parameters // Number
      Error( height < 0 , "create_board", "Parameter height ("+height+") might be negative" )(state,pp)
      New[S](TBoard.typ,Map[Identifier,RichExpression](
        TBoard.field_width -> 480,
        TBoard.field_height -> height)) // According to Windows Phone Spec.

    /** Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
    case "create_full_board" =>
      New[S](TBoard.typ,Map[Identifier,RichExpression](
        TBoard.field_width -> 480,
        TBoard.field_height -> 800)) // According to Windows Phone Spec.

    /** Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
    case "create_landscape_board" =>
      val List(width,height) = parameters // Number,Number
      Error( width < 0 , "create_landscape_board", "Width ("+width+") might be negative" )(state,pp)
      Error( height < 0 , "create_landscape_board", "Height ("+height+") might be negative" )(state,pp)
      New[S](TBoard.typ,Map[Identifier,RichExpression](
        TBoard.field_width -> width,
        TBoard.field_height -> height)) // According to Windows Phone Spec.
      // TODO: Landscape??

    case "create_picture" =>
      val List(width,height) = parameters // Number,Number
      Error( width < 0 , "create_picture", "Picture width ("+width+") might be negative" )(state,pp)
      Error( height < 0 , "create_picture", "Picture height ("+height+") might be negative" )(state,pp)
      New[S](TPicture.typ,Map[Identifier,RichExpression](
        TPicture.field_width -> width,
        TPicture.field_height -> height))

    /** Creates a new game board in portrait mode. On rotatable devices it will take the entire screen when posted. */
    case "create_portrait_board" =>
      val List(width,height) = parameters // Number,Number
      Error( width < 0 , "create_portrait_board", "Width ("+width+") might be negative" )(state,pp)
      Error( height < 0 , "create_portrait_board", "Height ("+height+") might be negative" )(state,pp)
      New[S](TBoard.typ,Map[Identifier,RichExpression](
        TBoard.field_width -> width,
        TBoard.field_height -> height))
      // TODO: Portrait??

    /** Gets a 48x48 icon picture. Use 'media->icon names' to retrieve the list of names available. */
    case "icon" =>
      val List(name) = parameters // String
      New[S](TPicture.typ,Map[Identifier,RichExpression](
        TPicture.field_width -> 48,
        TPicture.field_height -> 48
      ))

    /** Gets a 96x96 icon picture. Use 'media->icon names' to retrieve the list of names available. */
    case "large_icon" =>
      val List(name) = parameters // String
      New[S](TPicture.typ,Map[Identifier,RichExpression](
        TPicture.field_width -> 96,
        TPicture.field_height -> 96
      ))

    /** Searches the Windows Phone Marketplace (type in applications or music) */
    case "search_marketplace" =>
      val List(terms,typ) = parameters // String,String
      Error[S](Field[S](Singleton(SWeb.typ),SWeb.field_is_connected).not,"search_marketplace",
        "Check if the device is connected to the internet before using the connection")
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}