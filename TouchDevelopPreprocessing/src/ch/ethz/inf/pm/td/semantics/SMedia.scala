package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SMedia {

  val typName = "media"
  val typ = TouchType(typName, isSingleton = true)

}

class SMedia extends AAny {

  def getTyp = SMedia.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Chooses a picture from the media library */
    case "choose_picture" =>
      New[S](TPicture.typ) // TODO

    /** Creates a new game board */
    case "create_board" =>
      val List(height) = parameters // Number
      Error( height < 0 , "create_board: Parameter height ("+height+") might be negative" )(state,pp)
      New[S](TBoard.typ,480,height) // According to Windows Phone Spec.

    /** Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
    case "create_full_board" =>
      New[S](TBoard.typ,480,800) // According to Windows Phone Spec.

    /** Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
    case "create_landscape_board" =>
      New[S](TBoard.typ,480,800) // According to Windows Phone Spec.

    /** Creates a new picture of the given size */
    case "create_picture" =>
      val List(width,height) = parameters // Number,Number
      Error( width < 0 , "create_picture: Picture width ("+width+") might be negative" )(state,pp)
      Error( height < 0 , "create_picture: Picture height ("+height+") might be negative" )(state,pp)
      New[S](TPicture.typ,width,height)

    /** Creates a new game board in portrait mode. On rotatable devices it will take the entire screen when posted. */
    case "create_portrait_board" =>
      val List(width,height) = parameters // Number,Number
      New[S](TBoard.typ) // TODO

    /** Gets a 48x48 icon picture. Use 'media->icon names' to retrieve the list of names available. */
    case "icon" =>
      val List(name) = parameters // String
      New[S](TPicture.typ) // TODO

    /** Gets the list of built-in 48x48 icon names. You can see the icon list in the script settings. */
    case "icon_names" =>
      New[S](TString_Collection.typ) // TODO

    /** Gets a 96x96 icon picture. Use 'media->icon names' to retrieve the list of names available. */
    case "large_icon" =>
      val List(name) = parameters // String
      New[S](TPicture.typ) // TODO

    /** Gets the picture albums */
    case "picture_albums" =>
      New[S](TPicture_Albums.typ) // TODO

    /** Gets the pictures on the phone */
    case "pictures" =>
      New[S](TPictures.typ) // TODO

    /** Gets the playlists on the phone */
    case "playlists" =>
      New[S](TPlaylists.typ) // TODO

    /** Gets the saved pictures on the phone */
    case "saved_pictures" =>
      New[S](TPictures.typ) // TODO

    /** Searches the Windows Phone Marketplace (type in applications or music) */
    case "search_marketplace" =>
      val List(terms,typ) = parameters // String,String
      Skip; // TODO

    /** Gets the song albums on the phone */
    case "song_albums" =>
      New[S](TSong_Albums.typ) // TODO

    /** Gets the songs on the phone */
    case "songs" =>
      New[S](TSongs.typ) // TODO

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}