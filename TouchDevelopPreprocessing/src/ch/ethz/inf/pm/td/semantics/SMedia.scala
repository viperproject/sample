package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SMedia extends ASingleton {

  /** Gets the list of built-in 48x48 icon names. You can see the icon list in the script settings. */
  lazy val field_icon_names = new TouchField("icon names", TString_Collection.typeName)

  /** Gets the picture albums */
  lazy val field_picture_albums = new TouchField("picture albums", TPicture_Albums.typeName)

  /** Gets the pictures on the phone */
  lazy val field_pictures = new TouchField("pictures", TPictures.typeName)

  /** Gets the playlists on the phone */
  lazy val field_playlists = new TouchField("playlists", TPlaylists.typeName)

  /** Gets the saved pictures on the phone */
  lazy val field_saved_pictures = new TouchField("saved pictures", TPictures.typeName)

  /** Gets the song albums on the phone */
  lazy val field_song_albums = new TouchField("song albums", TSong_Albums.typeName)

  /** Gets the songs on the phone */
  lazy val field_songs = new TouchField("songs", TSongs.typeName)

  lazy val typeName = TypeName("Media")

  override def possibleFields = super.possibleFields ++ List(field_icon_names, field_picture_albums, field_pictures,
    field_playlists, field_saved_pictures, field_song_albums, field_songs)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {

    /** Chooses a picture from the media library */
    case "choose picture" =>
      val mediaPictures = Field[S](Singleton(SMedia), SMedia.field_pictures)
      val res = If[S](TPictures.collectionSize[S](mediaPictures) equal 0, Then = {
        Return[S](Invalid(TPicture, "picture selection may be aborted by the user"))(_, pp)
      }, Else = {
        Return[S](TPictures.collectionAllValues[S](mediaPictures))(_, pp)
      })
      res

    /** Creates a new game board */
    case "create board" =>
      val List(height) = parameters // Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckNonNegative[S](height, "create board", "height")
      }
      New[S](TBoard, Map(
        TBoard.field_width -> 480,
        TBoard.field_height -> height
      )) // According to Windows Phone Spec.

    /** Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
    case "create full board" =>
      New[S](TBoard, Map(
        TBoard.field_width -> 480,
        TBoard.field_height -> 800
      )) // According to Windows Phone Spec.

    /** Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
    case "create landscape board" =>
      val List(width, height) = parameters // Number,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckNonNegative[S](width, "create landscape board", "width")
        CheckNonNegative[S](height, "create landscape board", "height")
      }
      val result = New[S](TBoard, Map(
        TBoard.field_width -> width,
        TBoard.field_height -> height,
        TBoard.field_is_landscape -> True
      )) // According to Windows Phone Spec.
      result
      // TODO: Landscape??

    case "create picture" =>
      val List(width, height) = parameters // Number,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckNonNegative[S](width, "create picture", "width")
        CheckNonNegative[S](height, "create picture", "height")
      }
      New[S](TPicture, Map(
        TPicture.field_width -> width,
        TPicture.field_height -> height
      ))

    /** Creates a new game board in portrait mode. On rotatable devices it will take the entire screen when posted. */
    case "create portrait board" =>
      val List(width, height) = parameters // Number,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckNonNegative[S](width, "create portrait board", "width")
        CheckNonNegative[S](height, "create portrait board", "height")
      }
      New[S](TBoard, Map(
        TBoard.field_width -> width,
        TBoard.field_height -> height,
        TBoard.field_is_landscape -> False
      ))
    // TODO: Portrait??

    /** Gets a 48x48 icon picture. Use 'media->icon names' to retrieve the list of names available. */
    case "icon" =>
      val List(name) = parameters // String
      New[S](TPicture, Map(
        TPicture.field_width -> 48,
        TPicture.field_height -> 48
      ))

    /** Gets a 96x96 icon picture. Use 'media->icon names' to retrieve the list of names available. */
    case "large icon" =>
      val List(name) = parameters // String
      New[S](TPicture, Map(
        TPicture.field_width -> 96,
        TPicture.field_height -> 96
      ))

    /** Searches the Windows Phone Marketplace (type in applications or music) */
    case "search marketplace" =>
      val List(terms, typ) = parameters // String,String
      Error[S](Field[S](Singleton(SWeb), SWeb.field_is_connected).not, "search marketplace",
        "Check if the device is connected to the internet before searching the marketplace")
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}