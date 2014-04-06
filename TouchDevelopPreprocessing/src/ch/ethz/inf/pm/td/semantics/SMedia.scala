package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.analysis.interpreter._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:53 PM
 */

object SMedia {

  /** Gets the list of built-in 48x48 icon names. You can see the icon list in the script settings. */
  val field_icon_names = new TouchField("icon names",TString_Collection.typName)

  /** Gets the picture albums */
  val field_picture_albums = new TouchField("picture albums",TPicture_Albums.typName)

  /** Gets the pictures on the phone */
  val field_pictures = new TouchField("pictures",TPictures.typName)

  /** Gets the playlists on the phone */
  val field_playlists = new TouchField("playlists",TPlaylists.typName)

  /** Gets the saved pictures on the phone */
  val field_saved_pictures = new TouchField("saved pictures",TPictures.typName)

  /** Gets the song albums on the phone */
  val field_song_albums = new TouchField("song albums",TSong_Albums.typName)

  /** Gets the songs on the phone */
  val field_songs = new TouchField("songs",TSongs.typName)

  /** Width of phone screen (internal field, not in TD */
  val field_screen_width = new TouchField("screen width", TNumber.typName)

  /** Height of phone screen (internal field, not in TD */
  val field_screen_height = new TouchField("screen height", TNumber.typName)

  val typName = "Media"
  val typ = DefaultTouchType(typName, isSingleton = true, fields = List(field_icon_names, field_picture_albums, field_pictures,
    field_playlists, field_saved_pictures, field_song_albums, field_songs, field_screen_width, field_screen_height))

}

class SMedia extends AAny {

  def getTyp = SMedia.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    /** Chooses a picture from the media library */
    case "choose picture" =>
      NonDetReturn[S](TPicture.typ)

    /** Creates a new game board */
    case "create board" =>
      val List(height) = parameters // Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckNonNegative[S](height,"create board","height")
      }
      New[S](TBoard.typ,Map(
        TBoard.field_width -> 480,
        TBoard.field_height -> height
      )) // According to Windows Phone Spec.

    /** Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
    // TODO: Is this method deprecated? not documented in TouchDevelop API
    case "create full board" =>
      val widthExpr = Field[S](Singleton(SMedia.typ),SMedia.field_screen_width)
      val heightExpr = Field[S](Singleton(SMedia.typ),SMedia.field_screen_height)
      // Reasonable assumptions about screen size. not expressible with top field initializer
      val s1 = Assume[S](widthExpr > 0)
      val s2 = Assume[S](heightExpr > 0)(s1, pp)

      New[S](TBoard.typ,Map(
        TBoard.field_width -> widthExpr,
        TBoard.field_height -> heightExpr
      ))(s2, pp)

    /** Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
    case "create landscape board" =>
      val List(width,height) = parameters // Number,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckNonNegative[S](width,"create landscape board","width")
        CheckNonNegative[S](height,"create landscape board","height")
      }
      New[S](TBoard.typ,Map(
        TBoard.field_width -> width,
        TBoard.field_height -> height,
        TBoard.field_is_landscape -> True
      )) // According to Windows Phone Spec.
    // TODO: Landscape??

    case "create picture" =>
      val List(width,height) = parameters // Number,Number
    val checkedS =
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        val s1 = CheckNonNegative[S](width,"create picture","width")
        CheckNonNegative[S](height,"create picture","height")(s1,pp)
      } else state
      New[S](TPicture.typ,Map(
        TPicture.field_width -> width,
        TPicture.field_height -> height
      ))(checkedS, pp)

    /** Creates a new game board in portrait mode. On rotatable devices it will take the entire screen when posted. */
    case "create portrait board" =>
      val List(width,height) = parameters // Number,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckNonNegative[S](width,"create portrait board","width")
        CheckNonNegative[S](height,"create portrait board","height")
      }
      New[S](TBoard.typ,Map(
        TBoard.field_width -> width,
        TBoard.field_height -> height,
        TBoard.field_is_landscape -> False
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
        "Check if the device is connected to the internet before searching the marketplace")
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

  override def concreteSemantics(this0: TouchValue, method: String, params: List[TouchValue],
                                 interpreter: ConcreteInterpreter, pp: ProgramPoint): TouchValue = {

    val state = interpreter.state

    method match {
      case "create picture" => params match {
        case List(w@NumberV(width), h@NumberV(height)) =>
          interpreter.assertE(width > 0)(pp)
          interpreter.assertE(height > 0)(pp)
          state.createObject(TPicture.typ, Map("width" -> w, "height" -> h))
        case _ =>
          super.concreteSemantics(this0, method, params, interpreter, pp)
      }
      case "choose picture" =>
        interpreter.nonDetInputAt(pp).getOrElse(InvalidV(TPicture.typ))

      case "create full board" =>
        this0 match {
          case thisRef: RefV =>
            val phoneScreenWidth = state.getField(thisRef, SMedia.field_screen_width.getName)
            val phoneScreenHeight = state.getField(thisRef, SMedia.field_screen_height.getName)
            state.createObject(TBoard.typ, Map(
              TBoard.field_width.getName -> phoneScreenWidth,
              TBoard.field_height.getName -> phoneScreenHeight
            ))
        }

      case _ => super.concreteSemantics(this0, method, params, interpreter, pp)
    }
  }
}