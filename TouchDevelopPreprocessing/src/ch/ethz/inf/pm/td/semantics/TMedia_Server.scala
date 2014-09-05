
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Media Server
 *
 * A media server on the home network
 *
 * @author Lucas Brutschy
 */

object TMedia_Server extends AAny {

  /** Gets the detailed information about this device */
  lazy val field_device = new TouchField("device", TDevice.typeName)

  /** Gets the name of the printer */
  lazy val field_name = new TouchField("name", TString.typeName)

  /** Gets a list of all pictures */
  lazy val field_pictures = new TouchField("pictures", TMedia_Link_Collection.typeName)

  /** Gets a list of all songs */
  lazy val field_songs = new TouchField("songs", TMedia_Link_Collection.typeName)

  /** Gets a list of all videos */
  lazy val field_videos = new TouchField("videos", TMedia_Link_Collection.typeName)

  lazy val typeName = TypeName("Media Server")

  override def possibleFields = super.possibleFields ++ List(field_device, field_name, field_pictures, field_songs, field_videos)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Chooses a picture */
    case "choose picture" =>
      TopWithInvalid[S](TMedia_Link, "user may cancel picture selection")

    /** Chooses a song */
    case "choose song" =>
      TopWithInvalid[S](TMedia_Link, "user may cancel song selection")

    /** Chooses a video or a movie */
    case "choose video" =>
      TopWithInvalid[S](TMedia_Link, "user may cancel video selection")

    /** Searches for pictures in a particular date range. */
    case "search pictures by date" =>
      val List(start, end) = parameters // DateTime,DateTime
      Top[S](TMedia_Link_Collection)

    /** Searches for pictures in a particular date range. */
    case "search videos by date" =>
      val List(start, end) = parameters // DateTime,DateTime
      Top[S](TMedia_Link_Collection)

    /** Searches for songs */
    case "search songs" =>
      val List(term) = parameters // String
      Top[S](TMedia_Link_Collection)

    /** Searches for videos */
    case "search videos" =>
      val List(term) = parameters // String
      Top[S](TMedia_Link_Collection)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
