
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._

/**
 * Specifies the abstract semantics of Media Server
 *
 * A media server on the home network
 *
 * @author Lucas Brutschy
 */

object TMedia_Server {

  /** Gets the detailed information about this device */
  val field_device = new TouchField("device", TDevice.typName)

  /** Gets the name of the printer */
  val field_name = new TouchField("name", TString.typName)

  /** Gets a list of all pictures */
  val field_pictures = new TouchField("pictures", TMedia_Link_Collection.typName)

  /** Gets a list of all songs */
  val field_songs = new TouchField("songs", TMedia_Link_Collection.typName)

  /** Gets a list of all videos */
  val field_videos = new TouchField("videos", TMedia_Link_Collection.typName)

  val typName = "Media Server"
  val typ = DefaultTouchType(typName, isSingleton = false, fields = List(field_device, field_name, field_pictures, field_songs, field_videos))

}

class TMedia_Server extends AAny {

  def getTyp = TMedia_Server.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Chooses a picture */
    case "choose picture" =>
      TopWithInvalid[S](TMedia_Link.typ, "user may cancel picture selection")

    /** Chooses a song */
    case "choose song" =>
      TopWithInvalid[S](TMedia_Link.typ, "user may cancel song selection")

    /** Chooses a video or a movie */
    case "choose video" =>
      TopWithInvalid[S](TMedia_Link.typ, "user may cancel video selection")

    /** Searches for pictures in a particular date range. */
    case "search pictures by date" =>
      val List(start, end) = parameters // DateTime,DateTime
      Top[S](TMedia_Link_Collection.typ)

    /** Searches for pictures in a particular date range. */
    case "search videos by date" =>
      val List(start, end) = parameters // DateTime,DateTime
      Top[S](TMedia_Link_Collection.typ)

    /** Searches for songs */
    case "search songs" =>
      val List(term) = parameters // String
      Top[S](TMedia_Link_Collection.typ)

    /** Searches for videos */
    case "search videos" =>
      val List(term) = parameters // String
      Top[S](TMedia_Link_Collection.typ)

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
