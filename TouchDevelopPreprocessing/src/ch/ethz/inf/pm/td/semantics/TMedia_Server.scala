
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Media Server
 *
 * A media server on the home network
 *
 * @author Lucas Brutschy
 */ 

object TMedia_Server {

  /** Gets the detailled information about this device */
  val field_device = new TouchField("device",TDevice.typ)

  /** Gets the name of the printer */
  val field_name = new TouchField("name",TString.typ)

  /** Gets a list of all pictures */
  val field_pictures = new TouchField("pictures",TMedia_Link_Collection.typ)

  /** Gets a list of all songs */
  val field_songs = new TouchField("songs",TMedia_Link_Collection.typ)

  /** Gets a list of all videos */
  val field_videos = new TouchField("videos",TMedia_Link_Collection.typ)

  val typName = "Media_Server"
  val typ = new TouchType(typName,isSingleton = false,List(field_device,field_name,field_pictures,field_songs,field_videos))

}

class TMedia_Server extends AAny {

  def getTyp = TMedia_Server.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Chooses a picture */
    case "choose_picture" =>
      Top[S](TMedia_Link.typ)  // TODO: or invalid

    /** Chooses a song */
    case "choose_song" =>
      Top[S](TMedia_Link.typ)  // TODO: or invalid

    /** Chooses a video or a movie */
    case "choose_video" =>
      Top[S](TMedia_Link.typ) // TODO: or invalid

    /** Searches for pictures in a particular date range. */
    case "search_pictures_by_date" =>
    val List(start,end) = parameters // DateTime,DateTime
      Top[S](TMedia_Link_Collection.typ)

    /** Searches for songs */
    case "search_songs" =>
      val List(term) = parameters // String
      Top[S](TMedia_Link_Collection.typ)

    /** Searches for videos */
    case "search_videos" =>
      val List(term) = parameters // String
      Top[S](TMedia_Link_Collection.typ)

    /** Searches for videos in a particular date range. */
    case "search_videos_by_date" =>
      val List(start,end) = parameters // DateTime,DateTime
      Top[S](TMedia_Link_Collection.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
