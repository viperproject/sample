
package ch.ethz.inf.pm.td.semantics

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

  val typName = "Media Server"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TMedia_Server extends AAny {

  def getTyp = TMedia_Server.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Chooses a picture */
    // case "choose_picture" => 
    //   Return[S](Valid(TMedia_Link.typ))
    // DECLARATION AS FIELD: 
    //   /** Chooses a picture */
    //   val field_choose_picture = new TouchField("choose_picture",TMedia_Link.typ)

    /** Chooses a song */
    // case "choose_song" => 
    //   Return[S](Valid(TMedia_Link.typ))
    // DECLARATION AS FIELD: 
    //   /** Chooses a song */
    //   val field_choose_song = new TouchField("choose_song",TMedia_Link.typ)

    /** Chooses a video or a movie */
    // case "choose_video" => 
    //   Return[S](Valid(TMedia_Link.typ))
    // DECLARATION AS FIELD: 
    //   /** Chooses a video or a movie */
    //   val field_choose_video = new TouchField("choose_video",TMedia_Link.typ)

    /** Gets the detailled information about this device */
    // case "device" => 
    //   Return[S](Valid(TDevice.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the detailled information about this device */
    //   val field_device = new TouchField("device",TDevice.typ)

    /** Gets the name of the printer */
    // case "name" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the name of the printer */
    //   val field_name = new TouchField("name",TString.typ)

    /** Gets a list of all pictures */
    // case "pictures" => 
    //   Return[S](Valid(TMedia_Link_Collection.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets a list of all pictures */
    //   val field_pictures = new TouchField("pictures",TMedia_Link_Collection.typ)

    /** Searches for pictures in a particular date range. */
    // case "search_pictures_by_date" => 
    //   val List(start,end) = parameters // DateTime,DateTime
    //   Return[S](Valid(TMedia_Link_Collection.typ))

    /** Searches for songs */
    // case "search_songs" => 
    //   val List(term) = parameters // String
    //   Return[S](Valid(TMedia_Link_Collection.typ))

    /** Searches for videos */
    // case "search_videos" => 
    //   val List(term) = parameters // String
    //   Return[S](Valid(TMedia_Link_Collection.typ))

    /** Searches for videos in a particular date range. */
    // case "search_videos_by_date" => 
    //   val List(start,end) = parameters // DateTime,DateTime
    //   Return[S](Valid(TMedia_Link_Collection.typ))

    /** Gets a list of all songs */
    // case "songs" => 
    //   Return[S](Valid(TMedia_Link_Collection.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets a list of all songs */
    //   val field_songs = new TouchField("songs",TMedia_Link_Collection.typ)

    /** Gets a list of all videos */
    // case "videos" => 
    //   Return[S](Valid(TMedia_Link_Collection.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets a list of all videos */
    //   val field_videos = new TouchField("videos",TMedia_Link_Collection.typ)

    // FIELDS: , field_choose_picture, field_choose_song, field_choose_video, field_device, field_name, field_pictures, field_songs, field_videos

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
