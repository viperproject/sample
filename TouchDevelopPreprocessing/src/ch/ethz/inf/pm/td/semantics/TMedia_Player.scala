
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Media Player
 *
 * An audio/video player on the home network
 *
 * @author Lucas Brutschy
 */ 

object TMedia_Player {

  val typName = "Media_Player"
  val typ = TouchType(typName,isSingleton = false,List())

}

class TMedia_Player extends AAny {

  def getTyp = TMedia_Player.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the uri of the media currently active */
    // case "active_media" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the uri of the media currently active */
    //   val field_active_media = new TouchField("active_media",TString.typ)

    /** Gets the detailled information about this device */
    // case "device" => 
    //   Return[S](Valid(TDevice.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the detailled information about this device */
    //   val field_device = new TouchField("device",TDevice.typ)

    /** Indicates the media can be played, paused, resumed */
    // case "is_control_supported" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates the media can be played, paused, resumed */
    //   val field_is_control_supported = new TouchField("is_control_supported",TBoolean.typ)

    /** Indicates if the player is paused */
    // case "is_paused" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the player is paused */
    //   val field_is_paused = new TouchField("is_paused",TBoolean.typ)

    /** Indicates if the player is playing */
    // case "is_playing" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the player is playing */
    //   val field_is_playing = new TouchField("is_playing",TBoolean.typ)

    /** Indicates if the player is stopped */
    // case "is_stopped" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the player is stopped */
    //   val field_is_stopped = new TouchField("is_stopped",TBoolean.typ)

    /** Indicates if volume can be changed */
    // case "is_volume_supported" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if volume can be changed */
    //   val field_is_volume_supported = new TouchField("is_volume_supported",TBoolean.typ)

    /** Gets the name of the audio/video player */
    // case "name" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the name of the audio/video player */
    //   val field_name = new TouchField("name",TString.typ)

    /** Moves the player to the next media in the queue. */
    // case "next" => 
    //   Skip;

    /** Pauses the current media if any. */
    // case "pause" => 
    //   Skip;

    /** Plays the current media from the start. */
    // case "play" => 
    //   Skip;

    /** Plays a media from the home network. */
    // case "play_home_media" => 
    //   val List(media) = parameters // Media_Link
    //   Skip;

    /** Plays the media at the 'url' internet address. */
    // case "play_media" => 
    //   val List(url) = parameters // String
    //   Skip;

    /** Gets the position in seconds whithin the active media */
    // case "play_position" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the position in seconds whithin the active media */
    //   val field_play_position = new TouchField("play_position",TNumber.typ)

    /** Moves the player to the previous media in the queue. */
    // case "previous" => 
    //   Skip;

    /** Resumes playing the current media if any. */
    // case "resume" => 
    //   Skip;

    /** Sets the current value */
    // case "set_volume" => 
    //   val List(volume) = parameters // Number
    //   Skip;

    /** Gets the status of the player */
    // case "status" => 
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the status of the player */
    //   val field_status = new TouchField("status",TString.typ)

    /** Stops the current media if any. */
    // case "stop" => 
    //   Skip;

    /** Gets the current volume */
    // case "volume" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the current volume */
    //   val field_volume = new TouchField("volume",TNumber.typ)

    // FIELDS: , field_active_media, field_device, field_is_control_supported, field_is_paused, field_is_playing, field_is_stopped, field_is_volume_supported, field_name, field_play_position, field_status, field_volume

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
