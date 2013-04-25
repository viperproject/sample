
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

  /** Gets the uri of the media currently active */
  val field_active_media = new TouchField("active media",TString.typ)

  /** Gets the detailed information about this device */
  val field_device = new TouchField("device",TDevice.typ)

  /** Indicates the media can be played, paused, resumed */
  val field_is_control_supported = new TouchField("is control supported",TBoolean.typ)

  /** Indicates if the player is paused */
  val field_is_paused = new TouchField("is paused",TBoolean.typ)

  /** Indicates if the player is playing */
  val field_is_playing = new TouchField("is playing",TBoolean.typ)

  /** Indicates if the player is stopped */
  val field_is_stopped = new TouchField("is stopped",TBoolean.typ)

  /** Indicates if volume can be changed */
  val field_is_volume_supported = new TouchField("is volume supported",TBoolean.typ)

  /** Gets the name of the audio/video player */
  val field_name = new TouchField("name",TString.typ)

  /** Gets the status of the player */
  val field_status = new TouchField("status",TString.typ)

  /** Gets the current volume */
  val field_volume = new TouchField("volume",TNumber.typ)

  val typName = "Media Player"
  val typ = new TouchType(typName,isSingleton = false,fields = List(field_active_media, field_device,
    field_is_control_supported, field_is_paused, field_is_playing, field_is_stopped, field_is_volume_supported,
    field_name, field_status, field_volume))

}

class TMedia_Player extends AAny {

  def getTyp = TMedia_Player.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Moves the player to the next media in the queue. */
    case "next" =>
      Error[S](Field[S](this0,TMedia_Player.field_is_control_supported).not(),"next",
        "Trying to control a media play which might not support control")
      // TODO: Update active media
      Skip

    /** Pauses the current media if any. */
    case "pause" =>
      Error[S](Field[S](this0,TMedia_Player.field_is_control_supported).not(),"pause",
        "Trying to pause a media play which might not support control")
      // TODO: Update status
      Skip

    /** Plays the current media from the start. */
    case "play" =>
      Error[S](Field[S](this0,TMedia_Player.field_is_control_supported).not(),"play",
        "Trying to control a media play which might not support control")
      // TODO: Update status
      Skip

    /** Plays a media from the home network. */
    case "play home media" =>
      val List(media) = parameters // Media_Link
      Error[S](Field[S](this0,TMedia_Player.field_is_control_supported).not(),"play home media",
        "Trying to control a media play which might not support control")
      // TODO: Update active media
      Skip

    /** Plays the media at the 'url' internet address. */
    case "play media" =>
      val List(url) = parameters // String
      Error[S](Field[S](this0,TMedia_Player.field_is_control_supported).not(),"play media",
        "Trying to control a media play which might not support control")
      // TODO: Update status
      AssignField[S](this0,TMedia_Player.field_active_media,url)

    /** Gets the position in seconds whithin the active media */
    case "play position" =>
      Top[S](TNumber.typ)

    /** Moves the player to the previous media in the queue. */
    case "previous" =>
      Error[S](Field[S](this0,TMedia_Player.field_is_control_supported).not(),"previous",
        "Trying to control a media play which might not support control")
      // TODO: Update status
      Skip

    /** Resumes playing the current media if any. */
    case "resume" =>
      Error[S](Field[S](this0,TMedia_Player.field_is_control_supported).not(),"resume",
        "Trying to resume a media play which might not support control")
      // TODO: Update status
      Skip

    /** Sets the current value */
    case "set volume" =>
      val List(volume) = parameters // Number
      Error[S](Field[S](this0,TMedia_Player.field_is_volume_supported).not(),"set volume",
        "Trying to change the volume of a media play which might not be volume controllable")
      CheckInRangeInclusive[S](volume,0,1,"set volume","volume level")
      AssignField[S](this0,TMedia_Player.field_volume,volume)

    /** Stops the current media if any. */
    case "stop" =>
      Error[S](Field[S](this0,TMedia_Player.field_is_control_supported).not(),"stop",
        "Trying to stop a media play which might not support control")
      // TODO: Update status
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
