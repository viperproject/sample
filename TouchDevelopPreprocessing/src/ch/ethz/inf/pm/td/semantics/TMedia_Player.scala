
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TMedia_Player
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Media Player
 *
 * An audio/video player on the home network
 *
 * @author Lucas Brutschy
 */ 

object TMedia_Player extends Default_TMedia_Player {

  /** Gets the uri of the media currently active */
  lazy val field_active_media = ApiField("active media", TString)

  /** Gets the detailed information about this device */
  lazy val field_device = ApiField("device", TDevice)

  /** Indicates the media can be played, paused, resumed */
  lazy val field_is_control_supported = ApiField("is control supported", TBoolean)

  /** Indicates if the player is paused */
  lazy val field_is_paused = ApiField("is paused", TBoolean)

  /** Indicates if the player is playing */
  lazy val field_is_playing = ApiField("is playing", TBoolean)

  /** Indicates if the player is stopped */
  lazy val field_is_stopped = ApiField("is stopped", TBoolean)

  /** Indicates if volume can be changed */
  lazy val field_is_volume_supported = ApiField("is volume supported", TBoolean)

  /** Gets the name of the audio/video player */
  lazy val field_name = ApiField("name", TString)

  /** Gets the status of the player */
  lazy val field_status = ApiField("status", TString)

  /** Gets the current volume */
  lazy val field_volume = ApiField("volume", TNumber)

  override def possibleFields = super.possibleFields ++ List(field_active_media, field_device,
    field_is_control_supported, field_is_paused, field_is_playing, field_is_stopped, field_is_volume_supported,
    field_name, field_status, field_volume)


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
      Top[S](TNumber)

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
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](volume,0,1,"set volume","volume level")
      }
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
      
