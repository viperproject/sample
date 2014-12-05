
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Media Player
 *
 * An audio/video player on the home network
 *
 * @author Lucas Brutschy
 */

trait Default_TMedia_Player extends AAny {

  lazy val typeName = TypeName("Media Player")
          
  /** Rarely used: [**not implemented**] Gets the uri of the media currently active */
  def member_active_media = ApiMember(
    name = "active media",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Gets the detailled information about this device */
  def member_device = ApiMember(
    name = "device",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDevice,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Indicates the media can be played, paused, resumed */
  def member_is_control_supported = ApiMember(
    name = "is control supported",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Indicates if the player is paused */
  def member_is_paused = ApiMember(
    name = "is paused",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Indicates if the player is playing */
  def member_is_playing = ApiMember(
    name = "is playing",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Indicates if the player is stopped */
  def member_is_stopped = ApiMember(
    name = "is stopped",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Indicates if volume can be changed */
  def member_is_volume_supported = ApiMember(
    name = "is volume supported",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Gets the name of the audio/video player */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Moves the player to the next media in the queue. */
  def member_next = ApiMember(
    name = "next",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Pauses the current media if any. */
  def member_pause = ApiMember(
    name = "pause",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Plays a media from the home network. */
  def member_play_home_media = ApiMember(
    name = "play home media",
    paramTypes = List(ApiParam(TMedia_Link)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Plays the media at the 'url' internet address. */
  def member_play_media = ApiMember(
    name = "play media",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] Gets the position in seconds whithin the active media */
  def member_play_position = ApiMember(
    name = "play position",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Plays the current media from the start. */
  def member_play = ApiMember(
    name = "play",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Moves the player to the previous media in the queue. */
  def member_previous = ApiMember(
    name = "previous",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Resumes playing the current media if any. */
  def member_resume = ApiMember(
    name = "resume",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the current value */
  def member_set_volume = ApiMember(
    name = "set volume",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Gets the status of the player */
  def member_status = ApiMember(
    name = "status",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Stops the current media if any. */
  def member_stop = ApiMember(
    name = "stop",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the current volume */
  def member_volume = ApiMember(
    name = "volume",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "active media" -> member_active_media,
    "device" -> member_device,
    "is control supported" -> member_is_control_supported,
    "is paused" -> member_is_paused,
    "is playing" -> member_is_playing,
    "is stopped" -> member_is_stopped,
    "is volume supported" -> member_is_volume_supported,
    "name" -> member_name,
    "next" -> member_next,
    "pause" -> member_pause,
    "play home media" -> member_play_home_media,
    "play media" -> member_play_media,
    "play position" -> member_play_position,
    "play" -> member_play,
    "previous" -> member_previous,
    "resume" -> member_resume,
    "set volume" -> member_set_volume,
    "status" -> member_status,
    "stop" -> member_stop,
    "volume" -> member_volume
  )
            

}
          
