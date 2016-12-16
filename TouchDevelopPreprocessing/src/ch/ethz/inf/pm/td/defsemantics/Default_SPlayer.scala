/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiParam, DefaultSemantics}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Player
 *
 * Play, stop or resume songs, ...
 *
 * @author Lucas Brutschy
 */

trait Default_SPlayer extends ASingleton {

  lazy val typeName = TypeName("Player", isSingleton = true)

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "active song" -> member_active_song,
    "is muted" -> member_is_muted,
    "is paused" -> member_is_paused,
    "is playing" -> member_is_playing,
    "is repeating" -> member_is_repeating,
    "is shuffled" -> member_is_shuffled,
    "is stopped" -> member_is_stopped,
    "next" -> member_next,
    "on active song changed" -> member_on_active_song_changed,
    "on player state changed" -> member_on_player_state_changed,
    "pause" -> member_pause,
    "play home media" -> member_play_home_media,
    "play many" -> member_play_many,
    "play position" -> member_play_position,
    "play" -> member_play,
    "previous" -> member_previous,
    "resume" -> member_resume,
    "set repeating" -> member_set_repeating,
    "set shuffled" -> member_set_shuffled,
    "set sound volume" -> member_set_sound_volume,
    "sound volume" -> member_sound_volume,
    "stop" -> member_stop,
    "volume" -> member_volume
  )

  /** Frequently used: Gets the active song if any */
  def member_active_song = ApiMember(
    name = "active song",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSong,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates if the player is muted */
  def member_is_muted = ApiMember(
    name = "is muted",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates if the player is paused */
  def member_is_paused = ApiMember(
    name = "is paused",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates if the player is playing a song */
  def member_is_playing = ApiMember(
    name = "is playing",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Rarely used: Indicates if the player is repeating */
  def member_is_repeating = ApiMember(
    name = "is repeating",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates if the player is shuffled */
  def member_is_shuffled = ApiMember(
    name = "is shuffled",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates if the player is stopped */
  def member_is_stopped = ApiMember(
    name = "is stopped",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Moves to the next song in the queue of playing songs */
  def member_next = ApiMember(
    name = "next",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches a handler when the active song changes */
  def member_on_active_song_changed = ApiMember(
    name = "on active song changed",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches a handler when the player state changes */
  def member_on_player_state_changed = ApiMember(
    name = "on player state changed",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Pauses the currently playing song */
  def member_pause = ApiMember(
    name = "pause",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Plays an audio/video file from the home network */
  def member_play_home_media = ApiMember(
    name = "play home media",
    paramTypes = List(ApiParam(TMedia_Link)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Plays a collection of songs */
  def member_play_many = ApiMember(
    name = "play many",
    paramTypes = List(ApiParam(TSongs)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the position in seconds whithin the active song */
  def member_play_position = ApiMember(
    name = "play position",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Plays a Song */
  def member_play = ApiMember(
    name = "play",
    paramTypes = List(ApiParam(TSong,isMutated=true)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Moves to the previous song in the queue of playing songs */
  def member_previous = ApiMember(
    name = "previous",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Resumes a paused song */
  def member_resume = ApiMember(
    name = "resume",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the repeating on and off */
  def member_set_repeating = ApiMember(
    name = "set repeating",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the shuffling on and off */
  def member_set_shuffled = ApiMember(
    name = "set shuffled",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] Sets the sound volume level from 0 (silent) to 1 (current volume) */
  def member_set_sound_volume = ApiMember(
    name = "set sound volume",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] Gets the sound volume for sounds from 0 (silent) to 1 (current volume) */
  def member_sound_volume = ApiMember(
    name = "sound volume",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Stops playing a song */
  def member_stop = ApiMember(
    name = "stop",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Volume is no longer supported. */
  def member_volume = ApiMember(
    name = "volume",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )
            

}
          
