
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_SPlayer
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of player
 *
 * Play, stop or resume songs, ...
 *
 * @author Lucas Brutschy
 */

object SPlayer extends Default_SPlayer {

  /** Gets the active song if any */
  lazy val field_active_song = ApiField("active song", TSong, TopWithInvalidInitializer("player may not have an active song"))

  /** Indicates if the player is muted */
  lazy val field_is_muted = ApiField("is muted", TBoolean, TopInitializer)

  /** Indicates if the player is paused */
  lazy val field_is_paused = ApiField("is paused", TBoolean, TopInitializer)

  /** Indicates if the player is playing a song */
  lazy val field_is_playing = ApiField("is playing", TBoolean, TopInitializer)

  /** Indicates if the player is repeating */
  lazy val field_is_repeating = ApiField("is repeating", TBoolean, TopInitializer)

  /** Indicates if the player is shuffled */
  lazy val field_is_shuffled = ApiField("is shuffled", TBoolean, TopInitializer)

  /** Indicates if the player is stopped */
  lazy val field_is_stopped = ApiField("is stopped", TBoolean, TopInitializer)

  /** Gets the position in seconds whithin the active song */
  lazy val field_play_position = ApiField("play position", TNumber, TopInitializer)

  /** Gets the sound volume for sounds from 0 (silent) to 1 (current volume) */
  lazy val field_sound_volume = ApiField("sound volume", TNumber, TopInitializer)

  /** Volume is no longer supported. */
  lazy val field_volume = ApiField("volume", TNumber, TopInitializer)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_active_song_changed_handler = ApiField("active song changed", TAction, TopInitializer)
  lazy val field_player_state_changed_handler = ApiField("player state changed", TAction, TopInitializer)

  override def possibleFields = super.possibleFields ++ List(field_active_song, field_is_muted, field_is_paused,
    field_is_playing, field_is_repeating, field_is_shuffled, field_is_stopped, field_play_position, field_sound_volume,
    field_volume, field_active_song_changed_handler, field_player_state_changed_handler
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Moves to the next song in the queue of playing songs */
    case "next" =>
      AssignField[S](this0, SPlayer.field_play_position, Field[S](this0, SPlayer.field_play_position) + 1)

    /** Pauses the currently playing song */
    case "pause" =>
      if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations)
        Error[S](Field[S](this0, SPlayer.field_is_playing) equal False, "pause", "Player might not be playing")
      var curState = state
      curState = AssignField[S](this0, SPlayer.field_is_playing, False)(curState, pp)
      curState = AssignField[S](this0, SPlayer.field_is_paused, True)(curState, pp)
      curState = AssignField[S](this0, SPlayer.field_is_stopped, False)(curState, pp)
      curState

    /** Plays a Song */
    case "play" =>
      val List(song) = parameters // Song
      AssignField[S](this0, SPlayer.field_active_song, song)

    /** Attaches a handler when the active song changes */
    case "on active song changed" =>
      val List(changed) = parameters // Action
    val newState = AssignField[S](this0, SPlayer.field_active_song_changed_handler, changed)
      New[S](TEvent_Binding)(newState, pp)

    /** Attaches a handler when the player state changes */
    case "on player state changed" =>
      val List(changed) = parameters // Action
    val newState = AssignField[S](this0, SPlayer.field_player_state_changed_handler, changed)
      New[S](TEvent_Binding)(newState, pp)

    /** Plays an audio/video file from the home network */
    // case "play home media" =>
    //   val List(media) = parameters // Media_Link
    //   Skip;

    /** Plays a collection of songs */
    case "play many" =>
      val List(songs) = parameters // Songs
      AssignField[S](this0, SPlayer.field_active_song, TSongs.AllValues[S](songs))

    /** Moves to the previous song in the queue of playing songs */
    case "previous" =>
      AssignField[S](this0, SPlayer.field_play_position, Field[S](this0, SPlayer.field_play_position) - 1)

    /** Resumes a paused song */
    case "resume" =>
      if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations)
        Error[S](Field[S](this0, SPlayer.field_is_paused) equal False, "resume", "Player might not be paused")
      var curState = state
      curState = AssignField[S](this0, SPlayer.field_is_playing, True)(curState, pp)
      curState = AssignField[S](this0, SPlayer.field_is_paused, False)(curState, pp)
      curState = AssignField[S](this0, SPlayer.field_is_stopped, False)(curState, pp)
      curState

    /** Sets the repeating on and off */
    case "set repeating" =>
      val List(repeating) = parameters // Boolean
      AssignField[S](this0, SPlayer.field_is_repeating, repeating)

    /** Sets the shuffling on and off */
    case "set shuffled" =>
      val List(shuffled) = parameters // Boolean
      AssignField[S](this0, SPlayer.field_is_shuffled, shuffled)

    /** Sets the sound volume level from 0 (silent) to 1 (current volume) */
    case "set sound volume" =>
      val List(x) = parameters // Number
      if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](x, 0, 1, "set sound volume", "volume level")
      }
      super.forwardSemantics(this0, method, parameters, returnedType)

    /** Stops playing a song */
    case "stop" =>
      if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations)
        Error[S](Field[S](this0, SPlayer.field_is_playing) equal False, "play", "Player might not be playing")
      var curState = state
      curState = AssignField[S](this0, SPlayer.field_is_playing, False)(curState, pp)
      curState = AssignField[S](this0, SPlayer.field_is_paused, False)(curState, pp)
      curState = AssignField[S](this0, SPlayer.field_is_stopped, True)(curState, pp)
      curState

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
