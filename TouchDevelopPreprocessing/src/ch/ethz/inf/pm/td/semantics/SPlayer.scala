
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._

/**
 * Specifies the abstract semantics of player
 *
 * Play, stop or resume songs, ...
 *
 * @author Lucas Brutschy
 */

object SPlayer {


  /** Gets the active song if any */
  val field_active_song = new TouchField("active song", TSong.typName, topDefault = TopWithInvalidInitializer("player may not have an active song"))

  /** Indicates if the player is muted */
  val field_is_muted = new TouchField("is muted", TBoolean.typName)

  /** Indicates if the player is paused */
  val field_is_paused = new TouchField("is paused", TBoolean.typName)

  /** Indicates if the player is playing a song */
  val field_is_playing = new TouchField("is playing", TBoolean.typName)

  /** Indicates if the player is repeating */
  val field_is_repeating = new TouchField("is repeating", TBoolean.typName)

  /** Indicates if the player is shuffled */
  val field_is_shuffled = new TouchField("is shuffled", TBoolean.typName)

  /** Indicates if the player is stopped */
  val field_is_stopped = new TouchField("is stopped", TBoolean.typName)

  /** Gets the position in seconds whithin the active song */
  val field_play_position = new TouchField("play position", TNumber.typName)

  /** Gets the sound volume for sounds from 0 (silent) to 1 (current volume) */
  val field_sound_volume = new TouchField("sound volume", TNumber.typName)

  /** Volume is no longer supported. */
  val field_volume = new TouchField("volume", TNumber.typName)

  /** PRIVATE HANDLER FIELDS */
  val field_active_song_changed_handler = new TouchField("active song changed", TAction.typName)
  val field_player_state_changed_handler = new TouchField("player state changed", TAction.typName)

  val typName = "Player"
  val typ = DefaultTouchType(typName, isSingleton = true, fields = List(field_active_song, field_is_muted, field_is_paused,
    field_is_playing, field_is_repeating, field_is_shuffled, field_is_stopped, field_play_position, field_sound_volume,
    field_volume, field_active_song_changed_handler, field_player_state_changed_handler
  ))
}

class SPlayer extends AAny {

  def getTyp = SPlayer.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Moves to the next song in the queue of playing songs */
    case "next" =>
      AssignField[S](this0, SPlayer.field_play_position, Field[S](this0, SPlayer.field_play_position) + 1)

    /** Pauses the currently playing song */
    case "pause" =>
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations)
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
      New[S](TEvent_Binding.typ)(newState, pp)

    /** Attaches a handler when the player state changes */
    case "on player state changed" =>
      val List(changed) = parameters // Action
    val newState = AssignField[S](this0, SPlayer.field_player_state_changed_handler, changed)
      New[S](TEvent_Binding.typ)(newState, pp)

    /** Plays an audio/video file from the home network */
    // case "play home media" =>
    //   val List(media) = parameters // Media_Link
    //   Skip;

    /** Plays a collection of songs */
    case "play many" =>
      val List(songs) = parameters // Songs
      AssignField[S](this0, SPlayer.field_active_song, CollectionSummary[S](songs))

    /** Moves to the previous song in the queue of playing songs */
    case "previous" =>
      AssignField[S](this0, SPlayer.field_play_position, Field[S](this0, SPlayer.field_play_position) - 1)

    /** Resumes a paused song */
    case "resume" =>
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations)
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
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](x, 0, 1, "set sound volume", "volume level")
      }
      super.forwardSemantics(this0, method, parameters, returnedType)

    /** Stops playing a song */
    case "stop" =>
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations)
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
      
