
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of player
 *
 * Play, stop or resume songs, ...
 *
 * @author Lucas Brutschy
 */ 

object SPlayer {


  /** Gets the active song if any */
  val field_active_song = new TouchField("active_song",TSong.typ)

  /** Indicates if the player is muted */
  val field_is_muted = new TouchField("is_muted",TBoolean.typ)

  /** Indicates if the player is paused */
  val field_is_paused = new TouchField("is_paused",TBoolean.typ)

  /** Indicates if the player is playing a song */
  val field_is_playing = new TouchField("is_playing",TBoolean.typ)

  /** Indicates if the player is repeating */
  val field_is_repeating = new TouchField("is_repeating",TBoolean.typ)

  /** Indicates if the player is shuffled */
  val field_is_shuffled = new TouchField("is_shuffled",TBoolean.typ)

  /** Indicates if the player is stopped */
  val field_is_stopped = new TouchField("is_stopped",TBoolean.typ)

  /** Gets the position in seconds whithin the active song */
  val field_play_position = new TouchField("play_position",TNumber.typ)

  /** Gets the sound volume for sounds from 0 (silent) to 1 (current volume) */
  val field_sound_volume = new TouchField("sound_volume",TNumber.typ)

  val typName = "player"
  val typ = new TouchType(typName,isSingleton = true,List(field_active_song, field_is_muted, field_is_paused, field_is_playing, field_is_repeating, field_is_shuffled, field_is_stopped, field_play_position, field_sound_volume))

}

class SPlayer extends AAny {

  def getTyp = SPlayer.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Moves to the next song in the queue of playing songs */
    case "next" =>
      AssignField[S](this0,SPlayer.field_play_position,Field[S](this0,SPlayer.field_play_position) + 1)

    /** Pauses the currently playing song */
    case "pause" =>
      Error[S](Field[S](this0,SPlayer.field_is_playing) equal False,"pause","Player might not be playing")
      var curState = state
      curState = AssignField[S](this0,SPlayer.field_is_playing,False)(curState,pp)
      curState = AssignField[S](this0,SPlayer.field_is_paused,True)(curState,pp)
      curState = AssignField[S](this0,SPlayer.field_is_stopped,False)(curState,pp)
      curState

    /** Plays a Song */
    // case "play" =>
    //   val List(song) = parameters // Song
    //   Skip;

    /** Plays an audio/video file from the home network */
    // case "play_home_media" => 
    //   val List(media) = parameters // Media_Link
    //   Skip;

    /** Plays a collection of songs */
    // case "play_many" =>
    //   val List(songs) = parameters // Songs
    //   Skip;

    /** Moves to the previous song in the queue of playing songs */
    case "previous" =>
      AssignField[S](this0,SPlayer.field_play_position,Field[S](this0,SPlayer.field_play_position) - 1)

    /** Resumes a paused song */
    case "resume" =>
      Error[S](Field[S](this0,SPlayer.field_is_paused) equal False,"resume","Player might not be paused")
      var curState = state
      curState = AssignField[S](this0,SPlayer.field_is_playing,True)(curState,pp)
      curState = AssignField[S](this0,SPlayer.field_is_paused,False)(curState,pp)
      curState = AssignField[S](this0,SPlayer.field_is_stopped,False)(curState,pp)
      curState

    /** Sets the repeating on and off */
    case "set_repeating" =>
      val List(repeating) = parameters // Boolean
      AssignField[S](this0,SPlayer.field_is_repeating,repeating)

    /** Sets the shuffling on and off */
    case "set_shuffled" =>
      val List(shuffled) = parameters // Boolean
      AssignField[S](this0,SPlayer.field_is_shuffled,shuffled)

    /** Sets the sound volume level from 0 (silent) to 1 (current volume) */
    case "set_sound_volume" =>
      val List(x) = parameters // Number
      CheckInRangeInclusive[S](x,0,1,"set_sound_volume","volume level")
      super.forwardSemantics(this0,method,parameters)

    /** Stops playing a song */
    case "stop" =>
      Error[S](Field[S](this0,SPlayer.field_is_playing) equal False,"play","Player might not be playing")
      var curState = state
      curState = AssignField[S](this0,SPlayer.field_is_playing,False)(curState,pp)
      curState = AssignField[S](this0,SPlayer.field_is_paused,False)(curState,pp)
      curState = AssignField[S](this0,SPlayer.field_is_stopped,True)(curState,pp)
      curState

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
