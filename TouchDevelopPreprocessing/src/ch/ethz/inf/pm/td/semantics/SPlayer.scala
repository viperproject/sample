
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

  val typName = "player"
  val typ = new TouchType(typName,isSingleton = true,List())

}

class SPlayer extends AAny {

  def getTyp = SPlayer.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Gets the active song if any */
    // case "active_song" => 
    //   Return[S](Valid(TSong.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the active song if any */
    //   val field_active_song = new TouchField("active_song",TSong.typ)

    /** Indicates if the player is muted */
    // case "is_muted" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the player is muted */
    //   val field_is_muted = new TouchField("is_muted",TBoolean.typ)

    /** Indicates if the player is paused */
    // case "is_paused" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the player is paused */
    //   val field_is_paused = new TouchField("is_paused",TBoolean.typ)

    /** Indicates if the player is playing a song */
    // case "is_playing" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the player is playing a song */
    //   val field_is_playing = new TouchField("is_playing",TBoolean.typ)

    /** Indicates if the player is repeating */
    // case "is_repeating" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the player is repeating */
    //   val field_is_repeating = new TouchField("is_repeating",TBoolean.typ)

    /** Indicates if the player is shuffled */
    // case "is_shuffled" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the player is shuffled */
    //   val field_is_shuffled = new TouchField("is_shuffled",TBoolean.typ)

    /** Indicates if the player is stopped */
    // case "is_stopped" => 
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD: 
    //   /** Indicates if the player is stopped */
    //   val field_is_stopped = new TouchField("is_stopped",TBoolean.typ)

    /** Moves to the next song in the queue of playing songs */
    // case "next" => 
    //   Skip;

    /** Pauses the currently playing song */
    // case "pause" => 
    //   Skip;

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

    /** Gets the position in seconds whithin the active song */
    // case "play_position" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the position in seconds whithin the active song */
    //   val field_play_position = new TouchField("play_position",TNumber.typ)

    /** Moves to the previous song in the queue of playing songs */
    // case "previous" => 
    //   Skip;

    /** Resumes a paused song */
    // case "resume" => 
    //   Skip;

    /** Sets the repeating on and off */
    // case "set_repeating" => 
    //   val List(repeating) = parameters // Boolean
    //   Skip;

    /** Sets the shuffling on and off */
    // case "set_shuffled" => 
    //   val List(shuffled) = parameters // Boolean
    //   Skip;

    /** Sets the sound volume level from 0 (silent) to 1 (current volume) */
    // case "set_sound_volume" => 
    //   val List(x) = parameters // Number
    //   Skip;

    /** Gets the sound volume for sounds from 0 (silent) to 1 (current volume) */
    // case "sound_volume" => 
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD: 
    //   /** Gets the sound volume for sounds from 0 (silent) to 1 (current volume) */
    //   val field_sound_volume = new TouchField("sound_volume",TNumber.typ)

    /** Stops playing a song */
    // case "stop" => 
    //   Skip;

    // FIELDS: , field_active_song, field_is_muted, field_is_paused, field_is_playing, field_is_repeating, field_is_shuffled, field_is_stopped, field_play_position, field_sound_volume

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
