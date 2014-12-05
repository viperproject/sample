
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TSong
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Song
 *
 * A song
 *
 * @author Lucas Brutschy
 */ 

object TSong extends Default_TSong {

  /** Gets the song album containing the song */
  lazy val field_album = ApiField("album", TSong_Album)

  /** Gets the name of the artist */
  lazy val field_artist = ApiField("artist", TString)

  /** Gets the duration in seconds */
  lazy val field_duration = ApiField("duration", TNumber)

  /** Gets the genre of the song */
  lazy val field_genre = ApiField("genre", TString)

  /** Gets the name of the song */
  lazy val field_name = ApiField("name", TString)

  /** Gets the play count */
  lazy val field_play_count = ApiField("play count", TNumber)

  /** Gets a value whether the song is DRM protected */
  lazy val field_protected = ApiField("protected", TBoolean)

  /** Gets the users rating. -1 if not rated. */
  lazy val field_rating = ApiField("rating", TNumber)

  /** Gets the track number in the album */
  lazy val field_track = ApiField("track", TNumber)

  override def possibleFields = super.possibleFields ++ List(field_album, field_artist, field_duration, field_genre,
    field_name, field_play_count, field_protected, field_rating, field_track)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the song. */
    case "play" =>
       AssignField[S](Singleton(SPlayer),SPlayer.field_active_song,this0)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
