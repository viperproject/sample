
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Song
 *
 * A song
 *
 * @author Lucas Brutschy
 */ 

object TSong extends AAny {

  /** Gets the song album containing the song */
  lazy val field_album = new ApiField("album",TSong_Album.typeName)

  /** Gets the name of the artist */
  lazy val field_artist = new ApiField("artist",TString.typeName)

  /** Gets the duration in seconds */
  lazy val field_duration = new ApiField("duration",TNumber.typeName)

  /** Gets the genre of the song */
  lazy val field_genre = new ApiField("genre",TString.typeName)

  /** Gets the name of the song */
  lazy val field_name = new ApiField("name",TString.typeName)

  /** Gets the play count */
  lazy val field_play_count = new ApiField("play count",TNumber.typeName)

  /** Gets a value whether the song is DRM protected */
  lazy val field_protected = new ApiField("protected",TBoolean.typeName)

  /** Gets the users rating. -1 if not rated. */
  lazy val field_rating = new ApiField("rating",TNumber.typeName)

  /** Gets the track number in the album */
  lazy val field_track = new ApiField("track",TNumber.typeName)

  lazy val typeName = TypeName("Song")

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
      
