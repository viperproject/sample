
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Song
 *
 * A song
 *
 * @author Lucas Brutschy
 */ 

object TSong {

  /** Gets the song album containing the song */
  val field_album = new TouchField("album",TSong_Album.typ)

  /** Gets the name of the artist */
  val field_artist = new TouchField("artist",TString.typ)

  /** Gets the duration in seconds */
  val field_duration = new TouchField("duration",TNumber.typ)

  /** Gets the genre of the song */
  val field_genre = new TouchField("genre",TString.typ)

  /** Gets the name of the song */
  val field_name = new TouchField("name",TString.typ)

  /** Gets the play count */
  val field_play_count = new TouchField("play count",TNumber.typ)

  /** Gets a value whether the song is DRM protected */
  val field_protected = new TouchField("protected",TBoolean.typ)

  /** Gets the users rating. -1 if not rated. */
  val field_rating = new TouchField("rating",TNumber.typ)

  /** Gets the track number in the album */
  val field_track = new TouchField("track",TNumber.typ)

  val typName = "Song"
  val typ = new TouchType(typName,isSingleton = false, isImmutable = true, fields = List(field_album, field_artist, field_duration, field_genre, field_name, field_play_count, field_protected, field_rating, field_track))

}

class TSong extends AAny {

  def getTyp = TSong.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the song. */
    case "play" =>
       AssignField[S](Singleton(SPlayer.typ),SPlayer.field_active_song,this0)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
