
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Song Album
 *
 * A song album
 *
 * @author Lucas Brutschy
 */ 

object TSong_Album {

  /** Gets album art picture */
  val field_art = new TouchField("art",TPicture.typName)

  /** Gets the name of the artist */
  val field_artist = new TouchField("artist",TString.typName)

  /** Gets the duration in seconds */
  val field_duration = new TouchField("duration",TNumber.typName)

  /** Gets the genre of the song */
  val field_genre = new TouchField("genre",TString.typName)

  /** Indicates if the album has art */
  val field_has_art = new TouchField("has art",TBoolean.typName)

  /** Gets the name of the album */
  val field_name = new TouchField("name",TString.typName)

  /** Gets the songs */
  val field_songs = new TouchField("songs",TSongs.typName)

  /** Gets the thumbnail picture */
  val field_thumbnail = new TouchField("thumbnail",TPicture.typName)

  val typName = "Song Album"
  val typ = DefaultTouchType(typName,isSingleton = false, fields = List(field_art, field_artist, field_duration,
    field_genre, field_has_art, field_name, field_songs, field_thumbnail))

}

class TSong_Album extends AAny {

  def getTyp = TSong_Album.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the songs of the album */
    case "play" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
