
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
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
  val field_art = new TouchField("art",TPicture.typ)

  /** Gets the name of the artist */
  val field_artist = new TouchField("artist",TString.typ)

  /** Gets the duration in seconds */
  val field_duration = new TouchField("duration",TNumber.typ)

  /** Gets the genre of the song */
  val field_genre = new TouchField("genre",TString.typ)

  /** Indicates if the album has art */
  val field_has_art = new TouchField("has_art",TBoolean.typ)

  /** Gets the name of the album */
  val field_name = new TouchField("name",TString.typ)

  /** Gets the songs. TODO */
  /*val field_songs:TouchField = new TouchField("songs",TSongs.typ)*/

  /** Gets the thumbnail picture */
  val field_thumbnail = new TouchField("thumbnail",TPicture.typ)

  val typName = "Song_Album"
  val typ = new TouchType(typName,isSingleton = false,List(field_art, field_artist, field_duration, field_genre, field_has_art, field_name, /*field_songs,*/ field_thumbnail))

}

class TSong_Album extends AAny {

  def getTyp = TSong_Album.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the songs of the album */
    case "play" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
