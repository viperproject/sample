
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Song Album
 *
 * A song album
 *
 * @author Lucas Brutschy
 */ 

object TSong_Album extends AAny {

/** Gets album art picture */
  lazy val field_art = new TouchField("art",TPicture.typeName)

  /** Gets the name of the artist */
  lazy val field_artist = new TouchField("artist",TString.typeName)

  /** Gets the duration in seconds */
  lazy val field_duration = new TouchField("duration",TNumber.typeName)

  /** Gets the genre of the song */
  lazy val field_genre = new TouchField("genre",TString.typeName)

  /** Indicates if the album has art */
  lazy val field_has_art = new TouchField("has art",TBoolean.typeName)

  /** Gets the name of the album */
  lazy val field_name = new TouchField("name",TString.typeName)

  /** Gets the songs */
  lazy val field_songs = new TouchField("songs",TSongs.typeName)

  /** Gets the thumbnail picture */
  lazy val field_thumbnail = new TouchField("thumbnail",TPicture.typeName)

  lazy val typeName = TypeName("Song Album")

  override def possibleFields = super.possibleFields ++ List(field_art, field_artist, field_duration,
    field_genre, field_has_art, field_name, field_songs, field_thumbnail)

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the songs of the album */
    case "play" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
