
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Playlist
 *
 * A song playlist
 *
 * @author Lucas Brutschy
 */ 

object TPlaylist extends AAny {

  /** Gets the duration in seconds */
  lazy val field_duration = new TouchField("duration",TNumber.typeName)

  /** Gets the name of the song */
  lazy val field_name = new TouchField("name",TString.typeName)

  /** Gets the songs */
  lazy val field_songs = new TouchField("songs",TSongs.typeName)

  lazy val typeName = TypeName("Playlist")

  override def possibleFields = super.possibleFields ++ List(field_duration,field_name,field_songs)


  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the songs in the playlist */
    case "play" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
