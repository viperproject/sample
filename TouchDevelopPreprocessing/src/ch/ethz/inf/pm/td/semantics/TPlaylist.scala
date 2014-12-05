
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TPlaylist
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Playlist
 *
 * A song playlist
 *
 * @author Lucas Brutschy
 */ 

object TPlaylist extends Default_TPlaylist {

  /** Gets the duration in seconds */
  lazy val field_duration = ApiField("duration", TNumber)

  /** Gets the name of the song */
  lazy val field_name = ApiField("name", TString)

  /** Gets the songs */
  lazy val field_songs = ApiField("songs", TSongs)

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
      
