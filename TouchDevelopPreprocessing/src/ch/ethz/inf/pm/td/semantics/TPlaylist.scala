
package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Playlist
 *
 * A song playlist
 *
 * @author Lucas Brutschy
 */ 

object TPlaylist {

  /** Gets the duration in seconds */
  val field_duration = new TouchField("duration",TNumber.typName)

  /** Gets the name of the song */
  val field_name = new TouchField("name",TString.typName)

  /** Gets the songs */
  val field_songs = new TouchField("songs",TSongs.typName)

  val typName = "Playlist"
  val typ = new TouchType(typName,isSingleton = false, fields = List(field_duration,field_name,field_songs))

}

class TPlaylist extends AAny {

  def getTyp = TPlaylist.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Plays the songs in the playlist */
    case "play" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
