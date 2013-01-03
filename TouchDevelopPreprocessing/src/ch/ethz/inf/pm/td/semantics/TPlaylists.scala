
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Playlists
 *
 * A collection of playlists
 *
 * @author Lucas Brutschy
 */ 

object TPlaylists {

  val typName = "Playlists"
  val typ = TouchCollection(typName,TNumber.typ,TPlaylist.typ)

}

class TPlaylists extends ACollection {

  def getTyp = TPlaylists.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
