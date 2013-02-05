
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Media Player Collection
 *
 * A collection of media players
 *
 * @author Lucas Brutschy
 */ 

object TMedia_Player_Collection {

  val typName = "Media_Player_Collection"
  val typ = TouchCollection(typName,TNumber.typ,TMedia_Player.typ)

}

class TMedia_Player_Collection extends ACollection {

  def getTyp = TMedia_Player_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
