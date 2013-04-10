
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Song Albums
 *
 * A collection of albums
 *
 * @author Lucas Brutschy
 */ 

object TSong_Albums {

  val typName = "Song_Albums"
  val typ = TouchCollection(typName,"Number","Song_Album", immutableCollection = true)

}

class TSong_Albums extends ACollection {

  def getTyp = TSong_Albums.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
