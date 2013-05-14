
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Media Server Collection
 *
 * A collection of media servers
 *
 * @author Lucas Brutschy
 */ 

object TMedia_Server_Collection {

  val typName = "Media Server Collection"
  val typ = TouchCollection(typName,TNumber.typName,TMedia_Server.typName, immutableCollection = true)

}

class TMedia_Server_Collection extends ACollection {

  def getTyp = TMedia_Server_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
