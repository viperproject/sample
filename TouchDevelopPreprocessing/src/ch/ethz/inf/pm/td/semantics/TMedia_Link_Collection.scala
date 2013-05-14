
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Media Link Collection
 *
 * A list of media links on the home network
 *
 * @author Lucas Brutschy
 */ 

object TMedia_Link_Collection {

  val typName = "Media Link Collection"
  val typ = TouchCollection(typName,TNumber.typName,TMedia_Link.typName, immutableCollection = true)

}

class TMedia_Link_Collection extends ACollection {

  def getTyp = TMedia_Link_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
