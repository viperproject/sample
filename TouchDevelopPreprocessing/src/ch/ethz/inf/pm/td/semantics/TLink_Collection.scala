
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Link Collection
 *
 * A list of links
 *
 * @author Lucas Brutschy
 */ 

object TLink_Collection {

  val typName = "Link Collection"
  val typ = TouchCollection(typName,TNumber.typName,TLink.typName)

}

class TLink_Collection extends AMutable_Collection {

  def getTyp = TLink_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
