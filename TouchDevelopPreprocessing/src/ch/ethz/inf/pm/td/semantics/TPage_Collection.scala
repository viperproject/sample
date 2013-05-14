
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Page Collection
 *
 * A collection of page
 *
 * @author Lucas Brutschy
 */ 

object TPage_Collection {

  val typName = "Page Collection"
  val typ = TouchCollection(typName,"Number","Page", immutableCollection = true)

}

class TPage_Collection extends ACollection {

  def getTyp = TPage_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
