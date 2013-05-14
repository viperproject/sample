
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.sample.Reporter

/**
 * Specifies the abstract semantics of Message Collection
 *
 * A list of messages
 *
 * @author Lucas Brutschy
 */ 

object TMessage_Collection {

  val typName = "Message Collection"
  val typ = TouchCollection(typName,TNumber.typName,TMessage.typName,immutableCollection = true)

}

class TMessage_Collection extends AMutable_Collection {

  def getTyp = TMessage_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Sorts from the newest to oldest */
    case "sort by date" =>
      super.forwardSemantics[S](this0, "sort", parameters, returnedType)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
