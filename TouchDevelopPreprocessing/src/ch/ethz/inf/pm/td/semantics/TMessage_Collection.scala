
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Message Collection
 *
 * A list of messages
 *
 * @author Lucas Brutschy
 */ 

object TMessage_Collection {

  val typName = "Message_Collection"
  val typ = TouchCollection(typName,"Number","Message")

}

class TMessage_Collection extends AMutable_Collection {

  def getTyp = TMessage_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Sorts from the newest to oldest */
    case "sort_by_date" =>
      Skip; // Sorting is invariant for (size,elem) abstraction

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
