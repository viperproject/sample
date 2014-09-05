
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Message Collection
 *
 * A list of messages
 *
 * @author Lucas Brutschy
 */ 

object TMessage_Collection extends AMutable_Collection {

  lazy val typeName = TypeName("Message Collection")
  def keyTypeName = TNumber.typeName

  def valueTypeName = TMessage.typeName



  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Sorts from the newest to oldest */
    case "sort by date" =>
      Dummy[S](this0,method)
      Skip; // Sorting is invariant for (size,elem) abstraction

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
