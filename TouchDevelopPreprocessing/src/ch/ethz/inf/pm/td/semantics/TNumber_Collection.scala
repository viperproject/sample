
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Number Collection
 *
 * A collection of numbers
 *
 * @author Lucas Brutschy
 */ 

object TNumber_Collection {

  val typName = "Number Collection"
  val typ = TouchCollection(typName,TNumber.typ,TNumber.typ)

}

class TNumber_Collection extends AMutable_Collection {

  def getTyp = TNumber_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {
        
    /** Computes the average of the values */
    case "avg" =>
      Return[S](CollectionSummary[S](this0))

    /** Computes the maximum of the values */
    case "max" =>
      Return[S](CollectionSummary[S](this0))

    /** Computes the minimum of the values */
    case "min" =>
      Return[S](CollectionSummary[S](this0))

    /** Computes the sum of the values */
    case "sum" =>
      Return[S](CollectionSize[S](this0)*CollectionSummary[S](this0))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
