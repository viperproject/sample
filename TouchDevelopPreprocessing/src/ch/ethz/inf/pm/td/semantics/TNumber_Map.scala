
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Number Map
 *
 * A map of numbers to numbers
 *
 * TODO: These implementations are only valid for (size,elem) abstractions
 *
 * @author Lucas Brutschy
 */ 

object TNumber_Map {

  val typName = "Number_Map"
  val typ = TouchCollection(typName,"Number","Number")

}

class TNumber_Map extends AMutable_Collection {

  def getTyp = TNumber_Map.typ

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

    /** Extracts the elements at indices between start (inclusive) and end (non-inclusive). */
    case "slice" =>
      val List(start,end) = parameters // Number,Number
      // TODO: Copy of collection
      Return[S](Valid(TNumber_Map.typ))

    /** Computes the sum of the values */
    case "sum" =>
      Return[S]( (CollectionSize[S](this0)) * (CollectionSummary[S](this0)))

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
