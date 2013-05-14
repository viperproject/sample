
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.sample.Reporter

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

  val typName = "Number Map"
  val typ = TouchCollection(typName,"Number","Number")

}

class TNumber_Map extends AMutable_Collection {

  def getTyp = TNumber_Map.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
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

    /** Sets the i-th element */
    case "set at" =>
      val List(index,value) = parameters // Number,Element_Type
      Reporter.reportImprecision("This map access is not checked",pp)
      // We have no clue whether this is an update or not without a must analysis
      val case1 = CollectionUpdate[S](this0,index,value)
      val case2 = CollectionInsert[S](this0,index,value)
      state.lub(case1,case2)

    /** Extracts the elements at indices between start (inclusive) and end (non-inclusive). */
    case "slice" =>
      val List(start,end) = parameters // Number,Number
      Clone[S](this0)

    /** Computes the sum of the values */
    case "sum" =>
      Return[S]( (CollectionSize[S](this0)) * (CollectionSummary[S](this0)))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
      
