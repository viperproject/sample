
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Place Collection
 *
 * A collection of places
 *
 * @author Lucas Brutschy
 */ 

object TPlace_Collection {

  val typName = "Place_Collection"
  val typ = TouchCollection(typName,"Number","Place")

}

class TPlace_Collection extends AMutable_Collection {

  def getTyp = TPlace_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Sorts the places by distance to the location */
    case "sort_by_distance" =>
      val List(loc) = parameters // Location
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
      
