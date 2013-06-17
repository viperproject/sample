package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import RichNativeSemantics._

/**
 * User: lucas
 * Date: 11/26/12
 * Time: 1:16 PM
 */
object TLocation_Collection {

  val typName = "Location Collection"
  val typ = TouchCollection(typName,"Number","Location")

}

class TLocation_Collection extends AMutable_Collection {

  def getTyp = TLocation_Collection.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Sorts by distance to the location */
    case "sort by distance" =>
      val List(loc) = parameters // Location
      Dummy[S](this0,method)
      Skip // Sorting is invariant for (size,elem) abstraction

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
