package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.{TouchCollection, TouchType}
import RichNativeSemantics._
import ch.ethz.inf.pm.sample.Reporter

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
      super.forwardSemantics(this0, "sort", parameters, returnedType)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}
