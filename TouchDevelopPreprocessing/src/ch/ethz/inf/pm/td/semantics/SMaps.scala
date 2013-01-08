package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{State, ExpressionSet}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * User: lucas
 * Date: 11/26/12
 * Time: 1:09 PM
 */

object SMaps {

  val typName = "maps"
  val typ = TouchType(typName, isSingleton = true)

}

class SMaps extends AAny {

  def getTyp = SMaps.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a full screen Bing map. Use 'post to wall' to display it. */
    case "create_full_map" =>
      New[S](TMap.typ) // PRECISION: We might want to set zoom/center here

    /** Creates a Bing map. Use 'post to wall' to display it. */
    case "create_map" =>
      New[S](TMap.typ) // PRECISION: We might want to set zoom/center here

    /** Calculates the directions between two coordinates using Bing. */
    case "directions" =>
      val List(from,to,walking) = parameters // Location,Location,Boolean
      New[S](TLocation_Collection.typ) // TODO

    /** Shows the directions in the Bing map application. If search term is provided, location is ignored.
        Provide search term or location for start and end. */
    case "open_directions" =>
      val List(start_search,start_loc,end_search,end_loc) = parameters // String,Location,String,Location
      Skip; // TODO

    /** Opens the Bing map application. zoom between 0 (close) and 1 (far). */
    case "open_map" =>
      val List(center,search,zoom) = parameters // Location,String,Number
      CheckInRangeInclusive[S](zoom,0,1,method,"zoom")
      Skip; // TODO

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
