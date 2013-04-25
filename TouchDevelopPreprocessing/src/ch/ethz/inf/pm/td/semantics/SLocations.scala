package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * User: lucas
 * Date: 11/29/12
 * Time: 12:44 PM
 */

object SLocations {

  val typName = "Locations"
  val typ = new TouchType(typName, isSingleton = true)

}

class SLocations extends AAny {

  def getTyp = SLocations.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a new geo coordinate location */
    case "create location" =>
      val List(latitude,longitude) = parameters // Number,Number
      New[S](TLocation.typ,Map(
        TLocation.field_latitude -> latitude,
        TLocation.field_longitude -> longitude
      ))

    /** Creates an empty list of locations */
    case "create location list" =>
      New[S](TLocation_Collection.typ)

    /** Looks for an address near a location using Bing. */
    case "describe location" =>
      val List(location) = parameters // Location
      val ret = Top[S](TString.typ)
      ret

    /** Looks for the coordinate of an address using Bing. */
    case "search location" =>
      val List(address,postal_code,city,country) = parameters // String,String,String,String
      New[S](TLocation.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}