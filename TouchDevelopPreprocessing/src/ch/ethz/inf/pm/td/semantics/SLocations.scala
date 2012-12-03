package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * User: lucas
 * Date: 11/29/12
 * Time: 12:44 PM
 */
class SLocations extends Any {

  def getTypeName = "locations"

  def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                     (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates a new geo coordinate location */
    case "create_location" =>
      val List(latitude,longitude) = parameters // Number,Number
      New[S](TLocation.typ,latitude,longitude)

    /** Creates an empty list of locations */
    case "create_location_list" =>
      New[S](TLocation_Collection.typ)

    /** Looks for an address near a location using Bing. */
    case "describe_location" =>
      val List(location) = parameters // Location
      Return[S](Valid(TString.typ))

    /** Looks for the coordinate of an address using Bing. */
    case "search_location" =>
      val List(address,postal_code,city,country) = parameters // String,String,String,String
      New[S](TLocation.typ)

  }
}