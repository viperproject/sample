package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * User: lucas
 * Date: 12/3/12
 * Time: 3:20 PM
 */
class SCollections extends Any {

  def getTypeName = "collections"

  def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates an empty link collection */
    //case "create_link_collection" =>
    //  New[S](TLink_Collection.typ) // TODO

    /** Creates an empty location collection */
    case "create_location_collection" =>
      New[S](TLocation_Collection.typ) // TODO

    /** Creates an empty message collection */
    //case "create_message_collection" =>
    //  New[S](TMessage_Collection.typ) // TODO

    /** Creates an empty number collection */
    //case "create_number_collection" =>
    //  New[S](TNumber_Collection.typ) // TODO

    /** Creates an empty number map */
    //case "create_number_map" =>
    //  New[S](TNumber_Map.typ) // TODO

    /** Creates an empty place collection */
    //case "create_place_collection" =>
    //  New[S](TPlace_Collection.typ) // TODO

    /** Creates an empty string collection */
    case "create_string_collection" =>
      New[S](TString_Collection.typ) // TODO

    /** Creates an empty string map (case and culture sensitive) */
    //case "create_string_map" =>
    //  New[S](TString_Map.typ) // TODO

    case _ =>
      Unimplemented[S](this0.getType().toString+"."+method)


  }
}
