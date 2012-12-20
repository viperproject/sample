package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.compiler.TouchType

/**
 * Specifies the abstract semantics of collections
 *
 * Create collections of items.
 *
 * @author Lucas Brutschy
 */

object SCollections {

  val typName = "collections"
  val typ = TouchType(typName, isSingleton = true)

}

class SCollections extends AAny {

  def getTyp = SCollections.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates an empty link collection */
    case "create_link_collection" =>
      New[S](TLink_Collection.typ)

    /** Creates an empty location collection */
    case "create_location_collection" =>
      New[S](TLocation_Collection.typ)

    /** Creates an empty message collection */
    case "create_message_collection" =>
      New[S](TMessage_Collection.typ)

    /** Creates an empty number collection */
    case "create_number_collection" =>
      New[S](TNumber_Collection.typ)

    /** Creates an empty number map */
    case "create_number_map" =>
      New[S](TNumber_Map.typ)

    /** Creates an empty place collection */
    case "create_place_collection" =>
      New[S](TPlace_Collection.typ)

    /** Creates an empty string collection */
    case "create_string_collection" =>
      New[S](TString_Collection.typ)

    /** Creates an empty string map (case and culture sensitive) */
    case "create_string_map" =>
      New[S](TString_Map.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters)


  }
}
