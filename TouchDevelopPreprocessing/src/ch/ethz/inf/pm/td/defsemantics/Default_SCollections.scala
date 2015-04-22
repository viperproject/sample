
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Collections
 *
 * Create collections of items.
 *
 * @author Lucas Brutschy
 */

trait Default_SCollections extends ASingleton {

  lazy val typeName = TypeName("Collections", isSingleton = true)
          
  /** Never used: Creates an empty Action collection */
  def member_create_action_collection = ApiMember(
    name = "create action collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TAction),
    semantics = DefaultSemantics
  )

  /** Never used: Creates an empty DateTime collection */
  def member_create_date_time_collection = ApiMember(
    name = "create date time collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TDateTime),
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an empty link collection */
  def member_create_link_collection = ApiMember(
    name = "create link collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TLink),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an empty location collection */
  def member_create_location_collection = ApiMember(
    name = "create location collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TLocation),
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an empty message collection */
  def member_create_message_collection = ApiMember(
    name = "create message collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TMessage),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an empty number collection */
  def member_create_number_collection = ApiMember(
    name = "create number collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TNumber),
    semantics = DefaultSemantics
  )

  /** Frequently used: Creates an empty number map */
  def member_create_number_map = ApiMember(
    name = "create number map",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber_Map,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an empty Picture collection */
  def member_create_picture_collection = ApiMember(
    name = "create picture collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TPicture),
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates an empty place collection */
  def member_create_place_collection = ApiMember(
    name = "create place collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TPlace),
    semantics = DefaultSemantics
  )

  /** Never used: Creates an empty Picture collection */
  def member_create_sound_collection = ApiMember(
    name = "create sound collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TSound),
    semantics = DefaultSemantics
  )

  /** Frequently used: Creates an empty string collection */
  def member_create_string_collection = ApiMember(
    name = "create string collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates an empty string map (case and culture sensitive) */
  def member_create_string_map = ApiMember(
    name = "create string map",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString_Map,
    semantics = DefaultSemantics
  )

  /** Never used: Creates an empty User collection */
  def member_create_user_collection = ApiMember(
    name = "create user collection",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TUser),
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "create action collection" -> member_create_action_collection,
    "create date time collection" -> member_create_date_time_collection,
    "create link collection" -> member_create_link_collection,
    "create location collection" -> member_create_location_collection,
    "create message collection" -> member_create_message_collection,
    "create number collection" -> member_create_number_collection,
    "create number map" -> member_create_number_map,
    "create picture collection" -> member_create_picture_collection,
    "create place collection" -> member_create_place_collection,
    "create sound collection" -> member_create_sound_collection,
    "create string collection" -> member_create_string_collection,
    "create string map" -> member_create_string_map,
    "create user collection" -> member_create_user_collection
  )
            

}
          
