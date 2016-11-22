/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Place
 *
 * A named location
 *
 * @author Lucas Brutschy
 */

trait Default_TPlace extends AAny {

  lazy val typeName = TypeName("Place")
          
  /** Never used: Gets the category of the place */
  def member_category = ApiMember(
    name = "category",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Checks into the place (supported for Facebook) */
  def member_check_in = ApiMember(
    name = "check in",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the identifier of this place */
  def member_id = ApiMember(
    name = "id",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the link associated to the message */
  def member_link = ApiMember(
    name = "link",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the location of the place */
  def member_location = ApiMember(
    name = "location",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the name of the place */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets a url to the picture */
  def member_picture_link = ApiMember(
    name = "picture link",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the category of the place */
  def member_set_category = ApiMember(
    name = "set category",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the identifier of this place */
  def member_set_id = ApiMember(
    name = "set id",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the link associated to the place */
  def member_set_link = ApiMember(
    name = "set link",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the location of the place */
  def member_set_location = ApiMember(
    name = "set location",
    paramTypes = List(ApiParam(TLocation)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the name of the place */
  def member_set_name = ApiMember(
    name = "set name",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the url to the picture */
  def member_set_picture_link = ApiMember(
    name = "set picture link",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the source of this place */
  def member_set_source = ApiMember(
    name = "set source",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the source of this place */
  def member_source = ApiMember(
    name = "source",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Converts to a string name,lat,long */
  def member_to_string = ApiMember(
    name = "to string",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the additional values stored in the place */
  def member_values = ApiMember(
    name = "values",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString_Map,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "category" -> member_category,
    "check in" -> member_check_in,
    "id" -> member_id,
    "link" -> member_link,
    "location" -> member_location,
    "name" -> member_name,
    "picture link" -> member_picture_link,
    "set category" -> member_set_category,
    "set id" -> member_set_id,
    "set link" -> member_set_link,
    "set location" -> member_set_location,
    "set name" -> member_set_name,
    "set picture link" -> member_set_picture_link,
    "set source" -> member_set_source,
    "source" -> member_source,
    "to string" -> member_to_string,
    "values" -> member_values
  )
            

}
          
