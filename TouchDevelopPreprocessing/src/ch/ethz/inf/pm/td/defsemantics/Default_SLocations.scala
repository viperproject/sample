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
 * Specifies the abstract semantics of Locations
 *
 * Geo coordinates
 *
 * @author Lucas Brutschy
 */

trait Default_SLocations extends ASingleton {

  lazy val typeName = TypeName("Locations", isSingleton = true)
          
  /** Rarely used: Creates an empty list of locations */
  def member_create_location_list = ApiMember(
    name = "create location list",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TLocation),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a new geo coordinate location */
  def member_create_location = ApiMember(
    name = "create location",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Looks for an address near a location using Bing. */
  def member_describe_location = ApiMember(
    name = "describe location",
    paramTypes = List(ApiParam(TLocation)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Frequently used: Looks for the coordinate of an address using Bing. */
  def member_search_location = ApiMember(
    name = "search location",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "create location list" -> member_create_location_list,
    "create location" -> member_create_location,
    "describe location" -> member_describe_location,
    "search location" -> member_search_location
  )
            

}
          
