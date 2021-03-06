/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiParam, DefaultSemantics}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Maps
 *
 * Maps, location to address, address to location
 *
 * @author Lucas Brutschy
 */

trait Default_SMaps extends ASingleton {

  lazy val typeName = TypeName("Maps", isSingleton = true)

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "create full map" -> member_create_full_map,
    "create map" -> member_create_map,
    "directions" -> member_directions,
    "open directions" -> member_open_directions,
    "open map" -> member_open_map
  )

  /** Sometimes used: Creates a full screen Bing map. Use 'post to wall' to display it. */
  def member_create_full_map = ApiMember(
    name = "create full map",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMap,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a Bing map. Use 'post to wall' to display it. */
  def member_create_map = ApiMember(
    name = "create map",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMap,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Calculates the directions between two coordinates using Bing. */
  def member_directions = ApiMember(
    name = "directions",
    paramTypes = List(ApiParam(TLocation), ApiParam(TLocation), ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = GCollection(TLocation),
    pausesInterpreter = true,
    isAsync = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Shows the directions in the Bing map application. If search term is provided, location is ignored.Provide search term or location for start and end. */
  def member_open_directions = ApiMember(
    name = "open directions",
    paramTypes = List(ApiParam(TString), ApiParam(TLocation), ApiParam(TString), ApiParam(TLocation)),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Rarely used: Opens the Bing map application. zoom between 0 (close) and 1 (far). */
  def member_open_map = ApiMember(
    name = "open map",
    paramTypes = List(ApiParam(TLocation), ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )
            

}
          
