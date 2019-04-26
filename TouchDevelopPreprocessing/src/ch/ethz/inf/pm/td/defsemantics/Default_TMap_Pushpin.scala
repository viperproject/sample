/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Map Pushpin
 *
 * A map pushpin
 *
 * @author Lucas Brutschy
 */

trait Default_TMap_Pushpin extends AAny {

  lazy val typeName = TypeName("Map Pushpin")
          
  /** Never used: Gets the pushpin geo location */
  def member_location = ApiMember(
    name = "location",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )

  /** Never used: Set the handler invoked when the pushpin is tapped */
  def member_on_tap = ApiMember(
    name = "on tap",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the location of the pushpin */
  def member_set_location = ApiMember(
    name = "set location",
    paramTypes = List(ApiParam(TLocation)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Shows or hides the pushpin */
  def member_set_visible = ApiMember(
    name = "set visible",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "location" -> member_location,
    "on tap" -> member_on_tap,
    "set location" -> member_set_location,
    "set visible" -> member_set_visible
  )
            

}
          
