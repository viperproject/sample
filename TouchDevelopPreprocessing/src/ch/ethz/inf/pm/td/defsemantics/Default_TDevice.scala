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
 * Specifies the abstract semantics of Device
 *
 * A device on the home network
 *
 * @author Lucas Brutschy
 */

trait Default_TDevice extends AAny {

  lazy val typeName = TypeName("Device")
          
  /** Rarely used: [**not implemented**] [**obsolete**] Browses to the device control panel */
  def member_browse = ApiMember(
    name = "browse",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Checks if the device is connected */
  def member_is_connected = ApiMember(
    name = "is connected",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] [**obsolete**] Gets the manfacturer name */
  def member_manufacturer = ApiMember(
    name = "manufacturer",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Gets the friendly name of the device */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Sets the friendly name of the device */
  def member_set_name = ApiMember(
    name = "set name",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "browse" -> member_browse,
    "is connected" -> member_is_connected,
    "manufacturer" -> member_manufacturer,
    "name" -> member_name,
    "set name" -> member_set_name
  )
            

}
          
