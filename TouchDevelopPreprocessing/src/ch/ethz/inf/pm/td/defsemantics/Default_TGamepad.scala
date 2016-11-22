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
 * Specifies the abstract semantics of Gamepad
 *
 * A snapshot of the gamepad state
 *
 * @author Lucas Brutschy
 */

trait Default_TGamepad extends AAny {

  lazy val typeName = TypeName("Gamepad")
          
  /** Never used: Gets the `x` and `y` value of the selected axes. */
  def member_axes = ApiMember(
    name = "axes",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the pressed value of a button. Returns 0 if button missing. */
  def member_button_value = ApiMember(
    name = "button value",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the gamepad identifier */
  def member_id = ApiMember(
    name = "id",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the player index */
  def member_index = ApiMember(
    name = "index",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if a button is pressed. Returns false if button missing. */
  def member_is_button_pressed = ApiMember(
    name = "is button pressed",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if the gamepad is still connected. */
  def member_is_connected = ApiMember(
    name = "is connected",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the timestamp of this snapshot */
  def member_timestamp = ApiMember(
    name = "timestamp",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "axes" -> member_axes,
    "button value" -> member_button_value,
    "id" -> member_id,
    "index" -> member_index,
    "is button pressed" -> member_is_button_pressed,
    "is connected" -> member_is_connected,
    "timestamp" -> member_timestamp
  )
            

}
          
