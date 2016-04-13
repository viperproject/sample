
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
 * Specifies the abstract semantics of Phone
 *
 * Phone numbers, vibrate, etc...
 *
 * @author Lucas Brutschy
 */

trait Default_SPhone extends ASingleton {

  lazy val typeName = TypeName("Phone", isSingleton = true)
          
  /** Rarely used: Chooses an address from the contacts */
  def member_choose_address = ApiMember(
    name = "choose address",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Chooses a phone number from the contact list */
  def member_choose_phone_number = ApiMember(
    name = "choose phone number",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLink,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Starts a phone call */
  def member_dial_phone_number = ApiMember(
    name = "dial phone number",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Indicates if the phone is on 'battery' or 'external' power source. */
  def member_power_source = ApiMember(
    name = "power source",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Obsolete, use social->save contact instead */
  def member_save_contact = ApiMember(
    name = "save contact",
    paramTypes = List(ApiParam(TContact)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Allows the user to save the phone number */
  def member_save_phone_number = ApiMember(
    name = "save phone number",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Vibrates the phone for ... seconds (0.02 minimum) */
  def member_vibrate = ApiMember(
    name = "vibrate",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "choose address" -> member_choose_address,
    "choose phone number" -> member_choose_phone_number,
    "dial phone number" -> member_dial_phone_number,
    "power source" -> member_power_source,
    "save contact" -> member_save_contact,
    "save phone number" -> member_save_phone_number,
    "vibrate" -> member_vibrate
  )
            

}
          
