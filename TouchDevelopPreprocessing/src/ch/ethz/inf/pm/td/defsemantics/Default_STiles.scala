
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
 * Specifies the abstract semantics of Tiles
 *
 * tiles and notifications for Windows and Windows Phone
 *
 * @author Lucas Brutschy
 */

trait Default_STiles extends ASingleton {

  lazy val typeName = TypeName("Tiles", isSingleton = true)
          
  /** Never used: Pins or updates the default tile. */
  def member_pin_default = ApiMember(
    name = "pin default",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Pins or updates the default tile with a custom picture. */
  def member_pin_picture = ApiMember(
    name = "pin picture",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TNumber), ApiParam(TPicture)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Pins or updates the default tile with custom pictures. */
  def member_pin_pictures = ApiMember(
    name = "pin pictures",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber), ApiParam(TPicture), ApiParam(GCollection(TPicture))),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the counter of the default tile. Hidden if the number is not between 1 or 99. */
  def member_set_default_counter = ApiMember(
    name = "set default counter",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the front of a standard tile. */
  def member_set_default_text = ApiMember(
    name = "set default text",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the title, content and counter of the default tile. The counter is hidden if the number is not between 1 or 99. */
  def member_set_default = ApiMember(
    name = "set default",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "pin default" -> member_pin_default,
    "pin picture" -> member_pin_picture,
    "pin pictures" -> member_pin_pictures,
    "set default counter" -> member_set_default_counter,
    "set default text" -> member_set_default_text,
    "set default" -> member_set_default
  )
            

}
          
