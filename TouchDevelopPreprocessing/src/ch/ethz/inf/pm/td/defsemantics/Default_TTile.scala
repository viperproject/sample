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
 * Specifies the abstract semantics of Tile
 *
 * This type is no longer supported. See [](/tiles) for more information.
 *
 * @author Lucas Brutschy
 */

trait Default_TTile extends AAny {

  lazy val typeName = TypeName("Tile")
          
  /** Sometimes used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_back_icon = ApiMember(
    name = "back icon",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_back_title = ApiMember(
    name = "back title",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_background = ApiMember(
    name = "background",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_clear_back_icon = ApiMember(
    name = "clear back icon",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_clear_icon = ApiMember(
    name = "clear icon",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_content = ApiMember(
    name = "content",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_counter = ApiMember(
    name = "counter",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_height = ApiMember(
    name = "height",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_icon = ApiMember(
    name = "icon",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_panorama = ApiMember(
    name = "panorama",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_pin_to_start = ApiMember(
    name = "pin to start",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_set_back_icon = ApiMember(
    name = "set back icon",
    paramTypes = List(ApiParam(TPicture)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_set_back_title = ApiMember(
    name = "set back title",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_set_background = ApiMember(
    name = "set background",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_set_content = ApiMember(
    name = "set content",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_set_counter = ApiMember(
    name = "set counter",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_set_icon = ApiMember(
    name = "set icon",
    paramTypes = List(ApiParam(TPicture)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_set_panorama = ApiMember(
    name = "set panorama",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_set_title = ApiMember(
    name = "set title",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_title = ApiMember(
    name = "title",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] This action is no longer supported. See [](/tiles) for more information. */
  def member_width = ApiMember(
    name = "width",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "back icon" -> member_back_icon,
    "back title" -> member_back_title,
    "background" -> member_background,
    "clear back icon" -> member_clear_back_icon,
    "clear icon" -> member_clear_icon,
    "content" -> member_content,
    "counter" -> member_counter,
    "height" -> member_height,
    "icon" -> member_icon,
    "panorama" -> member_panorama,
    "pin to start" -> member_pin_to_start,
    "set back icon" -> member_set_back_icon,
    "set back title" -> member_set_back_title,
    "set background" -> member_set_background,
    "set content" -> member_set_content,
    "set counter" -> member_set_counter,
    "set icon" -> member_set_icon,
    "set panorama" -> member_set_panorama,
    "set title" -> member_set_title,
    "title" -> member_title,
    "width" -> member_width
  )
            

}
          
