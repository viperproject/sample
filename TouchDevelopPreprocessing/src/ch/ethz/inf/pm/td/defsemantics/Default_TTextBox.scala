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
 * Specifies the abstract semantics of TextBox
 *
 * A text box
 *
 * @author Lucas Brutschy
 */

trait Default_TTextBox extends AAny {

  lazy val typeName = TypeName("TextBox")
          
  /** Rarely used: Gets the background color */
  def member_background = ApiMember(
    name = "background",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the border color */
  def member_border = ApiMember(
    name = "border",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the font size */
  def member_font_size = ApiMember(
    name = "font size",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the foreground color */
  def member_foreground = ApiMember(
    name = "foreground",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the icon picture (max 173x173) */
  def member_icon = ApiMember(
    name = "icon",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the background color */
  def member_set_background = ApiMember(
    name = "set background",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the border color */
  def member_set_border = ApiMember(
    name = "set border",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the font size (small = 14, normal = 15, medium = 17, medium large = 19, large = 24, extra large = 32, extra extra large = 54, huge = 140 */
  def member_set_font_size = ApiMember(
    name = "set font size",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the foreground color */
  def member_set_foreground = ApiMember(
    name = "set foreground",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the icon picture (max 96 x 96) */
  def member_set_icon = ApiMember(
    name = "set icon",
    paramTypes = List(ApiParam(TPicture)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the text */
  def member_set_text = ApiMember(
    name = "set text",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the text */
  def member_text = ApiMember(
    name = "text",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "background" -> member_background,
    "border" -> member_border,
    "font size" -> member_font_size,
    "foreground" -> member_foreground,
    "icon" -> member_icon,
    "set background" -> member_set_background,
    "set border" -> member_set_border,
    "set font size" -> member_set_font_size,
    "set foreground" -> member_set_foreground,
    "set icon" -> member_set_icon,
    "set text" -> member_set_text,
    "text" -> member_text
  )
            

}
          
