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
 * Specifies the abstract semantics of Board Background Layer
 *
 * A background scene layer
 *
 * @author Lucas Brutschy
 */

trait Default_TBoard_Background_Layer extends AAny {

  lazy val typeName = TypeName("Board Background Layer")
          
  /** Never used: Gets a value indicating how the picture aligns horizontally. The default is `left`. */
  def member_align_x = ApiMember(
    name = "align x",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a value indicating how the picture aligns vertically. The default is `top`. */
  def member_align_y = ApiMember(
    name = "align y",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the layer distance */
  def member_distance = ApiMember(
    name = "distance",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the picture associated to the layer. */
  def member_picture = ApiMember(
    name = "picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a value indicating if the background repeats horizontally */
  def member_repeat_x = ApiMember(
    name = "repeat x",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a value indicating if the background repeats horizontally */
  def member_repeat_y = ApiMember(
    name = "repeat y",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets a value indicating how the picture aligns horizontally. The default is `left`. */
  def member_set_align_x = ApiMember(
    name = "set align x",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets a value indicating how the picture aligns vertically. The default is `top`. */
  def member_set_align_y = ApiMember(
    name = "set align y",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the layer distance */
  def member_set_distance = ApiMember(
    name = "set distance",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets a value indicating if the background repeats horizontally */
  def member_set_repeat_x = ApiMember(
    name = "set repeat x",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets a value indicating if the background repeats horizontally */
  def member_set_repeat_y = ApiMember(
    name = "set repeat y",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "align x" -> member_align_x,
    "align y" -> member_align_y,
    "distance" -> member_distance,
    "picture" -> member_picture,
    "repeat x" -> member_repeat_x,
    "repeat y" -> member_repeat_y,
    "set align x" -> member_set_align_x,
    "set align y" -> member_set_align_y,
    "set distance" -> member_set_distance,
    "set repeat x" -> member_set_repeat_x,
    "set repeat y" -> member_set_repeat_y
  )
            

}
          
