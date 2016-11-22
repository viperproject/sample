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
 * Specifies the abstract semantics of Board Background Scene
 *
 * A scene contains layers of parralax backgrounds.
 *
 * @author Lucas Brutschy
 */

trait Default_TBoard_Background_Scene extends ALinearCollection {

  lazy val typeName = TypeName("Board Background Scene")
          
  def keyType = TNumber

  def valueType = TBoard_Background_Layer

  /** Never used: Removes all layers from scene and resets the viewport */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a new layer on the scene. The distance determines the order of rendering and how fast the layer moves */
  def member_create_layer = ApiMember(
    name = "create layer",
    paramTypes = List(ApiParam(TNumber), ApiParam(TPicture)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TBoard_Background_Layer,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the view horizontal offset */
  def member_set_view_x = ApiMember(
    name = "set view x",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the view vertical offset */
  def member_set_view_y = ApiMember(
    name = "set view y",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the view horizontal offset */
  def member_view_x = ApiMember(
    name = "view x",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the view vertical offset */
  def member_view_y = ApiMember(
    name = "view y",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "clear" -> member_clear,
    "create layer" -> member_create_layer,
    "set view x" -> member_set_view_x,
    "set view y" -> member_set_view_y,
    "view x" -> member_view_x,
    "view y" -> member_view_y
  )
            

}
          
