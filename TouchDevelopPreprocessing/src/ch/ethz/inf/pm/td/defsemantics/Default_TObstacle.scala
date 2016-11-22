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
 * Specifies the abstract semantics of Obstacle
 *
 * An obstacle on a board
 *
 * @author Lucas Brutschy
 */

trait Default_TObstacle extends AAny {

  lazy val typeName = TypeName("Obstacle")
          
  /** Never used: Delete the obstacle */
  def member_delete_ = ApiMember(
    name = "delete",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Attaches a handler where a sprite bounces on the obstacle */
  def member_on_collision = ApiMember(
    name = "on collision",
    paramTypes = List(ApiParam(TSprite_Action)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the obstacle color */
  def member_set_color = ApiMember(
    name = "set color",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the obstacle thickness */
  def member_set_thickness = ApiMember(
    name = "set thickness",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "delete" -> member_delete_,
    "on collision" -> member_on_collision,
    "set color" -> member_set_color,
    "set thickness" -> member_set_thickness
  )
            

}
          
