
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
 * Specifies the abstract semantics of Sprite Sheet
 *
 * A sprite sheet which packs multiple frames in a single picture
 *
 * @author Lucas Brutschy
 */

trait Default_TSprite_Sheet extends AAny {

  lazy val typeName = TypeName("Sprite Sheet")
          
  /** Never used: Defines an animation as a custom sequence of frames. */
  def member_add_animation = ApiMember(
    name = "add animation",
    paramTypes = List(ApiParam(TString), ApiParam(GCollection(TString)), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TBoolean)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Defines a new frame in the sprite sheet */
  def member_add_frame = ApiMember(
    name = "add frame",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TBoolean)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Defines an animation as a continuous sequence of frames. The frame index starts at `1`. */
  def member_add_grid_animation = ApiMember(
    name = "add grid animation",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TBoolean)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Creates a new sprite displaying the given frame. */
  def member_create_sprite = ApiMember(
    name = "create sprite",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TSprite,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the picture associated to this sprite sheet. */
  def member_picture = ApiMember(
    name = "picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the frames as a rectangular grid. The tiles are numbered from top, left to bottom right starting at 0. */
  def member_set_frame_grid = ApiMember(
    name = "set frame grid",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the current frame displayed by sprite */
  def member_set_frame = ApiMember(
    name = "set frame",
    paramTypes = List(ApiParam(TSprite), ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add animation" -> member_add_animation,
    "add frame" -> member_add_frame,
    "add grid animation" -> member_add_grid_animation,
    "create sprite" -> member_create_sprite,
    "picture" -> member_picture,
    "set frame grid" -> member_set_frame_grid,
    "set frame" -> member_set_frame
  )
            

}
          
