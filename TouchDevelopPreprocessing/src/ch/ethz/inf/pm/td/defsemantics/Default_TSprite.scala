
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Sprite
 *
 * A sprite
 *
 * @author Lucas Brutschy
 */

trait Default_TSprite extends AAny {

  lazy val typeName = TypeName("Sprite")
          
  /** Never used: Gets the acceleration along x in pixels/sec^2 */
  def member_acceleration_x = ApiMember(
    name = "acceleration x",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the acceleration along y in pixels/sec^2 */
  def member_acceleration_y = ApiMember(
    name = "acceleration y",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the angle of the sprite in degrees */
  def member_angle = ApiMember(
    name = "angle",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the rotation speed in degrees/sec */
  def member_angular_speed = ApiMember(
    name = "angular speed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the bottom position in pixels */
  def member_bottom = ApiMember(
    name = "bottom",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the sprite color. */
  def member_color = ApiMember(
    name = "color",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Never used: Starts a new tween animation. */
  def member_create_animation = ApiMember(
    name = "create animation",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSprite_Animation,
    semantics = DefaultSemantics
  )

  /** Frequently used: Delete sprite. */
  def member_delete_ = ApiMember(
    name = "delete",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the sprite elasticity as a fraction of speed preservation per bounce (0-1) */
  def member_elasticity = ApiMember(
    name = "elasticity",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Fits the bounding box to the size of the text */
  def member_fit_text = ApiMember(
    name = "fit text",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the font size in pixels (for text sprites) */
  def member_font_size = ApiMember(
    name = "font size",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the fraction of speed loss between 0 and 1 */
  def member_friction = ApiMember(
    name = "friction",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the height in pixels */
  def member_height = ApiMember(
    name = "height",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Hide sprite. */
  def member_hide = ApiMember(
    name = "hide",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: True if sprite is deleted. */
  def member_is_deleted = ApiMember(
    name = "is deleted",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns true if sprite is not hidden */
  def member_is_visible = ApiMember(
    name = "is visible",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the left position in pixels */
  def member_left = ApiMember(
    name = "left",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the geo location assigned to the sprite */
  def member_location = ApiMember(
    name = "location",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the mass */
  def member_mass = ApiMember(
    name = "mass",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Use `Sprite Sheet` instead. */
  def member_move_clip = ApiMember(
    name = "move clip",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Moves sprite towards other sprite. */
  def member_move_towards = ApiMember(
    name = "move towards",
    paramTypes = List(ApiParam(TSprite), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Moves sprite. */
  def member_move = ApiMember(
    name = "move",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set the handler invoked when the sprite is dragged */
  def member_on_drag = ApiMember(
    name = "on drag",
    paramTypes = List(ApiParam(TVector_Action)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Add an action that fires for every display frame */
  def member_on_every_frame = ApiMember(
    name = "on every frame",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Set the handler invoked when the sprite is swiped */
  def member_on_swipe = ApiMember(
    name = "on swipe",
    paramTypes = List(ApiParam(TVector_Action)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Set the handler invoked when the sprite is tapped */
  def member_on_tap = ApiMember(
    name = "on tap",
    paramTypes = List(ApiParam(TPosition_Action)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Set the handler invoked when the sprite is touched initially */
  def member_on_touch_down = ApiMember(
    name = "on touch down",
    paramTypes = List(ApiParam(TPosition_Action)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Set the handler invoked when the sprite touch is released */
  def member_on_touch_up = ApiMember(
    name = "on touch up",
    paramTypes = List(ApiParam(TPosition_Action)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the opacity (between 0 transparent and 1 opaque) */
  def member_opacity = ApiMember(
    name = "opacity",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns the subset of sprites in the given set that overlap with sprite. */
  def member_overlap_with = ApiMember(
    name = "overlap with",
    paramTypes = List(ApiParam(TSprite_Set)),
    thisType = ApiParam(this),
    returnType = TSprite_Set,
    semantics = DefaultSemantics
  )

  /** Frequently used: Do the sprites overlap */
  def member_overlaps_with = ApiMember(
    name = "overlaps with",
    paramTypes = List(ApiParam(TSprite)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: The picture on a picture sprite (if it is a picture sprite) */
  def member_picture = ApiMember(
    name = "picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the right position in pixels */
  def member_right = ApiMember(
    name = "right",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the scaling applied when rendering the sprite. This scaling does not influence the bounding box. */
  def member_scale = ApiMember(
    name = "scale",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the x acceleration in pixels/sec^2 */
  def member_set_acceleration_x = ApiMember(
    name = "set acceleration x",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the y acceleration in pixels/sec^2 */
  def member_set_acceleration_y = ApiMember(
    name = "set acceleration y",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the acceleration in pixels/sec^2 */
  def member_set_acceleration = ApiMember(
    name = "set acceleration",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the angle of the sprite in degrees */
  def member_set_angle = ApiMember(
    name = "set angle",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the rotation speed in degrees/sec */
  def member_set_angular_speed = ApiMember(
    name = "set angular speed",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the bottom position in pixels */
  def member_set_bottom = ApiMember(
    name = "set bottom",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the clipping area for an image sprite (if it is an image sprite) */
  def member_set_clip = ApiMember(
    name = "set clip",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Sets the sprite color. */
  def member_set_color = ApiMember(
    name = "set color",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the sprite elasticity as a fraction of speed preservation per bounce (0-1) */
  def member_set_elasticity = ApiMember(
    name = "set elasticity",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the font size in pixels of the sprite (for text sprites) */
  def member_set_font_size = ApiMember(
    name = "set font size",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the friction to a fraction of speed loss between 0 and 1 */
  def member_set_friction = ApiMember(
    name = "set friction",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the height in pixels */
  def member_set_height = ApiMember(
    name = "set height",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the left position in pixels */
  def member_set_left = ApiMember(
    name = "set left",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the geo location of the sprite */
  def member_set_location = ApiMember(
    name = "set location",
    paramTypes = List(ApiParam(TLocation)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the sprite mass. */
  def member_set_mass = ApiMember(
    name = "set mass",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the sprite opacity (between 0 transparent and 1 opaque). */
  def member_set_opacity = ApiMember(
    name = "set opacity",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Updates picture on a picture sprite (if it is a picture sprite) */
  def member_set_picture = ApiMember(
    name = "set picture",
    paramTypes = List(ApiParam(TPicture)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Sets the position in pixels */
  def member_set_pos = ApiMember(
    name = "set pos",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the right position in pixels */
  def member_set_right = ApiMember(
    name = "set right",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the scaling applied to the sprite on rendering. This scaling does not influence the bounding box. */
  def member_set_scale = ApiMember(
    name = "set scale",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the shadow information */
  def member_set_shadow = ApiMember(
    name = "set shadow",
    paramTypes = List(ApiParam(TNumber), ApiParam(TColor), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the x speed in pixels/sec */
  def member_set_speed_x = ApiMember(
    name = "set speed x",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the y speed in pixels/sec */
  def member_set_speed_y = ApiMember(
    name = "set speed y",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the speed in pixels/sec */
  def member_set_speed = ApiMember(
    name = "set speed",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the current text baseline used when drawing text (for text sprites) */
  def member_set_text_baseline = ApiMember(
    name = "set text baseline",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Updates text on a text sprite (if it is a text sprite) */
  def member_set_text = ApiMember(
    name = "set text",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the top position in pixels */
  def member_set_top = ApiMember(
    name = "set top",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the width in pixels */
  def member_set_width = ApiMember(
    name = "set width",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the center horizontal position in pixels */
  def member_set_x = ApiMember(
    name = "set x",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the y position in pixels */
  def member_set_y = ApiMember(
    name = "set y",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the z-index of the sprite */
  def member_set_z_index = ApiMember(
    name = "set z index",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the associated sprite sheet */
  def member_sheet = ApiMember(
    name = "sheet",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSprite_Sheet,
    semantics = DefaultSemantics
  )

  /** Frequently used: Show sprite. */
  def member_show = ApiMember(
    name = "show",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets sprite speed direction towards other sprite with given magnitude. */
  def member_speed_towards = ApiMember(
    name = "speed towards",
    paramTypes = List(ApiParam(TSprite), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the speed along x in pixels/sec */
  def member_speed_x = ApiMember(
    name = "speed x",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the speed along y in pixels/sec */
  def member_speed_y = ApiMember(
    name = "speed y",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the current text baseline (for text sprites) */
  def member_text_baseline = ApiMember(
    name = "text baseline",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Frequently used: The text on a text sprite (if it is a text sprite) */
  def member_text = ApiMember(
    name = "text",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the top position in pixels */
  def member_top = ApiMember(
    name = "top",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the width in pixels */
  def member_width = ApiMember(
    name = "width",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the center horizontal position of in pixels */
  def member_x = ApiMember(
    name = "x",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the y position in pixels */
  def member_y = ApiMember(
    name = "y",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the z-index of the sprite */
  def member_z_index = ApiMember(
    name = "z index",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "acceleration x" -> member_acceleration_x,
    "acceleration y" -> member_acceleration_y,
    "angle" -> member_angle,
    "angular speed" -> member_angular_speed,
    "bottom" -> member_bottom,
    "color" -> member_color,
    "create animation" -> member_create_animation,
    "delete" -> member_delete_,
    "elasticity" -> member_elasticity,
    "fit text" -> member_fit_text,
    "font size" -> member_font_size,
    "friction" -> member_friction,
    "height" -> member_height,
    "hide" -> member_hide,
    "is deleted" -> member_is_deleted,
    "is visible" -> member_is_visible,
    "left" -> member_left,
    "location" -> member_location,
    "mass" -> member_mass,
    "move clip" -> member_move_clip,
    "move towards" -> member_move_towards,
    "move" -> member_move,
    "on drag" -> member_on_drag,
    "on every frame" -> member_on_every_frame,
    "on swipe" -> member_on_swipe,
    "on tap" -> member_on_tap,
    "on touch down" -> member_on_touch_down,
    "on touch up" -> member_on_touch_up,
    "opacity" -> member_opacity,
    "overlap with" -> member_overlap_with,
    "overlaps with" -> member_overlaps_with,
    "picture" -> member_picture,
    "right" -> member_right,
    "scale" -> member_scale,
    "set acceleration x" -> member_set_acceleration_x,
    "set acceleration y" -> member_set_acceleration_y,
    "set acceleration" -> member_set_acceleration,
    "set angle" -> member_set_angle,
    "set angular speed" -> member_set_angular_speed,
    "set bottom" -> member_set_bottom,
    "set clip" -> member_set_clip,
    "set color" -> member_set_color,
    "set elasticity" -> member_set_elasticity,
    "set font size" -> member_set_font_size,
    "set friction" -> member_set_friction,
    "set height" -> member_set_height,
    "set left" -> member_set_left,
    "set location" -> member_set_location,
    "set mass" -> member_set_mass,
    "set opacity" -> member_set_opacity,
    "set picture" -> member_set_picture,
    "set pos" -> member_set_pos,
    "set right" -> member_set_right,
    "set scale" -> member_set_scale,
    "set shadow" -> member_set_shadow,
    "set speed x" -> member_set_speed_x,
    "set speed y" -> member_set_speed_y,
    "set speed" -> member_set_speed,
    "set text baseline" -> member_set_text_baseline,
    "set text" -> member_set_text,
    "set top" -> member_set_top,
    "set width" -> member_set_width,
    "set x" -> member_set_x,
    "set y" -> member_set_y,
    "set z index" -> member_set_z_index,
    "sheet" -> member_sheet,
    "show" -> member_show,
    "speed towards" -> member_speed_towards,
    "speed x" -> member_speed_x,
    "speed y" -> member_speed_y,
    "text baseline" -> member_text_baseline,
    "text" -> member_text,
    "top" -> member_top,
    "width" -> member_width,
    "x" -> member_x,
    "y" -> member_y,
    "z index" -> member_z_index
  )
            

}
          
