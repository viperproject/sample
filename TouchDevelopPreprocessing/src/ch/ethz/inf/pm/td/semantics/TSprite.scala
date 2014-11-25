package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ExpressionInitializer, ApiField}
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.ApiMember
import ch.ethz.inf.pm.td.compiler.DefaultSemantics
import ch.ethz.inf.pm.td.compiler.ApiParam
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, TouchType, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Sprite
 *
 * A sprite
 *
 * @author Lucas Brutschy
 */


object TSprite extends AAny {

  lazy val typeName = TypeName("Sprite")

  /** Never used: Gets the acceleration along x in pixels/sec^2 */
  lazy val member_acceleration_x = new ApiMember("acceleration x", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Never used: Gets the acceleration along y in pixels/sec^2 */
  lazy val member_acceleration_y = new ApiMember("acceleration y", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Sometimes used: Gets the angle of the sprite in degrees */
  lazy val member_angle = new ApiMember("angle", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Sometimes used: Gets the rotation speed in degrees/sec */
  lazy val member_angular_speed = new ApiMember("angular speed", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Never used: Gets the bottom position in pixels */
  lazy val member_bottom = new ApiMember("bottom", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Frequently used: Returns the sprite color. */
  lazy val member_color = new ApiMember("color", List(), ApiParam(TSprite), TColor) with DefaultSemantics

  /** Never used: Starts a new tween animation. */
  lazy val member_create_animation = new ApiMember("create animation", List(), ApiParam(TSprite), TSprite_Animation) with DefaultSemantics

  /** Frequently used: Delete sprite. */
  lazy val member_delete = new ApiMember("delete", List(), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Rarely used: Gets the sprite elasticity as a fraction of speed preservation per bounce (0-1) */
  lazy val member_elasticity = new ApiMember("elasticity", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Sometimes used: Are these the same sprite */
  lazy val member_equals = new ApiMember("equals", List(ApiParam(TSprite)), ApiParam(TSprite), TBoolean) with DefaultSemantics

  /** Never used: Fits the bounding box to the size of the text */
  lazy val member_fit_text = new ApiMember("fit text", List(), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: Gets the font size in pixels (for text sprites) */
  lazy val member_font_size = new ApiMember("font size", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Never used: Gets the fraction of speed loss between 0 and 1 */
  lazy val member_friction = new ApiMember("friction", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Frequently used: Gets the height in pixels */
  lazy val member_height = new ApiMember("height", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Frequently used: Hide sprite. */
  lazy val member_hide = new ApiMember("hide", List(), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: True if sprite is deleted. */
  lazy val member_is_deleted = new ApiMember("is deleted", List(), ApiParam(TSprite), TBoolean) with DefaultSemantics

  /** Frequently used: Returns true if sprite is not hidden */
  lazy val member_is_visible = new ApiMember("is visible", List(), ApiParam(TSprite), TBoolean) with DefaultSemantics

  /** Never used: Gets the left position in pixels */
  lazy val member_left = new ApiMember("left", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Never used: Gets the geo location assigned to the sprite */
  lazy val member_location = new ApiMember("location", List(), ApiParam(TSprite), TLocation) with DefaultSemantics

  /** Rarely used: Gets the mass */
  lazy val member_mass = new ApiMember("mass", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Never used: [**obsolete**] Use `Sprite Sheet` instead. */
  lazy val member_move_clip = new ApiMember("move clip", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Sometimes used: Moves sprite towards other sprite. */
  lazy val member_move_towards = new ApiMember("move towards", List(ApiParam(TSprite), ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Moves sprite. */
  lazy val member_move = new ApiMember("move", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: Set the handler invoked when the sprite is dragged */
  lazy val member_on_drag = new ApiMember("on drag", List(ApiParam(TVector_Action)), ApiParam(TSprite), TEvent_Binding) with DefaultSemantics

  /** Never used: Add an action that fires for every display frame */
  lazy val member_on_every_frame = new ApiMember("on every frame", List(ApiParam(TAction)), ApiParam(TSprite), TEvent_Binding) with DefaultSemantics

  /** Never used: Set the handler invoked when the sprite is swiped */
  lazy val member_on_swipe = new ApiMember("on swipe", List(ApiParam(TVector_Action)), ApiParam(TSprite), TEvent_Binding) with DefaultSemantics

  /** Never used: Set the handler invoked when the sprite is tapped */
  lazy val member_on_tap = new ApiMember("on tap", List(ApiParam(TPosition_Action)), ApiParam(TSprite), TEvent_Binding) with DefaultSemantics

  /** Never used: Set the handler invoked when the sprite is touched initially */
  lazy val member_on_touch_down = new ApiMember("on touch down", List(ApiParam(TPosition_Action)), ApiParam(TSprite), TEvent_Binding) with DefaultSemantics

  /** Never used: Set the handler invoked when the sprite touch is released */
  lazy val member_on_touch_up = new ApiMember("on touch up", List(ApiParam(TPosition_Action)), ApiParam(TSprite), TEvent_Binding) with DefaultSemantics

  /** Sometimes used: Gets the opacity (between 0 transparent and 1 opaque) */
  lazy val member_opacity = new ApiMember("opacity", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Frequently used: Returns the subset of sprites in the given set that overlap with sprite. */
  lazy val member_overlap_with = new ApiMember("overlap with", List(ApiParam(TSprite_Set)), ApiParam(TSprite), TSprite_Set) with DefaultSemantics

  /** Frequently used: Do the sprites overlap */
  lazy val member_overlaps_with = new ApiMember("overlaps with", List(ApiParam(TSprite)), ApiParam(TSprite), TBoolean) with DefaultSemantics

  /** Sometimes used: The picture on a picture sprite (if it is a picture sprite) */
  lazy val member_picture = new ApiMember("picture", List(), ApiParam(TSprite), TPicture) with DefaultSemantics

  /** Never used: Gets the right position in pixels */
  lazy val member_right = new ApiMember("right", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Never used: Gets the scaling applied when rendering the sprite. This scaling does not influence the bounding box. */
  lazy val member_scale = new ApiMember("scale", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Rarely used: Sets the x acceleration in pixels/sec^2 */
  lazy val member_set_acceleration_x = new ApiMember("set acceleration x", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Rarely used: Sets the y acceleration in pixels/sec^2 */
  lazy val member_set_acceleration_y = new ApiMember("set acceleration y", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Rarely used: Sets the acceleration in pixels/sec^2 */
  lazy val member_set_acceleration = new ApiMember("set acceleration", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the angle of the sprite in degrees */
  lazy val member_set_angle = new ApiMember("set angle", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the rotation speed in degrees/sec */
  lazy val member_set_angular_speed = new ApiMember("set angular speed", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: Sets the bottom position in pixels */
  lazy val member_set_bottom = new ApiMember("set bottom", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Sometimes used: Sets the clipping area for an image sprite (if it is an image sprite) */
  lazy val member_set_clip = new ApiMember("set clip", List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Very frequently used: Sets the sprite color. */
  lazy val member_set_color = new ApiMember("set color", List(ApiParam(TColor)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Sometimes used: Sets the sprite elasticity as a fraction of speed preservation per bounce (0-1) */
  lazy val member_set_elasticity = new ApiMember("set elasticity", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: Sets the font size in pixels of the sprite (for text sprites) */
  lazy val member_set_font_size = new ApiMember("set font size", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the friction to a fraction of speed loss between 0 and 1 */
  lazy val member_set_friction = new ApiMember("set friction", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the height in pixels */
  lazy val member_set_height = new ApiMember("set height", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: Sets the left position in pixels */
  lazy val member_set_left = new ApiMember("set left", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Rarely used: Sets the geo location of the sprite */
  lazy val member_set_location = new ApiMember("set location", List(ApiParam(TLocation)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Rarely used: Sets the sprite mass. */
  lazy val member_set_mass = new ApiMember("set mass", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the sprite opacity (between 0 transparent and 1 opaque). */
  lazy val member_set_opacity = new ApiMember("set opacity", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Updates picture on a picture sprite (if it is a picture sprite) */
  lazy val member_set_picture = new ApiMember("set picture", List(ApiParam(TPicture)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Very frequently used: Sets the position in pixels */
  lazy val member_set_pos = new ApiMember("set pos", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: Sets the right position in pixels */
  lazy val member_set_right = new ApiMember("set right", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: Sets the scaling applied to the sprite on rendering. This scaling does not influence the bounding box. */
  lazy val member_set_scale = new ApiMember("set scale", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the x speed in pixels/sec */
  lazy val member_set_speed_x = new ApiMember("set speed x", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the y speed in pixels/sec */
  lazy val member_set_speed_y = new ApiMember("set speed y", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the speed in pixels/sec */
  lazy val member_set_speed = new ApiMember("set speed", List(ApiParam(TNumber), ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: Sets the current text baseline used when drawing text (for text sprites) */
  lazy val member_set_text_baseline = new ApiMember("set text baseline", List(ApiParam(TString)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Very frequently used: Updates text on a text sprite (if it is a text sprite) */
  lazy val member_set_text = new ApiMember("set text", List(ApiParam(TString)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: Sets the top position in pixels */
  lazy val member_set_top = new ApiMember("set top", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the width in pixels */
  lazy val member_set_width = new ApiMember("set width", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the center horizontal position in pixels */
  lazy val member_set_x = new ApiMember("set x", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Sets the y position in pixels */
  lazy val member_set_y = new ApiMember("set y", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Sometimes used: Sets the z-index of the sprite */
  lazy val member_set_z_index = new ApiMember("set z index", List(ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Never used: Gets the associated sprite sheet */
  lazy val member_sheet = new ApiMember("sheet", List(), ApiParam(TSprite), TSprite_Sheet) with DefaultSemantics

  /** Frequently used: Show sprite. */
  lazy val member_show = new ApiMember("show", List(), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Sometimes used: Sets sprite speed direction towards other sprite with given magnitude. */
  lazy val member_speed_towards = new ApiMember("speed towards", List(ApiParam(TSprite), ApiParam(TNumber)), ApiParam(TSprite,isMutated=true), TNothing) with DefaultSemantics

  /** Frequently used: Gets the speed along x in pixels/sec */
  lazy val member_speed_x = new ApiMember("speed x", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Frequently used: Gets the speed along y in pixels/sec */
  lazy val member_speed_y = new ApiMember("speed y", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Never used: Gets the current text baseline (for text sprites) */
  lazy val member_text_baseline = new ApiMember("text baseline", List(), ApiParam(TSprite), TString) with DefaultSemantics

  /** Frequently used: The text on a text sprite (if it is a text sprite) */
  lazy val member_text = new ApiMember("text", List(), ApiParam(TSprite), TString) with DefaultSemantics

  /** Never used: Gets the top position in pixels */
  lazy val member_top = new ApiMember("top", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Frequently used: Gets the width in pixels */
  lazy val member_width = new ApiMember("width", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Frequently used: Gets the center horizontal position of in pixels */
  lazy val member_x = new ApiMember("x", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Frequently used: Gets the y position in pixels */
  lazy val member_y = new ApiMember("y", List(), ApiParam(TSprite), TNumber) with DefaultSemantics

  /** Sometimes used: Gets the z-index of the sprite */
  lazy val member_z_index = new ApiMember("z index", List(), ApiParam(TSprite), TNumber) with DefaultSemantics


  override lazy val declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "acceleration x" -> member_acceleration_x,
    "acceleration y" -> member_acceleration_y,
    "angle" -> member_angle,
    "angular speed" -> member_angular_speed,
    "bottom" -> member_bottom,
    "color" -> member_color,
    "create animation" -> member_create_animation,
    "delete" -> member_delete,
    "elasticity" -> member_elasticity,
    "equals" -> member_equals,
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


  //  lazy val field_z_index = ApiField("z index",TNumber)
  //  lazy val field_y = ApiField("y",TNumber)
  //  lazy val field_x = ApiField("x",TNumber)
  //  lazy val field_width = ApiField("width",TNumber)
  //  lazy val field_top = ApiField("top",TNumber)
  //  lazy val field_text = ApiField("text",TString)
  //  lazy val field_text_baseline = ApiField("text baseline",TString)
  //  lazy val field_speed_y = ApiField("speed y",TNumber)
  //  lazy val field_speed_x = ApiField("speed x",TNumber)
  //  lazy val field_sheet = ApiField("sheet",TSprite_Sheet)
  //  lazy val field_scale = ApiField("scale",TNumber)
  //  lazy val field_right = ApiField("right",TNumber)
  //  lazy val field_picture = ApiField("picture",TPicture)
  //  lazy val field_opacity = ApiField("opacity",TNumber)
  //  lazy val field_mass = ApiField("mass",TNumber)
  //  lazy val field_location = ApiField("location",TLocation)
  //  lazy val field_left = ApiField("left",TNumber)
  //  lazy val field_is_visible = ApiField("is visible",TBoolean)
  //  lazy val field_is_deleted = ApiField("is deleted",TBoolean)
  //  lazy val field_height = ApiField("height",TNumber)
  //  lazy val field_friction = ApiField("friction",TNumber)
  //  lazy val field_font_size = ApiField("font size",TNumber)
  //  lazy val field_elasticity = ApiField("elasticity",TNumber)
  //  lazy val field_create_animation = ApiField("create animation",TSprite_Animation)
  //  lazy val field_color = ApiField("color",TColor)
  //  lazy val field_bottom = ApiField("bottom",TNumber)
  //  lazy val field_angular_speed = ApiField("angular speed",TNumber)
  //  lazy val field_angle = ApiField("angle",TNumber)
  //  lazy val field_acceleration_y = ApiField("acceleration y",TNumber)
  //  lazy val field_acceleration_x = ApiField("acceleration x",TNumber)


  //  override lazy val possibleFields:Set[ApiField] = super.possibleFields ++ Set(
  //    field_z_index,
  //    field_y,
  //    field_x,
  //    field_width,
  //    field_top,
  //    field_text,
  //    field_text_baseline,
  //    field_speed_y,
  //    field_speed_x,
  //    field_sheet,
  //    field_scale,
  //    field_right,
  //    field_picture,
  //    field_opacity,
  //    field_mass,
  //    field_location,
  //    field_left,
  //    field_is_visible,
  //    field_is_deleted,
  //    field_height,
  //    field_friction,
  //    field_font_size,
  //    field_elasticity,
  //    field_create_animation,
  //    field_color,
  //    field_bottom,
  //    field_angular_speed,
  //    field_angle,
  //    field_acceleration_y,
  //    field_acceleration_x
  //  )


  lazy val field_acceleration_x = new ApiField("acceleration x", TNumber.typeName)
  // Gets the acceleration along x in pixels/sec^2
  lazy val field_acceleration_y = new ApiField("acceleration y", TNumber.typeName)
  // Gets the acceleration along y in pixels/sec^2
  lazy val field_angle = new ApiField("angle", TNumber.typeName)
  // Gets the angle of the sprite in degrees
  lazy val field_angular_speed = new ApiField("angular speed", TNumber.typeName)
  // Gets the rotation speed in degrees/sec
  lazy val field_color = new ApiField("color", TColor.typeName)
  // Returns the sprite color.
  lazy val field_elasticity = new ApiField("elasticity", TNumber.typeName)
  // Gets the sprite elasticity as a fraction of speed preservation per bounce (0-1)
  lazy val field_friction = new ApiField("friction", TNumber.typeName)
  // Gets the fraction of speed loss between 0 and 1
  lazy val field_height = new ApiField("height", TNumber.typeName)
  // Gets the height in pixels
  lazy val field_is_deleted = new ApiField("is deleted", TBoolean.typeName, ExpressionInitializer(False(null)))
  // Returns false if sprite is not deleted
  lazy val field_is_visible = new ApiField("is visible", TBoolean.typeName)
  // Returns true if sprite is not hidden
  lazy val field_location = new ApiField("location", TLocation.typeName)
  // Gets the geo location assigned to the sprite
  lazy val field_mass = new ApiField("mass", TNumber.typeName)
  // Gets the mass
  lazy val field_opacity = new ApiField("opacity", TNumber.typeName)
  // Gets the opacity (between 0 transparent and 1 opaque)
  lazy val field_picture = new ApiField("picture", TPicture.typeName)
  lazy val field_speed_x = new ApiField("speed x", TNumber.typeName)
  // Gets the speed along x in pixels/sec
  lazy val field_speed_y = new ApiField("speed y", TNumber.typeName)
  // Gets the speed along y in pixels/sec
  lazy val field_text = new ApiField("text", TString.typeName)
  // The text on a text sprite (if it is a text sprite)
  lazy val field_width = new ApiField("width", TNumber.typeName)
  // Gets the width in pixels
  lazy val field_x = new ApiField("x", TNumber.typeName)
  // Gets the x position in pixels
  lazy val field_y = new ApiField("y", TNumber.typeName)
  // Gets the y position in pixels
  lazy val field_z_index = new ApiField("z index", TNumber.typeName) // Gets the z-index of the sprite

  lazy val field_clip_left = new ApiField("clip left", TNumber.typeName)
  lazy val field_clip_top = new ApiField("clip top", TNumber.typeName)
  lazy val field_clip_width = new ApiField("clip width", TNumber.typeName)
  lazy val field_clip_height = new ApiField("clip height", TNumber.typeName)

  /** Gets the scaling applied when rendering the sprite. This scaling does not influence the bounding box. */
  lazy val field_scale = new ApiField("scale", TNumber.typeName)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_drag_handler = new ApiField("drag handler", TVector_Action.typeName)
  lazy val field_swipe_handler = new ApiField("swipe handler", TVector_Action.typeName)
  lazy val field_tap_handler = new ApiField("tap handler", TPosition_Action.typeName)
  lazy val field_touch_down_handler = new ApiField("touch down handler", TPosition_Action.typeName)
  lazy val field_touch_up_handler = new ApiField("touch up handler", TPosition_Action.typeName)
  lazy val field_every_frame_handler = new ApiField("every frame handler", TAction.typeName)

  override def possibleFields = super.possibleFields ++ List(
    field_acceleration_x,
    field_acceleration_y,
    field_angle,
    field_angular_speed,
    field_color,
    field_elasticity,
    field_friction,
    field_height,
    field_is_visible,
    field_is_deleted,
    field_location,
    field_mass,
    field_opacity,
    field_picture,
    field_speed_x,
    field_speed_y,
    field_text,
    field_width,
    field_x,
    field_y,
    field_z_index,
    field_clip_left,
    field_clip_top,
    field_clip_width,
    field_clip_height,
    field_scale,
    field_drag_handler,
    field_swipe_handler,
    field_tap_handler,
    field_touch_down_handler,
    field_touch_up_handler,
    field_every_frame_handler
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {

    /** Starts a new tween animation. */
    case "create animation" =>
      New[S](TSprite_Animation)

    /** Delete sprite. */
    case "delete" =>
      AssignField[S](this0, TSprite.field_is_deleted, True)

    /** Are these the same sprite */
    case "equals" =>
      val List(other) = parameters // Sprite
      Dummy[S](this0, method)
      Top[S](TBoolean)

    /** Hide sprite. */
    case "hide" =>
      AssignField[S](this0, TSprite.field_is_visible, toRichExpression(0))

    /** Moves sprite. */
    case "move" =>
      val List(delta_x, delta_y) = parameters // Number,Number
    var curState = state
      curState = AssignField[S](this0, TSprite.field_x, Field[S](this0, TSprite.field_x) + delta_x)(curState, pp)
      curState = AssignField[S](this0, TSprite.field_y, Field[S](this0, TSprite.field_y) + delta_y)(curState, pp)
      curState

    /** Moves the clipping area and wraps around the image if needed (if it is an image sprite) */
    case "move clip" =>
      val List(x, y) = parameters // Number,Number
    var curState = state
      curState = AssignField[S](this0, TSprite.field_clip_left, Field[S](this0, TSprite.field_clip_left) + x)(curState, pp)
      curState = AssignField[S](this0, TSprite.field_clip_top, Field[S](this0, TSprite.field_clip_top) + y)(curState, pp)
      curState

    /** Moves sprite towards other sprite. */
    case "move towards" =>
      val List(other, fraction) = parameters // Sprite,Number
    val delta_x = (Field[S](other, TSprite.field_x) - Field[S](this0, TSprite.field_x)) * fraction
      val delta_y = (Field[S](other, TSprite.field_y) - Field[S](this0, TSprite.field_y)) * fraction
      var curState = state
      curState = AssignField[S](this0, TSprite.field_x, Field[S](this0, TSprite.field_x) + delta_x)(curState, pp)
      curState = AssignField[S](this0, TSprite.field_y, Field[S](this0, TSprite.field_y) + delta_y)(curState, pp)
      curState

    /** Set the handler invoked when the sprite is dragged */
    case "on drag" =>
      val List(dragged) = parameters // Vector_Action
    val newState = AssignField[S](this0, TSprite.field_drag_handler, dragged)
      New[S](TEvent_Binding)(newState, pp)

    /** Set the handler invoked when the sprite is swiped */
    case "on swipe" =>
      val List(swiped) = parameters // Vector_Action
    val newState = AssignField[S](this0, TSprite.field_swipe_handler, swiped)
      New[S](TEvent_Binding)(newState, pp)

    /** Set the handler invoked when the sprite is tapped */
    case "on tap" =>
      val List(tapped) = parameters // Position_Action
    val newState = AssignField[S](this0, TSprite.field_tap_handler, tapped)
      New[S](TEvent_Binding)(newState, pp)

    /** Set the handler invoked when the sprite is touched initially */
    case "on touch down" =>
      val List(touch_down) = parameters // Position_Action
    val newState = AssignField[S](this0, TSprite.field_touch_down_handler, touch_down)
      New[S](TEvent_Binding)(newState, pp)

    /** Set the handler invoked when the sprite touch is released */
    case "on touch up" =>
      val List(touch_up) = parameters // Position_Action
    val newState = AssignField[S](this0, TSprite.field_touch_up_handler, touch_up)
      New[S](TEvent_Binding)(newState, pp)

    case "on every frame" =>
      val List(act) = parameters // Action
    val newState = AssignField[S](this0, TSprite.field_every_frame_handler, act)
      New[S](TEvent_Binding)(newState, pp)

    /** Returns the subset of sprites in the given set that overlap with sprite. */
    case "overlap with" =>
      val List(sprites) = parameters // Sprite_Set
      Dummy[S](this0, method)
      Top[S](TSprite_Set)

    /** Do the sprites overlap */
    case "overlaps with" =>
      val List(other) = parameters // Sprite
      Dummy[S](this0, method)
      Top[S](TBoolean)

    /** Sets the acceleration in pixels/sec^2 */
    case "set acceleration" =>
      val List(vx, vy) = parameters // Number,Number
    val curState = AssignField[S](this0, TSprite.field_acceleration_x, vx)
      AssignField[S](this0, TSprite.field_acceleration_y, vy)(curState, pp)

    /** Sets the clipping area for an image sprite (if it is an image sprite) */
    case "set clip" =>
      val List(left, top, width, height) = parameters // Number,Number,Number,Number
    var curState = state
      curState = AssignField[S](this0, TSprite.field_clip_left, left)(curState, pp)
      curState = AssignField[S](this0, TSprite.field_clip_top, top)(curState, pp)
      curState = AssignField[S](this0, TSprite.field_clip_width, width)(curState, pp)
      curState = AssignField[S](this0, TSprite.field_clip_height, height)(curState, pp)
      curState

    /** Sets the position in pixels */
    case "set pos" =>
      val List(x, y) = parameters // Number,Number
      val curState = AssignField[S](this0, TSprite.field_x, x)
      AssignField[S](this0, TSprite.field_y, y)(curState, pp)

    /** Sets the speed in pixels/sec */
    case "set speed" =>
      val List(vx, vy) = parameters // Number,Number
    val curState = AssignField[S](this0, TSprite.field_speed_x, vx)
      AssignField[S](this0, TSprite.field_speed_y, vy)(curState, pp)

    /** Show sprite. */
    case "show" =>
      AssignField[S](this0, TSprite.field_is_visible, toRichExpression(1))

    /** Sets sprite speed direction towards other sprite with given magnitude. */
    case "speed towards" =>
      val List(other, magnitude) = parameters // Sprite,Number
      Dummy[S](this0, method)
      var curState = state
      curState = AssignField[S](this0, TSprite.field_speed_x, Valid(TNumber))(curState, pp)
      curState = AssignField[S](this0, TSprite.field_speed_y, Valid(TNumber))(curState, pp)
      curState

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}