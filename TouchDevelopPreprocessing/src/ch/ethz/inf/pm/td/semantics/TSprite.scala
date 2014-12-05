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
import ch.ethz.inf.pm.td.defsemantics.Default_TSprite
import ch.ethz.inf.pm.td.parser.TypeName

/**
 * Specifies the abstract semantics of Sprite
 *
 * A sprite
 *
 * @author Lucas Brutschy
 */


object TSprite extends Default_TSprite {

  lazy val field_acceleration_x = ApiField("acceleration x", TNumber)
  // Gets the acceleration along x in pixels/sec^2
  lazy val field_acceleration_y = ApiField("acceleration y", TNumber)
  // Gets the acceleration along y in pixels/sec^2
  lazy val field_angle = ApiField("angle", TNumber)
  // Gets the angle of the sprite in degrees
  lazy val field_angular_speed = ApiField("angular speed", TNumber)
  // Gets the rotation speed in degrees/sec
  lazy val field_color = ApiField("color", TColor)
  // Returns the sprite color.
  lazy val field_elasticity = ApiField("elasticity", TNumber)
  // Gets the sprite elasticity as a fraction of speed preservation per bounce (0-1)
  lazy val field_friction = ApiField("friction", TNumber)
  // Gets the fraction of speed loss between 0 and 1
  lazy val field_height = ApiField("height", TNumber)
  // Gets the height in pixels
  lazy val field_is_deleted = new ApiField("is deleted", TBoolean, ExpressionInitializer(False(null)))
  // Returns false if sprite is not deleted
  lazy val field_is_visible = ApiField("is visible", TBoolean)
  // Returns true if sprite is not hidden
  lazy val field_location = ApiField("location", TLocation)
  // Gets the geo location assigned to the sprite
  lazy val field_mass = ApiField("mass", TNumber)
  // Gets the mass
  lazy val field_opacity = ApiField("opacity", TNumber)
  // Gets the opacity (between 0 transparent and 1 opaque)
  lazy val field_picture = ApiField("picture", TPicture)
  lazy val field_speed_x = ApiField("speed x", TNumber)
  // Gets the speed along x in pixels/sec
  lazy val field_speed_y = ApiField("speed y", TNumber)
  // Gets the speed along y in pixels/sec
  lazy val field_text = ApiField("text", TString)
  // The text on a text sprite (if it is a text sprite)
  lazy val field_width = ApiField("width", TNumber)
  // Gets the width in pixels
  lazy val field_x = ApiField("x", TNumber)
  // Gets the x position in pixels
  lazy val field_y = ApiField("y", TNumber)
  // Gets the y position in pixels
  lazy val field_z_index = ApiField("z index", TNumber) // Gets the z-index of the sprite

  lazy val field_clip_left = ApiField("clip left", TNumber)
  lazy val field_clip_top = ApiField("clip top", TNumber)
  lazy val field_clip_width = ApiField("clip width", TNumber)
  lazy val field_clip_height = ApiField("clip height", TNumber)

  /** Gets the scaling applied when rendering the sprite. This scaling does not influence the bounding box. */
  lazy val field_scale = ApiField("scale", TNumber)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_drag_handler = ApiField("drag handler", TVector_Action)
  lazy val field_swipe_handler = ApiField("swipe handler", TVector_Action)
  lazy val field_tap_handler = ApiField("tap handler", TPosition_Action)
  lazy val field_touch_down_handler = ApiField("touch down handler", TPosition_Action)
  lazy val field_touch_up_handler = ApiField("touch up handler", TPosition_Action)
  lazy val field_every_frame_handler = ApiField("every frame handler", TAction)

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