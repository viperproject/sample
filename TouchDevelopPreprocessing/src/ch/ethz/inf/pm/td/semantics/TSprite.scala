package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.td.analysis.interpreter._
import ch.ethz.inf.pm.td.semantics.ExpressionInitializer
import ch.ethz.inf.pm.td.analysis.interpreter.RefV
import ch.ethz.inf.pm.td.analysis.interpreter.NumberV

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TSprite {

  val field_acceleration_x = new TouchField("acceleration x", TNumber.typName)
  // Gets the acceleration along x in pixels/sec^2
  val field_acceleration_y = new TouchField("acceleration y", TNumber.typName)
  // Gets the acceleration along y in pixels/sec^2
  val field_angle = new TouchField("angle", TNumber.typName)
  // Gets the angle of the sprite in degrees
  val field_angular_speed = new TouchField("angular speed", TNumber.typName)
  // Gets the rotation speed in degrees/sec
  val field_color = new TouchField("color", TColor.typName)
  // Returns the sprite color.
  val field_elasticity = new TouchField("elasticity", TNumber.typName)
  // Gets the sprite elasticity as a fraction of speed preservation per bounce (0-1)
  val field_friction = new TouchField("friction", TNumber.typName)
  // Gets the fraction of speed loss between 0 and 1
  val field_height = new TouchField("height", TNumber.typName)
  // Gets the height in pixels
  val field_is_deleted = new TouchField("is deleted", TBoolean.typName, ExpressionInitializer(False(null)))
  // Returns false if sprite is not deleted
  val field_is_visible = new TouchField("is visible", TBoolean.typName)
  // Returns true if sprite is not hidden
  val field_location = new TouchField("location", TLocation.typName)
  // Gets the geo location assigned to the sprite
  val field_mass = new TouchField("mass", TNumber.typName)
  // Gets the mass
  val field_opacity = new TouchField("opacity", TNumber.typName)
  // Gets the opacity (between 0 transparent and 1 opaque)
  val field_picture = new TouchField("picture", TPicture.typName)
  val field_speed_x = new TouchField("speed x", TNumber.typName)
  // Gets the speed along x in pixels/sec
  val field_speed_y = new TouchField("speed y", TNumber.typName)
  // Gets the speed along y in pixels/sec
  val field_text = new TouchField("text", TString.typName)
  // The text on a text sprite (if it is a text sprite)
  val field_width = new TouchField("width", TNumber.typName)
  // Gets the width in pixels
  val field_x = new TouchField("x", TNumber.typName)
  // Gets the x position in pixels
  val field_y = new TouchField("y", TNumber.typName)
  // Gets the y position in pixels
  val field_z_index = new TouchField("z index", TNumber.typName) // Gets the z-index of the sprite

  val field_clip_left = new TouchField("clip left", TNumber.typName)
  val field_clip_top = new TouchField("clip top", TNumber.typName)
  val field_clip_width = new TouchField("clip width", TNumber.typName)
  val field_clip_height = new TouchField("clip height", TNumber.typName)

  /** Gets the board associated with this sprite */
  val field_board = new TouchField("board", TBoard.typName, default = InvalidInitializer, topDefault = InvalidInitializer)

  /** Gets the scaling applied when rendering the sprite. This scaling does not influence the bounding box. */
  val field_scale = new TouchField("scale", TNumber.typName)

  /** PRIVATE HANDLER FIELDS */
  val field_drag_handler = new TouchField("drag handler", TVector_Action.typName)
  val field_swipe_handler = new TouchField("swipe handler", TVector_Action.typName)
  val field_tap_handler = new TouchField("tap handler", TPosition_Action.typName)
  val field_touch_down_handler = new TouchField("touch down handler", TPosition_Action.typName)
  val field_touch_up_handler = new TouchField("touch up handler", TPosition_Action.typName)
  val field_every_frame_handler = new TouchField("every frame handler", TAction.typName)

  val typName = "Sprite"
  val typ = DefaultTouchType(typName, isSingleton = false, fields = List(
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
    field_every_frame_handler,
    field_board
  ))

}

class TSprite extends AAny {

  def getTyp = TSprite.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)(implicit pp: ProgramPoint, state: S): S = method match {

    /** Starts a new tween animation. */
    case "create animation" =>
      New[S](TSprite_Animation.typ)

    /** Delete sprite. */
    case "delete" =>
      AssignField[S](this0, TSprite.field_is_deleted, True)

    /** Are these the same sprite */
    case "equals" =>
      val List(other) = parameters // Sprite
      Dummy[S](this0, method)
      Top[S](TBoolean.typ)

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
      New[S](TEvent_Binding.typ)(newState, pp)

    /** Set the handler invoked when the sprite is swiped */
    case "on swipe" =>
      val List(swiped) = parameters // Vector_Action
    val newState = AssignField[S](this0, TSprite.field_swipe_handler, swiped)
      New[S](TEvent_Binding.typ)(newState, pp)

    /** Set the handler invoked when the sprite is tapped */
    case "on tap" =>
      val List(tapped) = parameters // Position_Action
    val newState = AssignField[S](this0, TSprite.field_tap_handler, tapped)
      New[S](TEvent_Binding.typ)(newState, pp)

    /** Set the handler invoked when the sprite is touched initially */
    case "on touch down" =>
      val List(touch_down) = parameters // Position_Action
    val newState = AssignField[S](this0, TSprite.field_touch_down_handler, touch_down)
      New[S](TEvent_Binding.typ)(newState, pp)

    /** Set the handler invoked when the sprite touch is released */
    case "on touch up" =>
      val List(touch_up) = parameters // Position_Action
    val newState = AssignField[S](this0, TSprite.field_touch_up_handler, touch_up)
      New[S](TEvent_Binding.typ)(newState, pp)

    case "on every frame" =>
      val List(act) = parameters // Action
    val newState = AssignField[S](this0, TSprite.field_every_frame_handler, act)
      New[S](TEvent_Binding.typ)(newState, pp)

    /** Returns the subset of sprites in the given set that overlap with sprite. */
    case "overlap with" =>
      val List(sprites) = parameters // Sprite_Set
      Dummy[S](this0, method)
      Top[S](TSprite_Set.typ)

    /** Do the sprites overlap */
    case "overlaps with" =>
      val List(other) = parameters // Sprite
      Dummy[S](this0, method)
      Top[S](TBoolean.typ)

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

      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckLowerBound[S](x, 0, "set pos", "x")
        CheckLowerBound[S](y, 0, "set pos", "y")

        val boardExpr = Field[S](this0, TSprite.field_board)
        CheckStrictUpperBound[S](x, Field[S](boardExpr,
          TBoard.field_width), "set pos", "x")
        CheckStrictUpperBound[S](y, Field[S](boardExpr,
          TBoard.field_height), "set pos", "y")
      }

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
      curState = AssignField[S](this0, TSprite.field_speed_x, Valid(TNumber.typ))(curState, pp)
      curState = AssignField[S](this0, TSprite.field_speed_y, Valid(TNumber.typ))(curState, pp)
      curState

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

  override def concreteSemantics(this0: TouchValue, method: String, params: List[TouchValue], interpreter: ConcreteInterpreter, pp: ProgramPoint): TouchValue = method match {
    case "set pos" =>
      (this0, params) match {
        case (sref: RefV, List(x: NumberV, y: NumberV)) =>
          val state = interpreter.state

          val board = state.getField(sref, TSprite.field_board.getName).asInstanceOf[RefV]
          val NumberV(width) = state.getField(board, TBoard.field_width.getName)
          val NumberV(height) = state.getField(board, TBoard.field_height.getName)

          interpreter.assertE(x.v >= 0)(pp)
          interpreter.assertE(y.v >= 0)(pp)
          interpreter.assertE(x.v < width)(pp)
          interpreter.assertE(y.v < height)(pp)

          state.setField(sref, TSprite.field_x.getName, x)
          state.setField(sref, TSprite.field_y.getName, y)
          UnitV
      }
  }
}