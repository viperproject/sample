/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiMemberSemantics, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TBoard
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._

/**
 * @author Lucas Brutschy
 */
object TBoard extends Default_TBoard {

  /** Gets the background scene */
  lazy val field_background_scene = ApiField("background scene", TBoard_Background_Scene)

  /** The width in pixels */
  lazy val field_width = ApiField("width", TNumber)

  /** The height in pixels */
  lazy val field_height = ApiField("height", TNumber)

  /** The background color */
  lazy val field_background = ApiField("background", TColor)

  /** The background camera */
  lazy val field_background_camera = ApiField("background camera", TCamera)

  /** The background picture */
  lazy val field_background_picture = ApiField("background picture", TPicture)

  /** In debug mode, board displays speed and other info of sprites */
  lazy val field_debug_mode = ApiField("debug mode", TBoolean)

  /** The default friction for sprites to a fraction of speed loss between 0 and 1 */
  lazy val field_friction = ApiField("friction", TNumber)

  /** The uniform x acceleration for objects on the board to pixels/sec2 */
  lazy val field_gravity_x = ApiField("gravity x", TNumber)

  /** The uniform y acceleration for objects on the board to pixels/sec2 */
  lazy val field_gravity_y = ApiField("gravity y", TNumber)

  /** Gets a value indicating if the board is designed to be viewed in landscape mode */
  lazy val field_is_landscape = ApiField("is landscape", TBoolean)

  /** [**dbg**] Read the current position of virtual joystick */
  lazy val field_joystick = ApiField("joystick", TVector3)

  /** [**dbg**] joystick (default), wheel, balance, drag or swipe. */
  lazy val field_joystick_profile = ApiField("joystick profile", TVector3)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_swipe_handler = ApiField("swipe handler", TVector_Action)
  lazy val field_tap_handler = ApiField("tap handler", TPosition_Action)
  lazy val field_touch_down_handler = ApiField("touch down handler", TPosition_Action)
  lazy val field_touch_up_handler = ApiField("touch up handler", TPosition_Action)
  lazy val field_on_every_frame_handler = ApiField("every frame handler", TAction)


  /**
    * Copy is only used in foreach-loops to create a copy of the traversed collection
    * In the case of TBoard, it is automatically converted to a TSprite_Set
    */
  override lazy val member_copy = super.member_copy.copy(returnType = TSprite_Set,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
        var curState = state
        curState = New[S](TSprite_Set,initials = Map(
          TSprite_Set.field_entry -> Field[S](this0,TBoard.field_entry),
          TSprite_Set.field_count -> Field[S](this0,TBoard.field_count)
        ))(curState,pp)
        curState
      }
    })

  override def member_on_swipe =
    super.member_on_swipe.copy(semantics = AAction.EnableSemantics(TBoard.field_swipe_handler))

  override def member_on_tap =
    super.member_on_tap.copy(semantics = AAction.EnableSemantics(TBoard.field_tap_handler))

  override def member_on_touch_down =
    super.member_on_touch_down.copy(semantics = AAction.EnableSemantics(TBoard.field_touch_down_handler))

  override def member_on_touch_up =
    super.member_on_touch_up.copy(semantics = AAction.EnableSemantics(TBoard.field_touch_up_handler))

  override def member_on_every_frame =
    super.member_on_every_frame.copy(semantics = AAction.EnableSemantics(TBoard.field_on_every_frame_handler))

  override def mutedFields = super.mutedFields ++ List(
    field_width,
    field_height,
    field_debug_mode,
    field_friction,
    field_gravity_x,
    field_gravity_y,
    field_is_landscape
  )

  override def possibleFields = super.possibleFields ++ List(
    field_width,
    field_height,
    field_background,
    field_background_camera,
    field_background_picture,
    field_background_scene,
    field_debug_mode,
    field_friction,
    field_gravity_x,
    field_gravity_y,
    field_is_landscape,
    field_joystick,
    field_joystick_profile,
    field_swipe_handler,
    field_tap_handler,
    field_touch_down_handler,
    field_touch_up_handler,
    field_on_every_frame_handler
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** add an action that fires for every display frame. */
    case "add on every frame" =>
      val List(perform) = parameters // Action
      var curState = state
      curState = AssignField[S](this0, TBoard.field_on_every_frame_handler, perform)(curState,pp)
      curState = TAction.Enable[S](perform)(curState,pp)
      curState = New[S](TEvent_Binding)(curState,pp)
      curState

    // Clears the background camera
    case "clear background camera" =>
      AssignField(this0, TBoard.field_background_camera, Invalid(TCamera, "background camera may have been cleared"))

    // Clear the background picture
    case "clear background picture" =>
      AssignField(this0, TBoard.field_background_picture, Invalid(TPicture, "background picture may have been cleared"))

    // Clear all queued events related to this board
    case "clear events" =>
      Skip

    /** Stops and clears all the `every frame` timers */
    case "clear every frame timers" =>
      Skip

    // Create an anchor sprite.
    case "create anchor" =>
      val state1 = New[S](TSprite)
      val obj = state1.expr
      val state2 = super.forwardSemantics[S](this0, "add", List(obj), TNothing)(pp, state1)
      val state3 = state2.setExpression(obj)
      state3

    // Create walls around the board at the given distance.
    case "create boundary" =>
      val List(distance) = parameters
      Skip

    // Create a new ellipse sprite.
    case "create ellipse" =>
      val List(width, height) = parameters
      var curState = state
      curState = New[S](TSprite, Map(
        TSprite.field_width -> width,
        TSprite.field_height -> height
      ))(curState, pp)
      val obj = curState.expr
      curState = super.forwardSemantics[S](this0, "add", List(obj), TNothing)(pp, curState)
      curState = curState.setExpression(obj)
      curState

    // Create a line obstacle with given start point, and given extent. Elasticity is 0 for sticky, 1 for complete bounce.
    case "create obstacle" =>
      val List(x, y, x_segment, y_segment, elasticity) = parameters
      if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](x, 0, Field[S](this0, TBoard.field_width), "create obstacle", "x")
        CheckInRangeInclusive[S](y, 0, Field[S](this0, TBoard.field_height), "create obstacle", "y")
        CheckInRangeInclusive[S](x_segment, 0, Field[S](this0, TBoard.field_width) - x, "create obstacle", "x_segment")
        CheckInRangeInclusive[S](y_segment, 0, Field[S](this0, TBoard.field_width) - y, "create obstacle", "y_segment")
        CheckInRangeInclusive[S](elasticity, 0, 1, "create obstacle", "elasticity")
      }
      New[S](TObstacle)

    // Create a new picture sprite.
    case "create picture" =>
      val List(picture) = parameters
      val state1 = New[S](TSprite, Map(
        TSprite.field_width -> Field[S](picture, TPicture.field_width),
        TSprite.field_height -> Field[S](picture, TPicture.field_height),
        TSprite.field_picture -> picture
      ))
      val obj = state1.expr
      val state2 = super.forwardSemantics[S](this0, "add", List(obj), TNothing)(pp, state1)
      val state3 = state2.setExpression(obj)
      state3

    // Create a new rectangle sprite.
    case "create rectangle" =>
      val List(width, height) = parameters
      val state1 = New[S](TSprite, Map(
        TSprite.field_width -> width,
        TSprite.field_height -> height
      ))
      val obj = state1.expr
      val state2 = super.forwardSemantics[S](this0, "add", List(obj), TNothing)(pp, state1)
      val state3 = state2.setExpression(obj)
      state3

    // Create a spring between the two sprites.
    case "create spring" =>
      val List(sprite1, sprite2, stiffness) = parameters
      New[S](TSpring)

    // Create a new collection for sprites.
    case "create sprite set" =>
      New[S](TSprite_Set)

    /** Create a new sprite sheet. */
    case "create sprite sheet" =>
      val List(picture) = parameters // Picture
      New[S](TSprite_Sheet, initials = Map(TSprite_Sheet.field_picture -> picture))

    // Create a new text sprite.
    case "create text" =>
      val List(width, height, fontSize, text) = parameters
      val state1 = New[S](TSprite, Map(
        TSprite.field_text -> text,
        TSprite.field_width -> width,
        TSprite.field_height -> height
      ))
      val obj = state1.expr
      val state2 = super.forwardSemantics[S](this0, "add", List(obj), TNothing)(pp, state1)
      val state3 = state2.setExpression(obj)
      state3

    // Update positions of sprites on board.
    case "evolve" =>
      If[S](Count[S](this0) > 0, Then = {
        x: S =>
          var curState = x
          curState = CallApi[S](AllValues[S](this0), "set pos", List(Valid(TNumber), Valid(TNumber)), TNothing)(curState, pp)
          curState = CallApi[S](AllValues[S](this0), "set speed", List(Valid(TNumber), Valid(TNumber)), TNothing)(curState, pp)
          curState = CallApi[S](AllValues[S](this0), "set acceleration", List(Valid(TNumber), Valid(TNumber)), TNothing)(curState, pp)
          curState
      }, Else = {
        x: S => x
      })(state, pp)

    /** create a timer that fires for every display frame. */
    case "frame timer" =>
      Top[S](TTimer)

    // Sets the default friction for sprites to a fraction of speed loss between 0 and 1
    case "set friction" =>
      val List(friction) = parameters
      if (TouchAnalysisParameters.get.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](friction, 0, 1, "set friction", "friction")
      }
      AssignField(this0, TBoard.field_friction, friction)

    // Sets the uniform acceleration vector for objects on the board to pixels/sec^2
    case "set gravity" =>
      val List(gravity_x, gravity_y) = parameters
      val s = AssignField(this0, TBoard.field_gravity_x, gravity_x)
      AssignField(this0, TBoard.field_gravity_y, gravity_y)(s, pp)

    // Current touch point
    case "touch current" =>
      New(TVector3, Map(
        TVector3.field_x -> (0 ndToIncl Field[S](this0, TBoard.field_width)),
        TVector3.field_y -> (0 ndToIncl Field[S](this0, TBoard.field_height)),
        TVector3.field_z -> 0
      ))

    // Last touch end point
    case "touch end" =>
      New(TVector3, Map(
        TVector3.field_x -> (0 ndToIncl Field[S](this0, TBoard.field_width)),
        TVector3.field_y -> (0 ndToIncl Field[S](this0, TBoard.field_height)),
        TVector3.field_z -> 0
      ))

    // Last touch start point
    case "touch start" =>
      New(TVector3, Map(
        TVector3.field_x -> (0 ndToIncl Field[S](this0, TBoard.field_width)),
        TVector3.field_y -> (0 ndToIncl Field[S](this0, TBoard.field_height)),
        TVector3.field_z -> 0
      ))

    // Final touch velocity after touch ended
    case "touch velocity" =>
      New(TVector3, Map(
        TVector3.field_x -> Valid(TNumber),
        TVector3.field_y -> Valid(TNumber),
        TVector3.field_z -> 0
      ))

    // True if board is touched
    case "touched" =>
      Return[S](True, False)

    // Make updates visible.
    case "update on wall" =>
      state

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

}
