package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{TouchField, RichNativeSemantics, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * @author Lucas Brutschy
 */
object TBoard extends AMutable_Collection {

  /** Gets the background scene */
  lazy val field_background_scene = new TouchField("background scene", TBoard_Background_Scene.typeName)

  /** The width in pixels */
  lazy val field_width = new TouchField("width", TNumber.typeName)

  /** The height in pixels */
  lazy val field_height = new TouchField("height", TNumber.typeName)

  /** The background color */
  lazy val field_background = new TouchField("background", TColor.typeName)

  /** The background camera */
  lazy val field_background_camera = new TouchField("background camera", TCamera.typeName)

  /** The background picture */
  lazy val field_background_picture = new TouchField("background picture", TPicture.typeName)

  /** In debug mode, board displays speed and other info of sprites */
  lazy val field_debug_mode = new TouchField("debug mode", TBoolean.typeName)

  /** The default friction for sprites to a fraction of speed loss between 0 and 1 */
  lazy val field_friction = new TouchField("friction", TNumber.typeName)

  /** The uniform x acceleration for objects on the board to pixels/sec2 */
  lazy val field_gravity_x = new TouchField("gravity x", TNumber.typeName)

  /** The uniform y acceleration for objects on the board to pixels/sec2 */
  lazy val field_gravity_y = new TouchField("gravity y", TNumber.typeName)

  /** Gets a value indicating if the board is designed to be viewed in landscape mode */
  lazy val field_is_landscape = new TouchField("is landscape", TBoolean.typeName)

  /** [**dbg**] Read the current position of virtual joystick */
  lazy val field_joystick = new TouchField("joystick", TVector3.typeName)

  /** [**dbg**] joystick (default), wheel, balance, drag or swipe. */
  lazy val field_joystick_profile = new TouchField("joystick profile", TVector3.typeName)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_swipe_handler = new TouchField("swipe handler", TVector_Action.typeName)
  lazy val field_tap_handler = new TouchField("tap handler", TPosition_Action.typeName)
  lazy val field_touch_down_handler = new TouchField("touch down handler", TPosition_Action.typeName)
  lazy val field_touch_up_handler = new TouchField("touch up handler", TPosition_Action.typeName)

  /** String name of the type */
  lazy val typeName = TypeName("Board")

  def keyTypeName = TNumber.typeName

  def valueTypeName = TSprite.typeName

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
    field_touch_up_handler
  )

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** add an action that fires for every display frame. */
    case "add on every frame" =>
      val List(perform) = parameters // Action
      New[S](TEvent_Binding)

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
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
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
      If[S](CollectionSize[S](this0) > 0, Then = {
        x: S =>
          var curState = x
          curState = CallApi[S](CollectionSummary[S](this0), "set pos", List(Valid(TNumber), Valid(TNumber)), TNothing)(curState, pp)
          curState = CallApi[S](CollectionSummary[S](this0), "set speed", List(Valid(TNumber), Valid(TNumber)), TNothing)(curState, pp)
          curState = CallApi[S](CollectionSummary[S](this0), "set acceleration", List(Valid(TNumber), Valid(TNumber)), TNothing)(curState, pp)
          curState
      }, Else = {
        x: S => x
      })(state, pp)

    /** create a timer that fires for every display frame. */
    case "frame timer" =>
      Top[S](TTimer)

    /** set the handler that is invoked when the board is swiped */
    case "on swipe" =>
      val List(swiped) = parameters // Vector_Action
    val newState = AssignField[S](this0, TBoard.field_swipe_handler, swiped)
      New[S](TEvent_Binding)(newState, pp)

    /** set the handler that is invoked when the board is tapped */
    case "on tap" =>
      val List(tapped) = parameters // Position_Action
    val newState = AssignField[S](this0, TBoard.field_tap_handler, tapped)
      New[S](TEvent_Binding)(newState, pp)

    /** set the handler that is invoked when the board is touched */
    case "on touch down" =>
      val List(touch_down) = parameters // Position_Action
    val newState = AssignField[S](this0, TBoard.field_touch_down_handler, touch_down)
      New[S](TEvent_Binding)(newState, pp)

    /** set the handler that is invoked when the board touch is released */
    case "on touch up" =>
      val List(touch_up) = parameters // Position_Action
    val newState = AssignField[S](this0, TBoard.field_touch_up_handler, touch_up)
      New[S](TEvent_Binding)(newState, pp)

    // Sets the default friction for sprites to a fraction of speed loss between 0 and 1
    case "set friction" =>
      val List(friction) = parameters
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
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
        TVector3.field_x -> (0 ndTo Field[S](this0, TBoard.field_width)),
        TVector3.field_y -> (0 ndTo Field[S](this0, TBoard.field_height)),
        TVector3.field_z -> 0
      ))

    // Last touch end point
    case "touch end" =>
      New(TVector3, Map(
        TVector3.field_x -> (0 ndTo Field[S](this0, TBoard.field_width)),
        TVector3.field_y -> (0 ndTo Field[S](this0, TBoard.field_height)),
        TVector3.field_z -> 0
      ))

    // Last touch start point
    case "touch start" =>
      New(TVector3, Map(
        TVector3.field_x -> (0 ndTo Field[S](this0, TBoard.field_width)),
        TVector3.field_y -> (0 ndTo Field[S](this0, TBoard.field_height)),
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
      state // TODO: Check if reference exists in wall?

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

}
