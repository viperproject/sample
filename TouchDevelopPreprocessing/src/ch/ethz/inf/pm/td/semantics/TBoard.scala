package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{TouchType, TouchCollection}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

/**
 * @author Lucas Brutschy
 */
object TBoard {

  /**  The width in pixels */
  val field_width = new TouchField("width", TNumber.typName)

  /**  The height in pixels */
  val field_height = new TouchField("height", TNumber.typName)

  /**  The background color */
  val field_background = new TouchField("background",TColor.typName)

  /**  The background camera */
  val field_background_camera = new TouchField("background camera",TCamera.typName)

  /**  The background picture */
  val field_background_picture = new TouchField("background picture",TPicture.typName)

  /**  In debug mode, board displays speed and other info of sprites */
  val field_debug_mode = new TouchField("debug mode",TBoolean.typName)

  /**  The default friction for sprites to a fraction of speed loss between 0 and 1 */
  val field_friction = new TouchField("friction",TNumber.typName)

  /**  The uniform x acceleration for objects on the board to pixels/sec2 */
  val field_gravity_x = new TouchField("gravity x",TNumber.typName)

  /**  The uniform y acceleration for objects on the board to pixels/sec2 */
  val field_gravity_y = new TouchField("gravity y",TNumber.typName)

  /** Gets a value indicating if the board is designed to be viewed in landscape mode */
  val field_is_landscape = new TouchField("is landscape",TBoolean.typName)

  /** [**dbg**] Read the current position of virtual joystick */
  val field_joystick = new TouchField("joystick",TVector3.typName) // TODO: What is this and does it change?

  /** [**dbg**] joystick (default), wheel, balance, drag or swipe. */
  val field_joystick_profile = new TouchField("set joystick profile",TVector3.typName)

  /** String name of the type */
  val typName = "Board"

  val typ = TouchCollection(typName,"Number","Sprite",List(
    field_width,
    field_height,
    field_background,
    field_background_camera,
    field_background_picture,
    field_debug_mode,
    field_friction,
    field_gravity_x,
    field_gravity_y,
    field_is_landscape,
    field_joystick,
    field_joystick_profile
  ))

}

class TBoard extends AMutable_Collection {

  def getTyp = TBoard.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String,parameters:List[ExpressionSet],returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    // Clears the background camera
    case "clear background camera" =>
      AssignField(this0,TBoard.field_background_camera,Invalid(TCamera.typ))

    // Clear the background picture
    case "clear background picture" =>
      AssignField(this0,TBoard.field_background_picture,Invalid(TPicture.typ))

    // Clear all queued events related to this board
    case "clear events" =>
      Skip

    // Create an anchor sprite.
    case "create anchor" =>
      val state1 = New[S](TSprite.typ)
      val obj = state1.getExpression()
      val state2 = super.forwardSemantics[S](this0,"add",List(obj),TNothing.typ)(pp,state1)
      val state3 = state2.setExpression(obj)
      state3

    // Create walls around the board at the given distance.
    case "create boundary" =>
      val List(distance) = parameters
      Skip

    // Create a new ellipse sprite.
    case "create ellipse" =>
      val List(width,height) = parameters
      var curState = state
      curState = New[S](TSprite.typ,Map(
        TSprite.field_width -> width,
        TSprite.field_height -> height
      ))(curState,pp)
      val obj = curState.getExpression()
      curState = super.forwardSemantics[S](this0,"add",List(obj),TNothing.typ)(pp,curState)
      curState = curState.setExpression(obj)
      curState

    // Create a line obstacle with given start point, and given extent. Elasticity is 0 for sticky, 1 for complete bounce.
    case "create obstacle" =>
      val List(x, y, x_segment, y_segment, elasticity) = parameters
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](x,0,Field[S](this0,TBoard.field_width),"create obstacle","x")
        CheckInRangeInclusive[S](y,0,Field[S](this0,TBoard.field_height),"create obstacle","y")
        CheckInRangeInclusive[S](x_segment,0,Field[S](this0,TBoard.field_width)-x,"create obstacle","x_segment")
        CheckInRangeInclusive[S](y_segment,0,Field[S](this0,TBoard.field_width)-y,"create obstacle","y_segment")
        CheckInRangeInclusive[S](elasticity,0,1,"create obstacle","elasticity")
      }
      Skip

    // Create a new picture sprite.
    case "create picture" =>
      val List(picture) = parameters
      val state1 = New[S](TSprite.typ,Map(
        TSprite.field_width -> Field[S](picture,TPicture.field_width),
        TSprite.field_height -> Field[S](picture,TPicture.field_height),
        TSprite.field_picture -> picture
      ))
      val obj = state1.getExpression()
      val state2 = super.forwardSemantics[S](this0,"add",List(obj),TNothing.typ)(pp,state1)
      val state3 = state2.setExpression(obj)
      state3

    // Create a new rectangle sprite.
    case "create rectangle" =>
      val List(width,height) = parameters
      val state1 = New[S](TSprite.typ,Map(
        TSprite.field_width -> width,
        TSprite.field_height -> height
      ))
      val obj = state1.getExpression()
      val state2 = super.forwardSemantics[S](this0,"add",List(obj),TNothing.typ)(pp,state1)
      val state3 = state2.setExpression(obj)
      state3

    // Create a spring between the two sprites.
    case "create spring" =>
      val List(sprite1, sprite2, stiffness) = parameters
      Skip

    // Create a new collection for sprites.
    case "create sprite set" =>
      New[S](TSprite_Set.typ)

    // Create a new text sprite.
    case "create text" =>
      val List(width,height,fontSize,text) = parameters
      val state1 = New[S](TSprite.typ,Map(
        TSprite.field_text -> text,
        TSprite.field_width -> width,
        TSprite.field_height -> height
      ))
      val obj = state1.getExpression()
      val state2 = super.forwardSemantics[S](this0,"add",List(obj),TNothing.typ)(pp,state1)
      val state3 = state2.setExpression(obj)
      state3

    // Update positions of sprites on board.
    case "evolve" =>
      Skip // TODO: Modify sprites (or unified sprite)

    // Sets the default friction for sprites to a fraction of speed loss between 0 and 1
    case "set friction" =>
      val List(friction) = parameters
      if(TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](friction,0,1,"set friction","friction")
      }
      AssignField(this0,TBoard.field_friction,friction)

    // Sets the uniform acceleration vector for objects on the board to pixels/sec^2
    case "set gravity" =>
      val List(gravity_x,gravity_y) = parameters
      val s = AssignField(this0,TBoard.field_gravity_x,gravity_x)
      AssignField(this0,TBoard.field_gravity_y,gravity_y)(s,pp)

    // Current touch point
    case "touch current" =>
      New(TVector3.typ,Map(
        TVector3.field_x -> (0 ndTo Field[S](this0,TBoard.field_width)),
        TVector3.field_y -> (0 ndTo Field[S](this0,TBoard.field_height)),
        TVector3.field_z -> 0
      ))

    // Last touch end point
    case "touch end" =>
      New(TVector3.typ,Map(
        TVector3.field_x -> (0 ndTo Field[S](this0,TBoard.field_width)),
        TVector3.field_y -> (0 ndTo Field[S](this0,TBoard.field_height)),
        TVector3.field_z -> 0
      ))

    // Last touch start point
    case "touch start" =>
      New(TVector3.typ,Map(
        TVector3.field_x -> (0 ndTo Field[S](this0,TBoard.field_width)),
        TVector3.field_y -> (0 ndTo Field[S](this0,TBoard.field_height)),
        TVector3.field_z -> 0
      ))

    // Final touch velocity after touch ended
    case "touch velocity" =>
      New(TVector3.typ,Map(
        TVector3.field_x -> Valid(TNumber.typ),
        TVector3.field_y -> Valid(TNumber.typ),
        TVector3.field_z -> 0
      ))

    // True if board is touched
    case "touched" =>
      Return[S](True,False)

    // Make updates visible.
    case "update on wall" =>
      state // TODO: Check if reference exists in wall?

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}
