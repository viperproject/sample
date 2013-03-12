package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchCollection
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * @author Lucas Brutschy
 */
object TBoard {

  /**  The width in pixels */
  val field_width = new TouchField("width", TNumber.typ)

  /**  The height in pixels */
  val field_height = new TouchField("height", TNumber.typ)

  /**  The background color */
  val field_background = new TouchField("background",TColor.typ)

  /**  The background camera */
  val field_background_camera = new TouchField("background_camera",TCamera.typ)

  /**  The background picture */
  val field_background_picture = new TouchField("background_picture",TPicture.typ)

  /**  In debug mode, board displays speed and other info of sprites */
  val field_debug_mode = new TouchField("debug_mode",TBoolean.typ)

  /**  The default friction for sprites to a fraction of speed loss between 0 and 1 */
  val field_friction = new TouchField("friction",TNumber.typ)

  /**  The uniform x acceleration for objects on the board to pixels/sec2 */
  val field_gravity_x = new TouchField("gravity_x",TNumber.typ)

  /**  The uniform y acceleration for objects on the board to pixels/sec2 */
  val field_gravity_y = new TouchField("gravity_y",TNumber.typ)

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
    field_gravity_y
  ))

}

class TBoard extends ACollection {

  def getTyp = TBoard.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String,parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    // Clears the background camera
    case "clear_background_camera" =>
      AssignField(this0,TBoard.field_background_camera,Invalid(TCamera.typ))

    // Clear the background picture
    case "clear_background_picture" =>
      AssignField(this0,TBoard.field_background_picture,Invalid(TPicture.typ))

    // Clear all queued events related to this board
    case "clear_events" =>
      Skip

    // Create an anchor sprite.
    case "create_anchor" =>
      New(TSprite.typ) // TODO: Initialize stuff

    // Create walls around the board at the given distance.
    case "create_boundary" =>
      val List(distance) = parameters
      Skip // TODO: What?

    // Create a new ellipse sprite.
    case "create_ellipse" =>
      val List(width,height) = parameters
      New(TSprite.typ) // TODO: Initialize stuff

    // Create a line obstacle with given start point, and given extent. Elasticity is 0 for sticky, 1 for complete bounce.
    case "create_obstacle" =>
      val List(x, y, x_segment, y_segment, elasticity) = parameters
      // Range check x
      // Range check y
      // Range check x_segment
      // Range check y_segment
      // Elasticity
      // TODO: What to do with this?
      Skip

    // Create a new picture sprite.
    case "create_picture" =>
      val List(picture) = parameters
      New(TSprite.typ,Map(
        TSprite.field_width -> Field[S](picture,TPicture.field_width),
        TSprite.field_height -> Field[S](picture,TPicture.field_height),
        TSprite.field_picture -> picture
      ))

    // Create a new rectangle sprite.
    case "create_rectangle" =>
      val List(width,height) = parameters
      New(TSprite.typ,Map(
        TSprite.field_width -> width,
        TSprite.field_height -> height
      ))

    // Create a spring between the two sprites.
    case "create_spring" =>
      val List(sprite1, sprite2, stiffness) = parameters
      Skip // TODO: What to do with this? Return Nothing. Check range of stiffness?

    // Create a new collection for sprites.
    case "create_sprite_set" =>
      New[S](TSprite_Set.typ)

    // Create a new text sprite.
    case "create_text" =>
      val List(width,height,fontSize,text) = parameters
      New(TSprite.typ,Map(
        TSprite.field_text -> text,
        TSprite.field_width -> width,
        TSprite.field_height -> height
      ))

    // Update positions of sprites on board.
    case "evolve" =>
      Skip // TODO: Modify sprites (or unified sprite)

    // Sets the background color
    case "set_background" =>
      val List(background) = parameters
      AssignField(this0,TBoard.field_background,background)

    // Sets the background camera
    case "set_background_camera" =>
      val List(background_camera) = parameters
      AssignField(this0,TBoard.field_background_camera,background_camera)

    // Sets the background picture
    case "set_background_picture" =>
      val List(background_picture) = parameters
      AssignField(this0,TBoard.field_background_picture,background_picture)

    // In debug mode, board displays speed and other info of sprites
    case "set_debug_mode" =>
      val List(debug_mode) = parameters
      AssignField(this0,TBoard.field_debug_mode,debug_mode)

    // Sets the default friction for sprites to a fraction of speed loss between 0 and 1
    case "set_friction" =>
      val List(friction) = parameters
      // TODO: Check friction
      AssignField(this0,TBoard.field_friction,friction)

    // Sets the uniform acceleration vector for objects on the board to pixels/sec^2
    case "set_gravity" =>
      val List(gravity_x,gravity_y) = parameters
      AssignField(this0,TBoard.field_gravity_x,gravity_x) // FIXME: Effect lost
      AssignField(this0,TBoard.field_gravity_y,gravity_y)

    // Current touch point
    case "touch_current" =>
      New(TVector3.typ,Map(
        TVector3.field_x -> (0 ndTo Field[S](this0,TBoard.field_width)),
        TVector3.field_y -> (0 ndTo Field[S](this0,TBoard.field_height)),
        TVector3.field_z -> 0
      ))

    // Last touch end point
    case "touch_end" =>
      New(TVector3.typ,Map(
        TVector3.field_x -> (0 ndTo Field[S](this0,TBoard.field_width)),
        TVector3.field_y -> (0 ndTo Field[S](this0,TBoard.field_height)),
        TVector3.field_z -> 0
      ))
      // TODO: Can this be invalid?

    // Last touch start point
    case "touch_start" =>
      New(TVector3.typ,Map(
        TVector3.field_x -> (0 ndTo Field[S](this0,TBoard.field_width)),
        TVector3.field_y -> (0 ndTo Field[S](this0,TBoard.field_height)),
        TVector3.field_z -> 0
      ))
      // TODO: Can this be invalid?

    // Final touch velocity after touch ended
    case "touch_velocity" =>
      New(TVector3.typ,Map(
        TVector3.field_x -> Valid(TNumber.typ),
        TVector3.field_y -> Valid(TNumber.typ),
        TVector3.field_z -> 0
      ))
      // TODO: Can this be invalid?

    // True if board is touched
    case "touched" =>
      Return[S](True,False)

    // Make updates visible.
    case "update_on_wall" =>
      state // TODO: Check if reference exists in wall?

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}
