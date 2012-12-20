package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichExpression._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TSprite {

  val field_acceleration_x = new TouchField("acceleration_x",TNumber.typ)	// Gets the acceleration along x in pixels/sec^2
  val field_acceleration_y = new TouchField("acceleration_y",TNumber.typ)	// Gets the acceleration along y in pixels/sec^2
  val field_angle = new TouchField("angle",TNumber.typ)	          // Gets the angle of the sprite in degrees
  val field_angular_speed = new TouchField("angular_speed",TNumber.typ) 	// Gets the rotation speed in degrees/sec
  val field_color = new TouchField("color",TColor.typ)	          // Returns the sprite color.
  val field_elasticity = new TouchField("elasticity",TNumber.typ)	    // Gets the sprite elasticity as a fraction of speed preservation per bounce (0-1)
  val field_friction = new TouchField("friction",TNumber.typ)	      // Gets the fraction of speed loss between 0 and 1
  val field_height = new TouchField("height",TNumber.typ)	        // Gets the height in pixels
  val field_is_visible = new TouchField("is_visible",TBoolean.typ)	    // Returns true if sprite is not hidden
  val field_location = new TouchField("location",TLocation.typ)	    // Gets the geo location assigned to the sprite
  val field_mass = new TouchField("mass",TNumber.typ)	          // Gets the mass
  val field_opacity = new TouchField("opacity",TNumber.typ)	        // Gets the opacity (between 0 transparent and 1 opaque)
  val field_picture = new TouchField("picture",TPicture.typ)
  val field_speed_x = new TouchField("speed_x", TNumber.typ)        // Gets the speed along x in pixels/sec
  val field_speed_y = new TouchField("speed_y", TNumber.typ)        // Gets the speed along y in pixels/sec
  val field_text = new TouchField("text", TString.typ )          // The text on a text sprite (if it is a text sprite)
  val field_width = new TouchField("width",TNumber.typ)           // Gets the width in pixels
  val field_x = new TouchField("x", TNumber.typ)              // Gets the x position in pixels
  val field_y = new TouchField("y", TNumber.typ)              // Gets the y position in pixels
  val field_z_index = new TouchField("z_index",TNumber.typ)         // Gets the z-index of the sprite

  val typName = "Sprite"
  val typ = TouchType(typName,false,List(
      field_acceleration_x,
      field_acceleration_y,
      field_angle,
      field_angular_speed,
      field_color,
      field_elasticity,
      field_friction,
      field_height,
      field_is_visible,
      field_location,
      field_mass,
      field_opacity,
      field_speed_x,
      field_speed_y,
      field_text,
      field_width,
      field_x,
      field_y,
      field_z_index
  ))

}

class TSprite extends AAny {

  def getTyp = TSprite.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Delete sprite. */
    case "delete" =>
      Skip; // TODO

    /** Are these the same sprite */
    case "equals" =>
      val List(other) = parameters // Sprite
      New[S](TBoolean.typ) // TODO

    /** Hide sprite. */
    case "hide" =>
      AssignField[S](this0,TSprite.field_is_visible,toRichExpression(0))

    /** Moves sprite. */
    case "move" =>
      val List(delta_x,delta_y) = parameters // Number,Number
      Skip; // TODO

    /** Moves the clipping area and wraps around the image if needed (if it is an image sprite) */
    case "move_clip" =>
      val List(x,y) = parameters // Number,Number
      Skip; // TODO

    /** Moves sprite towards other sprite. */
    case "move_towards" =>
      val List(other,fraction) = parameters // Sprite,Number
      Skip; // TODO

    /** Returns the subset of sprites in the given set that overlap with sprite. */
    case "overlap_with" =>
      val List(sprites) = parameters // Sprite_Set
      New[S](TSprite_Set.typ) // TODO

    /** Do the sprites overlap */
    case "overlaps_with" =>
      val List(other) = parameters // Sprite
      New[S](TBoolean.typ) // TODO

    /** Sets the acceleration in pixels/sec^2 */
    case "set_acceleration" =>
      val List(vx,vy) = parameters // Number,Number
      AssignField[S](this0,TSprite.field_acceleration_x,vx) // TODO: AND!
      AssignField[S](this0,TSprite.field_acceleration_y,vy)

    /** Sets the x acceleration in pixels/sec^2 */
    case "set_acceleration_x" =>
      val List(vx) = parameters // Number
      AssignField[S](this0,TSprite.field_acceleration_x,vx)

    /** Sets the y acceleration in pixels/sec^2 */
    case "set_acceleration_y" =>
      val List(vy) = parameters // Number
      AssignField[S](this0,TSprite.field_acceleration_y,vy)

    /** Sets the angle of the sprite in degrees */
    case "set_angle" =>
      val List(angle) = parameters // Number
      AssignField[S](this0,TSprite.field_angle,angle)

    /** Sets the rotation speed in degrees/sec */
    case "set_angular_speed" =>
      val List(speed) = parameters // Number
      AssignField[S](this0,TSprite.field_angular_speed,speed)

    /** Sets the clipping area for an image sprite (if it is an image sprite) */
    case "set_clip" =>
      val List(left,top,width,height) = parameters // Number,Number,Number,Number
      Skip; // TODO

    /** Sets the sprite color. */
    case "set_color" =>
      val List(color) = parameters // Color
      AssignField[S](this0,TSprite.field_color,color)

    /** Sets the sprite elasticity as a fraction of speed preservation per bounce (0-1) */
    case "set_elasticity" =>
      val List(elasticity) = parameters // Number
      AssignField[S](this0,TSprite.field_elasticity,elasticity)

    /** Sets the friction to a fraction of speed loss between 0 and 1 */
    case "set_friction" =>
      val List(friction) = parameters // Number
      AssignField[S](this0,TSprite.field_friction,friction)

    /** Sets the height in pixels */
    case "set_height" =>
      val List(height) = parameters // Number
      AssignField[S](this0,TSprite.field_height,height)

    /** Sets the geo location of the sprite */
    case "set_location" =>
      val List(location) = parameters // Location
      AssignField[S](this0,TSprite.field_location,location)

    /** Sets the sprite mass. */
    case "set_mass" =>
      val List(mass) = parameters // Number
      AssignField[S](this0,TSprite.field_mass,mass)

    /** Sets the sprite opacity (between 0 transparent and 1 opaque). */
    case "set_opacity" =>
      val List(opacity) = parameters // Number
      AssignField[S](this0,TSprite.field_opacity,opacity)

    /** Updates picture on a picture sprite (if it is a picture sprite) */
    case "set_picture" =>
      val List(pic) = parameters // Picture
      AssignField[S](this0,TSprite.field_picture,pic)

    /** Sets the position in pixels */
    case "set_pos" =>
      val List(x,y) = parameters // Number,Number
      AssignField[S](this0,TSprite.field_x,x) // TODO: AND!
      AssignField[S](this0,TSprite.field_y,y)

    /** Sets the speed in pixels/sec */
    case "set_speed" =>
      val List(vx,vy) = parameters // Number,Number
      AssignField[S](this0,TSprite.field_speed_x,vx) // TODO: AND!
      AssignField[S](this0,TSprite.field_speed_y,vy)

    /** Sets the x speed in pixels/sec */
    case "set_speed_x" =>
      val List(vx) = parameters // Number
      AssignField[S](this0,TSprite.field_speed_x,vx)

    /** Sets the y speed in pixels/sec */
    case "set_speed_y" =>
      val List(vy) = parameters // Number
      AssignField[S](this0,TSprite.field_speed_y,vy)

    /** Updates text on a text sprite (if it is a text sprite) */
    case "set_text" =>
      val List(text) = parameters // String
      AssignField[S](this0,TSprite.field_text,text)

    /** Sets the width in pixels */
    case "set_width" =>
      val List(width) = parameters // Number
      AssignField[S](this0,TSprite.field_width,width)

    /** Sets the x position in pixels */
    case "set_x" =>
      val List(x) = parameters // Number
      AssignField[S](this0,TSprite.field_x,x)

    /** Sets the y position in pixels */
    case "set_y" =>
      val List(y) = parameters // Number
      AssignField[S](this0,TSprite.field_y,y)

    /** Sets the z-index of the sprite */
    case "set_z_index" =>
      val List(zindex) = parameters // Number
      AssignField[S](this0,TSprite.field_z_index,zindex)

    /** Show sprite. */
    case "show" =>
      AssignField[S](this0,TSprite.field_is_visible,toRichExpression(1))

    /** Sets sprite speed direction towards other sprite with given magnitude. */
    case "speed_towards" =>
      val List(other,magnitude) = parameters // Sprite,Number
      Skip; // TODO

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}