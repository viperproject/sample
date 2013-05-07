package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:10 PM
 */
object TSprite {

  val field_acceleration_x = new TouchField("acceleration x",TNumber.typ)	// Gets the acceleration along x in pixels/sec^2
  val field_acceleration_y = new TouchField("acceleration y",TNumber.typ)	// Gets the acceleration along y in pixels/sec^2
  val field_angle = new TouchField("angle",TNumber.typ)	          // Gets the angle of the sprite in degrees
  val field_angular_speed = new TouchField("angular speed",TNumber.typ) 	// Gets the rotation speed in degrees/sec
  val field_color = new TouchField("color",TColor.typ)	          // Returns the sprite color.
  val field_elasticity = new TouchField("elasticity",TNumber.typ)	    // Gets the sprite elasticity as a fraction of speed preservation per bounce (0-1)
  val field_friction = new TouchField("friction",TNumber.typ)	      // Gets the fraction of speed loss between 0 and 1
  val field_height = new TouchField("height",TNumber.typ)	        // Gets the height in pixels
  val field_is_visible = new TouchField("is visible",TBoolean.typ)	    // Returns true if sprite is not hidden
  val field_location = new TouchField("location",TLocation.typ)	    // Gets the geo location assigned to the sprite
  val field_mass = new TouchField("mass",TNumber.typ)	          // Gets the mass
  val field_opacity = new TouchField("opacity",TNumber.typ)	        // Gets the opacity (between 0 transparent and 1 opaque)
  val field_picture = new TouchField("picture",TPicture.typ)
  val field_speed_x = new TouchField("speed x", TNumber.typ)        // Gets the speed along x in pixels/sec
  val field_speed_y = new TouchField("speed y", TNumber.typ)        // Gets the speed along y in pixels/sec
  val field_text = new TouchField("text", TString.typ )          // The text on a text sprite (if it is a text sprite)
  val field_width = new TouchField("width",TNumber.typ)           // Gets the width in pixels
  val field_x = new TouchField("x", TNumber.typ)              // Gets the x position in pixels
  val field_y = new TouchField("y", TNumber.typ)              // Gets the y position in pixels
  val field_z_index = new TouchField("z index",TNumber.typ)         // Gets the z-index of the sprite

  val field_clip_left = new TouchField("clip left",TNumber.typ)
  val field_clip_top = new TouchField("clip top",TNumber.typ)
  val field_clip_width = new TouchField("clip width",TNumber.typ)
  val field_clip_height = new TouchField("clip height",TNumber.typ)

  val typName = "Sprite"
  val typ = new TouchType(typName,isSingleton = false,fields = List(
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
      field_clip_height
  ))

}

class TSprite extends AAny {

  def getTyp = TSprite.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

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
    //case "move" =>
    //  val List(delta_x,delta_y) = parameters // Number,Number
    //  Skip; // TODO

    /** Moves the clipping area and wraps around the image if needed (if it is an image sprite) */
    //case "move clip" =>
    //  val List(x,y) = parameters // Number,Number
    //  Skip; // TODO

    /** Moves sprite towards other sprite. */
    //case "move towards" =>
    //  val List(other,fraction) = parameters // Sprite,Number
    //  Skip; // TODO

    /** Returns the subset of sprites in the given set that overlap with sprite. */
    case "overlap with" =>
      val List(sprites) = parameters // Sprite_Set
      New[S](TSprite_Set.typ) // TODO

    /** Do the sprites overlap */
    case "overlaps with" =>
      val List(other) = parameters // Sprite
      Top[S](TBoolean.typ) // TODO

    /** Sets the acceleration in pixels/sec^2 */
    case "set acceleration" =>
      val List(vx,vy) = parameters // Number,Number
      val curState = AssignField[S](this0,TSprite.field_acceleration_x,vx)
      AssignField[S](this0,TSprite.field_acceleration_y,vy)(curState,pp)

    /** Sets the clipping area for an image sprite (if it is an image sprite) */
    case "set clip" =>
      val List(left,top,width,height) = parameters // Number,Number,Number,Number
      var curState = state
      curState = AssignField[S](this0,TSprite.field_clip_left,left)(curState,pp)
      curState = AssignField[S](this0,TSprite.field_clip_top,top)(curState,pp)
      curState = AssignField[S](this0,TSprite.field_clip_width,width)(curState,pp)
      curState = AssignField[S](this0,TSprite.field_clip_height,height)(curState,pp)
      Skip

    /** Sets the position in pixels */
    case "set pos" =>
      val List(x,y) = parameters // Number,Number
      val curState = AssignField[S](this0,TSprite.field_x,x)
      AssignField[S](this0,TSprite.field_y,y)(curState,pp)

    /** Sets the speed in pixels/sec */
    case "set speed" =>
      val List(vx,vy) = parameters // Number,Number
      val curState = AssignField[S](this0,TSprite.field_speed_x,vx)
      AssignField[S](this0,TSprite.field_speed_y,vy)(curState,pp)

    /** Show sprite. */
    case "show" =>
      AssignField[S](this0,TSprite.field_is_visible,toRichExpression(1))

    /** Sets sprite speed direction towards other sprite with given magnitude. */
    case "speed towards" =>
      val List(other,magnitude) = parameters // Sprite,Number

      //val newX = Field[S](other,TSprite.field_x) - Field[S](this0,TSprite.field_x)
      //val newY = Field[S](other,TSprite.field_y) - Field[S](this0,TSprite.field_y)
      //val normalizedX =
      // other.pos - this.pos
      //val state1 = CallApi[S](Field[S](other,TVector3.field_x),"subtract",List(Field))
      // TODO: Not trivial, implement vector normalization first

      val state1 = AssignField[S](this0,TSprite.field_speed_x,Valid(TNumber.typ))
      val state2 = AssignField[S](this0,TSprite.field_speed_y,Valid(TNumber.typ))(state1,pp)
      state2

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}