package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Picture
 *
 * A picture
 *
 * @author Lucas Brutschy
 */

object TPicture {

  /** Gets the width in pixels */
  val field_width = new TouchField("width", TNumber.typ,Valid(TNumber.typ)(null))

  /** Gets the height in pixels */
  val field_height = new TouchField("height", TNumber.typ,Valid(TNumber.typ)(null))

  /** Gets the location where the picture was taken; if any. */
  val field_location = new TouchField("location",TLocation.typ,Invalid(TLocation.typ)(null))

  /** Gets the date time where the picture was taken; if any. */
  val field_date = new TouchField("date",TDateTime.typ,Invalid(TLocation.typ)(null))

  val typName = "Picture"
  val typ = TouchType(typName,isSingleton = false,List(field_width,field_height,field_location,field_date))

}

class TPicture extends AAny {

  def getTyp = TPicture.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {


    /** Gets the pixel color at the given linear index */
    // case "at" =>
    //   val List(index) = parameters // Number
    //   Return[S](Valid(TColor.typ))

    /** Writes another picture at a given location. The opacity ranges from 0 (transparent) to 1 (opaque). */
    // case "blend" =>
    //   val List(other,left,top,angle,opacity) = parameters // Picture,Number,Number,Number,Number
    //   Skip;

    /** Changes the brightness of the picture. factor in [-1, 1]. */
     case "brightness" =>
       val List(factor) = parameters // Number
       CheckInRangeInclusive[S](factor,-1,1,"brightness","factor")
       Skip

    /** Clears the picture to a given color */
    case "clear" =>
       val List(color) = parameters // Color
       Skip

    /** Returns a copy of the image */
    case "clone" =>
      Clone[S](this0)

    /** Recolors the picture with the background and foreground color, based on a color threshold between 0.0 and 1.0 */
    // case "colorize" =>
    //   val List(background,foreground,threshold) = parameters // Color,Color,Number
    //   Skip;

    /** Changes the contrast of the picture. factor in [-1, 1]. */
    // case "contrast" =>
    //   val List(factor) = parameters // Number
    //   Skip;

    /** Gets the number of pixels */
     case "count" =>
       Return[S](Field[S](this0,TPicture.field_width)*Field[S](this0,TPicture.field_height))

    /** Crops a sub-image */
    // case "crop" =>
    //   val List(left,top,width,height) = parameters // Number,Number,Number,Number
    //   Skip;

    /** Makes picture gray */
    case "desaturate" =>
      Skip

    /** Draws an elliptic border with a given color */
    // case "draw_ellipse" =>
    //   val List(left,top,width,height,angle,c,thickness) = parameters // Number,Number,Number,Number,Number,Color,Number
    //   Skip;

    /** Draws a line between two points */
    case "draw_line" =>
      val List(x1,y1,x2,y2,color,thickness) = parameters // Number,Number,Number,Number,Color,Number
      CheckInRangeInclusive[S](x1,0,Field[S](this0,TPicture.field_width),"draw_line","x1")
      CheckInRangeInclusive[S](y1,0,Field[S](this0,TPicture.field_height),"draw_line","y1")
      CheckInRangeInclusive[S](x2,0,Field[S](this0,TPicture.field_width),"draw_line","x2")
      CheckInRangeInclusive[S](y2,0,Field[S](this0,TPicture.field_height),"draw_line","y2")
      Skip

    /** Draws a rectangle border with a given color */
    case "draw_rect" =>
      val List(left,top,width,height,angle,c,thickness) = parameters // Number,Number,Number,Number,Number,Color,Number
      CheckInRangeInclusive[S](left,0,Field[S](this0,TPicture.field_width),"draw_rect","left")
      CheckInRangeInclusive[S](top,0,Field[S](this0,TPicture.field_height),"draw_rect","top")
      CheckInRangeInclusive[S](left+width,0,Field[S](this0,TPicture.field_width),"draw_rect","left+width")
      CheckInRangeInclusive[S](top+height,0,Field[S](this0,TPicture.field_height),"draw_rect","top+height")
      CheckInRangeInclusive[S](angle,0,360,"draw_rect","angle")
      Error (thickness < 0, "draw_text: Parameter Thickness ("+thickness+") might be negative")(state,pp)
      Skip

    /** Draws some text border with a given color and font size */
    case "draw_text" =>
      val List(left,top,text,font,angle,color) = parameters // Number,Number,String,Number,Number,Color
      CheckInRangeInclusive[S](left,0,Field[S](this0,TPicture.field_width),"draw_text","left")
      CheckInRangeInclusive[S](top,0,Field[S](this0,TPicture.field_height),"draw_text","top")
      CheckInRangeInclusive[S](angle,0,360,"draw_text","angle")
      Skip

    /** Fills a ellipse with a given color */
    // case "fill_ellipse" =>
    //   val List(left,top,width,height,angle,color) = parameters // Number,Number,Number,Number,Number,Color
    //   Skip;

    /** Fills a rectangle with a given color */
    // case "fill_rect" =>
    //   val List(left,top,width,height,angle,color) = parameters // Number,Number,Number,Number,Number,Color
    //   Skip;

    /** Flips the picture horizontally */
    case "flip_horizontal" =>
       Skip

    /** Flips the picture vertically */
    case "flip_vertical" =>
       Skip

    /** Inverts the red, blue and green channels */
    case "invert" =>
       Skip

    /** Indicates if the picture width is greater than its height */
    case "is_panorama" =>
       Return[S](Field[S](this0,TPicture.field_width)>Field[S](this0,TPicture.field_height))

    /** Gets the pixel color */
     case "pixel" =>
       val List(x,y) = parameters // Number,Number
       CheckInRangeInclusive[S](x,0,Field[S](this0,TPicture.field_width),"pixel","x")
       CheckInRangeInclusive[S](y,0,Field[S](this0,TPicture.field_height),"pixel","y")
       Return[S](Valid(TColor.typ))

    /** Resizes the picture to the given size in pixels */
    case "resize" =>
      val List(width,height) = parameters // Number,Number
      val state1 = AssignField[S](this0,TPicture.field_height,height)
      val state2 = AssignField[S](this0,TPicture.field_width,width)(state1,pp)
      state2

    /** Saves the picture to the 'saved pictures' album. Returns the file name. */
    // case "save_to_library" =>
    //   Return[S](Valid(TString.typ))

    /** Sets the pixel color at a given pixel */
    case "set_pixel" =>
      val List(x,y,color) = parameters // Number,Number,Color
      CheckInRangeInclusive[S](x,0,Field[S](this0,TPicture.field_width),"set_pixel","x")
      CheckInRangeInclusive[S](y,0,Field[S](this0,TPicture.field_height),"set_pixel","y")
      Skip

    /** Shares this message (empty string to pick from a list) */
    // case "share" =>
    //   val List(where,message) = parameters // String,String
    //   Skip;

    /** Converts every pixel to gray and tints it with the given color. */
    // case "tint" =>
    //   val List(color) = parameters // Color
    //   Skip;

    /** Refreshes the picture on the wall */
    // case "update_on_wall" =>
    //   Skip;

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
