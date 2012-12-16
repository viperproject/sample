package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of Picture
 *
 * A picture
 *
 * @author Lucas Brutschy
 */

object TPicture {

  val field_width = new TouchField("width", TNumber.typ)
  val field_height = new TouchField("height", TNumber.typ)

  val typName = "Picture"
  val typ = TouchType(typName,isSingleton = false,List(field_width,field_height))

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
    // case "brightness" =>
    //   val List(factor) = parameters // Number
    //   Skip;

    /** Clears the picture to a given color */
    // case "clear" =>
    //   val List(color) = parameters // Color
    //   Skip;

    /** Returns a copy of the image */
    // case "clone" =>
    //   Return[S](Valid(TPicture.typ))
    // DECLARATION AS FIELD:
    //   /** Returns a copy of the image */
    //   field_clone = new TouchField("clone",TPicture.typ)

    /** Recolors the picture with the background and foreground color, based on a color threshold between 0.0 and 1.0 */
    // case "colorize" =>
    //   val List(background,foreground,threshold) = parameters // Color,Color,Number
    //   Skip;

    /** Changes the contrast of the picture. factor in [-1, 1]. */
    // case "contrast" =>
    //   val List(factor) = parameters // Number
    //   Skip;

    /** Gets the number of pixels */
    // case "count" =>
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the number of pixels */
    //   field_count = new TouchField("count",TNumber.typ)

    /** Crops a sub-image */
    // case "crop" =>
    //   val List(left,top,width,height) = parameters // Number,Number,Number,Number
    //   Skip;

    /** Gets the date time where the picture was taken; if any. */
    // case "date" =>
    //   Return[S](Valid(TDateTime.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the date time where the picture was taken; if any. */
    //   field_date = new TouchField("date",TDateTime.typ)

    /** Makes picture gray */
    // case "desaturate" =>
    //   Skip;

    /** Draws an elliptic border with a given color */
    // case "draw_ellipse" =>
    //   val List(left,top,width,height,angle,c,thickness) = parameters // Number,Number,Number,Number,Number,Color,Number
    //   Skip;

    /** Draws a line between two points */
    // case "draw_line" =>
    //   val List(x1,y1,x2,y2,color,thickness) = parameters // Number,Number,Number,Number,Color,Number
    //   Skip;

    /** Draws a rectangle border with a given color */
    // case "draw_rect" =>
    //   val List(left,top,width,height,angle,c,thickness) = parameters // Number,Number,Number,Number,Number,Color,Number
    //   Skip;

    /** Draws some text border with a given color and font size */
    case "draw_text" =>
      val List(x,y,text,font,degree,color) = parameters // Number,Number,String,Number,Number,Color

      Error (x < 0, "draw_text: Parameter X ("+x+") might be negative")(state,pp)
      Error (y < 0, "draw_text: Parameter Y ("+y+") might be negative")(state,pp)
      Error (x >= Field[S](this0,TPicture.field_width), "draw_text: Parameter X ("+x+") might be greater than width")(state,pp)
      Error (y >= Field[S](this0,TPicture.field_height), "draw_text: Parameter Y ("+y+") might be greater than height")(state,pp)

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
    // case "flip_horizontal" =>
    //   Skip;

    /** Flips the picture vertically */
    // case "flip_vertical" =>
    //   Skip;

    /** Gets the height in pixels */
    // case "height" =>
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the height in pixels */
    //   field_height = new TouchField("height",TNumber.typ)

    /** Inverts the red, blue and green channels */
    // case "invert" =>
    //   Skip;

    /** Indicates if the picture width is greater than its height */
    // case "is_panorama" =>
    //   Return[S](Valid(TBoolean.typ))
    // DECLARATION AS FIELD:
    //   /** Indicates if the picture width is greater than its height */
    //   field_is_panorama = new TouchField("is_panorama",TBoolean.typ)

    /** Gets the location where the picture was taken; if any. */
    // case "location" =>
    //   Return[S](Valid(TLocation.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the location where the picture was taken; if any. */
    //   field_location = new TouchField("location",TLocation.typ)

    /** Gets the pixel color */
    // case "pixel" =>
    //   val List(left,top) = parameters // Number,Number
    //   Return[S](Valid(TColor.typ))

    /** Resizes the picture to the given size in pixels */
    // case "resize" =>
    //   val List(width,height) = parameters // Number,Number
    //   Skip;

    /** Saves the picture to the 'saved pictures' album. Returns the file name. */
    // case "save_to_library" =>
    //   Return[S](Valid(TString.typ))
    // DECLARATION AS FIELD:
    //   /** Saves the picture to the 'saved pictures' album. Returns the file name. */
    //   field_save_to_library = new TouchField("save_to_library",TString.typ)

    /** Sets the pixel color at a given pixel */
    case "set_pixel" =>
      val List(x,y,color) = parameters // Number,Number,Color

      Error (x < 0, "set_pixel: Parameter X ("+x+") might be negative")(state,pp)
      Error (y < 0, "set_pixel: Parameter Y ("+y+") might be negative")(state,pp)
      Error (x >= Field[S](this0,TPicture.field_width), "set_pixel: Parameter X ("+x+") might be greater than width")(state,pp)
      Error (y >= Field[S](this0,TPicture.field_height), "set_pixel: Parameter Y ("+y+") might be greater than height")(state,pp)

      Skip

    /** Shares this message ('' to pick from a list) */
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

    /** Gets the width in pixels */
    // case "width" =>
    //   Return[S](Valid(TNumber.typ))
    // DECLARATION AS FIELD:
    //   /** Gets the width in pixels */
    //   field_width = new TouchField("width",TNumber.typ)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}
