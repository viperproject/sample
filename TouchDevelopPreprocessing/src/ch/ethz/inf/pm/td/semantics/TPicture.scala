package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.{DefaultTouchType, TouchType}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalAnalysisConstants
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.analysis.interpreter._

/**
 * Specifies the abstract semantics of Picture
 *
 * A picture
 *
 * @author Lucas Brutschy
 */

object TPicture {

  /** Gets the width in pixels */
  val field_width = new TouchField("width", TNumber.typName,
    default = ExpressionInitializer(0 ndTo PositiveInfinity(null)),
    topDefault = ExpressionInitializer(0 ndTo PositiveInfinity(null)))

  /** Gets the height in pixels */
  val field_height = new TouchField("height", TNumber.typName,
    default = ExpressionInitializer(0 ndTo PositiveInfinity(null)),
    topDefault = ExpressionInitializer(0 ndTo PositiveInfinity(null)))

  /** Gets the location where the picture was taken; if any. */
  val field_location = new TouchField("location", TLocation.typName, InvalidInitializer)

  /** Gets the date time where the picture was taken; if any. */
  val field_date = new TouchField("date", TDateTime.typName, InvalidInitializer)

  val typName = "Picture"
  val typ = DefaultTouchType(typName, isSingleton = false, fields = List(field_width, field_height, field_location, field_date))

}

class TPicture extends AAny {

  def getTyp = TPicture.typ

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {


    /** Gets the pixel color at the given linear index */
    case "at" =>
      val List(index) = parameters // Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations)
        CheckInRangeInclusive[S](index, 0, (Field[S](this0, TPicture.field_height) * Field[S](this0, TPicture.field_width)) - NumericalAnalysisConstants.epsilon, "at", "index")
      Top[S](TColor.typ)

    /** Writes another picture at a given location. The opacity ranges from 0 (transparent) to 1 (opaque). */
    case "blend" =>
      val List(other, left, top, angle, opacity) = parameters // Picture,Number,Number,Number,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](top, 0, Field[S](this0, TPicture.field_height) - NumericalAnalysisConstants.epsilon, "blend", "top")
        CheckInRangeInclusive[S](left, 0, Field[S](this0, TPicture.field_width) - NumericalAnalysisConstants.epsilon, "blend", "left")
        CheckInRangeInclusive[S](angle, 0, 360, "blend", "angle")
        CheckInRangeInclusive[S](opacity, 0, 1, "blend", "opacity")
        CheckInRangeInclusive[S](Field[S](other, TPicture.field_height), 0, Field[S](this0, TPicture.field_height) - top, "blend", "other->height")
        CheckInRangeInclusive[S](Field[S](other, TPicture.field_width), 0, Field[S](this0, TPicture.field_width) - top, "blend", "other->width")
      }
      Skip


    /** Writes an Scalable Vector Graphics (SVG) document at a given location. By default, this action uses the viewport size provided in the SVG document when width or height are negative. */
    case "blend svg" =>
      val List(markup, left, top, width, height, angle) = parameters // String,Number,Number,Number,Number,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](top, 0, Field[S](this0, TPicture.field_height) - NumericalAnalysisConstants.epsilon, "blend svg", "top")
        CheckInRangeInclusive[S](left, 0, Field[S](this0, TPicture.field_width) - NumericalAnalysisConstants.epsilon, "blend svg", "left")
        CheckInRangeInclusive[S](height, 0, Field[S](this0, TPicture.field_height) - top, "blend svg", "height")
        CheckInRangeInclusive[S](width, 0, Field[S](this0, TPicture.field_width) - left, "blend svg", "width")
        CheckInRangeInclusive[S](angle, 0, 360, "blend svg", "angle")
      }
      Skip

    /** Changes the brightness of the picture. factor in [-1, 1]. */
    case "brightness" =>
      val List(factor) = parameters // Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](factor, -1, 1, "brightness", "factor")
      }
      Skip

    /** Clears the picture to a given color */
    case "clear" =>
      val List(color) = parameters // Color
      Skip

    /** Returns a copy of the image */
    case "clone" =>
      Clone[S](this0)

    /** Recolors the picture with the background and foreground color, based on a color threshold between 0.0 and 1.0 */
    case "colorize" =>
      val List(background, foreground, threshold) = parameters // Color,Color,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](threshold, 0, 1, "colorize", "threshold")
      }
      Skip

    /** Changes the contrast of the picture. factor in [-1, 1]. */
    case "contrast" =>
      val List(factor) = parameters // Number
      CheckInRangeInclusive[S](factor, -1, 1, "contrast", "factor")
      Skip

    /** Gets the number of pixels */
    case "count" =>
      Return[S](Field[S](this0, TPicture.field_width) * Field[S](this0, TPicture.field_height))

    /** Crops a sub-image */
    case "crop" =>
      val List(left, top, width, height) = parameters // Number,Number,Number,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](left, 0, Field[S](this0, TPicture.field_width) - NumericalAnalysisConstants.epsilon, "crop", "left")
        CheckInRangeInclusive[S](top, 0, Field[S](this0, TPicture.field_height) - NumericalAnalysisConstants.epsilon, "crop", "top")
        CheckInRangeInclusive[S](width, 0, Field[S](this0, TPicture.field_width) - left, "crop", "width")
        CheckInRangeInclusive[S](height, 0, Field[S](this0, TPicture.field_height) - top, "crop", "height")
      }
      val state1 = AssignField[S](this0, TPicture.field_width, width)
      val state2 = AssignField[S](this0, TPicture.field_width, height)(state1, pp)
      state2

    /** Makes picture gray */
    case "desaturate" =>
      Skip

    /** Draws an elliptic border with a given color */
    case "draw ellipse" =>
      val List(left, top, width, height, angle, c, thickness) = parameters // Number,Number,Number,Number,Number,Color,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](left, 0, Field[S](this0, TPicture.field_width) - NumericalAnalysisConstants.epsilon, "draw ellipse", "left")
        CheckInRangeInclusive[S](top, 0, Field[S](this0, TPicture.field_height) - NumericalAnalysisConstants.epsilon, "draw ellipse", "top")
        CheckInRangeInclusive[S](width, 0, Field[S](this0, TPicture.field_width) - left, "draw ellipse", "width")
        CheckInRangeInclusive[S](height, 0, Field[S](this0, TPicture.field_height) - top, "draw ellipse", "height")
        CheckInRangeInclusive[S](angle, 0, 360, "draw ellipse", "angle")
        CheckNonNegative[S](thickness, "draw ellipse", "thickness")
      }
      Skip

    /** Draws a line between two points */
    case "draw line" =>
      val List(x1, y1, x2, y2, color, thickness) = parameters // Number,Number,Number,Number,Color,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](x1, 0, Field[S](this0, TPicture.field_width), "draw line", "x1")
        CheckInRangeInclusive[S](y1, 0, Field[S](this0, TPicture.field_height), "draw line", "y1")
        CheckInRangeInclusive[S](x2, 0, Field[S](this0, TPicture.field_width), "draw line", "x2")
        CheckInRangeInclusive[S](y2, 0, Field[S](this0, TPicture.field_height), "draw line", "y2")
        CheckNonNegative[S](thickness, "draw line", "thickness")
      }
      Skip

    /** Draws a path with a given color. */
    case "draw path" =>
      val List(left, top, angle, color, thickness, data) = parameters // Number,Number,Number,Color,Number,String
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](left, 0, Field[S](this0, TPicture.field_width), "draw path", "left")
        CheckInRangeInclusive[S](top, 0, Field[S](this0, TPicture.field_height), "draw path", "top")
        CheckInRangeInclusive[S](angle, 0, 360, "draw path", "angle")
        CheckNonNegative[S](thickness, "draw path", "thickness")
        // does not check for data
      }
      Skip

    /** Draws a rectangle border with a given color */
    case "draw rect" =>
      val List(left, top, width, height, angle, c, thickness) = parameters // Number,Number,Number,Number,Number,Color,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](left, 0, Field[S](this0, TPicture.field_width), "draw rect", "left")
        CheckInRangeInclusive[S](top, 0, Field[S](this0, TPicture.field_height), "draw rect", "top")
        CheckInRangeInclusive[S](left + width, 0, Field[S](this0, TPicture.field_width), "draw rect", "left+width")
        CheckInRangeInclusive[S](top + height, 0, Field[S](this0, TPicture.field_height), "draw rect", "top+height")
        CheckInRangeInclusive[S](angle, 0, 360, "draw rect", "angle")
        CheckNonNegative[S](thickness, "draw rect", "thickness")
      }
      Skip

    /** Draws some text border with a given color and font size */
    case "draw text" =>
      val List(left, top, text, font, angle, color) = parameters // Number,Number,String,Number,Number,Color
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        val s1 = CheckInRangeInclusive[S](left,0,Field[S](this0,TPicture.field_width),"draw text","left")
        val s2 = CheckInRangeInclusive[S](top,0,Field[S](this0,TPicture.field_height),"draw text","top")(s1, pp)
        CheckInRangeInclusive[S](angle,0,360,"draw text","angle")(s2, pp)
      } else Skip

    /** Fills a ellipse with a given color */
    case "fill ellipse" =>
      val List(left, top, width, height, angle, color) = parameters // Number,Number,Number,Number,Number,Color
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](left, 0, Field[S](this0, TPicture.field_width) - NumericalAnalysisConstants.epsilon, "fill ellipse", "left")
        CheckInRangeInclusive[S](top, 0, Field[S](this0, TPicture.field_height) - NumericalAnalysisConstants.epsilon, "fill ellipse", "top")
        CheckInRangeInclusive[S](width, 0, Field[S](this0, TPicture.field_width) - left, "fill ellipse", "width")
        CheckInRangeInclusive[S](height, 0, Field[S](this0, TPicture.field_height) - top, "fill ellipse", "height")
        CheckInRangeInclusive[S](angle, 0, 360, "fill ellipse", "angle")
      }
      Skip

    /** Fills a path with a given color. */
    case "fill path" =>
      val List(left, top, angle, color, data) = parameters // Number,Number,Number,Color,String
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](left, 0, Field[S](this0, TPicture.field_width), "fill path", "left")
        CheckInRangeInclusive[S](top, 0, Field[S](this0, TPicture.field_height), "fill path", "top")
        CheckInRangeInclusive[S](angle, 0, 360, "fill path", "angle")
        // does not check for data
      }
      Skip

    /** Fills a rectangle with a given color */
    case "fill rect" =>
      val List(left, top, width, height, angle, color) = parameters // Number,Number,Number,Number,Number,Color
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](left, 0, Field[S](this0, TPicture.field_width), "fill rect", "left")
        CheckInRangeInclusive[S](top, 0, Field[S](this0, TPicture.field_height), "fill rect", "top")
        CheckInRangeInclusive[S](left + width, 0, Field[S](this0, TPicture.field_width), "fill rect", "left+width")
        CheckInRangeInclusive[S](top + height, 0, Field[S](this0, TPicture.field_height), "fill rect", "top+height")
        CheckInRangeInclusive[S](angle, 0, 360, "fill rect", "angle")
      }
      Skip

    /** Flips the picture horizontally */
    case "flip horizontal" =>
      Skip

    /** Flips the picture vertically */
    case "flip vertical" =>
      Skip

    /** Inverts the red, blue and green channels */
    case "invert" =>
      Skip

    /** Indicates if the picture width is greater than its height */
    case "is panorama" =>
      Return[S](Field[S](this0, TPicture.field_width) > Field[S](this0, TPicture.field_height))

    /** Inverts the colors in the picture */
    case "negative" =>
      Skip

    /** Gets the pixel color */
    case "pixel" =>
      val List(x, y) = parameters // Number,Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](x, 0, Field[S](this0, TPicture.field_width) - NumericalAnalysisConstants.epsilon, "pixel", "x")
        CheckInRangeInclusive[S](y, 0, Field[S](this0, TPicture.field_height) - NumericalAnalysisConstants.epsilon, "pixel", "y")
      }
      New[S](TColor.typ)

    /** Resizes the picture to the given size in pixels */
    case "resize" =>
      val List(width, height) = parameters // Number,Number

      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        Error[S](width <= 0 && height <= 0, "resize", "Width and height may both be negative!")
      }

      // UNDOCUMENTED: Values <= 0 say "choose according to ratio". Both parameters <= 0: Resize to 300:150... We detect an error

      val state1 = If[S](width<=0,Then = { s =>
        // new_w = new_h * (old_w / old_h)
        AssignField[S](this0,TPicture.field_width, height * Field[S](this0,TPicture.field_width) / Field[S](this0,TPicture.field_height))(s,pp)
      }, Else = { s: S =>
        AssignField[S](this0,TPicture.field_width, width)(s,pp)
      })

      val state2 = If[S](height<=0,Then = { s: S =>
        // new_h = new_w * (old_h / old_w)
        AssignField[S](this0,TPicture.field_height, width * Field[S](this0,TPicture.field_height) / Field[S](this0,TPicture.field_width))(s,pp)
      }, Else = { s: S =>
        AssignField[S](this0,TPicture.field_height, height)(s,pp)
      })(state1,pp)

      val newwidth = Field[S](this0, TPicture.field_width)(state2, pp)
      val newheight = Field[S](this0, TPicture.field_height)(state2, pp)
      Assume[S](newwidth > 0 && newheight > 0)(state2, pp)

    /** Saves the picture to the 'saved pictures' album. Returns the file name. */
    case "save to library" =>
      Top[S](TString.typ)

    /** Sets the pixel color at a given pixel */
    case "set pixel" =>
      val List(x, y, color) = parameters // Number,Number,Color
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckLowerBound[S](x, 0, "set pixel", "x")
        CheckStrictUpperBound[S](x, Field[S](this0,TPicture.field_width), "set pixel", "x")
        CheckLowerBound[S](y, 0, "set pixel", "y")
        CheckStrictUpperBound[S](y, Field[S](this0,TPicture.field_height), "set pixel", "y")
      }
      Skip

    /** Shares this message (empty string to pick from a list) */
    case "share" =>
      val List(where, message) = parameters // String,String
      Skip

    /** Converts every pixel to gray and tints it with the given color. */
    case "tint" =>
      val List(color) = parameters // Color
      Skip

    /** Copy all pixels from the picture */
    case "to buffer" =>
      val List() = parameters //
      Top[S](TBuffer.typ)

    /** Encodes the image into a data uri using the desired quality (1 best, 0 worst). If the quality value is 1, the image is encoded as PNG, otherwise JPEG. */
    case "to data uri" =>
      val List(quality) = parameters // Number
      Top[S](TString.typ)

    /** Refreshes the picture on the wall */
    case "update on wall" =>
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }

  override def concreteSemantics(this0: TouchValue, method: String, params: List[TouchValue],
                                 interpreter: ConcreteInterpreter, pp: ProgramPoint): TouchValue = {
    val state = interpreter.state

    method match {
      case "set pixel" =>
        (this0, params) match {
          case (pic@RefV(typ, id), List(NumberV(x), NumberV(y), _)) =>
          if (x < 0) interpreter.failWithError(pp, InterpreterErrorType.AssertFailure, Some("Negative x coordinate"))
          if (y < 0) interpreter.failWithError(pp, InterpreterErrorType.AssertFailure, Some("Negative y coordinate"))
          val NumberV(width) = state.getField(pic, "width")
          val NumberV(height) = state.getField(pic, "height")
          if (x >= width) interpreter.failWithError(pp, InterpreterErrorType.AssertFailure, Some("Drawing out of width bound"))
          if (y >= height) interpreter.failWithError(pp, InterpreterErrorType.AssertFailure, Some("Drawing out of width bound"))
          UnitV
        }
      case "draw text" =>
        (this0, params) match {
          case (pic: RefV, List(NumberV(x), NumberV(y), StringV(text), NumberV(fontsize), NumberV(dir), color: RefV)) =>
            if (x < 0) interpreter.failWithError(pp, InterpreterErrorType.AssertFailure, Some("Negative x coordinate"))
            if (y < 0) interpreter.failWithError(pp, InterpreterErrorType.AssertFailure, Some("Negative y coordinate"))
            val NumberV(width) = state.getField(pic, "width")
            val NumberV(height) = state.getField(pic, "height")
            if (x >= width) interpreter.failWithError(pp, InterpreterErrorType.AssertFailure, Some("Drawing out of width bound"))
            if (y >= height) interpreter.failWithError(pp, InterpreterErrorType.AssertFailure, Some("Drawing out of height bound"))
            UnitV
        }
      case "fill rect" =>
        (this0, params) match {
          case (pic: RefV, List(NumberV(left), NumberV(top), NumberV(width), NumberV(height), NumberV(angle), color: RefV)) =>
            val NumberV(picwidth) = state.getField(pic, TPicture.field_width.getName)
            val NumberV(picheight) = state.getField(pic, TPicture.field_height.getName)

            interpreter.assertE(0 <= left && left <= picwidth)(pp)
            interpreter.assertE(0 <= top && top <= picheight)(pp)
            interpreter.assertE(0 <= left + width && left + width <= picwidth)(pp)
            interpreter.assertE(0 <= top + height && top + height  <= picheight)(pp)
            interpreter.assertE(0 <= angle && angle <= 360)(pp)

            UnitV
        }
      case "is panorama" =>
        this0 match {
          case pic: RefV =>
            val NumberV(picwidth) = state.getField(pic, TPicture.field_width.getName)
            val NumberV(picheight) = state.getField(pic, TPicture.field_height.getName)
            BooleanV(picwidth > picheight)
        }

      case "resize" =>
        (this0, params) match {
          case (pic@RefV(typ, id), List(NumberV(x), NumberV(y))) =>
            interpreter.assertE(x >= 0 || y >= 0)(pp)
            val NumberV(oldx) = state.getField(pic, "width")
            val NumberV(oldy) = state.getField(pic, "height")
            val (newx, newy) =
              if (x < 0) {
                (y * (oldx / oldy), y)
              } else if (y < 0) {
                (x, x * (y / x))
              } else (x,y)
            state.setField(pic, "width", NumberV(newx))
            state.setField(pic, "height", NumberV(newy))
            UnitV
        }

      case "post to wall" => UnitV

      case _ => super.concreteSemantics(this0, method, params, interpreter, pp)

    }
  }
}
