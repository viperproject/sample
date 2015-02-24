package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalAnalysisConstants
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.compiler.{SkipSemantics, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_TPicture
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Picture
 *
 * A picture
 *
 * @author Lucas Brutschy
 */

object TPicture extends Default_TPicture {

  override lazy val member_clear_rect = super.member_clear_rect.copy(semantics = SkipSemantics)
  override lazy val member_clear = super.member_clear.copy(semantics = SkipSemantics)

  /** Gets the width in pixels */
  lazy val field_width = new ApiField("width", TNumber,
    default = ExpressionInitializer(0 ndToIncl PositiveInfinity(null)),
    topDefault = ExpressionInitializer(0 ndToIncl PositiveInfinity(null)))

  /** Gets the height in pixels */
  lazy val field_height = new ApiField("height", TNumber,
    default = ExpressionInitializer(0 ndToIncl PositiveInfinity(null)),
    topDefault = ExpressionInitializer(0 ndToIncl PositiveInfinity(null)))

  /** Gets the location where the picture was taken; if any. */
  lazy val field_location = new ApiField("location", TLocation, InvalidInitializer("picture may not have a location"))

  /** Gets the date time where the picture was taken; if any. */
  lazy val field_date = new ApiField("date", TDateTime, InvalidInitializer("picture may not have a date"))

  override def possibleFields = super.possibleFields ++ List(field_width, field_height, field_location, field_date)

  override def mutedFields = super.mutedFields ++ List(field_width, field_height)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {


    /** Gets the pixel color at the given linear index */
    case "at" =>
      val List(index) = parameters // Number
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations)
        CheckInRangeInclusive[S](index, 0, (Field[S](this0, TPicture.field_height) * Field[S](this0, TPicture.field_width)) - NumericalAnalysisConstants.epsilon, "at", "index")
      Top[S](TColor)

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

    /** Returns a copy of the image */
    case "clone" =>
      Return[S](this0)
      //Clone[S](this0)

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
        CheckInRangeInclusive[S](left, 0, Field[S](this0, TPicture.field_width), "draw text", "left")
        CheckInRangeInclusive[S](top, 0, Field[S](this0, TPicture.field_height), "draw text", "top")
        CheckInRangeInclusive[S](angle, 0, 360, "draw text", "angle")
      }
      Skip

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
      New[S](TColor)

    /** Resizes the picture to the given size in pixels */
    case "resize" =>
      val List(width, height) = parameters // Number,Number

      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        Error[S](width <= 0 && height <= 0, "resize", "Width and height may both be negative!")
      }

      // UNDOCUMENTED: Values <= 0 say "choose according to ratio". Both parameters <= 0: Resize to 300:150... We detect an error

      val state1 = If[S](width <= 0, Then = {
        s: S =>
          // new_w = new_h * (old_w / old_h)
          AssignField[S](this0, TPicture.field_width, height * Field[S](this0, TPicture.field_width) / Field[S](this0, TPicture.field_height))(s, pp)
      }, Else = {
        s: S =>
          AssignField[S](this0, TPicture.field_width, width)(s, pp)
      })

      val state2 = If[S](height <= 0, Then = {
        s: S =>
          // new_h = new_w * (old_h / old_w)
          AssignField[S](this0, TPicture.field_height, width * Field[S](this0, TPicture.field_height) / Field[S](this0, TPicture.field_width))(s, pp)
      }, Else = {
        s: S =>
          AssignField[S](this0, TPicture.field_height, height)(s, pp)
      })(state1, pp)

      state2

    /** Saves the picture to the 'saved pictures' album. Returns the file name. */
    case "save to library" =>
      Top[S](TString)

    /** Sets the pixel color at a given pixel */
    case "set pixel" =>
      val List(x, y, color) = parameters // Number,Number,Color
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](x, 0, Field[S](this0, TPicture.field_width) - NumericalAnalysisConstants.epsilon, "set pixel", "x")
        CheckInRangeInclusive[S](y, 0, Field[S](this0, TPicture.field_height) - NumericalAnalysisConstants.epsilon, "set pixel", "y")
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
      Top[S](TBuffer)

    /** Encodes the image into a data uri using the desired quality (1 best, 0 worst). If the quality value is 1, the image is encoded as PNG, otherwise JPEG. */
    case "to data uri" =>
      val List(quality) = parameters // Number
      Top[S](TString)

    /** Refreshes the picture on the wall */
    case "update on wall" =>
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
