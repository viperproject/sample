
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Color
 *
 * A argb color (alpha, red, green, blue)
 *
 * @author Lucas Brutschy
 */

trait Default_TColor extends AAny {

  lazy val typeName = TypeName("Color")
          
  /** Sometimes used: Gets the normalized alpha value (0.0-1.0) */
  def member_A = ApiMember(
    name = "A",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the normalized blue value (0.0-1.0) */
  def member_B = ApiMember(
    name = "B",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the normalized green value (0.0-1.0) */
  def member_G = ApiMember(
    name = "G",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the normalized red value (0.0-1.0) */
  def member_R = ApiMember(
    name = "R",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Composes a new color using alpha blending */
  def member_blend = ApiMember(
    name = "blend",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the brightness component of the color. */
  def member_brightness = ApiMember(
    name = "brightness",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Makes a darker color by a delta between 0 and 1. */
  def member_darken = ApiMember(
    name = "darken",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Checks if the color is equal to the other */
  def member_equals = ApiMember(
    name = "equals",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the hue component of the color. */
  def member_hue = ApiMember(
    name = "hue",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Frequently used: Makes a lighter color by a delta between 0 and 1. */
  def member_lighten = ApiMember(
    name = "lighten",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Creates a new color by changing the alpha channel from 0 (transparent) to 1 (opaque). */
  def member_make_transparent = ApiMember(
    name = "make transparent",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the saturation component of the color. */
  def member_saturation = ApiMember(
    name = "saturation",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Convert color to HTML syntax (either #FF002A or rgba(255, 0, 42, 0.5) when A is non-1) */
  def member_to_html = ApiMember(
    name = "to html",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "A" -> member_A,
    "B" -> member_B,
    "G" -> member_G,
    "R" -> member_R,
    "blend" -> member_blend,
    "brightness" -> member_brightness,
    "darken" -> member_darken,
    "equals" -> member_equals,
    "hue" -> member_hue,
    "lighten" -> member_lighten,
    "make transparent" -> member_make_transparent,
    "saturation" -> member_saturation,
    "to html" -> member_to_html
  )
            

}
          
