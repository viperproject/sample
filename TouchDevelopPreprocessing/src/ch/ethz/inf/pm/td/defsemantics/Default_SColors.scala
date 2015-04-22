
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Colors
 *
 * New or built-in colors
 *
 * @author Lucas Brutschy
 */

trait Default_SColors extends ASingleton {

  lazy val typeName = TypeName("Colors", isSingleton = true)
          
  /** Frequently used: Gets the accent color in the current theme */
  def member_accent = ApiMember(
    name = "accent",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the background color in the current theme */
  def member_background = ApiMember(
    name = "background",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the color that has the ARGB value of #FF000000 */
  def member_black = ApiMember(
    name = "black",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the color that has the ARGB value of #FF0000FF */
  def member_blue = ApiMember(
    name = "blue",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the color that has the ARGB value of #FFA52A2A */
  def member_brown = ApiMember(
    name = "brown",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the chrome color in the current theme (control background) */
  def member_chrome = ApiMember(
    name = "chrome",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the color that has the ARGB value of #FF00FFFF */
  def member_cyan = ApiMember(
    name = "cyan",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the color that has the ARGB value of #FFA9A9A9 */
  def member_dark_gray = ApiMember(
    name = "dark gray",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the foreground color in the current theme */
  def member_foreground = ApiMember(
    name = "foreground",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Rarely used: Creates a color from the alpha, hue, saturation, brightness channels (0.0-1.0 range) */
  def member_from_ahsb = ApiMember(
    name = "from ahsb",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a color from the alpha, red, green, blue channels (0.0-1.0 range) */
  def member_from_argb = ApiMember(
    name = "from argb",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a color from the hue, saturation, brightness channels (0.0-1.0 range) */
  def member_from_hsb = ApiMember(
    name = "from hsb",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Creates a color from the red, green, blue channels (0.0-1.0 range) */
  def member_from_rgb = ApiMember(
    name = "from rgb",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the color that has the ARGB value of #FF808080 */
  def member_gray = ApiMember(
    name = "gray",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the color that has the ARGB value of #FF008000 */
  def member_green = ApiMember(
    name = "green",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates if the user is using a light theme in their phone */
  def member_is_light_theme = ApiMember(
    name = "is light theme",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the color that has the ARGB value of #FFD3D3D3 */
  def member_light_gray = ApiMember(
    name = "light gray",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Computes an intermediate color */
  def member_linear_gradient = ApiMember(
    name = "linear gradient",
    paramTypes = List(ApiParam(TColor), ApiParam(TColor), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the color that has the ARGB value of #FFFF00FF */
  def member_magenta = ApiMember(
    name = "magenta",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the color that has the ARGB value of #FFFFA500 */
  def member_orange = ApiMember(
    name = "orange",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the color that has the ARGB value of #FFFFCBDB */
  def member_pink = ApiMember(
    name = "pink",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the color that has the ARGB value of #FF800080 */
  def member_purple = ApiMember(
    name = "purple",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Renamed to 'random' */
  def member_rand = ApiMember(
    name = "rand",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Picks a random color */
  def member_random = ApiMember(
    name = "random",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the color that has the ARGB value of #FFFF0000 */
  def member_red = ApiMember(
    name = "red",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the color that has the ARGB value of #FF704214 */
  def member_sepia = ApiMember(
    name = "sepia",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the subtle color in the current theme (light gray) */
  def member_subtle = ApiMember(
    name = "subtle",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the color that has the ARGB value of #00FFFFFF */
  def member_transparent = ApiMember(
    name = "transparent",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Never used: Picks a color from a color wheel where the hue is between 0 and 1. */
  def member_wheel = ApiMember(
    name = "wheel",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the color that has the ARGB value of #FFFFFFFF */
  def member_white = ApiMember(
    name = "white",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the color that has the ARGB value of #FFFFFF00 */
  def member_yellow = ApiMember(
    name = "yellow",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TColor,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "accent" -> member_accent,
    "background" -> member_background,
    "black" -> member_black,
    "blue" -> member_blue,
    "brown" -> member_brown,
    "chrome" -> member_chrome,
    "cyan" -> member_cyan,
    "dark gray" -> member_dark_gray,
    "foreground" -> member_foreground,
    "from ahsb" -> member_from_ahsb,
    "from argb" -> member_from_argb,
    "from hsb" -> member_from_hsb,
    "from rgb" -> member_from_rgb,
    "gray" -> member_gray,
    "green" -> member_green,
    "is light theme" -> member_is_light_theme,
    "light gray" -> member_light_gray,
    "linear gradient" -> member_linear_gradient,
    "magenta" -> member_magenta,
    "orange" -> member_orange,
    "pink" -> member_pink,
    "purple" -> member_purple,
    "rand" -> member_rand,
    "random" -> member_random,
    "red" -> member_red,
    "sepia" -> member_sepia,
    "subtle" -> member_subtle,
    "transparent" -> member_transparent,
    "wheel" -> member_wheel,
    "white" -> member_white,
    "yellow" -> member_yellow
  )
            

}
          
