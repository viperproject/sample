package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichExpression._

/**
 * Specifies the abstract semantics of colors
 *
 * New or built-in colors
 *
 * @author Lucas Brutschy
 */

object SColors {

  val typName = "colors"
  val typ = TouchType(typName, isSingleton = true)

}

class SColors extends AAny {

  def getTyp = SColors.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    // Gets the accent color in the current theme
    case "accent" =>
      New(TColor.typ,1,0 to 1,0 to 1,0 to 1)

    // Gets the background color in the current theme
    case "background" =>
      New(TColor.typ,1,0 to 1,0 to 1,0 to 1)

    // Gets the color that has the ARGB value of #FF000000
    case "black" =>
      New(TColor.typ,1,0,0,0)

    // Gets the color that has the ARGB value of #FF0000FF
    case "blue" =>
      New(TColor.typ,1,0,0,1)

    // Gets the color that has the ARGB value of #FFA52A2A
    case "brown" =>
      New(TColor.typ,1,0.647, 0.165, 0.165) // TODO: Precision?

    // Gets the chrome color in the current theme (control background)
    case "chrome" =>
      New(TColor.typ,1,0 to 1,0 to 1,0 to 1)

    // Gets the color that has the ARGB value of #FF00FFFF
    case "cyan" =>
      New(TColor.typ,1,0,1,1)

    // Gets the color that has the ARGB value of #FFA9A9A9
    case "dark_gray" =>
      New(TColor.typ,1,0.663,0.663,0.663) // TODO: Precision?

    // Gets the foreground color in the current theme
    case "foreground" =>
      New(TColor.typ,1,0 to 1,0 to 1,0 to 1)

    // Creates a color from the alpha, hue, saturation, brightness channels (0.0-1.0 range)
    case "from_ahsb" =>
      val List(a,h,s,b) = parameters
      CheckInRangeInclusive[S](a,0,1,"from_ahsb","alpha")
      CheckInRangeInclusive[S](h,0,1,"from_ahsb","hue")
      CheckInRangeInclusive[S](s,0,1,"from_ahsb","saturation")
      CheckInRangeInclusive[S](b,0,1,"from_ahsb","brightness")
      // PRECISION: COMPUTE RGB
      New(TColor.typ,a,0 to 1,0 to 1,0 to 1,h,s,b)

    // Creates a color from the alpha, red, green, blue channels (0.0-1.0 range)
    case "from_argb" =>
      val List(a,r,g,b) = parameters
      CheckInRangeInclusive[S](a,0,1,"from_argb","alpha")
      CheckInRangeInclusive[S](r,0,1,"from_argb","red")
      CheckInRangeInclusive[S](g,0,1,"from_argb","green")
      CheckInRangeInclusive[S](b,0,1,"from_argb","blue")
      New(TColor.typ,a,r,g,b)(state,pp)

    // Creates a color from the hue, saturation, brightness channels (0.0-1.0 range)
    case "from_hsb" =>
      val List(h,s,b) = parameters
      CheckInRangeInclusive[S](h,0,1,"from_hsb","hue")
      CheckInRangeInclusive[S](s,0,1,"from_hsb","saturation")
      CheckInRangeInclusive[S](b,0,1,"from_hsb","brightness")
      // PRECISION: Compute RGB
      New(TColor.typ,1,0 to 1,0 to 1,0 to 1,h,s,b)

    // Creates a color from the red, green, blue channels (0.0-1.0 range)
    case "from_rgb" =>
      val List(r,g,b) = parameters
      CheckInRangeInclusive[S](r,0,1,"from_rgb","red")
      CheckInRangeInclusive[S](g,0,1,"from_rgb","green")
      CheckInRangeInclusive[S](b,0,1,"from_rgb","blue")
      New(TColor.typ,1,r,g,b)

    case "gray" => // Gets the color that has the ARGB value of #FF808080
      New(TColor.typ,1,0.502, 0.502, 0.502) // TODO: Precision?

    // Gets the color that has the ARGB value of #FF008000
    case "green" =>
      New(TColor.typ,1,0, 0.502, 0) // TODO: Precision?

    // Indicates if the user is using a light theme in his phone
    case "is_light_theme" =>
      Return(Environment.isLightTheme)

    //Gets the color that has the ARGB value of #FFD3D3D3
    case "light_gray" =>
      New(TColor.typ, 1, 0.827, 0.827, 0.827) // TODO: Precision?

    // Computes an intermediate color
    case "linear_gradient" =>
      val List(colA,colB,frac) = parameters
      CheckInRangeInclusive[S](frac,0,1,"linear_gradient","fraction")
      val frac1 = toRichExpression(1) - frac
      val a = frac * Field[S](colA,TColor.field_A) + frac1 * Field[S](colB,TColor.field_A)
      val r = frac * Field[S](colA,TColor.field_R) + frac1 * Field[S](colB,TColor.field_R)
      val g = frac * Field[S](colA,TColor.field_G) + frac1 * Field[S](colB,TColor.field_G)
      val b = frac * Field[S](colA,TColor.field_B) + frac1 * Field[S](colB,TColor.field_B)
      New(TColor.typ,a,r,g,b)

    //Gets the color that has the ARGB value of #FFFF00FF
    case "magenta" =>
      New(TColor.typ,1,1,0,1)

    // Gets the color that has the ARGB value of #FFFFA500
    case "orange" =>
      New(TColor.typ,1,1,0.647,0) // TODO: Precision?

    // Gets the color that has the ARGB value of #FF800080
    case "purple" =>
      New(TColor.typ,1,0.502,0,0.502) // TODO: Precision?

    // Picks a random color                                                                       -
    case "random" =>
      New(TColor.typ,1,0 to 1,0 to 1,0 to 1)

    // Picks a random color (OBSOLETE)
    case "rand" =>
      New(TColor.typ,1,0 to 1,0 to 1,0 to 1)

    // Gets the color that has the ARGB value of #FFFF0000
    case "red" =>
      New(TColor.typ,1,1,0,0)

    // Gets the color that has the ARGB value of #FF704214
    case "sepia" =>
      New(TColor.typ,1,0.439,0.259,0.078) // TODO: Precision?

    // Gets the subtle color in the current theme (light gray)
    case "subtle" =>
      New(TColor.typ,1,0 to 1,0 to 1,0 to 1)

    // Gets the color that has the ARGB value of #00FFFFFF
    case "transparent" =>
      New(TColor.typ,0,1,1,1)

    // Gets the color that has the ARGB value of #FFFFFFFF
    case "white" =>
      New(TColor.typ,1,1,1,1)

    // Gets the color that has the ARGB value of #FFFFFF00
    case "yellow" =>
      New(TColor.typ,1,1,1,0)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }

}