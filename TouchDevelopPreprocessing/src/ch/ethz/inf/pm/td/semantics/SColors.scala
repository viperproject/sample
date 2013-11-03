package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.semantics.RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

/**
 * Specifies the abstract semantics of colors
 *
 * New or built-in colors
 *
 * @author Lucas Brutschy
 */

object SColors {

  // STRIPPED DOWN FOR PERFORMANCE

  // Indicates if the user is using a light theme in his phone
  val field_is_light_theme = new TouchField("is light theme",TBoolean.typName)

  val typName = "Colors"
  val typ = new TouchType(typName, isSingleton = true, fields = List(field_is_light_theme))

}

class SColors extends AAny {

  def getTyp = SColors.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    // Gets the accent color in the current theme
    case "accent" => Top[S](TColor.typ)
//      New(TColor.typ,Map(
//        TColor.field_A -> 1
//      ))

    // Gets the background color in the current theme
    case "background" => Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1
//      ))

    // Gets the color that has the ARGB value of #FF000000
    case "black" => Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 0,
//        TColor.field_G -> 0,
//        TColor.field_B -> 0
//      ))

    // Gets the color that has the ARGB value of #FF0000FF
    case "blue" =>  Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 0,
//        TColor.field_G -> 0,
//        TColor.field_B -> 1
//      ))

    // Gets the color that has the ARGB value of #FFA52A2A
    case "brown" => Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 0.647,
//        TColor.field_G ->  0.165,
//        TColor.field_B ->  0.165
//      )) // TODO: Precision?

    // Gets the chrome color in the current theme (control background)
    case "chrome" => Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1
//      ))

    // Gets the color that has the ARGB value of #FF00FFFF
    case "cyan" => Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 0,
//        TColor.field_G -> 1,
//        TColor.field_B -> 1
//      ))


    case "equals" =>
      Top[S](TBoolean.typ)

    // Gets the color that has the ARGB value of #FFA9A9A9
    case "dark gray" => Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 0.663,
//        TColor.field_G -> 0.663,
//        TColor.field_B -> 0.663
//      )) // TODO: Precision?

    // Gets the foreground color in the current theme
    case "foreground" => Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1
//      ))

    // Creates a color from the alpha, hue, saturation, brightness channels (0.0-1.0 range)
    case "from ahsb" =>
      val List(a,h,s,b) = parameters
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](a,0,1,"from ahsb","alpha")
        CheckInRangeInclusive[S](h,0,1,"from ahsb","hue")
        CheckInRangeInclusive[S](s,0,1,"from ahsb","saturation")
        CheckInRangeInclusive[S](b,0,1,"from ahsb","brightness")
      }
      Top[S](TColor.typ)
      // PRECISION: COMPUTE RGB
//      New(TColor.typ,Map(
//        TColor.field_A -> a,
//        TColor.field_hue -> h,
//        TColor.field_saturation -> s,
//        TColor.field_brightness -> b
//      ))

    // Creates a color from the alpha, red, green, blue channels (0.0-1.0 range)
    case "from argb" =>
      val List(a,r,g,b) = parameters
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](a,0,1,"from argb","alpha")
        CheckInRangeInclusive[S](r,0,1,"from argb","red")
        CheckInRangeInclusive[S](g,0,1,"from argb","green")
        CheckInRangeInclusive[S](b,0,1,"from argb","blue")
      }
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//       TColor.field_A -> a,
//       TColor.field_R -> r,
//       TColor.field_G -> g,
//       TColor.field_B -> b
//      ))

    // Creates a color from the hue, saturation, brightness channels (0.0-1.0 range)
    case "from hsb" =>
      val List(h,s,b) = parameters
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](h,0,1,"from hsb","hue")
        CheckInRangeInclusive[S](s,0,1,"from hsb","saturation")
        CheckInRangeInclusive[S](b,0,1,"from hsb","brightness")
      }
      Top[S](TColor.typ)
      // PRECISION: Compute RGB
//     Top[S](TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_hue -> h,
//        TColor.field_saturation -> s,
//        TColor.field_brightness -> b
//      ))

    // Creates a color from the red, green, blue channels (0.0-1.0 range)
    case "from rgb" =>
      val List(r,g,b) = parameters
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](r,0,1,"from rgb","red")
        CheckInRangeInclusive[S](g,0,1,"from rgb","green")
        CheckInRangeInclusive[S](b,0,1,"from rgb","blue")
      }
      Top[S](TColor.typ)
//      Top[S](TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> r,
//        TColor.field_G -> g,
//        TColor.field_B -> b
//      ))

    case "gray" => // Gets the color that has the ARGB value of #FF808080
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 0.502,
//        TColor.field_G -> 0.502,
//        TColor.field_B -> 0.502
//      )) // TODO: Precision?

    // Gets the color that has the ARGB value of #FF008000
    case "green" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 0,
//        TColor.field_G -> 0.502,
//        TColor.field_B -> 0
//      )) // TODO: Precision?

    //Gets the color that has the ARGB value of #FFD3D3D3
    case "light gray" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 0.827,
//        TColor.field_G -> 0.827,
//        TColor.field_B -> 0.827
//      )) // TODO: Precision?

    // Computes an intermediate color
    case "linear gradient" =>
      val List(colA,colB,frac) = parameters
      if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
        CheckInRangeInclusive[S](frac,0,1,"linear gradient","fraction")
      }
      Top[S](TColor.typ)
//      val frac1 = toRichExpression(1) - frac
//      val a = frac * Field[S](colA,TColor.field_A) + frac1 * Field[S](colB,TColor.field_A)
//      val r = frac * Field[S](colA,TColor.field_R) + frac1 * Field[S](colB,TColor.field_R)
//      val g = frac * Field[S](colA,TColor.field_G) + frac1 * Field[S](colB,TColor.field_G)
//      val b = frac * Field[S](colA,TColor.field_B) + frac1 * Field[S](colB,TColor.field_B)
//     New(TColor.typ,Map(
//        TColor.field_A -> a,
//        TColor.field_R -> r,
//        TColor.field_G -> g,
//        TColor.field_B -> b
//      ))

    //Gets the color that has the ARGB value of #FFFF00FF
    case "magenta" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 1,
//        TColor.field_G -> 0,
//        TColor.field_B -> 1
//      ))

    // Gets the color that has the ARGB value of #FFFFA500
    case "orange" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 1,
//        TColor.field_G -> 0.647,
//        TColor.field_B -> 0
//      )) // TODO: Precision?

    /** Gets the color that has the ARGB value of #FFFFCBDB */
    case "pink" =>
      Top[S](TColor.typ)

    // Gets the color that has the ARGB value of #FF800080
    case "purple" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 0.502,
//        TColor.field_G -> 0,
//        TColor.field_B -> 0.502
//      )) // TODO: Precision?

    // Picks a random color                                                                       -
    case "random" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1
//      ))

    // Picks a random color (OBSOLETE)
    case "rand" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1
//      ))

    // Gets the color that has the ARGB value of #FFFF0000
    case "red" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 1,
//        TColor.field_G -> 0,
//        TColor.field_B -> 0
//      ))

    // Gets the color that has the ARGB value of #FF704214
    case "sepia" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 0.439,
//        TColor.field_G -> 0.259,
//        TColor.field_B -> 0.078
//      )) // TODO: Precision?

    // Gets the subtle color in the current theme (light gray)
    case "subtle" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1
//      ))

    // Gets the color that has the ARGB value of #00FFFFFF
    case "transparent" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 0,
//        TColor.field_R -> 1,
//        TColor.field_G -> 1,
//        TColor.field_B -> 1
//      ))

    // Gets the color that has the ARGB value of #FFFFFFFF
    case "white" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 1,
//        TColor.field_G -> 1,
//        TColor.field_B -> 1
//      ))

    // Gets the color that has the ARGB value of #FFFFFF00
    case "yellow" =>
      Top[S](TColor.typ)
//     New(TColor.typ,Map(
//        TColor.field_A -> 1,
//        TColor.field_R -> 1,
//        TColor.field_G -> 1,
//        TColor.field_B -> 0
//      ))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }

}