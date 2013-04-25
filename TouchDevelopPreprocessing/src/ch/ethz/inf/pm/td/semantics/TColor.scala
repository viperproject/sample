package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 *
 * STRIPPED DOWN FOR PERFORMANCE
 *
 * User: lucas
 * Date: 11/8/12
 * Time: 6:13 PM
 */
object TColor {

//  val field_A = new TouchField("A",TNumber.typ,ExpressionInitializer(0 ndTo 1))
//  val field_R = new TouchField("R",TNumber.typ,ExpressionInitializer(0 ndTo 1))
//  val field_G = new TouchField("G",TNumber.typ,ExpressionInitializer(0 ndTo 1))
//  val field_B = new TouchField("B",TNumber.typ,ExpressionInitializer(0 ndTo 1))
//
//  // we also treat hue,saturation,brightness as fields.
//  // since we are not able to convert between reps, ARGB is Top if initialized with HSB,
//  // and HSB is TOP if initialized with ARGB.
//  // PRECISION: Implement HSB / ARGB conversion
//
//  val field_hue = new TouchField("hue",TNumber.typ,ExpressionInitializer(0 ndTo 1))
//  val field_saturation = new TouchField("saturation",TNumber.typ,ExpressionInitializer(0 ndTo 1))
//  val field_brightness = new TouchField("brightness",TNumber.typ,ExpressionInitializer(0 ndTo 1))

  val typName = "Color"
  val typ = new TouchType(typName,isSingleton = false) //List(field_A,field_R,field_B,field_G,field_hue,field_saturation,field_brightness))

}

class TColor extends AAny {

  def getTyp = TColor.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)(implicit pp:ProgramPoint,state:S):S = method match {

    // It is too expensive and pointless to represent all colors. Every color just returns a value between 0 and 1

    case "blend" => Top[S](TColor.typ)
    case "darken" => Top[S](TColor.typ)
    case "equals" => Top[S](TColor.typ)
    case "lighten" => Top[S](TColor.typ)
    case "make transparent" => Top[S](TColor.typ)
    case "A" => Return[S](0 ndTo 1)
    case "R" => Return[S](0 ndTo 1)
    case "G" => Return[S](0 ndTo 1)
    case "B" => Return[S](0 ndTo 1)
    case "hue" => Return[S](0 ndTo 1)
    case "saturation" => Return[S](0 ndTo 1)
    case "brightness" => Return[S](0 ndTo 1)

//    /** Composes a new color using alpha blending */
//    case "blend" =>
//      val List(other) = parameters // Color
//      val fraction = 1 - Field[S](other,TColor.field_A)
//      val a = Field[S](this0,TColor.field_A)
//      val r = (1 - fraction) * Field[S](this0,TColor.field_R) + fraction * Field[S](other,TColor.field_R)
//      val g = (1 - fraction) * Field[S](this0,TColor.field_G) + fraction * Field[S](other,TColor.field_G)
//      val b = (1 - fraction) * Field[S](this0,TColor.field_B) + fraction * Field[S](other,TColor.field_B)
//      New[S](TColor.typ,Map(
//        TColor.field_A -> a,
//        TColor.field_R -> r,
//        TColor.field_G -> g,
//        TColor.field_B -> b
//      ))
//
//    /** Makes a darker color by a delta between 0 and 1. */
//    case "darken" =>
//      val List(delta) = parameters // Number
//      CheckInRangeInclusive[S](delta,0,1,"darken","delta")
//      // TODO: check if the following values might get negative
//      val a = Field[S](this0,TColor.field_A)
//      val r = Field[S](this0,TColor.field_R) - delta
//      val g = Field[S](this0,TColor.field_G) - delta
//      val b = Field[S](this0,TColor.field_B) - delta
//      New[S](TColor.typ,Map(
//        TColor.field_A -> a,
//        TColor.field_R -> r,
//        TColor.field_G -> g,
//        TColor.field_B -> b
//      ))
//
//    /** Checks if the color is equal to the other */
//    case "equals" =>
//      val List(other) = parameters // Color
//      val equalA = Field[S](this0,TColor.field_A) equal Field[S](other,TColor.field_A)
//      val equalR = Field[S](this0,TColor.field_R) equal Field[S](other,TColor.field_R)
//      val equalG = Field[S](this0,TColor.field_G) equal Field[S](other,TColor.field_G)
//      val equalB = Field[S](this0,TColor.field_B) equal Field[S](other,TColor.field_B)
//      Return(equalA && equalR && equalG && equalB)
//
//    /** Makes a lighter color by a delta between 0 and 1. */
//    case "lighten" =>
//      val List(delta) = parameters // Number
//      CheckInRangeInclusive[S](delta,0,1,"lighten","delta")
//      // TODO: check if the following values might get negative
//      val a = Field[S](this0,TColor.field_A)
//      val r = Field[S](this0,TColor.field_R) + delta
//      val g = Field[S](this0,TColor.field_G) + delta
//      val b = Field[S](this0,TColor.field_B) + delta
//      New[S](TColor.typ,Map(
//        TColor.field_A -> a,
//        TColor.field_R -> r,
//        TColor.field_G -> g,
//        TColor.field_B -> b
//      ))
//
//    /** Creates a new color by changing the alpha channel from 0 (transparent) to 1 (opaque). */
//    case "make transparent" =>
//      val List(alpha) = parameters // Number
//      CheckInRangeInclusive[S](alpha,0,1,"make transparent","alpha")
//      val r = Field[S](this0,TColor.field_R)
//      val g = Field[S](this0,TColor.field_G)
//      val b = Field[S](this0,TColor.field_B)
//      New[S](TColor.typ,Map(
//        TColor.field_A -> alpha,
//        TColor.field_R -> r,
//        TColor.field_G -> g,
//        TColor.field_B -> b
//      ))

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}