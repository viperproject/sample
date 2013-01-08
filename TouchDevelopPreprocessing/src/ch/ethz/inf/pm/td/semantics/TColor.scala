package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichExpression._

/**
 * User: lucas
 * Date: 11/8/12
 * Time: 6:13 PM
 */
object TColor {

  val field_A = new TouchField("A",TNumber.typ)
  val field_R = new TouchField("R",TNumber.typ)
  val field_G = new TouchField("G",TNumber.typ)
  val field_B = new TouchField("B",TNumber.typ)

  // we also treat hue,saturation,brightness as fields.
  // since we are not able to convert between reps, ARGB is Top if initialized with HSB,
  // and HSB is TOP if initialized with ARGB.
  // PRECISION: Implement HSB / ARGB conversion

  val field_hue = new TouchField("hue",TNumber.typ)
  val field_saturation = new TouchField("saturation",TNumber.typ)
  val field_brightness = new TouchField("brightness",TNumber.typ)

  val typName = "Color"
  val typ = TouchType(typName,isSingleton = false, List(field_A,field_R,field_B,field_G,field_hue,field_saturation,field_brightness))

}

class TColor extends AAny {

  def getTyp = TColor.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])(implicit pp:ProgramPoint,state:S):S = method match {

    /** Composes a new color using alpha blending */
    case "blend" =>
      val List(other) = parameters // Color
      val fraction = 1 - Field[S](other,TColor.field_A)
      val a = Field[S](this0,TColor.field_A)
      val r = (1 - fraction) * Field[S](this0,TColor.field_R) + fraction * Field[S](other,TColor.field_R)
      val g = (1 - fraction) * Field[S](this0,TColor.field_G) + fraction * Field[S](other,TColor.field_G)
      val b = (1 - fraction) * Field[S](this0,TColor.field_B) + fraction * Field[S](other,TColor.field_B)
      New[S](TColor.typ, a, r, g, b)

    /** Makes a darker color by a delta between 0 and 1. */
    case "darken" =>
      val List(delta) = parameters // Number
      CheckInRangeInclusive[S](delta,0,1,"darken","delta")
      // TODO: check if the following values might get negative
      val a = Field[S](this0,TColor.field_A)
      val r = Field[S](this0,TColor.field_R) - delta
      val g = Field[S](this0,TColor.field_G) - delta
      val b = Field[S](this0,TColor.field_B) - delta
      New[S](TColor.typ,a,r,g,b)

    /** Checks if the color is equal to the other */
    case "equals" =>
      val List(other) = parameters // Color
      val equalA = Field[S](this0,TColor.field_A) equal Field[S](other,TColor.field_A)
      val equalR = Field[S](this0,TColor.field_R) equal Field[S](other,TColor.field_R)
      val equalG = Field[S](this0,TColor.field_G) equal Field[S](other,TColor.field_G)
      val equalB = Field[S](this0,TColor.field_B) equal Field[S](other,TColor.field_B)
      Return(equalA && equalR && equalG && equalB)

    /** Makes a lighter color by a delta between 0 and 1. */
    case "lighten" =>
      val List(delta) = parameters // Number
      CheckInRangeInclusive[S](delta,0,1,"lighten","delta")
      // TODO: check if the following values might get negative
      val a = Field[S](this0,TColor.field_A)
      val r = Field[S](this0,TColor.field_R) + delta
      val g = Field[S](this0,TColor.field_G) + delta
      val b = Field[S](this0,TColor.field_B) + delta
      New[S](TColor.typ,a,r,g,b)

    /** Creates a new color by changing the alpha channel from 0 (transparent) to 1 (opaque). */
    case "make_transparent" =>
      val List(alpha) = parameters // Number
      CheckInRangeInclusive[S](alpha,0,1,"make_transparent","alpha")
      val r = Field[S](this0,TColor.field_R)
      val g = Field[S](this0,TColor.field_G)
      val b = Field[S](this0,TColor.field_B)
      New[S](TColor.typ,alpha,r,g,b)

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}