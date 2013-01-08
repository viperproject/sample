package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint

/**
 * Specifies the abstract semantics of TextBox
 *
 * A text box
 *
 * @author Lucas Brutschy
 */

object TTextBox {

  val field_background = new TouchField("background",TColor.typ)
  val field_border = new TouchField("border",TColor.typ)
  val field_font_size = new TouchField("font_size",TNumber.typ)
  val field_foreground = new TouchField("foreground",TColor.typ)
  val field_icon = new TouchField("icon",TPicture.typ)
  val field_text = new TouchField("text",TString.typ)

  val typName = "TextBox"
  val typ = TouchType(typName,isSingleton = false,List(field_background,field_border,field_font_size,field_foreground,
    field_icon,field_text))

}

class TTextBox extends AAny {

  def getTyp = TTextBox.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet])
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Sets the font size (small = 14, normal = 15, medium = 17, medium large = 19, large = 24, extra large = 32,
      * extra extra large = 54, huge = 140 */
     case "set_font_size" =>
       val List(size) = parameters // Number
       CheckInRangeInclusive[S](size,14,140,"set_font_size","size")
       super.forwardSemantics(this0,method,parameters) // Handle setter

    /** Sets the icon picture (max 96 x 96) */
    case "set_icon" =>
       val List(pic) = parameters // Picture
       CheckInRangeInclusive[S](Field[S](pic,TPicture.field_width),0,96,"set_icon","Icon Width")
       CheckInRangeInclusive[S](Field[S](pic,TPicture.field_height),0,96,"set_icon","Icon Height")
       super.forwardSemantics(this0,method,parameters) // Handle setter

    case _ =>
      super.forwardSemantics(this0,method,parameters)

  }
}