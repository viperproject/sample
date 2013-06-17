package ch.ethz.inf.pm.td.semantics

import RichNativeSemantics._
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters

/**
 * Specifies the abstract semantics of TextBox
 *
 * A text box
 *
 * @author Lucas Brutschy
 */

object TTextBox {

  val field_background = new TouchField("background",TColor.typName)
  val field_border = new TouchField("border",TColor.typName)
  val field_font_size = new TouchField("font size",TNumber.typName)
  val field_foreground = new TouchField("foreground",TColor.typName)
  val field_icon = new TouchField("icon",TPicture.typName)
  val field_text = new TouchField("text",TString.typName)

  val typName = "TextBox"
  val typ = new TouchType(typName,isSingleton = false, fields = List(field_background,field_border,field_font_size,field_foreground,
    field_icon,field_text))

}

class TTextBox extends AAny {

  def getTyp = TTextBox.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Sets the font size (small = 14, normal = 15, medium = 17, medium large = 19, large = 24, extra large = 32,
      * extra extra large = 54, huge = 140 */
     case "set font size" =>
       val List(size) = parameters // Number
       //if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
       // CheckInRangeInclusive[S](size,14,140,"set font size","size") THIS IS NOT VALID ANYMORE
       //}
       super.forwardSemantics(this0,method,parameters,returnedType) // Handle setter

    /** Sets the icon picture (max 96 x 96) */
    case "set icon" =>
       val List(pic) = parameters // Picture
       if (TouchAnalysisParameters.reportNoncriticalParameterBoundViolations) {
         CheckInRangeInclusive[S](Field[S](pic,TPicture.field_width),0,96,"set icon","Icon Width")
         CheckInRangeInclusive[S](Field[S](pic,TPicture.field_height),0,96,"set icon","Icon Height")
       }
       super.forwardSemantics(this0,method,parameters,returnedType) // Handle setter

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}