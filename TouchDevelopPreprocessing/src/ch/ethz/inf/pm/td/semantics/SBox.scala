
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of box
 *
 * Based on "guessing" the semantics - no documentation available            
 *
 * @author Lucas Brutschy
 */ 

object SBox {

  val field_is_init = new TouchField("is init",TBoolean.typ)
  
  val field_left_horizontal_alignment = new TouchField("left horizontal alignment",TNumber.typ)
  val field_right_horizontal_alignment = new TouchField("right horizontal alignment",TNumber.typ)
  
  val field_left_vertical_alignment = new TouchField("left vertical alignment",TNumber.typ)
  val field_right_vertical_alignment = new TouchField("right vertical alignment",TNumber.typ)
  
  val field_font_size = new TouchField("font size",TNumber.typ)

  val field_top_margin = new TouchField("top margin",TNumber.typ)
  val field_left_margin = new TouchField("left margin",TNumber.typ)
  val field_right_margin = new TouchField("right margin",TNumber.typ)
  val field_bottom_margin = new TouchField("bottom margin",TNumber.typ)

  val field_bottom_padding = new TouchField("bottom padding",TNumber.typ)
  val field_top_padding = new TouchField("top padding",TNumber.typ)
  val field_left_padding = new TouchField("left padding",TNumber.typ)
  val field_right_padding = new TouchField("right padding",TNumber.typ)

  val field_background = new TouchField("background",TColor.typ)
  val field_foreground = new TouchField("foreground",TColor.typ)
  
  val field_border_color = new TouchField("border color",TColor.typ)
  val field_border_width = new TouchField("border width",TNumber.typ)
  
  val field_height = new TouchField("height",TNumber.typ)
  val field_width = new TouchField("width",TNumber.typ)

  val field_page_width = new TouchField("page width",TNumber.typ)
  val field_page_height = new TouchField("page height",TNumber.typ)

  val field_text_wrapping_wrap = new TouchField("text wrapping wrap",TBoolean.typ)
  val field_text_wrapping_minimumwidth = new TouchField("text wrapping minimumwidth",TNumber.typ)

  val field_horizontal_scrolling = new TouchField("horizontal scrolling ",TBoolean.typ)
  val field_vertical_scrolling = new TouchField("vertical scrolling",TBoolean.typ)

  val field_horizontal_layout = new TouchField("horizontal layout",TBoolean.typ)
  val field_vertical_layout = new TouchField("vertical layout",TBoolean.typ)
  

  val typName = "Box"
  val typ = new TouchType(typName,isSingleton = true, fields = List(
      field_is_init,
      field_left_horizontal_alignment,
      field_right_horizontal_alignment,
      field_left_vertical_alignment,
      field_right_vertical_alignment,
      field_font_size,
      field_top_margin,
      field_left_margin,
      field_right_margin,
      field_bottom_margin,
      field_bottom_padding,
      field_top_padding,
      field_left_padding,
      field_right_padding,
      field_background,
      field_foreground,
      field_border_color,
      field_border_width,
      field_height,
      field_width,
      field_page_width,
      field_page_height,
      field_text_wrapping_wrap,
      field_text_wrapping_minimumwidth,
      field_horizontal_scrolling,
      field_vertical_scrolling,
      field_horizontal_layout,
      field_vertical_layout
  ))

}

class SBox extends AAny {

  def getTyp = SBox.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {


    case "set horizontal alignment" =>
      var curState = state
      val List(left,right) = parameters
      curState = AssignField[S](this0,SBox.field_left_horizontal_alignment,left)(curState,pp)
      curState = AssignField[S](this0,SBox.field_right_horizontal_alignment,right)(curState,pp)
      curState

    case "set vertical alignment" =>
      var curState = state
      val List(left,right) = parameters
      curState = AssignField[S](this0,SBox.field_left_vertical_alignment,left)(curState,pp)
      curState = AssignField[S](this0,SBox.field_right_vertical_alignment,right)(curState,pp)
      curState


    case "set margins" =>
      var curState = state
      val List(top,right,bottom,left) = parameters
      curState = AssignField[S](this0,SBox.field_top_margin,top)(curState,pp)
      curState = AssignField[S](this0,SBox.field_right_margin,right)(curState,pp)
      curState = AssignField[S](this0,SBox.field_bottom_margin,bottom)(curState,pp)
      curState = AssignField[S](this0,SBox.field_left_margin,left)(curState,pp)
      curState

    case "set padding" =>
      var curState = state
      val List(top,right,bottom,left) = parameters
      curState = AssignField[S](this0,SBox.field_top_padding,top)(curState,pp)
      curState = AssignField[S](this0,SBox.field_right_padding,right)(curState,pp)
      curState = AssignField[S](this0,SBox.field_bottom_padding,bottom)(curState,pp)
      curState = AssignField[S](this0,SBox.field_left_padding,left)(curState,pp)
      curState

    case "use horizontal layout" =>
      AssignField[S](this0,SBox.field_vertical_layout,True)

    case "use vertical layout" =>
      AssignField[S](this0,SBox.field_vertical_layout,True)

    case "on tapped" =>
      Skip

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}




      
