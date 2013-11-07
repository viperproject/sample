
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

  val field_is_init = new TouchField("is init",TBoolean.typName)

  val field_horizontal_align = new TouchField("horizontal align",TString.typName)
  val field_vertical_align = new TouchField("vertical align",TString.typName)
  
  val field_left_horizontal_alignment = new TouchField("left horizontal alignment",TNumber.typName)
  val field_right_horizontal_alignment = new TouchField("right horizontal alignment",TNumber.typName)
  
  val field_left_vertical_alignment = new TouchField("left vertical alignment",TNumber.typName)
  val field_right_vertical_alignment = new TouchField("right vertical alignment",TNumber.typName)
  
  val field_font_size = new TouchField("font size",TNumber.typName)
  val field_font_weight = new TouchField("font weight",TString.typName)

  val field_top_margin = new TouchField("top margin",TNumber.typName)
  val field_left_margin = new TouchField("left margin",TNumber.typName)
  val field_right_margin = new TouchField("right margin",TNumber.typName)
  val field_bottom_margin = new TouchField("bottom margin",TNumber.typName)

  val field_bottom_padding = new TouchField("bottom padding",TNumber.typName)
  val field_top_padding = new TouchField("top padding",TNumber.typName)
  val field_left_padding = new TouchField("left padding",TNumber.typName)
  val field_right_padding = new TouchField("right padding",TNumber.typName)

  val field_background = new TouchField("background",TColor.typName)
  val field_foreground = new TouchField("foreground",TColor.typName)
  
  val field_border_color = new TouchField("border color",TColor.typName)
  val field_border_width_top = new TouchField("border width top",TNumber.typName)
  val field_border_width_right = new TouchField("border width right",TNumber.typName)
  val field_border_width_left = new TouchField("border width left",TNumber.typName)
  val field_border_width_bottom = new TouchField("border width bottom",TNumber.typName)
  
  val field_height = new TouchField("height",TNumber.typName)
  val field_width = new TouchField("width",TNumber.typName)

  val field_page_width = new TouchField("page width",TNumber.typName)
  val field_page_height = new TouchField("page height",TNumber.typName)

  val field_text_wrapping_wrap = new TouchField("text wrapping wrap",TBoolean.typName)
  val field_text_wrapping_minimumwidth = new TouchField("text wrapping minimumwidth",TNumber.typName)

  val field_horizontal_scrolling = new TouchField("horizontal scrolling ",TBoolean.typName)
  val field_vertical_scrolling = new TouchField("vertical scrolling",TBoolean.typName)

  val field_overlay_layout = new TouchField("overlay layout",TBoolean.typName)
  val field_horizontal_layout = new TouchField("horizontal layout",TBoolean.typName)
  val field_vertical_layout = new TouchField("vertical layout",TBoolean.typName)

  /** Specify how to compute box width (0 = shrink to fit content, 1 = stretch to fit frame, , 0.5 = stretch to half width) */
  val field_horizontal_stretch = new TouchField("horizontal stretch",TNumber.typName)
  val field_vertical_stretch = new TouchField("vertical stretch",TNumber.typName)

  val field_min_width = new TouchField("min width",TNumber.typName)
  val field_max_width = new TouchField("max width",TNumber.typName)

  val field_min_height = new TouchField("min height",TNumber.typName)
  val field_max_height = new TouchField("max height",TNumber.typName)

  // PRIVATE
  val field_text_edited_handler = new TouchField("text edited handler",TText_Action.typName)
  val field_text_editing_handler = new TouchField("text editing handler",TText_Action.typName)
  val field_tapped_handler = new TouchField("tapped handler",TAction.typName)

  val typName = "Box"
  val typ = new TouchType(typName,isSingleton = true, fields = List(
      field_is_init,
      field_vertical_align,
      field_horizontal_align,
      field_left_horizontal_alignment,
      field_right_horizontal_alignment,
      field_left_vertical_alignment,
      field_right_vertical_alignment,
      field_font_size,
      field_font_weight,
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
      field_border_width_top,
      field_border_width_right,
      field_border_width_left,
      field_border_width_bottom,
      field_height,
      field_width,
      field_page_width,
      field_page_height,
      field_text_wrapping_wrap,
      field_text_wrapping_minimumwidth,
      field_horizontal_scrolling,
      field_vertical_scrolling,
      field_overlay_layout,
      field_horizontal_layout,
      field_vertical_layout,
      field_horizontal_stretch,
      field_vertical_stretch,
      field_min_width,
      field_max_width,
      field_min_height,
      field_max_height,
      field_text_edited_handler,
      field_text_editing_handler,
      field_tapped_handler
  ))

}

class SBox extends AAny {

  def getTyp = SBox.typ

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Get the number of pixels in an em */
    case "pixels per em" =>
      Top[S](TNumber.typ)

    // OBSOLETE
    case "set horizontal alignment" =>
      var curState = state
      val List(left,right) = parameters
      curState = AssignField[S](this0,SBox.field_left_horizontal_alignment,left)(curState,pp)
      curState = AssignField[S](this0,SBox.field_right_horizontal_alignment,right)(curState,pp)
      curState

    // OBSOLETE
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

    /** Arrange boxes inside this box as layers on top of each other. */
    case "use overlay layout" =>
      AssignField[S](this0,SBox.field_overlay_layout,True)

    case "use horizontal layout" =>
      AssignField[S](this0,SBox.field_horizontal_layout,True)

    case "use vertical layout" =>
      AssignField[S](this0,SBox.field_vertical_layout,True)

    /** Display editable text. */
    case "edit text" =>
       val List(text,multiline) = parameters // String,Boolean
       Skip

    /** Display editable text, with the given binding. */
    case "edit" =>
      val List(style,value,changehandler) = parameters // String,String,Text_Action
      AssignField[S](this0,SBox.field_text_editing_handler,changehandler)

    /** Set what happens when the user has finished editing the text in the box. */
    case "on text edited" =>
      val List(handler) = parameters // Text_Action
      AssignField[S](this0,SBox.field_text_edited_handler,handler)

    /** Set what happens whenever the text in the box is being edited. */
    case "on text editing" =>
       val List(handler) = parameters // Text_Action
       AssignField[S](this0,SBox.field_text_editing_handler,handler)

    /** Set what happens when the box is tapped. */
    case "on tapped" =>
      val List(handler) = parameters // Action
      AssignField[S](this0,SBox.field_tapped_handler,handler)

    /** Set the color and width of the border. */
    case "set border" =>
      val List(color,width) = parameters // Color,Number
      var curState = state
      curState = AssignField[S](this0,SBox.field_border_color,color)(curState,pp)
      curState = AssignField[S](this0,SBox.field_border_width_top,width)(curState,pp)
      curState = AssignField[S](this0,SBox.field_border_width_right,width)(curState,pp)
      curState = AssignField[S](this0,SBox.field_border_width_bottom,width)(curState,pp)
      curState = AssignField[S](this0,SBox.field_border_width_left,width)(curState,pp)
      curState

    /** Set the width of each border. */
    case "set border widths" =>
      val List(top,right,bottom,left) = parameters // Number,Number,Number,Number
      var curState = state
      curState = AssignField[S](this0,SBox.field_border_width_top,top)(curState,pp)
      curState = AssignField[S](this0,SBox.field_border_width_right,right)(curState,pp)
      curState = AssignField[S](this0,SBox.field_border_width_bottom,bottom)(curState,pp)
      curState = AssignField[S](this0,SBox.field_border_width_left,left)(curState,pp)
      curState


    /** Set lower and upper limits on the height of this box. */
    case "set height range" =>
      val List(min_height,max_height) = parameters // Number,Number
      var curState = state
      curState = AssignField[S](this0,SBox.field_min_height,min_height)(curState,pp)
      curState = AssignField[S](this0,SBox.field_max_height,max_height)(curState,pp)
      curState

    /** Set lower and upper limits on the width of this box. */
    case "set width range" =>
      val List(min_width,max_width) = parameters // Number,Number
      var curState = state
      curState = AssignField[S](this0,SBox.field_min_width,min_width)(curState,pp)
      curState = AssignField[S](this0,SBox.field_max_width,max_width)(curState,pp)
      curState

    /** Specify whether to use scrollbars on overflow. */
    case "set scrolling" =>
      val List(horizontal_scrolling,vertical_scrolling) = parameters // Boolean,Boolean
      var curState = state
      curState = AssignField[S](this0,SBox.field_horizontal_scrolling,horizontal_scrolling)(curState,pp)
      curState = AssignField[S](this0,SBox.field_vertical_scrolling,vertical_scrolling)(curState,pp)
      curState

    /** Set whether to break long lines, and specify what length is too short for breaking */
    case "set text wrapping" =>
      val List(wrap,minimumwidth) = parameters // Boolean,Number
      var curState = state
      curState = AssignField[S](this0,SBox.field_text_wrapping_wrap,wrap)(curState,pp)
      curState = AssignField[S](this0,SBox.field_text_wrapping_minimumwidth,minimumwidth)(curState,pp)
      curState

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)

  }
}




      
