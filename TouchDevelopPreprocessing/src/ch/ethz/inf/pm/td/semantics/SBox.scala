
package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of box
 *
 * Based on "guessing" the semantics - no documentation available            
 *
 * @author Lucas Brutschy
 */

object SBox extends ASingleton {

  lazy val field_is_init = new ApiField("is init", TBoolean.typeName)

  lazy val field_horizontal_align = new ApiField("horizontal align", TString.typeName)
  lazy val field_vertical_align = new ApiField("vertical align", TString.typeName)

  lazy val field_left_horizontal_alignment = new ApiField("left horizontal alignment", TNumber.typeName)
  lazy val field_right_horizontal_alignment = new ApiField("right horizontal alignment", TNumber.typeName)

  lazy val field_left_vertical_alignment = new ApiField("left vertical alignment", TNumber.typeName)
  lazy val field_right_vertical_alignment = new ApiField("right vertical alignment", TNumber.typeName)

  lazy val field_font_size = new ApiField("font size", TNumber.typeName)
  lazy val field_font_weight = new ApiField("font weight", TString.typeName)

  lazy val field_top_margin = new ApiField("top margin", TNumber.typeName)
  lazy val field_left_margin = new ApiField("left margin", TNumber.typeName)
  lazy val field_right_margin = new ApiField("right margin", TNumber.typeName)
  lazy val field_bottom_margin = new ApiField("bottom margin", TNumber.typeName)

  lazy val field_bottom_padding = new ApiField("bottom padding", TNumber.typeName)
  lazy val field_top_padding = new ApiField("top padding", TNumber.typeName)
  lazy val field_left_padding = new ApiField("left padding", TNumber.typeName)
  lazy val field_right_padding = new ApiField("right padding", TNumber.typeName)

  lazy val field_background = new ApiField("background", TColor.typeName)
  lazy val field_foreground = new ApiField("foreground", TColor.typeName)

  lazy val field_border_color = new ApiField("border color", TColor.typeName)
  lazy val field_border_width_top = new ApiField("border width top", TNumber.typeName)
  lazy val field_border_width_right = new ApiField("border width right", TNumber.typeName)
  lazy val field_border_width_left = new ApiField("border width left", TNumber.typeName)
  lazy val field_border_width_bottom = new ApiField("border width bottom", TNumber.typeName)

  lazy val field_height = new ApiField("height", TNumber.typeName)
  lazy val field_width = new ApiField("width", TNumber.typeName)

  lazy val field_page_width = new ApiField("page width", TNumber.typeName)
  lazy val field_page_height = new ApiField("page height", TNumber.typeName)

  lazy val field_text_wrapping_wrap = new ApiField("text wrapping wrap", TBoolean.typeName)
  lazy val field_text_wrapping_minimumwidth = new ApiField("text wrapping minimumwidth", TNumber.typeName)

  lazy val field_horizontal_scrolling = new ApiField("horizontal scrolling ", TBoolean.typeName)
  lazy val field_vertical_scrolling = new ApiField("vertical scrolling", TBoolean.typeName)

  lazy val field_overlay_layout = new ApiField("overlay layout", TBoolean.typeName)
  lazy val field_horizontal_layout = new ApiField("horizontal layout", TBoolean.typeName)
  lazy val field_vertical_layout = new ApiField("vertical layout", TBoolean.typeName)

  /** Specify how to compute box width (0 = shrink to fit content, 1 = stretch to fit frame, , 0.5 = stretch to half width) */
  lazy val field_horizontal_stretch = new ApiField("horizontal stretch", TNumber.typeName)
  lazy val field_vertical_stretch = new ApiField("vertical stretch", TNumber.typeName)

  lazy val field_min_width = new ApiField("min width", TNumber.typeName)
  lazy val field_max_width = new ApiField("max width", TNumber.typeName)

  lazy val field_min_height = new ApiField("min height", TNumber.typeName)
  lazy val field_max_height = new ApiField("max height", TNumber.typeName)

  // PRIVATE
  lazy val field_text_edited_handler = new ApiField("text edited handler", TText_Action.typeName)
  lazy val field_text_editing_handler = new ApiField("text editing handler", TText_Action.typeName)
  lazy val field_tapped_handler = new ApiField("tapped handler", TAction.typeName)

  lazy val typeName = TypeName("Box")
  override def possibleFields = super.possibleFields ++ (List(
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

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Get the number of pixels in an em */
    case "pixels per em" =>
      Top[S](TNumber)

    // OBSOLETE
    case "set horizontal alignment" =>
      var curState = state
      val List(left, right) = parameters
      curState = AssignField[S](this0, SBox.field_left_horizontal_alignment, left)(curState, pp)
      curState = AssignField[S](this0, SBox.field_right_horizontal_alignment, right)(curState, pp)
      curState

    // OBSOLETE
    case "set vertical alignment" =>
      var curState = state
      val List(left, right) = parameters
      curState = AssignField[S](this0, SBox.field_left_vertical_alignment, left)(curState, pp)
      curState = AssignField[S](this0, SBox.field_right_vertical_alignment, right)(curState, pp)
      curState

    case "set margins" =>
      var curState = state
      val List(top, right, bottom, left) = parameters
      curState = AssignField[S](this0, SBox.field_top_margin, top)(curState, pp)
      curState = AssignField[S](this0, SBox.field_right_margin, right)(curState, pp)
      curState = AssignField[S](this0, SBox.field_bottom_margin, bottom)(curState, pp)
      curState = AssignField[S](this0, SBox.field_left_margin, left)(curState, pp)
      curState

    case "set padding" =>
      var curState = state
      val List(top, right, bottom, left) = parameters
      curState = AssignField[S](this0, SBox.field_top_padding, top)(curState, pp)
      curState = AssignField[S](this0, SBox.field_right_padding, right)(curState, pp)
      curState = AssignField[S](this0, SBox.field_bottom_padding, bottom)(curState, pp)
      curState = AssignField[S](this0, SBox.field_left_padding, left)(curState, pp)
      curState

    /** Arrange boxes inside this box as layers on top of each other. */
    case "use overlay layout" =>
      AssignField[S](this0, SBox.field_overlay_layout, True)

    case "use horizontal layout" =>
      AssignField[S](this0, SBox.field_horizontal_layout, True)

    case "use vertical layout" =>
      AssignField[S](this0, SBox.field_vertical_layout, True)

    /** Display editable text. */
    case "edit text" =>
      val List(text, multiline) = parameters // String,Boolean
      Skip

    /** Display editable text, with the given binding. */
    case "edit" =>
      val List(style, value, changehandler) = parameters // String,String,Text_Action
    val newState = AssignField[S](this0, SBox.field_text_editing_handler, changehandler)
      New[S](TEvent_Binding)(newState, pp)

    /** Set what happens when the user has finished editing the text in the box. */
    case "on text edited" =>
      val List(handler) = parameters // Text_Action
    val newState = AssignField[S](this0, SBox.field_text_edited_handler, handler)
      New[S](TEvent_Binding)(newState, pp)

    /** Set what happens whenever the text in the box is being edited. */
    case "on text editing" =>
      val List(handler) = parameters // Text_Action
    val newState = AssignField[S](this0, SBox.field_text_editing_handler, handler)
      New[S](TEvent_Binding)(newState, pp)

    /** Set what happens when the box is tapped. */
    case "on tapped" =>
      val List(handler) = parameters // Action
    val newState = AssignField[S](this0, SBox.field_tapped_handler, handler)
      New[S](TEvent_Binding)(newState, pp)

    /** Set the color and width of the border. */
    case "set border" =>
      val List(color, width) = parameters // Color,Number
    var curState = state
      curState = AssignField[S](this0, SBox.field_border_color, color)(curState, pp)
      curState = AssignField[S](this0, SBox.field_border_width_top, width)(curState, pp)
      curState = AssignField[S](this0, SBox.field_border_width_right, width)(curState, pp)
      curState = AssignField[S](this0, SBox.field_border_width_bottom, width)(curState, pp)
      curState = AssignField[S](this0, SBox.field_border_width_left, width)(curState, pp)
      curState

    /** Set the width of each border. */
    case "set border widths" =>
      val List(top, right, bottom, left) = parameters // Number,Number,Number,Number
    var curState = state
      curState = AssignField[S](this0, SBox.field_border_width_top, top)(curState, pp)
      curState = AssignField[S](this0, SBox.field_border_width_right, right)(curState, pp)
      curState = AssignField[S](this0, SBox.field_border_width_bottom, bottom)(curState, pp)
      curState = AssignField[S](this0, SBox.field_border_width_left, left)(curState, pp)
      curState


    /** Set lower and upper limits on the height of this box. */
    case "set height range" =>
      val List(min_height, max_height) = parameters // Number,Number
    var curState = state
      curState = AssignField[S](this0, SBox.field_min_height, min_height)(curState, pp)
      curState = AssignField[S](this0, SBox.field_max_height, max_height)(curState, pp)
      curState

    /** Set lower and upper limits on the width of this box. */
    case "set width range" =>
      val List(min_width, max_width) = parameters // Number,Number
    var curState = state
      curState = AssignField[S](this0, SBox.field_min_width, min_width)(curState, pp)
      curState = AssignField[S](this0, SBox.field_max_width, max_width)(curState, pp)
      curState

    /** Specify whether to use scrollbars on overflow. */
    case "set scrolling" =>
      val List(horizontal_scrolling, vertical_scrolling) = parameters // Boolean,Boolean
    var curState = state
      curState = AssignField[S](this0, SBox.field_horizontal_scrolling, horizontal_scrolling)(curState, pp)
      curState = AssignField[S](this0, SBox.field_vertical_scrolling, vertical_scrolling)(curState, pp)
      curState

    /** Set whether to break long lines, and specify what length is too short for breaking */
    case "set text wrapping" =>
      val List(wrap, minimumwidth) = parameters // Boolean,Number
    var curState = state
      curState = AssignField[S](this0, SBox.field_text_wrapping_wrap, wrap)(curState, pp)
      curState = AssignField[S](this0, SBox.field_text_wrapping_minimumwidth, minimumwidth)(curState, pp)
      curState

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}




      
