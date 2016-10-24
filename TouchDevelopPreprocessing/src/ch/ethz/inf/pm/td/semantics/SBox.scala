
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{SkipSemantics, TouchType}
import ch.ethz.inf.pm.td.defsemantics.Default_SBox
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of box
 *
 * Based on "guessing" the semantics - no documentation available            
 *
 * @author Lucas Brutschy
 */

object SBox extends Default_SBox {

  lazy val field_is_init = ApiField("is init", TBoolean)

  lazy val field_horizontal_align = ApiField("horizontal align", TString)
  lazy val field_vertical_align = ApiField("vertical align", TString)

  lazy val field_left_horizontal_alignment = ApiField("left horizontal alignment", TNumber)
  lazy val field_right_horizontal_alignment = ApiField("right horizontal alignment", TNumber)

  lazy val field_left_vertical_alignment = ApiField("left vertical alignment", TNumber)
  lazy val field_right_vertical_alignment = ApiField("right vertical alignment", TNumber)

  lazy val field_font_size = ApiField("font size", TNumber)
  lazy val field_font_weight = ApiField("font weight", TString)

  lazy val field_top_margin = ApiField("top margin", TNumber)
  lazy val field_left_margin = ApiField("left margin", TNumber)
  lazy val field_right_margin = ApiField("right margin", TNumber)
  lazy val field_bottom_margin = ApiField("bottom margin", TNumber)

  lazy val field_bottom_padding = ApiField("bottom padding", TNumber)
  lazy val field_top_padding = ApiField("top padding", TNumber)
  lazy val field_left_padding = ApiField("left padding", TNumber)
  lazy val field_right_padding = ApiField("right padding", TNumber)

  lazy val field_background = ApiField("background", TColor)
  lazy val field_foreground = ApiField("foreground", TColor)

  lazy val field_border_color = ApiField("border color", TColor)
  lazy val field_border_width_top = ApiField("border width top", TNumber)
  lazy val field_border_width_right = ApiField("border width right", TNumber)
  lazy val field_border_width_left = ApiField("border width left", TNumber)
  lazy val field_border_width_bottom = ApiField("border width bottom", TNumber)

  lazy val field_height = ApiField("height", TNumber)
  lazy val field_width = ApiField("width", TNumber)

  lazy val field_page_width = ApiField("page width", TNumber)
  lazy val field_page_height = ApiField("page height", TNumber)

  lazy val field_text_wrapping_wrap = ApiField("text wrapping wrap", TBoolean)
  lazy val field_text_wrapping_minimumwidth = ApiField("text wrapping minimumwidth", TNumber)

  lazy val field_horizontal_scrolling = ApiField("horizontal scrolling ", TBoolean)
  lazy val field_vertical_scrolling = ApiField("vertical scrolling", TBoolean)

  lazy val field_overlay_layout = ApiField("overlay layout", TBoolean)
  lazy val field_horizontal_layout = ApiField("horizontal layout", TBoolean)
  lazy val field_vertical_layout = ApiField("vertical layout", TBoolean)

  /** Specify how to compute box width (0 = shrink to fit content, 1 = stretch to fit frame, , 0.5 = stretch to half width) */
  lazy val field_horizontal_stretch = ApiField("horizontal stretch", TNumber)
  lazy val field_vertical_stretch = ApiField("vertical stretch", TNumber)

  lazy val field_min_width = ApiField("min width", TNumber)
  lazy val field_max_width = ApiField("max width", TNumber)

  lazy val field_min_height = ApiField("min height", TNumber)
  lazy val field_max_height = ApiField("max height", TNumber)

  // PRIVATE
  lazy val field_text_edited_handler = ApiField("text edited handler", TText_Action)
  lazy val field_text_editing_handler = ApiField("text editing handler", TText_Action)
  lazy val field_tapped_handler = ApiField("tapped handler", TAction)

  override def member_on_text_edited =
    super.member_on_text_edited.copy(semantics = AAction.EnableSemantics(SBox.field_text_edited_handler))

  override def member_on_text_editing =
    super.member_on_text_editing.copy(semantics = AAction.EnableSemantics(SBox.field_text_editing_handler))

  override def member_on_tapped =
    super.member_on_tapped.copy(semantics = AAction.EnableSemantics(SBox.field_tapped_handler))

  override lazy val possibleFields = super.possibleFields ++ Set(
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
  )

  override lazy val mutedFields = super.mutedFields ++ Set(
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
    field_max_height
  )

  override lazy val member_add_background_picture = super.member_add_background_picture.copy(semantics = SkipSemantics)


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
      var curState = state
      curState = AssignField[S](this0, SBox.field_text_editing_handler, changehandler)(curState,pp)
      curState = TText_Action.Enable[S](changehandler)(curState,pp)
      curState = New[S](TEvent_Binding)(curState,pp)
      curState

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




      
