
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Box
 *
 * Current box element in the page.
 *
 * @author Lucas Brutschy
 */

trait Default_SBox extends ASingleton {

  lazy val typeName = TypeName("Box", isSingleton = true)
          
  /** Never used: Sets the background picture. The picture must be a resource or from the web. The size of the picture does not impact the size of the box. */
  def member_add_background_picture = ApiMember(
    name = "add background picture",
    paramTypes = List(ApiParam(TPicture), ApiParam(TString), ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Display editable text, bound to the given string reference. */
  def member_edit_ref = ApiMember(
    name = "edit ref",
    paramTypes = List(ApiParam(TString), ApiParam(GRef(TString))),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Display editable text. */
  def member_edit_text = ApiMember(
    name = "edit text",
    paramTypes = List(ApiParam(TString), ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Display editable text, for the given content and change handler. */
  def member_edit = ApiMember(
    name = "edit",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TText_Action)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used:  */
  def member_is_init = ApiMember(
    name = "is init",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Set what happens when the box is tapped. */
  def member_on_tapped = ApiMember(
    name = "on tapped",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Set what happens when the user has finished editing the text in the box. */
  def member_on_text_edited = ApiMember(
    name = "on text edited",
    paramTypes = List(ApiParam(TText_Action)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Set what happens whenever the text in the box is being edited. */
  def member_on_text_editing = ApiMember(
    name = "on text editing",
    paramTypes = List(ApiParam(TText_Action)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Get the total height of the page. */
  def member_page_height = ApiMember(
    name = "page height",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Get the total width of the page. */
  def member_page_width = ApiMember(
    name = "page width",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Get the number of pixels in an em */
  def member_pixels_per_em = ApiMember(
    name = "pixels per em",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the background color. */
  def member_set_background = ApiMember(
    name = "set background",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set the width of each border. */
  def member_set_border_widths = ApiMember(
    name = "set border widths",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set the color and width of the border. */
  def member_set_border = ApiMember(
    name = "set border",
    paramTypes = List(ApiParam(TColor), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Set font family in this box and contained boxes. */
  def member_set_font_family = ApiMember(
    name = "set font family",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set font size in this box and contained boxes. */
  def member_set_font_size = ApiMember(
    name = "set font size",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set font weight in this box and contained boxes. */
  def member_set_font_weight = ApiMember(
    name = "set font weight",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the foreground color of elements. */
  def member_set_foreground = ApiMember(
    name = "set foreground",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set lower and upper limits on the height of this box. */
  def member_set_height_range = ApiMember(
    name = "set height range",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set the height of this box. */
  def member_set_height = ApiMember(
    name = "set height",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Specify how to arrange the content of this box */
  def member_set_horizontal_align = ApiMember(
    name = "set horizontal align",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] align (0,0)=center (1,0)=left, (0,1)=right, (1,1)=stretch */
  def member_set_horizontal_alignment = ApiMember(
    name = "set horizontal alignment",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Specify how to compute box width (0 = shrink to fit content, 1 = stretch to fit frame, , 0.5 = stretch to half width) */
  def member_set_horizontal_stretch = ApiMember(
    name = "set horizontal stretch",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set the margins of this box (to leave space around the outside of this box). */
  def member_set_margins = ApiMember(
    name = "set margins",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set the padding of this box (to leave space around the contents of this box). */
  def member_set_padding = ApiMember(
    name = "set padding",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Specify whether to use scrollbars on overflow. */
  def member_set_scrolling = ApiMember(
    name = "set scrolling",
    paramTypes = List(ApiParam(TBoolean), ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set whether to break long lines, and specify what length is too short for breaking */
  def member_set_text_wrapping = ApiMember(
    name = "set text wrapping",
    paramTypes = List(ApiParam(TBoolean), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Specify how to arrange the content of this box */
  def member_set_vertical_align = ApiMember(
    name = "set vertical align",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] align (0,0)=center (1,0)=top, (0,1)=bottom, (1,1)=stretch */
  def member_set_vertical_alignment = ApiMember(
    name = "set vertical alignment",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Specify how to compute box height (0 = shrink to fit content, 1 = stretch to fit frame, 0.5 = stretch to half height) */
  def member_set_vertical_stretch = ApiMember(
    name = "set vertical stretch",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set lower and upper limits on the width of this box. */
  def member_set_width_range = ApiMember(
    name = "set width range",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Set the width of this box. */
  def member_set_width = ApiMember(
    name = "set width",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Arrange boxes inside this box from left to right. */
  def member_use_horizontal_layout = ApiMember(
    name = "use horizontal layout",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Arrange boxes inside this box as layers on top of each other. */
  def member_use_overlay_layout = ApiMember(
    name = "use overlay layout",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Arrange boxes inside this box from top to bottom. */
  def member_use_vertical_layout = ApiMember(
    name = "use vertical layout",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add background picture" -> member_add_background_picture,
    "edit ref" -> member_edit_ref,
    "edit text" -> member_edit_text,
    "edit" -> member_edit,
    "is init" -> member_is_init,
    "on tapped" -> member_on_tapped,
    "on text edited" -> member_on_text_edited,
    "on text editing" -> member_on_text_editing,
    "page height" -> member_page_height,
    "page width" -> member_page_width,
    "pixels per em" -> member_pixels_per_em,
    "set background" -> member_set_background,
    "set border widths" -> member_set_border_widths,
    "set border" -> member_set_border,
    "set font family" -> member_set_font_family,
    "set font size" -> member_set_font_size,
    "set font weight" -> member_set_font_weight,
    "set foreground" -> member_set_foreground,
    "set height range" -> member_set_height_range,
    "set height" -> member_set_height,
    "set horizontal align" -> member_set_horizontal_align,
    "set horizontal alignment" -> member_set_horizontal_alignment,
    "set horizontal stretch" -> member_set_horizontal_stretch,
    "set margins" -> member_set_margins,
    "set padding" -> member_set_padding,
    "set scrolling" -> member_set_scrolling,
    "set text wrapping" -> member_set_text_wrapping,
    "set vertical align" -> member_set_vertical_align,
    "set vertical alignment" -> member_set_vertical_alignment,
    "set vertical stretch" -> member_set_vertical_stretch,
    "set width range" -> member_set_width_range,
    "set width" -> member_set_width,
    "use horizontal layout" -> member_use_horizontal_layout,
    "use overlay layout" -> member_use_overlay_layout,
    "use vertical layout" -> member_use_vertical_layout
  )
            

}
          
