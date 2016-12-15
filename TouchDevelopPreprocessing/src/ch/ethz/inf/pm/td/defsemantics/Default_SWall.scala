/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiMember, ApiParam, DefaultSemantics}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Wall
 *
 * Ask or display values on the wall...
 *
 * @author Lucas Brutschy
 */

trait Default_SWall extends ASingleton {

  lazy val typeName = TypeName("Wall", isSingleton = true)

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "add button" -> member_add_button,
    "ask boolean" -> member_ask_boolean,
    "ask number" -> member_ask_number,
    "ask string" -> member_ask_string,
    "button icon names" -> member_button_icon_names,
    "clear background" -> member_clear_background,
    "clear buttons" -> member_clear_buttons,
    "clear" -> member_clear,
    "create text box" -> member_create_text_box,
    "current page" -> member_current_page,
    "display search" -> member_display_search,
    "height" -> member_height,
    "icon names" -> member_icon_names,
    "pages" -> member_pages,
    "pick date" -> member_pick_date,
    "pick string" -> member_pick_string,
    "pick time" -> member_pick_time,
    "pop page with transition" -> member_pop_page_with_transition,
    "pop page" -> member_pop_page,
    "prompt" -> member_prompt,
    "push new page" -> member_push_new_page,
    "screenshot" -> member_screenshot,
    "set background camera" -> member_set_background_camera,
    "set background cloud picture" -> member_set_background_cloud_picture,
    "set background picture" -> member_set_background_picture,
    "set background" -> member_set_background,
    "set foreground" -> member_set_foreground,
    "set page transition style" -> member_set_page_transition_style,
    "set reversed" -> member_set_reversed,
    "set subtitle" -> member_set_subtitle,
    "set title" -> member_set_title,
    "set transform matrix" -> member_set_transform_matrix,
    "show back button" -> member_show_back_button,
    "show title bar" -> member_show_title_bar,
    "width" -> member_width
  )

  /** Frequently used: Add a new button. icon must be the name of a built-in icon, text must be non-empty. */
  def member_add_button = ApiMember(
    name = "add button",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TPage_Button,
    semantics = DefaultSemantics
  )

  /** Frequently used: Prompts the user with ok and cancel buttons */
  def member_ask_boolean = ApiMember(
    name = "ask boolean",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Prompts the user to input a number */
  def member_ask_number = ApiMember(
    name = "ask number",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Prompts the user to input a string */
  def member_ask_string = ApiMember(
    name = "ask string",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the list of available page button names. */
  def member_button_icon_names = ApiMember(
    name = "button icon names",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Never used: Clears the background color, picture and camera */
  def member_clear_background = ApiMember(
    name = "clear background",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Clears the application bar buttons and hides the bar */
  def member_clear_buttons = ApiMember(
    name = "clear buttons",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Clears the background, buttons and entries */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Creates an updatable text box */
  def member_create_text_box = ApiMember(
    name = "create text box",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TTextBox,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the current page displayed on the wall */
  def member_current_page = ApiMember(
    name = "current page",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPage,
    semantics = DefaultSemantics
  )

  /** Sometimes used: [**obsolete**] This action is not supported anymore. */
  def member_display_search = ApiMember(
    name = "display search",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the height of the screen (in pixels). */
  def member_height = ApiMember(
    name = "height",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Use button icon names instead. */
  def member_icon_names = ApiMember(
    name = "icon names",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Returns the current back stack of pages, starting from the current page to the bottom page. */
  def member_pages = ApiMember(
    name = "pages",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TPage),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Prompts the user to pick a date. Returns a datetime whose date is set, the time is 12:00:00. */
  def member_pick_date = ApiMember(
    name = "pick date",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Prompts the user to pick a string from a list. Returns the selected index. */
  def member_pick_string = ApiMember(
    name = "pick string",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(GCollection(TString))),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Rarely used: Prompts the user to pick a time. Returns a datetime whose time is set, the date is undefined. */
  def member_pick_time = ApiMember(
    name = "pick time",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TDateTime,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Same as `wall->pop_page`, but lets you use specific animation. */
  def member_pop_page_with_transition = ApiMember(
    name = "pop page with transition",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Pops the current page and restores the previous wall page. Returns false if already on the default page. */
  def member_pop_page = ApiMember(
    name = "pop page",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Frequently used: Prompts the user with a ok button */
  def member_prompt = ApiMember(
    name = "prompt",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Pushes an empty page on the wall. */
  def member_push_new_page = ApiMember(
    name = "push new page",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPage,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Takes a screenshot of the wall. */
  def member_screenshot = ApiMember(
    name = "screenshot",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the wall background camera. */
  def member_set_background_camera = ApiMember(
    name = "set background camera",
    paramTypes = List(ApiParam(TCamera)),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    isAsync = true,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the wall background as a cloud picture. The best resolution will be picked and the picture might be clipped to fit the screen. */
  def member_set_background_cloud_picture = ApiMember(
    name = "set background cloud picture",
    paramTypes = List(ApiParam(TCloud_Picture)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the wall background picture. The picture will be resized and clipped to the screen background as needed. */
  def member_set_background_picture = ApiMember(
    name = "set background picture",
    paramTypes = List(ApiParam(TPicture)),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the wall background color. */
  def member_set_background = ApiMember(
    name = "set background",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the wall foreground color of elements. */
  def member_set_foreground = ApiMember(
    name = "set foreground",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Sets the animation for push/pop of pages. */
  def member_set_page_transition_style = ApiMember(
    name = "set page transition style",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Reverses the elements on the wall and inserts new ones at the bottom. */
  def member_set_reversed = ApiMember(
    name = "set reversed",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the subtitle of the wall. */
  def member_set_subtitle = ApiMember(
    name = "set subtitle",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the title of the wall. */
  def member_set_title = ApiMember(
    name = "set title",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Sets the 3x3 affine matrix transformation applied to the wall. */
  def member_set_transform_matrix = ApiMember(
    name = "set transform matrix",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if the back button should be visible on the current page. The back button gets visible automatically when the app is paused or stopped in the editor. */
  def member_show_back_button = ApiMember(
    name = "show back button",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if the title and subtitle bar should be visible on a page. */
  def member_show_title_bar = ApiMember(
    name = "show title bar",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the width of the screen (in pixels). */
  def member_width = ApiMember(
    name = "width",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )
            

}
          
