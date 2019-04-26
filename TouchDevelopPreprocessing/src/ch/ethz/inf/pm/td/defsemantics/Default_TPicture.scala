/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Picture
 *
 * A picture
 *
 * @author Lucas Brutschy
 */

trait Default_TPicture extends AAny {

  lazy val typeName = TypeName("Picture")

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "at" -> member_at,
    "blend svg" -> member_blend_svg,
    "blend" -> member_blend,
    "brightness" -> member_brightness,
    "clear rect" -> member_clear_rect,
    "clear" -> member_clear,
    "clone" -> member_clone,
    "colorize" -> member_colorize,
    "contrast" -> member_contrast,
    "count" -> member_count,
    "crop" -> member_crop,
    "date" -> member_date,
    "desaturate" -> member_desaturate,
    "docs render" -> member_docs_render,
    "draw ellipse" -> member_draw_ellipse,
    "draw line" -> member_draw_line,
    "draw path" -> member_draw_path,
    "draw rect" -> member_draw_rect,
    "draw text" -> member_draw_text,
    "fill ellipse" -> member_fill_ellipse,
    "fill path" -> member_fill_path,
    "fill rect" -> member_fill_rect,
    "flip horizontal" -> member_flip_horizontal,
    "flip vertical" -> member_flip_vertical,
    "height" -> member_height,
    "invert" -> member_invert,
    "is panorama" -> member_is_panorama,
    "location" -> member_location,
    "negative" -> member_negative,
    "pixel" -> member_pixel,
    "readonly url" -> member_readonly_url,
    "resize" -> member_resize,
    "save to library" -> member_save_to_library,
    "set pixel" -> member_set_pixel,
    "share" -> member_share,
    "tint" -> member_tint,
    "to buffer" -> member_to_buffer,
    "to data uri" -> member_to_data_uri,
    "update on wall" -> member_update_on_wall,
    "width" -> member_width,
    "write buffer" -> member_write_buffer
  )

  /** Rarely used: Gets the pixel color at the given linear index */
  def member_at = ApiMember(
    name = "at",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Writes an Scalable Vector Graphics (SVG) document at a given location. By default, this action uses the viewport size provided in the SVG document when width or height are negative. */
  def member_blend_svg = ApiMember(
    name = "blend svg",
    paramTypes = List(ApiParam(TString), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Writes another picture at a given location. The opacity ranges from 0 (transparent) to 1 (opaque). */
  def member_blend = ApiMember(
    name = "blend",
    paramTypes = List(ApiParam(TPicture), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Changes the brightness of the picture. factor in [-1, 1]. */
  def member_brightness = ApiMember(
    name = "brightness",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Clears a rectangle on a the picture to a given color */
  def member_clear_rect = ApiMember(
    name = "clear rect",
    paramTypes = List(ApiParam(TColor), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Clears the picture to a given color */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Returns a copy of the image */
  def member_clone = ApiMember(
    name = "clone",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Recolors the picture with the background and foreground color, based on a color threshold between 0.0 and 1.0 */
  def member_colorize = ApiMember(
    name = "colorize",
    paramTypes = List(ApiParam(TColor), ApiParam(TColor), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Changes the contrast of the picture. factor in [-1, 1]. */
  def member_contrast = ApiMember(
    name = "contrast",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the number of pixels */
  def member_count = ApiMember(
    name = "count",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Crops a sub-image */
  def member_crop = ApiMember(
    name = "crop",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the date time where the picture was taken; if any. */
  def member_date = ApiMember(
    name = "date",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Makes picture monochromatic (black and white) */
  def member_desaturate = ApiMember(
    name = "desaturate",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Shows an art picture in the docs. */
  def member_docs_render = ApiMember(
    name = "docs render",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Draws an elliptic border with a given color */
  def member_draw_ellipse = ApiMember(
    name = "draw ellipse",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TColor), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Draws a line between two points */
  def member_draw_line = ApiMember(
    name = "draw line",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TColor), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Draws a path with a given color. */
  def member_draw_path = ApiMember(
    name = "draw path",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TColor), ApiParam(TNumber), ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Draws a rectangle border with a given color */
  def member_draw_rect = ApiMember(
    name = "draw rect",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TColor), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Draws some text border with a given color and font size */
  def member_draw_text = ApiMember(
    name = "draw text",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TString), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Fills a ellipse with a given color */
  def member_fill_ellipse = ApiMember(
    name = "fill ellipse",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Fills a path with a given color. */
  def member_fill_path = ApiMember(
    name = "fill path",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TColor), ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Fills a rectangle with a given color */
  def member_fill_rect = ApiMember(
    name = "fill rect",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Flips the picture horizontally */
  def member_flip_horizontal = ApiMember(
    name = "flip horizontal",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Flips the picture vertically */
  def member_flip_vertical = ApiMember(
    name = "flip vertical",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the height in pixels */
  def member_height = ApiMember(
    name = "height",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Inverts the red, blue and green channels */
  def member_invert = ApiMember(
    name = "invert",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates if the picture width is greater than its height */
  def member_is_panorama = ApiMember(
    name = "is panorama",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the location where the picture was taken; if any. */
  def member_location = ApiMember(
    name = "location",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLocation,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Inverts the colors in the picture */
  def member_negative = ApiMember(
    name = "negative",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the pixel color */
  def member_pixel = ApiMember(
    name = "pixel",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TColor,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Get the web address of an art resource; invalid if not available. */
  def member_readonly_url = ApiMember(
    name = "readonly url",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Frequently used: Resizes the picture to the given size in pixels */
  def member_resize = ApiMember(
    name = "resize",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Saves the picture and returns the file name if successful. */
  def member_save_to_library = ApiMember(
    name = "save to library",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    pausesInterpreter = true,
    isAsync = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the pixel color at a given pixel */
  def member_set_pixel = ApiMember(
    name = "set pixel",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Rarely used: Shares this message ('' to pick from a list) */
  def member_share = ApiMember(
    name = "share",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Converts every pixel to gray and tints it with the given color. */
  def member_tint = ApiMember(
    name = "tint",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Copy all pixels from the picture */
  def member_to_buffer = ApiMember(
    name = "to buffer",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TBuffer,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Encodes the image into a data uri using the desired quality (1 best, 0 worst). If the quality value is 1, the image is encoded as PNG, otherwise JPEG. */
  def member_to_data_uri = ApiMember(
    name = "to data uri",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TString,
    pausesInterpreter = true,
    isAsync = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Refreshes the picture on the wall */
  def member_update_on_wall = ApiMember(
    name = "update on wall",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the width in pixels */
  def member_width = ApiMember(
    name = "width",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Never used: Copy pixels from `buffer` to the picture */
  def member_write_buffer = ApiMember(
    name = "write buffer",
    paramTypes = List(ApiParam(TBuffer)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )
            

}
          
