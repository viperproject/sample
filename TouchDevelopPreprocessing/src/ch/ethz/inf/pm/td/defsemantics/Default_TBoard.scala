/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Board
 *
 * A board to build 2D games
 *
 * @author Lucas Brutschy
 */

trait Default_TBoard extends AMutableLinearCollection {

  lazy val typeName = TypeName("Board")
          
  def keyType = TNumber

  def valueType = TSprite

  /** Never used: add an action that fires for every display frame. */
  def member_add_on_every_frame = ApiMember(
    name = "add on every frame",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the background scene */
  def member_background_scene = ApiMember(
    name = "background scene",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoard_Background_Scene,
    semantics = DefaultSemantics
  )

  /** Rarely used: Clears the background camera */
  def member_clear_background_camera = ApiMember(
    name = "clear background camera",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Clear the background picture */
  def member_clear_background_picture = ApiMember(
    name = "clear background picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Clear all queued events related to this board */
  def member_clear_events = ApiMember(
    name = "clear events",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Stops and clears all the `every frame` timers */
  def member_clear_every_frame_timers = ApiMember(
    name = "clear every frame timers",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Create an anchor sprite. */
  def member_create_anchor = ApiMember(
    name = "create anchor",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TSprite,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Create walls around the board at the given distance. */
  def member_create_boundary = ApiMember(
    name = "create boundary",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Create a new ellipse sprite. */
  def member_create_ellipse = ApiMember(
    name = "create ellipse",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TSprite,
    semantics = DefaultSemantics
  )

  /** Frequently used: Create a line obstacle with given start point, and given width and height. Elasticity is 0 for sticky, 1 for complete bounce. */
  def member_create_obstacle = ApiMember(
    name = "create obstacle",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TObstacle,
    semantics = DefaultSemantics
  )

  /** Frequently used: Create a new picture sprite. */
  def member_create_picture = ApiMember(
    name = "create picture",
    paramTypes = List(ApiParam(TPicture)),
    thisType = ApiParam(this),
    returnType = TSprite,
    semantics = DefaultSemantics
  )

  /** Frequently used: Create a new rectangle sprite. */
  def member_create_rectangle = ApiMember(
    name = "create rectangle",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TSprite,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Create a spring between the two sprites. */
  def member_create_spring = ApiMember(
    name = "create spring",
    paramTypes = List(ApiParam(TSprite), ApiParam(TSprite), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TSpring,
    semantics = DefaultSemantics
  )

  /** Frequently used: Create a new collection for sprites. */
  def member_create_sprite_set = ApiMember(
    name = "create sprite set",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSprite_Set,
    semantics = DefaultSemantics
  )

  /** Never used: Create a new sprite sheet. */
  def member_create_sprite_sheet = ApiMember(
    name = "create sprite sheet",
    paramTypes = List(ApiParam(TPicture)),
    thisType = ApiParam(this),
    returnType = TSprite_Sheet,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Create a new text sprite. */
  def member_create_text = ApiMember(
    name = "create text",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TSprite,
    semantics = DefaultSemantics
  )

  /** Frequently used: Update positions of sprites on board. */
  def member_evolve = ApiMember(
    name = "evolve",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: gets the timer that fires for every display frame. */
  def member_frame_timer = ApiMember(
    name = "frame timer",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TTimer,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the height in pixels */
  def member_height = ApiMember(
    name = "height",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets a value indicating if the board is designed to be viewed in landscape mode */
  def member_is_landscape = ApiMember(
    name = "is landscape",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: add an action that fires for every display frame. */
  def member_on_every_frame = ApiMember(
    name = "on every frame",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: set the handler that is invoked when the board is swiped */
  def member_on_swipe = ApiMember(
    name = "on swipe",
    paramTypes = List(ApiParam(TVector_Action)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: set the handler that is invoked when the board is tapped */
  def member_on_tap = ApiMember(
    name = "on tap",
    paramTypes = List(ApiParam(TPosition_Action)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: set the handler that is invoked when the board is touched */
  def member_on_touch_down = ApiMember(
    name = "on touch down",
    paramTypes = List(ApiParam(TPosition_Action)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: set the handler that is invoked when the board touch is released */
  def member_on_touch_up = ApiMember(
    name = "on touch up",
    paramTypes = List(ApiParam(TPosition_Action)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the background camera */
  def member_set_background_camera = ApiMember(
    name = "set background camera",
    paramTypes = List(ApiParam(TCamera)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the background picture */
  def member_set_background_picture = ApiMember(
    name = "set background picture",
    paramTypes = List(ApiParam(TPicture)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Sets the background color */
  def member_set_background = ApiMember(
    name = "set background",
    paramTypes = List(ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: In debug mode, board displays speed and other info of sprites */
  def member_set_debug_mode = ApiMember(
    name = "set debug mode",
    paramTypes = List(ApiParam(TBoolean)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the default friction for sprites to a fraction of speed loss between 0 and 1 */
  def member_set_friction = ApiMember(
    name = "set friction",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the uniform acceleration vector for objects on the board to pixels/sec^2 */
  def member_set_gravity = ApiMember(
    name = "set gravity",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Current touch point */
  def member_touch_current = ApiMember(
    name = "touch current",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Last touch end point */
  def member_touch_end = ApiMember(
    name = "touch end",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Last touch start point */
  def member_touch_start = ApiMember(
    name = "touch start",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Rarely used: Final touch velocity after touch ended */
  def member_touch_velocity = ApiMember(
    name = "touch velocity",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TVector3,
    semantics = DefaultSemantics
  )

  /** Frequently used: True if board is touched */
  def member_touched = ApiMember(
    name = "touched",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Make updates visible. */
  def member_update_on_wall = ApiMember(
    name = "update on wall",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Very frequently used: Gets the width in pixels */
  def member_width = ApiMember(
    name = "width",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add on every frame" -> member_add_on_every_frame,
    "background scene" -> member_background_scene,
    "clear background camera" -> member_clear_background_camera,
    "clear background picture" -> member_clear_background_picture,
    "clear events" -> member_clear_events,
    "clear every frame timers" -> member_clear_every_frame_timers,
    "create anchor" -> member_create_anchor,
    "create boundary" -> member_create_boundary,
    "create ellipse" -> member_create_ellipse,
    "create obstacle" -> member_create_obstacle,
    "create picture" -> member_create_picture,
    "create rectangle" -> member_create_rectangle,
    "create spring" -> member_create_spring,
    "create sprite set" -> member_create_sprite_set,
    "create sprite sheet" -> member_create_sprite_sheet,
    "create text" -> member_create_text,
    "evolve" -> member_evolve,
    "frame timer" -> member_frame_timer,
    "height" -> member_height,
    "is landscape" -> member_is_landscape,
    "on every frame" -> member_on_every_frame,
    "on swipe" -> member_on_swipe,
    "on tap" -> member_on_tap,
    "on touch down" -> member_on_touch_down,
    "on touch up" -> member_on_touch_up,
    "set background camera" -> member_set_background_camera,
    "set background picture" -> member_set_background_picture,
    "set background" -> member_set_background,
    "set debug mode" -> member_set_debug_mode,
    "set friction" -> member_set_friction,
    "set gravity" -> member_set_gravity,
    "touch current" -> member_touch_current,
    "touch end" -> member_touch_end,
    "touch start" -> member_touch_start,
    "touch velocity" -> member_touch_velocity,
    "touched" -> member_touched,
    "update on wall" -> member_update_on_wall,
    "width" -> member_width
  )
            

}
          
