
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
 * Specifies the abstract semantics of Sprite Animation
 *
 * A animation to animate sprite properties.
 *
 * @author Lucas Brutschy
 */

trait Default_TSprite_Animation extends AAny {

  lazy val typeName = TypeName("Sprite Animation")
          
  /** Never used: Creating a beating animation */
  def member_beat = ApiMember(
    name = "beat",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Changes the color of the sprite */
  def member_color = ApiMember(
    name = "color",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TString), ApiParam(TColor)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: deletes the sprite */
  def member_delete_ = ApiMember(
    name = "delete",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Fades in to fully opaque */
  def member_fade_in = ApiMember(
    name = "fade in",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Fades out to transparent */
  def member_fade_out = ApiMember(
    name = "fade out",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Changes the opacity of the sprite */
  def member_fade = ApiMember(
    name = "fade",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Starts a new animation and continues with the current animation */
  def member_fork = ApiMember(
    name = "fork",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TSprite_Animation,
    semantics = DefaultSemantics
  )

  /** Never used: Sets a different frame from the sprite sheet */
  def member_frame = ApiMember(
    name = "frame",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Modifies the sprite height */
  def member_height = ApiMember(
    name = "height",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Hides the sprite */
  def member_hide = ApiMember(
    name = "hide",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a value indicating if the animation is still running */
  def member_is_active = ApiMember(
    name = "is active",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Moves the sprite to a given location using separate easing for x and y */
  def member_move_to = ApiMember(
    name = "move to",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TString), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Raised when the animation started playing */
  def member_on_start = ApiMember(
    name = "on start",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Raised when the animation stopped playing */
  def member_on_stop = ApiMember(
    name = "on stop",
    paramTypes = List(ApiParam(TAction)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TEvent_Binding,
    semantics = DefaultSemantics
  )

  /** Never used: Starts playing an animation from the sprite sheet, if any */
  def member_play_frames = ApiMember(
    name = "play frames",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: play sound */
  def member_play_sound = ApiMember(
    name = "play sound",
    paramTypes = List(ApiParam(TSound)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Scales up and fades out an object */
  def member_puff_out = ApiMember(
    name = "puff out",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Repeats the latest animation. Negative ``count`` makes infinite repetition. ``yoyo`` makes the animation repeat back and forth. */
  def member_repeat = ApiMember(
    name = "repeat",
    paramTypes = List(ApiParam(TNumber), ApiParam(TBoolean)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Calls a user handler during the animation. ``handler`` receives a number from 0 to 1 during the tweeining. */
  def member_run = ApiMember(
    name = "run",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TString), ApiParam(TNumber_Action)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Scales the sprite */
  def member_scale = ApiMember(
    name = "scale",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the current time scale factor */
  def member_set_time_scale = ApiMember(
    name = "set time scale",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: shows the sprite */
  def member_show = ApiMember(
    name = "show",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Waits for a number of seconds */
  def member_sleep = ApiMember(
    name = "sleep",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Stops this animation */
  def member_stop = ApiMember(
    name = "stop",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Changes the text of the sprite. */
  def member_text = ApiMember(
    name = "text",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the current time scale factor */
  def member_time_scale = ApiMember(
    name = "time scale",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Rotates the sprite. */
  def member_turn_to = ApiMember(
    name = "turn to",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Waits for the other animation to complete before proceding. */
  def member_wait_for = ApiMember(
    name = "wait for",
    paramTypes = List(ApiParam(TSprite_Animation)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Waits till the animation completes. This action will evolve the board if needed. */
  def member_wait = ApiMember(
    name = "wait",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Modifies the sprite width */
  def member_width = ApiMember(
    name = "width",
    paramTypes = List(ApiParam(TNumber), ApiParam(TString), ApiParam(TString), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "beat" -> member_beat,
    "color" -> member_color,
    "delete" -> member_delete_,
    "fade in" -> member_fade_in,
    "fade out" -> member_fade_out,
    "fade" -> member_fade,
    "fork" -> member_fork,
    "frame" -> member_frame,
    "height" -> member_height,
    "hide" -> member_hide,
    "is active" -> member_is_active,
    "move to" -> member_move_to,
    "on start" -> member_on_start,
    "on stop" -> member_on_stop,
    "play frames" -> member_play_frames,
    "play sound" -> member_play_sound,
    "puff out" -> member_puff_out,
    "repeat" -> member_repeat,
    "run" -> member_run,
    "scale" -> member_scale,
    "set time scale" -> member_set_time_scale,
    "show" -> member_show,
    "sleep" -> member_sleep,
    "stop" -> member_stop,
    "text" -> member_text,
    "time scale" -> member_time_scale,
    "turn to" -> member_turn_to,
    "wait for" -> member_wait_for,
    "wait" -> member_wait,
    "width" -> member_width
  )
            

}
          
