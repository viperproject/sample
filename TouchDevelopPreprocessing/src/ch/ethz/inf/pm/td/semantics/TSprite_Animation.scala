/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.TouchType
import ch.ethz.inf.pm.td.defsemantics.Default_TSprite_Animation
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of Sprite Animation
 *
 * A animation to animate sprite properties.
 *
 * @author Lucas Brutschy
 */

object TSprite_Animation extends Default_TSprite_Animation {

  /** Gets the current time scale factor */
  lazy val field_time_scale = ApiField("time scale", TNumber)

  /** PRIVATE HANDLER FIELDS */
  lazy val field_start_handler = ApiField("start handler", TAction)
  lazy val field_stop_handler = ApiField("stop handler", TAction)

  override def member_on_start =
    super.member_on_start.copy(semantics = AAction.EnableSemantics(TSprite_Animation.field_start_handler))

  override def member_on_stop =
    super.member_on_stop.copy(semantics = AAction.EnableSemantics(TSprite_Animation.field_stop_handler))

  override def possibleFields = super.possibleFields ++ List(field_start_handler, field_stop_handler, field_time_scale)

  override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: String, parameters: List[ExpressionSet], returnedType: TouchType)
                                              (implicit pp: ProgramPoint, state: S): S = method match {

    /** Creating a beating animation */
    case "beat" =>
      val List(duration, cycle, value) = parameters // Number,Number,Number
      Skip

    /** Changes the color of the sprite */
    case "color" =>
      val List(duration, easing, shape, c) = parameters // Number,String,String,Color
      Skip

    /** deletes the sprite */
    case "delete" =>
      val List() = parameters //
      Skip

    /** Fades in to fully opaque */
    case "fade in" =>
      val List(duration, easing) = parameters // Number,String
      Skip

    /** Fades out to transparent */
    case "fade out" =>
      val List(duration, easing) = parameters // Number,String
      Skip

    /** Starts a new animation and continues with the current animation */
    case "fork" =>
      Return[S](this0)
      //Clone[S](this0)

    /** Sets a different frame from the sprite sheet */
    case "frame" =>
      val List(name) = parameters // String
      Skip

    /** Hides the sprite */
    case "hide" =>
      val List() = parameters //
      Skip

    /** Moves the sprite to a given location using separate easing for x and y */
    case "move to" =>
      val List(duration, easing, shape, x, y) = parameters // Number,String,String,Number,Number
      Skip

    /** Starts playing an animation from the sprite sheet, if any */
    case "play frames" =>
      val List(animation) = parameters // String
      Skip

    /** play sound */
    case "play sound" =>
      val List(sound) = parameters // Sound
      Skip

    /** Scales up and fades out an object */
    case "puff out" =>
      val List(duration, easing, scale) = parameters // Number,String,Number
      Skip

    /** Repeats the latest animation. Negative ``count`` makes infinite repetition. ``yoyo`` makes the animation repeat back and forth. */
    case "repeat" =>
      val List(count, yoyo) = parameters // Number,Boolean
      Skip

    /** Calls a user handler during the animation. ``handler`` receives a number from 0 to 1 during the tweeining. */
    case "run" =>
      val List(duration, easing, shape, handler) = parameters // Number,String,String,Number_Action
      Skip

    /** Scales the sprite */
    case "scale" =>
      val List(duration, easing, shape, value) = parameters // Number,String,String,Number
      Skip

    /** shows the sprite */
    case "show" =>
      val List() = parameters //
      Skip

    /** Waits for a number of seconds */
    case "sleep" =>
      val List(duration) = parameters // Number
      Skip

    /** Stops this animation */
    case "stop" =>
      val List() = parameters //
      Skip

    /** Changes the text of the sprite. */
    case "text" =>
      val List(duration,easing,shape,value) = parameters // Number,String,String,String
      Skip

    /** Rotates the sprite. */
    case "turn to" =>
      val List(duration, easing, shape, angle) = parameters // Number,String,String,Number
      Skip

    /** Waits for the other animation to complete before proceding. */
    case "wait for" =>
      val List(animation) = parameters // Sprite_Animation
      Skip

    case _ =>
      super.forwardSemantics(this0, method, parameters, returnedType)

  }
}
      
