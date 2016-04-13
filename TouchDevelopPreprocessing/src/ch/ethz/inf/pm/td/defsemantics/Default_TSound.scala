
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
 * Specifies the abstract semantics of Sound
 *
 * A sound effect
 *
 * @author Lucas Brutschy
 */

trait Default_TSound extends AAny {

  lazy val typeName = TypeName("Sound")
          
  /** Sometimes used: Gets the duration in seconds. */
  def member_duration = ApiMember(
    name = "duration",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the panning, ranging from -1.0 (full left) to 1.0 (full right). */
  def member_pan = ApiMember(
    name = "pan",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Not supported anymore */
  def member_pause = ApiMember(
    name = "pause",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the pitch adjustment, ranging from -1 (down one octave) to 1 (up one octave). */
  def member_pitch = ApiMember(
    name = "pitch",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Plays the song with different volume (0 to 1), pitch (-1 to 1) and pan (-1 to 1). */
  def member_play_special = ApiMember(
    name = "play special",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Frequently used: Plays the sound effect */
  def member_play = ApiMember(
    name = "play",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Not supported anymore */
  def member_resume = ApiMember(
    name = "resume",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the panning, ranging from -1.0 (full left) to 1.0 (full right). */
  def member_set_pan = ApiMember(
    name = "set pan",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sets the pitch adjustment, ranging from -1 (down one octave) to 1 (up one octave). */
  def member_set_pitch = ApiMember(
    name = "set pitch",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the volume from 0 (silent) to 1 (full volume). */
  def member_set_volume = ApiMember(
    name = "set volume",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Not supported anymore */
  def member_state = ApiMember(
    name = "state",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Not supported anymore */
  def member_stop = ApiMember(
    name = "stop",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the volume from 0 (silent) to 1 (full volume) */
  def member_volume = ApiMember(
    name = "volume",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "duration" -> member_duration,
    "pan" -> member_pan,
    "pause" -> member_pause,
    "pitch" -> member_pitch,
    "play special" -> member_play_special,
    "play" -> member_play,
    "resume" -> member_resume,
    "set pan" -> member_set_pan,
    "set pitch" -> member_set_pitch,
    "set volume" -> member_set_volume,
    "state" -> member_state,
    "stop" -> member_stop,
    "volume" -> member_volume
  )
            

}
          
