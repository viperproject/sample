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
 * Specifies the abstract semantics of Song
 *
 * A song
 *
 * @author Lucas Brutschy
 */

trait Default_TSong extends AAny {

  lazy val typeName = TypeName("Song")

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "album" -> member_album,
    "artist" -> member_artist,
    "duration" -> member_duration,
    "genre" -> member_genre,
    "name" -> member_name,
    "play count" -> member_play_count,
    "play" -> member_play,
    "protected" -> member_protected,
    "rating" -> member_rating,
    "track" -> member_track
  )

  /** Frequently used: Gets the song album containing the song */
  def member_album = ApiMember(
    name = "album",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSong_Album,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the name of the artist */
  def member_artist = ApiMember(
    name = "artist",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the duration in seconds */
  def member_duration = ApiMember(
    name = "duration",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the genre of the song */
  def member_genre = ApiMember(
    name = "genre",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the name of the song */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the play count */
  def member_play_count = ApiMember(
    name = "play count",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Plays the song. */
  def member_play = ApiMember(
    name = "play",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] Gets a value whether the song is DRM protected */
  def member_protected = ApiMember(
    name = "protected",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the users rating. -1 if not rated. */
  def member_rating = ApiMember(
    name = "rating",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the track number in the album */
  def member_track = ApiMember(
    name = "track",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )
            

}
          
