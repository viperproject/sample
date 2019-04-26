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
 * Specifies the abstract semantics of Media Link
 *
 * A media file on the home network
 *
 * @author Lucas Brutschy
 */

trait Default_TMedia_Link extends AAny {

  lazy val typeName = TypeName("Media Link")
          
  /** Rarely used: [**obsolete**] Gets the album if available */
  def member_album = ApiMember(
    name = "album",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Gets the author if available */
  def member_author = ApiMember(
    name = "author",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Gets the date if available */
  def member_date = ApiMember(
    name = "date",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDateTime,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Gets the duration in seconds (0 for pictures) */
  def member_duration = ApiMember(
    name = "duration",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Gets the kind of media (video, song, picture) */
  def member_kind = ApiMember(
    name = "kind",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Plays or displays the media on the phone */
  def member_play = ApiMember(
    name = "play",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**obsolete**] Gets the title if available */
  def member_title = ApiMember(
    name = "title",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "album" -> member_album,
    "author" -> member_author,
    "date" -> member_date,
    "duration" -> member_duration,
    "kind" -> member_kind,
    "play" -> member_play,
    "title" -> member_title
  )
            

}
          
