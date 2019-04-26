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
 * Specifies the abstract semantics of Media Server
 *
 * A media server on the home network
 *
 * @author Lucas Brutschy
 */

trait Default_TMedia_Server extends AAny {

  lazy val typeName = TypeName("Media Server")
          
  /** Rarely used: [**not implemented**] [**obsolete**] Chooses a picture */
  def member_choose_picture = ApiMember(
    name = "choose picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMedia_Link,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] [**obsolete**] Chooses a song */
  def member_choose_song = ApiMember(
    name = "choose song",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMedia_Link,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] [**obsolete**] Chooses a video or a movie */
  def member_choose_video = ApiMember(
    name = "choose video",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TMedia_Link,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] [**obsolete**] Gets the detailled information about this device */
  def member_device = ApiMember(
    name = "device",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TDevice,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] [**obsolete**] Gets the name of the printer */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Gets a list of all pictures */
  def member_pictures = ApiMember(
    name = "pictures",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Link),
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Searches for pictures in a particular date range. */
  def member_search_pictures_by_date = ApiMember(
    name = "search pictures by date",
    paramTypes = List(ApiParam(TDateTime), ApiParam(TDateTime)),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Link),
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Searches for songs */
  def member_search_songs = ApiMember(
    name = "search songs",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Link),
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Searches for videos in a particular date range. */
  def member_search_videos_by_date = ApiMember(
    name = "search videos by date",
    paramTypes = List(ApiParam(TDateTime), ApiParam(TDateTime)),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Link),
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Searches for videos */
  def member_search_videos = ApiMember(
    name = "search videos",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Link),
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Gets a list of all songs */
  def member_songs = ApiMember(
    name = "songs",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Link),
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Gets a list of all videos */
  def member_videos = ApiMember(
    name = "videos",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TMedia_Link),
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "choose picture" -> member_choose_picture,
    "choose song" -> member_choose_song,
    "choose video" -> member_choose_video,
    "device" -> member_device,
    "name" -> member_name,
    "pictures" -> member_pictures,
    "search pictures by date" -> member_search_pictures_by_date,
    "search songs" -> member_search_songs,
    "search videos by date" -> member_search_videos_by_date,
    "search videos" -> member_search_videos,
    "songs" -> member_songs,
    "videos" -> member_videos
  )
            

}
          
