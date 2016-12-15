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
 * Specifies the abstract semantics of Media
 *
 * Pictures and music...
 *
 * @author Lucas Brutschy
 */

trait Default_SMedia extends ASingleton {

  lazy val typeName = TypeName("Media", isSingleton = true)

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "choose picture" -> member_choose_picture,
    "create board" -> member_create_board,
    "create full board" -> member_create_full_board,
    "create landscape board" -> member_create_landscape_board,
    "create picture" -> member_create_picture,
    "create portrait board" -> member_create_portrait_board,
    "icon names" -> member_icon_names,
    "icon" -> member_icon,
    "large icon" -> member_large_icon,
    "picture albums" -> member_picture_albums,
    "pictures" -> member_pictures,
    "playlists" -> member_playlists,
    "saved pictures" -> member_saved_pictures,
    "search marketplace" -> member_search_marketplace,
    "song albums" -> member_song_albums,
    "songs" -> member_songs,
    "play note" -> member_play_note,
    "tone" -> member_tone
  )

  /** Sometimes used: Chooses a picture from the media library */
  def member_choose_picture = ApiMember(
    name = "choose picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    pausesInterpreter = true,
    semantics = DefaultSemantics
  )

  /** Frequently used: Creates a new game board */
  def member_create_board = ApiMember(
    name = "create board",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoard,
    semantics = DefaultSemantics
  )

  /** Frequently used: [**obsolete**] Creates a new game board in portrait mode. On rotatable devices it will take the entire screen when posted. */
  def member_create_full_board = ApiMember(
    name = "create full board",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoard,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a new game board in landscape mode. On rotatable devices it will take the entire screen when posted. */
  def member_create_landscape_board = ApiMember(
    name = "create landscape board",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoard,
    semantics = DefaultSemantics
  )

  /** Frequently used: Creates a new picture of the given size */
  def member_create_picture = ApiMember(
    name = "create picture",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Creates a new game board in portrait mode. On rotatable devices it will take the entire screen when posted. */
  def member_create_portrait_board = ApiMember(
    name = "create portrait board",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TBoard,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the list of built-in 48x48 icon names. You can see the icon list in the script settings. */
  def member_icon_names = ApiMember(
    name = "icon names",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = GCollection(TString),
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets a 48x48 icon picture. Use 'media->icon names' to retrieve the list of names available. */
  def member_icon = ApiMember(
    name = "icon",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets a 96x96 icon picture. Use 'media->icon names' to retrieve the list of names available. */
  def member_large_icon = ApiMember(
    name = "large icon",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: Plays a monotone note */
  def member_play_note = ApiMember(
    name = "play note",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    pausesInterpreter = true,
    isAsync = true,
    semantics = DefaultSemantics
  )

  /** Never used: Plays a monotone sine wave */
  def member_tone = ApiMember(
    name = "tone",
    paramTypes = List(ApiParam(TNumber), ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the picture albums */
  def member_picture_albums = ApiMember(
    name = "picture albums",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture_Albums,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the pictures on the phone */
  def member_pictures = ApiMember(
    name = "pictures",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPictures,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the playlists on the phone */
  def member_playlists = ApiMember(
    name = "playlists",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPlaylists,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the saved pictures on the phone */
  def member_saved_pictures = ApiMember(
    name = "saved pictures",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPictures,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] Searches the Windows Phone Store (type in applications or music) */
  def member_search_marketplace = ApiMember(
    name = "search marketplace",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the song albums on the phone */
  def member_song_albums = ApiMember(
    name = "song albums",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSong_Albums,
    semantics = DefaultSemantics
  )

  /** Frequently used: Gets the songs on the phone */
  def member_songs = ApiMember(
    name = "songs",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSongs,
    semantics = DefaultSemantics
  )
            

}
          
