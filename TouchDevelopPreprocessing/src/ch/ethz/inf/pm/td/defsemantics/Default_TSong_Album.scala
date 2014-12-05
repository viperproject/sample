
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Song Album
 *
 * A song album
 *
 * @author Lucas Brutschy
 */

trait Default_TSong_Album extends AAny {

  lazy val typeName = TypeName("Song Album")
          
  /** Sometimes used: Gets album art picture */
  def member_art = ApiMember(
    name = "art",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the name of the artist */
  def member_artist = ApiMember(
    name = "artist",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the duration in seconds */
  def member_duration = ApiMember(
    name = "duration",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the genre of the song */
  def member_genre = ApiMember(
    name = "genre",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Indicates if the album has art */
  def member_has_art = ApiMember(
    name = "has art",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the name of the album */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: Plays the songs of the album */
  def member_play = ApiMember(
    name = "play",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets the songs */
  def member_songs = ApiMember(
    name = "songs",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSongs,
    semantics = DefaultSemantics
  )

  /** Rarely used: Gets the thumbnail picture */
  def member_thumbnail = ApiMember(
    name = "thumbnail",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "art" -> member_art,
    "artist" -> member_artist,
    "duration" -> member_duration,
    "genre" -> member_genre,
    "has art" -> member_has_art,
    "name" -> member_name,
    "play" -> member_play,
    "songs" -> member_songs,
    "thumbnail" -> member_thumbnail
  )
            

}
          
