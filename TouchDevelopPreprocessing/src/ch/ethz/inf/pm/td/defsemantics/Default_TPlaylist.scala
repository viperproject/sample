
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Playlist
 *
 * A song playlist
 *
 * @author Lucas Brutschy
 */

trait Default_TPlaylist extends AAny {

  lazy val typeName = TypeName("Playlist")
          
  /** Never used: [**not implemented**] Gets the duration in seconds */
  def member_duration = ApiMember(
    name = "duration",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Gets the name of the song */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Plays the songs in the playlist */
  def member_play = ApiMember(
    name = "play",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: [**not implemented**] Gets the songs */
  def member_songs = ApiMember(
    name = "songs",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TSongs,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "duration" -> member_duration,
    "name" -> member_name,
    "play" -> member_play,
    "songs" -> member_songs
  )
            

}
          
