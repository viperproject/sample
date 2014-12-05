
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Playlists
 *
 * A collection of playlists
 *
 * @author Lucas Brutschy
 */

trait Default_TPlaylists extends ALinearCollection {

  lazy val typeName = TypeName("Playlists")
          
  def keyType = TNumber

  def valueType = TPlaylist


}
          
