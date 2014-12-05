
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Songs
 *
 * A collection of songs
 *
 * @author Lucas Brutschy
 */

trait Default_TSongs extends ALinearCollection {

  lazy val typeName = TypeName("Songs")
          
  def keyType = TNumber

  def valueType = TSong

  /** Sometimes used: Plays the song. */
  def member_play = ApiMember(
    name = "play",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "play" -> member_play
  )
            

}
          
