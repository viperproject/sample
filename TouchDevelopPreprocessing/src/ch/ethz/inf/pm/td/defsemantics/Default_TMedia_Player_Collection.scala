
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Media Player Collection
 *
 * A collection of media players
 *
 * @author Lucas Brutschy
 */

trait Default_TMedia_Player_Collection extends ALinearCollection {

  lazy val typeName = TypeName("Media Player Collection")
          
  def keyType = TNumber

  def valueType = TMedia_Player

  /** Never used: [**not implemented**] [**obsolete**] Adds a new player to the collection */
  def member_add = ApiMember(
    name = "add",
    paramTypes = List(ApiParam(TMedia_Player)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**not implemented**] [**obsolete**] Clears the players */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add" -> member_add,
    "clear" -> member_clear
  )
            

}
          
