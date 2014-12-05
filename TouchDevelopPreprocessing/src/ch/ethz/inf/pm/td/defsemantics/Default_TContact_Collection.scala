
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Contact Collection
 *
 * A collection of contacts
 *
 * @author Lucas Brutschy
 */

trait Default_TContact_Collection extends ALinearCollection {

  lazy val typeName = TypeName("Contact Collection")
          
  def keyType = TNumber

  def valueType = TContact

  /** Never used: [**obsolete**] Adds a contact */
  def member_add = ApiMember(
    name = "add",
    paramTypes = List(ApiParam(TContact)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Clears the collection */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Gets the name of this collection */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Sets the contact at index */
  def member_set_at = ApiMember(
    name = "set at",
    paramTypes = List(ApiParam(TNumber), ApiParam(TContact)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: [**obsolete**] Sets the name of this collection */
  def member_set_name = ApiMember(
    name = "set name",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add" -> member_add,
    "clear" -> member_clear,
    "name" -> member_name,
    "set at" -> member_set_at,
    "set name" -> member_set_name
  )
            

}
          
