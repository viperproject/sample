
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Message Collection
 *
 * A list of messages
 *
 * @author Lucas Brutschy
 */

trait Default_TMessage_Collection extends AMutable_Collection {

  lazy val typeName = TypeName("Message Collection")
          
  def keyType = TNumber

  def valueType = TMessage

  /** Never used: Gets the identifier of the next set of messages */
  def member_continuation = ApiMember(
    name = "continuation",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Sets the identifier of the next set of messages */
  def member_set_continuation = ApiMember(
    name = "set continuation",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Sorts from the newest to oldest */
  def member_sort_by_date = ApiMember(
    name = "sort by date",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "continuation" -> member_continuation,
    "set continuation" -> member_set_continuation,
    "sort by date" -> member_sort_by_date
  )
            

}
          
