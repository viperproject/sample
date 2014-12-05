
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of String Collection
 *
 * A collection of strings
 *
 * @author Lucas Brutschy
 */

trait Default_TString_Collection extends AMutable_Collection {

  lazy val typeName = TypeName("String Collection")
          
  def keyType = TNumber

  def valueType = TString

  /** Sometimes used: Concatenates the separator and items into a string */
  def member_join = ApiMember(
    name = "join",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Shares the content (email, sms, social) */
  def member_share = ApiMember(
    name = "share",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "join" -> member_join,
    "share" -> member_share
  )
            

}
          
