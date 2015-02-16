
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Cloud Session
 *
 * A cloud data session
 *
 * @author Lucas Brutschy
 */

trait Default_TCloud_Session extends AAny {

  lazy val typeName = TypeName("Cloud Session")
          
  /** Sometimes used: Gets a string that uniquely identifies this cloud session; other users can connect by using this string. */
  def member_id = ApiMember(
    name = "id",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if the current user owns this session */
  def member_is_owned = ApiMember(
    name = "is owned",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets information about the user that owns this session */
  def member_owner = ApiMember(
    name = "owner",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TUser,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Query server about current state of this session. You must be the authenticated owner. */
  def member_server_info = ApiMember(
    name = "server info",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Gets a string that describes this cloud session */
  def member_title = ApiMember(
    name = "title",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "id" -> member_id,
    "is owned" -> member_is_owned,
    "owner" -> member_owner,
    "server info" -> member_server_info,
    "title" -> member_title
  )
            

}
          
