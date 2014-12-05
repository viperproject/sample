
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of User
 *
 * A user account
 *
 * @author Lucas Brutschy
 */

trait Default_TUser extends AAny {

  lazy val typeName = TypeName("User")
          
  /** Never used: Gets the about-me text of the user */
  def member_about = ApiMember(
    name = "about",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a value idincating if the user is the same as the other. */
  def member_equals = ApiMember(
    name = "equals",
    paramTypes = List(ApiParam(TUser)),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Indicates if the user has a picture */
  def member_has_picture = ApiMember(
    name = "has picture",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Gets a unique identifier for the user. */
  def member_id = ApiMember(
    name = "id",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the name of the user */
  def member_name = ApiMember(
    name = "name",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the url of the user picture where original is the unmodified user picture, square is 50x50, small has 50px width, normal has 100px width, large has roughly 200px width */
  def member_picture_address = ApiMember(
    name = "picture address",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the user picture where original is the unmodified user picture, square is 50x50, small has 50px width, normal has 100px width, large has roughly 200px width */
  def member_picture = ApiMember(
    name = "picture",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: Download user-data if needed */
  def member_preload = ApiMember(
    name = "preload",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "about" -> member_about,
    "equals" -> member_equals,
    "has picture" -> member_has_picture,
    "id" -> member_id,
    "name" -> member_name,
    "picture address" -> member_picture_address,
    "picture" -> member_picture,
    "preload" -> member_preload
  )
            

}
          
