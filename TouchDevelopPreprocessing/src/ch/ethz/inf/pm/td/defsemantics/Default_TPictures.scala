
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Pictures
 *
 * A collection of pictures
 *
 * @author Lucas Brutschy
 */

trait Default_TPictures extends ALinearCollection {

  lazy val typeName = TypeName("Pictures")
          
  def keyType = TNumber

  def valueType = TPicture

  /** Rarely used: Finds a picture by name and returns the index. Returns -1 if not found. */
  def member_find = ApiMember(
    name = "find",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the full resolution of i-th picture. */
  def member_full = ApiMember(
    name = "full",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the thumbnail of i-th picture. */
  def member_thumbnail = ApiMember(
    name = "thumbnail",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "find" -> member_find,
    "full" -> member_full,
    "thumbnail" -> member_thumbnail
  )
            

}
          
