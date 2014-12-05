
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Cloud Picture
 *
 * A picture hosted on OneDrive.
 *
 * @author Lucas Brutschy
 */

trait Default_TCloud_Picture extends AAny {

  lazy val typeName = TypeName("Cloud Picture")
          
  /** Never used: Downloads the picture with a particular size. */
  def member_download_picture = ApiMember(
    name = "download picture",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the picture with a particular size. */
  def member_to_picture = ApiMember(
    name = "to picture",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TPicture,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "download picture" -> member_download_picture,
    "to picture" -> member_to_picture
  )
            

}
          
