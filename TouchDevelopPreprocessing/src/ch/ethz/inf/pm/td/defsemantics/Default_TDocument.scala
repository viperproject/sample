
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Document
 *
 * A document resource
 *
 * @author Lucas Brutschy
 */

trait Default_TDocument extends AAny {

  lazy val typeName = TypeName("Document")
          
  /** Never used: Shows a document link in the docs. */
  def member_docs_render = ApiMember(
    name = "docs render",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the web address for the document. */
  def member_url = ApiMember(
    name = "url",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "docs render" -> member_docs_render,
    "url" -> member_url
  )
            

}
          