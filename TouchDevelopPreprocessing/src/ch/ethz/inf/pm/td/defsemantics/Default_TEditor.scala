
package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Editor
 *
 * An interface to TouchDevelop editor
 *
 * @author Lucas Brutschy
 */

trait Default_TEditor extends AAny {

  lazy val typeName = TypeName("Editor")
          
  /** Never used: Place a message on an AST node */
  def member_annotate_ast = ApiMember(
    name = "annotate ast",
    paramTypes = List(ApiParam(TString), ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Returns the AST of the script currently in the editor */
  def member_current_script_ast = ApiMember(
    name = "current script ast",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: The id of the script currently in the editor */
  def member_current_script_id = ApiMember(
    name = "current script id",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Get (Azure) web site deployment settings. */
  def member_deployment_settings = ApiMember(
    name = "deployment settings",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: [**dbg**] Compiles and packages the current script. */
  def member_package_current_script = ApiMember(
    name = "package current script",
    paramTypes = List(ApiParam(TJson_Object)),
    thisType = ApiParam(this),
    returnType = TJson_Object,
    semantics = DefaultSemantics
  )

  /** Never used: Replace standard 'running plugin' message with something else */
  def member_progress = ApiMember(
    name = "progress",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Signal that the current tutorial step is done. */
  def member_tutorial_step_completed = ApiMember(
    name = "tutorial step completed",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Uploads a payload to a temporary blob and retreives the url. While the URL will remain unchanged, the data will eventually be wiped out. */
  def member_upload_json = ApiMember(
    name = "upload json",
    paramTypes = List(ApiParam(TJson_Builder)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Authenticates the user for the given app name and returns the token if successful. The identity of the token is contructed with ``{app name} - {user id}``. */
  def member_user_token = ApiMember(
    name = "user token",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "annotate ast" -> member_annotate_ast,
    "current script ast" -> member_current_script_ast,
    "current script id" -> member_current_script_id,
    "deployment settings" -> member_deployment_settings,
    "package current script" -> member_package_current_script,
    "progress" -> member_progress,
    "tutorial step completed" -> member_tutorial_step_completed,
    "upload json" -> member_upload_json,
    "user token" -> member_user_token
  )
            

}
          
