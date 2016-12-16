/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of App Env
 *
 * Various properties of application environment
 *
 * @author Lucas Brutschy
 */

trait Default_TApp_Env extends AAny {

  lazy val typeName = TypeName("App Env")

  override def declarations: Map[String, ApiMember] = super.declarations ++ Map(
    "backend url" -> member_backend_url,
    "form factor" -> member_form_factor,
    "has host" -> member_has_host,
    "has shell" -> member_has_shell,
    "initial url" -> member_initial_url,
    "operating system" -> member_operating_system,
    "runtime kind" -> member_runtime_kind,
    "set temporary setting" -> member_set_temporary_setting,
    "temporary setting" -> member_temporary_setting,
    "user agent" -> member_user_agent
  )

  /** Never used: Return URL of the cloud backend service if any. */
  def member_backend_url = ApiMember(
    name = "backend url",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Get device 'size': "phone", "tablet", or "desktop" */
  def member_form_factor = ApiMember(
    name = "form factor",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Indicates if the `app->host_exec` action can be used to run host commands. */
  def member_has_host = ApiMember(
    name = "has host",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: [**beta**] Indicates if the `app->run_command` action can be used to run shell commands. */
  def member_has_shell = ApiMember(
    name = "has shell",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TBoolean,
    semantics = DefaultSemantics
  )

  /** Never used: Initial URL used to launch the website; invalid when `->runtime kind` is "editor" */
  def member_initial_url = ApiMember(
    name = "initial url",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Get current OS: "windows", "osx", "linux", "wp", "ios", "android", ... */
  def member_operating_system = ApiMember(
    name = "operating system",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Where are we running from: "editor", "website", "nodejs", "mobileapp", "plugin" */
  def member_runtime_kind = ApiMember(
    name = "runtime kind",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Sets an in-memory editor setting. The setting is stored until the page is refreshed */
  def member_set_temporary_setting = ApiMember(
    name = "set temporary setting",
    paramTypes = List(ApiParam(TString), ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Retreives an in-memory editor setting */
  def member_temporary_setting = ApiMember(
    name = "temporary setting",
    paramTypes = List(ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )

  /** Never used: Get the browser name and version */
  def member_user_agent = ApiMember(
    name = "user agent",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TString,
    semantics = DefaultSemantics
  )
            

}
          
