/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.td.analysis.ApiField
import ch.ethz.inf.pm.td.defsemantics.Default_TApp_Env

/**
 * Customizes the abstract semantics of App Env
 *
 * Various properties of application environment
 *
 * @author Lucas Brutschy
 */

object TApp_Env extends Default_TApp_Env {

  lazy val field_user_agent = ApiField("user agent",TString)
  lazy val field_runtime_kind = ApiField("runtime kind",TString)
  lazy val field_operating_system = ApiField("operating system",TString)
  lazy val field_initial_url = ApiField("initial url",TString)
  lazy val field_has_shell = ApiField("has shell",TBoolean)
  lazy val field_has_host = ApiField("has host",TBoolean)
  lazy val field_form_factor = ApiField("form factor",TString)
  lazy val field_backend_url = ApiField("backend url",TString)

  override lazy val possibleFields = super.possibleFields ++ Set(
    field_user_agent,
    field_runtime_kind,
    field_operating_system,
    field_initial_url,
    field_has_shell,
    field_has_host,
    field_form_factor,
    field_backend_url
  )

}
          
