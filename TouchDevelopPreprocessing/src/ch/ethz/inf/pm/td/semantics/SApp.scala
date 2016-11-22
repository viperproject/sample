/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, State}
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis._
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.defsemantics.Default_SApp
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
 * Specifies the abstract semantics of App
 *
 * Interact with the app runtime
 *
 * @author Lucas Brutschy
 */

object SApp extends Default_SApp {

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "javascript import" -> member_javascript_import
  )

  lazy val member_javascript_import = ApiMember(
    name = "javascript import",
    paramTypes = List(ApiParam(TString),ApiParam(TString)),
    thisType = ApiParam(this),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  override lazy val member_stop = super.member_stop.copy(semantics = StopSemantics)
  override lazy val member_log = super.member_log.copy(semantics = SkipSemantics)
  override lazy val member_restart = super.member_log.copy(semantics = ExitSemantics)
  override lazy val member_create_logger = super.member_create_logger.copy(semantics = ValidPureSemantics)

  /** Never used: Aborts the execution if the condition is false. */
  override lazy val member_fail_if_not = ApiMember("fail if not", List(ApiParam(TBoolean)), ApiParam(this), TNothing, new ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, member: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
      val List(condition) = parameters // Boolean
      if (TouchAnalysisParameters.get.printValuesInWarnings)
        Error[S](condition.not(), "fail if not", "fail if not " + condition + " might fail")
      else
        Error[S](condition.not(), "fail if not", "fail if not might fail")
      Skip
    }
  })

  lazy val field_server_response = ApiField("server response",TServer_Response,TopInitializer)
  lazy val field_server_request = ApiField("server request",TServer_Request,TopInitializer)
  lazy val field_env = ApiField("env",TApp_Env,TopInitializer)
  lazy val field_editor = ApiField("editor",TEditor,TopInitializer)
  lazy val field_current_handler = ApiField("current handler",TEvent_Binding,TopInitializer)
  lazy val field_script_id = ApiField("script id",TString,TopInitializer)

   override lazy val possibleFields = super.possibleFields ++ Set(
     field_server_response,
     field_server_request,
     field_env,
     field_editor,
     field_current_handler,
     field_script_id
   )

}
      
