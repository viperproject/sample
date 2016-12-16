/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics._
import ch.ethz.inf.pm.td.analysis.{ApiField, MethodSummaries, TouchAnalysisParameters}
import ch.ethz.inf.pm.td.compiler._
import ch.ethz.inf.pm.td.parser.TypeName

/**
  * This implements helper functions that I use for the analysis.
  *
  * In particular, that keeps track of whether handlers are enabled, and what type they are.
  *
  * @author Lucas Brutschy
  */

object SHelpers extends ASingleton {


  lazy val typeName = TypeName("Helpers", isSingleton = true)

  // ==== Cloud tracking
  lazy val field_last_operation = ApiField("*last operation", TString)
  lazy val member_yield =
    ApiMember(
      name = "yield",
      paramTypes = List(),
      thisType = ApiParam(this),
      returnType = TNothing,
      isAsync = true,
      pausesInterpreter = true,
      semantics = SkipSemantics // only semantics is "pausing interpreter"
    )
  val cloudArgumentFields = new scala.collection.mutable.HashMap[String, Set[ApiField]]
  // ==== Handler tracking
  val handlerCreatorMethods = new scala.collection.mutable.HashMap[String,ApiMember]
  val handlerEnabledFields = new scala.collection.mutable.HashMap[String,ApiField]
  val handlerTypes = new scala.collection.mutable.HashMap[String,TypeName]


  def getCloudArgumentFields[S <: State[S]](eventID: String)(implicit state: S): Set[ApiField] = {
    cloudArgumentFields(eventID)
  }

  def getCloudArgumentField(eventID: String, typ: AAny, name: String): ApiField = {
    val field = ApiField(name, typ)
    if (!cloudArgumentFields.contains(eventID) || !cloudArgumentFields(eventID).contains(field))
      cloudArgumentFields += (eventID -> (cloudArgumentFields.getOrElse(name, Set.empty) + field))
    field
  }

  def createHandler(handlerName:String, t:TypeName):String = {
    val n = handlerEnabledFieldName(handlerName)
    handlerTypes.put(handlerName, t)
    if (TouchAnalysisParameters.get.conditionalHandlers) {
      handlerEnabledFields.put(handlerName, ApiField(n, TString))
    }
    handlerCreatorMethods.put("create "+handlerName,ApiMember(
      name = "create " + handlerName,
      paramTypes = List(),
      thisType = ApiParam(this),
      returnType = TypeList.getTypeOrFail(t),
      semantics = new ApiMemberSemantics {
        override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember,
                                                     parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {
          val typ = TypeList.getTypeOrFail(t).asInstanceOf[AAction]
          var curState = state
          MethodSummaries.collectClosureEntry(handlerName,curState)
          curState = New[S](typ,Map(AAction.field_handlerName -> String(handlerName)))(curState,pp)
          curState
        }
      }
    ))
    n
  }

  def handlerEnabledFieldName(handlerName: String): String = {
    handlerName + " enabled"
  }

  override def declarations: Map[String, ApiMember] = {
    super.declarations ++ handlerCreatorMethods + ("yield" -> member_yield)
  }

  override def possibleFields: Set[Identifier] = {
    super.possibleFields ++ handlerEnabledFields.values ++ cloudArgumentFields.values.flatten + field_last_operation
  }

  override def reset(): Unit = {
    handlerEnabledFields.clear()
    handlerCreatorMethods.clear()
    handlerTypes.clear()
  }

}

