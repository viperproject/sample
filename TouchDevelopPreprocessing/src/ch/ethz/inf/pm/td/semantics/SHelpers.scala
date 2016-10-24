
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.td.analysis.{ApiField, MethodSummaries, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler.{CFGGenerator, TouchException, TouchType, TypeList}
import ch.ethz.inf.pm.td.parser.TypeName
import RichNativeSemantics._

/**
  * This implements helper functions that I use for the analysis.
  *
  * In particular, that keeps track of whether handlers are enabled, and what type they are.
  *
  * @author Lucas Brutschy
  */

object SHelpers extends ASingleton {

  val handlerEnabledFields = new scala.collection.mutable.HashMap[String,ApiField]
  val handlerTypes = new scala.collection.mutable.HashMap[String,TypeName]

  lazy val typeName = TypeName("Helpers",isSingleton = true)

  val CreateMethod = """create (.+)""".r

  def handlerEnabledFieldName(handlerName:String):String = {
     handlerName+" enabled"
  }

  def createHandler(handlerName:String, t:TypeName):String = {
    val n = handlerEnabledFieldName(handlerName)
    handlerTypes.put(handlerName, t)
    handlerEnabledFields.put(handlerName, ApiField(n, TString))
    n
  }

  override def possibleFields: Set[Identifier] = {
    super.possibleFields ++ handlerEnabledFields.values
  }

  override def reset(): Unit = {
    handlerEnabledFields.clear()
  }

  override def forwardSemantics[S <: State[S]](this0:ExpressionSet, method:String, parameters:List[ExpressionSet], returnedType:TouchType)
                                              (implicit pp:ProgramPoint,state:S):S = method match {

    /** Creates an action with the given name */
    case CreateMethod(handlerName) if CFGGenerator.isHandlerIdent(handlerName) =>

      (handlerTypes.get(handlerName), handlerEnabledFields.get(handlerName)) match {

        case (Some(t),Some(field)) =>

          val typ = TypeList.getTypeOrFail(t).asInstanceOf[AAction]
          var curState = state
          MethodSummaries.collectClosureEntry(handlerName,curState)
          curState = New[S](typ,Map(AAction.field_handlerName -> String(handlerName)))(curState,pp)
          curState

        case _ =>
          throw TouchException("Could not recover handler")

      }

    case _ =>
      super.forwardSemantics(this0,method,parameters,returnedType)


  }
}

