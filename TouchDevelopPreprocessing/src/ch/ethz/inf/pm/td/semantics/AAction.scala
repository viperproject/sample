/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.reporting.Reporter
import ch.ethz.inf.pm.td.analysis.{TouchAnalysisParameters, MethodSummaries, ApiField, RichNativeSemantics}
import ch.ethz.inf.pm.td.compiler._
import RichNativeSemantics._

object AAction {

  /** Stores a string representing the handler in the code. When an action is defined in the code, the
    * corresponding action is created with a unique name (e.g. program point based) and this object is
    * returned with the handlerName field set to the name of the created action. If this field
    * is top, and run is executed, we have to go to top, since we do not know what is executed */
  lazy val field_handlerName = ApiField("*handlername",TString)


  /**
    * Implements a standard abstract semantics for enabling an event handler and returning the
    * event binding
    *
    * @param handlerField the field that stores the event handler
    * @param argIndex the index of the action provided to this method, defaulting to zero (first argument)
    */
  case class EnableSemantics(handlerField:ApiField,argIndex:Int = 0) extends ApiMemberSemantics {
    override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember,
                                                 parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S): S = {

      assert(parameters.length > argIndex)
      assert(parameters(argIndex).typ.isInstanceOf[AAction])

      var curState = state
      curState = AssignField[S](this0, handlerField, parameters(argIndex))(curState,pp)
      curState = parameters(argIndex).typ.asInstanceOf[AAction].Enable[S](parameters(argIndex))(curState,pp)
      curState = New[S](TEvent_Binding)(curState,pp)
      curState

    }
  }


}

/**
 * General definition for Actions (closure types)
 *
 * @author Lucas Brutschy
 */
trait AAction extends AAny {

  def Enable[S <: State[S]](this0:ExpressionSet)(implicit state: S, pp: ProgramPoint):S = {

    if (TouchAnalysisParameters.get.conditionalHandlers) {
      EvalConstant[S](Field[S](this0, AAction.field_handlerName)) match {
        case SetDomain.Default.Bottom() =>
          Reporter.reportImpreciseSemantics("Going to bottom, no handler name", pp)
          state.bottom()
        case SetDomain.Default.Top() =>
          Reporter.reportImpreciseSemantics("Handler name is top", pp)
          state.top()
        case SetDomain.Default.Inner(xs) =>
          Lattice.bigLub(
            xs map { x =>
              AssignField[S](Singleton(SHelpers), SHelpers.handlerEnabledFieldName(x.constant), String("enabled"))(state, pp)
            }
          )
      }
    } else state

  }

  def Disable[S <: State[S]](this0:ExpressionSet)(implicit state: S, pp: ProgramPoint):S = {

    if (TouchAnalysisParameters.get.conditionalHandlers) {
      EvalConstant[S](Field[S](this0, AAction.field_handlerName)) match {
        case SetDomain.Default.Bottom() =>
          Reporter.reportImpreciseSemantics("Going to bottom, no handler name", pp)
          state.bottom()
        case SetDomain.Default.Top() =>
          Reporter.reportImpreciseSemantics("Handler name is top", pp)
          state.top()
        case SetDomain.Default.Inner(xs) =>
          Lattice.bigLub(
            xs map { x =>
              AssignField[S](Singleton(SHelpers), SHelpers.handlerEnabledFieldName(x.constant), String(""))(state, pp)
            }
          )
      }
    } else state

  }

  def Run[S <: State[S]](this0:ExpressionSet, parameters: List[ExpressionSet])(implicit state: S, pp: ProgramPoint):S = {

    val compiler = SystemParameters.compiler.asInstanceOf[TouchCompiler]

    def defaultBehavior() = {
      if (TouchAnalysisParameters.get.defaultToUnsound) {
        Top[S](actionReturnValue)
      } else {
        // call all handlers
        Lattice.bigLub(compiler.allMethods map { x =>
          if (CFGGenerator.isHandlerIdent(x.name.toString)) {
            MethodSummaries.collect(pp, x, state, parameters)
          } else {
            Reporter.reportImpreciseSemantics("Going to bottom, invalid handler name", pp)
            state.bottom()
          }
        })
      }
    }

    EvalConstant[S](Field[S](this0,AAction.field_handlerName)) match {
      case SetDomain.Default.Bottom() =>
        Reporter.reportImpreciseSemantics("Going to bottom, no handler name", pp)
        state.bottom()
      case SetDomain.Default.Top() =>
        Reporter.reportImpreciseSemantics("Handler name is top", pp)
        defaultBehavior()
      case SetDomain.Default.Inner(xs) =>
        Lattice.bigLub(
          xs map { x =>
            compiler.getMethod(x.constant, parameters map (_.typ)) match {
              case Some(method) =>
                MethodSummaries.collect(pp, method, state, parameters)
              case _ =>
                Reporter.reportImpreciseSemantics("Invalid handler name " + x.constant, pp)
                defaultBehavior()
            }
          }
        )
    }

  }

  /** Run the inline action. */
  def member_run = ApiMember(
    name = "run",
    paramTypes = actionArguments,
    thisType = ApiParam(this),
    returnType = actionReturnValue,
    semantics = new ApiMemberSemantics {
      override def forwardSemantics[S <: State[S]](this0: ExpressionSet, method: ApiMember, parameters: List[ExpressionSet])(implicit pp: ProgramPoint, state: S) = {
        var curState = state
        curState = Enable[S](this0)(curState,pp)
        curState = Run[S](this0,parameters)(curState,pp)
        val expr = curState.expr
        curState = Disable[S](this0)(curState,pp)
        curState.setExpression(expr)
      }
    }
  )

  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "run" -> member_run
  )

  def actionArguments: List[ApiParam]
  def actionReturnValue: AAny

  override def possibleFields = super.possibleFields + AAction.field_handlerName

}
