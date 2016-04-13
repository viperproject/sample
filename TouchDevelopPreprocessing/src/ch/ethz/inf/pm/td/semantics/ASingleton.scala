/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.abstractdomain.{VariableIdentifier, ExpressionSet, State}
import ch.ethz.inf.pm.td.analysis.RichNativeSemantics
import ch.ethz.inf.pm.td.compiler.{TouchCompiler, TouchSingletonProgramPoint}

/**
 * Super-class of all singletons
 */
trait ASingleton extends AAny {

  override def isSingleton = true

  def reference:ExpressionSet = {
    val singletonProgramPoint = TouchSingletonProgramPoint(this.name)
    ExpressionSet(VariableIdentifier(this.name.toLowerCase)(this, singletonProgramPoint))
  }

  /**
   * Initializes the singleton in the initial state
   *
   * @param state The state where the singleton is not initialized
   * @tparam S The type of the state
   * @return A state where the singleton is initialized
   */
  def initialize[S <: State[S]](state:S):S = {
    val singletonProgramPoint = TouchSingletonProgramPoint(this.name)
    val curState = RichNativeSemantics.New[S](this)(state, singletonProgramPoint)
    val obj = curState.expr
    val variable = ExpressionSet(VariableIdentifier(this.name.toLowerCase)(this, singletonProgramPoint))
    RichNativeSemantics.Assign[S](variable, obj)(curState, singletonProgramPoint)
  }

  /**
   * Some singletons (data, records) need to be set up depending on the current script.
   *
   * @param compiler The compiler
   * @param firstStart Initialize for the first start, or any start of the script.
   *                   Do we compute a fixpoint over all starts?
   */
  def setUp(compiler:TouchCompiler,firstStart:Boolean) = ()

  /**
   * Resets persistent data
   */
  def reset() = ()

}
