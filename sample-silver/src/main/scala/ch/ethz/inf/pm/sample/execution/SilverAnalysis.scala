/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverMethodDeclaration, SilverProgramDeclaration}

trait SilverAnalysis[S <: State[S]] {
  def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[S]
}

trait SilverForwardAnalysis[S <: State[S]]
  extends SilverAnalysis[S] {
  protected def analyze(method: SilverMethodDeclaration, initial: S): CfgResult[S] = {
    val interpreter = FinalResultForwardInterpreter[S]()
    interpreter.execute(method.body, initial)
  }
}

trait SilverInterproceduralForwardAnalysis[S <: State[S]]
  extends SilverForwardAnalysis[S] {
  def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration, callsInProgram: Map[String, Set[BlockPosition]]): CfgResult[S]
}

case class SimpleSilverForwardAnalysis[S <: State[S]](builder: SilverEntryStateBuilder[S])
  extends SilverForwardAnalysis[S] {
  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[S] =
    analyze(method, builder.build(program, method))
}

case class SimpleInterproceduralSilverForwardAnalysis[S <: State[S]](val builder: SilverEntryStateBuilder[S])
  extends SilverInterproceduralForwardAnalysis[S] {

  //TODO @flurin this is never called in the interprocedural setting
  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[S] = ???

  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration, callsInProgram: Map[String, Set[BlockPosition]]): CfgResult[S] = {
    val interpreter = FinalResultInterproceduralForwardInterpreter[S](program, builder, callsInProgram)
    interpreter.execute(method.body, builder.build(program, method))
  }
}

trait SilverEntryStateBuilder[S <: State[S]] {
  def top: S

  def build(program: SilverProgramDeclaration, method: SilverMethodDeclaration): S
}

/**
  * A simple entry state builder that starts with the top state and initializes
  * the arguments.
  *
  * @tparam S The type of the state.
  * @author Jerome Dohrau
  */
trait SimpleEntryStateBuilder[S <: State[S]]
  extends SilverEntryStateBuilder[S] {
  override def build(program: SilverProgramDeclaration, method: SilverMethodDeclaration): S = {
    method.initializeArgument(top)
  }
}