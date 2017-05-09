/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverMethodDeclaration, SilverProgramDeclaration}

trait SilverAnalysis[S <: State[S]] {
  /**
    * Analyzes all methods of the given program.
    *
    * @param program The program to analyze.
    * @return The result of the analysis.
    */
  def analyze(program: SilverProgramDeclaration): ProgramResult[S] = {
    val results = DefaultProgramResult[S](program)
    for (method <- program.methods) {
      val result = analyze(program, method)
      results.setResult(method.name, result)
    }
    results
  }

  /**
    * Analyzes the given method.
    *
    * @param program The program.
    * @param method  The method to analyze.
    * @return The result of the analysis.
    */
  def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[S]
}

trait SilverForwardAnalysis[S <: State[S]]
  extends SilverAnalysis[S] {
  protected def analyze(method: SilverMethodDeclaration, initial: S): CfgResult[S] = {
    val interpreter = FinalResultForwardInterpreter[S]()
    interpreter.execute(method.body, initial)
  }
}

case class SimpleSilverForwardAnalysis[S <: State[S]](builder: SilverEntryStateBuilder[S])
  extends SilverForwardAnalysis[S] {
  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[S] =
    analyze(method, builder.build(program, method))
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