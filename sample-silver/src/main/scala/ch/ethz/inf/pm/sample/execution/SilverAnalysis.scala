/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.execution.InterproceduralSilverInterpreter.CallGraphMap
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverIdentifier, SilverMethodDeclaration, SilverProgramDeclaration}

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

trait IntraproceduralSilverAnalysis[S <: State[S]]
  extends SilverAnalysis[S] {
}

trait InterproceduralSilverAnalysis[S <: State[S]]
  extends SilverAnalysis[S] {
  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[S] = {
    throw new RuntimeException("This method is not applicable for interprocedural analyses")
  }

  def analyze(program: SilverProgramDeclaration, mainMethods: Set[SilverIdentifier], callsInProgram: CallGraphMap): ProgramResult[S]
}

trait SilverForwardAnalysis[S <: State[S]]
  extends IntraproceduralSilverAnalysis[S] {
  protected def analyze(method: SilverMethodDeclaration, initial: S): CfgResult[S] = {
    val interpreter = FinalResultForwardInterpreter[S](method.body, initial)
    interpreter.execute()
  }
}

trait InterproceduralSilverForwardAnalysis[S <: State[S]]
  extends InterproceduralSilverAnalysis[S] {
}

trait InterproceduralSilverBackwardAnalysis[S <: State[S]]
  extends InterproceduralSilverAnalysis[S] {
}

case class SimpleSilverForwardAnalysis[S <: State[S]](builder: SilverEntryStateBuilder[S])
  extends SilverForwardAnalysis[S] {
  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[S] =
    analyze(method, builder.build(program, method))
}

case class SimpleInterproceduralSilverForwardAnalysis[S <: State[S]](builder: SilverEntryStateBuilder[S], callStringLength: Option[Int] = CallString.DefaultLength)
  extends InterproceduralSilverForwardAnalysis[S] {
  override def analyze(program: SilverProgramDeclaration, mainMethods: Set[SilverIdentifier], callsInProgram: CallGraphMap): ProgramResult[S] = {
    val interpreter = FinalResultInterproceduralForwardInterpreter[S](program, mainMethods, builder, callsInProgram, callStringLength)
    interpreter.executeInterprocedural()
  }
}


trait SilverBackwardAnalysis[S <: State[S]]
  extends IntraproceduralSilverAnalysis[S] {
  protected def analyze(method: SilverMethodDeclaration, initial: S): CfgResult[S] = {
    val interpreter = FinalResultBackwardInterpreter[S](method.body, initial)
    interpreter.execute()
  }
}

case class SimpleSilverBackwardAnalysis[S <: State[S]](builder: SilverEntryStateBuilder[S])
  extends SilverBackwardAnalysis[S] {
  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[S] =
    analyze(method, builder.build(program, method))
}

case class SimpleInterproceduralSilverBackwardAnalysis[S <: State[S]](builder: SilverEntryStateBuilder[S], callStringLength: Option[Int] = CallString.DefaultLength)
  extends InterproceduralSilverBackwardAnalysis[S] {
  override def analyze(program: SilverProgramDeclaration, mainMethods: Set[SilverIdentifier], callsInProgram: CallGraphMap): ProgramResult[S] = {
    val interpreter = FinalResultInterproceduralBackwardInterpreter[S](program, mainMethods, builder, callsInProgram, callStringLength)
    interpreter.executeInterprocedural()
  }
}

/**
  * An entry state builder that provides the entry state of an analysis.
  *
  * @tparam S The type of the state.
  * @author Jerome Dohrau
  */
trait SilverEntryStateBuilder[S <: State[S]] {
  /**
    * The default state used to build the entry state of an analysis.
    *
    * @return The default state.
    */
  def default: S

  /**
    * Builds and returns an entry state for the analysis of the given method of
    * the given program.
    *
    * By default this method takes the default state and initializes the
    * variables for the arguments of the given method in the given program.
    *
    * @param program The program.
    * @param method  The method.
    * @return The entry state.
    */
  def build(program: SilverProgramDeclaration, method: SilverMethodDeclaration): S = initializeArguments(default, program, method)

  /**
    * Takes the given state and initializes the variables for the arguments of
    * the given method in the given program.
    *
    * @param state   The state.
    * @param program The program.
    * @param method  The method.
    * @return The state with the arguments initialized.
    */
  def initializeArguments(state: S, program: SilverProgramDeclaration, method: SilverMethodDeclaration): S = {
    val declarations = method.arguments ++ method.returns
    declarations.foldLeft(state) {
      case (state, declaration) =>
        val evaluated = declaration.variable.forwardSemantics(state)
        val argument = evaluated.expr
        evaluated.removeExpression().createVariableForArgument(argument, declaration.typ)
    }
  }

  /**
    * Builds and returns an entry state for the analysis of the given method of
    * the given program under the assumption that the analyzed method is a callee.
    *
    * This builder can be used if the entrystate for a method should be different depending on whether the method
    * is called by other methods or analysed "at the top of the callgraph".
    *
    * @param program The program.
    * @param method  The method.
    * @return The entry state.
    */
  def buildForMethodCallEntry(program: SilverProgramDeclaration, method: SilverMethodDeclaration): S = build(program, method)
}
