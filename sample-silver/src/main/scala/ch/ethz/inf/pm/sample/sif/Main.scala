/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.sif

import ch.ethz.inf.pm.sample.abstractdomain.{IntegerOctagonAnalysisEntryState, IntegerOctagonAnalysisState}
import ch.ethz.inf.pm.sample.execution.{CfgResult, FinalResultForwardInterpreter, SilverAnalysis}
import ch.ethz.inf.pm.sample.inference.SilverExtender
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, SilverMethodDeclaration, SilverProgramDeclaration}
import ch.ethz.inf.pm.sample.sif.Config.NumericalState
import viper.silver.{ast => sil}

object Config {
  /**
    * The type of the state used for the numerical analysis.
    */
  type NumericalState = IntegerOctagonAnalysisState

  /**
    * The entry state used for the numerical analysis.
    */
  val numericalEntry = IntegerOctagonAnalysisEntryState
}

/**
  * The analysis.
  */
object Analysis extends SilverAnalysis[NumericalState] {
  /**
    * Analyzes the given method.
    *
    * @param program The program.
    * @param method  The method to analyze.
    * @return The result of the analysis.
    */
  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[NumericalState] = {
    // build entry state
    val entry = Config.numericalEntry.build(program, method)
    // create the interpreter and run it on the given method
    val interpreter = FinalResultForwardInterpreter(method.body, entry)
    interpreter.execute()
  }
}

object Main extends SilverExtender[NumericalState] {

  /**
    * The analysis to run.
    */
  override val analysis: SilverAnalysis[NumericalState] = Analysis

  /**
    * The main method that runs the analysis with the given arguments.
    *
    * @param arguments The arguments.
    */
  override def main(arguments: Array[String]): Unit = super.main(arguments)

  /**
    * Infers a list of preconditions for the given method using the given
    * analysis result.
    *
    * @param method The method for which the preconditions are inferred.
    * @param result The analysis result.
    * @return The inferred preconditions.
    */
  override def inferPreconditions(method: sil.Method, result: CfgResult[NumericalState]): Seq[sil.Exp] = {
    val existing = method.pres
    val inferred = {
      val position = firstPosition(result.cfg.entry)
      val state = result.preStateAt(position)
      getConstraints(state)
    }
    existing ++ inferred
  }

  /**
    * Infers a list of postconditions for the given method using the given
    * analysis result.
    *
    * @param method The method for which the postconditions are inferred.
    * @param result The analysis result.
    * @return The inferred postconditions.
    */
  override def inferPostconditions(method: sil.Method, result: CfgResult[NumericalState]): Seq[sil.Exp] = {
    val existing = method.posts
    val inferred = {
      val position = lastPosition(result.cfg.exit.get)
      val state = result.postStateAt(position)
      getConstraints(state)
    }
    existing ++ inferred
  }

  /**
    * Infers a list of invariants for the given while loop using the given
    * analysis result.
    *
    * @param loop   The while loop for which the invariants are inferred.
    * @param result The analysis result.
    * @return The inferred invariants.
    */
  override def inferInvariants(loop: sil.While, result: CfgResult[NumericalState]): Seq[sil.Exp] = {
    val existing = loop.invs
    val inferred = {
      val position = getLoopPosition(loop, result.cfg)
      val state = result.preStateAt(position)
      getConstraints(state)
    }
    existing ++ inferred
  }

  private def getConstraints(state: NumericalState): Seq[sil.Exp] = {
    val domain = state.domain
    val identifiers = domain.ids.toSet
    val constraints = domain.getConstraints(identifiers)
    constraints.map(DefaultSampleConverter.convert).toSeq
  }
}
