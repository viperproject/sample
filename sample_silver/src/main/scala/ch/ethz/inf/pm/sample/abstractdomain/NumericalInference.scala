/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{IntegerOctagons, NumericalDomain}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.inference.{SilverExtender, SilverInferenceRunner}
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import viper.silver.{ast => sil}

/**
  * An inference based on a numerical analysis.
  *
  * @tparam S The type of the state.
  * @tparam D The type of the numerical domain.
  * @author Jerome Dohrau
  */
trait NumericalInferenceRunner[S <: NumericalAnalysisState[S, D], D <: NumericalDomain[D]]
  extends SilverInferenceRunner[S] {

  override def inferInvariants(loop: sil.While, result: CfgResult[S]): Seq[sil.Exp] = {
    val position = getLoopPosition(loop, result.cfg)
    val inferred = result.preStateAt(position).specifications
    val converted = inferred.map(DefaultSampleConverter.convert)
    loop.invs ++ converted.toSeq
  }
}

/**
  * An interprocedural inference based on a numerical analysis.
  *
  * @tparam S The type of the state.
  * @tparam D The type of the numerical domain.
  * @author Flurin Rindisbacher
  */
trait InterproceduralNumericalInferenceRunner[S <: NumericalAnalysisState[S, D], D <: NumericalDomain[D]]
  extends NumericalInferenceRunner[S, D] with InterproceduralSilverInferenceRunner[S] {

  private def asConjunction(specifications: Set[Expression]): sil.Exp = {
    if (specifications.nonEmpty)
      specifications.map(DefaultSampleConverter.convert).reduce((left, right) => sil.And(left, right)())
    else
      sil.TrueLit()()
  }

  //
  // For the interprocedural case we take all possible method-call entry states and extend the program with a disjunction
  //
  override def inferPreconditions(method: sil.Method, result: CfgResult[S]): Seq[sil.Exp] = {
    val position = firstPosition(result.cfg.entry)
    val existing = method.pres
    // get a set of inferred preconditions for each method-call (call-string)
    val inferred: Seq[Set[Expression]] = resultsToWorkWith.map(_.preStateAt(position).specifications)
    // represent each set of preconditions as a conjunction
    val conjuctionsPerCall: Seq[sil.Exp] = inferred.map(asConjunction).distinct
    // Or() all the possible preconditions
    if (conjuctionsPerCall.nonEmpty) {
      val inferredPreconditions: sil.Exp = conjuctionsPerCall.reduce((left, right) => sil.Or(left, right)())
      existing :+ inferredPreconditions
    } else {
      existing
    }
  }

  //
  //  Similar to invariants() we add postconditions in the form
  //    ensures pre1 => post1
  //    ...
  //
  //  For a bottom-up analysis there will be no precondition and the postconditions will be:
  //  ensures post1
  //  ensures post2 ...
  //
  override def inferPostconditions(method: sil.Method, result: CfgResult[S]): Seq[sil.Exp] = {
    val position = lastPosition(result.cfg.exit.get)
    val existing = method.posts
    // we only allow postconditions that talk about formalArgs and formalReturns
    val allowedIdentifiers = method.formalReturns.map(_.name).toSet ++ method.formalArgs.map(_.name).toSet

    val inferredPostconditions: Seq[sil.Exp] = {
      for (result <- resultsToWorkWith) yield {
        val precondition = asConjunction(result.entryState().specifications)
        val inferred = result.postStateAt(position).specifications
          // make sure we don't use expressions containing local variables
          .filter(_.ids.map(_.getName).toSet subsetOf allowedIdentifiers)
        val converted = inferred.map(DefaultSampleConverter.convert)
        // add (precondition => postcondition) for every encountered call-string
        precondition match {
          case _: sil.TrueLit => converted
          case p => converted.map(sil.Implies(p, _)())
        }
      }
    }.flatten

    existing ++ inferredPostconditions.distinct
  }

  //
  // we add invariants in the form of
  //    method-precondition => invariant
  // for all the call-strings we saw during analysis
  //
  override def inferInvariants(loop: sil.While, result: CfgResult[S]): Seq[sil.Exp] = {
    val position = getLoopPosition(loop, result.cfg)
    val existing = loop.invs
    val inferredInvariants: Seq[sil.Exp] = {
      for (result <- resultsToWorkWith) yield {
        val precondition = asConjunction(result.entryState().specifications)
        val inferred = result.preStateAt(position).specifications
        val converted = inferred.map(DefaultSampleConverter.convert)
        // add (precondition => invariant) for every encountered call-string
        precondition match {
          case _: sil.TrueLit => converted
          case p => converted.map(sil.Implies(p, _)())
        }
      }
    }.flatten
    existing ++ inferredInvariants
  }
}

/**
  * An interprocedural inference based on the integer octagon analysis.
  * The analysis uses call-strings with the length bounded to SystemParameters.callStringLength.
  *
  * @author Flurin Rindisbacher
  */
object InterproceduralIntegerOctagonInference
  extends InterproceduralNumericalInferenceRunner[IntegerOctagonAnalysisState, IntegerOctagons] {
  override val analysis: InterproceduralSilverForwardAnalysis[IntegerOctagonAnalysisState] = InterproceduralIntegerOctagonAnalysis.analysis
}

/**
  * An interprocedural bottom-up inference based on the integer octagon analysis.
  * The analysis first analyses all callees and then reuses the result in the callers.
  */
object InterproceduralIntegerOctagonBottomUpInference
  extends InterproceduralNumericalInferenceRunner[IntegerOctagonAnalysisState, IntegerOctagons]
    with InterproceduralSilverBottomUpInferenceRunner[IntegerOctagonAnalysisState]
    with InterproceduralSilverBottomUpAnalysisRunner[IntegerOctagonAnalysisState] {
  override val analysis: BottomUpAnalysis[IntegerOctagonAnalysisState] = SimpleInterproceduralSilverForwardBottomUpAnalysis(IntegerOctagonAnalysisEntryState)
}

/**
  * An interprocedural bottom-up inference based on the integer octagon analysis.
  * The analysis first analyses all callees and then reuses the result in the callers.
  *
  * The analysis returns a JSON-string with the inferred changes to the original program
  */
object InterproceduralIntegerOctagonBottomUpInferenceWithJsonExport
  extends InterproceduralNumericalInferenceRunner[IntegerOctagonAnalysisState, IntegerOctagons]
    with InterproceduralSilverBottomUpInferenceRunner[IntegerOctagonAnalysisState]
    with InterproceduralSilverBottomUpAnalysisRunner[IntegerOctagonAnalysisState]
    with SilverJsonExporter[IntegerOctagonAnalysisState] {

  override val analysis: BottomUpAnalysis[IntegerOctagonAnalysisState] = SimpleInterproceduralSilverForwardBottomUpAnalysis(IntegerOctagonAnalysisEntryState)

  override def main(arguments: Array[String]): Unit = {
    // run the analysis and print result as json
    export(arguments)
    val string = getString((new File(arguments(0)).toPath).toString)
    println(string)
  }
}

/**
  * An inference based on the integer octagon analysis.
  *
  * @author Jerome Dohrau
  */
object IntegerOctagonInference
  extends NumericalInferenceRunner[IntegerOctagonAnalysisState, IntegerOctagons]
    with SilverExtender[IntegerOctagonAnalysisState] {
  override val analysis: SilverAnalysis[IntegerOctagonAnalysisState] = IntegerOctagonAnalysis.analysis
}