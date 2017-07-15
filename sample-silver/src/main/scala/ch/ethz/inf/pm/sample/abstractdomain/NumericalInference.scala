/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{IntegerOctagons, NumericalDomain}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, InterproceduralSilverBottomUpAnalysisRunner, InterproceduralSilverInferenceRunner, SilverInferenceRunner}
import viper.silver.ast._

/**
  * An inference based on a numerical analysis.
  *
  * @tparam S The type of the state.
  * @tparam D The type of the numerical domain.
  * @author Jerome Dohrau
  */
trait NumericalInferenceRunner[S <: NumericalAnalysisState[S, D], D <: NumericalDomain[D]]
  extends SilverInferenceRunner[Set[Expression], S] {

  override def preconditions(existing: Seq[Exp], position: BlockPosition, result: CfgResult[S]): Seq[Exp] = existing

  override def postconditions(existing: Seq[Exp], position: BlockPosition, result: CfgResult[S]): Seq[Exp] = existing

  override def invariants(existing: Seq[Exp], position: BlockPosition, result: CfgResult[S]): Seq[Exp] = {
    val inferred = result.preStateAt(position).specifications
    val converted = inferred.map(DefaultSampleConverter.convert)
    existing ++ converted.toSeq
  }

  override def fields(existing: Seq[Field], position: BlockPosition, result: CfgResult[S]): Seq[Field] = existing
}

/**
  * An interprocedural inference based on a numerical analysis.
  *
  * @tparam S The type of the state.
  * @tparam D The type of the numerical domain.
  * @author Flurin Rindisbacher
  */
trait InterproceduralNumericalInferenceRunner[S <: NumericalAnalysisState[S, D], D <: NumericalDomain[D]]
  extends NumericalInferenceRunner[S, D] with InterproceduralSilverInferenceRunner[Set[Expression], S] {

  private def asConjunction(specifications: Set[Expression]): Option[Exp] = {
    if (specifications.nonEmpty)
      Some(specifications.map(DefaultSampleConverter.convert).reduce((left, right) => And(left, right)()))
    else
      None
  }

  //
  // For the interprocedural case we take all possible method-call entry states and extend the program with a disjunction
  //
  override def preconditions(existing: Seq[Exp], position: BlockPosition, result: CfgResult[S]): Seq[Exp] = {
    // get a set of inferred preconditions for each method-call (call-string)
    val inferred: Seq[Set[Expression]] = resultsToWorkWith.map(_.preStateAt(position).specifications)
    // represent each set of preconditions as a conjunction
    val conjuctionsPerCall: Seq[Exp] = inferred.flatMap(asConjunction)
    // Or() all the possible preconditions
    if (conjuctionsPerCall.nonEmpty) {
      val inferredPreconditions: Exp = conjuctionsPerCall.reduce((left, right) => Or(left, right)())
      existing :+ inferredPreconditions
    } else {
      existing
    }
  }

  //
  // we add invariants in the form of
  //    method-precondition => invariant
  // for all the call-strings we saw during analysis
  //
  override def invariants(existing: Seq[Exp], position: BlockPosition, result: CfgResult[S]): Seq[Exp] = {
    val inferredInvariants: Seq[Exp] = {
      for (result <- resultsToWorkWith) yield {
        val precondition = asConjunction(result.entryState().specifications)
        val inferred = result.preStateAt(position).specifications
        val converted = inferred.map(DefaultSampleConverter.convert)
        // add (precondition => invariant) for every encountered call-string
        precondition match {
          case Some(p) =>
            converted.map(Implies(p, _)())
          case None =>
            converted
        }
      }
    }.flatten
    existing ++ inferredInvariants
  }
}

/**
  * An inference based on the integer octagon analysis.
  *
  * @author Jerome Dohrau
  */
object IntegerOctagonInference
  extends NumericalInferenceRunner[IntegerOctagonAnalysisState, IntegerOctagons] {
  override val analysis: SilverAnalysis[IntegerOctagonAnalysisState] = IntegerOctagonAnalysis.analysis
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
  * The analysis first analysies all callees and then reuses then reuses the result in the callers.
  */
object InterproceduralIntegerOctagonBottomUpInference
  extends InterproceduralNumericalInferenceRunner[IntegerOctagonAnalysisState, IntegerOctagons]
    // run the analysis bottom-up
    with InterproceduralSilverBottomUpAnalysisRunner[IntegerOctagonAnalysisState] {
  override val analysis: BottomUpAnalysis[IntegerOctagonAnalysisState] = SimpleInterproceduralSilverForwardBottomUpAnalysis(IntegerOctagonAnalysisEntryState)
}