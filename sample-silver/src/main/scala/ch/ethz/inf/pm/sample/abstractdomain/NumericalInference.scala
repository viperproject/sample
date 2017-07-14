/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{IntegerOctagons, NumericalDomain}
import ch.ethz.inf.pm.sample.execution.{BlockPosition, CfgResult, InterproceduralSilverForwardAnalysis, SilverAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, InterproceduralSilverInferenceRunner, SilverInferenceRunner}
import viper.silver.ast.{And, Exp, Field, Or}

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

  //
  // For the interprocedural case we take all possible method-call entry states and extend the program with a disjunction
  //
  override def preconditions(existing: Seq[Exp], position: BlockPosition, result: CfgResult[S]): Seq[Exp] = {
    // get a set of inferred preconditions for each method-call (call-string)
    val inferred: Seq[Set[Expression]] = resultsToWorkWith.map(_.preStateAt(position).specifications).filter(_.nonEmpty)
    // represent each set of preconditions as one And()
    val conjuctionPerCall: Seq[Exp] = inferred.map(_.map(DefaultSampleConverter.convert).reduce((left, right) => And(left, right)()))
    // Or() alls the possible preconditions
    if (conjuctionPerCall.nonEmpty) {
      val additionalPreconditions: Exp = conjuctionPerCall.reduce((left, right) => Or(left, right)())
      existing :+ additionalPreconditions
    } else {
      existing
    }
  }

  override def invariants(existing: Seq[Exp], position: BlockPosition, result: CfgResult[S]): Seq[Exp] = {
    val inferred = resultsToWorkWith.map(_.preStateAt(position)).reduce(_ lub _).specifications
    val converted = inferred.map(DefaultSampleConverter.convert)
    existing ++ converted
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
  * An interprocedural inference based on the integer octagon analysis
  * The analysis uses full-length (full precision) call-strings. Use an approximate call-string for recursive methods.
  *
  * @author Flurin Rindisbacher
  */
object InterproceduralIntegerOctagonInference
  extends InterproceduralNumericalInferenceRunner[IntegerOctagonAnalysisState, IntegerOctagons] {
  override val analysis: InterproceduralSilverForwardAnalysis[IntegerOctagonAnalysisState] = InterproceduralIntegerOctagonAnalysis.analysis
}