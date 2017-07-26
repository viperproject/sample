/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{IntegerOctagons, NumericalDomain}
import ch.ethz.inf.pm.sample.execution.{BlockPosition, CfgResult, SilverAnalysis}
import ch.ethz.inf.pm.sample.inference.{SilverExtender, SilverInferenceRunner}
import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
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

  override def inferInvariants(loop: sil.While, position: BlockPosition, result: CfgResult[S]): Seq[sil.Exp] = {
    val inferred = result.preStateAt(position).specifications
    val converted = inferred.map(DefaultSampleConverter.convert)
    loop.invs ++ converted.toSeq
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

  override def main(args: Array[String]): Unit = {
    require(args.nonEmpty, "No file specified")

    val compilable = Compilable.Path(new File(args(0)).toPath)
    val program = compile(compilable)

    val result = run(program)
    val extended = extendProgram(program, result)

    println(extended)
  }
}