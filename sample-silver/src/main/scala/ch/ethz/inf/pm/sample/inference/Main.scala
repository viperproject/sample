/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.inference

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.IntegerOctagons
import ch.ethz.inf.pm.sample.analysis._
import ch.ethz.inf.pm.sample.domain.{HeapNode, MayAliasGraph, MustAliasGraph}
import ch.ethz.inf.pm.sample.execution.{BlockPosition, CfgResult, SilverAnalysis, SimpleSilverForwardAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverCompiler, SilverJsonExporter}
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisState.SimplePermissionAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.{PermissionAnalysis, PermissionAnalysisEntryStateBuilder, PermissionInferenceRunner}
import viper.silver.{ast => sil}

/**
  * An inference that infers permissions and numerical values using an octagon
  * analysis.
  *
  * @author Jerome Dohrau
  */
object Main {
  /**
    * The type of the may alias domain used by the alias analysis.
    */
  type May = MayAliasGraph

  /**
    * The type of the must alias domain used by the alias analysis.
    */
  type Must = MustAliasGraph

  /**
    * The type of the numerical domain used by the value analysis.
    */
  type Numerical = IntegerOctagons

  /**
    * The type of the alias analysis state.
    */
  type A = SimpleAliasAnalysisState

  /**
    * The type of the permission analysis state.
    */
  type P = SimplePermissionAnalysisState

  /**
    * The type of the value analysis state.
    */
  type V = SimpleHeapAndSemanticAnalysisState[May, Numerical, HeapNode]

  /**
    * The permission inference.
    */
  val permission = new PermissionInferenceRunner[P, A, May, Must] with SilverJsonExporter[P] {

    override val analysis: SilverAnalysis[P] = PermissionAnalysis[P, A, May, Must](
      aliasAnalysisStateBuilder = AliasAnalysisEntryStateBuilder(),
      permissionAnalysisStateBuilder = PermissionAnalysisEntryStateBuilder()
    )
  }

  /**
    * The numerical value inference.
    */
  val numerical = new SilverInferenceRunner[V]
    with SilverJsonExporter[V] {

    override val analysis: SilverAnalysis[V] = SimpleSilverForwardAnalysis(
      builder = HeapAndOctagonAnalysisEntryState
    )

    override def inferPostconditions(method: sil.Method, position: BlockPosition, result: CfgResult[V]): Seq[sil.Exp] = {
      val state = result.postStateAt(position)
      method.posts
    }

    override def inferInvariants(loop: sil.While, position: BlockPosition, result: CfgResult[V]): Seq[sil.Exp] = {
      val state = result.preStateAt(position)
      loop.invs
    }
  }

  /**
    * Runs the inference.
    *
    * @param args The first arguments is expected to be the file to be analyzed.
    */
  def main(args: Array[String]): Unit = {
    assert(args.nonEmpty, "No file specified.")

    // compile program
    val compiler = new SilverCompiler()
    val compilable = Compilable.Path(new File(args(0)).toPath)
    val program = compiler.compile(compilable)

    val permissionResults = permission.run(program)
    permission.exportProgram(program, permissionResults)
    println(permission.specificationsAsJson("permission"))

    val numericalResults = numerical.run(program)
    numerical.exportProgram(program, numericalResults)
    println(numerical.specificationsAsJson("numerical"))
  }
}