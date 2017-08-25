/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.inference

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.InterproceduralIntegerOctagonBottomUpInferenceWithJsonExport
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.IntegerOctagons
import ch.ethz.inf.pm.sample.analysis._
import ch.ethz.inf.pm.sample.domain.{HeapNode, MayAliasGraph, MustAliasGraph}
import ch.ethz.inf.pm.sample.execution.{CfgResult, SilverAnalysis, SimpleSilverForwardAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.Compilable
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, SilverCompiler, SilverIdentifier, SilverJsonExporter}
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisState.SimplePermissionAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.util.Context
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
  val permission = new PermissionInferenceRunner[P, A, May, Must]
    with SilverJsonExporter[P]
    with SilverExtender[P] {

    override def extendMethod(method: sil.Method, result: CfgResult[P]): sil.Method = {
      // TODO: There might be a better place to set the current method.
      Context.setMethod(SilverIdentifier(method.name))
      super.extendMethod(method, result)
    }

    override def exportMethod(method: sil.Method, result: CfgResult[P]): Unit = {
      // TODO: There might be a better place to set the current method.
      Context.setMethod(SilverIdentifier(method.name))
      super.exportMethod(method, result)
    }

    override val analysis: SilverAnalysis[P] = PermissionAnalysis[P, A, May, Must](
      aliasAnalysisStateBuilder = AliasAnalysisEntryStateBuilder(),
      permissionAnalysisStateBuilder = PermissionAnalysisEntryStateBuilder()
    )
  }

  /**
    * An inference that infers the values of numerical variables and fields.
    */
  val numerical = new SilverInferenceRunner[V]
    with SilverJsonExporter[V]
    with SilverExtender[V] {

    override val analysis: SilverAnalysis[V] = SimpleSilverForwardAnalysis(
      builder = HeapAndOctagonAnalysisEntryState
    )

    override def inferPostconditions(method: sil.Method, result: CfgResult[V]): Seq[sil.Exp] = {
      val allowed = method.formalArgs.map(_.name).toSet ++ method.formalReturns.map(_.name).toSet
      val position = lastPosition(result.cfg.exit.get)
      val inferred = result.postStateAt(position).specifications
      val filtered = inferred.filter { constraint =>
        val actual = constraint.ids.toSet.map(_.getName.split("\\.")(0))
        actual subsetOf allowed
      }
      val converted = filtered.map(DefaultSampleConverter.convert)
      method.posts ++ converted
    }

    override def inferInvariants(loop: sil.While, result: CfgResult[V]): Seq[sil.Exp] = {
      val position = getLoopPosition(loop, result.cfg)
      val inferred = result.preStateAt(position).specifications
      val converted = inferred.map(DefaultSampleConverter.convert)
      loop.invs ++ converted.toSeq
    }
  }

  /**
    * An interprocedural inference that infers the values of numerical
    * variables.
    */
  val interprocedural = InterproceduralIntegerOctagonBottomUpInferenceWithJsonExport

  /**
    * Runs the inference.
    *
    * @param args The first arguments is expected to be the file to be analyzed.
    */
  def main(args: Array[String]): Unit = {
    assert(args.nonEmpty, "No file specified.")
    val file = args(0)
    val pretty = args.length > 1 && args(1) == "--pretty"

    // compile program
    val compiler = new SilverCompiler()
    val compilable = Compilable.Path(new File(file).toPath)
    val program = compiler.compile(compilable)

    // flurin's inference
    /*val interproceduralResults = interprocedural.run(program)
    interprocedural.exportProgram(program, interproceduralResults)
    println(interprocedural.specificationsAsJson(file))*/

    // jerome's inference
    val numericalResults = numerical.run(program)
    val numericalProgram = numerical.extendProgram(program, numericalResults)
    val permissionResults = permission.run(numericalProgram)

    if (pretty) {
      // pretty print extend program
      val extended = permission.extendProgram(numericalProgram, permissionResults)
      println(extended)
    } else {
      // export as json
      numerical.exportProgram(program, numericalResults)
      permission.extractSpecifications(numerical)
      permission.exportProgram(program, permissionResults)
      println(permission.specificationsAsJson(file))
    }
  }
}
