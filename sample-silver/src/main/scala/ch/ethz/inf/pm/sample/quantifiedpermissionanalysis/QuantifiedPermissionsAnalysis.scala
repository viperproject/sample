/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, Identifier}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.inference.SilverExtender
import ch.ethz.inf.pm.sample.oorepresentation.DummyProgramPoint
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, DefaultSilverConverter, SilverMethodDeclaration, SilverProgramDeclaration}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionsParameters._
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

/**
  * @author Jerome Dohrau
  * @author Severin Münger
  */
case class QuantifiedPermissionsAnalysis()
  extends SilverAnalysis[QuantifiedPermissionState] with LazyLogging {

  def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[QuantifiedPermissionState] = {

    Context.clearMethodSpecificInfo()
    Context.setMethodContext(DefaultSilverConverter.prog, method)

    val numericalEntry = numericalEntryStateBuilder.build(program, method)
    val numericalInterpreter = FinalResultForwardInterpreter[NumericalStateType](method.body, numericalEntry)
    val numericalResult = numericalInterpreter.execute()

    Context.setNumericalResult(method.name.name, numericalResult)

    val entry = QuantifiedPermissionEntryState.build(program, method)
    val interpreter = new QuantifiedPermissionInterpreter(method.body, entry)
    val result = interpreter.execute()

    result
  }
}

/**
  * An analysis runner for the quantified permission inference.
  *
  * @author Jerome Dohrau
  * @author Severin Münger
  */
object QuantifiedPermissionsAnalysisRunner
  extends SilverExtender[QuantifiedPermissionState] {

  SystemParameters.isValueDrivenHeapAnalysis = false

  override val analysis: QuantifiedPermissionsAnalysis = QuantifiedPermissionsAnalysis()

  override def extendProgram(program: sil.Program, results: ProgramResult[QuantifiedPermissionState]): sil.Program = {
    val extended = super.extendProgram(program, results)
    extended.copy(functions = extended.functions ++ Context.getAuxiliaryFunctions.values)(pos = extended.pos, info = extended.info, errT = extended.errT)
  }

  override def extendMethod(method: sil.Method, cfgResult: CfgResult[QuantifiedPermissionState]): sil.Method = {
    Context.prepareMethodForExtension(method.name)
    super.extendMethod(method, cfgResult)
  }

  override def inferArguments(method: sil.Method, result: CfgResult[QuantifiedPermissionState]): Seq[sil.LocalVarDecl] = {
    // TODO: Add read parameter.
    // TODO: Add ghost sets parameters.
    super.inferArguments(method, result)
  }

  override def inferPreconditions(method: sil.Method, result: CfgResult[QuantifiedPermissionState]): Seq[sil.Exp] = {
    val position = firstPosition(result.cfg.entry)
    val state = result.preStateAt(position)
    val inferred = SpecificationGenerator.generate(state.records.head)
    method.pres ++ inferred
  }

  override def inferInvariants(loop: sil.While, result: CfgResult[QuantifiedPermissionState]): Seq[sil.Exp] = {
    val position = getLoopPosition(loop, result.cfg)
    val state = result.preStateAt(position)
    val inferred = SpecificationGenerator.generate(state.records.head)

    val numerical = Context.getNumericalResult()
    val domain = numerical.preStateAt(position).domain
    val constraints = domain.getConstraints(domain.ids.toSet)
    val converted = constraints.map(DefaultSampleConverter.convert)

    loop.invs ++ converted ++ inferred
  }
}

object QuantifiedPermissionEntryState
  extends SilverEntryStateBuilder[QuantifiedPermissionState] {

  override def default: QuantifiedPermissionState = QuantifiedPermissionState(
    pp = DummyProgramPoint,
    expr = ExpressionSet(),
    records = PermissionRecords() :: Nil,
    isTop = false,
    isBottom = false
  )

  override def build(program: SilverProgramDeclaration, method: SilverMethodDeclaration): QuantifiedPermissionState = {
    val fields = program.fields.map(_.variable.id: Identifier)
    val map = fields.map(_ -> (PermissionTree.Initial: PermissionTree)).toMap
    val records = PermissionRecords(map = map) :: Nil
    super.build(program, method).copy(records = records)
  }
}