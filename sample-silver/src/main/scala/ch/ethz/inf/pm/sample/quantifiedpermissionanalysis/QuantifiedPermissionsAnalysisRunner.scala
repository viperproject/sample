/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.inference.SilverExtender
import viper.silver.ast._
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 28.08.17.
  */
/** SIL analysis runner that uses the default QuantifiedPermissions analysis. */
object QuantifiedPermissionsAnalysisRunner extends SilverExtender[QuantifiedPermissionsState] {
  SystemParameters.isValueDrivenHeapAnalysis = false

  override def extendProgram(program: sil.Program, results: ProgramResult[QuantifiedPermissionsState]): sil.Program = {
    val tempProg = super.extendProgram(program, results)
    tempProg.copy(functions = tempProg.functions ++ Context.getAuxiliaryFunctions.values)(pos = tempProg.pos, info = tempProg.info)
  }

  override def extendMethod(method: sil.Method, cfgResult: CfgResult[QuantifiedPermissionsState]): sil.Method = {
    Context.prepareMethodForExtension(method.name)
    super.extendMethod(method, cfgResult)
  }

  override def inferArguments(method: Method, result: CfgResult[QuantifiedPermissionsState]): Seq[LocalVarDecl] = super.inferArguments(method, result)

  override def inferFields(newStmt: NewStmt, position: BlockPosition, result: CfgResult[QuantifiedPermissionsState]): Seq[Field] = super.inferFields(newStmt, position, result)

  override def inferPreconditions(method: Method, result: CfgResult[QuantifiedPermissionsState]): Seq[Exp] = super.inferPreconditions(method, result)

  override def inferPostconditions(method: Method, result: CfgResult[QuantifiedPermissionsState]): Seq[Exp] = super.inferPostconditions(method, result)

  override def inferInvariants(loop: While, result: CfgResult[QuantifiedPermissionsState]): Seq[Exp] = super.inferInvariants(loop, result)

  /**
    * The analysis to run.
    */

  override val analysis = _
}
