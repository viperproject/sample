/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.execution.SilverAnalysis
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverInferenceRunner
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState.SimpleAliasAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisState.SimplePermissionAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.util.PermissionTree
import viper.silver.ast.{Exp, Field}
import viper.silver.{ast => sil}


trait PermissionInferenceRunner[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[A, T]]
  extends SilverInferenceRunner[PermissionTree, T] {
  /**
    * Modifies the list of preconditions using the specifications provided by
    * the given state.
    *
    * @param existing The list of existing preconditions.
    * @param state    The state providing the specifications.
    * @return The modified list of preconditions.
    */
  override def preconditions(existing: Seq[Exp], state: T): Seq[Exp] = ???

  /**
    * Modifies the list of postconditions using the specifications provided by
    * the given state.
    *
    * @param existing The list of existing postconditions.
    * @param state    The state providing the specifications.
    * @return The modified list of postconditions.
    */
  override def postconditions(existing: Seq[Exp], state: T): Seq[Exp] = ???

  /**
    * Modifies the list of postconditions using the specifications provided by
    * the given state.
    *
    * @param existing The list of existing invariants.
    * @param state    The state providing the specifications.
    * @return The modified list of postconditions.
    */
  override def invariants(existing: Seq[Exp], state: T): Seq[Exp] = ???

  /**
    * Modifies the list of fields of a new statement using specifications
    * provided by the given field.
    *
    * @param existing The existing list of fields.
    * @param state    The state providing the specifications.
    * @return The modified list of fields.
    */
  override def fields(existing: Seq[Field], state: T): Seq[Field] = ???
}

object PermissionInference
  extends PermissionInferenceRunner[SimpleAliasAnalysisState, SimplePermissionAnalysisState] {
  override val analysis: SilverAnalysis[SimplePermissionAnalysisState] = PermissionAnalysis(AliasAnalysisEntryState, PermissionAnalysisEntryState)
}
