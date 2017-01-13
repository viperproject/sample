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
import viper.silver.{ast => sil}


trait PermissionInferenceRunner[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[A, T]]
  extends SilverInferenceRunner[PermissionTree, T] {
  /**
    * Modifies the list of preconditions using the given specifications.
    *
    * @param existing       The list of existing preconditions.
    * @param specifications The specifications.
    * @return The modified list of preconditions.
    */
  override def preconditions(existing: Seq[sil.Exp], specifications: PermissionTree): Seq[sil.Exp] = ???

  /**
    * Modifies the list of postcondition using the given specifications.
    *
    * @param existing       The list of existing postconditions.
    * @param specifications The specifications.
    * @return The modified list of postconditions.
    */
  override def postconditions(existing: Seq[sil.Exp], specifications: PermissionTree): Seq[sil.Exp] = ???

  /**
    * Modifies the list of invariants using the given specifications.
    *
    * @param existing       The list of existing invariants.
    * @param specifications The specifications.
    * @return The modified list of postconditions.
    */
  override def invariants(existing: Seq[sil.Exp], specifications: PermissionTree): Seq[sil.Exp] = ???

  /**
    * Modifies the list of fields of a new statement using the given
    * specification.
    *
    * @param existing      The existing list of fields.
    * @param specification The specification.
    * @return The modified list of fields.
    */
  override def fields(existing: Seq[sil.Field], specification: PermissionTree): Seq[sil.Field] = ???
}

object PermissionInference
  extends PermissionInferenceRunner[SimpleAliasAnalysisState, SimplePermissionAnalysisState] {
  override val analysis: SilverAnalysis[SimplePermissionAnalysisState] = PermissionAnalysis(AliasAnalysisEntryState, PermissionAnalysisEntryState)
}
