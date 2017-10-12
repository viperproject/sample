/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.util

import viper.carbon.CarbonVerifier
import viper.silicon.Silicon
import viper.silver.reporter.NoopReporter
import viper.silver.verifier.Verifier

/**
  * Some utility functions for verifiers.
  *
  * @author Jerome Dohrau
  */
object Verifiers {
  /**
    * Returns an instance of the silicon verifier.
    *
    * @return A silicon instance.
    */
  def silicon: Verifier = {
    val instance = new Silicon()
    instance.parseCommandLine(Seq("--ignoreFile", "dummy.sil"))
    instance
  }

  /**
    * Returns an instance of the carbon verifier.
    *
    * @return
    */
  def carbon: Verifier = {
    val instance = new CarbonVerifier()
    instance
  }
}
