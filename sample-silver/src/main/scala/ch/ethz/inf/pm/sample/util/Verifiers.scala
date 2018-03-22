/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.util

import viper.carbon.CarbonVerifier
import viper.silicon.Silicon
import viper.silver.verifier.{Success, VerificationResult, Verifier}
import viper.silver.{ast => sil}

/**
  * Some utility functions for verifiers.
  *
  * @author Jerome Dohrau
  */
object Verifiers {

  /**
    * Returns an instance of the default verifier.
    *
    * @return A verifier instance.
    */
  def default: Verifier = carbon

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
    * @return A carbon instance.
    */
  def carbon: Verifier = {
    val instance = new CarbonVerifier()
    instance
  }

  /**
    * Tries to verify the given program.
    *
    * @param program The program to verify.
    * @return The output of the verification
    */
  def verify(program: sil.Program): VerificationResult = {
    // get verifier
    val verifier = default
    // verify program
    verifier.start()
    val result = verifier.verify(program)
    verifier.stop()
    // return result
    result
  }

  /**
    * Tries to verify the given program.
    *
    * @param program The program to verify.
    * @return True if and only if the program verifies.
    */
  def doesVerify(program: sil.Program): Boolean =
    verify(program) match {
      case Success => true
      case _ => false
    }
}
