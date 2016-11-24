/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import viper.silicon.Silicon
import viper.silver.verifier.Success
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 16/11/16.
  */
object ProgramUtils {

  def isFunctionInjective(function: sil.Function, prog: sil.Program): Boolean = isFunctionInjective(function, prog, function.formalArgs.zipWithIndex.map{ case (_, index) => index }.toSet)

  def isFunctionInjective(function: sil.Function, prog: sil.Program, args: Set[Int]): Boolean = {
    val newProg: sil.Program = sil.Program(prog.domains, prog.fields, prog.functions, prog.predicates, Seq())()
    val silicon = new Silicon(Seq(("startedBy", "viper.silicon.SiliconTests")))
    silicon.parseCommandLine(Seq("dummy.sil"))
    silicon.config.initialize { case _ => silicon.config.initialized = true }
    silicon.start()
    silicon.verify(newProg) match {
      case Success => true
      case _ => false
    }
  }

}
