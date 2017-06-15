/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverIdentifier, SilverProgramDeclaration}

/**
  * A ProgramResult consists of CfgResults for each available identifier. Identifiers are usually
  * methods but any valid SilverIdentifier is supported.
  *
  * @tparam S the state to be used in CfgResult
  * @author Flurin Rindisbacher
  */
trait ProgramResult[S <: State[S]] {

  /**
    * Get a single CfgResult for one identifier (usually a method)
    *
    * @param ident the identifier (e.g method.name).
    * @return a CfgResult.
    */
  def getResult(ident: SilverIdentifier): CfgResult[S]

  /**
    * Set a single CfgResult for an identifier (e.g. a method).
    *
    * @param ident     the identifier (e.g method.name).
    * @param cfgResult the CfgResult.
    */
  def setResult(ident: SilverIdentifier, cfgResult: CfgResult[S]): Unit

  /**
    * Available identifiers for which results have been set or initialized.
    *
    * @return List of available identifiers.
    */
  def identifiers: Iterable[SilverIdentifier]

  /**
    * Initializes the cfg results for all methods using the given helper function
    *
    * @param cfgResultInitializer helper called for each result to be initialized
    * @param state      State to use for the initialization
    */
  def initialize(cfgResultInitializer: (SampleCfg, S) => CfgResult[S], state: S): Unit

  /**
    * Initializes the cfg results for all methods using the given helper function
    *
    * @param cfgResultInitializer helper called for each result to be initialized
    */
  def initialize(cfgResultInitializer: SampleCfg => CfgResult[S]): Unit
}

class DefaultProgramResult[S <: State[S]](program: SilverProgramDeclaration)
  extends ProgramResult[S] {

  var results: Map[SilverIdentifier, CfgResult[S]] = Map.empty

  override def initialize(i: (SampleCfg, S) => CfgResult[S], state: S) {
    for (method <- program.methods) {
      setResult(method.name, i(method.body, state))
    }
  }

  override def initialize(i: (SampleCfg) => CfgResult[S]): Unit = {
    for (method <- program.methods) {
      setResult(method.name, i(method.body))
    }
  }

  /**
    * Get a single CfgResult for one identifier (usually a method)
    *
    * @param ident the identifier (e.g method.name).
    * @return a REFERENCE to the CfgResult stored for this identifier.
    */
  override def getResult(ident: SilverIdentifier): CfgResult[S] = results(ident)


  /**
    * Set a single CfgResult for an identifier (e.g. a method). Note that getResult() returns a reference and you
    * may skip calling setResult() again if you only change some states.
    *
    * @param ident     the identifier (e.g method.name).
    * @param cfgResult the CfgResult.
    */
  override def setResult(ident: SilverIdentifier, cfgResult: CfgResult[S]): Unit = results += ident -> cfgResult

  override def identifiers: Iterable[SilverIdentifier] = results.keys
}

object DefaultProgramResult {
  def apply[S <: State[S]](program: SilverProgramDeclaration): DefaultProgramResult[S] =
    new DefaultProgramResult[S](program: SilverProgramDeclaration)
}