/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.execution

import ch.ethz.inf.pm.sample.abstractdomain.State

/** Performs forward or backward interpretation of `ControlFlowGraph`s.
  *
  * With the associated `CFGStateFactory`, it's possible to use different
  * implementations of `CFGState`, for example to keep more information around
  * than just the most recent pre- and post-states of each statement.
  *
  * @tparam S the type of states
  * @todo maybe enrich the trait with additional methods
  */
trait Interpreter[S <: State[S]] {
  type C <: CFGState[S]

  def cfgStateFactory: CFGStateFactory[S, C]
}
