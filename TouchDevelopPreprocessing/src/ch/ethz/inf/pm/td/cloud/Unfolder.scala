/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.sample.abstractdomain.State
import ch.ethz.inf.pm.td.cloud.AbstractEventGraph.GeneralEventGraph

object Unfolder {

  def unfold[S <: State[S]](f: GeneralEventGraph[S]): BoundedEventGraph.Graph = ???

}

