/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.cloud

import ch.ethz.inf.pm.td.cloud.eventgraph.SystemSpecification

object TouchDevelopSystemSpecification {

  lazy val spec = SystemSpecification(

    operations = List(


    ),

    // Must specify commutativity between all operations.
    // You can leave out pairs where both operations are
    // queries, or the first operation has a larger (alphabetical)
    // ID than the second (due to symmetry)
    commutativitySpecs = Map(


    ),

    // Must specify absorption between all updates
    absorptionSpecs = Map(


    )

  )

}
