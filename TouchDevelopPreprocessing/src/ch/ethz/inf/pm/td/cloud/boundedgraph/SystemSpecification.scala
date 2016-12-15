/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package ch.ethz.inf.pm.td.cloud.boundedgraph

import ch.ethz.inf.pm.td.cloud.boundedgraph.Graph._

case class SystemSpecification (

    operations:List[Operation],

    // Must specify commutativity between all operations.
    // You can leave out pairs where both operations are
    // queries, or the first operation has a larger (alphabetical)
    // ID than the second (due to symmetry)
    commutativitySpecs:Map[(OperationID,OperationID),Expr],

    // Must specify absorption between all updates
    absorptionSpecs:Map[(OperationID,OperationID),Expr]

)

object SystemSpecification {

  lazy val ReadWriteRegister =
    new SystemSpecification(
      operations = List( Query("rx"), Query("ry"), Update("wx"), Update("wy")  ),
      commutativitySpecs = Map(
        ("wx","wx") -> Equal(IntArgLeft(0),IntArgRight(0)),
        ("wx","wy") -> True,
        ("wy","wx") -> Equal(IntArgLeft(0),IntArgRight(0)),
        ("rx","wx") -> False,
        ("ry","wy") -> False,
        ("rx","wy") -> True,
        ("ry","wx") -> True
      ),
      absorptionSpecs = Map(
        ("wx","wx") -> True,
        ("wx", "wy") -> False,
        ("wy", "wx") -> False,
        ("wy","wy") -> True
      )
    )

  lazy val PutGetMap =
    new SystemSpecification(
      operations = List( Query("get"), Update("put") ),
      commutativitySpecs = Map(
        ("put","put") -> Or(Unequal(StringArgLeft(0),StringArgRight(0)),Equal(IntArgLeft(0),IntArgRight(0))),
        ("get","put") -> Unequal(StringArgLeft(0),StringArgRight(0))
      ),
      absorptionSpecs = Map(
        ("put","put") -> Equal(StringArgLeft(0),StringArgRight(0))
      )
    )


}
