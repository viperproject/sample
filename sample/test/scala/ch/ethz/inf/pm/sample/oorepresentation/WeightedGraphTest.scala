/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.oorepresentation

import ch.ethz.inf.pm.sample.test.SampleTest
import org.scalatest.{FunSuite, Matchers}

class WeightedGraphTest extends FunSuite with Matchers with SampleTest {

  type NodeValType = String
  class TestGraph extends WeightedGraph[NodeValType, Nothing]

  test("Getting predecessors of node") {
    val graph = new TestGraph
    val nodeIds = for (i <- 0 to 5 ) yield graph.addNode("foo")
    val edges = List(0->2, 1->2, 2->3, 2->4)
    for ((from, to) <- edges) {
      graph.addEdge(nodeIds(from), nodeIds(to), None)
    }

    val predecessors = graph.getDirectPredecessors(nodeIds(2)) map (i => nodeIds.indexOf(i))
    predecessors should equal (Set(0,1))
  }

  test("Getting successors of node") {
    val graph = new TestGraph
    val nodeIds = for (i <- 0 to 5 ) yield graph.addNode("foo")
    val edges = List(0->2, 1->2, 2->3, 2->4)
    for ((from, to) <- edges) {
      graph.addEdge(nodeIds(from), nodeIds(to), None)
    }

    val successors = graph.getDirectSuccessors(nodeIds(2)) map (i => nodeIds.indexOf(i))
    successors should equal (Set(3,4))
  }


}
