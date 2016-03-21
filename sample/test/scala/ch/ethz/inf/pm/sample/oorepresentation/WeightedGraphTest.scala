package ch.ethz.inf.pm.sample.oorepresentation

import org.scalatest.{FunSuite, Matchers}

class WeightedGraphTest extends FunSuite with Matchers {

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
