package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.ApronInterface
import scala.collection.immutable.TreeSet

object TestHeapGraph {
  def main(args: Array[String]) {
    type S = ApronInterface

    val head = LocalVariableVertex("head")(null)
    val p = LocalVariableVertex("p")(null)
    val n1 = DefiniteHeapVertex(1)(null)
    val n2 = DefiniteHeapVertex(2)(null)
    val n3 = DefiniteHeapVertex(3)(null)
    val nSum1 = SummaryHeapVertex(4)(null)
    val nSum2 = SummaryHeapVertex(5)(null)
    val vertices1 = TreeSet.empty[Vertex] + head + p + NullVertex + n1 + n2
    val vertices2 = TreeSet.empty[Vertex] + head + p + NullVertex + n1 + n2 + n3
    val edgeHeadN1 = EdgeWithState[S](head, null, None, n1)
    val edgeHeadN2 = EdgeWithState[S](head, null, None, n2)
    val edgePN1 = EdgeWithState[S](p, null, None, n1)
    val edgePN3 = EdgeWithState[S](p, null, None, n3)
    val edgeN2N1 = EdgeWithState[S](n2, null, Some("next"), n1)
    val edgeN1N3 = EdgeWithState[S](n1, null, Some("next"), n3)
    val edgeN1NULL = EdgeWithState[S](n1, null, Some("next"), NullVertex)
    val edgeN3NULL = EdgeWithState[S](n3, null, Some("next"), NullVertex)
    val edges1 = Set.empty[EdgeWithState[S]] + edgeHeadN2 + edgeHeadN1 + edgeN1NULL + edgePN1 + edgeN2N1
    val edges2 = Set.empty[EdgeWithState[S]] + edgeHeadN2 + edgeHeadN1 + edgeN3NULL + edgePN3 + edgeN2N1 + edgeN1N3
    val graph1 = HeapGraph[S](vertices1, edges1)
    val graph2 = HeapGraph[S](vertices2, edges2)
    val initMaxEdges = graph1.initialMaxEdges(graph2)

    println("OUTCOME OF mcs(graph1, graph2): ")
    val (i, edgeMap) = graph1.mcs(graph2)
    val mcsGraph = graph1.glb(graph2)
    for (entry <- i)
      println(entry)
    println("Size of i is " + i.size)

  }

}
