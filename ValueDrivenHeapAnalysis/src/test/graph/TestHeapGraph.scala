package test.graph

import graph._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{ApronInterface, ConstrainedPolyhedra}
import scala.collection.immutable.TreeSet
import ch.ethz.inf.pm.sample.util.Timer

object TestHeapGraph {
  def main(args: Array[String]) {
    val head = new LocalVariableVertex("head", null)
    val p = new LocalVariableVertex("p", null)
    val NULL = new NullVertex()
    val n1 = new DefiniteHeapVertex(1, null)
    val n2 = new DefiniteHeapVertex(2, null)
    val n3 = new DefiniteHeapVertex(3, null)
    val nSum1 = new SummaryHeapVertex(4, null)
    val nSum2 = new SummaryHeapVertex(5, null)
    val vertices1 = TreeSet.empty[Vertex] + head + p + NULL + n1 + n2
    val vertices2 = TreeSet.empty[Vertex] + head + p + NULL + n1 + n2 + n3
    val edgeHeadN1 = new EdgeWithState[ApronInterface](head, null, None, n1)
    val edgeHeadN2 = new EdgeWithState[ApronInterface](head, null, None, n2)
    val edgePN1 = new EdgeWithState[ApronInterface](p, null, None, n1)
    val edgePN3 = new EdgeWithState[ApronInterface](p, null, None, n3)
    val edgeN2N1 = new EdgeWithState[ApronInterface](n2, null, Some("next"), n1)
    val edgeN1N3 = new EdgeWithState[ApronInterface](n1, null, Some("next"), n3)
    val edgeN1NULL = new EdgeWithState[ApronInterface](n1, null, Some("next"), NULL)
    val edgeN3NULL = new EdgeWithState[ApronInterface](n3, null, Some("next"), NULL)
    val edges1 = Set.empty[EdgeWithState[ApronInterface]] + edgeHeadN2 + edgeHeadN1 + edgeN1NULL + edgePN1 + edgeN2N1
    val edges2 = Set.empty[EdgeWithState[ApronInterface]] + edgeHeadN2 + edgeHeadN1 + edgeN3NULL + edgePN3 + edgeN2N1 + edgeN1N3
    val graph1 = new HeapGraph[ApronInterface](vertices1, edges1)
    val graph2 = new HeapGraph[ApronInterface](vertices2, edges2)
    val initMaxEdges = graph1.initialMaxEdges(graph2)

    println("OUTCOME OF mcs(graph1, graph2): ")
    val (i, edgeMap) = graph1.mcs(graph2)
    val mcsGraph = graph1.glb(graph2)
    for (entry <- i)
      println(entry)
    println("Size of i is " + i.size)

  }

}
