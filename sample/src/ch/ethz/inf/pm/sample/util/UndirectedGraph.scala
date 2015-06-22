package ch.ethz.inf.pm.sample.util

import ch.ethz.inf.pm.sample.SystemParameters

/**
 * A general-purpose undirected graph
 */
class UndirectedGraph[N] {

  case class Node(value: N) {
    var adj: List[Node] = Nil
  }

  case class Edge(n1: Node, n2: Node)

  var nodes: Map[N, Node] = Map.empty
  var edges: List[Edge] = Nil

  /**
   * Add a node to this graph
   */
  def addNode(v: N) = {
    if (SystemParameters.DEBUG) assert(!nodes.contains(v))
    nodes += v -> Node(v)
  }

  /**
   * Add an undirected edge to this graph
   */
  def addEdge(n1: N, n2: N) = {
    val e = Edge(nodes(n1), nodes(n2))
    if (!(edges.contains(e) || edges.contains(Edge(nodes(n2), nodes(n1))))) {
      edges ::= e
      nodes(n1).adj ::= nodes(n2)
      nodes(n2).adj ::= nodes(n1)
    }
  }

  /**
   * Return all nodes reachable from start node
   */
  def reachableNodesFrom(start: Node): Set[N] = {
    def reach(toVisit: List[Node], acc: Set[Node]): Set[Node] = {
      toVisit match {
        case Nil => acc
        case h::tail =>
          val unseen = h.adj.toSet diff acc
          reach(unseen.toList ++ tail, acc + h)
      }
    }

    reach(List(start), Set()).map(_.value)
  }

  /**
   * Get the (strongly connected) components of this graph
   */
  def getComponents: List[Set[N]] = {
    var components: List[Set[N]] = Nil
    var unvisited: Set[N] = Set() ++ nodes.keys

    while (unvisited.nonEmpty) {
      val reached = reachableNodesFrom(nodes(unvisited.head))
      unvisited = unvisited -- reached
      components ::= reached
    }
    components
  }

  override def toString: String = {
    val out = for ((k,v) <- nodes) yield k + ":" + v.adj.mkString(",")
    out.mkString("\n")
  }
}

/**
 * UndirectedGraph companion.
 */
object UndirectedGraph {

  /**
   * Builds up a graph from a set of nodes and an adjacency function adj
   */
  def build[N](nodeValues: List[N], adj: (N,N) => Boolean): UndirectedGraph[N]  = {
    val g = new UndirectedGraph[N]
    for (n <- nodeValues)
      g.addNode(n)

    for (n1 <- nodeValues;
         n2 <- nodeValues
         if adj(n1, n2)) {
      g.addEdge(n1,n2)
    }
    g
  }
}




