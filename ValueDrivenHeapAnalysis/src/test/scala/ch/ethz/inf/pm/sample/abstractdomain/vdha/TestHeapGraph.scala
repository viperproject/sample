/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron

import scala.collection.immutable.TreeSet

object TestHeapGraph {
  def main(args: Array[String]) {
    type S = Apron.Polyhedra

    val head = LocalVariableVertex("head")(null)
    val p = LocalVariableVertex("p")(null)
    val n1 = DefiniteHeapVertex(1)(null)
    val n2 = DefiniteHeapVertex(2)(null)
    val n3 = DefiniteHeapVertex(3)(null)
    val nSum1 = SummaryHeapVertex(4)(null)
    val nSum2 = SummaryHeapVertex(5)(null)
    val vertices1 = TreeSet.empty[Vertex] + head + p + NullVertex + n1 + n2
    val vertices2 = TreeSet.empty[Vertex] + head + p + NullVertex + n1 + n2 + n3
    val edgeHeadN1 = Edge[S](head, null, None, n1)
    val edgeHeadN2 = Edge[S](head, null, None, n2)
    val edgePN1 = Edge[S](p, null, None, n1)
    val edgePN3 = Edge[S](p, null, None, n3)
    val edgeN2N1 = Edge[S](n2, null, Some("next"), n1)
    val edgeN1N3 = Edge[S](n1, null, Some("next"), n3)
    val edgeN1NULL = Edge[S](n1, null, Some("next"), NullVertex)
    val edgeN3NULL = Edge[S](n3, null, Some("next"), NullVertex)
    val edges1 = Set.empty[Edge[S]] + edgeHeadN2 + edgeHeadN1 + edgeN1NULL + edgePN1 + edgeN2N1
    val edges2 = Set.empty[Edge[S]] + edgeHeadN2 + edgeHeadN1 + edgeN3NULL + edgePN3 + edgeN2N1 + edgeN1N3
    val graph1 = HeapGraph[S](vertices1, edges1)
    val graph2 = HeapGraph[S](vertices2, edges2)

    println("OUTCOME OF mcs(graph1, graph2): ")
    val mcs = graph1.mcs(graph2)
    println(mcs)
    println("Size of i is " + mcs.size)
  }

}
