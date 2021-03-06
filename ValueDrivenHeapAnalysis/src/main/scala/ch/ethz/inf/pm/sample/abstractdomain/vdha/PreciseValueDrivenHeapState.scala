/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.abstractdomain.vdha.PreciseValueDrivenHeapState.EdgeStateDomain

/** Combines each value state with a ghost state to be more precise
  * in the presence of ambiguous out-going edges.
  *
  * Currently, ghost state is only added when creating variables for arguments
  * as well as after materializing.
  */
trait PreciseValueDrivenHeapState[
    S <: SemanticDomain[S],
    T <: PreciseValueDrivenHeapState[S, T]]
  extends ValueDrivenHeapState[EdgeStateDomain[S], T] { this: T =>

  override def createVariableForArgument(variable: VariableIdentifier, typ: Type) =
    super.createVariableForArgument(variable, typ).addGhostState()

  override protected def materializePath(pathToMaterialize: List[String]) =
    super.materializePath(pathToMaterialize).addGhostState()

  /** Finds non-summary nodes in the heap graph that have more than one
    * out-going edges for the same field.
    *
    * If the ghost state on these out-going edges is not already disjoint,
    * this method create additional ghost state to disambiguate these edges.
    *
    * @return the value-driven heap state, possibly with more ghost state
    */
  def addGhostState(): T = {
    require(abstractHeap.isNormalized)

    val groupedEdges = abstractHeap.edges.groupBy(edge => (edge.source, edge.field))
    val newEdges: Set[Edge[EdgeStateDomain[S]]] = groupedEdges.map {
      case ((source, field), outgoingEdges) =>
        if (!source.isInstanceOf[SummaryHeapVertex] && outgoingEdges.size > 1) {
          // 'source' is a non-summary node that has more than one out-going edge
          // for the same field

          // When building the list of states on these edges, it's important
          // to consider that some of these states might be identical.
          // Thus, convert the set of out-going edges to a list first.
          val ambiguityStates = outgoingEdges.toList.map(_.state.edgeAmbiguityState)

          // We only need to add ghost state if for some pair of edges,
          // the ghost states intersect
          val hasAmbiguity = ambiguityStates.tails.exists {
            case head :: tail => tail.exists(!head.glb(_).isBottom)
            case Nil => false
          }
          if (hasAmbiguity) {
            // Create a new ghost variable with a distinct value
            // for each ambiguous edge
            val freshId = PreciseValueDrivenHeapState.makeFreshGhostVariableId()
            outgoingEdges.zipWithIndex.map {
              case (edge, index) =>
                val newIdState = edge.state.edgeAmbiguityState.add(freshId, SetDomain.Default.Inner[Int](Set(index)))
                edge.copy(state = edge.state.copy(edgeAmbiguityState = newIdState))
            }
          } else outgoingEdges
        } else outgoingEdges
    }.flatten.toSet
    val newAbstractHeap = abstractHeap.copy(edges = newEdges)
    copy(abstractHeap = newAbstractHeap)
  }
}

object PreciseValueDrivenHeapState {
  /** Type of edge states in the precise value-driven heap analysis. */
  final case class EdgeStateDomain[S <: SemanticDomain[S]](
      valueState: S,
      edgeAmbiguityState: EdgeAmbiguityState)
    extends SemanticCartesianProductDomain[S, EdgeAmbiguityState, EdgeStateDomain[S]] {
    def factory(valueState: S, edgeAmbiguityState: EdgeAmbiguityState): EdgeStateDomain[S] =
      EdgeStateDomain(valueState, edgeAmbiguityState)

    override def _1: S = valueState

    override def _2: EdgeAmbiguityState = edgeAmbiguityState
  }

  def makeTopEdgeState[S <: SemanticDomain[S]](state: S): EdgeStateDomain[S] =
    EdgeStateDomain(state, EdgeAmbiguityState())

  /** Default implementation of the precise value-driven heap state. */
  case class Default[S <: SemanticDomain[S]](
      abstractHeap: HeapGraph[EdgeStateDomain[S]],
      generalValState: EdgeStateDomain[S],
      expr: ExpressionSet,
      isTop: Boolean = false)
    extends PreciseValueDrivenHeapState[S, Default[S]] {

    override def factory(
        abstractHeap: HeapGraph[EdgeStateDomain[S]],
        generalValState: EdgeStateDomain[S],
        expr: ExpressionSet,
        isTop: Boolean) =
      Default[S](abstractHeap, generalValState, expr, isTop)
  }

  private val nextGhostVariableId = new ThreadLocal[Int]

  def makeFreshGhostVariableId(): Int = {
    val id = nextGhostVariableId.get
    nextGhostVariableId.set(id + 1)
    id
  }
}

case class EdgeAmbiguityState(
    map: Map[Int, SetDomain.Default[Int]] = Map.empty[Int, SetDomain.Default[Int]],
    isTop: Boolean = true,
    override val isBottom: Boolean = false)
  extends FunctionalDomain[Int, SetDomain.Default[Int], EdgeAmbiguityState]
  with DummySemanticDomain[EdgeAmbiguityState] {

  def get(key: Int): SetDomain.Default[Int] =
    map.getOrElse(key, defaultValue)

  def defaultValue: SetDomain.Default[Int] = {
    if (isBottom)
      SetDomain.Default.Bottom[Int]
    else
      SetDomain.Default.Top[Int]
  }

  def functionalFactory(
      map: Map[Int, SetDomain.Default[Int]],
      isBottom: Boolean,
      isTop: Boolean) = {
    var newIsBottom = isBottom
    var newIsTop = isTop && map.isEmpty
    // Treat the ghost state as bottom when at least one ghost variable
    // is bottom
    if (map.values.exists(_.isBottom)) {
      newIsBottom = true
      newIsTop = false
    }
    val newMap = map.filterNot(_._2.isTop)
    EdgeAmbiguityState(newMap, newIsTop, newIsBottom)
  }
}