package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.DefaultSetDomain
import ch.ethz.inf.pm.sample.oorepresentation.Type

/** Default implementation of the precise value-driven heap state. */
case class DefaultPreciseValueDrivenHeapState[S <: SemanticDomain[S]](
      abstractHeap: HeapGraph[SemanticAndGhostCartesianProductDomain[S]],
      generalValState: SemanticAndGhostCartesianProductDomain[S],
      expr: ExpressionSet,
      isTop: Boolean = false,
      override val isBottom: Boolean = false)
  extends PreciseValueDrivenHeapState[S] {

  override def factory(
      abstractHeap: HeapGraph[W],
      generalValState: W,
      expr: ExpressionSet,
      isTop: Boolean,
      isBottom: Boolean) =
    DefaultPreciseValueDrivenHeapState[S](abstractHeap, generalValState, expr, isTop, isBottom)
}

/** Combines each value state with a ghost state to be more precise
  * in the presence of ambiguous out-going edges.
  *
  * Currently, ghost state is only added when creating variables for arguments
  * as well as after materializing.
  */
trait PreciseValueDrivenHeapState[S <: SemanticDomain[S]]
  extends ValueDrivenHeapState[
    SemanticAndGhostCartesianProductDomain[S],
    PreciseValueDrivenHeapState[S]] {

  // Alias for the type of states on edges
  protected type W = SemanticAndGhostCartesianProductDomain[S]

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
  def addGhostState(): PreciseValueDrivenHeapState[S] = {
    require(abstractHeap.isNormalized)

    val groupedEdges = abstractHeap.edges.groupBy(edge => (edge.source, edge.field))
    val newEdges: Set[EdgeWithState[W]] = groupedEdges.map {
      case ((source, field), outgoingEdges) =>
        if (!source.isInstanceOf[SummaryHeapVertex] && outgoingEdges.size > 1) {
          // 'source' is a non-summary node that has more than one out-going edge
          // for the same field

          // When building the list of states on these edges, it's important
          // to consider that some of these states might be identical.
          // Thus, convert the set of out-going edges to a list first.
          val ambiguityStates = outgoingEdges.toList.map(_.state._2)

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
                val newIdState = edge.state._2.add(freshId, DefaultSetDomain[Int](Set(index)))
                edge.copy(state = edge.state.copy(_2 = newIdState))
            }
          } else outgoingEdges
        } else outgoingEdges
    }.flatten.toSet
    val newAbstractHeap = abstractHeap.copy(edges = newEdges)
    copy(abstractHeap = newAbstractHeap)
  }
}

object PreciseValueDrivenHeapState {
  // TODO: Should make this variable thread-local
  private var lastGhostVariableId: Int = -1

  def makeFreshGhostVariableId(): Int = {
    lastGhostVariableId += 1
    lastGhostVariableId
  }
}

case class GhostStateDomain(
    map: Map[Int, DefaultSetDomain[Int]] = Map.empty[Int, DefaultSetDomain[Int]],
    isTop: Boolean = true,
    isBottom: Boolean = false)
  extends FunctionalDomain[Int, DefaultSetDomain[Int], GhostStateDomain] {

  def get(key: Int): DefaultSetDomain[Int] =
    map.getOrElse(key, defaultValue)

  def defaultValue: DefaultSetDomain[Int] = {
    if (isBottom)
      DefaultSetDomain[Int](isBottom = true)
    else
      DefaultSetDomain[Int](isTop = true)
  }

  def functionalFactory(
      map: Map[Int, DefaultSetDomain[Int]],
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
    GhostStateDomain(newMap, newIsTop, newIsBottom)
  }
}

/** Combines a `SemanticDomain` with the `GhostStateDomain`. */
case class SemanticAndGhostCartesianProductDomain[S <: SemanticDomain[S]](
    _1: S,
    _2: GhostStateDomain = GhostStateDomain())
  extends HalfSemanticCartesianProductDomain[
    S,
    GhostStateDomain,
    SemanticAndGhostCartesianProductDomain[S]] {

  def factory(_1: S, _2: GhostStateDomain) =
    SemanticAndGhostCartesianProductDomain(_1, _2)
}