package ch.ethz.inf.pm.sample.abstractdomain.vdha

import ch.ethz.inf.pm.sample.abstractdomain._

/**
 * Combines a heap graph with a condition that the heap graph must satisfy.
 *
 * It is currently only used temporarily for expression evaluation. Such an
 * object is basically a `ValueDrivenHeapState` without an expression and
 * without the gigantic set of `State`-specific methods.
 *
 * The condition is (currently) not applied eagerly to the edges.
 */
case class CondHeapGraph[S <: SemanticDomain[S]](
    heap: HeapGraph[S],
    cond: S,
    takenPaths: Set[RootedPath[S]] = Set.empty[RootedPath[S]]) {

  import Utilities._
  import CondHeapGraph._

  require(edgeLocalIds(cond).isEmpty,
    "condition must not contain edge-local identifiers")

  require(accPathIds(cond).map(_.objPath).forall(objPath =>
    takenPaths.exists(_.accPath.startsWith(objPath))),
    "condition must only contain access path identifiers for taken paths")

  def takenPath(path: List[String]): RootedPath[S] =
    takenPaths.find(_.accPath == path).get

  /**
   * Intersects this and another heap graph as well as their associated
   * conditions.
   *
   * This cheap intersection requires that the two heap graphs are sub-graphs
   * of some normalized heap graph.
   *
   * In the resulting heap sub-graph, an edge only occurs if
   * it occurred in both heap sub-graphs (strong equality).
   */
  def intersect(other: CondHeapGraph[S]): CondHeapGraph[S] = {
    require(heap.vertices == other.heap.vertices,
      "the vertices of the two heap graphs do not match")
    // TODO: Check the full precondition (the edges must no have been modified)

    val newEdges = heap.edges intersect other.heap.edges
    val newHeap = heap.copy(edges = newEdges)
    val newCond = glbPreserveIds(cond, other.cond)
    val newPaths = takenPaths union other.takenPaths
    CondHeapGraph(newHeap, newCond, newPaths)
  }

  /** Applies the condition to each edge state. */
  def apply(): CondHeapGraph[S] =
    copy(heap = heap.mapEdgeStates(glbPreserveIds(_, cond)))

  /** Applies a function to both the condition and all edge states. */
  def map(f: S => S): CondHeapGraph[S] =
    copy(cond = f(cond), heap = heap.mapEdgeStates(f))

  /** Applies a function to each edge. */
  def mapEdges(f: Edge[S] => S) =
    copy(heap = heap.copy(edges = heap.edges.map(e => e.copy(state = f(e)))))

  /**
   * Returns whether either the heap or its condition are certainly bottom.
   * @todo this method is not precise, as the condition is not applied eagerly
   * @todo also return true if the taken paths are contradicting
   */
  def isBottom: Boolean =
    heap.isBottom() || cond.lessEqual(cond.bottom())

  /**
   * Prunes the conditional heap-graph, e.g., removes edges with a bottom state
   * as well as unreachable vertices.
   */
  def prune: CondHeapGraph[S] = {
    val (newHeap, idsToRemove) = heap.prune()
    val newCond = cond.removeVariables(idsToRemove)
    copy(heap = newHeap, cond = newCond)
  }

  /**
   * Finds all `AccessPathIdentifier`s contained within the given expression
   * and returns a conditional heap sub-graph for every possible combination
   * of corresponding paths taken through the heap.
   *
   * That is, in each returned conditional heap sub-graph, the taken path
   * is fixed for each `AccessPathIdentifer` in the given expression.
   *
   * @param expr the expression whose `AccessPathIdentifier`s to consider
   * @return the sequence of conditional heap sub-graphs
   */
  def evalExp(expr: Expression): CondHeapGraphSeq[S] = {
    // Translate non-numeric VariableIdentifiers to AccessPathIdentifiers
    val accessPathIds = expr.getIdentifiers.collect {
      case v: VariableIdentifier if !v.getType.isNumericalType =>
        AccessPathIdentifier(v)
      case apId: AccessPathIdentifier => apId
    }

    // If there are no AccessPathIdentifers, just return this
    accessPathIds.foldLeft[CondHeapGraphSeq[S]](this)((condHeaps, apId) => {
      condHeaps.intersect(evalAccessPathId(apId))
    })
  }

  /**
   * Builds a conditional heap sub-graph for every possible path that could be
   * taken in the heap for the given `AccessPathIdentifier`.
   *
   * Edges that certainly don't exist in a heap where a particular path is
   * taken are removed.
   *
   * Note that the path conditions are not applied to the edges of the
   * conditional heaps yet.
   *
   * @param ap the access path identifier to consider heap graph paths for
   * @return a conditional heap sub-graph for every path that could be taken
   */
  def evalAccessPathId(ap: AccessPathIdentifier): CondHeapGraphSeq[S] = {
    // Get path to the non-null receiver of the field access
    var paths = heap.paths(ap.objPath)
    if (ap.getType.isNumericalType) {
      paths = paths.filter(_.target.isInstanceOf[HeapVertex])
    }

    var result = List.empty[CondHeapGraph[S]]
    for (path <- paths) {
      var cond = path.condition

      if (ap.getType.isNumericalType) {
        val field = ap.path.last
        val targetVertex = path.target.asInstanceOf[HeapVertex]

        // Rename edge local identifier that corresponds to the access path
        val renameFrom = edgeLocalIds(cond).filter(_.field == field).toList
        assert(renameFrom.size == 1, "there should be exactly one identifier to rename")
        cond = cond.rename(renameFrom, List(ap))

        // AccessPathIdentifier must agree also with the ValueHeapIdentifier
        val resId = ValueHeapIdentifier(targetVertex, field, ap.getType, ap.pp)
        cond = cond.assume(new BinaryArithmeticExpression(resId, ap, ArithmeticOperator.==, null))
      }

      // Remove all edge local identifiers
      cond = cond.removeVariables(edgeLocalIds(cond))

      // Remove all edges that were NOT taken on this access path
      // Never remove edges going out of a summary node.
      var edgesToRemove = path.edges.map(edge => {
        val outEdges = heap.outEdges(edge.source, edge.field)
        val otherOutEdges = outEdges - edge
        otherOutEdges
      }).flatten.toSet
      edgesToRemove = edgesToRemove.filter(!_.source.isInstanceOf[SummaryHeapVertex])

      cond = Utilities.glbPreserveIds(this.cond, cond)

      val prunedHeap = heap.removeEdges(edgesToRemove)
      result = CondHeapGraph(prunedHeap, cond, Set(path)) :: result
    }
    CondHeapGraphSeq(result)(lattice)
  }

  /** Returns a sequence of heap sub-graphs on which the given expression
    * has been assumed.
    */
  def assume(cond: Expression): CondHeapGraphSeq[S] = {
    val result: CondHeapGraphSeq[S] = cond match {
      case Constant("false", _, _) => CondHeapGraphSeq(Seq())(lattice)
      case Constant("true", _, _) => this
      case VariableIdentifier(_, _, _, _)
           | NegatedBooleanExpression(VariableIdentifier(_, _, _, _))
           | BinaryArithmeticExpression(_, _, _, _) =>
        evalExp(cond).apply().map(_.assume(cond))
      case NegatedBooleanExpression(e) =>
        assume(negateExpression(e))
      case BinaryBooleanExpression(l,r,o,t) => {
        val result: CondHeapGraphSeq[S] = o match {
          case BooleanOperator.&& =>
            assume(l).assume(r)
          case BooleanOperator.|| =>
            // Delay joining, just return all heap graphs
            CondHeapGraphSeq(assume(l).condHeaps ++ assume(r).condHeaps)(lattice)
        }
        result
      }
      case ReferenceComparisonExpression(left, right, op, returnTyp) => {
        import ArithmeticOperator._

        evalExp(left).intersect(evalExp(right)).apply().mapCondHeaps(condHeap => {
          def targetVertex(exp: Expression): Vertex = exp match {
            case (Constant("null", _, _)) => NullVertex
            case AccessPathIdentifier(path) => condHeap.takenPath(path).target
          }

          val leftTarget = targetVertex(left)
          val rightTarget = targetVertex(right)

          op match {
            case `==` =>
              if (leftTarget == rightTarget) Seq(condHeap) else Seq()
            case `!=` =>
              if (leftTarget != rightTarget || leftTarget.isInstanceOf[SummaryHeapVertex]) Seq(condHeap) else Seq()
          }
        })
      }
      case _ =>
        println(s"CondHeapGraph.assume: $cond is not supported.")
        this
    }
    result.prune
  }

  def lattice = cond.bottom()
}

object CondHeapGraph {
  /** Converts a `ValueDrivenHeapState` to a `CondHeapGraph`. */
  def apply[S <: SemanticDomain[S], T <: ValueDrivenHeapState[S, T]](
      state: T): CondHeapGraph[S] =
    CondHeapGraph(state.abstractHeap, state.generalValState)

  /**
   * Implicitly converts a conditional heap graph to a singleton conditional
   * heap graph sequence.
   */
  implicit def CondHeapGraphToCondHeapGraphSeq[S <: SemanticDomain[S]](
      condHeap: CondHeapGraph[S]): CondHeapGraphSeq[S] =
    CondHeapGraphSeq(Seq(condHeap))(condHeap.cond.bottom())

  implicit def CondHeapGraphSeqToCondHeapGraphSeq[S <: SemanticDomain[S]](
      condHeapGraphSeq: CondHeapGraphSeq[S]): Seq[CondHeapGraph[S]] =
    condHeapGraphSeq.condHeaps
}

/**
 * Wraps a sequence of `CondHeapGraph`s and provides convenience methods.
 * It can be empty, representing bottom.
 *
 * @param lattice used to get access to the bottom element of the value lattice
 *
 * @todo prune bottom conditional heap graphs
 * @todo could model as set domain
 */
case class CondHeapGraphSeq[S <: SemanticDomain[S]]
    (condHeaps: Seq[CondHeapGraph[S]])(implicit lattice: S) {

  import Utilities._

  /**
   * Intersects all conditional heap graphs in this and another
   * given sequence pair-wise.
   *
   * This cheap intersection requires that the all heap graphs are sub-graphs
   * of some normalized heap graph.
   */
  def intersect(other: CondHeapGraphSeq[S]): CondHeapGraphSeq[S] =
    condHeaps.map(l => other.condHeaps.map(r => l.intersect(r))).flatten

  /** Applies the condition to each heap graph. */
  def apply(): CondHeapGraphSeq[S] =
    condHeaps.map(_.apply())

  /** Maps each condition and the state of each edge. */
  def map(f: S => S): CondHeapGraphSeq[S] =
    condHeaps.map(condHeap => condHeap.map(f))

  /** Assumes the given expression on each heap graph. */
  def assume(cond: Expression): CondHeapGraphSeq[S] =
    condHeaps.map(condHeap => condHeap.assume(cond).condHeaps).flatten

  /**
   * Maps conditional heap graphs with the given function.
   * The function may return multiple conditional heap graph.
   * They will be joined.
   */
  def mapCondHeaps(f: CondHeapGraph[S] => Seq[CondHeapGraph[S]]): CondHeapGraphSeq[S] =
    condHeaps.map(f).flatten

  /** Prunes all conditional heap graphs and removes the ones that are bottom. */
  def prune: CondHeapGraphSeq[S] =
    condHeaps.map(_.prune).filter(!_.isBottom)

  def isBottom: Boolean =
    condHeaps.isEmpty

  /**
   * Prunes and then joins all conditional heap graphs in this sequence
   * and returns the resulting conditional heap graph.
   *
   * Before joining, it removes the access path identifiers from the heaps.
   */
  def join: CondHeapGraph[S] = {
    val prunedHeaps = prune.condHeaps
    val newVertices = prunedHeaps.map(_.heap.vertices).flatten.toSet
    val newEdges = prunedHeaps.map(_.heap.mapEdgeStates(removeAccessPathIdentifiers)).map(_.edges).flatten.toSet
    val newHeap = HeapGraph(newVertices, newEdges).joinCommonEdges()
    val newCond = removeAccessPathIdentifiers(Lattice.bigLub(prunedHeaps.map(_.cond), lattice))
    CondHeapGraph(newHeap, newCond).prune
  }

  private implicit def CondHeapGraphSeqToCondHeapGraphSeq
  (condHeaps: Seq[CondHeapGraph[S]]): CondHeapGraphSeq[S] =
    CondHeapGraphSeq(condHeaps)
}