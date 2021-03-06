/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.oorepresentation

import ch.ethz.inf.pm.sample.{SystemParameters, _}
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.CFGState


object WeightedGraph {

  class Default[Node,Weight] extends WeightedGraph[Node,Weight] {

  }

}

/**
 * This class represents an oriented weighted graph.
 *
 * @author Pietro Ferrara, Lucas Brutschy
 */
trait WeightedGraph[Node, Weight] {

  var nodes: List[Node] = Nil
  var edges: Set[(Int, Int, Option[Weight])] = Set.empty

  def removeNode(node: Node) {
    val index: Int = this.addNodeIfNotExisting(node)
    var newEdges: Set[(Int, Int, Option[Weight])] = Set.empty
    for ((i1, i2, w) <- edges) {
      val index1 = if (i1 > index) i1 - 1 else i1
      val index2 = if (i2 > index) i2 - 1 else i2
      if (index != i1 && index != i2)
        newEdges = newEdges + ((index1, index2, w))
    }
    edges = newEdges
    nodes = remove(nodes, index)
  }

  /**
   * Add a node to the current graph iff it is not yet in the graph. Otherwise, it returns the index
   * of the existing node.
   *
   * @param node the node to be added if it does not exist
   * @return the index of the(evantually inserted) node
   */
  def addNodeIfNotExisting(node: Node): Int = {
    for (i <- nodes.indices)
      if (nodes.apply(i).equals(node)) return i
    nodes = nodes ::: node :: Nil
    nodes.lastIndexOf(node)
  }

  /**
   * Replace a node with the given one
   *
   * @param index the index of the node to be replaced
   * @param  node the node to be inserted
   */
  def setNode(index: Int, node: Node) {
    nodes = nodes.updated(index, node)
  }

  /**
   * Return the node ids without sorting edges
   *
   * @return the leaves of the graph
   */
  def getLeaves: Set[Node] = {
    getLeavesIds map { id => nodes(id) }
  }

  def getLeavesIds: Set[Int] = {
    var notLeafs: Set[Int] = Set.empty
    for ((i1, i2, w) <- edges) //{
      notLeafs = notLeafs + i1
    var result: Set[Int] = Set.empty
    for (i <- nodes.indices)
      if (!notLeafs.contains(i))
        result = result + i
    result
  }

  def initialBlockInLoop(index: Int): Boolean = {
    if (entryEdges(index).size <= 1) return false
    var prevEdges: Set[(Int, Int, Option[Weight])] = this.exitEdges(index)
    var nextEdges: Set[(Int, Int, Option[Weight])] = prevEdges

    while (nextEdges.nonEmpty) {
      prevEdges = nextEdges
      nextEdges = Set.empty
      for ((i1, i2, w) <- prevEdges) {
        if (i2 > i1)
          nextEdges = nextEdges ++ this.exitEdges(i2)
        if (i2 == index)
          return true
      }
    }
    false
  }

  override def toString: String = {
    var result: String = ""
    var i: Int = 0
    while (i < nodes.size) {
      val node: Node = nodes.apply(i)
      result = result +
        "Node n." + i + "\n-----------------\n" +
        "Pred: " + entryNodesToString(i) +
        "\nSucc: " + exitNodesToString(i) +
        "\nBlock:\n" + nodeToString(node) + "\n-----------------\n"
      i = i + 1
    }
    result
  }

  protected def nodeToString(node: Node) = node.toString

  def entryNodesToString(i: Int): String = {
    val preds = for ((from, _, weight) <- entryEdges(i))
      yield from + weightToString(weight)

    preds mkString ", "
  }

  def exitNodesToString(i: Int): String = {
    val succ = for ((_, to, weight) <- exitEdges(i))
      yield to + weightToString(weight)

    succ mkString ", "
  }

  def exitEdges(nodeIndex: Int): Set[(Int, Int, Option[Weight])] = {
    for (e@(from, to, _) <- edges if from == nodeIndex) yield e
  }

  private def weightToString(weight: Option[Weight]): String = weight match {
    case None => ""
    case null => ""
    case Some(x) => "(" + x.toString + ")"
  }

  /**
   * It adds a node containing the given statements to the control flow graph.
   * It adds an edge from each entry point to the fresh node (related with the given condition).
   * It returns the index of the fresh node. 
   */
  def addSummaryNode(statements: Node, entryPoints: List[(Int, Option[Weight])]): Int = {
    val index: Int = this.addNode(statements)
    for (entryIndex: (Int, Option[Weight]) <- entryPoints)
      this.addEdge(entryIndex._1, index, entryIndex._2)
    index
  }

  /**
    * Add a node to the current graph
    *
    * @param node the node to be added
    * @return the index of the inserted node
    */
  def addNode(node: Node): Int = {
    nodes = nodes ::: node :: Nil;
    nodes.lastIndexOf(node)
  }

  /**
    * Add an edge to the current graph
    *
    * @param from   the node from which the edge starts
    * @param to     the node to which point the edge
    * @param weight the weight of the edge or None if this edge is not linked to a weight
    */
  def addEdge(from: Int, to: Int, weight: Option[Weight]): Unit = edges = edges.+((from, to, weight))

  def getDirectSuccessors(nodeIndex: Int): Set[Int] = {
    for ((from, to, _) <- exitEdges(nodeIndex)) yield to
  }

  def getDirectPredecessors(nodeIndex: Int): Set[Int] = {
    for ((from, to, _) <- entryEdges(nodeIndex)) yield from
  }

  def entryEdges(nodeIndex: Int): Set[(Int, Int, Option[Weight])] = {
    for (e@(from, to, _) <- edges if to == nodeIndex) yield e
  }

  private def remove[T1](list: List[T1], index: Int): List[T1] = list match {
    case x :: x1 if index != 0 => x :: remove(x1, index - 1)
    case x :: x1 if index == 0 => remove(x1, index - 1)
    case Nil => Nil
  }
}


case class LabeledGraph[Node, Weight]() extends WeightedGraph[Node, Weight] {

  private var nodeClasses = Map.empty[Node, String]
  private var nodeLabels = Map.empty[Node, String]
  private var edgeLabels = Map.empty[Weight, String]
  private var partitioning = Map.empty[Node, Node]

  def setNodeLabel(a: Node, b: String): Unit = {
    nodeLabels = nodeLabels + (a -> b)
  }

  def getNodeLabel(a: Node): String = {
    nodeLabels.getOrElse(a, "")
  }

  def setEdgeLabel(a: Weight, b: String): Unit = {
    edgeLabels = edgeLabels + (a -> b)
  }

  def getEdgeLabel(a: Weight): String = {
    edgeLabels.getOrElse(a, "")
  }

  def setNodeClass(a: Node, b: String): Unit = {
    nodeClasses = nodeClasses + (a -> b)
  }

  def getNodeClass(a: Node): String = {
    nodeClasses.getOrElse(a, "")
  }

  def addPartitioning(value: Node, target: Node): Unit =
    partitioning = partitioning + (value -> target)

  def getPartition(value: Node): Option[Node] =
    partitioning.get(value)

}

/**
 * Denotes position of a (top-level) statement in a `ControlFlowGraph`
 *
 * @param blockIdx index of block in which the statement occurs
 * @param stmtIdx the index within the block
 */
case class CfgLocation(blockIdx: Int, stmtIdx: Int)

/**
 * This class represents a control flow graph, i.e. a weighted graph where
 * - nodes are list of statements (a sequential piece of code)
 * - the weight of edges are (possible) boolean values, meaning in which way
 * (<code>true</code> or <code>false</code>) we have to evaluate the condition
 * or if we have always to follow such edge
 *
 * Thus from each node we can have one exiting edge with no weight, or two edgeds
 * (one <code>true</code> and the other <code>false</code>)
 *
 * This class extends <code>Statement</code> in order to be as much generic as
 * possible. For instance, consider a statement like <code>variable=if(B) then 1 else 2</code>.
 * Such assignments are allowed by some programming languages (e.g. <code>Scala</code>).
 * Since we want to be as generic as possible, we allow that a statement may be also
 * a structured control flow graph.
 *
 * @author Pietro Ferrara,Lucas Brutschy
 */
class ControlFlowGraph(val programpoint: ProgramPoint) extends Statement(programpoint) with WeightedGraph[List[Statement], Boolean] {

  def forwardSemantics[S <: State[S]](state: S): S = new ControlFlowGraphExecution[S](this, state).forwardSemantics(state).exitState()

  def backwardSemantics[S <: State[S]](state: S): S = ???

  def refiningSemantics[S <: State[S]](state: S, oldPreState: S): S = new ControlFlowGraphExecution[S](this, state).definiteBackwardSemantics(state).entryState()

  override def addNode(st: scala.collection.immutable.List[Statement]) = super.addNode(st)

  override def toSingleLineString(): String = {
    if (this.nodes.size != 1) return toString
    var result: String = ""
    for (st <- this.nodes.head)
      result = result + st.toSingleLineString()
    result
  }

  override def getChildren: List[Statement] = nodes.flatten

  def statementAt(blockIdx: Int, stmtIdx: Int): Statement = getBasicBlockStatements(blockIdx)(stmtIdx)

  def getBasicBlockStatements(index: Int): List[Statement] = nodes(index)

  /**
   * Check if a basic block has a conditional expression as the last statement.
   * We determine this by looking for conditional outgoing edges.
   * @param blockIndex the block to look at
   */
  def hasExitCondition(blockIndex: Int): Boolean = {
    val outEdges = exitEdges(blockIndex)
    if (outEdges.isEmpty)
      return false

    val allConditional = outEdges.forall({ case (_, _, cond) => cond.isDefined })
    val allUnconditional = outEdges.forall({ case (_, _, cond) => cond.isEmpty })
    // should be a correct assumption about valid CFGs, otherwise we are in trouble
    if (SystemParameters.DEBUG) assert(allConditional || allUnconditional)
    allConditional
  }

  override protected def nodeToString(node: List[Statement]) = ToStringUtilities.listToDotCommaRepresentationSingleLine(node)

}

class ControlFlowGraphExecution[S <: State[S]](val cfg: ControlFlowGraph, val state: S) extends CFGState[S] with WeightedGraph[List[S], Boolean]{

  this.nodes = getList[List[S]](this.cfg.nodes.size, state.bottom() :: Nil)

  def statesOfBlock(idx: Int): List[S] = nodes(idx)

  def stateFactory: S = state

  def this(cfgEx: ControlFlowGraphExecution[S]) {
    this(cfgEx.cfg, cfgEx.state)
    this.nodes = cfgEx.nodes // TODO: Implement later
  }

  def glb(left: ControlFlowGraphExecution[S], right: ControlFlowGraphExecution[S]): ControlFlowGraphExecution[S] = {
    if (!left.cfg.equals(right.cfg))
      throw new CFGSemanticException("It is not possible to compute the glb of the analysis of two different control flow graphs")
    val result: ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](left.cfg, state)
    for (i <- 0 to Math.max(left.nodes.length - 1, right.nodes.length - 1)) {
      if (i >= left.nodes.length)
        result.addNode(right.nodes.apply(i))
      else if (i >= right.nodes.length)
        result.addNode(left.nodes.apply(i))
      else
        result.addNode(this.glbOnListOfStates(left.nodes.apply(i), right.nodes.apply(i)))
    }
    result
  }

  def definiteBackwardSemantics(exitState: S): ControlFlowGraphExecution[S] =
    throw new NotImplementedError("No backward semantics")

  def exitState(): S = {
    var result: S = state.bottom()
    for (i <- nodes.indices) {
      var isExitPoint: Boolean = true
      for ((from, to, weight) <- cfg.edges) {
        if (from equals i)
          isExitPoint = false
      }
      if (isExitPoint) this.getExecution(i) match {
        case Nil =>
        case x => result = result.lub(this.getExecution(i).last)

      }
    }
    result
  }

  private def getExecution(i: Int): List[S] = {
    if (i >= 0 && i < nodes.size) nodes.apply(i)
    else null
  }

  def entryState(): S = nodes.head.head

  def forwardSemantics(initialState: S): ControlFlowGraphExecution[S] = {
    val result: ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](this)
    var l = Set(0)
    var iterationCount = Map.empty[Int, Int]
    while (l.nonEmpty) {
      val i = l.fold(Integer.MAX_VALUE)((a: Int, b: Int) => if (a > b) b else a)
      l = l - i
      val itNumber = iterationCount.get(i) match { case Some(x) => x; case None => 0 }
      val entry = if (i == 0) initialState else computeEntryState(result, i, itNumber)
      val previousEntry = if (result.getExecution(i).isEmpty) state.bottom() else result.getExecution(i).head
      if (!entry.lessEqual(previousEntry)) {
        result.setNode(i, this.forwardBlockSemantics(entry, cfg.nodes.apply(i)))
        l = l ++ cfg.getDirectSuccessors(i)
        iterationCount = iterationCount + ((i, itNumber + 1))
      }
    }
    result
  }

  def backwardSemantics(exitState: S): ControlFlowGraphExecution[S] = ???

  def lessEqual(right: ControlFlowGraphExecution[S]): Boolean = lessEqualOnLists[List[S]](this.nodes, right.nodes, checkBlockLessEqual)

  private def checkBlockLessEqual(left: List[S], right: List[S]): Boolean = lessEqualOnLists[S](left, right, lessEqualOnStates)

  private def lessEqualOnStates(left: S, right: S): Boolean = left.lessEqual(right)

  private def lessEqualOnLists[T](left: List[T], right: List[T], operator: (T, T) => Boolean): Boolean = {
    for (i <- left.indices) {
      val state: T = left.apply(i)
      if (state != null) {
        if (right.size <= i) return false
        val rightState: T = right.apply(i)
        if (rightState == null) return false
        if (!operator(state, rightState)) return false
      }
    }
    true
  }

  def lub(right: ControlFlowGraphExecution[S]): ControlFlowGraphExecution[S] = {
    if (!this.cfg.equals(right.cfg)) throw new CFGSemanticException("Cannot work with executions of different Control Flow Graphs")
    val result: ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](this.cfg, this.state)
    result.nodes = this.lubListListStates(this.nodes, right.nodes)
    result
  }

  private implicit val programPointOrdering = new Ordering[ProgramPoint] {
    def compare(p1: ProgramPoint, p2: ProgramPoint): Int = p1.toString.compare(p2.toString)
  }

  def widening(right: ControlFlowGraphExecution[S]): ControlFlowGraphExecution[S] = {
    if (!this.cfg.equals(right.cfg)) throw new CFGSemanticException("Cannot work with executions of different Control Flow Graphs")
    val result: ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](this.cfg, this.state)
    result.nodes = this.wideningListListStates(this.nodes, right.nodes)
    result
  }

  private def wideningListListStates(left: List[List[S]], right: List[List[S]]): List[List[S]] = {
    upperBoundsOnLists[List[S]](left, right, wideningListStates)
  }

  private def wideningListStates(left: List[S], right: List[S]): List[S] = {
    upperBoundsOnLists[S](left, right, wideningOnStates)
  }

  private def wideningOnStates(left: S, right: S): S = {
    left.widening(right)
  }

  private def upperBoundsOnLists[T](left: List[T], right: List[T], operator: (T, T) => T): List[T] = {
    val maxSize = if (right.size > left.size) right.size else left.size
    var result: List[T] = Nil; //new List[T](maxSize)
    for (i <- 0 until maxSize) {
      val elLeft: Option[T] = safeApply(left, i)
      val elRight: Option[T] = safeApply(right, i)
      if (elLeft.isEmpty && elRight.isEmpty)
        result = result ::: null.asInstanceOf[T] :: Nil
      else if (elLeft.isEmpty)
        result = result ::: elRight.get :: Nil
      else if (elRight.isEmpty)
        result = result ::: elLeft.get :: Nil
      else result = result ::: operator(elLeft.get, elRight.get) :: Nil
    }
    result
  }

  private def safeApply[T](list: List[T], index: Int): Option[T] = {
    if (index < 0 || index >= list.size) None
    else Some(list.apply(index))
  }

  override def toString: String = {
    var result: String = ""
    for (i <- nodes.indices) {
      val node: List[S] = nodes.apply(i)
      result = result +
        "Node n." + i + "\n-----------------\n" +
        "Entry node:\n" + entryNodesToString(i) +
        "\nExit node:\n" + exitNodesToString(i) + "\n"
      for (j <- node.indices) {
        result = result + node.apply(j).toString + "\n"
        if (j < node.size - 1)
          result = result + "| " + cfg.nodes.apply(i).apply(j).toSingleLineString + "\nV\n"
      }
    }
    result
  }

  def setStatesOfBlock(blockIdx: Int, states: List[S]): Unit =
    setNode(blockIdx, states)

  override protected def nodeToString(node: List[S]) = if (node != null) ToStringUtilities.listToNewLineRepresentation[S](node) else "BOTTOM"

  private def glbOnListOfStates(left: List[S], right: List[S]): List[S] = left match {
    case x :: xs => right match {
      case y :: ys => x.glb(y) :: this.glbOnListOfStates(xs, ys)
      case Nil => x :: this.glbOnListOfStates(xs, Nil); //throw new CFGSemanticException("I cannot make the glb of lists of different length")
    }
    case Nil => right match {
      case Nil => Nil
      case y :: ys => y :: this.glbOnListOfStates(ys, Nil); //throw new CFGSemanticException("I cannot make the glb of lists of different length")
    }
  }

  private def computeEntryState(current: ControlFlowGraphExecution[S], index: Int, it: Int): S = {
    var result: S = state.bottom()

    // Join incoming states
    for ((from, to, weight) <- cfg.edges) {
      if (to equals index) {

        // Try to see if we have already computed the given block...
        val pointedBy: List[S] = current.getExecution(from)

        // ..and if not we take the results of the previous iteration
        if (pointedBy != Nil) {
          val state =
            if (weight.isEmpty) pointedBy.last
            else if (weight.get) pointedBy.last.testTrue()
            else pointedBy.last.testFalse()
          result = result.lub(state)
        }

      }
    }

    // Widen with previous result
    if (current.getExecution(index).nonEmpty) {

      val previousEntry = current.getExecution(index).head
      if (it > SystemParameters.wideningLimit)
      // Widening is not necessarily a commutative operator
      // The state from the latest iteration should be the second operand
        result = previousEntry.widening(result)
      else
        result = previousEntry.lub(result)

    }

    result
  }

  private def getList[T](size: Int, el: T): List[T] = size match {
    case 0 => Nil
    case _ => el :: getList(size - 1, el)
  }

  private def forwardBlockSemantics(entryState: S, block: List[Statement]): List[S] = block match {
    case x :: xs =>
      val modifiedState = entryState.before(identifyingPP(x))
      val resultingState = x.forwardSemantics(modifiedState)
      modifiedState :: forwardBlockSemantics(resultingState, xs)
    case Nil => entryState :: Nil
  }

  /**
    * Returns a program point uniquely identifying a single statement as used in
    * the block semantics (!). The computation finds the leftmost involved program
    * point.
    *
    * @param s A statement as used in the block semantics
    * @return The leftmost involved program point
    */
  private def identifyingPP(s: Statement): ProgramPoint = s match {
    case Assignment(pp, l, r) => (pp :: identifyingPP(l) :: identifyingPP(r) :: Nil).min
    case MethodCall(pp, m, _, p, _, _) => (pp :: identifyingPP(m) :: p.map(identifyingPP)).min
    case VariableDeclaration(pp, v, _, r) =>
      List(Some(pp), Some(identifyingPP(v)), r.map(identifyingPP)).flatten.min
    case FieldAccess(pp, obj, _, _) => List(pp, identifyingPP(obj)).min
    case _ => s.getPC()
  }

  private def lubListListStates(left: List[List[S]], right: List[List[S]]): List[List[S]] = {
    upperBoundsOnLists[List[S]](left, right, lubListStates)
  }

  private def lubListStates(left: List[S], right: List[S]): List[S] = {
    upperBoundsOnLists[S](left, right, lubOnStates)
  }

  private def lubOnStates(left: S, right: S): S = {
    left.lub(right)
  }
}

class CFGSemanticException(message: String) extends Exception(message)