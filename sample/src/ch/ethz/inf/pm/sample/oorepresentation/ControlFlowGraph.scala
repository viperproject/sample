package ch.ethz.inf.pm.sample.oorepresentation

//import scala.collection.mutable._

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.SystemParameters

/**
 * This class represents an oriented weighted graph.
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
trait WeightedGraph[T, W] {

  var nodes: List[T] = Nil
  var edges: Set[(Int, Int, Option[W])] = Set.empty
  val weightLabel = "weight"

  def removeNode(node: T) {
    val index: Int = this.addNodeIfNotExisting(node)
    var newEdges: Set[(Int, Int, Option[W])] = Set.empty
    for ((i1, i2, w) <- edges) {
      val index1 = if (i1 > index) i1 - 1 else i1
      val index2 = if (i2 > index) i2 - 1 else i2
      if (index != i1 && index != i2)
        newEdges = newEdges + ((index1, index2, w))
    }
    edges = newEdges
    nodes = remove(nodes, index)
  }

  private def remove[T1](list: List[T1], index: Int): List[T1] = list match {
    case x :: x1 if index != 0 => x :: remove(x1, index - 1)
    case x :: x1 if index == 0 => remove(x1, index - 1)
    case Nil => Nil
  }

  /**
   * Add a node to the current graph
   *
   * @param node the node to be added
   * @return the index of the inserted node
   */
  def addNode(node: T): Int = {
    nodes = nodes ::: node :: Nil; nodes.lastIndexOf(node)
  }

  /**
   * Add a node to the current graph iff it is not yet in the graph. Otherwise, it returns the index
   * of the existing node.
   *
   * @param node the node to be added if it does not exist
   * @return the index of the(evantually inserted) node
   */
  def addNodeIfNotExisting(node: T): Int = {
    for (i <- 0 to nodes.length - 1)
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
  def setNode(index: Int, node: T) {
    nodes = nodes.updated(index, node)
  }

  /**
   * Add an edge to the current graph
   *
   * @param from the node from which the edge starts
   * @param to the node to which point the edge
   * @param weight the weight of the edge or None if this edge is not linked to a weight
   */
  def addEdge(from: Int, to: Int, weight: Option[W]): Unit = edges = edges.+((from, to, weight))

  /**
   * Return the node ids without sorting edges
   *
   * @return the leaves of the graph
   */
  def getLeafs: Set[T] = {
    getLeafIds map { id => nodes(id) }
  }

  def getLeafIds: Set[Int] = {
    var notLeafs: Set[Int] = Set.empty
    for ((i1, i2, w) <- edges) //{
      notLeafs = notLeafs + i1
    var result: Set[Int] = Set.empty
    for (i <- 0 to nodes.length - 1)
      if (!notLeafs.contains(i))
        result = result + i
    result
  }

  def entryEdges(nodeIndex: Int): Set[(Int, Int, Option[W])] = {
    for (e@(from, to, _) <- edges if to == nodeIndex) yield e
  }

  def exitEdges(nodeIndex: Int): Set[(Int, Int, Option[W])] = {
    for (e@(from, to, _) <- edges if from == nodeIndex) yield e
  }

  def initialBlockInLoop(index: Int): Boolean = {
    if (entryEdges(index).size <= 1) return false
    var prevEdges: Set[(Int, Int, Option[W])] = this.exitEdges(index)
    var nextEdges: Set[(Int, Int, Option[W])] = prevEdges

    while (nextEdges.size > 0) {
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
      val node: T = nodes.apply(i)
      result = result +
        "Node n." + i + "\n-----------------\n" +
        "Entry node:\n" + entryNodesToString(i) +
        "\nExit node:\n" + exitNodesToString(i) +
        "\nBlock:\n" + nodeToString(node) + "\n-----------------\n"
      i = i + 1
    }
    result
  }

  /**
   * It adds a node containing the given statements to the control flow graph.
   * It adds an edge from each entry point to the fresh node (related with the given condition).
   * It returns the index of the fresh node. 
   */
  def addSummaryNode(statements: T, entryPoints: List[(Int, Option[W])]): Int = {
    val index: Int = this.addNode(statements)
    for (entryindex: (Int, Option[W]) <- entryPoints)
      this.addEdge(entryindex _1, index, entryindex _2)
    index
  }

  def getDirectSuccessors(nodeIndex: Int): Set[Int] = {
    for ((from, to, _) <- exitEdges(nodeIndex)) yield to
  }

  def getDirectPredecessors(nodeIndex: Int): Set[Int] = {
    for ((from, to, _) <- entryEdges(nodeIndex)) yield from
  }



  protected def nodeToString(node: T) = node.toString

  private def weightToString(weight: Option[W]): String = weight match {
    case None => ""
    case null => ""
    case Some(x) => x.toString
  }

  protected def entryNodesToString(i: Int): String = {
    var result: String = ""
    for (edge: (Int, Int, Option[W]) <- edges) {
      if (edge._2 == i)
        result = result + edge._1 + ", " + weightLabel + ":" + weightToString(edge._3) + "\n"
    }
    result
  }

  protected def exitNodesToString(i: Int): String = {
    var result: String = ""
    for (edge: (Int, Int, Option[W]) <- edges) {
      if (edge._1 == i)
        result = result + edge._2 + ", " + weightLabel + ":" + weightToString(edge._3) + "\n"
    }
    result
  }
}

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
 * @author Pietro Ferrara
 * @version 0.1
 */
class ControlFlowGraph(val programpoint: ProgramPoint) extends Statement(programpoint) with WeightedGraph[List[Statement], Boolean] {
  override val weightLabel = "condition"

  def forwardSemantics[S <: State[S]](state: S): S = new ControlFlowGraphExecution[S](this, state).forwardSemantics(state).exitState()

  def backwardSemantics[S <: State[S]](state: S, oldPreState: S): S = new ControlFlowGraphExecution[S](this, state).definiteBackwardSemantics(state).entryState()

  def happensBefore(pc1: LineColumnProgramPoint, pc2: LineColumnProgramPoint): Boolean = {
    var rowsAfterPc1: Set[Int] = this.afterPC(pc1)
    var rowsAfterPc2: Set[Int] = this.afterPC(pc2)
    return rowsAfterPc1.contains(pc2.getLine) && !rowsAfterPc2.contains(pc1.getLine)
  }

  private def afterPC(pc: LineColumnProgramPoint): Set[Int] = afterBlock(giveBlockOf(pc))

  def afterBlock(startingBlock: Int): Set[Int] = {
    var result: Set[Int] = Set.empty[Int]
    var blocksalreadyvisited: Set[Int] = Set.empty[Int]
    if (startingBlock == -1) return Set.empty[Int]
    var blockstovisit: Set[Int] = Set(startingBlock)
    while (blockstovisit.size > 0) {
      var b = blockstovisit.head
      blockstovisit = blockstovisit - b
      blocksalreadyvisited = blocksalreadyvisited + b
      blockstovisit = blockstovisit ++ this.getDirectSuccessors(b)
      for (st <- nodes.apply(b))
        result = result + st.getPC().asInstanceOf[LineColumnProgramPoint].getLine
      blockstovisit = blockstovisit -- blocksalreadyvisited
    }
    return result
  }

  def beforeBlock(startingBlock: Int): Set[Int] = {
    var result: Set[Int] = Set.empty[Int]
    var blocksalreadyvisited: Set[Int] = Set.empty[Int]
    if (startingBlock == -1) return Set.empty[Int]
    var blockstovisit: Set[Int] = Set(0)
    while (blockstovisit.size > 0) {
      var b = blockstovisit.head
      blockstovisit = blockstovisit - b
      blocksalreadyvisited = blocksalreadyvisited + b
      result=result+b;
      blockstovisit = blockstovisit ++ getDirectPredecessors(b)
    }
    return result
  }

  def indexesafterBlockWithouthCurrentBlock(startingBlock: Int): Set[Int] = {
    var result: Set[Int] = Set.empty[Int]
    var blocksalreadyvisited: Set[Int] = Set.empty[Int]
    if (startingBlock == -1) return Set.empty[Int]
    val exitedges = this.exitEdges(startingBlock)
    var blockstovisit: Set[Int] = Set(startingBlock)
    for (e <- exitedges)
      if (e._3 != None && e._3.get == true)
        blockstovisit = blockstovisit + e._2
    while (blockstovisit.size > 0) {
      var b = blockstovisit.head
      blockstovisit = blockstovisit - b
      blocksalreadyvisited = blocksalreadyvisited + b
      if (b != startingBlock) blockstovisit = blockstovisit ++ this.getDirectSuccessors(b)
      result = result + b
      blockstovisit = blockstovisit -- blocksalreadyvisited
    }
    return result
  }

  private def giveBlockOf(pc: LineColumnProgramPoint): Int = {
    for (i <- 0 to nodes.size - 1) {
      for (st <- nodes.apply(i))
        if (st.getPC() != null && pc != null && st.getPC().asInstanceOf[LineColumnProgramPoint].getLine == pc.getLine)
          return i
    }
    return -1
  }

  /**
   * Return an ordered list of indexes that could be used for the computation of
   * fixpoints over CFG.
   * There is not guarantee that this sequence will give better results than others
   * since it is based on some heuristics and should be deeply tested.
   *
   * @return A sequence of all the indexes of the CFG
   */
  def getIterativeSequence(): List[Int] = this.getIterativeSequence(Nil, Nil, 0)

  private def getIterativeSequence(alreadyVisited: List[Int], pendingBlocks: List[Int], next: Int): List[Int] = {
    //We consider the next blocks that have not yet been visited
    def exitNodes = this.getDirectSuccessors(next)
    def exitNodesNotVisited = exitNodes.--(alreadyVisited).-(next)
    if (!exitNodesNotVisited.isEmpty) {
      //We take the minimum since we expect to be the optimal one (heuristic)
      val min = this.getMin(exitNodesNotVisited)
      return next :: getIterativeSequence(alreadyVisited ::: next :: Nil, pendingBlocks ++ (exitNodesNotVisited - min), min)
    }
    else {
      def pendingNodesNotVisited = pendingBlocks.toSet.--(alreadyVisited).-(next)
      if (pendingNodesNotVisited.isEmpty) {
        //We are at the end!
        //assert(this.nodes.size==alreadyVisited.size+1)
        return next :: Nil
      }
      else {
        //Otherwise we consider the minimum of the pendingBlocks
        val min = this.getMin(pendingNodesNotVisited.toSet)
        return next :: getIterativeSequence(alreadyVisited ::: next :: Nil, pendingBlocks ++ (exitNodesNotVisited - min), min)
      }
    }
  }

  private def getMin(l: Set[Int]): Int = {
    assert(!l.isEmpty)
    var min: Int = 0 - 1
    for (el <- l)
      if (min == 0 - 1 || el < min) min = el
    assert(min >= 0)
    return min
  }

  override protected def nodeToString(node: List[Statement]) = ToStringUtilities.listToDotCommaRepresentationSingleLine(node)

  override def addNode(st: scala.collection.immutable.List[Statement]) = super.addNode(st);

  //Work-around for Java interfacing

  override def toSingleLineString(): String = {
    if (this.nodes.size != 1) return toString()
    var result: String = ""
    for (st <- this.nodes.apply(0))
      result = result + st.toSingleLineString()
    result
  }

  override def getChildren: List[Statement] = nodes.flatten

  def getBasicBlockStatements(index: Int): List[Statement] = nodes(index)
}

class ControlFlowGraphExecution[S <: State[S]](val cfg: ControlFlowGraph, val state: S) extends CFGState[S] with WeightedGraph[List[S], Boolean]{

  this.nodes = getList[List[S]](this.cfg.nodes.size, state.bottom() :: Nil)

  def getStatesOfBlock(idx: Int): List[S] = nodes(idx)

  def factoryState: S = state

  def this(cfgEx: ControlFlowGraphExecution[S]) {
    this(cfgEx.cfg, cfgEx.state)
    this.nodes = cfgEx.nodes // TODO: Implement later
  }

  override protected def nodeToString(node: List[S]) = if (node != null) ToStringUtilities.listToNewLineRepresentation[S](node) else "BOTTOM"

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

  def definiteBackwardSemantics(exitState: S): ControlFlowGraphExecution[S] =
    throw new NotImplementedError("No backward semantics")

  def exitState(): S = {
    var result: S = state.bottom()
    for (i <- 0 to nodes.size - 1) {
      var isExitPoint: Boolean = true
      for ((from, to, weight) <- cfg.edges) {
        if (from equals (i))
          isExitPoint = false
      }
      if (isExitPoint) this.getExecution(i) match {
        case Nil =>
        case x => result = result.lub(this.getExecution(i).last)

      }
    }
    result
  }

  def entryState(): S = nodes.apply(0).apply(0)

  def forwardSemantics(initialState: S): ControlFlowGraphExecution[S] = {
    val result: ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](this)
    var l = Set(0)
    var iterationCount = Map.empty[Int, Int]
    while (!l.isEmpty) {
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

  private def computeEntryState(current: ControlFlowGraphExecution[S], index: Int, it: Int): S = {
    var result: S = state.bottom()

    // Join incoming states
    for ((from, to, weight) <- cfg.edges) {
      if (to equals (index)) {

        // Try to see if we have already computed the given block...
        val pointedBy: List[S] = current.getExecution(from)

        // ..and if not we take the results of the previous iteration
        if (pointedBy != Nil) {
          val state =
            if (weight == None) pointedBy.last
            else if (weight.get == true) pointedBy.last.testTrue()
            else pointedBy.last.testFalse()
          result = result.lub(state)
        }

      }
    }

    // Widen with previous result
    if (!current.getExecution(index).isEmpty) {

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

  private def getExecution(i: Int): List[S] = {
    if (i >= 0 && i < nodes.size) nodes.apply(i)
    else null
  }

  private def getList[S](size: Int, el: S): List[S] = size match {
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

  private implicit val programPointOrdering = new Ordering[ProgramPoint] {
    def compare(p1: ProgramPoint, p2: ProgramPoint): Int = p1.toString.compare(p2.toString)
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
    case MethodCall(pp, m, _, p, _) => (pp :: identifyingPP(m) :: p.map(identifyingPP)).min
    case VariableDeclaration(pp, v, _, r) =>
      List(Some(pp), Some(identifyingPP(v)), r.map(identifyingPP)).flatten.min
    case FieldAccess(pp, obj, _, _) => List(pp, identifyingPP(obj)).min
    case _ => s.getPC()
  }

  def lessEqual(right: ControlFlowGraphExecution[S]): Boolean = lessEqualOnLists[List[S]](this.nodes, right.nodes, checkBlockLessEqual)

  private def checkBlockLessEqual(left: List[S], right: List[S]): Boolean = lessEqualOnLists[S](left, right, lessEqualOnStates)

  private def lessEqualOnStates(left: S, right: S): Boolean = left.lessEqual(right)

  private def lessEqualOnLists[T](left: List[T], right: List[T], operator: (T, T) => Boolean): Boolean = {
    for (i <- 0 to left.size - 1) {
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

  private def lubListListStates(left: List[List[S]], right: List[List[S]]): List[List[S]] = {
    upperBoundsOnLists[List[S]](left, right, lubListStates)
  }

  private def lubListStates(left: List[S], right: List[S]): List[S] = {
    upperBoundsOnLists[S](left, right, lubOnStates)
  }

  private def lubOnStates(left: S, right: S): S = {
    left.lub(right)
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
    for (i <- 0 to maxSize - 1) {
      val elLeft: Option[T] = safeApply(left, i)
      val elRight: Option[T] = safeApply(right, i)
      if (elLeft.equals(None) && elRight.equals(None))
        result = result ::: null.asInstanceOf[T] :: Nil
      else if (elLeft.equals(None))
        result = result ::: elRight.get :: Nil
      else if (elRight.equals(None))
        result = result ::: elLeft.get :: Nil
      else result = result ::: operator(elLeft.get, elRight.get) :: Nil
    }
    result.toList
  }

  private def safeApply[T](list: List[T], index: Int): Option[T] = {
    if (index < 0 || index >= list.size) None
    else Some(list.apply(index))
  }

  override def toString: String = {
    var result: String = ""
    for (i <- 0 to nodes.size - 1) {
      val node: List[S] = nodes.apply(i)
      result = result +
        "Node n." + i + "\n-----------------\n" +
        "Entry node:\n" + entryNodesToString(i) +
        "\nExit node:\n" + exitNodesToString(i) + "\n"
      for (j <- 0 to node.size - 1) {
        result = result + node.apply(j).toString + "\n"
        if (j < node.size - 1)
          result = result + "| " + cfg.nodes.apply(i).apply(j).toSingleLineString + "\nV\n"
      }
    }
    result
  }

}

class CFGSemanticException(message: String) extends Exception(message)