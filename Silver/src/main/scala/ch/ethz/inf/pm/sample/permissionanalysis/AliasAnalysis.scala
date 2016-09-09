/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.{Analysis, ForwardEntryStateBuilder, SimpleForwardAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilverAnalysisRunner
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState.Default
import ch.ethz.inf.pm.sample.permissionanalysis.AliasGraph._
import ch.ethz.inf.pm.sample.permissionanalysis.HeapNode._
import ch.ethz.inf.pm.sample.permissionanalysis.Types._
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging

object Dummy
{
  def main(args: Array[String]): Unit = {
    val m1 = Map(1 -> "a", 2 -> "b")
    val m2 = m1 + (1 -> "c")
    val m3 = m1.updated(1, "c")
    println(m2(1))
    // outputs "c"
    println(m3(1))
    // outputs "c"
  }
}

/** Various type shortcuts.
  *
  * @author Jerome Dohrau
  */
object Types
{
  type AccessPath = List[Identifier]

  type Store = Map[String, Set[HeapNode]]

  type Heap = Map[HeapNode, FieldMap]

  type FieldMap = Map[String, Set[HeapNode]]

  def Store(): Store = Map.empty

  def Heap(): Heap = Map.empty

  def FieldMap(): FieldMap = Map.empty
}

/** A heap node used in the alias graph.
  *
  * @param name The name of the node.
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
case class HeapNode(name: String,
                    typ: Type = DummyRefType,
                    pp: ProgramPoint = DummyProgramPoint)
  extends Identifier
{
  /** Returns the name of the identifier.
    *
    * @return The name of the identifier.
    */
  override def getName: String = name

  /** Returns the name of the field that is represented by the identifier.
    *
    * @return The name of the field that is represented by the identifier.
    */
  override def getField: Option[String] = None

  /** Returns true if the identifier represents exactly one variable.
    *
    * @return True if the identifier represents exactly one variable.
    */
  override def representsSingleVariable: Boolean = true

  override def equals(other: Any) = other match {
    case node: HeapNode => name equals node.name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode
}

object HeapNode
{
  /** The unique wildcard node.
    */
  object WildcardNode
    extends HeapNode("*")
  {
    /** Returns true if the identifier represents exactly one variable.
      *
      * @return True if the identifier represents exactly one variable.
      */
    override def representsSingleVariable: Boolean = false
  }

  /**
    * The unique summary node.
    */
  object SummaryNode
    extends HeapNode("Σ") {
    /** Returns true if the identifier represents exactly one variable.
      *
      * @return True if the identifier represents exactly one variable.
      */
    override def representsSingleVariable: Boolean = false
  }

  /** The unique unknown node.
    */
  object UnknownNode
    extends HeapNode("?")
  {
    /** Returns true if the identifier represents exactly one variable.
      *
      * @return True if the identifier represents exactly one variable.
      */
    override def representsSingleVariable: Boolean = false
  }

  /** The unique null node.
    */
  object NullNode
    extends HeapNode("null")

  /** The unique new node used to represent a newly created heap location.
    */
  object NewNode
    extends HeapNode("new")
}

/** A graph representing alias information.
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait AliasGraph[T <: AliasGraph[T]]
{
  this: T =>

  /** Returns the store that maps variables to sets of heap nodes.
    *
    * @return The store.
    */
  def store: Store

  /** Returns the heap that maps heap nodes to mappings from field names to set
    * of heap nodes.
    *
    * @return The heap.
    */
  def heap: Heap

  /** Returns the set of fields declared in the program. The set only contains
    * fields with a reference types since all other fields are irrelevant for
    * the alias analysis.
    *
    * @return The set of fields.
    */
  def fields: Set[String]

  def ids = IdentifierSet.Top

  /** Returns the current program point.
    *
    * @return The current program point.
    */
  def currentPP: ProgramPoint

  /** Returns true if the graph is a must alias graph.
    *
    * @return True if the graph is a must alias graph
    */
  def isMayAliasGraph: Boolean = this.isInstanceOf[MayAliasGraph]

  /** Returns true if the graph is a must alias graph.
    *
    * @return true if the graph is a must alias graph.
    */
  def isMustAliasGraph: Boolean = this.isInstanceOf[MustAliasGraph]

  /** Returns true if the given access paths may / must alias (depending on
    * whether this is a may or must alias graph).
    *
    * @param first  The first access path.
    * @param second The second access path.
    * @return True if the given access paths may / must alias.
    */
  def pathsAlias(first: List[String], second: List[String]): Boolean

  /** Initializes the alias graph.
    *
    * @param fields The set of fields in the program.
    */
  def initialize(fields: Set[String]): T = {
    // prepare the initial heap
    val fieldMap = fields.foldLeft(FieldMap()) { (map, field) => map + (field -> initialValue()) }

    val x = initialValue() - NullNode
    val heap = x.foldLeft(Heap()) { (map, node) => map + (node -> fieldMap) }
    // set initial heap
    copy(heap = heap, fields = fields)
  }

  /** Returns the least upper bound of this and the other alias graph.
    *
    * @param other The other alias graph.
    * @return The least upper bound of this and the other alias graph.
    */
  def lub(other: T): T

  /** Returns the greatest lower bound of this and the other alias graph.
    *
    * @param other The other alias graph.
    * @return The greatest lower bound of this and the other alias graph.
    */
  def glb(other: T): T

  /** Returns true if this alias graph is less than or equal to the other alias
    * graph.
    *
    * @param other The other alias graph.
    * @return True if this alias graph is less than or equal to the other alias
    *         graph.
    */
  def lessEqual(other: T): Boolean

  /** Adds the variable with the given name to the store.
    *
    * @param name The name of the variable.
    * @return The alias graph with the variable added.
    */
  def addVariable(name: String): T =
    copy(store = store + (name -> initialValue()))

  /** Adds a node with the given name to the heap.
    *
    * @param name   The name of the node.
    * @return The alias graph with the node added
    */
  def addNode(name: String): T = {
    // create node and initialize its fields
    val node = HeapNode(name)
    val fieldMap = fields.foldLeft(FieldMap()) { (map, field) => map + (field -> initialValue()) }
    // update heap
    copy(heap = heap + (node -> fieldMap))
  }

  /** Removes the variable with the given name from the store.
    *
    * @param name The name of the variable.
    * @return The alias graph with the variable removed.
    */
  def removeVariable(name: String): T =
    copy(store = store - name)

  /** Removes all variables satisfying the given predicate.
    *
    * @param filter The filter predicate.
    * @return The alias graph with the variables removed.
    */
  def pruneVariables(filter: String => Boolean): T =
    copy(store = store.filterKeys(filter))

  /**
    *
    * @param left
    * @param right
    * @param operator
    * @return
    */
  def assumeComparison(left: Expression, right: Expression, operator: ArithmeticOperator.Value): Option[T] = {
    // the function used to evaluate an expression
    def evaluate(expression: Expression): (Set[HeapNode], Set[HeapNode]) = expression match {
      case Constant("null", _, _) => (Set.empty, Set(NullNode))
      case VariableIdentifier(variable, _) => (Set.empty, evaluatePath(variable :: Nil))
      case AccessPathIdentifier(accessPath) =>
        val path = accessPath.map(_.getName)
        val receivers = evaluateReceiver(path)
        val values = evaluateLast(receivers, path)
        (receivers, values)
    }

    def update(target: Expression,
               receivers: Set[HeapNode],
               values: Set[HeapNode],
               mask: Set[HeapNode],
               state: Option[(Store, Heap)]): Option[(Store, Heap)] = (target, state) match {
      case (_, None) => None
      case (VariableIdentifier(variable, _), Some((store, heap))) =>
        val newValues = values & mask
        if (newValues.isEmpty && isMayAliasGraph) None
        else {
          val newStore = store + (variable -> newValues)
          Some(newStore, heap)
        }
      case (AccessPathIdentifier(path), state) =>
        val field = path.last.getName
        receivers.foldLeft(state) {
          case (None, _) => None
          case (Some((store, heap)), receiver) =>
            val fieldMap = heap.getOrElse(receiver, Map.empty)
            val values = fieldMap.getOrElse(field, Set.empty)
            val newValues = values & mask
            if (newValues.isEmpty && isMayAliasGraph) None
            else {
              val newFieldMap = fieldMap + (field -> newValues)
              val newHeap = heap + (receiver -> newFieldMap)
              Some(store, newHeap)
            }
        }
      case _ => state
    }

    // evaluate left and right expression
    val (leftReceivers, leftValues) = evaluate(left)
    val (rightReceivers, rightValues) = evaluate(right)

    // compute masks for left and right values
    val (leftMask, rightMask) = operator match {
      case ArithmeticOperator.== =>
        (rightValues, leftValues)
      case ArithmeticOperator.!= =>
        val left = if (rightValues.size == 1 || isMayAliasGraph) leftValues -- rightValues else leftValues
        val right = if (leftValues.size == 1 || isMayAliasGraph) rightValues -- leftValues else rightValues
        (left, right)
    }

    // update state
    val state0 = Some(store, heap)
    val state1 = update(left, leftReceivers, leftValues, leftMask, state0)
    val s3 = update(right, rightReceivers, rightValues, rightMask, state1)
    s3.map { case (newStore, newHeap) => copy(store = newStore, heap = newHeap) }
  }

  /** Assigns the given value to the given target.
    *
    * @param target The target of the assignment.
    * @param value  The value of the assignment.
    * @return
    */
  def assign(target: Expression, value: Expression): T = {
    println(s"target: $target, value: $value")
    if (target.typ.isObject) {

      // evaluate value
      val values = value match {
        case Constant("null", _, _) => Set(NullNode: HeapNode)
        case NewNode => Set(NewNode: HeapNode)
        case VariableIdentifier(variable, _) => evaluatePath(variable :: Nil)
        case AccessPathIdentifier(path) => evaluatePath(path.map(_.getName))
        case _ => throw new NotImplementedError("An assign implementation is missing.")
      }

      target match {
        case VariableIdentifier(variable, _) =>
          if (values contains NewNode) {
            // update store and heap
            val node = HeapNode(variable, target.typ, target.pp)
            val newStore = store + (variable -> Set(node))
            val newHeap = heap + (node -> heap.getOrElse(NewNode, Map.empty))
            // update state
            copy(store = newStore, heap = newHeap)
          } else {
              val newStore = store + (variable -> values)
              copy(store = newStore)
          }
        case AccessPathIdentifier(path) =>
          val receivers = evaluateReceiver(path.map(_.getName))
          val field = path.last.getName
          val newHeap = receivers.foldLeft(heap) {
            case (h, receiver) =>
              val fieldMap = h.getOrElse(receiver, Map.empty)
              val newFieldMap = if (!receiver.representsSingleVariable && this.isInstanceOf[MayAliasGraph]){
                // weak update
                val fieldValues = fieldMap.getOrElse(field, Set.empty)
                fieldMap + (field -> (fieldValues ++ values))
              } else {
                // strong update
                fieldMap + (field -> values)
              }
              h + (receiver -> newFieldMap)
          }
          copy(heap = newHeap)
        case _ => throw new NotImplementedError("An assign implementation is missing.")
      }
    } else {
      target match {
        case _: VariableIdentifier => this
        case _: AccessPathIdentifier => this
        case _ => throw new NotImplementedError("An assign implementation is missing.")
      }
    }
  }

  def havoc(path: List[String]): T = {
    if (path.isEmpty) this
    else if (path.length == 1) {
      val variable = path.head
      val newStore = store + (variable -> Set(WildcardNode: HeapNode))
      copy(store = newStore)
    } else {
      val receivers = evaluateReceiver(path)
      val field = path.last
      val newHeap = receivers.foldLeft(heap) { (heap, receiver) =>
        val fieldMap = heap.getOrElse(receiver, Map.empty)
        val newFieldMap = fieldMap + (field -> Set(WildcardNode: HeapNode))
        heap + (receiver -> newFieldMap)
      }
      copy(heap = newHeap)
    }
  }

  /** Materializes the path represented by the given expression.
    *
    * @param expression The expression to materialize.
    * @return The alias graph with the given expression materialized.
    */
  def materialize(expression: Expression): T = expression match {
    case VariableIdentifier(variable, _) => materialize(variable :: Nil)
    case AccessPathIdentifier(path) => materialize(path.map(_.getName))
    case _ => this
  }

  /** Materializes the given access path.
    *
    * @param path The access path to materialize.
    * @return The alias graph with the given access path materialized.
    */
  def materialize(path: List[String]): T = {
    // materialize variable
    val variable = path.head
    val fields = path.tail

    val (finalGraph, _) = fields.foldLeft(materializeVariable(variable)) {
      case ((graph, receivers), field) =>
      receivers.foldLeft((graph, Set.empty[HeapNode])) {
        case ((currGraph, currValues), receiver) =>
          val (newGraph, newValues) = currGraph.materializeField(receiver, field)
          (newGraph, currValues ++ newValues)
      }
    }

    finalGraph
  }

  /** Materializes the variable with the given name.
    *
    * @param variable The name of the variable to materialize.
    * @return The alias graph with the variable materialized.
    */
  def materializeVariable(variable: String): (T, Set[HeapNode]) = {
    // get value of the variable
    val values = store.getOrElse(variable, Set.empty)

    // it should not happen that the summary node gets materialized
    if (values contains WildcardNode) {
      // only materialize wildcard node in the may alias graph
      if (isMayAliasGraph) {
        // map variable to all nodes
        val allNodes = heap.keySet ++ heap.values.flatMap { map => map.values.flatten } - UnknownNode
        val newStore = store + (variable -> allNodes)
        (copy(store = newStore), allNodes)
      } else (this, Set.empty)
    } else if (values contains UnknownNode) {
      // create fresh heap node
      val fresh = HeapNode(variable)
      // remove unknown node and add fresh node
      val newValues = values - UnknownNode + fresh
      val newStore = store + (variable -> newValues)
      // update heap with the fresh node
      val newHeap = heap + (fresh -> heap(UnknownNode))
      (copy(store = newStore, heap = newHeap), newValues)
    } else {
      // there is nothing to materialize
      (this, values)
    }
  }

  /** Materializes the field with the given receiver and name.
    *
    * @param receiver The receiver of the field.
    * @param field    The name of the field.
    * @return The alias graph with the field materialized.
    */
  protected def materializeField(receiver: HeapNode, field: String): (T, Set[HeapNode]) = {
    // get value of the field
    val values = heap.getOrElse(receiver, Map.empty).getOrElse(field, Set.empty)

    // it should not happen that the summary node gets materialized
    if (values contains WildcardNode) {
      // only materialize wildcard node in the may alias graph
      if (isMayAliasGraph) {
        // map field to all nodes
        val allNodes: Set[HeapNode] = heap.keySet ++ heap.values.flatMap { map => map.values.flatten } - UnknownNode
        val newFieldMap = heap.getOrElse(receiver, Map.empty) + (field -> allNodes)
        val newHeap = heap + (receiver -> newFieldMap)
        (copy(heap = newHeap), allNodes)
      } else (this, Set.empty)
    }else if (values contains UnknownNode) {
      // create fresh heap node
      val fresh = HeapNode(s"${receiver.name}.$field")
      // remove unknown node and add fresh node and update heap with fresh node
      val newValue = values - UnknownNode + fresh
      val newFieldMap = heap.getOrElse(receiver, Map.empty) + (field -> newValue)
      val newHeap = heap + (receiver -> newFieldMap) + (fresh -> heap(UnknownNode))
      (copy(heap = newHeap), newValue)
    } else {
      // there is nothing to materialize
      (this, values)
    }
  }

  /** Performs an abstract garbage collection by pruning all unreachable heap
    * nodes.
    *
    * @return The updated alias graph.
    */
  def pruneUnreachableNodes(): T = {
    // retrieve nodes reachable from variables
    var reachable = store.foldLeft(Set.empty[HeapNode]) {
      (set, entry) => set ++ entry._2
    }
    // recursively retrieve nodes reachable via field accesses
    var oldSize = 0
    while (reachable.size > oldSize) {
      oldSize = reachable.size
      reachable = reachable ++ reachable.foldLeft(Set.empty[HeapNode]) {
        (set, node) => set ++ heap.getOrElse(node, Map.empty).foldLeft(Set.empty[HeapNode])  {
          (set, entry) => set ++ entry._2
        }
      }
    }
    // remove all unreachable heap nodes
    var newHeap = heap
    val unreachable = (heap.keySet diff reachable) - UnknownNode
    for (key <- unreachable) {
      newHeap = newHeap - key
    }
    // return graph with updated heap
    copy(heap = newHeap)
  }

  /** Sets the current program point to the given program point.
    *
    * @param pp The new current program point.
    */
  def before(pp: ProgramPoint) = copy(currentPP = pp)

  /** Evaluates the receiver of the given access path. That is, all but the last
    * field of the access path are evaluated.
    *
    * @param path The path.
    * @return The set of receivers.
    */
  def evaluateReceiver(path: List[String]): Set[HeapNode] = {
    if (path.isEmpty) Set.empty
    else {
      // evaluate variable access
      val variable = path.head
      val variableValues = store.getOrElse(variable, Set.empty)

      val fields = path.tail
      if (fields.isEmpty) variableValues
      else {
        // evaluate intermediate field accesses
        fields.init.foldLeft(variableValues) { case (values, field) =>
          values.foldLeft(Set.empty[HeapNode]) { case (receivers, node) =>
            val fieldMap = heap.getOrElse(node, Map.empty)
            val fieldValues = fieldMap.getOrElse(field, Set.empty)
            receivers ++ fieldValues
          }
        }
      }
    }
  }

  /** Evaluates the last field of the given access path on the given set of
    * receivers.
    *
    * @param receivers The set of receivers.
    * @param path      The path.
    * @return The set of values.
    */
  def evaluateLast(receivers: Set[HeapNode], path: List[String]) : Set[HeapNode] = {
    if (path.length <= 1) receivers
    else {
      // evaluate last field access
      val lastField = path.last
      receivers.foldLeft(Set.empty[HeapNode]) {
        case (values, node) => values ++ heap.getOrElse(node, Map.empty).getOrElse(lastField, Set.empty)
      }
    }
  }

  /** Evaluates the given access path.
    *
    * @param path The access path to evaluate
    * @return The set of heap nodes the access path points to.
    */
  def evaluatePath(path: List[String]): Set[HeapNode] = {
    val receivers = evaluateReceiver(path)
    evaluateLast(receivers, path)
  }

  /** Assumes that the receivers of the given expression are all non null.
    *
    * @param expression The expression.
    * @return The alias graph with the receivers assumed to be non null.
    */
  def assumeNonNullReceiver(expression: Expression): Option[T] = expression match {
    case AccessPathIdentifier(path) =>
      if (path.length <= 1) Some(this)
      else {
        // evaluate variable access
        val variable = path.head.getName
        val variableValues = store.getOrElse(variable, Set.empty)

        // make sure variable is not null
        val (newVariableValues, newStore) = if (variableValues contains NullNode) {
          // report possible null pointer dereference
          Reporter.reportGenericWarning("Possible null pointer dereference", currentPP)
          // remove null node
          val newValues = variableValues - NullNode
          (newValues, store + (variable -> newValues))
        } else (variableValues, store)

        // make sure all but the last field are not null
        val fields = path.tail.map(_.getName)
        val (receiverValues, newHeap) = fields.init.foldLeft((newVariableValues, heap)) { case ((oldReceivers, oldHeap), field) =>
          oldReceivers.foldLeft(Set.empty[HeapNode], oldHeap) { case ((currentReceivers, currentHeap), node) =>
            val fieldMap = currentHeap.getOrElse(node, Map.empty)
            val fieldValues = fieldMap.getOrElse(field, Set.empty)
            if (node.representsSingleVariable && (fieldValues contains NullNode)) {
              // report possible null pointer dereference
              Reporter.reportGenericWarning("Possible null pointer dereference", currentPP)
              // remove null node
              val newFieldValues = fieldValues - NullNode
              val newFieldMap = fieldMap + (field -> newFieldValues)
              val newHeap = currentHeap + (node -> newFieldMap)
              (currentReceivers ++ newFieldValues, newHeap)
            } else (currentReceivers ++ fieldValues, currentHeap)
          }
        }

        if (receiverValues.isEmpty && isMayAliasGraph) None
        else Some(copy(store = newStore, heap = newHeap))
      }
    case _ => Some(this)
  }

  /** Returns the default initial value for a variable or a field.
    *
    * For the may alias analysis, this is the set containing the null node and
    * the summary node. For the must alias analysis this is the set containing
    * the unknown node.
    *
    * @return The default initial value.
    */
  def initialValue(): Set[HeapNode] = this match {
    case _: MayAliasGraph => Set(UnknownNode, NullNode)
    case _: MustAliasGraph => Set(UnknownNode)
  }

  def defaultValue(): Set[HeapNode] = this match {
    case _: MayAliasGraph => Set(WildcardNode)
    case _: MustAliasGraph => Set(UnknownNode)
  }

  override def toString: String = {
    // edges representing the store
    val storeEdges = store.flatMap { case (variable, values) =>
      values.map { value => "\t\"" + variable + "\" -> \"o:" + value + "\"" }
    }.toList.sorted.mkString("\n")
    // edges representing the heap
    val heapEdges = heap.flatMap { case (node, map) =>
      map.flatMap { case (field, values) =>
        values.map { value => "\t\"o:" + node + "\" -> \"o:" + value + "\" [label= " + field + "]" }
      }
    }.toList.sorted.mkString("\n")

    s"digraph {\n$storeEdges\n$heapEdges\n}"
  }

  /** Copies the alias graph but updates the store and the heap if the
    * corresponding arguments are defined.
    *
    * @param store     The new store.
    * @param heap      The new heap.
    * @param fields    The set of fields in the program.
    * @param currentPP The current program point
    * @return The updated copy of the alias graph.
    */
  def copy(store: Store = store,
           heap: Heap = heap,
           fields: Set[String] = fields,
           currentPP: ProgramPoint = currentPP): T
}

object AliasGraph
{
  /** A graph representing may alias information.
    *
    * @param store     The store.
    * @param heap      The heap.
    * @param fields    The set of fields in the program.
    * @param currentPP The current program point.
    */
  case class MayAliasGraph(store: Store = Store(),
                           heap: Heap = Heap(),
                           fields: Set[String] = Set.empty,
                           currentPP: ProgramPoint = DummyProgramPoint)
    extends AliasGraph[MayAliasGraph]
  {
    /** Returns true if the given access paths may / must alias (depending on
      * whether this is a may or must alias graph).
      *
      * @param first  The first access path.
      * @param second The second access path.
      * @return True if the given access paths may / must alias.
      */
    override def pathsAlias(first: List[String], second: List[String]): Boolean = {
      def firstEval = evaluatePath(first) - NullNode
      def secondEval = evaluatePath(second) - NullNode
      def intersection = firstEval & secondEval
      intersection.nonEmpty || (firstEval contains WildcardNode) || (secondEval contains WildcardNode)
    }

    /** Returns the least upper bound of this and the other alias graph.
      *
      * @param other The other alias graph.
      * @return The least upper bound of this and the other alias graph.
      */
    override def lub(other: MayAliasGraph): MayAliasGraph = {
      // the function used to combine two field maps. the function can also
      // be used to combine two stores since they have the same type
      def combine(map1: FieldMap, map2: FieldMap): FieldMap = {
        val fields = map1.keySet ++ map2.keySet
        fields.foldLeft(FieldMap()) { (map, field) =>
          (map1.get(field), map2.get(field)) match {
            case (None, None) => map
            case (Some(values), None) => map + (field -> values)
            case (None, Some(values)) => map + (field -> values)
            case (Some(values1), Some(values2)) =>
              // we keep the unknown node only if there is no summary node
              val union = values1 ++ values2
              val values = if (union contains WildcardNode) union - UnknownNode else union
              map + (field -> values)
          }
        }
      }

      // combine stores
      val newStore = combine(store, other.store)

      // combine heap node-wise
      val nodes = heap.keySet ++ other.heap.keySet
      val newHeap = nodes.foldLeft(Heap()) { (heap, node) =>
        (this.heap.get(node), other.heap.get(node)) match {
          case (None, None) => heap
          case (Some(map), None) => heap + (node -> map)
          case (None, Some(map)) => heap + (node -> map)
          case (Some(map1), Some(map2)) => heap + (node -> combine(map1, map2))
        }
      }

      // update alias graph
      copy(store = newStore,
        heap = newHeap,
        fields = fields ++ other.fields,
        currentPP = DummyProgramPoint)
    }

    /** Returns the greatest lower bound of this and the other alias graph.
      *
      * @param other The other alias graph.
      * @return The greatest lower bound of this and the other alias graph.
      */
    override def glb(other: MayAliasGraph): MayAliasGraph = {
      // the function used to combine two field maps. the function can also
      // be used to combine two stores since they have the same type
      def combine(map1: FieldMap, map2: FieldMap): FieldMap = {
        val fields = map1.keySet ++ map2.keySet
        fields.foldLeft(FieldMap()) { (map, field) =>
          (map1.get(field), map2.get(field)) match {
            case (None, _) => map
            case (_, None )=> map
            case (Some(values1), Some(values2)) => map + (field -> (values1 & values2))
          }
        }
      }

      // combine stores
      val newStore = combine(store, other.store)

      // combine heap node-wise
      val nodes = heap.keySet ++ other.heap.keySet
      val newHeap = nodes.foldLeft(Heap()) { (heap, node) =>
        (this.heap.get(node), other.heap.get(node)) match {
          case (None, _) => heap
          case (_, None) => heap
          case (Some(map1), Some(map2)) => heap + (node -> combine(map1, map2))
        }
      }

      // update alias graph
      copy(store = newStore
        , heap = newHeap,
        fields = fields & other.fields,
        currentPP = DummyProgramPoint)
    }

    /** Returns true if this alias graph is less than or equal to the other alias
      * graph.
      *
      * @param other The other alias graph.
      * @return True if this alias graph is less than or equal to the other alias
      *         graph.
      */
    override def lessEqual(other: MayAliasGraph): Boolean = {
      // the function to compare two field maps. This function can also
      // be used to compare two stores since they have the same type
      def compare(map1: FieldMap, map2: FieldMap): Boolean = {
        map1.forall { case (field, values1) =>
          val values2 = map2.getOrElse(field, Set.empty)
          val otherWildcard = values2 contains WildcardNode
          val otherUnknown = otherWildcard || ((values2 contains UnknownNode) && !(values1 contains WildcardNode))
          val isSubset = otherUnknown || (values1 subsetOf values2)
          otherWildcard || otherUnknown || isSubset
        }
      }

      // compare stores
      val storeComparison = compare(store, other.store)

      // compare heaps node-wise
      val heapComparison = heap.forall { case (node, map1) =>
        val map2 = other.heap.getOrElse(node, FieldMap())
        compare(map1, map2)
      }

      storeComparison && heapComparison
    }

    /** Copies the alias graph but updates the store, the heap, and current
      * program point if the corresponding arguments are defined.
      *
      * @param store     The new store.
      * @param heap      The new heap.
      * @param fields    The set of fields in the program.
      * @param currentPP The current program point.
      * @return The updated copy of the alias graph.
      */
    override def copy(store: Store,
                      heap: Heap,
                      fields: Set[String],
                      currentPP: ProgramPoint): MayAliasGraph =
      MayAliasGraph(store, heap, fields, currentPP)
  }

  /** A graph representing must alias information.
    *
    * @param store     The store.
    * @param heap      The heap.
    * @param fields    The set of fields in the program
    * @param currentPP The current program point.
    */
  case class MustAliasGraph(store: Store = Store(),
                            heap: Heap = Heap(),
                            fields: Set[String] = Set.empty,
                            currentPP: ProgramPoint = DummyProgramPoint)
    extends AliasGraph[MustAliasGraph]
  {
    /** Returns true if the given access paths may / must alias (depending on
      * whether this is a may or must alias graph).
      *
      * @param first  The first access path.
      * @param second The second access path.
      * @return True if the given access paths may / must alias.
      */
    override def pathsAlias(first: List[String], second: List[String]): Boolean = {
      val leftEval = evaluatePath(first) -- Set(UnknownNode, WildcardNode)
      val rightEval = evaluatePath(second) -- Set(UnknownNode, WildcardNode)
      val intersection = leftEval & rightEval
      intersection.nonEmpty
    }

    /** Returns the least upper bound of this and the other alias graph.
      *
      * @param other The other alias graph.
      * @return The least upper bound of this and the other alias graph.
      */
    override def lub(other: MustAliasGraph): MustAliasGraph = {
      // the function used to combine two field maps. the function can also
      // be used to combine two stores since they have the same type
      def combine(map1: FieldMap, map2: FieldMap): FieldMap = {
        val fields = map1.keySet ++ map2.keySet
        fields.foldLeft(FieldMap()) { (map, field) =>
          (map1.get(field), map2.get(field)) match {
            case (None, _) => map
            case (_, None) => map
            case (Some(values1), Some(values2)) =>
              val intersection = values1 & values2
              val values = if (intersection.isEmpty) Set(UnknownNode: HeapNode) else intersection
              map + (field -> values)
          }
        }
      }
      // combine stores
      val newStore = combine(store, other.store)

      // combine heaps node-wise
      val nodes = heap.keySet ++ other.heap.keySet
      val newHeap = nodes.foldLeft(Heap()) { (heap, node) =>
        (this.heap.get(node), other.heap.get(node)) match {
          case (None, _) => heap
          case (_, None) => heap
          case (Some(map1), Some(map2)) => heap + (node -> combine(map1, map2))
        }
      }

      // update alias graph
      copy(store = newStore,
        heap = newHeap,
        fields = fields ++ other.fields,
        currentPP = DummyProgramPoint)
    }

    /** Returns the greatest lower bound of this and the other alias graph.
      *
      * @param other The other alias graph.
      * @return The greatest lower bound of this and the other alias graph.
      */
    override def glb(other: MustAliasGraph): MustAliasGraph = {
      // the function used to combine two field maps. the function can also
      // be used to combine two stores since they have the same type
      def combine(map1: FieldMap, map2: FieldMap): FieldMap = {
        val fields = map1.keySet ++ map2.keySet
        fields.foldLeft(FieldMap()) { (map, field) =>
          (map1.get(field), map2.get(field)) match {
            case (None, None) => map
            case (Some(values), None) => map + (field -> values)
            case (None, Some(values)) => map + (field -> values)
            case (Some(values1), Some(values2)) =>
              // we keep the unknown node only if it is present in both maps
              val union = values1 ++ values2
              val intersection = values1 & values2
              val values = (union - UnknownNode) ++ intersection
              map + (field -> values)
          }
        }
      }

      // combine stores
      val newStore = combine(store, other.store)

      // combine heaps node-wise
      val nodes = heap.keySet ++ other.heap.keySet
      val newHeap = nodes.foldLeft(Heap()) { (heap, node) =>
        (this.heap.get(node), other.heap.get(node)) match {
          case (None, None) => heap
          case (Some(map), None) => heap + (node -> map)
          case (None, Some(map)) => heap + (node -> map)
          case (Some(map1), Some(map2)) => heap + (node -> combine(map1, map2))
        }
      }

      // update alias graph
      copy(store = newStore,
        heap = newHeap,
        fields = fields & other.fields,
        currentPP = DummyProgramPoint)
    }

    /** Returns true if this alias graph is less than or equal to the other alias
      * graph.
      *
      * @param other The other alias graph.
      * @return True if this alias graph is less than or equal to the other alias
      *         graph.
      */
    override def lessEqual(other: MustAliasGraph): Boolean = {
      // the function to compare two field maps. This function can also
      // be used to compare two stores since they have the same type
      def compare(map1: FieldMap, map2: FieldMap): Boolean = {
        map2.forall { case (field, values2) =>
          val values1 = map1.getOrElse(field, Set.empty)
          val thisUnknown = values1 contains UnknownNode
          val isSuperset = thisUnknown || (values2 subsetOf values1)
          thisUnknown || isSuperset
        }
      }

      // compare stores
      val storeComparison = compare(store, other.store)

      // compare heaps node-wise
      val heapComparison = other.heap.forall { case (node, map2) =>
        val map1 = heap.getOrElse(node, FieldMap())
        compare(map1, map2)
      }

      storeComparison && heapComparison
    }

    /** Copies the alias graph but updates the store, the heap, and the current
      * program point if the corresponding arguments are defined.
      *
      * @param store     The new store.
      * @param heap      The new heap.
      * @param fields    The set of fields in the program.
      * @param currentPP The new current program point.
      * @return The updated copy of the alias graph.
      */
    override def copy(store: Store,
                      heap: Heap,
                      fields: Set[String],
                      currentPP: ProgramPoint): MustAliasGraph =
      MustAliasGraph(store, heap, fields, currentPP)
  }
}

/**
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait AliasAnalysisState[T <: AliasAnalysisState[T]]
  extends SimpleState[T]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging
{
  this: T =>

  /** Returns the current program point.
    *
    * @return The current program point.
    */
  def currentPP: ProgramPoint

  /** Returns true if materialization is allowed.
    *
    * @return True if materialization is allowed.
    */
  def materialization: Boolean

  /** Returns the result of the previous statement.
    *
    * @return The result of the previous statement.
    */
  def result: ExpressionSet

  /** Returns the graph representing the may alias information.
    *
    * @return The graph representing the may alias information.
    */
  def may: MayAliasGraph

  /** Returns the graph representing the must alias information.
    *
    * @return The graph representing the must alias information.
    */
  def must: MustAliasGraph

  /** Returns an alias analysis state instance.
    *
    * @return An alias analysis state instance.
    */
  override def factory(): T = top()

  /** Returns the alias analysis state representing the top element.
    *
    * @return The alias analysis state representing the top element.
    */
  override def top(): T = copy(isTop = true, isBottom = false)

  /** Returns the alias analysis state representing the bottom element.
    *
    * @return The alias analysis state representing the bottom element.
    */
  override def bottom(): T = copy(isTop = false, isBottom = true)

  /** Returns the least upper bound of this and the given other alias analysis
    * state.
    *
    * @param other The other alias analysis state.
    * @return The least upper bound of this and the given other alias analysis
    *         state.
    */
  override def lub(other: T): T = {
    logger.trace(s"lub($this, $other)")

    if (this.isTop || other.isBottom) this
    else if (this.isBottom || other.isTop) other
    else {
      // least upper bound of may and must alias graph
      val lubMay = may lub other.may
      val lubMust = must lub other.must
      // update state
      copy(
        currentPP = DummyProgramPoint,
        materialization = materialization || other.materialization,
        result = result lub other.result,
        may = lubMay,
        must = lubMust
      )
    }
  }

  /** Returns the greatest lower bound of this and the given other alias
    * analysis state.
    *
    * @param other The other alias analysis state.
    * @return The greatest lower bound of this and the given other alias
    *         analysis state.
    */
  override def glb(other: T): T = {
    logger.trace(s"glb($this, $other)")

    if (this.isBottom || other.isTop) this
    else if (this.isTop || other.isBottom) other
    else {
      // greatest lower bound of may and must alias graph
      val glbMay = may glb other.may
      val glbMust = must glb other.must
      // update state
      copy(
        currentPP = DummyProgramPoint,
        materialization = materialization && other.materialization,
        result = result glb other.result,
        may = glbMay,
        must = glbMust
      )
    }
  }

  /** Returns the widening of this and the given other alias analysis state.
    *
    * @param other The other alias analysis state.
    * @return The widening of this and the given other alias analysis state.
    */
  override def widening(other: T): T = {
    logger.trace(s"widening($this, $other)")

    if (this.isTop || other.isBottom) this
    else if (this.isBottom || other.isTop) other
    else {
      // widening (or rather lub) of may and must alias graph
      val widenMay = may lub other.may
      val widenMust = must lub other.must
      // update state
      copy(
        currentPP = DummyProgramPoint,
        materialization = false,
        result = result widening other.result,
        may = widenMay,
        must = widenMust
      )
    }
  }

  /** Returns true if this alias analysis state is less than or equal to the
    * given other alias analysis state.
    *
    * @param other The alias analysis state to compare against.
    * @return True if this alias analysis state is less than or equal to the
    *         given other alias analysis state.
    */
  override def lessEqual(other: T): Boolean = {
    logger.trace(s"lessEqual($other)")

    if (this.isBottom || other.isTop) true
    else if (this.isTop || other.isBottom) false
    else (may lessEqual other.may) && (must lessEqual other.must)
  }

  /** Adds the given variable to the alias analysis state.
    *
    * @param variable The variable to add.
    * @param typ      The type of the variable.
    * @param pp       The program point where the variable is created.
    * @return The alias analysis state with the variable added.
    */
  override def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    logger.trace(s"createVariable($variable, $typ, $pp)")

    if (typ.isObject) {
      // add variable to may and must alias graph
      val name = variable.name
      val newMay = may.addVariable(name)
      val newMust = must.addVariable(name)
      // new result
      val result = ExpressionSet(variable)
      // update result and may and must alias graphs
      copy(result = result, may = newMay, must = newMust)
    } else this
  }

  /** Adds the given variable for an argument to the alias analysis state.
    *
    * @param variable The variable to add.
    * @param typ      The type of the variable.
    * @return The alias analysis state with the variable added.
    */
  override def createVariableForArgument(variable: VariableIdentifier, typ: Type): T =
    createVariable(variable, typ, DummyProgramPoint)

  /** Creates an object
    *
    * @param typ The type of the object to create.
    * @param pp  The program point where the program is created.
    * @return The alias analysis state with the object created.
    */
  override def createObject(typ: Type, pp: ProgramPoint): T = {
    logger.trace(s"createObject($typ, $pp)")

    // add node to may and must alias graphs
    val node = NewNode
    val newMay = may.addNode(node.name)
    val newMust = must.addNode(node.name)
    // new result
    val result = ExpressionSet(node)
    // update result and may and must alias graphs
    copy(result = result, may = newMay, must = newMust)
  }

  /** Removes the given variable from the alias analysis state.
    *
    * @param variable The variable to remove.
    * @return The alias analysis state with the variable removed
    */
  override def removeVariable(variable: VariableIdentifier): T = {
    logger.trace(s"removeVariable($variable)")

    // remove variable from may and must alias graph
    val newMay = may.removeVariable(variable.name)
    val newMust = must.removeVariable(variable.name)
    // update may and must alias graphs
    copy(may = newMay, must = newMust)
  }

  /** Removes all variables satisfying the given predicate.
    *
    * @param filter The filter predicate.
    * @return The alias analysis state with the variables removed.
    */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = {
    logger.trace(s"pruneVariables($filter)")

    // remove variables satisfying the predicate from may and must alias graph
    val nameFilter: String => Boolean = name => filter(VariableIdentifier(name)(DummyRefType))
    val newMay = may.pruneVariables(nameFilter)
    val newMust = must.pruneVariables(nameFilter)
    // update may and must alias graphs
    copy(may = newMay, must = newMust)
  }

  override def command(cmd: Command): T = cmd match {
    case InhaleCommand(expression) => inhale(expression.getSingle.get)
    case ExhaleCommand(expression) => exhale(expression.getSingle.get)
    case PreconditionCommand(condition) => command(InhaleCommand(condition))
    case PostconditionCommand(condition) => command(ExhaleCommand(condition))
    case InvariantCommand(condition) => command(ExhaleCommand(condition)).command(InhaleCommand(condition))
    case _ => super.command(cmd)
  }

  /** Inhales the given expression.
    *
    * @param expression The expression to inhale.
    * @return The alias analysis state with the given expression inhaled.
    */
  def inhale(expression: Expression): T ={
    logger.trace(s"inhale($expression)")

    assume(expression)
  }

  /** Exhales the given expression.
    *
    * @param expression The expression to exhale.
    * @return The alias analysis state with the given expression exhaled.
    */
  def exhale(expression: Expression): T = {
    logger.trace(s"exhale($expression)")

    expression match {
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, _) => exhale(left).exhale(right)
      case PermissionExpression(location, _, _) => location match {
        case AccessPathIdentifier(accessPath) =>
          // TODO: Only assume receivers to be non null if permission is > none
          val path = accessPath.map(_.getName)
          val newMay = may
            .assumeNonNullReceiver(location)
            .map(_.havoc(path))
            .map(_.pruneUnreachableNodes())
          val newMust = must
            .assumeNonNullReceiver(location)
            .map(_.havoc(path))
            .map(_.pruneUnreachableNodes())
          if (newMay.isEmpty || newMust.isEmpty) bottom()
          else copy(may = newMay.get, must = newMust.get)
        case _ => throw new IllegalArgumentException("An exhale of a permission must occur via an access path identifier.")
      }
      case _ if expression.typ.isBooleanType => this
      case _ => throw new IllegalArgumentException("An exhale must occur via a boolean expression or a permission expression")
    }
  }

  /** Assumes the given expression.
    *
    * @param expression The expression to assume.
    * @return The alias analysis with the given expression assumed.
    */
  override def assume(expression: Expression): T = {
    logger.trace(s"assume($expression)")

    // TODO: What if the state is top?

    expression match {
      case Constant("true", _, _) => this
      case Constant("false", _, _) => bottom()
      case _: Identifier => this
      case _: BinaryArithmeticExpression => this
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, _) => assume(left).assume(right)
      case BinaryBooleanExpression(left, right, BooleanOperator.||, _) => assume(left) lub assume(right)
      case negation: NegatedBooleanExpression => negation.exp match {
        case Constant("true", _, _) => bottom()
        case Constant("false", _, _) => this
        case _: Identifier => this
        case _: BinaryArithmeticExpression => this
        case BinaryBooleanExpression(left, right, operator, typ) =>
          // negate expression by applying de morgan's law
          val negatedLeft = NegatedBooleanExpression(left)
          val negatedRight = NegatedBooleanExpression(right)
          val negatedOperator = BooleanOperator.negate(operator)
          val negated = BinaryBooleanExpression(negatedLeft, negatedRight, negatedOperator, typ)
          assume(negated)
        case NegatedBooleanExpression(doublyNegated) => assume(doublyNegated)
        case ReferenceComparisonExpression(left, right, operator, typ) =>
          // negate expression by negating operator
          val negatedOperator = ArithmeticOperator.negate(operator)
          val negated = ReferenceComparisonExpression(left, right, negatedOperator, typ)
          assume(negated)
        case _ => throw new NotImplementedError("An assume implementation is missing.")
      }
      case comparison: ReferenceComparisonExpression =>
        // assume comparison in may and must alias graph
        val left = comparison.left
        val right = comparison.right
        val operator = comparison.op
        val assumedMay = may
          .materialize(left)
          .materialize(right)
          .assumeComparison(left, right, operator)
        val assumedMust = must
          .materialize(left)
          .materialize(right)
          .assumeComparison(left, right, operator)
        // update state
        if (assumedMay.isEmpty || assumedMust.isEmpty) bottom()
        else copy(may = assumedMay.get, must = assumedMust.get).pruneUnreachableHeap()
      case PermissionExpression(location, _, _) =>
        // TODO: If the amount of the permission is not guaranteed to be positive, do nothing
        // assume that receiver of the location is non null
        val assumedMay = may.assumeNonNullReceiver(location)
        val assumedMust = must.assumeNonNullReceiver(location)
        // update state
        if (assumedMay.isEmpty || assumedMust.isEmpty) bottom()
        else copy(may = assumedMay.get, must = assumedMust.get).pruneUnreachableHeap()
      case _ => throw new NotImplementedError("An assume implementation is missing.")
    }
  }

  /** Evaluates the given constant.
    *
    * @param value The value of the constant.
    * @param typ   The type of the constant.
    * @param pp    The program point where the constant is created.
    * @return The alias analysis state with the result set to the evaluated
    *         constant.
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    logger.trace(s"evalConstant($value, $typ, $pp)")

    // update result
    val result = ExpressionSet(Constant(value, typ, pp))
    copy(result = result)
  }

  /** Sets the current expression to the expression representing the given
    * variable.
    *
    * @param identifier The variable.
    * @return The updated alias analysis state.
    */
  override def getVariableValue(identifier: Identifier): T = {
    logger.trace(s"getVariableValue($identifier)")

    // access path
    val accessPath = identifier match {
      case variable: VariableIdentifier => variable :: Nil
      case _ => throw new IllegalArgumentException("A variable access must occur via a variable identifier.")
    }

    // materialize path
    val path = accessPath.map(_.getName)
    val updatedMay = if (materialization) may.materialize(path) else may
    val updatedMust = if (materialization) must.materialize(path) else must

    // new current expression
    val result = ExpressionSet(identifier)
    // update state
    copy(result = result, may = updatedMay, must = updatedMust)
  }

  /** Sets the current expression to the expression representing the value of
    * given field.
    *
    * @param receiver The receiver of the field.
    * @param field    The name of the field.
    * @param typ      The type of the field.
    * @return The updated alias analysis state.
    */
  override def getFieldValue(receiver: Expression, field: String, typ: Type): T = {
    logger.trace(s"getFieldValue($receiver, $field, $typ)")

    // full access path
    val accessPath = receiver match {
      case variable: VariableIdentifier => variable :: VariableIdentifier(field)(typ) :: Nil
      case AccessPathIdentifier(receiverPath) => receiverPath :+ VariableIdentifier(field)(typ)
      case _ => throw new IllegalArgumentException("A field access must occur via an identifier.")
    }
    // materialize path
    val path = accessPath.map(_.getName)
    val updatedMay = if (materialization) this.may.materialize(path) else may
    val updatedMust = if (materialization) must.materialize(path) else must

    // new current expression
    val result = ExpressionSet(AccessPathIdentifier(accessPath))
    // update state
    copy(result = result, may = updatedMay, must = updatedMust)
  }

  /** Assigns the given expression to the given variable.
    *
    * @param target The assigned variable.
    * @param value  The assigned expression.
    * @return The alias analysis state after the variable assignment.
    */
  override def assignVariable(target: Expression, value: Expression): T = {
    logger.trace(s"assignVariable($target, $value)")

    target match {
      case _: VariableIdentifier =>
        // assume value contains no null receivers
        val accessedMay = may.assumeNonNullReceiver(value)
        val accessedMust = must.assumeNonNullReceiver(value)
        if (accessedMay.isEmpty || accessedMust.isEmpty) bottom()
        else {
          // perform assignment in may and must alias graph
          val assignedMay = accessedMay.get.assign(target, value)
          val assignedMust = accessedMust.get.assign(target, value)
          // update state
          copy(may = assignedMay, must = assignedMust).pruneUnreachableHeap()
        }
      case _ => throw new IllegalArgumentException("A variable assignment must occur via a variable identifier.")
    }
  }

  /** Assigns the given expression to the given field of the given receiver.
    *
    * @param target The target of the assignment.
    * @param field  The assigned field.
    * @param value  The assigned value.
    * @return The alias analysis state after the field assignment.
    */
  override def assignField(target: Expression, field: String, value: Expression): T = {
    logger.trace(s"assignField($target, $field, $value)")

    target match {
      case _: AccessPathIdentifier =>
        // assume target and value contain no null receivers
        val nonNullMay = may.assumeNonNullReceiver(target).flatMap(_.assumeNonNullReceiver(value))
        val nonNullMust = must.assumeNonNullReceiver(target).flatMap(_.assumeNonNullReceiver(value))
        if (nonNullMay.isEmpty || nonNullMust.isEmpty) bottom()
        else {
          // perform assignment in may and must alias graph
          val assignedMay = nonNullMay.get.assign(target, value)
          val assignedMust = nonNullMust.get.assign(target, value)
          // update state
          copy(may = assignedMay, must = assignedMust).pruneUnreachableHeap()
        }
      case _ => throw new IllegalArgumentException("A field assignment must occur via an access path identifier.")
    }
  }

  /** Forgets the value of the given variable.
    *
    * @param variable The variable to forget.
    * @return The alias analysis state after the variable was forgotten.
    */
  override def setVariableToTop(variable: Expression): T = {
    logger.trace(s"setVariableToTpo($variable)")

    variable match {
      case VariableIdentifier(variable, _) =>
        val path = variable :: Nil
        val newMay = may.havoc(path)
        val newMust = must.havoc(path)
        copy(may = newMay, must = newMust)
      case _ => throw new IllegalArgumentException("Argument must be a variable identifier.")
    }
  }

  /** Assigns an expression to an argument.
    *
    * @param argument     The assigned argument
    * @param expression The expression to be assigned
    * @return The abstract state after the assignment
    */
  override def setArgument(argument: ExpressionSet, expression: ExpressionSet): T = ???

  /** Performs an abstract garbage collection by pruning all unreachable nodes.
    *
    * @return The updated alias analysis state.
    */
  override def pruneUnreachableHeap(): T = {
    logger.trace("pruneUnreachableHeap")

    // prune unreachable nodes in may and must alias graph
    val prunedMay = may.pruneUnreachableNodes()
    val prunedMust = must.pruneUnreachableNodes()
    // update state
    copy(may = prunedMay, must = prunedMust)
  }

  /** Sets the current program point to the given program point.
    *
    * @param pp The program point to be set.
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): T = {
    logger.trace(s"before($pp)")

    // set program point
    copy(currentPP = pp, may = may.before(pp), must = must.before(pp))
  }

  /** Sets the current expression.
    *
    * @param expression The current expression
    * @return The alias analysis state with the updated current expression.
    */
  override def setExpression(expression: ExpressionSet): T =  copy(result = expression)

  /** Removes the current expression.
    *
    * @return The alias analysis state with the current expression removed.
    */
  override def removeExpression(): T = copy(result = ExpressionSet())

  /** Returns the current expression.
    *
    * @return The current expression.
    */
  override def expr: ExpressionSet = result

  override def throws(expression: ExpressionSet): T = ???

  /** Returns true if the two given access paths may alias.
    *
    * @param first  The first access path.
    * @param second The second access path.
    * @return True if the two given access paths may alias.
    */
  def pathsMayAlias(first: AccessPath, second: AccessPath): Boolean = {
    val firstPath = first.map(_.getName)
    val secondPath = second.map(_.getName)
    may.pathsAlias(firstPath, secondPath)
  }

  /** Returns true if the two given access paths must alias.
    *
    * @param first  The first access path.
    * @param second The second access path.
    * @return True if the two given access paths must alias.
    */
  def pathsMustAlias(first: AccessPath, second: AccessPath): Boolean = {
    val firstPath = first.map(_.getName)
    val secondPath = second.map(_.getName)
    must.pathsAlias(firstPath, secondPath)
  }

  override def toString =
    s"MAY ALIAS GRAPH:\n$may\n" +
    s"MUST ALIAS GRAPH:\n$must"

  /** Copies the alias analysis state but updates fields, current program point,
    * materialization flag, result, may alias graph, must alias graph, top flag,
    * and bottom flag if the corresponding arguments are defined.
    *
    * @param currentPP       The new current program point.
    * @param materialization The new materialization flag.
    * @param result          The new result.
    * @param may             The new may alias graph.
    * @param must            The new must alias graph.
    * @param isTop           The new top flag.
    * @param isBottom        The new bottom flag.
    * @return The updated copy of the alias analysis state.
    */
  def copy(currentPP: ProgramPoint = currentPP,
           materialization: Boolean = materialization,
           result: ExpressionSet = result,
           may: MayAliasGraph = may,
           must: MustAliasGraph = must,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): T
}

object AliasAnalysisState
{
  /** The default implementation of the alias analysis state.
    *
    * @param currentPP       The current program point.
    * @param materialization The materialization flag.
    * @param result          The result.
    * @param may             The may alias graph.
    * @param must            The must alias graph.
    * @param isTop           The top flag.
    * @param isBottom        The bottom flag.
    */
  case class Default(currentPP: ProgramPoint = DummyProgramPoint,
                     materialization: Boolean = true,
                     result: ExpressionSet = ExpressionSet(),
                     may: MayAliasGraph = MayAliasGraph(),
                     must: MustAliasGraph = MustAliasGraph(),
                     isTop: Boolean = false,
                     isBottom: Boolean = false)
  extends AliasAnalysisState[Default]
  {
    /** Copies the alias analysis state but updates fields, current program point,
      * materialization flag, result, may alias graph, must alias graph, top flag,
      * and bottom flag if the corresponding arguments are defined.
      *
      * @param currentPP       The new current program point.
      * @param materialization The new materialization flag.
      * @param result          The new result.
      * @param may             The new may alias graph.
      * @param must            The new must alias graph.
      * @param isTop           The new top flag.
      * @param isBottom        The new bottom flag.
      * @return The updated copy of the alias analysis state.
      */
    override def copy(currentPP: ProgramPoint,
                      materialization: Boolean,
                      result: ExpressionSet,
                      may: MayAliasGraph,
                      must: MustAliasGraph,
                      isTop: Boolean,
                      isBottom: Boolean): Default =
      Default(currentPP, materialization, result, may, must, isTop, isBottom)
  }
}

/** An entry state builder of the alias analysis.
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait AliasAnalysisStateBuilder[T <: AliasAnalysisState[T]]
  extends ForwardEntryStateBuilder[T]
{
  override def build(method: MethodDeclaration): T = {
    // retrieve the set of fields declared in the program
    val fields = method.classDef.fields
      .filter(_.typ.isObject)
      .map(_.variable.toString)
      .toSet

    val may = MayAliasGraph().initialize(fields)
    val must = MustAliasGraph().initialize(fields)
    val initial = topState.copy(may = may, must = must)

    method.initializeArgument(initial)
  }
}

/** The entry state for the alias analysis.
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
object AliasAnalysisEntryState
  extends AliasAnalysisStateBuilder[Default]
{
  override def topState: Default = Default()
}

/** An alias analysis runner.
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait AliasAnalysisRunner[T <: AliasAnalysisState[T]]
  extends SilverAnalysisRunner[T]
{
  /** Runs the analysis on the Silver program whose name is passed as first
    * argument and reports errors and warnings.
    *
    * @param arguments The arguments to the runner.
    */
  override def main(arguments: Array[String]): Unit = {
    // check whether there is a first argument (the path to the file)
    if (arguments.isEmpty) throw new IllegalArgumentException("No file specified")

    // run analysis
    val path = new File(arguments(0)).toPath
    val results = run(path)

    println("\n*******************\n* Analysis Result *\n*******************\n")
    val cfgStates = results.map(result => result.method.name.toString -> result.cfgState).toMap
    for ((method, graph) <- cfgStates) {
      println("******************* " + method + "\n")
      // printing the entry state of the control-flow graph
      println(graph.entryState())

      // blocks withing the control-flow graph
      val blocks = graph.cfg.nodes

      // withing each block...
      var i = 0
      for (statements <- blocks) {
        if (statements.isEmpty) {
          // post-states of each statement
          val states = graph.blockStates(i).last
          for (s <- states) {
            println("\n******************* \n")
            println(s)
          }
        } else {
          // printing the block pre-state
          println("\n+++++++++++++++++++ BLOCK " + i + "+++++++++++++++++++\n")
          println(graph.blockStates(i).last.head)
          // post-states of each statement
          val states = graph.blockStates(i).last.drop(1)
          // print statements and corresponding post-states
          for ((c: Statement, s) <- statements zip states) {
            println("\n******************* " + c + "\n")
            println(s)
          }
        }
        i = i + 1
      }

      println("\n******************* \n")
      println(graph.exitState()) // printing the exit state of the control-flow graph
    }
  }

  override def toString: String = "Alias Analysis"
}

/** The alias analysis.
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
object AliasAnalysis
  extends AliasAnalysisRunner[Default]
{
  override val analysis: Analysis[Default] = SimpleForwardAnalysis[Default](AliasAnalysisEntryState)

  override def toString: String = "Alias Analysis"
}
