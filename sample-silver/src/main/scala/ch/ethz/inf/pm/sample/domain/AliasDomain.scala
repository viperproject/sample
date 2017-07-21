/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.domain.AliasGraph.{Heap, Store}
import ch.ethz.inf.pm.sample.domain.HeapNode._
import ch.ethz.inf.pm.sample.domain.util.Replacements
import ch.ethz.inf.pm.sample.oorepresentation.silver.RefType
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}

/**
  * A domain that keeps track of alias information.
  *
  * @tparam T The type of the elements of the alias domain.
  * @tparam I The type of the identifiers used by the alias domain.
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait AliasDomain[T <: AliasDomain[T, I], I <: Identifier]
  extends HeapDomain[T, I] {
  this: T =>

  /**
    * Creates an element of the alias domain with the given fields.
    *
    * @param fields The fields
    * @return The alias graph.
    */
  def factory(fields: Seq[Identifier]): T

  /**
    * Returns whether the locations corresponding to the two given expressions
    * may alias.
    *
    * When in doubt this method returns true.
    *
    * @param left  The first expression.
    * @param right The second expression.
    * @return True if the locations may alias.
    */
  def mayAlias(left: Expression, right: Expression): Boolean

  /**
    * Returns whether the locations corresponding to the two given expressions
    * must alias.
    *
    * @param left  The first expression.
    * @param right The second expression.
    * @return True if the locations must alias.
    */
  def mustAlias(left: Expression, right: Expression): Boolean
}

/**
  * A heap node used by the alias analysis to represent heap locations in the
  * alias graph.
  *
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait HeapNode extends Identifier.HeapIdentifier {
  override def getField: Option[String] = None

  override def pp: ProgramPoint = DummyProgramPoint

  override def typ: Type = RefType()
}

object HeapNode {

  /**
    * The top heap node that represents all possible heap locations.
    */
  case object TopNode extends HeapNode {
    override def getName: String = "⊤"

    override def representsSingleVariable: Boolean = false
  }

  /**
    * The unknown node that represents all heap locations that are not
    * materialized.
    */
  case object UnknownNode extends HeapNode {
    override def getName: String = "?"

    override def representsSingleVariable: Boolean = false
  }

  /**
    * The summary node.
    */
  case object SummaryNode extends HeapNode {
    override def getName: String = "Σ"

    override def representsSingleVariable: Boolean = false
  }

  /**
    * The null node that represents the null value.
    */
  case object NullNode extends HeapNode {
    override def getName: String = "null"

    override def representsSingleVariable: Boolean = false
  }

  /**
    * The new node that represents a newly created heap location.
    */
  case object NewNode extends HeapNode {
    override def getName: String = "new"

    override def representsSingleVariable: Boolean = true
  }

  /**
    * A heap node representing a single heap location.
    *
    * @param name The name of the heap location.
    */
  case class SimpleNode(name: String) extends HeapNode {
    override def getName: String = name

    override def representsSingleVariable: Boolean = true
  }

}

/**
  * A graph representing alias information.
  *
  * @tparam T The type of the alias graph.
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
trait AliasGraph[T <: AliasGraph[T]]
  extends AliasDomain[T, HeapNode] {
  this: T =>

  /**
    * Returns the map representing the store of the alias graph.
    *
    * @return The store.
    */
  def store: Store

  /**
    * Returns the map representing the heap of the alias graph.
    *
    * @return The heap.
    */
  def heap: Heap

  /**
    * The flag indicating whether materialization is allowed.
    *
    * @return The materialization flag.
    */
  def materialization: Boolean

  def factory(fields: Seq[Identifier]): T = {
    // prepare field map
    val map: Store = fields.foldLeft(Map.empty: Store) {
      case (map, field) => map + (field -> initialValues)
    }
    // initialize heap
    val nodes = initialValues - NullNode
    val heap: Heap = nodes.foldLeft(Map.empty: Heap) {
      case (heap, node) => heap + (node -> map)
    }
    // create alias graph
    copy(
      isTop = false,
      isBottom = false,
      store = store,
      heap = heap
    )
  }

  override def factory(): T = top()

  override def top(): T = copy(
    isTop = true,
    isBottom = false,
    store = Map.empty,
    heap = Map.empty
  )

  override def bottom(): T = copy(
    isTop = false,
    isBottom = true,
    store = Map.empty,
    heap = Map.empty
  )

  override def addVariable(variable: VariableIdentifier): (T, Replacement) = {
    // create variable
    val newStore = store + (variable -> initialValues)
    // return updated domain and replacement
    val updated = copy(store = newStore)
    val replacement = Replacements.identity
    (updated, replacement)
  }

  override def removeVariable(variable: VariableIdentifier): (T, Replacement) = {
    // remove variable
    val newStore = store - variable
    // return updated domain and replacement
    val updated = copy(store = newStore)
    val replacement = Replacements.identity
    (updated, replacement)
  }

  override def assignVariable(variable: VariableIdentifier, expression: Expression): (T, Replacement) = {
    // evaluate value
    val (domain, values, replacement) = evaluate(expression)
    // update store
    val newStore = domain.store + (variable -> values)
    // return updated domain and replacement
    val updated = domain.copy(store = newStore)
    (updated, replacement)
  }

  override def assignField(target: AccessPathIdentifier, expression: Expression): (T, Replacement) = {
    // get receiver and field
    val receiver = AccessPathIdentifier(target.path.init)
    val field = target.path.last
    // evaluate receiver and value
    val (domain1, receivers, replacement1) = evaluateReceiver(receiver)
    val (domain2, values, replacement2) = domain1.evaluate(expression)
    // determine whether a strong update is possible
    val strong = strongUpdate(receivers) || (values contains TopNode)
    // update heap
    val newHeap = receivers.foldLeft(domain2.heap) {
      case (heap, receiver) =>
        val map = heap.getOrElse(receiver, Map.empty)
        val newValue = if (strong) values else values ++ map.getOrElse(field, Set.empty)
        val newMap = map + (field -> newValue)
        heap + (receiver -> newMap)
    }
    // return updated heap and replacement
    val updated = domain2.copy(heap = newHeap)
    val replacement = replacement1 >> replacement2

    (updated, replacement)
  }

  override def havoc(expression: Expression): (T, Replacement) = expression match {
    case variable: VariableIdentifier => assignVariable(variable, TopNode)
    case target: AccessPathIdentifier => assignField(target, TopNode)
  }

  override def inhale(condition: Expression): (T, Replacement) = condition match {
    case Constant("true", _, _) => (this, Replacements.identity)
    case Constant("false", _, _) => (bottom(), Replacements.identity)
    case _: Identifier => (this, Replacements.identity)
    case _: BinaryArithmeticExpression => (this, Replacements.identity)
    case NegatedBooleanExpression(argument) => argument match {
      case Constant("true", typ, pp) => inhale(Constant("false", typ, pp))
      case Constant("false", typ, pp) => inhale(Constant("true", typ, pp))
      case NegatedBooleanExpression(doublyNegated) => inhale(doublyNegated)
      case BinaryBooleanExpression(left, right, operator) =>
        val negatedLeft = NegatedBooleanExpression(left)
        val negatedRight = NegatedBooleanExpression(right)
        val negatedOperator = BooleanOperator.negate(operator)
        val negated = BinaryBooleanExpression(negatedLeft, negatedRight, negatedOperator)
        inhale(negated)
      case ReferenceComparisonExpression(left, right, operator) =>
        val negatedOperator = ReferenceOperator.negate(operator)
        val negated = ReferenceComparisonExpression(left, right, negatedOperator)
        inhale(negated)
      case _ => inhale(argument)
    }
    case BinaryBooleanExpression(left, right, BooleanOperator.&&) =>
      val (domain, replacement1) = inhale(left)
      val (updated, replacement2) = domain.inhale(right)
      val replacement = replacement1 >> replacement2
      (updated, replacement)
    case BinaryBooleanExpression(left, right, BooleanOperator.||) =>
      val (domain1, replacement1) = inhale(left)
      val (domain2, replacement2) = inhale(right)
      val updated = domain1 lub domain2
      val replacement = replacement1 lub replacement2
      (updated, replacement)
    case comparison: ReferenceComparisonExpression =>
      assumeComparison(comparison)
    case FieldAccessPredicate(location, _, _, _) =>
      // evaluate location to trigger its materialization
      val (updated, _, replacement) = evaluate(location)
      (updated, replacement)
    case _ => ???
  }

  override def exhale(condition: Expression): (T, Replacement) = condition match {
    case BinaryBooleanExpression(left, right, BooleanOperator.&&) =>
      val (domain, replacement1) = exhale(left)
      val (updated, replacement2) = domain.exhale(right)
      val replacement = replacement1 >> replacement2
      (updated, replacement)
    case FieldAccessPredicate(location, _, _, _) =>
      havoc(location)
    case _ => ???
  }

  override def garbageCollect(): (T, Replacement) = {
    // nodes reachable from variables plus unknown node
    var reachable = store.values.foldLeft(Set(UnknownNode: HeapNode))(_ ++ _)
    // nodes reachable via field accesses
    var size = 0
    while (size < reachable.size) {
      size = reachable.size
      reachable ++= reachable.foldLeft(Set.empty[HeapNode]) { (set, node) =>
        val map = heap.getOrElse(node, Map.empty)
        val values = map.values.foldLeft(Set.empty[HeapNode])(_ ++ _)
        set ++ values
      }
    }
    // remove all unreachable nodes from the heap
    val unreachable = heap.keySet -- reachable
    val newHeap = unreachable.foldLeft(heap) { case (result, node) => result - node }
    // return updated domain and replacement
    val updated = copy(heap = newHeap)
    val replacement = Replacements.remove(unreachable.collect { case id: Identifier => id })
    (updated, replacement)
  }

  override def getValue(expression: Expression): Set[HeapNode] = {
    val (_, values, _) = copy(materialization = true).evaluate(expression)
    values
  }

  protected def initialValues: Set[HeapNode]

  protected def topValues: Set[HeapNode]

  protected def strongUpdate(target: Set[HeapNode]): Boolean

  /**
    * Evaluates the given expression.
    *
    * This method materializes variable and field accesses if the
    * materialization flag is set.
    *
    * @param expression The expression to be evaluated.
    * @return The updated domain, values, and corresponding replacement.
    */
  protected def evaluate(expression: Expression): (T, Set[HeapNode], Replacement) = expression match {
    case TopNode =>
      val values = Set(TopNode: HeapNode)
      val replacement = Replacements.identity
      (this, values, replacement)
    case NewNode =>
      val values = Set(UnknownNode: HeapNode)
      val replacement = Replacements.identity
      (this, values, replacement)
    case Constant("null", _, _) =>
      val values = Set(NullNode: HeapNode)
      val replacement = Replacements.identity
      (this, values, replacement)
    case variable: VariableIdentifier =>
      evaluateVariable(variable)
    case AccessPathIdentifier(path) =>
      val receiver = AccessPathIdentifier(path.init)
      val field = path.last
      val (domain, receivers, replacement1) = evaluateReceiver(receiver)
      val (updated, values, replacement2) = domain.evaluateField(receivers, field)
      val replacement = replacement1 >> replacement2
      (updated, values, replacement)
    case _ => ???
  }

  /**
    * Evaluates the receivers and values of the given expression.
    *
    * @param expression The expression.
    * @return The updated domain, receivers, values, and corresponding replacement.
    */
  def evaluateWithReceiver(expression: Expression): (T, Set[HeapNode], Set[HeapNode], Replacement) = expression match {
    case AccessPathIdentifier(path) =>
      val receiver = AccessPathIdentifier(path.init)
      val field = path.last
      val (domain, receivers, replacement1) = evaluateReceiver(receiver)
      val (updated, values, replacement2) = domain.evaluateField(receivers, field)
      val replacement = replacement1 >> replacement2
      (updated, receivers, values, replacement)
    case _ =>
      val receiver = Set.empty[HeapNode]
      val (updated, value, replacement) = evaluate(expression)
      (updated, receiver, value, replacement)
  }

  /**
    * Evaluates the given expression as a receiver.
    *
    * This method materializes variable and field accesses if the
    * materialization flag is set.
    *
    * @param expression The expression to be evaluated.
    * @return The updated domain, value, and corresponding replacement.
    */
  protected def evaluateReceiver(expression: Expression): (T, Set[HeapNode], Replacement) = expression match {
    case variable: VariableIdentifier =>
      assumeNonNull(variable).evaluateVariable(variable)
    case AccessPathIdentifier(variable :: fields) =>
      fields.foldLeft(assumeNonNull(variable).evaluateVariable(variable)) { case ((domain, receivers, replacement), field) =>
        val nonNull = receivers.foldLeft(domain) { (x, receiver) => x.assumeNonNull(receiver, field) }
        nonNull.evaluateField(receivers, field)
      }
  }

  /**
    * Evaluates the given variable.
    *
    * This method materializes the variable if the materialization flag is set.
    *
    * @param identifier The variable to be evaluated.
    * @return The updated domain, value, and corresponding replacement
    */
  protected def evaluateVariable(identifier: Identifier): (T, Set[HeapNode], Replacement) = {
    // get value of the variable
    val values = store.getOrElse(identifier, Set.empty)
    // materialize
    if ((values contains TopNode) && materialization) {
      // update store
      val result = topValues
      val newStore = store + (identifier -> result)
      // return updated domain, value, and replacement
      val updated = copy(store = newStore)
      (updated, result, Replacements.identity)
    } else if ((values contains UnknownNode) && materialization) {
      // create heap node
      val node = SimpleNode(identifier.getName)
      // merge maps of unknown node and existing node
      val map1 = heap(UnknownNode)
      val merged = heap.get(node) match {
        case Some(map2) => mergeStores(map1, map2, _ ++ _)
        case None => map1
      }
      // update store and heap
      val result = values - UnknownNode + node
      val newStore = store + (identifier -> result)
      val newHeap = heap + (node -> merged)
      // return updated domain, value, and replacement
      val updated = copy(store = newStore, heap = newHeap)
      val replacement = Replacements.expand(UnknownNode, Set(UnknownNode, node))
      (updated, result, replacement)
    } else {
      // return unchanged domain, value, and replacement
      (this, values, Replacements.identity)
    }
  }

  /**
    * Evaluates the field access with the given receivers and field.
    *
    * @param receivers The set of receivers.
    * @param field     The field.
    * @return The updated domain, value, and corresponding replacement.
    */
  protected def evaluateField(receivers: Set[HeapNode], field: Identifier): (T, Set[HeapNode], Replacement) = {
    receivers.foldLeft((this, Set.empty: Set[HeapNode], Replacements.identity)) { case ((domain, result, replacement1), receiver) =>
      val (updated, values, replacement2) = domain.evaluateField(receiver, field)
      (updated, result ++ values, replacement1 >> replacement2)
    }
  }

  /**
    * Evaluates the field access with the given receiver and field.
    *
    * @param receiver The receiver.
    * @param field    The field.
    * @return The updated domain, value, and corresponding replacement.
    */
  protected def evaluateField(receiver: HeapNode, field: Identifier): (T, Set[HeapNode], Replacement) = {
    // get value of the field
    val map = heap.getOrElse(receiver, Map.empty)
    val values = map.getOrElse(field, Set.empty)
    // materialize
    if ((values contains TopNode) && materialization) {
      // update heap
      val result = topValues
      val newMap = map + (field -> result)
      val newHeap = heap + (receiver -> newMap)
      // return updated domain, value, and replacement
      val updated = copy(heap = newHeap)
      (updated, result, Replacements.identity)
    } else if (values contains UnknownNode) {
      if (materialization) {
        // create heap node
        val node = SimpleNode(s"$receiver.$field")
        // merge maps of unknown node and existing node
        val map1 = heap(UnknownNode)
        val merged = heap.get(node) match {
          case Some(map2) => mergeStores(map1, map2, _ ++ _)
          case None => map1
        }
        // update heap
        val result = values - UnknownNode + node
        val newMap = map + (field -> result)
        val newHeap = heap + (receiver -> newMap) + (node -> merged)
        // return updated domain, value, and replacement
        val updated = copy(heap = newHeap)
        val replacement = Replacements.expand(UnknownNode, Set(UnknownNode, node))
        (updated, result, replacement)
      } else {
        // add summary node if it does not exist
        val summaryHeap = heap.get(SummaryNode) match {
          case Some(_) => heap
          case None =>
            val fields = heap(UnknownNode).keySet
            val summaryMap = fields.foldLeft(Map.empty: Store) {
              case (map, field) => map + (field -> Set(SummaryNode))
            }
            heap + (SummaryNode -> summaryMap)
        }
        // update heap
        val result = values - UnknownNode + SummaryNode
        val newMap = map + (field -> result)
        val newHeap = summaryHeap + (receiver -> newMap)
        // return updated domain, value, and replacement
        val updated = copy(heap = newHeap)
        val replacement = Replacements.expand(UnknownNode, Set(UnknownNode, SummaryNode))
        (updated, result, replacement)
      }
    } else {
      // return unchanged domain, value, and replacement
      (this, values, Replacements.identity)
    }
  }

  /**
    * Assumes that the value of the given variable is not null.
    *
    * @param identifier The identifier representing the variable.
    * @return The updated domain.
    */
  protected def assumeNonNull(identifier: Identifier): T = {
    val values = store.getOrElse(identifier, Set.empty) - NullNode
    val newStore = store + (identifier -> values)
    copy(store = newStore)
  }

  /**
    * Assumes that the value of the given field of the given receiver is not
    * null.
    *
    * @param receiver The receiver.
    * @param field    The field.
    * @return The updated domain.
    */
  protected def assumeNonNull(receiver: HeapNode, field: Identifier): T = {
    val map = heap.getOrElse(receiver, Map.empty)
    val values = map.getOrElse(field, Set.empty) - NullNode
    val newMap = map + (field -> values)
    val newHeap = heap + (receiver -> newMap)
    copy(heap = newHeap)
  }

  /**
    * Assumes that the given reference comparison holds.
    *
    * @param comparison The comparision to be assumed.
    * @return The updated domain and the corresponding replacement.
    */
  protected def assumeComparison(comparison: ReferenceComparisonExpression): (T, Replacement)

  /**
    * Helper method to merge two stores. This method can also be used to merge
    * two field maps since they have the same type.
    *
    * @param store1 The first store.
    * @param store2 The second store.
    * @param merge  The function used to merge the values.
    * @return The merged store.
    */
  protected def mergeStores(store1: Store,
                            store2: Store,
                            merge: (Set[HeapNode], Set[HeapNode]) => Set[HeapNode]): Store = {
    val keys = store1.keySet ++ store2.keySet
    keys.foldLeft(Map.empty: Store) {
      case (map, key) =>
        val values1 = store1.getOrElse(key, Set.empty)
        val values2 = store2.getOrElse(key, Set.empty)
        map + (key -> merge.apply(values1, values2))
    }
  }

  override def toString: String =
    if (isTop) "⊤"
    else if (isBottom) "⊥"
    else toDot

  def toDot: String = {
    // edges representing the store
    val storeEdges = store.flatMap {
      case (variable, values) =>
        values.map {
          value => "   \"" + variable + "\" -> \"o:" + value + "\""
        }
    }.toList.sorted.mkString("\n")

    // edges representing the heap
    val heapEdges = heap.flatMap {
      case (node, map) =>
        map.flatMap {
          case (field, values) =>
            values.map {
              value => "   \"o:" + node + "\" -> \"o:" + value + "\" [label=" + field + "]"
            }
        }
    }.toList.sorted.mkString("\n")

    s"digraph {\n$storeEdges\n$heapEdges\n}"
  }

  /**
    * Copies the alias graph and updates the top flag, bottom flag, store, heap,
    * and materialization flag if the corresponding arguments are defined.
    *
    * @param isTop           The top flag.
    * @param isBottom        The bottom flag.
    * @param store           The store.
    * @param heap            The heap.
    * @param materialization The materialization flag.
    * @return The updated alias graph.
    */
  def copy(isTop: Boolean = isTop,
           isBottom: Boolean = isBottom,
           store: Store = store,
           heap: Heap = heap,
           materialization: Boolean = materialization): T
}

object AliasGraph {
  type Store = Map[Identifier, Set[HeapNode]]

  type Heap = Map[HeapNode, Store]
}

/**
  * A graph representing may alias information.
  *
  * @param isTop           The top flag.
  * @param isBottom        THe bottom flag.
  * @param store           The store.
  * @param heap            The heap.
  * @param materialization The materialization flag.
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
case class MayAliasGraph(isTop: Boolean = false,
                         isBottom: Boolean = false,
                         store: Store = Map.empty,
                         heap: Heap = Map.empty,
                         materialization: Boolean = true)
  extends AliasGraph[MayAliasGraph] {

  override def lub(other: MayAliasGraph): MayAliasGraph =
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else {

      def merge(values1: Set[HeapNode], values2: Set[HeapNode]): Set[HeapNode] = {
        val union = values1 ++ values2
        if (union contains TopNode) union - UnknownNode
        else union
      }

      // combine stores
      val newStore = mergeStores(store, other.store, merge)

      // combine heaps
      val nodes = heap.keySet ++ other.heap.keySet
      val newHeap = nodes.foldLeft(Map.empty: Heap) { (h, node) =>
        (heap.get(node), other.heap.get(node)) match {
          case (None, None) => h
          case (Some(map), None) => h + (node -> map)
          case (None, Some(map)) => h + (node -> map)
          case (Some(map1), Some(map2)) => h + (node -> mergeStores(map1, map2, merge))
        }
      }

      copy(
        store = newStore,
        heap = newHeap,
        materialization = materialization && other.materialization
      )
    }

  override def glb(other: MayAliasGraph): MayAliasGraph = bottom()

  override def widening(other: MayAliasGraph): MayAliasGraph = {
    val domain = this lub other
    domain.copy(materialization = false)
  }

  override def lessEqual(other: MayAliasGraph): Boolean = {
    /**
      * Compares two stores. This function can also be used to compare two field
      * maps since they have the same type.
      *
      * @param store1 The first store.
      * @param store2 The second store.
      * @return True if the first store is smaller than or equal to the second
      *         store.
      */
    def compare(store1: Store, store2: Store): Boolean = {
      store1.forall {
        case (field, values1) =>
          val values2 = store2.getOrElse(field, Set.empty)
          if (values2 contains TopNode) true
          else if ((values2 contains UnknownNode) && !(values1 contains TopNode)) true
          else values1 subsetOf values2
      }
    }

    compare(store, other.store) && heap.forall {
      case (node, map1) =>
        val map2 = other.heap.getOrElse(node, Map.empty)
        compare(map1, map2)
    }
  }

  override def mayAlias(left: Expression, right: Expression): Boolean = {
    val domain = copy(materialization = true)
    val (_, values1, _) = domain.evaluate(left)
    val (_, values2, _) = domain.evaluate(right)

    if ((values1 contains TopNode) || (values2 contains TopNode)) true
    else {
      val intersection = (values1 & values2) - NullNode
      intersection.nonEmpty
    }
  }

  override def mustAlias(left: Expression, right: Expression): Boolean = {
    val domain = copy(materialization = true)
    val (_, values1, _) = domain.evaluate(left)
    val (_, values2, _) = domain.evaluate(right)

    if (values1.size == 1 && values2.size == 1) {
      val value1 = values1.head
      val value2 = values2.head
      value1 == value2 && value1.representsSingleVariable
    } else false
  }

  override protected def initialValues: Set[HeapNode] =
    Set(UnknownNode, NullNode)

  override protected def topValues: Set[HeapNode] =
    heap.keySet ++ heap.values.flatMap(_.values.flatten) - UnknownNode

  override protected def strongUpdate(target: Set[HeapNode]): Boolean =
    target.size == 1 && target.head.representsSingleVariable

  override protected def assumeComparison(comparison: ReferenceComparisonExpression): (MayAliasGraph, Replacement) = {
    val ReferenceComparisonExpression(left, right, operator) = comparison
    val (domain1, receivers1, values1, replacement1) = evaluateWithReceiver(left)
    val (domain2, receivers2, values2, replacement2) = domain1.evaluateWithReceiver(right)

    val (mask1, mask2) = operator match {
      case ReferenceOperator.== => (values2, values1)
      case ReferenceOperator.!= => (values1 -- values2, values2 -- values1)
    }

    val updated = domain2
      .filter(left, receivers1, mask1)
      .filter(right, receivers2, mask2)
    val replacement = replacement1 >> replacement2

    (updated, replacement)
  }

  private def filter(target: Expression, receivers: Set[HeapNode], mask: Set[HeapNode]): MayAliasGraph = {
    if (isTop || isBottom) this
    else target match {
      case Constant("null", _, _) => this
      case variable: VariableIdentifier =>
        val values = store.getOrElse(variable, Set.empty) & mask
        if (values.isEmpty) bottom()
        else {
          val newStore = store + (variable -> values)
          copy(store = newStore)
        }
      case AccessPathIdentifier(path) =>
        val field = path.last
        if (strongUpdate(receivers)) {
          receivers.foldLeft(this) {
            case (domain, receiver) =>
              val map = domain.heap.getOrElse(receiver, Map.empty)
              val values = map.getOrElse(field, Set.empty) & mask
              if (values.isEmpty) bottom()
              else {
                val newMap = map + (field -> values)
                val newHeap = heap + (receiver -> newMap)
                domain.copy(heap = newHeap)
              }
          }
        } else this
    }
  }

  override def copy(isTop: Boolean = isTop,
                    isBottom: Boolean = isBottom,
                    store: Store = store,
                    heap: Heap = heap,
                    materialization: Boolean = materialization): MayAliasGraph =
    MayAliasGraph(isTop, isBottom, store, heap, materialization)
}

/**
  * A graph representing must alias information.
  *
  * @param isTop           The top flag.
  * @param isBottom        The bottom flag.
  * @param store           The store
  * @param heap            The heap.
  * @param materialization The materialization flag.
  * @author Caterina Urban
  * @author Jerome Dohrau
  */
case class MustAliasGraph(isTop: Boolean = false,
                          isBottom: Boolean = false,
                          store: Store = Map.empty,
                          heap: Heap = Map.empty,
                          materialization: Boolean = true)
  extends AliasGraph[MustAliasGraph] {

  override def lub(other: MustAliasGraph): MustAliasGraph =
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else {

      def merge(values1: Set[HeapNode], values2: Set[HeapNode]): Set[HeapNode] = {
        val union = values1 ++ values2
        if (union.size == 1) union
        else Set(UnknownNode)
      }

      // combine stores
      val newStore = mergeStores(store, other.store, merge)

      // combine heaps
      val nodes = heap.keySet ++ other.heap.keySet
      val newHeap = nodes.foldLeft(Map.empty: Heap) { (result, node) =>
        (heap.get(node), other.heap.get(node)) match {
          case (None, _) => result
          case (_, None) => result
          case (Some(map1), Some(map2)) => result + (node -> mergeStores(map1, map2, merge))
        }
      }

      copy(
        store = newStore,
        heap = newHeap,
        materialization = materialization && other.materialization
      )
    }

  override def glb(other: MustAliasGraph): MustAliasGraph = bottom()

  override def widening(other: MustAliasGraph): MustAliasGraph = {
    val domain = this lub other
    domain.copy(materialization = false)
  }

  override def lessEqual(other: MustAliasGraph): Boolean = {
    /**
      * Compares two stores. This function can also be used to compare two field
      * maps since they have the same type.
      *
      * @param store1 The first store.
      * @param store2 The second store.
      * @return True if the first store is smaller than or equal to the second
      *         store.
      */
    def compare(store1: Store, store2: Store): Boolean = {
      store2.forall {
        case (field, values2) =>
          val values1 = store1.getOrElse(field, Set.empty)
          if (values1 contains UnknownNode) true
          else values2 subsetOf values1
      }
    }

    compare(store, other.store) && other.heap.forall {
      case (node, map2) =>
        val map1 = heap.getOrElse(node, Map.empty)
        compare(map1, map2)
    }
  }

  override def mayAlias(left: Expression, right: Expression): Boolean = true

  override def mustAlias(left: Expression, right: Expression): Boolean = {
    val domain = copy(materialization = true)
    val values1 = domain.evaluate(left)._2 -- Set(TopNode, UnknownNode, NullNode)
    val values2 = domain.evaluate(right)._2 -- Set(TopNode, UnknownNode, NullNode)
    val intersection = values1 & values2
    intersection.nonEmpty
  }

  override protected def initialValues: Set[HeapNode] = Set(UnknownNode)

  override protected def topValues: Set[HeapNode] = Set(TopNode)

  override protected def strongUpdate(target: Set[HeapNode]): Boolean = true

  override protected def assumeComparison(comparison: ReferenceComparisonExpression): (MustAliasGraph, Replacement) = {
    val ReferenceComparisonExpression(left, right, operator) = comparison
    val (domain1, receivers1, values1, replacement1) = evaluateWithReceiver(left)
    val (domain2, receivers2, values2, replacement2) = domain1.evaluateWithReceiver(right)

    val (mask1, mask2) = operator match {
      case ReferenceOperator.== => (values2, values1)
      case ReferenceOperator.!= => (values1.empty, values2.empty)
    }

    val updated = domain2
      .filter(left, receivers1, mask1)
      .filter(right, receivers2, mask2)
    val replacement = replacement1 >> replacement2

    (updated, replacement)
  }

  private def filter(target: Expression, receivers: Set[HeapNode], mask: Set[HeapNode]): MustAliasGraph =
    if (isTop || isBottom) this
    else target match {
      case Constant("null", _, _) => this
      case variable: VariableIdentifier =>
        val values = store.getOrElse(variable, Set.empty) & mask
        val newStore = store + (variable -> values)
        copy(store = newStore)
      case AccessPathIdentifier(path) =>
        val field = path.last
        if (strongUpdate(receivers)) {
          receivers.foldLeft(this) { case (domain, receiver) =>
            val map = domain.heap.getOrElse(receiver, Map.empty)
            val values = map.getOrElse(field, Set.empty) & mask
            val newMap = map + (field -> values)
            val newHeap = heap + (receiver -> newMap)
            domain.copy(heap = newHeap)
          }
        } else this
    }

  override def copy(isTop: Boolean = isTop,
                    isBottom: Boolean = isBottom,
                    store: Store = store,
                    heap: Heap = heap,
                    materialization: Boolean = materialization): MustAliasGraph =
    MustAliasGraph(isTop, isBottom, store, heap, materialization)
}