package ch.ethz.inf.pm.sample.abstractdomain
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.util.UndirectedGraph


/**
 * A <code>Replacement</code> is a map from sets of identifiers to sets of identifiers.
 * Each relation in the map represents the set of identifiers that should be
 * merged into the given set of ids.
 *
 * For instance, merge([{a, b} -> {c}]) in a state in which a->[0..0] and b->[1..1] will produce
 * a state in which c -> [0..0] lub [1..1] = [0..1] and there are no a and b
 * merge([{a} -> {b, c}]) in a state in which a->[0..0] will produce
 * a state in which b -> [0..0] and c -> [0..0] and there is no a
 *
 * The creator of a replacement can give hints to the implementor about which operations are
 * required to perform the replacements using boolean flags. This way, an efficient handling
 * of simple operations can be implemented. These flags are completely optional.
 *
 * The flags are:
 *
 * isPureRenaming == A set of replacements of form {a}->{b} which can be executed
 *                   sequentially (i.e. union of all left side disjoint to union of all right sides)
 *
 * isPureExpanding == A set of replacements of form {a}->{a,b} which can be executed sequentially
 *
 * isPureRemoving == A set of replacements of form {a}->{}
 *
 * @author Pietro Ferrara
 * @version 0.1
 */

class Replacement( val value : scala.collection.mutable.HashMap[Set[Identifier], Set[Identifier]] = new scala.collection.mutable.HashMap[Set[Identifier], Set[Identifier]](),
                   val isPureRenaming:Boolean = false,
                   val isPureExpanding:Boolean = false,
                   val isPureRemoving:Boolean = false) {

  /**
   * Compute lub of replacements. Note that this was developed with the interval domain in mind
   * (may be too specific for that case).
   * @author Raphael Fuchs
   */
  def lub(other: Replacement): Replacement = {
    type Entry = (Set[Identifier], Set[Identifier])
    def adjacent(x: Entry, y: Entry): Boolean = x != y && !(x._2 intersect y._2).isEmpty

    val entries = (value.toList ++ other.value.toList).distinct
    val g = UndirectedGraph.build(entries, adjacent _)
    val lubEntries =
      for (component <- g.getComponents) yield {
        component reduceLeft {(l:Entry, r:Entry) => (l._1 union r._1, l._2 union r._2) }
      }

    new Replacement(scala.collection.mutable.HashMap(lubEntries: _*))
  }

  def glb(other: Replacement): Replacement = new Replacement(
    value.retain( {
      case (a, b) => other.value.keySet.contains(a) && other.value.apply(a).equals(b);
    }
    )
  )

  def isEmpty() = value.isEmpty;

  def keySet() = value.keySet;

  def apply(k : Set[Identifier]) = value.apply(k);

  def ++ (other:Replacement) =
    new Replacement(value ++ other.value,
      isPureRenaming = isPureRenaming && other.isPureRenaming,
      isPureExpanding = isPureExpanding && other.isPureExpanding,
      isPureRemoving = isPureRemoving && other.isPureRemoving
    )

  def >> (other:Replacement): Replacement = {
    if (this.value.isEmpty) return other
    if (other.value.isEmpty) return this

    val rep = new Replacement

    for ((fromsLeft, tosLeft) <- this.value) {
      val newTosLeft = tosLeft.diff(other.value.keys.flatten.toSet)
      if (!newTosLeft.isEmpty || tosLeft.isEmpty)
        rep.value += (fromsLeft -> newTosLeft)
    }

    def replace(in: Set[Identifier], inverseReplacement: Replacement): (Set[Identifier], Set[Identifier]) = {
      var result = in
      var replacedIds = Set.empty[Identifier]
      for ((fromsLeft, tosLeft) <- inverseReplacement.value) {
        val replaceIds = in.intersect(tosLeft)
        if (!replaceIds.isEmpty) {
          result = result.diff(replaceIds).union(fromsLeft)
        }
        replacedIds = replacedIds ++ replaceIds
      }
      (result, replacedIds)
    }

    for ((fromsRight, tosRight) <- other.value) {
      val (newFromsRight, replacedIds) = replace(fromsRight, this)
      rep.value += (newFromsRight -> tosRight)
      if (!replacedIds.isEmpty) {
        rep.value += (replacedIds -> Set.empty[Identifier])
      }
    }

    rep
  }

  /**
   * Prett-print replacement in the notation described above
   */
  override def toString = {
    val lines = for ((k,v) <- value) yield k.mkString("{",",","}") + "->" + v.mkString("{",",","}")
    lines.mkString(", ")
  }
}

/**
 * A <code>HeapDomain</code> is a domain aimed at tracking information
 * about the heap structure.
 *
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
// TODO: type parameters are in the wrong order - not consistent
trait HeapDomain[T <: HeapDomain[T, I], I <: HeapIdentifier[I]]
  extends Analysis
  with Lattice[T]
  with LatticeWithReplacement[T] { this: T =>

  /**
   * Gets the Heap Identifiers to which the provided Variable points to
   * in the Heap graph.
   *
   * @param key variable
   * @return  Heap Identifiers to which the variable points to
   */
  def get(key: VariableIdentifier): HeapIdSetDomain[I]

  /**
   * Gets the Heap Identifiers to which the provided Variable points to
   * in the Heap graph.
   *
   * @param key Heap Identifier
   * @return  Heap Identifiers to which the Heap Identifier points to
   */
  def get(key: I): HeapIdSetDomain[I]

  /**
   * This method creates an object of a given type
   *
   * @param typ The type of the object to be created
   * @param pp The point of the program that creates the reference
   * @return the identifier of the created object and the state of the heap after that
   */
  def createObject(typ: Type, pp: ProgramPoint): (HeapIdSetDomain[I], T, Replacement)

  /**
   * This method creates an array
   *
   * @param length the length of the array to be created
   * @param typ its type
   * @param pp the program point
   * @param state the state of the semantic domain at that point.
   * @return the heap id of the array, the state after this action, and the replacement caused by the creation of the array
   */
  def createArray[S <: SemanticDomain[S]](length: Expression, typ: Type, pp: ProgramPoint, state: S): (HeapIdSetDomain[I], T, Replacement)

  /**
   * This method returns an id of the length of a given array
   *
   * @param arrayId the id of the array we want to know the length
   * @return the heap id of the length of the array, the state after this action, and a replacement
   */
  def getArrayLength(arrayId: Assignable): (HeapIdSetDomain[I], T, Replacement)

  /**
   * This method returns the identifier of the field of an object
   *
   * @param objectIdentifier the identifier of the object to be accessed
   * @param name the name of the field
   * @param typ the type of the accessed field
   * @param pp the program point that accesses the field
   * @return the identifier of accessed field and the state of the heap after that
   */
  def getFieldIdentifier(objectIdentifier: Assignable, name: String, typ: Type, pp: ProgramPoint): (HeapIdSetDomain[I], T, Replacement)

  /**
   * This method is used to signal that we have ended to assign something. For instance,
   * in TVLA we could create some temporary nodes when assigning. This method signals to
   * TVLA to drop all these temporary nodes.
   * @return the state of the heap after the action and a replacement
   */
  def endOfAssignment(): (T, Replacement)

  /**
   * This method returns the identifier of the cell of an array
   * @param arrayIdentifier the identifier of the array to be accessed
   * @param index the index used to access the array
   * @param state the state of the semantic domain at that point. This could be useful to precisely
   *              analyze inside which bounds of the array the access is
   * @param typ the type of the accessed cell
   * @return the identifier of accessed cell, the state of the heap after that (since we could create new
   *         abstract ids when accessing the array in order to be more precise), and the eventual replacements (e.g.,
   *         if the heap analyzed has summarize or splitted some cells)
    */
  def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier: Assignable, index: Expression, state: S, typ: Type): (HeapIdSetDomain[I], T, Replacement)

  /**
   * This method sets to top a given variable
   *
   * @param variable the variable to be set to top
   * @return the state after this action
   */
  def setToTop(variable: Assignable): (T, Replacement)

  /**
   * This method assigns a given variable to the given expression
   *
   * @param variable the variable to be assigned
   * @param expr the expression to be assigned
   * @param state the state of the semantic domain at that point
   * @return the state after this action and the eventual replacements (e.g.,
   *         if the heap analyzed has summarize or splitted some cells)
   */
  def assign[S <: SemanticDomain[S]](variable: Assignable, expr: Expression, state: S): (T, Replacement)

  /**
   * This method assigns a given field of a given objectto the given expression
   *
   * @param obj the object whose field has to be assigned
   * @param field the field to be assigned
   * @param expr the expression to be assigned
   * @return the state after this action and the eventual replacements (e.g.,
   *         if the heap analyzed has summarize or splitted some cells)
   */
  def assignField(obj: Assignable, field: String, expr: Expression): (T, Replacement)

  /**
   * This method assigns a given field of a given objectto the given expression
   *
   * @param obj the array whose cell has to be assigned
   * @param index the index to be assigned
   * @param expr the expression to be assigned
   * @param state the state of the semantic domain (useful to refine eventually the splitting of the array)
   * @return the state after this action and the eventual replacements (e.g.,
   *         if the heap analyzed has summarize or splitted some cells)
   */
  def assignArrayCell[S <: SemanticDomain[S]](obj: Assignable, index: Expression, expr: Expression, state: S): (T, Replacement)

  /**
   * This method set a paramenter (usually the parameter passed to a method) to the given expression
   *
   * @param variable the variable to set
   * @param expr the expression to set
   * @return the state after this action
   */
  def setArgument(variable: Assignable, expr: Expression): (T, Replacement)

  /**
   * This method assumes that a given expression holds
   *
   * @param expr the expression to be assumed
   * @return the state after this action
   */
  def assume(expr: Expression): (T, Replacement)

  /**
   * This method creates a variable
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action
    */
  def createVariable(variable: Assignable, typ: Type): (T, Replacement)

  /**
   * This method creates a variable that is a parameter of the analyzed method
   *
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action and a map relating identifiers to the path starting with the parameter
   *         to access them (this is useful for the heap domain that has to create abstract references to approximate
   *         the initial heap structure)
   */
  def createVariableForArgument(variable: Assignable, typ: Type, path: List[String]): (T, Map[Identifier, List[String]], Replacement)

  /**
   * This method removed a variable
   * @param variable the variable to be removed
   * @return the state after this action
   */
  def removeVariable(variable: Assignable): (T, Replacement)

  /**
   * This method provides the backward semantics of assignment
   *
   * @param variable
   * @param expr
   * @return the state before variable=expr
   */
  def backwardAssign(variable: Assignable, expr: Expression): (T, Replacement)

  /**
   * This method returns all the ids over whom the HeapDomain is defined
   *
   * @return all ids contained in the heap
   */
  def getIds(): scala.collection.Set[Identifier]

  /**
   * Creates the heap structure for an empty collection
   *
   * @param collTyp  The type of the collection
   * @param keyTyp  The type of the collection's keys
   * @param valueTyp The type of the collection's values
   * @param lengthTyp  The type of the collection's length
   * @param pp The program point at which the collection is created
   * @return The Heapidentifier of the newly created collection and the heap with the newly created collection
    */
  def createEmptyCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, originalCollectionTyp: Option[Type], keyCollectionTyp: Option[Type], pp: ProgramPoint): (HeapIdSetDomain[I], T, Replacement)

  /**
   * Returns the collection identifier or if a summary collection for
   * this collection identifier exists the identifier of the summary collection.
   *
   * @param collection
   * @return Either the summary collection identifier or the collection identifier
   */
  def getSummaryCollectionIfExists(collection: Assignable): HeapIdSetDomain[I]

  def insertCollectionTopElement(collection: Assignable, pp: ProgramPoint): (HeapIdSetDomain[I], T, Replacement)

  /**
   * Returns the key identifier of a collection's key-value tuple
   *
   * @param collectionTuple The tuple's identifier
   * @return The key identifier
   */
  def getCollectionKeyByTuple(collectionTuple: Assignable): Assignable

  /**
   * Returns the value identifier of a collection's key-value tuple.
   *
   * @param collectionTuple The tuple's identifier
   * @return The value identifier
   */
  def getCollectionValueByTuple(collectionTuple: Assignable): Assignable

  /**
   * Returns the identifier of the collection's key-value tuple given the key identifier.
   *
   * @param keyId The identifier of the key
   * @return  The tuple identifier
   */
  def getCollectionTupleByKey(keyId: Assignable): Assignable

  /**
   * Returns the identifier of the collection's key-value tuple given the value identifier.
   *
   * @param valueId The identifier of the value
   * @return The tuple identifier
   */
  def getCollectionTupleByValue(valueId: Assignable): Assignable

  /**
   * Indicates whether a collection represents multiple collections or not.
   *
   * @param collectionId The collection
   * @return True if collection is a summary node, false otherwise.
   */
  def isSummaryCollection(collectionId: Assignable): Boolean

  def getOriginalCollection(collection: Assignable): HeapIdSetDomain[I]

  def getKeysCollection(collection: Assignable): HeapIdSetDomain[I]

  /**
   * Returns all key-value tuple identifiers of a collection approximation.
   *
   * @param collectionApprox The collection approximation
   * @return All tuple identifiers of the collection approximation
   */
  def getCollectionTuples(collectionApprox: Assignable): HeapIdSetDomain[I]

  /**
   * Returns the collection over-approximation identifier for the given collection.
   *
   * @param collection The collection
   * @return The collection over-approximation identifier
   */
  def getCollectionOverApproximation(collection: Assignable): HeapIdSetDomain[I]

  /**
   * Returns the collection under-approximation identifier for the given collection.
   *
   * @param collection The collection
   * @return The collection under-approximation identifier
   */
  def getCollectionUnderApproximation(collection: Assignable): HeapIdSetDomain[I]

  /**
   * Returns all the key identifiers of a collection. approximation
   *
   * @param collectionApprox  The collection approximation
   * @return  All key identifiers of the collection approximation
   */
  def getCollectionKeys(collectionApprox: Assignable): HeapIdSetDomain[I]

  /**
   * Returns all the value identifiers of a collection approximation.
   *
   * @param collectionApprox The collection approximation
   * @return  All value identifiers of the collection
   */
  def getCollectionValues(collectionApprox: Assignable): HeapIdSetDomain[I]

  /**
   * Adds a key-value tuple to the Heap structure if no
   * other tuple exists in the same collection that has the same program point.
   *
   * @param collection  The collection to which the new tuple shall be added.
   * @param pp  The program point that identifies the tuple.
   * @return The tuple identifier of the new tuple, the heap after the insertion, and the replacements that
   *         need to be applied to the value domain
   */
  def insertCollectionElement(collection: Assignable, pp: ProgramPoint): (HeapIdSetDomain[I], T, Replacement)

  /**
   * Inserts a collection Element just to one approximation (either Must or May analysis)
   * .
   * @param collectionApprox  The Identifier of the approximation to which the element shall be added
   * @param pp The program point that identifies the tuple
   * @return The tuple identifier of the new tuple, the heap after the insertion, and the replacements that
   *         need to be applied to the value domain
   */
  def insertCollectionElementToApprox(collectionApprox: Assignable, pp: ProgramPoint): (HeapIdSetDomain[I], T, Replacement)

  /**
   * Removes the specified key-value tuple from the collection.
   *
   * @param collectionTuple The key-value tuple that shall be removed
   * @return  The new heap after the collection tuple has been removed
   */
  def removeCollectionElement(collectionTuple: Assignable): T

  /**
   * Returns the identifier representing the length of the given collection
   *
   * @param collection The collection from which we want to access the length
   * @return The identifier of the collection's length
   */
  def getCollectionLength(collection: Assignable): HeapIdSetDomain[I]

  /**
   * Performs abstract garbage collection
   */
  def getUnreachableHeap: Set[I]

  /**
   * Converts summary nodes to regular nodes whenever possible and sound
   */
  def optimizeSummaryNodes: (T, Replacement)
}

trait Assignable {
  def pp : ProgramPoint
  def getType : Type
}


case class CollectionContainsExpression(collection: Expression, key: Expression, value: Expression, returnTyp: Type, pp: ProgramPoint) extends Expression {

  def getType: Type = returnTyp
  def getIdentifiers: Set[Identifier] = Set.empty[Identifier]

  override def equals(obj: Any): Boolean = obj match {
    case CollectionContainsExpression(ppX, collectionX, keyX, valueX, returnTypeX) =>
      ppX.equals(pp) && collectionX.equals(collection) && keyX.equals(key) && valueX.equals(value) && returnTypeX.equals(returnTyp)
    case _ => false
  }

  override def toString: String = "(" + key + ", " + value.toString() + ") in " + collection.toString()

  def transform(f: (Expression) => Expression): Expression = {
    f(CollectionContainsExpression(f(collection), f(key), f(value), returnTyp, pp))
  }

}

abstract class HeapIdSetDomain[I <: HeapIdentifier[I]](
    pp: ProgramPoint,
    _value: Set[I] = Set.empty[I],
    _isTop: Boolean = false,
    _isBottom: Boolean = false)
  extends SetDomain[I, HeapIdSetDomain[I]](_value, _isTop, _isBottom)
  with Expression {

  def this() = this(null)

  override def equals(x : Any) : Boolean = x match {
    case x : I => if(value.size==1) return x.equals(value.head); else return false;
    case _ => return super.equals(x);
  }

  def convert(add : I) : HeapIdSetDomain[I]

  def merge(rep:Replacement) : HeapIdSetDomain[I] = {

    if (this.isBottom || this.isTop || this.value.isEmpty) return this

    var result = this.value
    for ((froms,tos) <- rep.value) {

      val fromsI = froms collect { case x:I => x }
      val tosI = tos collect { case x:I => x }

      if (!this.value.intersect(fromsI).isEmpty) {
         result = result -- fromsI
         result = result ++ tosI
      }

    }

    setFactory(result)
  }

  override def transform(f:(Expression => Expression)):Expression =
    this.setFactory(this.value.map( x => f(x).asInstanceOf[I] ))

  def lubWithReplacement[S <: SemanticDomain[S]](other: HeapIdSetDomain[I], state: S): (HeapIdSetDomain[I], Replacement) =
    (super.lub(other), new Replacement)

  // Used to know if it's definite - glb - or maybe - lub.
  def combinator[S <: Lattice[S]](s1 : S, s2 : S) : S
  def heapCombinator[H <: LatticeWithReplacement[H], S <: SemanticDomain[S]](h1 : H, h2 : H, s1 : S, s2 : S) : (H, Replacement)

}

class MaybeHeapIdSetDomain[I <: HeapIdentifier[I]](
    val pp: ProgramPoint,
    _value: Set[I] = Set.empty[I],
    _isTop: Boolean = false,
    _isBottom: Boolean = false)
  extends HeapIdSetDomain[I](pp, _value, _isTop, _isBottom) {

  def this() = this(null)

  def setFactory (_value: Set[I] = Set.empty[I], _isTop: Boolean = false, _isBottom: Boolean = false): HeapIdSetDomain[I] =
    new MaybeHeapIdSetDomain[I](pp, _value, _isTop, _isBottom)

  def convert(add : I) : HeapIdSetDomain[I] = new MaybeHeapIdSetDomain(add.pp).add(add)

  override def getType : Type = {
    var res=SystemParameters.getType().bottom()
    for (a <- this.value) res=res.lub(a.getType)
    res
  }

  def combinator[S <: Lattice[S]](s1 : S, s2 : S) : S = s1.lub(s2)

  def heapCombinator[H <: LatticeWithReplacement[H], S <: SemanticDomain[S]](h1: H, h2: H, s1: S, s2: S): (H, Replacement) = h1.lubWithReplacement(h2)

  def getIdentifiers : Set[Identifier] = this.value.asInstanceOf[Set[Identifier]]
}

class DefiniteHeapIdSetDomain[I <: HeapIdentifier[I]](
    val pp: ProgramPoint,
    _value: Set[I] = Set.empty[I],
    _isTop: Boolean = false,
    _isBottom: Boolean = false)
  extends HeapIdSetDomain[I](pp, _value, _isTop, _isBottom) {

  def setFactory (_value: Set[I] = Set.empty[I], _isTop: Boolean = false, _isBottom: Boolean = false): HeapIdSetDomain[I] =
    new DefiniteHeapIdSetDomain[I](pp,_value,_isTop,_isBottom)

  def convert(add : I) : HeapIdSetDomain[I] = new DefiniteHeapIdSetDomain(add.pp).add(add)

  override def getType : Type = {
    var res=SystemParameters.getType().top()
    for(a <- this.value)
      res=res.glb(a.getType)
    res
  }

  def combinator[S <: Lattice[S]](s1 : S, s2 : S) : S = s1.glb(s2)

  def heapCombinator[H <: LatticeWithReplacement[H], S <: SemanticDomain[S]](h1: H, h2: H, s1: S, s2: S): (H, Replacement) = h1.lubWithReplacement(h2)

  def getIdentifiers : Set[Identifier] = this.value.asInstanceOf[Set[Identifier]]
}
