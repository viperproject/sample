package ch.ethz.inf.pm.sample.abstractdomain.heapanalysis

import ch.ethz.inf.pm.sample.ToStringUtilities
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{NativeMethodSemantics, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.property.Property


/**
 * The representation of a domain which consists of a may and a must part
 *
 * @tparam T1 The type of the first domain
 * @tparam T2 The type of the second domain
 * @tparam T The type of the current domain
 * @author Lucas Brutschy
 */
abstract class MayMustDomain[T1 <: Lattice[T1], T2 <: Lattice[T2], T <: MayMustDomain[T1, T2, T]]
(val may: T1, val must: T2) extends Lattice[T] {
  this: T =>

  def factory(a: T1, b: T2): T

  def setMay(a: T1) = factory(a, must)

  def setMust(b: T2) = factory(may, b)

  override def factory(): T = factory(may.factory(), must.factory())

  def top(): T = factory(may.top(), must.bottom())

  def bottom(): T = factory(may.bottom(), must.bottom())

  def lub(other: T): T = factory(may.lub(other.may), must.glb(other.must))

  def glb(other: T): T = factory(may.glb(other.may), must.glb(other.must))

  def widening(other: T): T = factory(may.widening(other.may), must.glb(other.must))

  def lessEqual(r: T): Boolean = {
    if (this.may.lessEqual(this.may.bottom())) return true
    if (r.may.lessEqual(r.may.bottom())) return false
    may.lessEqual(r.may) && r.must.lessEqual(must)
  }

  override def equals(a: Any): Boolean = a match {
    case right: T =>
      this.may.equals(right.may) && this.must.equals(right.must)
    case _ => false
  }

  override def toString =
    "May:\n" + ToStringUtilities.indent(may.toString) +
      "\nMust:\n" + ToStringUtilities.indent(must.toString)

}

/**
 * The representation of a may must domain supporting the operations of the semantic domain.
 *
 * @tparam T1 The type of the first domain
 * @tparam T2 The type of the second domain
 * @tparam T The type of the current domain
 * @author Lucas Brutschy
 */
abstract class SemanticMayMustDomain[T1 <: SemanticDomain[T1], T2 <: SemanticDomain[T2], T <: SemanticMayMustDomain[T1, T2, T]](a1: T1, a2: T2)
  extends MayMustDomain[T1, T2, T](a1, a2) with SemanticDomain[T] {
  this: T =>

  def getIds = may.ids ++ must.ids

  def setToTop(variable: Identifier): T = factory(may.setToTop(variable), must.setToTop(variable))

  def assign(variable: Identifier, expr: Expression): T = factory(may.assign(variable, expr), must.assign(variable, expr))

  def setArgument(variable: Identifier, expr: Expression): T = factory(may.setArgument(variable, expr), must.setArgument(variable, expr))

  def assume(expr: Expression): T = factory(may.assume(expr), must.assume(expr))

  def merge(r: Replacement): T = factory(may.merge(r), must.merge(r))

  def createVariable(variable: Identifier, typ: Type): T = factory(may.createVariable(variable, typ), must.createVariable(variable, typ))

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    val (a1, b1) = may.createVariableForArgument(variable, typ, path)
    val (a2, b2) = must.createVariableForArgument(variable, typ, path)
    (factory(a1, a2), b1 ++ b2)
  }

  def removeVariable(variable: Identifier): T = factory(may.removeVariable(variable), must.removeVariable(variable))

  def access(field: Identifier): T = factory(may.access(field), must.access(field))

  def backwardAccess(field: Identifier): T = factory(may.backwardAccess(field), must.backwardAccess(field))

  def backwardAssign(oldPreState: T, variable: Identifier, expr: Expression): T = factory(may.backwardAssign(oldPreState.may, variable, expr), must.backwardAssign(oldPreState.must, variable, expr))

  def getStringOfId(id: Identifier): String = "(may: " + may.getStringOfId(id) + ", must: " + must.getStringOfId(id) + ")"

}

/**
 * This implements a simple heap domain with may and must information
 *
 * @author Lucas Brutschy
 */
class MayMustHeapDomain(may: MayMustHeapEnvironment, must: MayMustHeapEnvironment)
  extends SemanticMayMustDomain[MayMustHeapEnvironment, MayMustHeapEnvironment, MayMustHeapDomain](may, must)
  with HeapDomain[MayMustHeapDomain, MayMustIdentifier] {

  /**
   * Gets the Heap Identifiers to which the provided Variable points to
   * in the Heap graph.
   *
   * @param key variable
   * @return  Heap Identifiers to which the variable points to
   */
  def get(key: VariableIdentifier): HeapIdSetDomain[MayMustIdentifier] = may.get(key)

  /**
   * Gets the Heap Identifiers to which the provided Variable points to
   * in the Heap graph.
   *
   * @param key Heap Identifier
   * @return  Heap Identifiers to which the Heap Identifier points to
   */
  def get(key: MayMustIdentifier): HeapIdSetDomain[MayMustIdentifier] = may.get(key)

  /**
   * This method creates an object of a given type
   *
   * @param typ The type of the object to be created
   * @param pp The point of the program that creates the reference
   * @return the identifier of the created object and the state of the heap after that
   */
  def createObject(typ: Type, pp: ProgramPoint): (HeapIdSetDomain[MayMustIdentifier], MayMustHeapDomain, Replacement) = {
    factory(may.createObject(typ, pp), must.createObject(typ, pp))
  }

  /**
   * This method returns the identifier of the field of an object
   *
   * @param objectIdentifier the identifier of the object to be accessed
   * @param name the name of the field
   * @param typ the type of the accessed field
   * @param pp the program point that accesses the field
   * @return the identifier of accessed field and the state of the heap after that
   */
  def getFieldIdentifier(objectIdentifier: Assignable, name: String, typ: Type, pp: ProgramPoint):
  (HeapIdSetDomain[MayMustIdentifier], MayMustHeapDomain, Replacement) = objectIdentifier match {
    case s: MayMustIdentifier =>
      new FieldAndMayMustIdentifier(s, name, typ)
    case _ =>
      throw Exception("Unknown object identifier in may must heap domain")
  }

  /**
   * This method sets to top a given variable
   *
   * @param variable the variable to be set to top
   * @return the state after this action
   */
  def setToTop(variable: Assignable): (MayMustHeapDomain, Replacement) =
    (factory(may.setToTop(variable), must.setToBottom(variable)), new Replacement)

  /**
   * This method assigns a given variable to the given expression
   *
   * @param variable the variable to be assigned
   * @param expr the expression to be assigned
   * @param state the state of the semantic domain at that point
   * @return the state after this action and the eventual replacements (e.g.,
   *         if the heap analyzed has summarize or splitted some cells)
   */
  def assign[S <: SemanticDomain[S]](variable: Assignable, expr: Expression, state: S): (MayMustHeapDomain, Replacement) =
    (factory(may.assign(variable, expr), must.assign(variable, expr)), new Replacement)

  /**
   * This method assigns a given field of a given objectto the given expression
   *
   * @param obj the object whose field has to be assigned
   * @param field the field to be assigned
   * @param expr the expression to be assigned
   * @return the state after this action and the eventual replacements (e.g.,
   *         if the heap analyzed has summarize or splitted some cells)
   */
  def assignField(obj: Assignable, field: String, expr: Expression): (MayMustHeapDomain, Replacement) =
    (factory(may.assignField(obj, field, expr), must.assignField(obj, field, expr)), new Replacement)


  /**
   * This method creates a variable
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action
   */
  def createVariable(variable: Assignable, typ: Type): (MayMustHeapDomain, Replacement) =
    (factory(may.createVariable(variable, typ), must.createVariable(variable, typ)), new Replacement)

  /**
   * This method removed a variable
   * @param variable the variable to be removed
   * @return the state after this action
   */
  def removeVariable(variable: Assignable): (MayMustHeapDomain, Replacement) =
    (factory(may.removeVariable(variable), must.removeVariable(variable)), new Replacement)

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
  def createEmptyCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type, originalCollectionTyp: Option[Type], keyCollectionTyp: Option[Type], pp: ProgramPoint): (HeapIdSetDomain[MayMustIdentifier], MayMustHeapDomain, Replacement) = ???

  /**
   * Returns the collection identifier or if a summary collection for
   * this collection identifier exists the identifier of the summary collection.
   *
   * @param collection
   * @return Either the summary collection identifier or the collection identifier
   */
  def getSummaryCollectionIfExists(collection: Assignable): HeapIdSetDomain[MayMustIdentifier] = ???

  def insertCollectionTopElement(collection: Assignable, pp: ProgramPoint): (HeapIdSetDomain[MayMustIdentifier], MayMustHeapDomain, Replacement) = ???

  /**
   * Returns the key identifier of a collection's key-value tuple
   *
   * @param collectionTuple The tuple's identifier
   * @return The key identifier
   */
  def getCollectionKeyByTuple(collectionTuple: Assignable): Assignable = ???

  /**
   * Returns the value identifier of a collection's key-value tuple.
   *
   * @param collectionTuple The tuple's identifier
   * @return The value identifier
   */
  def getCollectionValueByTuple(collectionTuple: Assignable): Assignable = ???

  /**
   * Returns the identifier of the collection's key-value tuple given the key identifier.
   *
   * @param keyId The identifier of the key
   * @return  The tuple identifier
   */
  def getCollectionTupleByKey(keyId: Assignable): Assignable = ???

  /**
   * Returns the identifier of the collection's key-value tuple given the value identifier.
   *
   * @param valueId The identifier of the value
   * @return The tuple identifier
   */
  def getCollectionTupleByValue(valueId: Assignable): Assignable = ???

  /**
   * Indicates whether a collection represents multiple collections or not.
   *
   * @param collectionId The collection
   * @return True if collection is a summary node, false otherwise.
   */
  def isSummaryCollection(collectionId: Assignable): Boolean = {

  }

  def getOriginalCollection(collection: Assignable): HeapIdSetDomain[MayMustIdentifier] = ???

  def getKeysCollection(collection: Assignable): HeapIdSetDomain[MayMustIdentifier] = ???

  /**
   * Returns all key-value tuple identifiers of a collection approximation.
   *
   * @param collectionApprox The collection approximation
   * @return All tuple identifiers of the collection approximation
   */
  def getCollectionTuples(collectionApprox: Assignable): HeapIdSetDomain[MayMustIdentifier] = ???

  /**
   * Returns the collection over-approximation identifier for the given collection.
   *
   * @param collection The collection
   * @return The collection over-approximation identifier
   */
  def getCollectionOverApproximation(collection: Assignable): HeapIdSetDomain[MayMustIdentifier] = ???

  /**
   * Returns the collection under-approximation identifier for the given collection.
   *
   * @param collection The collection
   * @return The collection under-approximation identifier
   */
  def getCollectionUnderApproximation(collection: Assignable): HeapIdSetDomain[MayMustIdentifier] = ???

  /**
   * Returns all the key identifiers of a collection. approximation
   *
   * @param collectionApprox  The collection approximation
   * @return  All key identifiers of the collection approximation
   */
  def getCollectionKeys(collectionApprox: Assignable): HeapIdSetDomain[MayMustIdentifier] = ???

  /**
   * Returns all the value identifiers of a collection approximation.
   *
   * @param collectionApprox The collection approximation
   * @return  All value identifiers of the collection
   */
  def getCollectionValues(collectionApprox: Assignable): HeapIdSetDomain[MayMustIdentifier] = ???

  /**
   * Adds a key-value tuple to the Heap structure if no
   * other tuple exists in the same collection that has the same program point.
   *
   * @param collection  The collection to which the new tuple shall be added.
   * @param pp  The program point that identifies the tuple.
   * @return The tuple identifier of the new tuple, the heap after the insertion, and the replacements that
   *         need to be applied to the value domain
   */
  def insertCollectionElement(collection: Assignable, pp: ProgramPoint): (HeapIdSetDomain[MayMustIdentifier], MayMustHeapDomain, Replacement) = ???

  /**
   * Inserts a collection Element just to one approximation (either Must or May analysis)
   * .
   * @param collectionApprox  The Identifier of the approximation to which the element shall be added
   * @param pp The program point that identifies the tuple
   * @return The tuple identifier of the new tuple, the heap after the insertion, and the replacements that
   *         need to be applied to the value domain
   */
  def insertCollectionElementToApprox(collectionApprox: Assignable, pp: ProgramPoint): (HeapIdSetDomain[MayMustIdentifier], MayMustHeapDomain, Replacement) = ???

  /**
   * Removes the specified key-value tuple from the collection.
   *
   * @param collectionTuple The key-value tuple that shall be removed
   * @return  The new heap after the collection tuple has been removed
   */
  def removeCollectionElement(collectionTuple: Assignable): MayMustHeapDomain = ???

  /**
   * Returns the identifier representing the length of the given collection
   *
   * @param collection The collection from which we want to access the length
   * @return The identifier of the collection's length
   */
  def getCollectionLength(collection: Assignable): HeapIdSetDomain[MayMustIdentifier] = ???

  /**
   * Performs abstract garbage collection
   */
  def getUnreachableHeap: Set[MayMustIdentifier] = ???

  /**
  Computes the upper bound of two elements, returning a replacement

  @param left One of the two values
  @param right The other value
  @return The least upper bound, that is, an element that is greater or equal than the two arguments
    */
  def lubWithReplacement(left: MayMustHeapDomain, right: MayMustHeapDomain): (MayMustHeapDomain, Replacement) = ???

  /**
  Computes the greatest lower bound of two elements

     @param left One of the two values
  @param right The other value
  @return The greatest upper bound, that is, an element that is less or equal than the two arguments, and greater or equal than any other lower bound of the two arguments
    */
  def glbWithReplacement(left: MayMustHeapDomain, right: MayMustHeapDomain): (MayMustHeapDomain, Replacement) = ???

  /**
  Computes widening of two elements

     @param left The previous value
  @param right The new value
  @return The widening of <code>left</code> and <code>right</code>
    */
  def wideningWithReplacement(left: MayMustHeapDomain, right: MayMustHeapDomain): (MayMustHeapDomain, Replacement) = ???

  def factory(a: MayMustHeapEnvironment, b: MayMustHeapEnvironment): MayMustHeapDomain = new MayMustHeapDomain(a, b)

  def optimizeSummaryNodes: (MayMustHeapDomain, Replacement) = ()

  def getLabel(): String = "Simple may must heap analysis"

  def parameters(): List[(String, Any)] = Nil

  def setParameter(label: String, value: Any): Unit = ()

  def getProperties: List[Property] = Nil

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil

  def reset(): Unit = ()

  def createArray[S <: SemanticDomain[S]](length: Expression, typ: Type, pp: ProgramPoint, state: S): (HeapIdSetDomain[MayMustIdentifier], MayMustHeapDomain, Replacement) = ???

  def getArrayLength(arrayId: Assignable): (HeapIdSetDomain[MayMustIdentifier], MayMustHeapDomain, Replacement) = ???

  def assignArrayCell[S <: SemanticDomain[S]](obj: Assignable, index: Expression, expr: Expression, state: S): (MayMustHeapDomain, Replacement) = ???

  def backwardAssign(variable: Assignable, expr: Expression): (MayMustHeapDomain, Replacement) = ???

  def endOfAssignment(): (MayMustHeapDomain, Replacement) = ()

  def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier: Assignable, index: Expression, state: S, typ: Type): (HeapIdSetDomain[MayMustIdentifier], MayMustHeapDomain, Replacement) = ???

  def setArgument(variable: Assignable, expr: Expression): (MayMustHeapDomain, Replacement) = ???

  def createVariableForArgument(variable: Assignable, typ: Type, path: List[String]): (MayMustHeapDomain, Map[Identifier, List[String]], Replacement) = ???

}


case class MayMustHeapEnvironment(map: Map[Identifier, HeapSet] = Map.empty[Identifier, HeapSet],
                                  override val isBottom: Boolean = false,
                                  isTop: Boolean = false)
  extends FunctionalDomain[Identifier, HeapSet, MayMustHeapEnvironment]
  with SemanticDomain[MayMustHeapEnvironment] {

  /**
   * Returns the value of key. It is not implemented since in some domains if the domain is not defined on the given
   * key we have a top value, in some others we have a bottom value. So we decided to let to the particular instance
   * of the domain the opportunity to define it in order to have a total function (that is more convenient when
   * defining the other lattice operators).
   *
   * @param key The key
   * @return The value related to the given key
   */
  def get(key: Identifier): MayHeapSetDomain[MayMustIdentifier] = this.value.get(key) match {
    case None => dom.top()
    case Some(x) => x
  }

  def functionalFactory(map: Map[Identifier, HeapSet] = Map.empty[Identifier, HeapSet],
                        isBottom: Boolean = false,
                        isTop: Boolean = false): MayMustHeapEnvironment = {
    MayMustHeapEnvironment(map, isBottom, isTop)
  }
}

case class HeapSet(
                    value: Set[MayMustIdentifier] = Set.empty[MayMustIdentifier],
                    isTop: Boolean = false,
                    isBottom: Boolean = false) extends SetDomain[MayMustIdentifier, HeapSet] {

  /**
   * Constructs a new set domain of the concrete type
   *
   * @return a fresh, empty instance of the set domain
   */
  override def setFactory(value: Set[MayMustIdentifier], isTop: Boolean, isBottom: Boolean): HeapSet =
    new HeapSet(value, isTop, isBottom)

}

case class MayMustIdentifier(pp: ProgramPoint, typ: Type, summary: Boolean, count: Integer) extends HeapIdentifier[MayMustIdentifier] {

  /**
  Returns the name of the identifier. We suppose that if two identifiers return the same name if and only
    if they are the same identifier
    @return The name of the identifier
    */
  def getName: String = pp.toString + (if (summary) "Σ" else "") + (if (count > 0) "*v" + count else "")

  /**
  Returns the name of the field that is represented by this identifier if it is a heap identifier.

    @return The name of the field pointed by this identifier
    */
  def getField: Option[String] = None

  /**
  Since an abstract identifier can be an abstract node of the heap, it can represent more than one concrete
    identifier. This function tells if a node is a summary node.

    @return true iff this identifier represents exactly one variable
    */
  def representsSingleVariable: Boolean = summary

}

case class FieldAndMayMustIdentifier(pp: MayMustIdentifier, name: String, typ: Type) extends HeapIdentifier[FieldAndMayMustIdentifier] {

  /**
  Returns the name of the identifier. We suppose that if two identifiers return the same name if and only
   if they are the same identifier
   @return The name of the identifier
    */
  def getName: String = pp.toString + "->" + name

  /**
  Returns the name of the field that is represented by this identifier if it is a heap identifier.

   @return The name of the field pointed by this identifier
    */
  def getField: Option[String] = Some(name)

  /**
  Since an abstract identifier can be an abstract node of the heap, it can represent more than one concrete
   identifier. This function tells if a node is a summary node.

   @return true iff this identifier represents exactly one variable
    */
  def representsSingleVariable: Boolean = pp.representsSingleVariable

}