package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample._

/**
 * The representation of a <a href="http://en.wikipedia.org/wiki/Lattice_%28order%29">lattice</a> structure
 *
 * @param <T> The current type of the Lattice
 * @author Pietro Ferrara
 * @since 0.1
 */
trait Lattice[T <: Lattice[T]] {

  /**
  Returns a new instance of the lattice
   @return A new instance of the current object
    */
  def factory() : T

  /**
  Returns the top value of the lattice
   @return The top value, that is, a value x that is greater or equal than any other value
    */
  def top() : T

  /**
  Returns the bottom value of the lattice
   @return The bottom value, that is, a value x that is less or equal than any other value
    */
  def bottom() : T

  /**
  Computes the upper bound of two elements

   @param left One of the two values
  @param right The other value
  @return The least upper bound, that is, an element that is greater or equal than the two arguments
    */
  def lub(left : T, right : T) : T

  /**
  Computes the greatest lower bound of two elements

   @param left One of the two values
  @param right The other value
  @return The greatest upper bound, that is, an element that is less or equal than the two arguments, and greater or equal than any other lower bound of the two arguments
    */
  def glb(left : T, right : T) : T

  /**
  Computes widening of two elements

   @param left The previous value
  @param right The new value
  @return The widening of <code>left</code> and <code>right</code>
    */
  def widening(left : T, right : T) : T

  /**
  Returns true iff <code>this</code> is less or equal than <code>r</code>

   @param r The value to compare
  @return true iff <code>this</code> is less or equal than <code>r</code>
    */
  def lessEqual(r : T) : Boolean
}

/**
 * The representation of a state of our analysis.
 * Two main components can be distinguished:
 * - a SymbolicAbstractValue that is aimed at representing the expression returned by the previous statement
 * - an HeapAndAnotherDomain state, that is, an abstract state of an heap analysis and of another semantic domain
 *
 * This is the most generic level to build up an abstract state. We strongly discourage the use of this interface
 * since there are simpler interface (e.g., SemanticDomain or HeapAndAnotherDomain)
 *
 * @param <S> The current type of the state
 * @author Pietro Ferrara
 * @since 0.1
 */
trait State[S <: State[S]] extends Lattice[S] {

  /**
  Signals that we are going to analyze the statement at program point pp
   This is particularly important to eventually partition a state following
   the specified directives

   @param pp The point of the program that is going to be analyzed
  @return The abstract state eventually modified
    */
  def before(pp : ProgramPoint) : S

  /**
  Creates an object

  @param typ The dynamic type of the created object
  @param pp The point of the program that creates the object
  @param fields If this is defined, the given fields will be created instead of the types fields (e.g. for reducing
                  the set of initialized fields)
  @return The abstract state after the creation of the object
    */
  def createObject(typ : Type, pp : ProgramPoint, fields : Option[Set[Identifier]] = None) : S

  /**
  Creates a variable

   @param x The name of the variable
  @param typ The static type of the variable                                    \
  @param pp The program point that creates the variable
  @return The abstract state after the creation of the variable
    */
  def createVariable(x : ExpressionSet, typ : Type, pp : ProgramPoint) : S

  /**
  Creates a variable for an argument

   @param x The name of the argument
  @param pp The static type of the argument
  @return The abstract state after the creation of the argument
    */
  def createVariableForArgument(x : ExpressionSet, typ : Type) : S

  /**
  Assigns an expression to a variable

   @param x The assigned variable
  @param right The assigned expression
  @return The abstract state after the assignment
    */
  def assignVariable(x : ExpressionSet, right : ExpressionSet) : S

  /**
  Create an array of length

   @param length The length of the array
  @param typ The type of the array
  @param length The program point that created the array
  @return The abstract state after the creation of the array
    */
  def createArray(length : ExpressionSet, typ : Type, pp : ProgramPoint) : S

  /**
  Assigns an expression to a field of an object

   @param obj The object whose field is assigned
  @param field The assigned field
  @param right The assigned expression
  @return The abstract state after the assignment
    */
  def assignField(obj : List[ExpressionSet], field : String, right : ExpressionSet) : S

  /**
  Assign a cell of an array

   @param obj The object on which the array assignment
  @param index The assigned index
  @param typ The type of the cell
  @param right The assigned expression
  @return The abstract state obtained after the array cell assignment
    */
  def assignArrayCell(obj : ExpressionSet, index : ExpressionSet, right : ExpressionSet, typ : Type) : S

  /**
  Assigns an expression to an argument

   @param x The assigned argument
  @param right The expression to be assigned
  @return The abstract state after the assignment
    */
  def setArgument(x : ExpressionSet, right : ExpressionSet) : S

  /**
  Forgets the value of a variable

   @param x The variable to be forgotten
  @return The abstract state obtained after forgetting the variable
    */
  def setVariableToTop(x : ExpressionSet) : S

  /**
  Removes a variable

   @param x The variable to be removed
  @return The abstract state obtained after removing the variable
    */
  def removeVariable(x : ExpressionSet) : S

  /**
  Throws an exception

   @param t The thrown exception
  @return The abstract state after the thrown
    */
  def throws(t : ExpressionSet) : S

  /**
  Gets the value of a variable

   @param id The variable to access
  @return The abstract state obtained after accessing the variable, that is, the state that contains as expression the symbolic representation of the value of the given variable
    */
  def getVariableValue(id : Assignable) : S

  /**
  Accesses a field of an object

   @param obj The object on which the field access is performed
  @param field The name of the field
  @param typ The type of the field
  @return The abstract state obtained after the field access, that is, the state that contains as expression the symbolic representation of the value of the given field access
    */
  def getFieldValue(obj : List[ExpressionSet], field : String, typ : Type) : S

  /**
  Accesses a field of an object

   @param obj The object on which the field access is performed
  @param field The name of the field
  @param typ The type of the field
  @return The abstract state obtained after the field access, that is, the state that contains as expression the symbolic representation of the value of the given field access
    */
  def getArrayCell(obj : ExpressionSet, index : ExpressionSet, typ : Type) : S

  /**
  Returns the identifier representing the length of the given array

   @param array The array from which we want to access the length
  @return A state that contains as expression the symbolic representation of the length of the given array
    */
  def getArrayLength(array : ExpressionSet) : S

  /**
  Performs the backward semantics of a variable access

   @param id The accessed variable
  @return The abstract state obtained BEFORE accessing the variable
    */
  def backwardGetVariableValue(id : Assignable) : S

  /**
  Performs the backward semantics of a field access

   @param obj The object on which the field access is performed
  @param field The name of the field
  @param typ The type of the field
  @return The abstract state obtained before the field access
    */
  def backwardGetFieldValue(objs : List[ExpressionSet], field : String, typ : Type) : S

  /**
  Performs the backward semantics of an assignment

   @param x The assigned variable
  @param right The assigned expression
  @return The abstract state before the assignment
    */
  def backwardAssignVariable(x : ExpressionSet, right : ExpressionSet) : S

  /**
  Evaluates a numerical constant

   @param value The string representing the numerical constant
  @param typ The type of the numerical constant
  @param pp The program point that contains the constant
  @return The abstract state after the evaluation of the constant, that is, the state that contains an expression representing this constant
    */
  def evalConstant(value : String, typ : Type, pp : ProgramPoint) : S

  /**
  Assumes that a boolean expression holds

   @param cond The assumed expression
  @return The abstract state after assuming that the expression holds
    */
  def assume(cond : ExpressionSet) : S

  /**
  Assumes that the current expression holds

   @return The abstract state after assuming that the expression holds
    */
  def testTrue() : S

  /**
  Assumes that the current expression does not hold

   @return The abstract state after assuming that the expression does not hold
    */
  def testFalse() : S

  /**
  Returns the current expression

   @return The current expression
    */
  def getExpression() : ExpressionSet

  /**
  Sets the current expression

   @param expr The current expression
  @return The abstract state after changing the current expression with the given one
    */
  def setExpression(expr : ExpressionSet) : S

  /**
  Removes the current expression

   @return The abstract state after removing the current expression
    */
  def removeExpression() : S

  /**
  Creates an empty collection.

    @param collTyp  The type of the collection
  @param keyTyp The type of the collection's keys
  @param valueTyp The type of the collection's values
  @param lengthTyp The type of the collection length
  @param tpp  The program point at which the collection is created
    */
  def createCollection(collTyp: Type, keyTyp: Type, valueTyp: Type, lengthTyp: Type,  tpp: ProgramPoint, fields : Option[Set[Identifier]] = None) : S

  /**
  Gets the Identifier of all the keys of the collection that match the given key expresssion.
     A key expression (key) matches a Identifier if the Identifier represents a key of the collection
     and has value k assigned such that
        lub(k, key) != bottom

    @param collectionSet  The collection expressions
  @param keySet The key expressions
  @return The state that has the mapped Identifier as expression
    */
  def getCollectionKeyByKey(collectionSet: ExpressionSet, keySet: ExpressionSet): S

  /**
  Gets the Identifier of all the values of the collection for which the key Identifier matches the given
    key expression.
    A key expression (key) matches a Identifier if the Identifier represents a key of the collection
    and has value k assigned such that
        lub(k, key) != bottom

    @param collectionSet  The collection expressions
  @param keySet The key expressions
  @param valueTyp The type of the collection's values
  @return The state that has the mapped Identifier as expression
    */
  def getCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet, valueTyp: Type): S

  /**
  Gets the HeapIdentifier of all the values of the collection that match the given value expresssion.
    A value expression (value) matches a HeapIdentifier if the Heapidentifier represents a value of the collection
    and has value v assigned such that
        lub(v, value) != bottom

    @param collectionSet  The collection expressions
  @param valueSet The value expressions
  @return The state that has the mapped HeapIdentifiers as expression
    */
  def getCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet): S

  /**
  Creates a new collection that contains all keys of the provided collection (fromCollection)
    as values.

    @param fromCollection The collection from which the keys shall be extracted
  @param collTyp  The collection type of the newly created collection
  @param keyTyp  The key type of the newly created collection
  @param valueTyp The value type of the newly created collection
  @param lengthTyp  The length type of the newly created collection@param pp
  @return The state that contains the newly created collection and has it's CollectionHeapIdentifier as expression
    */
  def extractCollectionKeys(fromCollectionSet: ExpressionSet, newKeyValueSet: ExpressionSet, collTyp:Type, keyTyp:Type, valueTyp:Type, lengthTyp:Type, pp:ProgramPoint): S

  /**
  Copies all the key-value tuples from one collection to the other.

    @param fromCollectionSet The collection from which the tuples are copied.
  @param toCollectionSet The collection to which the tuples are copied to.
  @param keyTyp  The key type of the collections
  @param valueTyp  The value type of the collection
  @return The state that has a copy of all the tuples of the fromCollection in the toCollection
    */
  def copyCollection(fromCollectionSet: ExpressionSet, toCollectionSet: ExpressionSet, keyTyp: Type, valueTyp: Type): S

  /**
  Creates a new key-value tuple and adds it to the collection.

    @param collectionSet The collection to which the key-value pair shall be added
  @param keySet  The expression that is assigned to the key node
  @param rightSet  The expression that is assigned to the value node
  @param pp  The program point that the new tuple shall have.
               Key-value tuples of a collection are distinguished by their program point.
               If a tuple with this program point already exists in the collection the new tuple
               will be summarized with this tuple.
  @return The state that contains the collection with the added collection-tuple
    */
  def insertCollectionValue(collectionSet: ExpressionSet, keySet: ExpressionSet, rightSet: ExpressionSet, pp: ProgramPoint): S

  /**
  Removes the values from the collection who's key identifiers match the given key expression.
    If the key expression (k) matches the key identifier's assigned value exactly, the tuple is completely removed.
    Otherwise the tuple is not removed but the semantic state contains the assumption
      key identifier != k

    @param collectionSet The collection from which the value is removed
  @param keySet The key expressions
  @param valueTyp The value type of the collection
  @return The state in which the collection is represented without the collection value
    */
  def removeCollectionValueByKey(collectionSet: ExpressionSet, keySet: ExpressionSet, valueTyp: Type): S

  /**
  Removes the values from the collection who's value identifiers match the given value expression.
    If the value expression (v) matches the value identifier's assigned value exactly, the tuple is completely removed.
    Otherwise the tuple is not removed but the semantic state contains the assumption
      value identifier != v

    @param collectionSet The collection from which the value is removed
  @param valueSet The value expressions
  @param keyTyp The key type of the collection
  @return The state in which the collection is represented without the collection value
    */
  def removeCollectionValueByValue(collectionSet: ExpressionSet, valueSet: ExpressionSet, keyTyp: Type): S

  /**
   * Assigns the value expression to all key identifiers of the collection.
   *
   * @param collectionSet The collection
   * @param valueSet  The value expression
   * @param keyTyp  The collection's key type
   * @return  The state in which all the key identifiers of the collection have the value expression assigned
   */
  def assignAllCollectionKeys(collectionSet: ExpressionSet, valueSet: ExpressionSet, keyTyp: Type): S

  /**
   * Removes all the key-value tuples from a collection and sets it's length to 0.
   * @param collectionSet The collection to be cleared
   * @param keyTyp The key type of the collection
   * @param valueTyp The value type of the collection
   * @return The state with the cleared collection
   */
  def clearCollection(collectionSet: ExpressionSet, keyTyp: Type, valueTyp: Type) : S

  /**
  Returns the identifier representing the length of the given collection.
    @param collectionSet The collection from which we want to access the length
  @return A state that contains as expression the symbolic representation of the length of the given collection
    */
  def getCollectionLength(collectionSet: ExpressionSet) : S

  /**
   * Indicates whether any collection in the ExpressionSet represents multiple collections.
   *
   * @param collectionId The collection set
   * @return True if any collection is a summary node, false otherwise.
   */
  def isSummaryCollection(collectionSet: ExpressionSet): Boolean

  /**
   * Removes all variables satisfying filter
   */
  def pruneVariables(filter:Identifier => Boolean) : S

  /**
   * Performs abstract garbage collection
   */
  def pruneUnreachableHeap() : S

}

/**
 * The representation of a <a href="http://en.wikipedia.org/wiki/Lattice_%28order%29">lattice</a> structure
 * that when joins, meets or widens returns a replacement.
 *
 * @param <T> The current type of the LatticeWithReplacement
 * @author Pietro Ferrara
 * @since 0.1
 */
trait LatticeWithReplacement[T <: LatticeWithReplacement[T]] {

  /**
  Computes the upper bound of two elements, returning a replacement

  @param left One of the two values
  @param right The other value
  @return The least upper bound, that is, an element that is greater or equal than the two arguments
    */
  def lubWithReplacement(left : T, right : T) : (T, Replacement)

  /**
  Computes the greatest lower bound of two elements

     @param left One of the two values
  @param right The other value
  @return The greatest upper bound, that is, an element that is less or equal than the two arguments, and greater or equal than any other lower bound of the two arguments
    */
  def glbWithReplacement(left : T, right : T) : (T, Replacement)

  /**
  Computes widening of two elements

     @param left The previous value
  @param right The new value
  @return The widening of <code>left</code> and <code>right</code>
    */
  def wideningWithReplacement(left : T, right : T) : (T, Replacement)

}

/**
 * Some trivial helper functions that executes forward/backward semantics on single and list of states
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
object UtilitiesOnStates {

  def forwardExecuteStatement[S <: State[S]](state : S, statement : Statement) : (ExpressionSet, S)= {
    val finalState : S =statement.forwardSemantics[S](state);
    val expr=finalState.getExpression();
    (expr, finalState);
  }

  def backwardExecuteStatement[S <: State[S]](state : S, statement : Statement) : (ExpressionSet, S)= {
    val finalState : S =statement.backwardSemantics[S](state);
    val expr=finalState.getExpression();
    (expr, finalState.removeExpression());
  }

  def forwardExecuteListStatements[S <: State[S]](state : S, statements : List[Statement]) : (List[ExpressionSet], S)= statements match {
    case Nil => (Nil, state)
    case statement :: xs =>
      val state1 : S =statement.forwardSemantics[S](state);
      val expr=state1.getExpression();
      val (otherExpr, finalState)= forwardExecuteListStatements[S](state1, xs)
      (expr :: otherExpr, finalState.removeExpression());
  }

  def backwardExecuteListStatements[S <: State[S]](state : S, statements : List[Statement]) : (List[ExpressionSet], S)= statements match {
    case Nil => (Nil, state)
    case statement :: xs =>
      val state1 : S =statement.normalize().backwardSemantics[S](state);
      val expr=state1.getExpression();
      val (otherExpr, finalState)= backwardExecuteListStatements[S](state1, xs)
      (expr :: otherExpr, finalState);
  }
}