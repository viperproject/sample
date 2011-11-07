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
  def factory() : T;
  
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
   @return The abstract state after the creation of the object
   */
  def createObject(typ : Type, pp : ProgramPoint) : S
  
  	
  /**
   Creates an object for an argument. Its semantics is different from <code>createObject</code> since this object is used
   to create the initial state of the heap.  
   
   @param typ The static type of the argument
   @param pp The point of the program that contains the declaration of the argument
   @param path One of the possible shortest path to reach this argument
   @return The abstract state after the creation of the object for the argument
   */
  def createObjectForArgument(typ : Type, pp : ProgramPoint, path : List[String]) : S
  
  /**
   Creates a variable
   
   @param x The name of the variable
   @param typ The static type of the variable                                    \
   @param pp The program point that creates the variable
   @return The abstract state after the creation of the variable
   */
  def createVariable(x : SymbolicAbstractValue[S], typ : Type, pp : ProgramPoint) : S
  
  /**
   Creates a variable for an argument
   
   @param x The name of the argument
   @param pp The static type of the argument
   @return The abstract state after the creation of the argument
   */  
  def createVariableForArgument(x : SymbolicAbstractValue[S], typ : Type) : S

  /**
   Assigns an expression to a variable
   
   @param x The assigned variable
   @param right The assigned expression
   @return The abstract state after the assignment
   */
  def assignVariable(x : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S]) : S

  /**
   Create an array of length

   @param length The length of the array
   @param typ The type of the array
   @param length The program point that created the array
   @return The abstract state after the creation of the array
   */
  def createArray(length : SymbolicAbstractValue[S], typ : Type, pp : ProgramPoint) : S

  /**
   Assigns an expression to a field of an object

   @param obj The object whose field is assigned
   @param field The assigned field
   @param right The assigned expression
   @return The abstract state after the assignment
   */
  def assignField(obj : List[SymbolicAbstractValue[S]], field : String, right : SymbolicAbstractValue[S]) : S

  /**
   Assign a cell of an array

   @param obj The object on which the array assignment
   @param index The assigned index
   @param typ The type of the cell
   @param right The assigned expression
   @return The abstract state obtained after the array cell assignment
   */
  def assignArrayCell(obj : SymbolicAbstractValue[S], index : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S], typ : Type) : S
  
  /**
   Assigns an expression to an argument
   
   @param x The assigned argument
   @param right The expression to be assigned
   @return The abstract state after the assignment
   */
  def setArgument(x : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S]) : S
  
  /**
   Forgets the value of a variable
   
   @param x The variable to be forgotten
   @return The abstract state obtained after forgetting the variable
   */
  def setVariableToTop(x : SymbolicAbstractValue[S]) : S
  
  /**
   Removes a variable
   
   @param x The variable to be removed
   @return The abstract state obtained after removing the variable
   */
  def removeVariable(x : SymbolicAbstractValue[S]) : S
  
  /**
   Throws an exception
   
   @param t The thrown exception
   @return The abstract state after the thrown
   */
  def throws(t : SymbolicAbstractValue[S]) : S
  
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
  def getFieldValue(obj : List[SymbolicAbstractValue[S]], field : String, typ : Type) : S


   /**
   Accesses a field of an object

   @param obj The object on which the field access is performed
   @param field The name of the field
   @param typ The type of the field
   @return The abstract state obtained after the field access, that is, the state that contains as expression the symbolic representation of the value of the given field access
   */
  def getArrayCell(obj : SymbolicAbstractValue[S], index : SymbolicAbstractValue[S], typ : Type) : S


  /**
   Returns the identifier representing the length of the given array

   @param array The array from which we want to access the length
   @return A state that contains as expression the symbolic representation of the length of the given array
   */
  def getArrayLength(array : SymbolicAbstractValue[S]) : S

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
  def backwardGetFieldValue(objs : List[SymbolicAbstractValue[S]], field : String, typ : Type) : S
  
  /**
   Performs the backward semantics of an assignment
   
   @param x The assigned variable
   @param right The assigned expression
   @return The abstract state before the assignment
   */
  def backwardAssignVariable(x : SymbolicAbstractValue[S], right : SymbolicAbstractValue[S]) : S
  
  /**
   Evaluates a numerical constant
   
   @param value The string representing the numerical constant
   @param typ The type of the numerical constant
   @param pp The program point that contains the constant
   @return The abstract state after the evaluation of the constant, that is, the state that contains an expression representing this constant
   */
  def evalNumericalConstant(value : String, typ : Type, pp : ProgramPoint) : S
  
  /**
   Assumes that a boolean expression holds
   
   @param cond The assumed expression 
   @return The abstract state after assuming that the expression holds
   */
  def assume(cond : SymbolicAbstractValue[S]) : S
  
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
  def getExpression() : SymbolicAbstractValue[S]
  
  /**
   Sets the current expression
   
   @param expr The current expression
   @return The abstract state after changing the current expression with the given one
   */
  def setExpression(expr : SymbolicAbstractValue[S]) : S
  
  /**
   Removes the current expression
   
   @return The abstract state after removing the current expression
   */
  def removeExpression() : S

}


/**
 * The representation of a <a href="http://en.wikipedia.org/wiki/Lattice_%28order%29">lattice</a> structure
 * for the heap analysis. It is similar to LatticeWithReplacement but lub, glb and widening require the state
 * of the semantic domain in order to refine their precision.
 *
 * @param <T> The current type of the HeapLattice
 * @author Pietro Ferrara
 * @since 0.1
 */
trait HeapLattice[T <: HeapLattice[T]] {

  /**
   Returns a new instance of the lattice
   @return A new instance of the current object
   */
  def factory() : T;

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
     @param leftSemantic The state of the semantic domain on the left state
     @param rightSemantic The state of the semantic domain on the right state
     @return The least upper bound, that is, an element that is greater or equal than the two arguments
     */
    def lubWithReplacement[S <: SemanticDomain[S]](left : T, right : T, leftSemantic : S, rightSemantic : S) : (T, Replacement)

    /**
     Computes the greatest lower bound of two elements

     @param left One of the two values
     @param right The other value
     @param leftSemantic The state of the semantic domain on the left state
     @param rightSemantic The state of the semantic domain on the right state
     @return The greatest upper bound, that is, an element that is less or equal than the two arguments, and greater or equal than any other lower bound of the two arguments
     */
    def glbWithReplacement[S <: SemanticDomain[S]](left : T, right : T, leftSemantic : S, rightSemantic : S) : (T, Replacement)

    /**
     Computes widening of two elements

     @param left The previous value
     @param right The new value
     @param leftSemantic The state of the semantic domain on the left state
     @param rightSemantic The state of the semantic domain on the right state
     @return The widening of <code>left</code> and <code>right</code>
     */
    def wideningWithReplacement[S <: SemanticDomain[S]](left : T, right : T, leftSemantic : S, rightSemantic : S) : (T, Replacement)

  /**
   Returns true iff <code>this</code> is less or equal than <code>r</code>

   @param r The value to compare
   @return true iff <code>this</code> is less or equal than <code>r</code>, and a replacement to compare the two states
   */
  def lessEqualWithReplacement(r : T) : (Boolean, Replacement)
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
   Returns a new instance of the lattice
   @return A new instance of the current object
   */
  def factory() : T;

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

  /**
   Returns true iff <code>this</code> is less or equal than <code>r</code>

   @param r The value to compare
   @return true iff <code>this</code> is less or equal than <code>r</code>, and a replacement to compare the two states
   */
  def lessEqualWithReplacement(r : T) : (Boolean, Replacement)
}

/** 
 * Some trivial helper functions that executes forward/backward semantics on single and list of states
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
object UtilitiesOnStates {
   
	def forwardExecuteStatement[S <: State[S]](state : S, statement : Statement) : (SymbolicAbstractValue[S], S)= {
	  val finalState : S =statement.forwardSemantics[S](state);
	  val expr=finalState.getExpression();
	  var resultingState=finalState.bottom()
	  for(exp <- expr.getExpressions())
		  resultingState=resultingState.lub(resultingState, expr.get(exp));
	  (expr, resultingState);
	}

 	def backwardExecuteStatement[S <: State[S]](state : S, statement : Statement) : (SymbolicAbstractValue[S], S)= {
	  val finalState : S =statement.backwardSemantics[S](state);
	  val expr=finalState.getExpression();
	  (expr, finalState.removeExpression());
	}
 
	def forwardExecuteListStatements[S <: State[S]](state : S, statements : List[Statement]) : (List[SymbolicAbstractValue[S]], S)= statements match {
	  case Nil => (Nil, state)
	  case statement :: xs =>
	  	val state1 : S =statement.forwardSemantics[S](state);
	  	val expr=state1.getExpression();
	  	val (otherExpr, finalState)= forwardExecuteListStatements[S](state1, xs)
	  	(expr :: otherExpr, finalState.removeExpression());
	}
 
	def backwardExecuteListStatements[S <: State[S]](state : S, statements : List[Statement]) : (List[SymbolicAbstractValue[S]], S)= statements match {
	  case Nil => (Nil, state)
	  case statement :: xs =>
	  	val state1 : S =statement.normalize().backwardSemantics[S](state);
	  	val expr=state1.getExpression();
	  	val (otherExpr, finalState)= backwardExecuteListStatements[S](state1, xs)
	  	(expr :: otherExpr, finalState);
	}
}