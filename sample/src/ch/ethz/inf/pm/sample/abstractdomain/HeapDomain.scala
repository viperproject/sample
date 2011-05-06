package ch.ethz.inf.pm.sample.abstractdomain
import ch.ethz.inf.pm.sample.oorepresentation._

/**
 * A <code>Replacement</code> is a map from sets of identifiers to identifier.
 * Each relation in the map represents the set of identifiers that should be
 * merged into the given id.
 *
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
class Replacement extends scala.collection.mutable.HashMap[Set[Identifier], Identifier]

/**
 * A <code>HeapDomain</code> is a domain aimed at tracking information
 * about the heap structure.
 *
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
trait HeapDomain[T <: HeapDomain[T, I], I <: HeapIdentifier[I]] extends Analysis {

  /**
   This method creates an object of a given type
  
   @param typ The type of the object to be created
   @param pp The point of the program that creates the reference
   @return the identifier of the created object and the state of the heap after that
   */ 
  def createObject(typ : Type, pp : ProgramPoint) : (I, T, Replacement);
  
  /**
   This method returns the identifier of the field of an object
  
   @param objectIdentifier the identifier of the object to be accessed
   @param field the name of the field
   @param typ the type of the accessed field
   @return the identifier of accessed field and the state of the heap after that
   */ 
  def getFieldIdentifier(objectIdentifier : Expression, name : String, typ : Type) : (I, T, Replacement);

  /**
   This method sets to top a given variable

   @param variable the variable to be set to top
   @return the state after this action
   */
  def setToTop(variable : Identifier) : (T, Replacement);

  /**
   This method assigns a given variable to the given expression

   @param variable the variable to be assigned
   @param expr the expression to be assigned
   @return the state after this action
   */
  def assign(variable : Identifier, expr : Expression) : (T, Replacement);

  /**
   This method set a paramenter (usually the parameter passed to a method) to the given expression

   @param variable the variable to set
   @param expr the expression to set
   @return the state after this action
   */
  def setParameter(variable : Identifier, expr : Expression) : (T, Replacement);

  /**
   This method assumes that a given expression holds

   @param expr the expression to be assumed
   @return the state after this action
   */
  def assume(expr : Expression) : (T, Replacement);

  /**
   This method creates a variable

   @param variable the variable to be created
   @param typ its type
   @return the state after this action
   */
  def createVariable(variable : Identifier, typ : Type) : (T, Replacement);

  /**
   This method creates a variable that is a parameter of the analyzed method

   @param variable the variable to be created
   @param typ its type
   @return the state after this action and a map relating identifiers to the path starting with the parameter
     to access them (this is useful for the heap domain that has to create abstract references to approximate
     the initial heap structure)
   */
  def createVariableForParameter(variable : Identifier, typ : Type, path : List[String]) : (T, Map[Identifier, List[String]], Replacement);

  /**
   This method removed a variable

   @param variable the variable to be removed
   @param typ its type
   @return the state after this action
   */
  def removeVariable(variable : Identifier) : (T, Replacement);


  /**
   This method provides the backward semantics of assignment

   @param variable
   @param expr
   @return the state before variable=expr
   */
  def backwardAssign(variable : Identifier, expr : Expression) : (T, Replacement);

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
  def heaplub(left : T, right : T) : (T, Replacement)

  /**
   Computes the greatest lower bound of two elements

   @param left One of the two values
   @param right The other value
   @return The greatest upper bound, that is, an element that is less or equal than the two arguments, and greater or equal than any other lower bound of the two arguments
   */
  def heapglb(left : T, right : T) : (T, Replacement)

  /**
   Computes widening of two elements

   @param left The previous value
   @param right The new value
   @return The widening of <code>left</code> and <code>right</code>
   */
  def heapwidening(left : T, right : T) : (T, Replacement)

  /**
   Returns true iff <code>this</code> is less or equal than <code>r</code>

   @param r The value to compare
   @return true iff <code>this</code> is less or equal than <code>r</code>
   */
  def lessEqual(r : T) : Boolean

}

trait AddressedDomain[I <: HeapIdentifier[I]] {
  def getAddresses() : Set[I];
}