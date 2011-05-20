package ch.ethz.inf.pm.sample.abstractdomain
import ch.ethz.inf.pm.sample.oorepresentation._

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
 * @author Pietro Ferrara
 * @version 0.1
 */
class Replacement(val value : scala.collection.mutable.HashMap[Set[Identifier], Set[Identifier]]) extends  {
  def this() {
    this(new scala.collection.mutable.HashMap[Set[Identifier], Set[Identifier]]());
  }
  //I'm not sure if this cast is fine
  def lub(l : Replacement, r : Replacement) : Replacement = new Replacement(l.value.++(r.value));

  def glb(l : Replacement, r : Replacement) : Replacement = new Replacement(
    l.value.retain( {
        case (a, b) => r.value.keySet.contains(a) && r.value.apply(a).equals(b);
      }
    )
  )

  def isEmpty() = value.isEmpty;

  def keySet() = value.keySet;

  def apply(k : Set[Identifier]) = value.apply(k);
}

/**
 * A <code>HeapDomain</code> is a domain aimed at tracking information
 * about the heap structure.
 *
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
trait HeapDomain[T <: HeapDomain[T, I], I <: HeapIdentifier[I]] extends Analysis with LatticeWithReplacement[T] {

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
   This method returns the identifier of the cell of an array

   @param arrayIdentifier the identifier of the array to be accessed
   @param index the index used to access the array
   @param state the state of the semantic domain at that point. This could be useful to precisely
   analyze inside which bounds of the array the access is
   @param typ the type of the accessed cell
   @return the identifier of accessed cell, the state of the heap after that (since we could create new
   abstract ids when accessing the array in order to be more precise), and the eventual replacements (e.g.,
   if the heap analyzed has summarize or splitted some cells)
   */
  def getArrayCell[S <: SemanticDomain[S]](arrayIdentifier : Expression, index : Expression, state : S, typ : Type) : (I, T, Replacement);

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
   @param state the state of the semantic domain at that point
   @return the state after this action and the eventual replacements (e.g.,
   if the heap analyzed has summarize or splitted some cells)
   */
  def assign[S <: SemanticDomain[S]](variable : Identifier, expr : Expression, state : S) : (T, Replacement);

  /**
   This method assigns a given field of a given objectto the given expression

   @param obj the object whose field has to be assigned
   @param field the field to be assigned
   @param expr the expression to be assigned
   @return the state after this action and the eventual replacements (e.g.,
   if the heap analyzed has summarize or splitted some cells)
   */
  def assignField(obj : Identifier, field : String, expr : Expression) : (T, Replacement);

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

}

trait AddressedDomain[I <: HeapIdentifier[I]] {
  def getAddresses() : Set[I];
}