package ch.ethz.inf.pm.sample.abstractdomain
import ch.ethz.inf.pm.sample.oorepresentation._


/** 
 * A <code>SemanticDomain</code> is a domain on which some functions
 * on variables and expressions are provided
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
trait SemanticDomain[T <: SemanticDomain[T]] extends Lattice[T] {

  /**
   For each set of identifiers in the domain of f, this method merges these identifiers
   into the given one.

   @param f The identifiers to merge
   @return the state after the merge
   */
  def merge(f : Replacement) : T;

  /**
   This method returns representing the value of the given identifier
  
   @param id the identifier
   @return the string representing its state
   */
  def getStringOfId(id : Identifier) : String;
  
  /**
   This method sets to top a given variable
  
   @param variable the variable to be set to top
   @return the state after this action
   */
  def setToTop(variable : Identifier) : T;
  
  /**
   This method assigns a given variable to the given expression
  
   @param variable the variable to be assigned
   @param expr the expression to be assigned
   @return the state after this action
   */
  def assign(variable : Identifier, expr : Expression) : T;
  
  /**
   This method set an argument to the given expression
  
   @param variable the argument to set
   @param expr the expression to set
   @return the state after this action
   */
  def setArgument(variable : Identifier, expr : Expression) : T;
  
  /**
   This method assumes that a given expression holds
  
   @param expr the expression to be assumed
   @return the state after this action
   */ 
  def assume(expr : Expression) : T;
  
  /**
   This method creates a variable
  
   @param variable the variable to be created
   @param typ its type
   @return the state after this action
   */
  def createVariable(variable : Identifier, typ : Type) : T;
  
  /**
   This method creates a variable that is an argument of the analyzed method
  
   @param variable the variable to be created
   @param typ its type
   @return the state after this action and a map relating identifiers to the path starting with the argument
     to access them (this is useful for the heap domain that has to create abstract references to approximate 
     the initial heap structure)
   */
  def createVariableForArgument(variable : Identifier, typ : Type, path : List[String]) : (T, Map[Identifier, List[String]]);

  /**
   This method removed a variable
  
   @param variable the variable to be removed
   @param typ its type
   @return the state after this action
   */
  def removeVariable(variable : Identifier) : T;
  
  /**
   This method represents the semantics when accessing an identifier
  
   @param field the accessed id
   @return the state after this action
   */
  def access(field : Identifier) : T;
  
  /**
   This method represents the backward semantics when accessing an identifier
  
   @param field the accessed id
   @return the state before this action
   */
  def backwardAccess(field : Identifier) : T;
  
  /**
   This method provides the backward semantics of assignment
  
   @param variable
   @param expr 
   @return the state before variable=expr
   */
  def backwardAssign(variable : Identifier, expr : Expression) : T;

  /**
   This method returns all the ids over whom the SemanticDomain is defined

   @return all ids contained in the abstract domain
   */
  def getIds() : scala.collection.Set[Identifier]
}

/** 
 * A <code>SimplifiedSemanticDomain</code> is a simplified version of the <code>SemanticDomain</code>
 * Some methods are implemented relying on a common semantics that can be applied to 
 * the most part of the existing domains.
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
trait SimplifiedSemanticDomain[T <: SimplifiedSemanticDomain[T]] extends SemanticDomain[T] {
  override def setArgument(variable : Identifier, expr : Expression) : T = this.assign(variable, expr);
  override def createVariableForArgument(variable : Identifier, typ : Type, path : List[String]) : (T, Map[Identifier, List[String]]) = {
    var result = Map.empty[Identifier, List[String]];
    result=result+((variable, path ::: variable.toString() :: Nil));
    return (this.createVariable(variable, typ), result);
  }
  
  override def access(field : Identifier) : T = this.asInstanceOf[T];
  override def backwardAccess(field : Identifier) : T = throw new SymbolicSemanticException("Backward analysis not supported");
  override def backwardAssign(variable : Identifier, expr : Expression) : T = throw new SymbolicSemanticException("Backward analysis not supported");
}


class SymbolicSemanticException(message : String) extends Exception(message)
class SemanticException(message : String) extends Exception(message)
