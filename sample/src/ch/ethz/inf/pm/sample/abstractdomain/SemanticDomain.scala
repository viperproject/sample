package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation._

/**
 * A <code>SemanticDomain</code> is a domain on which some functions
 * on variables and expressions are provided
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
trait SemanticDomain[T <: SemanticDomain[T]]
  extends Lattice[T] {
  this: T =>

  /**
   * For each set of identifiers in the domain of f, this method merges these identifiers
   * into the given one.
   *
   * @param f The identifiers to merge
   * @return the state after the merge
   */
  def merge(f: Replacement): T

  /**
   * This method returns representing the value of the given identifier
   *
   * @param id the identifier
   * @return the string representing its state
   */
  def getStringOfId(id: Identifier): String

  /**
   * This method sets to top a given variable
   *
   * @param variable the variable to be set to top
   * @return the state after this action
   */
  def setToTop(variable: Identifier): T

  /**
   * This method assigns a given variable to the given expression
   *
   * @param variable the variable to be assigned
   * @param expr the expression to be assigned
   * @return the state after this action
   */
  def assign(variable: Identifier, expr: Expression): T

  /**
   * This method set an argument to the given expression
   *
   * @param variable the argument to set
   * @param expr the expression to set@return the state after this action
   *
   */
  def setArgument(variable: Identifier, expr: Expression): T

  /**
   * This method assumes that a given expression hold
   *
   * @param expr the expression to be assumed
   * @return the state after this action
   */
  def assume(expr: Expression): T

  def areEqual(left: Expression, right: Expression): BooleanDomain = {
    val equalsExpression = BinaryArithmeticExpression(left, right, ArithmeticOperator.==, null)

    val leftEqualsRight = this.assume(equalsExpression)
    val leftNotEqualsRight = this.assume(NegatedBooleanExpression(equalsExpression))

    if (!leftEqualsRight.lessEqual(this.bottom()) && leftNotEqualsRight.lessEqual(this.bottom())) {
      // must be equal
      return BooleanDomain.True
    } else if (leftEqualsRight.lessEqual(this.bottom())) {
      // must be not equal
      return BooleanDomain.False
    }

    BooleanDomain.Top
  }

  /**
   * This method creates a variable.
   *
   * @param variable the variable to be created
   * @param typ its type
   * @return the state after this action
   */
  def createVariable(variable: Identifier, typ: Type): T

  /**
   * Creates a variable whose type is the type of the given identifier.
   */
  def createVariable(variable: Identifier): T =
    createVariable(variable, variable.typ)

  /**
   * Returns a copy of this state with all given variables created.
   */
  def createVariables[I <: Identifier](variables: Set[I]): T =
    variables.foldLeft(this)(_.createVariable(_))

  /**
  This method creates a variable that is an argument of the analyzed method
  
   @param variable the variable to be created
  @param typ its type
  @return the state after this action and a map relating identifiers to the path starting with the argument
     to access them (this is useful for the heap domain that has to create abstract references to approximate 
     the initial heap structure)
    */
  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]): (T, Map[Identifier, List[String]])

  /**
   * This method removed a variable.
   * @param id the variable to be removed
   * @return the state after this action
   */
  def removeVariable(id: Identifier): T

  /**
   * Removes a set of variables from the state.
   * @param variables the variables to be removed
   * @return the state after this action
   */
  def removeVariables[I <: Identifier](variables: Set[I]): T =
    variables.foldLeft(this)(_.removeVariable(_))

  /**
   * This method represents the semantics when accessing an identifier
   *
   * @param id the accessed id
   * @return the state after this action
   */
  def access(id: Identifier): T

  /**
   * This method represents the backward semantics when accessing an identifier
   *
   * @param id the accessed id
   * @return the state before this action
   */
  def backwardAccess(id: Identifier): T

  /**
   * This method provides the backward semantics of assignment
   *
   * @param id the assigned id
   * @param expr the right hand side
   * @return the state before variable = expr
   */
  def backwardAssign(oldPreState: T, id: Identifier, expr: Expression): T

  /** Returns all identifiers over whom the `SemanticDomain` is defined. */
  def ids: Set[Identifier]

  /**
   * This method renames variable form the list <code>form</code> to variables form the list <code>to</code>
   * so that in the resulting state a variable with name from(i) has the name to(i).
   * @param from
   * @param to
   * @return state after renaming variables from first list to variables in second list
   *
   * @author Milos Novacek
   */
  def rename(from: List[Identifier], to: List[Identifier]): T = {
    assert(from.length == to.length)
    rename((from zip to).toMap)
  }

  def rename[I <: Identifier, J <: Identifier](map: Map[I, J]): T = {
    val replacement = new Replacement(isPureRenaming = true)
    for ((from, to) <- map)
      replacement.value += (Set[Identifier](from) -> Set[Identifier](to))
    merge(replacement)
  }

  /**
   * May try to explain an error
   *
   * @param expr An error-expression that should be infeasible but exposes an error
   * @return If a cause of the error is found, it returns an explanation and the program point of the cause
   */
  def explainError(expr: Expression): Set[(String, ProgramPoint)] = Set.empty

}

/** Semantic domain whose methods do not change the state in any way.
  * Use it as mixin to turn an arbitrary `Lattice` into a `SemanticDomain`.
  * This is useful when building a `SemanticCartesianProductDomain`
  * of a `SemanticDomain` and some other `Lattice`.
  * @tparam T the self-type of the lattice
  */
trait DummySemanticDomain[T <: DummySemanticDomain[T]] extends SemanticDomain[T] {
  this: T =>
  def ids = Set.empty

  def backwardAssign(oldPreState: T, variable: Identifier, expr: Expression) = this

  def backwardAccess(field: Identifier) = this

  def access(field: Identifier) = this

  def removeVariable(variable: Identifier) = this

  def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = (this, Map.empty)

  def createVariable(variable: Identifier, typ: Type) = this

  def assume(expr: Expression) = this

  def setArgument(variable: Identifier, expr: Expression) = this

  def assign(variable: Identifier, expr: Expression) = this

  def setToTop(variable: Identifier) = this

  def getStringOfId(id: Identifier): String = ""

  def merge(f: Replacement) = this
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
  this: T =>
  override def setArgument(variable: Identifier, expr: Expression): T = this.assign(variable, expr)

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]): (T, Map[Identifier, List[String]]) = {
    var result = Map.empty[Identifier, List[String]]
    result = result + ((variable, path ::: variable.toString :: Nil))
    (this.createVariable(variable, typ), result)
  }

  override def access(field: Identifier): T = this

  override def backwardAccess(field: Identifier): T = throw new SymbolicSemanticException("Backward analysis not supported")

  override def backwardAssign(oldPreState: T, variable: Identifier, expr: Expression): T = throw new SymbolicSemanticException("Backward analysis not supported")
}


class SymbolicSemanticException(message: String) extends Exception(message)

class SemanticException(message: String) extends Exception(message)
