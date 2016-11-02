package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron.Polyhedra
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{Apron, NumericalDomain}
import ch.ethz.inf.pm.sample.execution.ForwardEntryStateBuilder
import ch.ethz.inf.pm.sample.oorepresentation.{MethodDeclaration, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.NumericalAnalysisState.SimpleNumericalAnalysisState

/**
  * @author Severin Münger
  *         Added on 02/11/16.
  */
trait NumericalAnalysisState[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]]
  extends SimpleState[T]
  with StateWithRefiningAnalysisStubs[T]
{
  this: T =>

  def numDom: N

  def copy(numDom: N = numDom): T

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable*/
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    copy(numDom = numDom.createVariable(x, typ))
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument*/
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): T = {
    copy(numDom = numDom.createVariable(x, typ))
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment*/
  override def assignVariable(x: Expression, right: Expression): T = {
    x match {
      case x: VariableIdentifier => copy(numDom = numDom.assign(x, right))
      case _ => this
    }
  }

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment*/
  override def assignField(obj: Expression, field: String, right: Expression): T = {
    // We don't track numerical information on the heap
    this
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable*/
  override def setVariableToTop(varExpr: Expression): T = {
    varExpr match {
      case ident: VariableIdentifier => copy(numDom = numDom.setToTop(ident))
      case _ => this
    }
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable*/
  override def removeVariable(varExpr: VariableIdentifier): T = {
    copy(numDom = numDom.removeVariable(varExpr))
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object on which the field access is performed
    * @param field the name of the field
    * @param typ   the type of the field
    * @return The abstract state obtained after the field access, that is,
    *         a new state whose `ExpressionSet` holds the symbolic representation of the value of the given field.*/
  override def getFieldValue(obj: Expression, field: String, typ: Type): T = {
    // We don't track numerical information on the heap
    this
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds*/
  override def assume(cond: Expression): T = {
    copy(numDom = numDom.assume(cond))
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified*/
  override def before(pp: ProgramPoint): T = {
    // Nothing to do here
    this
  }

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object*/
  override def createObject(typ: Type, pp: ProgramPoint): T = {
    // Nothing to do here
    this
  }

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant*/
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    // Nothing to do here
    this
  }

  /** Returns the current expression. */
  override def expr: ExpressionSet = {
    ExpressionSet()
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable*/
  override def getVariableValue(id: Identifier): T = {
    // Nothing to do here
    this
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): T = {
    // Nothing to do here
    this
  }

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = {
    // Nothing to do here
    this
  }

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression*/
  override def removeExpression(): T = {
    // Nothing to do here
    this
  }

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment*/
  override def setArgument(x: ExpressionSet, right: ExpressionSet): T = {
    // Nothing to do here
    this
  }

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one*/
  override def setExpression(expr: ExpressionSet): T = {
    // Nothing to do here
    this
  }

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown*/
  override def throws(t: ExpressionSet): T = ???

  override def ids: IdentifierSet = ???

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object*/
  override def factory(): T = {
    copy()
  }

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value*/
  override def top(): T = {
    copy(numDom = numDom.top())
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value*/
  override def bottom(): T = {
    copy(numDom = numDom.bottom())
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments*/
  override def lub(other: T): T = {
    copy(numDom = numDom.lub(other.numDom))
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments*/
  override def glb(other: T): T = {
    copy(numDom = numDom.glb(other.numDom))
  }

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`*/
  override def widening(other: T): T = {
    copy(numDom = numDom.widening(other.numDom))
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`*/
  override def lessEqual(other: T): Boolean = {
    numDom.lessEqual(other.numDom)
  }

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return bottom*/
  override def isBottom: Boolean = {
    numDom.isBottom
  }

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return bottom */
  override def isTop: Boolean = {
    numDom.isTop
  }
}

object NumericalAnalysisState
{
  /** The default implementation of the alias analysis state.
    *
    * @param numDom           The numerical domain lattice element.
    */
  case class SimpleNumericalAnalysisState(numDom: Apron.Polyhedra = Apron.Polyhedra.Bottom)
    extends NumericalAnalysisState[Apron.Polyhedra, SimpleNumericalAnalysisState]
  {
    /**
      *
      * @param numDom           The numerical domain lattice element.
      * @return The updated copy of the alias analysis state.
      */
    override def copy(numDom: Apron.Polyhedra = numDom): SimpleNumericalAnalysisState = {
      SimpleNumericalAnalysisState(numDom)
    }
  }
}

trait NumericalAnalysisStateBuilder[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]]
  extends ForwardEntryStateBuilder[T]
{
  override def build(method: MethodDeclaration): T = {
    val initial = topState.copy()
    method.initializeArgument(initial)
  }
}

object NumericalAnalysisEntryState
  extends NumericalAnalysisStateBuilder[Apron.Polyhedra, SimpleNumericalAnalysisState]
{
  override def topState: SimpleNumericalAnalysisState = SimpleNumericalAnalysisState()
}

case class PolyhedraState(override val numDom: Polyhedra)
  extends NumericalAnalysisState[Apron.Polyhedra, PolyhedraState] {

  override def copy(numDom: Polyhedra = numDom): PolyhedraState = {
    PolyhedraState(numDom)
  }
}