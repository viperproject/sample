package ch.ethz.inf.pm.sample.stupidanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Apron
import ch.ethz.inf.pm.sample.abstractdomain.vdha.{HeapGraph, DefaultHeapEntryStateBuilder, ValueDrivenHeapState}
import ch.ethz.inf.pm.sample.execution.{EntryStateBuilder, SimpleAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.sil.SilAnalysisRunner
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}

/** Stupid Analysis State
  *
  * @author Caterina Urban
  */
case class StupidState(expressionSet: ExpressionSet, numerical: Apron.Polyhedra)
    extends SimpleState[StupidState]
    with StateWithBackwardAnalysisStubs[StupidState]
{
  /** Creates a variable given a `VariableIdentifier`.
    * Implementations can already assume that this state is non-bottom.
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): StupidState =
    copy(numerical = numerical.createVariable(x))

  /** Creates an argument variable given a `VariableIdentifier`.
    * Implementations can already assume that this state is non-bottom.
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): StupidState = this

  /** Removes the given variable.
    * Implementations can assume this state is non-bottom
    */
  override def removeVariable(varExpr: VariableIdentifier): StupidState =
    copy(numerical = numerical.removeVariable(varExpr))

  /** Returns a new state whose `ExpressionSet` holds the value of the given field.
    * Implementations can already assume that this state is non-bottom.
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): StupidState = this //

  /** Assumes an expression.
    * Implementations can already assume that this state is non-bottom.
    */
  override def assume(cond: Expression): StupidState = this.copy(numerical = numerical.assume(cond))

  /** Assigns an expression to a variable.
    * Implementations can already assume that this state is non-bottom.
    */
  override def assignVariable(x: Expression, right: Expression): StupidState = x match {
    case i: Identifier => this.copy(numerical = numerical.assign(i,right))
    case _ => sys.error("No idea what to do here")
  }

  /** Sets given variable/ids to top
    * Implementations can assume this state is non-bottom
    */
  override def setVariableToTop(varExpr: Expression): StupidState = varExpr match {
    case i: Identifier => this.copy(numerical = numerical.setToTop(i))
    case _ => sys.error("No idea what to do here")
  }

  /** Assigns an expression to a field.
    * Implementations can already assume that this state is non-bottom.
    */
  override def assignField(obj: Expression, field: String, right: Expression): StupidState = ???

  /**
   * Assigns an expression to an argument
   *
   * @param x The assigned argument
   * @param right The expression to be assigned
   * @return The abstract state after the assignment
   */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): StupidState = ???

  /**
   * Removes the current expression
   *
   * @return The abstract state after removing the current expression
   */
  override def removeExpression(): StupidState = copy(expressionSet = ExpressionSet()).setUnitExpression()

  /**
   * Throws an exception
   *
   * @param t The thrown exception
   * @return The abstract state after the thrown
   */
  override def throws(t: ExpressionSet): StupidState = ???

  /**
   * Removes all variables satisfying filter
   */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): StupidState = ???

  /**
   * Evaluates a numerical constant
   *
   * @param value The string representing the numerical constant
   * @param typ The type of the numerical constant
   * @param pp The program point that contains the constant
   * @return The abstract state after the evaluation of the constant, that is, the
   *         state that contains an expression representing this constant
   */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): StupidState = {
    if (this.isBottom) return this
    this.setExpression(ExpressionSet(new Constant(value, typ, pp)))
  }

  /**
   * Signals that we are going to analyze the statement at program point pp
   * This is particularly important to eventually partition a state following
   * the specified directives
   *
   * @param pp The point of the program that is going to be analyzed
   * @return The abstract state eventually modified
   */
  override def before(pp: ProgramPoint): StupidState = this

  /**
   * Performs abstract garbage collection
   */
  override def pruneUnreachableHeap(): StupidState = this

  /** Returns the current expression */
  override def expr: ExpressionSet = this.expressionSet

  /**
   * Creates an object
   *
   * @param typ The dynamic type of the created object
   * @param pp The point of the program that creates the object
   * @return The abstract state after the creation of the object
   */
  override def createObject(typ: Type, pp: ProgramPoint): StupidState = this

  /**
   * Sets the current expression
   *
   * @param expr The current expression
   * @return The abstract state after changing the current expression with the given one
   */
  override def setExpression(expr: ExpressionSet): StupidState = this.copy(expressionSet = expr)

  /**
   * Gets the value of a variable
   *
   * @param id The variable to access
   * @return The abstract state obtained after accessing the variable, that is, the state that contains
   *         as expression the symbolic representation of the value of the given variable
   */
  override def getVariableValue(id: Identifier): StupidState = copy(expressionSet = new ExpressionSet(id.typ).add(id))

  /**
   * Returns the bottom value of the lattice
   *
   * @return The bottom value, that is, a value x that is less or equal than any other value
   */
  override def bottom(): StupidState = StupidState(expressionSet.bottom(),numerical.bottom())

  /**
   * Computes widening of two elements
   *
   * @param other The new value
   * @return The widening of <code>left</code> and <code>right</code>
   */
  override def widening(other: StupidState): StupidState =
    StupidState(expressionSet widening other.expressionSet,numerical widening other.numerical)

  /**
   * Returns true iff <code>this</code> is less or equal than <code>r</code>
   *
   * @param other The value to compare
   * @return true iff <code>this</code> is less or equal than <code>r</code>
   */
  override def lessEqual(other: StupidState): Boolean = numerical lessEqual other.numerical

  /**
   * Returns the top value of the lattice
   *
   * @return The top value, that is, a value x that is greater or equal than any other value
   */
  override def top(): StupidState = StupidState(expressionSet.top(),numerical.top())

  /**
   * Computes the upper bound of two elements
   *
   * @param other The other value
   * @return The least upper bound, that is, an element that is greater or equal than the two arguments
   */
  override def lub(other: StupidState): StupidState =
    StupidState(expressionSet lub other.expressionSet,numerical lub other.numerical)

  /**
   * Returns a new instance of the lattice
   *
   * @return A new instance of the current object
   */
  override def factory(): StupidState = StupidState(expressionSet.factory(),numerical.factory())

  /**
   * Computes the greatest lower bound of two elements
   *
   * @param other The other value
   * @return The greatest upper bound, that is, an element that is less or equal than the two arguments,
   *         and greater or equal than any other lower bound of the two arguments
   */
  override def glb(other: StupidState): StupidState =
    StupidState(expressionSet glb other.expressionSet,numerical glb other.numerical)

  /**
   * Checks whether the given domain element is equivalent to bottom ("false")
   * @return bottom
   */
  override def isBottom: Boolean = expressionSet.isBottom || numerical.isBottom

  /**
   * Checks whether the given domain element is equivalent to bottom ("false")
   * @return bottom
   */
  override def isTop: Boolean = expressionSet.isTop && numerical.isTop
}

object StupidAnalysisRunner extends SilAnalysisRunner[StupidState] {
  val analysis = SimpleAnalysis[StupidState](StupidEntryStateBuilder)

  override def toString = "Stupid Analysis"
}

object StupidEntryStateBuilder extends EntryStateBuilder[StupidState] {
  override def topState: StupidState = StupidState(ExpressionSet(),Apron.Polyhedra.Top.factory()).setUnitExpression()
}