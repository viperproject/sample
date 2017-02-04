/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.{Apron, IntegerOctagons, NumericalDomain}
import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.execution.SilverEntryStateBuilder
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverMethodDeclaration, SilverProgramDeclaration}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}

/**
  * @author Severin Münger
  *         Added on 02/11/16.
  */
sealed trait NumericalAnalysisState[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]]
  extends SimpleState[T]
  with StateWithRefiningAnalysisStubs[T]
{
  this: T =>

  def numDom: N

  def currentPP: ProgramPoint

  def copy(currentPP: ProgramPoint = currentPP, expr: ExpressionSet = expr, numDom: N = numDom): T

  override def command(cmd: Command): T = this

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
    if (typ.isObject) {
      copy(expr = ExpressionSet(x), numDom = numDom.createVariable(x, typ))
    } else {
      copy(numDom = numDom.createVariable(x, typ))
    }
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment*/
  override def assignVariable(x: Expression, right: Expression): T = x match {
    case left: VariableIdentifier =>
      val newNumDom =
        if (right.ids.toSetOrFail.forall {
          case _: VariableIdentifier => true
          case _ => false
        }) {
          numDom.assign(left, right)
        }
        else numDom.setToTop(left)
      copy(numDom = newNumDom)
    case _ => throw new IllegalStateException()
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
  override def setVariableToTop(varExpr: Expression): T = varExpr match {
    case id: VariableIdentifier => this.copy(numDom = numDom.setToTop(id))
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable*/
  override def removeVariable(varExpr: VariableIdentifier): T = {
    copy(expr = ExpressionSet(varExpr), numDom = numDom.removeVariable(varExpr))
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
    obj match {
      case obj: Identifier =>
        val fieldId = VariableIdentifier(field)(typ, currentPP)
        copy(expr = ExpressionSet(AccessPathIdentifier((obj :: Nil) :+ fieldId)))
      case _ => this
    }
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds*/
  override def assume(cond: Expression): T = {
    val condCNFConjuncts = Utils.toCNFConjuncts(cond)
    var newNumDom = numDom
    condCNFConjuncts.foreach { conjunct =>
      if (conjunct.contains {
        case _: AccessPathIdentifier => true
        case _ => false
      }) {
        conjunct.ids.toSetOrFail.foreach(id => if (newNumDom.ids.contains(id)) newNumDom = newNumDom.setToTop(id))
      }
      else newNumDom = newNumDom.assume(conjunct)
    }
    copy(numDom = newNumDom)
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified*/
  override def before(pp: ProgramPoint): T = {
    // Nothing to do here
    copy(currentPP = pp)
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
    copy(expr = ExpressionSet(Constant(value, typ, pp)))
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable*/
  override def getVariableValue(id: Identifier): T = {
    copy(expr = ExpressionSet(id))
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): T = throw new UnsupportedOperationException()

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = throw new UnsupportedOperationException()

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression*/
  override def removeExpression(): T = {
    copy(expr = ExpressionSet())
  }

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment*/
  override def setArgument(x: ExpressionSet, right: ExpressionSet): T = throw new UnsupportedOperationException()

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one*/
  override def setExpression(expr: ExpressionSet): T = {
    copy(expr = expr)
  }

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown*/
  override def throws(t: ExpressionSet): T = throw new UnsupportedOperationException()

  override def ids: IdentifierSet = numDom.ids

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object*/
  override def factory(): T =
    copy(
      expr = ExpressionSet(),
      currentPP = DummyProgramPoint,
      numDom = numDom.factory())

  def isTop: Boolean = numDom.isTop

  def isBottom: Boolean = numDom.isBottom

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments*/
  override def lub(other: T): T = {
    copy(
      numDom = numDom.lub(other.numDom),
      expr = expr.lub(other.expr))
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments*/
  override def glb(other: T): T = {
    copy(
      numDom = numDom.glb(other.numDom),
      expr = expr.glb(other.expr))
  }

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`*/
  override def widening(other: T): T = {
    copy(
      expr = expr.widening(other.expr),
      numDom = numDom.widening(other.numDom))
  }

}

case class PolyhedraAnalysisState(currentPP: ProgramPoint = DummyProgramPoint,
                                  expr: ExpressionSet = ExpressionSet(),
                                  numDom: Apron.Polyhedra = Apron.Polyhedra.Bottom.factory())
  extends NumericalAnalysisState[Apron.Polyhedra, PolyhedraAnalysisState]
{
  override def copy(currentPP: ProgramPoint = currentPP,
                    expr: ExpressionSet = expr,
                    numDom: Apron.Polyhedra = numDom): PolyhedraAnalysisState =
    PolyhedraAnalysisState(currentPP, expr, numDom)

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value*/
  override def top(): PolyhedraAnalysisState = PolyhedraAnalysisState.Top

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value */
  override def bottom(): PolyhedraAnalysisState = PolyhedraAnalysisState.Bottom

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`*/
  override def lessEqual(other: PolyhedraAnalysisState): Boolean = (this, other) match {
    case (PolyhedraAnalysisState.Bottom, _) | (_, PolyhedraAnalysisState.Top) => true
    case (PolyhedraAnalysisState.Top, _) | (_, PolyhedraAnalysisState.Bottom) => false
    case _ => numDom.lessEqual(other.numDom)
  }
}

object PolyhedraAnalysisState {
  object Top extends PolyhedraAnalysisState(numDom = Apron.Polyhedra.Top)

  object Bottom extends PolyhedraAnalysisState(numDom = Apron.Polyhedra.Bottom)
}

case class OctagonAnalysisState(currentPP: ProgramPoint = DummyProgramPoint,
                                expr: ExpressionSet = ExpressionSet(),
                                numDom: IntegerOctagons = IntegerOctagons.Bottom)
  extends NumericalAnalysisState[IntegerOctagons, OctagonAnalysisState]
{
  override def copy(currentPP: ProgramPoint = currentPP,
                    expr: ExpressionSet = expr,
                    numDom: IntegerOctagons = numDom): OctagonAnalysisState =
    OctagonAnalysisState(currentPP, expr, numDom)

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value*/
  override def top(): OctagonAnalysisState = OctagonAnalysisState.Top

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value */
  override def bottom(): OctagonAnalysisState = OctagonAnalysisState.Bottom

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`*/
  override def lessEqual(other: OctagonAnalysisState): Boolean = (this, other) match {
    case (OctagonAnalysisState.Bottom, _) | (_, OctagonAnalysisState.Top) => true
    case (OctagonAnalysisState.Top, _) | (_, OctagonAnalysisState.Bottom) => false
    case _ => numDom.lessEqual(other.numDom)
  }
}

object OctagonAnalysisState {
  object Top extends OctagonAnalysisState(numDom = IntegerOctagons.Top)

  object Bottom extends OctagonAnalysisState(numDom = IntegerOctagons.Bottom)
}

trait NumericalAnalysisStateBuilder[N <: NumericalDomain[N], T <: NumericalAnalysisState[N, T]] extends SilverEntryStateBuilder[T] {
  override def build(program: SilverProgramDeclaration, method: SilverMethodDeclaration): T = {
    top.copy()
  }
}

object PolyhedraAnalysisEntryState extends NumericalAnalysisStateBuilder[Apron.Polyhedra, PolyhedraAnalysisState] {
  override def top: PolyhedraAnalysisState = PolyhedraAnalysisState()
}

object OctagonAnalysisEntryState extends NumericalAnalysisStateBuilder[IntegerOctagons, OctagonAnalysisState] {
  override def top: OctagonAnalysisState = OctagonAnalysisState()
}