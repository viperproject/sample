/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.abstractdomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.SystemParameters
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.Set

/** The representation of a <a href="http://en.wikipedia.org/wiki/Lattice_%28order%29">lattice</a> structure.
  *
  * @tparam T The current type of the Lattice
  * @author Pietro Ferrara, Lucas Brutschy
  * @since 0.1
  */
trait Lattice[T <: Lattice[T]] {
  this: T =>

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  def factory(): T

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  def top(): T

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  def bottom(): T

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  def lub(other: T): T

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  def glb(other: T): T

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  def widening(other: T): T

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  def lessEqual(other: T): Boolean

  /** Glb as implemented in many domains is not "strict" enough in that
    * it does not remove identifiers uncommon to `this` and `other`. Not pretty,
    * but we can't just add another operation to the domain implementations since
    * we won't have access to those and we also want to preserve the old behavior.
    */
  def strictGlb(other: T): T = glb(other)

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return bottom
    */
  def isBottom: Boolean

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return bottom
    */
  def isTop: Boolean

  /** Checks semantic equality of the two states. Not necessarily an equal representation / hashcode!
    *
    * @todo should this be the same as equals?
    */
  def equivalent(other: T): Boolean = other.lessEqual(this) && this.lessEqual(other)

}

object Lattice extends LazyLogging {

  /** Defines a top lattice element.
    *
    * @author Lucas Brutschy
    */
  trait Top[S <: Lattice[S]] extends Lattice[S] {
    this:S =>

    override final def isTop: Boolean = true
    override final def isBottom: Boolean = false
    override final def lessEqual(other: S): Boolean = other.isTop
    override final def lub(other: S): S = this
    override final def widening(other: S): S = this
    override final def glb(other: S): S = other
    override def toString = "⊤"

  }

  /** Defines a lattice element that is neither top nor bottom.
    *
    * @author Lucas Brutschy
    */
  trait Inner[S <: Lattice[S], I <: Lattice.Inner[S,I]] extends Lattice[S] {
    this:S =>

    override final def isTop: Boolean = false
    override final def isBottom: Boolean = false

    def lub(other: S): S = other match {
      case _ if other.isBottom => this
      case _ if other.isTop    => other
      case a:I @unchecked      => lubInner(a)
    }
    def lubInner(other:I):S

    def glb(other: S): S = other match {
      case _ if other.isBottom => other
      case _ if other.isTop    => this
      case a:I @unchecked      => glbInner(a)
    }
    def glbInner(other:I):S

    def widening(other: S): S = other match {
      case _ if other.isBottom => this
      case _ if other.isTop    => other
      case a:I @unchecked      => wideningInner(a)
    }
    def wideningInner(other:I):S

    def lessEqual(other: S): Boolean = other match {
      case _ if other.isBottom => false
      case _ if other.isTop    => true
      case a:I @unchecked      => lessEqualInner(a)
    }
    def lessEqualInner(other:I): Boolean

  }

  /** Defines a bottom lattice element.
    *
    * @author Lucas Brutschy
    */
  trait Bottom[S <: Lattice[S]] extends Lattice[S] {
    this:S =>

    override final def isTop: Boolean = false
    override final def isBottom: Boolean = true
    override final def lessEqual(r: S): Boolean = true
    override final def lub(other: S): S = other
    override final def widening(other: S): S = other
    override final def glb(other: S): S = this
    override def toString = "⊥"

  }

  /** Mixin that causes a lattice to have a must semantics, where the join operator uses the greatest lower bound.
    *
    * @tparam T the self-type of the lattice
    */
  @Deprecated
  trait Must[T <: Must[T]] extends Lattice[T] {
    this: T =>
    override def lub(other: T) = glb(other)
  }

  /** Returns the least upper bound of one or more lattice elements. */
  def bigLub[S <: Lattice[S]](elements: Iterable[S]): S = {
    require(elements.nonEmpty, "there must be at least one element")
    elements.reduceLeft(_ lub _)
  }

  /** Returns the least upper bound of zero or more lattice elements. */
  def bigLub[S <: Lattice[S]](elements: Iterable[S], lattice: S): S = {
    if (elements.isEmpty) lattice.bottom()
    else bigLub(elements)
  }

  /** Returns the greatest lower bound of one or more lattice elements. */
  def bigGlb[S <: Lattice[S]](elements: Iterable[S]): S = {
    require(elements.nonEmpty, "there must be at least one element")
    elements.reduceLeft(_ glb _)
  }

  /** Returns the widening of multiple one or more elements. */
  def bigWidening[S <: Lattice[S]](elements: Iterable[S]): S = {
    require(elements.nonEmpty, "there must be at least one element")
    elements.reduceLeft(_ widening _)
  }

  /** Returns (over-approximation) of least fix point of function iterates.  */
  def lfp[S <: Lattice[S]](s: S, f: (S => S), wideningLimit: Int): S = {
    var iteration = 1
    var prev = s
    var cur = prev.lub(f(prev))
    while (!cur.lessEqual(prev)) {
      prev = cur
      iteration += 1
      if (iteration > wideningLimit) {
        if (iteration > wideningLimit + 10) {
          logger.debug("Looks like we are not terminating here!" + cur.toString)
        }
        cur = prev.widening(f(prev))
      }
      else cur = prev.lub(f(prev))
    }
    cur
  }

}

/** A Lattice that does not do anything useful except for preserving its one value. */
trait DummyLattice[T <: DummyLattice[T]] extends Lattice[T] {
  this:T =>
}

object DummyLattice {

  trait Bottom[T <: DummyLattice[T]] extends DummyLattice[T] with Lattice.Bottom[T] {
    this:T =>
  }

  trait Top[T <: DummyLattice[T]] extends DummyLattice[T] with Lattice.Top[T] {
    this:T =>
  }

}

/** The representation of a state of our analysis.
  *
  * Two main components can be distinguished:
  * - a `SymbolicAbstractValue` that is aimed at representing the expression returned by the previous statement
  * - an `HeapAndAnotherDomain` state, that is, an abstract state of an heap analysis and of another semantic domain
  *
  * This is the most generic level to build up an abstract state. We strongly discourage the use of this interface
  * since there are simpler interface (e.g., `SemanticDomain` or `HeapAndAnotherDomain`).
  *
  * @tparam S The current type of the state
  * @author Pietro Ferrara, Lucas Brutschy
  * @since 0.1
 */
trait State[S <: State[S]] extends Lattice[S] {
  this: S =>

  /** Assigns an expression to a field of an object.
    *
    * @param obj the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  def assignField(obj: ExpressionSet, field: String, right: ExpressionSet): S

  /** Refining backward transformer for field assignments.
    *
    * @param oldPreState state before this operation
    * @param obj field target object
    * @param field field to be assigned
    * @param right assigned expression
    * @return refined pre state before the field assignment
    */
  def refiningAssignField(oldPreState: S, obj: ExpressionSet, field: String, right: ExpressionSet): S

  /** Assigns an expression to a variable.
    *
    * @param x The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  def assignVariable(x: ExpressionSet, right: ExpressionSet): S

  /** Performs refining backward assignment of variables.
    *
    * @param oldPreState the pre state to be refined
    * @param x The assigned variable
    * @param right The assigned expression
    * @return The abstract state before the assignment
    */
  def refiningAssignVariable(oldPreState: S, x: ExpressionSet, right: ExpressionSet): S

  /** Assumes that a boolean expression holds.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  def assume(cond: ExpressionSet): S

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  def before(pp: ProgramPoint): S

  /** Executes the given command.
    *
    * @param cmd The command to execute.
    * @return The abstract state after the execution of the given command.
    */
  def command(cmd: Command): S = this // throw new UnsupportedOperationException(s"Unknown command: $cmd")

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  def createObject(typ: Type, pp: ProgramPoint): S

  /** Creates a variable.
    *
    * @param x The name of the variable
    * @param typ The static type of the variable
    * @param pp The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  def createVariable(x: ExpressionSet, typ: Type, pp: ProgramPoint): S

  /** Creates a variable for an argument.
    *
    * @param x The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  def createVariableForArgument(x: ExpressionSet, typ: Type): S

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ The type of the numerical constant
    * @param pp The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  def evalConstant(value: String, typ: Type, pp: ProgramPoint): S

  /** May try to explain an error.
    *
    * @param expr An error-expression that should be infeasible but exposes an error
    * @return If a cause of the error is found, it returns an explanation and the program point of the cause
    */
  def explainError(expr: ExpressionSet): Set[(String, ProgramPoint)] = Set.empty

  /** Returns the current expression. */
  def expr: ExpressionSet

  /** Accesses a field of an object.
    *
    * @param obj the object on which the field access is performed
    * @param field the name of the field
    * @param typ the type of the field
    * @return The abstract state obtained after the field access, that is,
    *         the state that contains as expression the symbolic representation of the value of the given field access
    */
  def getFieldValue(obj: ExpressionSet, field: String, typ: Type): S

  /** Performs the backward semantics of a field access.
    *
    * @param obj the object on which the field access is performed
    * @param field the name of the field
    * @param typ the type of the field
    * @return the abstract state obtained before the field access
    */
  def refiningGetFieldValue(obj: ExpressionSet, field: String, typ: Type): S

  /** Returns all objects pointed to by the field which may / must match the given filter.
    *
    * @param objs An expression containing objects
    * @param field  The name of the field
    * @param typ    The type of the field
    * @param filter The filter, that, given an object and a state, returns whether it matches
    * @return       A may set and a must set of object
    */
  def getFieldValueWhere(objs: ExpressionSet, field: String, typ: Type, filter:(Identifier,S) => Boolean): (Set[Identifier],Set[Identifier]) =
    (Set.empty,Set.empty)

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  def getVariableValue(id: Identifier): S

  def merge(r:Replacement):S = this

  /** Performs abstract garbage collection. */
  def pruneUnreachableHeap(): S

  /** Removes all variables satisfying filter. */
  def pruneVariables(filter: VariableIdentifier => Boolean): S

  /** Performs the backward semantics of a variable access.
    *
    * @param id The accessed variable
    * @return The abstract state obtained BEFORE accessing the variable
    */
  def refiningGetVariableValue(id: Identifier): S

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression
    */
  def removeExpression(): S

  /** Undoes the effect of object creation.
    *
    * Intended to be the backward version of createObject
    * and should only be used on a post state immediately after object creation.
    *
    * @param oldPreState
    * @param obj the heap id of the object to be removed
    * @param fields the fields that were created
    * @return state without the object
    */
  def removeObject(oldPreState: S, obj: ExpressionSet, fields: Option[Set[Identifier]]): S

  /** Removes a variable.
    *
    * @param x The variable to be removed
    * @return The abstract state obtained after removing the variable
    */
  def removeVariable(x: ExpressionSet): S

  /** Assigns an expression to an argument.
    *
    * @param x The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    */
  def setArgument(x: ExpressionSet, right: ExpressionSet): S

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  def setExpression(expr: ExpressionSet): S

  /** Forgets the value of a variable.
    *
    * @param x The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  def setVariableToTop(x: ExpressionSet): S

  /** Assumes that the current expression holds.
    *
    * @return The abstract state after assuming that the expression holds
    */
  def testTrue(): S

  /** Assumes that the current expression does not hold.
    *
    * @return The abstract state after assuming that the expression does not hold
    */
  def testFalse(): S

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    */
  def throws(t: ExpressionSet): S

  /** Undoes the effect of pruning the unreachable heap ids. That is,
    * all heap ids present in `preState` but not in this state are created
    * and set to top. Everything else stays the same as in the post state (this state).
    *
    * @param preState old pre state before heap pruning was applied
    * @return unpruned heap
    */
  def undoPruneUnreachableHeap(preState: S): S

  /** Undoes the effect of `pruneVariables`.
    *
    * All the variables that existed in `unprunedPreState` and that match the given filter are created and set to top.
    *
    * @param unprunedPreState state before pruning
    * @param filter the filter that was used to prune variables
    * @return state with pruned variables created again
    */
  def undoPruneVariables(unprunedPreState: S, filter: VariableIdentifier => Boolean): S
}

trait StateWithRefiningAnalysisStubs[S <: StateWithRefiningAnalysisStubs[S]] extends SimpleState[S] {
  this: S =>

  def refiningAssignField(oldPreState: S, obj: Expression, field: String, right: Expression) = ???
  def refiningAssignVariable(oldPreState: S, x: Expression, right: Expression) = ???
  def refiningGetFieldValue(obj: ExpressionSet, field: String, typ: Type) = ???
  def refiningGetVariableValue(id: Identifier) = ???
  def removeObject(oldPreState: S, obj: ExpressionSet, fields: Option[Set[Identifier]]) = ???
  def undoPruneUnreachableHeap(preState: S) = ???
  def undoPruneVariables(unprunedPreState: S, filter: VariableIdentifier => Boolean) = ???

}

/** Implements some methods of `State` that take `ExpressionSet`s as argument,
  * performs the corresponding operations pair-wise for all `Expression`s
  * and finally computes the upper bound or all resulting states.
  *
  * Classes implementing this trait only need to supply operations
  * for single `Expression`s, not `ExpressionSet`s and can thus avoid a lot of boiler-plate code.
  * In addition, the implemented methods also handle the cases where this state or any argument is bottom.
  *
  * @tparam S the self-type of the state
  */
trait SimpleState[S <: SimpleState[S]] extends State[S] {
  this: S =>

  /** Executes the given function only if this state and the given `ExpressionSet` are non-bottom. */
  def unlessBottom(set: ExpressionSet, f: => S): S =
    if (isBottom) {
      bottom()
    } else if (set.isBottom) {
      bottom()
    } else f

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The name of the variable
    * @param typ The static type of the variable
    * @param pp The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): S

  def createVariable(x: ExpressionSet, typ: Type, pp: ProgramPoint): S = {
    require(x.getNonTop.forall(_.isInstanceOf[VariableIdentifier]),
      "can only create variable from variable identifiers")
    unlessBottom(x, {
      val variable = unpackSingle(x).asInstanceOf[VariableIdentifier]
      createVariable(variable, typ, pp).setUnitExpression()
    })
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  def createVariableForArgument(x: VariableIdentifier, typ: Type): S

  def createVariableForArgument(x: ExpressionSet, typ: Type): S = {
    require(x.getNonTop.forall(_.isInstanceOf[VariableIdentifier]),
      "can only create variable from variable identifiers")
    unlessBottom(x, {
      val variable = unpackSingle(x).asInstanceOf[VariableIdentifier]
      createVariableForArgument(variable, typ).setUnitExpression()
    })
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  def assignVariable(x: Expression, right: Expression): S

  def assignVariable(leftSet: ExpressionSet, rightSet: ExpressionSet): S = {
    unlessBottom(leftSet, {
      unlessBottom(rightSet, {
        val result = if (rightSet.isTop) {
          setVariableToTop(leftSet)
        } else {
          Lattice.bigLub(for (
            left <- leftSet.getNonTop;
            right <- rightSet.getNonTop)
            yield assignVariable(left, right))
        }
        result.setUnitExpression()
      })
    })
  }

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  def assignField(obj: Expression, field: String, right: Expression): S

  def assignField(objSet: ExpressionSet, field: String, rightSet: ExpressionSet): S = {
    unlessBottom(objSet, {
      unlessBottom(rightSet, {
        val result = if (rightSet.isTop) {
          val t = getFieldValue(objSet, field, rightSet.getType())
          t.setVariableToTop(t.expr)
        } else {
          Lattice.bigLub(for (
            obj <- objSet.getNonTop;
            right <- rightSet.getNonTop)
          yield assignField(obj, field, right))
        }
        result.setUnitExpression()
      })
    })
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  def setVariableToTop(varExpr: Expression): S

  def setVariableToTop(varSet: ExpressionSet): S = {
    unlessBottom(varSet, {
      val result = Lattice.bigLub(varSet.getNonTop.map(setVariableToTop))
      result.removeExpression()
    })
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    */
  def removeVariable(varExpr: VariableIdentifier): S

  def removeVariable(varSet: ExpressionSet): S = {
    require(varSet.getNonTop.forall(_.isInstanceOf[VariableIdentifier]))

    unlessBottom(varSet, {
      val result = Lattice.bigLub(varSet.getNonTop.map { x => removeVariable (x.asInstanceOf[VariableIdentifier])})
      result.removeExpression()
    })
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj the object on which the field access is performed
    * @param field the name of the field
    * @param typ the type of the field
    * @return The abstract state obtained after the field access, that is,
    *         a new state whose `ExpressionSet` holds the symbolic representation of the value of the given field.
    */
  def getFieldValue(obj: Expression, field: String, typ: Type): S

  def getFieldValue(objSet: ExpressionSet, field: String, typ: Type): S = {
    unlessBottom(objSet, {
      Lattice.bigLub(objSet.getNonTop.map(getFieldValue(_, field, typ)))
    })
  }

  /** Performs refining backward assignment of variables.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param oldPreState the pre state to be refined
    * @param x The assigned variable
    * @param right The assigned expression
    * @return The abstract state before the assignment
    */
  def refiningAssignVariable(oldPreState: S, x: Expression, right: Expression): S

  def refiningAssignVariable(oldPreState: S, varSet: ExpressionSet, rhsSet: ExpressionSet): S = {
    unlessBottom(varSet, {
      unlessBottom(rhsSet, {
        val result = if (rhsSet.isTop) {
          setVariableToTop(varSet)
        } else {
          Lattice.bigLub(for (
            left <- varSet.getNonTop;
            right <- rhsSet.getNonTop)
          yield refiningAssignVariable(oldPreState, left, right))
        }
        result.removeExpression()
      })
    })
  }

  /** Refining backward transformer for field assignments.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param oldPreState state before this operation
    * @param obj field target object
    * @param field field to be assigned
    * @param right assigned expression
    * @return refined pre state before the field assignment
    */
  def refiningAssignField(oldPreState: S, obj: Expression, field: String, right: Expression): S

  def refiningAssignField(oldPreState: S, objSet: ExpressionSet, field: String, rightSet: ExpressionSet): S = {
    unlessBottom(objSet, {
      unlessBottom(rightSet, {
        val result = if (rightSet.isTop) {
          val t = this.getFieldValue(objSet, field, rightSet.getType())
          t.setVariableToTop(t.expr).setExpression(ExpressionFactory.unitExpr)
        } else {
          Lattice.bigLub(for (
            obj <- objSet.getNonTop;
            right <- rightSet.getNonTop)
          yield refiningAssignField(oldPreState, obj, field, right))
        }
        result.setUnitExpression()
      })
    })
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  def assume(cond: Expression): S

  def assume(condSet: ExpressionSet): S = {
    // Return this, not bottom, when set of conditions is empty
    if (isBottom || condSet.isBottom || condSet.isTop || condSet._2.isTop) this
    else {
      Lattice.bigLub(condSet.getNonTop.map(assume))
    }
  }

  /** May try to explain an error.
    *
    * @param expr An error-expression that should be infeasible but exposes an error
    * @return If a cause of the error is found, it returns an explanation and the program point of the cause
    */
  def explainError(expr:Expression) : Set[(String, ProgramPoint)] = Set.empty

  override def explainError(expr: ExpressionSet): Set[(String, ProgramPoint)] =
    expr.getNonTop.flatMap(this.explainError)

  /** Assumes that the current expression holds.
    *
    * @return The abstract state after assuming that the expression holds
    */
  def testTrue(): S = assume(expr).setUnitExpression()

  /** Assumes that the current expression does not hold.
    *
    * @return The abstract state after assuming that the expression does not hold
    */
  def testFalse(): S = assume(expr.not()).setUnitExpression()

  /** @todo merge with `removeExpression`. */
  def setUnitExpression(): S = {
    val unitExp = new UnitExpression(SystemParameters.typ.top(), DummyProgramPoint)
    setExpression(ExpressionSet(unitExp))
  }

  private def unpackSingle(set: ExpressionSet): Expression = {
    require(set.getNonTop.size == 1, "ExpressionSet must contain exactly one Expression")
    set.getNonTop.head
  }
}

/** The representation of a <a href="http://en.wikipedia.org/wiki/Lattice_%28order%29">lattice</a> structure
  * that when joins, meets or widens returns a replacement.
  *
  * @tparam T The current type of the LatticeWithReplacement
  * @author Pietro Ferrara
  * @since 0.1
  */
trait LatticeWithReplacement[T <: LatticeWithReplacement[T]] {

  /** Computes the least upper bound of two elements, returning a replacement.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  def lubWithReplacement(other: T): (T, Replacement)

  /** Computes the greatest lower bound of two elements, returning a replacement.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  def glbWithReplacement(other: T): (T, Replacement)

  /** Computes the widening of two elements, returning a replacement.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  def wideningWithReplacement(other: T): (T, Replacement)

}

/** Trait adding Inhale/Exhale methods to a SimpleState.
  *
  * @author Caterina Urban
  */
trait SimplePermissionState[S <: SimplePermissionState[S]] extends SimpleState[S] {
  this: S =>

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  def inhale(acc: Expression): S

  def inhale(acc: ExpressionSet): S = unlessBottom(acc, {
    Lattice.bigLub(acc.getNonTop.map(inhale))
  })

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  def exhale(acc: Expression): S

  def exhale(acc: ExpressionSet): S = unlessBottom(acc, {
    Lattice.bigLub(acc.getNonTop.map(exhale))
  })
}

/** A command that updates an abstract state.
  *
  * @author Jerome Dohrau
  */
trait Command {
}

/** Some trivial helper functions that execute forward/backward semantics on single and list of states.
  *
  * @author Pietro Ferrara
  */
object UtilitiesOnStates {

  def forwardExecuteStatement[S <: State[S]](state: S, statement: Statement): (ExpressionSet, S) = {
    val finalState = statement.forwardSemantics[S](state)
    val expr = finalState.expr
    (expr, finalState)
  }

  def refiningExecuteStatement[S <: State[S]](state: S, oldPreState: S, statement: Statement): (ExpressionSet, S) = {
    val finalState = statement.refiningSemantics[S](state, oldPreState)
    val expr = finalState.expr
    (expr, finalState.setExpression(ExpressionFactory.unitExpr))
  }

}