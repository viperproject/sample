/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution.EntryStateBuilder
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, PermType, SilverSpecification}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, MethodDeclaration, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionsState2.{Bottom, Top}
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

import scala.collection.{Seq, mutable}

/**
  * @author Severin Münger
  *         Added on 29.11.16.
  */
object QuantifiedPermissionsEntryStateBuilder2 extends EntryStateBuilder[QuantifiedPermissionsState2] {

  override def build(method: MethodDeclaration): QuantifiedPermissionsState2 = {
    QuantifiedPermissionsState2()
  }

  override def topState = QuantifiedPermissionsState2()
}

object QuantifiedPermissionsState2 {
  object Top extends QuantifiedPermissionsState2(true, false)

  object Bottom extends QuantifiedPermissionsState2(false, true)
}

case class QuantifiedPermissionsState2(isTop: Boolean = false,
                                       isBottom: Boolean = false,
                                       expr: ExpressionSet = ExpressionSet(),
                                       currentPP: ProgramPoint = DummyProgramPoint
                                      )
  extends SimplePermissionState[QuantifiedPermissionsState2]
    with StateWithRefiningAnalysisStubs[QuantifiedPermissionsState2]
    with SilverSpecification
    with LazyLogging {

  // result of the alias analysis before the current program point
  private lazy val preFirstRunInfo = Context.preFirstRunInfo(currentPP)

  // result of the alias analysis after the current program point
//  private lazy val postFirstRunInfo = Context.postFirstRunInfo(currentPP)

  def copy(isTop: Boolean = isTop,
           isBottom: Boolean = isBottom,
           expr: ExpressionSet = expr,
           currentPP: ProgramPoint = currentPP) = QuantifiedPermissionsState2(isTop, isBottom, expr, currentPP)

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  override def inhale(acc: Expression): QuantifiedPermissionsState2 = throw new NotImplementedError()

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  override def exhale(acc: Expression): QuantifiedPermissionsState2 = throw new NotImplementedError()

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState2 = {
    this
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): QuantifiedPermissionsState2 = {
    this
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  override def assignVariable(x: Expression, right: Expression): QuantifiedPermissionsState2 = {
    this
  }

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
    */
  override def assignField(obj: Expression, field: String, right: Expression): QuantifiedPermissionsState2 = {
    this
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  override def setVariableToTop(varExpr: Expression): QuantifiedPermissionsState2 = {
    this
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    */
  override def removeVariable(varExpr: VariableIdentifier): QuantifiedPermissionsState2 = {
    this
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object on which the field access is performed
    * @param field the name of the field
    * @param typ   the type of the field
    * @return The abstract state obtained after the field access, that is,
    *         a new state whose `ExpressionSet` holds the symbolic representation of the value of the given field.
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): QuantifiedPermissionsState2 = {
    this
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): QuantifiedPermissionsState2 = {
    this
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): QuantifiedPermissionsState2 = {
    this.copy(currentPP = pp)
  }

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): QuantifiedPermissionsState2 = {
    this
  }

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState2 = {
    this
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): QuantifiedPermissionsState2 = {
    this
  }

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression
    */
  override def removeExpression(): QuantifiedPermissionsState2 = copy(expr = ExpressionSet())

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): QuantifiedPermissionsState2 = {
    this
  }

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): QuantifiedPermissionsState2 = copy(expr = expr)

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): QuantifiedPermissionsState2 = QuantifiedPermissionsState2()

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): QuantifiedPermissionsState2 = Top

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): QuantifiedPermissionsState2 = Bottom

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: QuantifiedPermissionsState2): QuantifiedPermissionsState2 = {
    (this, other) match {
      case (Bottom, Bottom) => Bottom
      case (Bottom, _) => other
      case (_, Bottom) => this
      case (_, _) => this
    }
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: QuantifiedPermissionsState2): QuantifiedPermissionsState2 = {
    this
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: QuantifiedPermissionsState2): Boolean = {
    (this, other) match {
      case (Bottom, _) => true
      case (_, Bottom) => false
      case (_, _) => true
    }
  }

  // SPECIFICATIONS
  
  /** Modifies the list of formal arguments using information stored in the
    * current state.
    *
    * @param existing The list of existing formal arguments.
    * @return The modified list of formal arguments*/
  override def formalArguments(existing: Seq[sil.LocalVarDecl]): Seq[sil.LocalVarDecl] = {
    var newFormalArguments = existing
    preFirstRunInfo.permissionRecords = preFirstRunInfo.permissionRecords.transform {
      case ReadPermission =>
        VariableIdentifier(Context.getRdAmountVariable.name)(PermType)
      case other => other
    }
    Context.rdAmountVariable match {
      case Some(rdAmount) => if (!newFormalArguments.contains(rdAmount)) newFormalArguments = newFormalArguments :+ rdAmount
      case None =>
    }
    newFormalArguments
  }

  /**
    * Modifies the list of preconditions using information stored in the current
    * state.
    *
    * @param existing The list of existing preconditions.
    * @return The modified list of preconditions.
    */
  override def preconditions(existing: Seq[sil.Exp]): Seq[sil.Exp] = {
    var newPreconditions = existing
    Context.rdAmountVariable match {
      case Some(rdAmount) => newPreconditions = newPreconditions :+ sil.And(sil.PermLtCmp(ZeroPerm, rdAmount.localVar)(), sil.PermLtCmp(rdAmount.localVar, WritePerm)())()
      case None =>
    }
    val fieldAccessFunctions: mutable.Map[String, sil.Function] = mutable.Map()
    preFirstRunInfo.permissionRecords.permissions foreach { case (fieldName, permissionTree) =>
      val quantifiedVariableDecl = Context.getQuantifiedVarDecl
      val quantifiedVariable = quantifiedVariableDecl.localVar
      val fieldAccess = viper.silver.ast.FieldAccess(quantifiedVariable, sil.Field(fieldName, sil.Ref)())()
      //      val permissionTreeWithoutFieldAccesses = permissionTree
      val permissionTreeWithoutFieldAccesses = permissionTree.transform {
        case FieldExpression(typ, field, receiver) =>
          if (!fieldAccessFunctions.contains(field)) {
            val fun = sil.Function(Context.createNewUniqueFunctionIdentifier("get_" + field), Seq(quantifiedVariableDecl), DefaultSampleConverter.convert(typ), Seq(), Seq(), None)()
            fieldAccessFunctions.put(field, fun)
            Context.auxiliaryFunctions.put(fun.name, fun)
          }
          FunctionCallExpression(typ, fieldAccessFunctions(field).name, Seq(receiver))
        case other => other
      }
      val implies = sil.Implies(sil.TrueLit()(), sil.FieldAccessPredicate(fieldAccess, permissionTreeWithoutFieldAccesses.toSilExpression(quantifiedVariable))())()
      val forall = sil.Forall(Seq(quantifiedVariableDecl), Seq(), implies)()
      newPreconditions = newPreconditions :+ forall
    }
    fieldAccessFunctions.foreach {
      case (fieldName, function) =>
        val quantifiedVarDecl = Context.getQuantifiedVarDecl
        val quantifiedVar = quantifiedVarDecl.localVar
        val field = sil.Field(fieldName, function.typ)()
        val implies = sil.Implies(sil.PermGtCmp(sil.CurrentPerm(sil.FieldAccess(quantifiedVar, field)())(), ZeroPerm)(), sil.EqCmp(sil.FuncApp(function, Seq(quantifiedVar))(), sil.FieldAccess(quantifiedVar, field)())())()
        newPreconditions = newPreconditions :+ sil.InhaleExhaleExp(sil.Forall(Seq(quantifiedVarDecl), Seq(), implies)(), sil.TrueLit()())()
    }
    newPreconditions
  }

  /**
    * Modifies the list of invariants using information stored in the current
    * state.
    *
    * @param existing The list of existing invariants.
    * @return The modified list of invariants.
    */
  override def invariants(existing: Seq[sil.Exp]): Seq[sil.Exp] = {
    existing
  }

  // STUBS

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: QuantifiedPermissionsState2): QuantifiedPermissionsState2 = throw new UnsupportedOperationException

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    */
  override def throws(t: ExpressionSet): QuantifiedPermissionsState2 = throw new UnsupportedOperationException

  override def ids: IdentifierSet = throw new UnsupportedOperationException

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): QuantifiedPermissionsState2 = throw new UnsupportedOperationException

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): QuantifiedPermissionsState2 = throw new UnsupportedOperationException
}