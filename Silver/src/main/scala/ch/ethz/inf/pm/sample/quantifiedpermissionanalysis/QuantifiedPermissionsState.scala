/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.execution.EntryStateBuilder
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, MethodDeclaration, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionsState.{Bottom, Top}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.SetDescription.InnerSetDescription
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

import scala.collection.Seq

/**
  * @author Severin Münger
  *         Added on 29.11.16.
  */
object QuantifiedPermissionsEntryStateBuilder extends EntryStateBuilder[QuantifiedPermissionsState] {

  override def build(method: MethodDeclaration): QuantifiedPermissionsState = {
    QuantifiedPermissionsState()
  }

  override def topState = QuantifiedPermissionsState()
}

object QuantifiedPermissionsState {
  object Top extends QuantifiedPermissionsState(true, false)

  object Bottom extends QuantifiedPermissionsState(false, true)
}

case class QuantifiedPermissionsState(isTop: Boolean = false,
                                      isBottom: Boolean = false,
                                      expr: ExpressionSet = ExpressionSet(),
                                      visited: Set[ProgramPoint] = Set(),
                                      currentPP: ProgramPoint = DummyProgramPoint,
                                      permissions: PermissionRecords = PermissionRecords(),
                                      expressions: Map[(ProgramPoint, Expression), SetDescription] = Map()
                                      )
  extends SimplePermissionState[QuantifiedPermissionsState]
    with StateWithRefiningAnalysisStubs[QuantifiedPermissionsState]
    with SilverSpecification
    with LazyLogging {

  // result of the alias analysis after the current program point
//  private lazy val postFirstRunInfo = Context.postFirstRunInfo(currentPP)

  // BASIC METHODS

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): QuantifiedPermissionsState = QuantifiedPermissionsState()

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): QuantifiedPermissionsState = Top

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): QuantifiedPermissionsState = Bottom

  def copy(isTop: Boolean = isTop,
           isBottom: Boolean = isBottom,
           expr: ExpressionSet = expr,
           visited: Set[ProgramPoint] = visited,
           currentPP: ProgramPoint = currentPP,
           permissions: PermissionRecords = permissions,
           expressions: Map[(ProgramPoint, Expression), SetDescription] = expressions) =
    QuantifiedPermissionsState(isTop, isBottom, expr, visited, currentPP, permissions, expressions)

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression
    */
  override def removeExpression(): QuantifiedPermissionsState = copy(expr = ExpressionSet())

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): QuantifiedPermissionsState = copy(expr = expr)

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: QuantifiedPermissionsState): QuantifiedPermissionsState = (this, other) match {
    case (Bottom, Bottom) => Bottom
    case (Bottom, _) => other
    case (_, Bottom) => this
    case (_, _) =>
      val newPermissions = (other.visited.subsetOf(visited), visited.subsetOf(other.visited)) match {
        case (true, _) => permissions
        case (_, true) => other.permissions
        case (false, false) => permissions.lub(other.permissions)
      }
      val newExpressions = expressions ++ other.expressions.transform {
        case (key, expressionCollection) => if (expressions.contains(key)) expressions(key).lub(expressionCollection) else expressionCollection
      }
      copy(
        expr = expr lub other.expr,
        visited = visited ++ other.visited,
        permissions = newPermissions,
        expressions = newExpressions
      )
  }

  def lub(other: QuantifiedPermissionsState, cond: ExpressionSet): QuantifiedPermissionsState = (cond.getSingle.isDefined && !cond.getSingle.get.isInstanceOf[UnitExpression], this, other) match {
    case (false, _, _) | (_, Bottom, _) | (_, _, Bottom) => lub(other)
    case (_, _, _) =>
      val newPermissions = (other.visited.subsetOf(visited), visited.subsetOf(other.visited)) match {
        case (true, _) => permissions
        case (_, true) => other.permissions
        case (false, false) => permissions.cond(cond.getSingle.get, other.permissions)
      }
      val newExpressions = expressions ++ other.expressions.transform {
        case (key, expressionCollection) => if (expressions.contains(key)) expressions(key).lub(expressionCollection) else expressionCollection
      }
      copy(
        expr = expr lub other.expr,
        visited = visited ++ other.visited,
        permissions = newPermissions,
        expressions = newExpressions
      )
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: QuantifiedPermissionsState): Boolean = {
    (this, other) match {
      case (Bottom, _) => true
      case (_, Bottom) => false
      case (_, _) => expressions.forall { case (key, setDescription) => setDescription.lessEqual(other.expressions.getOrElse(key, SetDescription.Bottom)) }
    }
  }

  // ABSTRACT TRANSFORMERS

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  override def assignVariable(x: Expression, right: Expression): QuantifiedPermissionsState = x match {
    case left: VariableIdentifier =>
      val (newPermissions, newExpressions) = left.typ match {
        case _: RefType =>
          (permissions, expressions.transform {
            case (_, setDescription) => setDescription.transformAssignVariable(left, right)
          })
        case IntType => if (!visited.contains(currentPP)) (permissions.transformExpressions(e => if (e.equals(left)) right else e), expressions.map {
          case ((pp, keyExpr), setDescription) => (pp, keyExpr.transform(e => if (e.equals(left)) right else e)) -> setDescription.transformAssignVariable(left, right)
        }) else (permissions, expressions)
        case _ => throw new IllegalStateException()
      }
      copy(
        permissions = newPermissions,
        expressions = newExpressions
      )
    case _ => throw new IllegalStateException()
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
  override def assignField(obj: Expression, field: String, right: Expression): QuantifiedPermissionsState = {
    val receiver = obj match {
      case FieldExpression(_, `field`, rec) => rec
      case _ => throw new IllegalStateException()
    }
    val key = (currentPP, receiver)
    val newPermissions =
      if (!visited.contains(currentPP)) permissions.undoLastRead(field).max(field, ExpressionDescription(currentPP, receiver), WritePermission)
      else permissions
    var newExpressions = right.typ match {
      case _: RefType =>
        expressions.transform {
          case (_, setDescription) => setDescription.transformAssignField(receiver, field, right)
        }
      case IntType => expressions
      case _ => throw new IllegalStateException()
    }
    newExpressions = newExpressions + (key -> newExpressions.getOrElse(key, SetDescription.Bottom).lub(InnerSetDescription(receiver)))
    copy(
      permissions = newPermissions,
      expressions = newExpressions
    )
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    */
  override def removeVariable(varExpr: VariableIdentifier): QuantifiedPermissionsState = copy(expr = ExpressionSet(varExpr))

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
  override def getFieldValue(obj: Expression, field: String, typ: Type): QuantifiedPermissionsState = {
    val key = (currentPP, obj)
    val newPermissions =
      if (!visited.contains(currentPP)) permissions.max(field, ExpressionDescription(currentPP, obj), SymbolicReadPermission())
      else permissions
    val newExpressions = expressions + (key -> expressions.getOrElse(key, SetDescription.Bottom).lub(InnerSetDescription(obj)))
    copy(
      expr = ExpressionSet(FieldExpression(typ, field, obj)),
      permissions = newPermissions,
      expressions = newExpressions
    )
  }

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  override def inhale(acc: Expression): QuantifiedPermissionsState = this

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  override def exhale(acc: Expression): QuantifiedPermissionsState = acc match {
    case FieldAccessPredicate(FieldExpression(_, field, receiver), num, denom, _) =>
      val key = (currentPP, receiver)
      val newPermissions =
        if (!visited.contains(currentPP)) permissions.undoLastRead(field).add(field, ExpressionDescription(currentPP, receiver), FractionalPermission(num, denom))
        else permissions
      val newExpressions = expressions + (key -> expressions.getOrElse(key, SetDescription.Bottom).lub(InnerSetDescription(receiver)))
      copy(
        permissions = newPermissions,
        expressions = newExpressions
      )
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): QuantifiedPermissionsState = copy(currentPP = pp)

  def after(pp: ProgramPoint): QuantifiedPermissionsState = copy(visited = visited + pp)

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = copy(expr = ExpressionSet(Constant(value, typ, pp)))

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): QuantifiedPermissionsState = copy(expr = ExpressionSet(id))

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    copy(
      visited = visited ++ other.visited,
      expressions = expressions ++ other.expressions.transform {
        case (key, setDescription) =>
          if (expressions.contains(key))
            if (setDescription.lessEqual(expressions(key))) expressions(key).lub(setDescription)
            else expressions(key).widening(setDescription)
          else setDescription
      }
    )
  }

  // SPECIFICATIONS

  /** Modifies the list of formal arguments using information stored in the
    * current state.
    *
    * @param existing The list of existing formal arguments.
    * @return The modified list of formal arguments*/
  override def formalArguments(existing: Seq[sil.LocalVarDecl]): Seq[sil.LocalVarDecl] = {
    var newFormalArguments = existing
    if (permissions.exists((arg) => arg._2.exists {
      case PermissionLeaf(_, _: SymbolicReadPermission) => true
      case _ => false
    })) {
      val rdAmount = Context.getRdAmountVariable
      if (!newFormalArguments.contains(rdAmount)) newFormalArguments = newFormalArguments :+ rdAmount
    }
    expressions.foreach {
      case (key, setDescription: InnerSetDescription) =>
        if (!setDescription.isFinite && !newFormalArguments.contains(Context.getSetFor(key)))
          newFormalArguments = newFormalArguments :+ Context.getSetFor(key)
    }
    newFormalArguments
  }

  private def getMaxRdValue(permAmount: FractionalPermission, readAmount: Int): FractionalPermission = permAmount match {
    case FractionalPermission(numerator, denominator) =>
      if (readAmount == 0) FractionalPermission(1, 1)
      else FractionalPermission(denominator - numerator, denominator * readAmount)
  }

  private def getMaxRdValueTupled = (getMaxRdValue _).tupled

  /**
    * Modifies the list of preconditions using information stored in the current
    * state.
    *
    * @param existing The list of existing preconditions.
    * @return The modified list of preconditions.
    */
  override def preconditions(existing: Seq[sil.Exp]): Seq[sil.Exp] = {
    var newPreconditions = existing
    if (permissions.exists((arg) => arg._2.exists {
      case PermissionLeaf(_, _: SymbolicReadPermission) => true
      case _ => false
    })) {
      val rdAmount = Context.getRdAmountVariable.localVar
      val readPaths = permissions.flatMap { case (_, tree) => tree.getReadPaths }.toSet
      if (readPaths.nonEmpty) {
        var min = getMaxRdValue(readPaths.head._1, readPaths.head._2)
        readPaths.foreach { cur => if (getMaxRdValueTupled(cur) < min) min = getMaxRdValueTupled(cur) }
        newPreconditions = newPreconditions :+ sil.And(sil.PermLtCmp(ZeroPerm, rdAmount)(), sil.PermLtCmp(rdAmount, sil.FractionalPerm(sil.IntLit(min.numerator)(), sil.IntLit(min.denominator)())())())()
      } else {
        newPreconditions = newPreconditions :+ sil.And(sil.PermLtCmp(ZeroPerm, rdAmount)(), sil.PermLtCmp(rdAmount, WritePerm)())()
      }
    }
    permissions.foreach { case (fieldName, permissionTree) =>
      val quantifiedVariableDecl = Context.getQuantifiedVarDecl
      val quantifiedVariable = quantifiedVariableDecl.localVar
      val fieldAccess = viper.silver.ast.FieldAccess(quantifiedVariable, sil.Field(fieldName, sil.Ref)())()
      val implies = sil.Implies(sil.TrueLit()(), sil.FieldAccessPredicate(fieldAccess, permissionTree.toSilExpression(expressions, quantifiedVariable))())()
      val forall = sil.Forall(Seq(quantifiedVariableDecl), Seq(), implies)()
      newPreconditions = newPreconditions :+ forall
    }
    Context.fieldAccessFunctions.foreach {
      case (fieldName, function) =>
        val quantifiedVarDecl = Context.getQuantifiedVarDecl
        val quantifiedVar = quantifiedVarDecl.localVar
        val field = sil.Field(fieldName, function.typ)()
        val implies = sil.Implies(sil.PermGtCmp(sil.CurrentPerm(sil.FieldAccess(quantifiedVar, field)())(), ZeroPerm)(), sil.EqCmp(sil.FuncApp(function, Seq(quantifiedVar))(), sil.FieldAccess(quantifiedVar, field)())())()
        newPreconditions = newPreconditions :+ sil.InhaleExhaleExp(sil.Forall(Seq(quantifiedVarDecl), Seq(), implies)(), sil.TrueLit()())()
    }
    expressions.foreach {
      case (key, setDescription: InnerSetDescription) =>
        if (!setDescription.isFinite)
          newPreconditions = newPreconditions :+ setDescription.toSetDefinition(Context.getQuantifiedVarDecl, Context.getSetFor(key))
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
    var newInvariants = existing
    permissions.foreach { case (fieldName, permissionTree) =>
      val quantifiedVariableDecl = Context.getQuantifiedVarDecl
      val quantifiedVariable = quantifiedVariableDecl.localVar
      val fieldAccess = viper.silver.ast.FieldAccess(quantifiedVariable, sil.Field(fieldName, sil.Ref)())()
      val implies = sil.Implies(sil.TrueLit()(), sil.FieldAccessPredicate(fieldAccess, permissionTree.toSilExpression(expressions, quantifiedVariable))())()
      val forall = sil.Forall(Seq(quantifiedVariableDecl), Seq(), implies)()
      newInvariants = newInvariants :+ forall
    }
    Context.fieldAccessFunctions.foreach {
      case (fieldName, function) =>
        val quantifiedVarDecl = Context.getQuantifiedVarDecl
        val quantifiedVar = quantifiedVarDecl.localVar
        val field = sil.Field(fieldName, function.typ)()
        val implies = sil.Implies(sil.PermGtCmp(sil.CurrentPerm(sil.FieldAccess(quantifiedVar, field)())(), ZeroPerm)(), sil.EqCmp(sil.FuncApp(function, Seq(quantifiedVar))(), sil.FieldAccess(quantifiedVar, field)())())()
        newInvariants = newInvariants :+ sil.InhaleExhaleExp(sil.Forall(Seq(quantifiedVarDecl), Seq(), implies)(), sil.TrueLit()())()
    }
    expressions.foreach {
      case (key, setDescription: InnerSetDescription) =>
        if (!setDescription.isFinite)
          newInvariants = newInvariants :+ setDescription.toSetDefinition(Context.getQuantifiedVarDecl, Context.getSetFor(key))
    }
    val numDom: NumericalDomain[_] = Context.postNumericalInfo(currentPP).numDom
    val constraints = numDom.getConstraints(numDom.ids.getNonTop)
    if (constraints.nonEmpty) newInvariants = newInvariants :+ numDom.getConstraints(numDom.ids.getNonTop).map(expr => DefaultSampleConverter.convert(expr)).reduce((left, right) => sil.And(left, right)())
    newInvariants
  }

  // DO NOTHING ON THESE OPERATIONS

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = {
    this
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): QuantifiedPermissionsState = {
    this
  }

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = this

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): QuantifiedPermissionsState = this

  // STUBS

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown
    */
  override def throws(t: ExpressionSet): QuantifiedPermissionsState = throw new UnsupportedOperationException

  override def ids: IdentifierSet = throw new UnsupportedOperationException

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): QuantifiedPermissionsState = throw new UnsupportedOperationException

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): QuantifiedPermissionsState = throw new UnsupportedOperationException

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): QuantifiedPermissionsState = throw new UnsupportedOperationException

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: QuantifiedPermissionsState): QuantifiedPermissionsState = throw new UnsupportedOperationException

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  override def setVariableToTop(varExpr: Expression): QuantifiedPermissionsState = throw new UnsupportedOperationException()
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
    Lattice.bigLub(acc.toSetOrFail.map(inhale))
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
    Lattice.bigLub(acc.toSetOrFail.map(exhale))
  })
}