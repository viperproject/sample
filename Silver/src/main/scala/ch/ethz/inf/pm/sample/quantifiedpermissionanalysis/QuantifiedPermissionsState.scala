/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, ExpressionSet, FieldAccessPredicate, _}
import ch.ethz.inf.pm.sample.execution.EntryStateBuilder
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{BoolType, DefaultSampleConverter, PermType, SilverSpecification}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionsState.{Bottom, Top}
import com.typesafe.scalalogging.LazyLogging
import viper.silver.ast.{Type => _, _}
import viper.silver.{ast => sil}

import scala.Seq
import scala.collection._

/**
  * Abstract state for our analysis
  *
  * @author Severin Münger
  *         Added on 19/10/16.
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
                                      currentPP: ProgramPoint = DummyProgramPoint,
                                      var permissionRecords: PermissionRecords = PermissionRecords(),
                                      changingExpressions: Map[Expression, Set[Expression]] = Map())
  extends SimplePermissionState[QuantifiedPermissionsState]
    with StateWithRefiningAnalysisStubs[QuantifiedPermissionsState]
    with SilverSpecification
    with LazyLogging {

  // RESULTS FROM ALIAS AND NUMERICAL ANALYSIS

  // result of the alias analysis before the current program point
  lazy val preAliases = Context.preAliases(currentPP)

  // result of the alias analysis after the current program point
  lazy val postAliases = Context.postAliases(currentPP)

  // result of the alias analysis before the current program point
  lazy val preNumericalInfo = Context.preNumericalInfo(currentPP)

  // result of the alias analysis after the current program point
  lazy val postNumericalInfo = Context.postNumericalInfo(currentPP)

  // BASIC METHODS

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): QuantifiedPermissionsState = Bottom

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): QuantifiedPermissionsState = Top

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object */
  def factory(): QuantifiedPermissionsState = QuantifiedPermissionsState()

  def copy(isTop: Boolean = isTop,
           isBottom: Boolean = isBottom,
           expr: ExpressionSet = expr,
           currentPP: ProgramPoint = currentPP,
           permissionRecords: PermissionRecords = permissionRecords,
           changingExpressions: Map[Expression, Set[Expression]] = changingExpressions):
  QuantifiedPermissionsState = QuantifiedPermissionsState(isTop, isBottom, expr, currentPP, permissionRecords, changingExpressions)

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments */
  override def lub(other: QuantifiedPermissionsState): QuantifiedPermissionsState = {
    copy(isTop = isTop || other.isTop,
      isBottom = isBottom && other.isBottom,
      expr = expr lub other.expr,
      permissionRecords = permissionRecords lub other.permissionRecords,
      changingExpressions = changingExpressions ++ other.changingExpressions.map {
        case (changingExpression, values) => changingExpression -> (values ++ changingExpressions.getOrElse(changingExpression, Set()))
      })
  }

  // ABSTRACT TRANSFORMERS

  /** Executes the given command.
    *
    * @param cmd The command to execute.
    * @return The abstract state after the execution of the given command.*/
  override def command(cmd: Command): QuantifiedPermissionsState = {
    logger.info("Command: " + cmd.getClass.toString)
    cmd match {
      case _ => throw new UnsupportedOperationException("Unsupported command encountered: " + cmd)
    }
  }

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission */
  override def inhale(acc: Expression): QuantifiedPermissionsState = {
    acc match {
      case FieldAccessPredicate(id, n, d, _) =>
        val newPermissionRecords =
          id match {
            case FieldExpression(typ, field, receiver) =>
              val permTreeWithoutRead = permissionRecords.permissions(field).undoLastRead
              permissionRecords.permissions(field) = permTreeWithoutRead
              permissionRecords.sub(field, receiver, FractionalPermission(n, d))
            case _ => throw new UnsupportedOperationException
          }
        copy(permissionRecords = newPermissionRecords)
      case ForallExpression(leftCond, right, quantifiedVariable) =>
        // TODO: handle forall
        ???
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, _) =>
        // TODO: handle conjunctions, maybe split to multiple inhales?
        ???
      case _ => throw new UnsupportedOperationException(acc.toString)
    }
  }

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission */
  override def exhale(acc: Expression): QuantifiedPermissionsState = {
    acc match {
      case FieldAccessPredicate(id, n, d, _) =>
        val newPermissionRecords =
        id match {
          case FieldExpression(typ, field, receiver) =>
            val permTreeWithoutRead = permissionRecords.permissions(field).undoLastRead
            permissionRecords.permissions(field) = permTreeWithoutRead
            permissionRecords.add(field, receiver, FractionalPermission(n, d))
          case _ => throw new UnsupportedOperationException
        }
        copy(permissionRecords = newPermissionRecords)
      case ForallExpression(leftCond, right, quantifiedVariable) =>
        // TODO: handle forall
        ???
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, _) =>
        // TODO: handle conjunctions, maybe split to multiple exhales?
        ???
      case _ => throw new UnsupportedOperationException(acc.toString)
    }
  }

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = {
    // Nothing to do here
    this
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): QuantifiedPermissionsState = {
    // Nothing to do here
    this
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment */
  override def assignVariable(x: Expression, right: Expression): QuantifiedPermissionsState = {
    val replacer = (e: Expression) => if (e.equals(x)) right else e
    val newPermissionRecords = permissionRecords.transform(replacer)
    val newChangingExpressions = changingExpressions.map {
      case (changingExpression, values) => changingExpression.transform(replacer) -> values.map(e => e.transform(replacer))
    } + (x -> Set(right))
    copy(permissionRecords = newPermissionRecords, changingExpressions = newChangingExpressions)
  }

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment */
  override def assignField(obj: Expression, field: String, right: Expression): QuantifiedPermissionsState = {
    val typ = right.typ
    val receiver = obj match {
      case FieldExpression(_, _, rec) => rec
      case _ => throw new IllegalStateException("Obj expression has to be a FieldExpression!")
    }
    val transformer = (orig: Expression) => orig match {
      case FieldExpression(_, `field`, rec) => ConditionalExpression(ReferenceComparisonExpression(receiver, rec, ArithmeticOperator.==, BoolType), right, orig, right.typ)
      case _ => orig
    }
    val newChangingExpressions =
    if (typ.isObject) {
      val fieldExpression = FieldExpression(typ, field, receiver)
      val replacer = (e: Expression) => if (e.equals(fieldExpression)) right else e
      changingExpressions.map {
        case (changingExpression, values) => changingExpression.transform(replacer) -> values.map(e => e.transform(replacer))
      } + (fieldExpression -> Set(right))
    } else {
      changingExpressions
    }
    val newPermissionRecords = permissionRecords.undoLastRead(field).transform(transformer).max(field, receiver, WritePermission)
    copy(permissionRecords = newPermissionRecords, changingExpressions = newChangingExpressions)
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable */
  override def setVariableToTop(varExpr: Expression): QuantifiedPermissionsState = ???

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable */
  override def removeVariable(varExpr: VariableIdentifier): QuantifiedPermissionsState = {
    copy(expr = ExpressionSet(varExpr))
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object on which the field access is performed
    * @param field the name of the field
    * @param typ   the type of the field
    * @return The abstract state obtained after the field access, that is,
    *         a new state whose `ExpressionSet` holds the symbolic representation of the value of the given field. */
  override def getFieldValue(obj: Expression, field: String, typ: Type): QuantifiedPermissionsState = {
    // TODO: handle field access
    val newPermissionRecords = permissionRecords.max(field, obj, SymbolicReadPermission)
    copy(expr = ExpressionSet(FieldExpression(typ, field, obj)), permissionRecords = newPermissionRecords)
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds */
  override def assume(cond: Expression): QuantifiedPermissionsState = {
    this
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified */
  override def before(pp: ProgramPoint): QuantifiedPermissionsState = {
    this.copy(currentPP = pp)
  }

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object */
  override def createObject(typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = {
    // TODO: implement
    this
  }

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): QuantifiedPermissionsState = {
    copy(expr = ExpressionSet(Constant(value, typ, pp)))
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable */
  override def getVariableValue(id: Identifier): QuantifiedPermissionsState = {
    copy(expr = ExpressionSet(id))
  }

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression */
  override def removeExpression(): QuantifiedPermissionsState = {
    copy(expr = ExpressionSet())
  }

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one */
  override def setExpression(expr: ExpressionSet): QuantifiedPermissionsState = {
    copy(expr = expr)
  }

  // SPECIFICATIONS
  /** Modifies the list of formal arguments using information stored in the
    * current state.
    *
    * @param existing The list of existing formal arguments.
    * @return The modified list of formal arguments*/
  override def formalArguments(existing: Seq[LocalVarDecl]): Seq[LocalVarDecl] = {
    var newFormalArguments = existing
    permissionRecords = permissionRecords.transform {
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
      case Some(rdAmount) => newPreconditions = newPreconditions :+ And(PermLtCmp(ZeroPerm, rdAmount.localVar)(), PermLtCmp(rdAmount.localVar, WritePerm)())()
      case None =>
    }
    val fieldAccessFunctions: mutable.Map[String, sil.Function] = mutable.Map()
    permissionRecords.permissions foreach { case (fieldName, permissionTree) =>
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
        newPreconditions = newPreconditions :+ sil.InhaleExhaleExp(sil.Forall(Seq(quantifiedVarDecl), Seq(), implies)(), TrueLit()())()
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
  override def invariants(existing: Seq[Exp]): Seq[Exp] = {
    val setDefinitions: mutable.Map[Expression, Expression] = mutable.Map()
    permissionRecords = permissionRecords.transform(expr => if (changingExpressions.contains(expr)) {
      expr
    } else expr)
    existing
  }

  // STUBS

  override def ids: IdentifierSet = ???

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments */
  override def glb(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other` */
  override def widening(other: QuantifiedPermissionsState): QuantifiedPermissionsState = ???

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other` */
  override def lessEqual(other: QuantifiedPermissionsState): Boolean = ???

  /** Throws an exception.
    *
    * @param t The thrown exception
    * @return The abstract state after the thrown */
  override def throws(t: ExpressionSet): QuantifiedPermissionsState = ???

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): QuantifiedPermissionsState = ???

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): QuantifiedPermissionsState = ???

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): QuantifiedPermissionsState = ???
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