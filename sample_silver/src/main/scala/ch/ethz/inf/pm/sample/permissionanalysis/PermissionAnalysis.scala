/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.analysis.{AliasAnalysisEntryStateBuilder, AliasAnalysisState, SimpleAliasAnalysisState}
import ch.ethz.inf.pm.sample.domain.{AliasDomain, MayAliasGraph, MustAliasGraph}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.inference.SilverSpecification
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.Expression
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisState.SimplePermissionAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisTypes._
import ch.ethz.inf.pm.sample.permissionanalysis.util.{Context, Permission, PermissionStack, PermissionTree}
import com.typesafe.scalalogging.LazyLogging

/**
  * Various type shortcuts.
  *
  * @author Jerome Dohrau
  */
object PermissionAnalysisTypes {
  type AccessPath = List[Identifier]

  type Tuple = (AccessPath, Permission)

  type Tuples = List[Tuple]

  def toIdentifier(path: AccessPath): Expression =
    if (path.length == 1) path.head
    else AccessPathIdentifier(path)
}

/** Used to represent new objects in access paths.
  *
  * @param typ The type of the object.
  * @param pp  The program point associated with the object.
  */
case class NewObject(typ: Type, pp: ProgramPoint = DummyProgramPoint) extends Identifier.HeapIdentifier {
  /**
    * Returns the name of the identifier. We suppose that if two identifiers return the same name if and only
    * if they are the same identifier
    *
    * @return The name of the identifier
    */
  override def getName: String = "new()"

  /**
    * Returns the name of the field that is represented by this identifier if it is a heap identifier.
    *
    * @return The name of the field pointed by this identifier
    */
  override def getField: Option[String] = ???

  /**
    * Since an abstract identifier can be an abstract node of the heap, it can represent more than one concrete
    * identifier. This function tells if a node is a summary node.
    *
    * @return true iff this identifier represents exactly one variable
    */
  override def representsSingleVariable: Boolean = ???
}

/**
  * A state of the permission analysis.
  *
  * @tparam T    The type of the permission analysis state.
  * @tparam A    The type of the alias analysis state.
  * @tparam May  The type of the may alias domain used by the alias analysis.
  * @tparam Must The type of the must alias domain used by the alias analysis.
  * @author Jerome Dohrau
  */
trait PermissionAnalysisState[T <: PermissionAnalysisState[T, A, May, Must], A <: AliasAnalysisState[A, May, Must], May <: AliasDomain[May, _], Must <: AliasDomain[Must, _]]
  extends SilverState[T]
    with SilverSpecification[PermissionStack]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {
  this: T =>

  // current program point
  def currentPP: ProgramPoint

  // result of the previous statement
  def result: ExpressionSet

  // permission stack
  def stack: PermissionStack

  def inferred: Option[PermissionStack]

  // result of the alias analysis before the current program point
  lazy val preAliases = Context.getAliases[A].preStateAt(currentPP)

  // result of the alias analysis after the current program point
  lazy val postAliases = Context.getAliases[A].postStateAt(currentPP)

  // the list of access paths
  lazy val paths: List[AccessPath] = stack.trees.foldLeft(List.empty[AccessPath]) {
    (list, tree) => tree.fold(list) { case (xs, (x, _)) => x :: xs }
  }

  /* ------------------------------------------------------------------------- *
   * LATTICE FUNCTIONS
   */

  override def factory(): T = {
    logger.trace("factory")
    this
  }

  override def top(): T = {
    logger.trace("top")
    copy(
      result = result.top(),
      stack = stack.top(),
      inferred = None,
      isBottom = false,
      isTop = true)
  }

  override def bottom(): T = {
    logger.trace("bottom")
    copy(
      result = result.bottom(),
      stack = stack.bottom(),
      inferred = None,
      isBottom = true,
      isTop = false)
  }

  override def lub(other: T): T = {
    logger.trace(s"lub(${this.toString}, ${other.toString})")
    copy(
      isBottom = isBottom && other.isBottom,
      isTop = isTop || other.isTop,
      stack = stack lub other.stack)
  }

  override def glb(other: T): T = {
    logger.trace("glb")
    copy(
      isBottom = isBottom || other.isBottom,
      isTop = isTop && other.isTop,
      stack = stack glb other.stack)
  }

  override def widening(other: T): T = {
    logger.trace(s"widening(${this.toString}, ${other.toString})")
    copy(
      isBottom = isBottom && other.isBottom,
      isTop = isTop || other.isTop,
      stack = stack widening other.stack)
  }

  override def lessEqual(other: T): Boolean = {
    logger.trace(s"lessEqual(${this.toString}, ${other.toString})")
    // handle cases involving bottom and top
    if (isBottom || other.isTop) true
    else if (other.isBottom || isTop) false
    else stack lessEqual other.stack
  }

  /* ------------------------------------------------------------------------- *
   * SILVER STATE FUNCTIONS
   */

  def inhale(expression: Expression): T =
    _inhale(expression).makeSelfFraming()

  def exhale(expression: Expression): T =
    _exhale(expression).makeSelfFraming()

  def makeSelfFraming(): T = {
    copy(stack = stack.mapTrees(_.makeSelfFraming()))
  }

  override def precondition(expression: Expression): T =
    _inhale(expression).saveSpecifications()

  override def postcondition(expression: Expression): T =
    exhale(expression).saveSpecifications()

  override def invariant(expression: Expression): T = {
    // TODO: "assert" the invariant instead of exhaling and inhaling it.
    val inhaled = inhale(expression)
    inhaled.setSpecifications(inhaled.stack).exhale(expression)
  }

  override def enterLoop(): T = copy(stack = stack.pop)

  override def leaveLoop(): T = copy(stack = stack.push)

  /**
    * Inhales the given expression but does not make the state self-framing.
    */
  private def _inhale(expression: Expression): T = {
    logger.trace(s"inhale($expression)")
    expression match {
      case BinaryBooleanExpression(left, right, BooleanOperator.&&) =>
        inhale(right).inhale(left)
      case FieldAccessPredicate(identifier, amount) =>
        // get access path
        val location = path(identifier)
        // get the amount of permission that is inhaled
        val inhaled = permission(amount)
        // add permission to all paths that must alias
        val updated = stack.mapPermissions { (path, tree) =>
          if (mustBeSame(postAliases, path, location))
            (tree.permission minus inhaled) lub Permission.none
          else
            tree.permission
        }
        copy(stack = updated).read(location.dropRight(1))
      case _ => assume(expression)
    }
  }

  /**
    * Exhales the given expression but does not make the state self-framing.
    */
  private def _exhale(expression: Expression): T = {
    logger.trace(s"exhale($expression)")
    expression match {
      case BinaryBooleanExpression(left, right, BooleanOperator.&&) =>
        // inhale both sides of the conjunction
        _exhale(left)._exhale(right)
      case FieldAccessPredicate(identifier, amount) =>
        // get access path
        val location = path(identifier)
        // get the amount of permission that is exhaled
        val exhaled = permission(amount)
        // subtract permission form all paths that may alias
        val updated = stack.mapPermissions { (path, tree) =>
          if (mustBeSame(preAliases, path, location)) {
            if (tree.permission().isNone && tree.nonEmpty()) Permission.read plus exhaled
            else tree.permission() plus exhaled
          } else if (mayBeSame(preAliases, path, location)) {
            if (tree.permission().isSome) tree.permission() plus exhaled
            else if (tree.nonEmpty()) Permission.read plus exhaled
            else tree.permission()
          } else tree.permission()
        }
        copy(stack = updated).access(location, exhaled)
      case bool if bool.typ.isBooleanType =>
        // we do not assert boolean conditions since the analysis would fail
        // in all cases where we are not able to prove that something holds.
        read(bool)
      case _ =>
        throw new IllegalArgumentException("An exhale must occur via a boolean or a permission expression.")
    }
  }

  /* ------------------------------------------------------------------------- *
   * STATE FUNCTIONS
   */

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param variable The name of the argument.
    * @param typ      The static type of the argument.
    * @return The abstract state after the creation of the argument.
    */
  override def createVariableForArgument(variable: VariableIdentifier, typ: Type): T = {
    logger.trace("createVariableForArgument")
    this
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom.
    *
    * @param varExpr The variable to be removed.
    * @return The abstract state obtained after removing the variable.
    */
  override def removeVariable(varExpr: VariableIdentifier): T = {
    logger.trace("removeVariable")
    this
  }

  /** Accesses a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   The object on which the field access is performed.
    * @param field The name of the field.
    * @param typ   The type of the field.
    * @return The abstract state obtained after the field access, that is,
    *         a new state whose `ExpressionSet` holds the symbolic
    *         representation of the value of the given field.
    */
  override def getFieldValue(obj: Expression, field: String, typ: Type): T = {
    logger.trace("getFieldValue")
    obj match {
      case id: VariableIdentifier =>
        val fieldId = VariableIdentifier(field)(typ)
        val newPath = AccessPathIdentifier(List(id, fieldId))
        copy(result = ExpressionSet(newPath))
      case AccessPathIdentifier(path) =>
        val fieldId = VariableIdentifier(field)(typ)
        val newPath = AccessPathIdentifier(path ++ List(fieldId))
        copy(result = ExpressionSet(newPath))
      case _ => throw new IllegalArgumentException("A field access must occur via an identifier.")
    }
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param condition The assumed expression.
    * @return The abstract state after assuming that the expression holds.
    */
  override def assume(condition: Expression): T = {
    logger.trace("assume")
    // add read permissions for all access paths appearing in the condition
    read(condition)
  }

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param variable The name of the variable.
    * @param typ      The static type of the variable.
    * @param pp       The program point that creates the variable.
    * @return The abstract state after the creation of the variable.
    */
  override def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    logger.trace("createVariable")
    copy(stack = stack.mapTrees(_.remove(variable)))
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param left  The assigned variable.
    * @param right The assigned expression.
    * @return The abstract state after the assignment.
    */
  override def assignVariable(left: Expression, right: Expression): T = {
    logger.trace("assignVariable")
    left match {
      case variable: VariableIdentifier =>
        // check whether assigned variable is a reference
        if (variable.typ.isObject) {
          // case 1: the assigned variable is a reference
          // get access paths corresponding to lhs and rhs
          val leftPath = path(left)
          val rightPath = path(right)
          // assign rhs path to lhs path
          assign(leftPath, rightPath).write(leftPath).read(rightPath).makeSelfFraming()
        } else {
          // case 2: assigned variable is not a reference
          // add read permission for all access paths appearing in rhs
          read(right).makeSelfFraming()
        }
      case _ => throw new IllegalArgumentException("A variable assignment must occur via a variable identifier.")
    }
  }

  /** Forgets the value of a variable.
    *
    * Implementations can assume this state is non-bottom.
    *
    * @param varExpr The variable to be forgotten.
    * @return The abstract state obtained after forgetting the variable.
    */
  override def setVariableToTop(varExpr: Expression): T = ???

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   The object whose field is assigned.
    * @param field The assigned field.
    * @param right The assigned expression.
    * @return the abstract state after the assignment.
    */
  override def assignField(obj: Expression, field: String, right: Expression): T = {
    logger.trace("assignField")
    obj match {
      case left@AccessPathIdentifier(leftPath) =>
        // check whether lhs is a reference
        if (obj.typ.isObject) {
          // case 1: the assigned field is a reference
          // get access paths corresponding to rhs
          val rightPath = path(right)

          val accessPaths = paths.sortBy(-_.length)
          // process long paths before short ones
          val assigned =
            if (rightPath.isEmpty) assign(leftPath, rightPath)
            else accessPaths.foldLeft(this) {
              case (res, path) if path.nonEmpty =>
                val p = toIdentifier(path)
                if (path == leftPath) res.assign(path, rightPath)
                else if (path.length > 1 && postAliases.mayAlias(p, right))
                  if (postAliases.mustAlias(p, right))
                    res.assign(path, rightPath)
                  else
                    res lub res.assign(path, rightPath)
                else
                  res
              case (res, _) => res
            }
          assigned.write(leftPath).read(rightPath).makeSelfFraming()
        } else {
          // case 2: the assigned field is not a reference
          // add write permission for lhs and write permission for all access paths on rhs
          write(leftPath).read(right).makeSelfFraming()
        }
      case _ => throw new IllegalArgumentException("A field assignment must occur via an access path identifier.")
    }
  }

  /** Assigns an expression to an argument.
    *
    * @param x     The assigned argument.
    * @param right The expression to be assigned.
    * @return The abstract state after the assignment.
    */
  override def setArgument(x: ExpressionSet, right: ExpressionSet): T = ??? // ignore

  /** Removes the current expression.
    *
    * @return The abstract state after removing the current expression
    */
  override def removeExpression(): T = {
    logger.trace("removeExpression")
    copy(result = ExpressionSet())
  }

  /** Throws an exception.
    *
    * @param t The thrown exception.
    * @return The abstract state after the thrown.
    */
  override def throws(t: ExpressionSet): T = ??? // ignore

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = ??? // ignore

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant.
    * @param typ   The type of the numerical constant.
    * @param pp    The program point that contains the constant.
    * @return The abstract state after the evaluation of the constant, that is,
    *         the state that contains an expression representing this constant.
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    logger.trace("evalConstant")
    val constant = Constant(value, typ)(pp)
    copy(result = ExpressionSet(constant))
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following
    * the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed.
    * @return The abstract state eventually modified.
    */
  override def before(pp: ProgramPoint): T = {
    logger.trace(s"before($pp)")
    copy(currentPP = pp)
  }

  /** Performs abstract garbage collection.
    */
  override def pruneUnreachableHeap(): T = ??? // ignore

  /** Returns the current expression.
    */
  override def expr: ExpressionSet = {
    logger.trace("expr")
    result
  }

  /** Creates an object.
    *
    * @param typ The dynamic type of the created object.
    * @param pp  The point of the program that creates the object.
    * @return The abstract state after the creation of the object.
    */
  override def createObject(typ: Type, pp: ProgramPoint): T = {
    logger.trace("createObject")
    val obj = NewObject(typ, pp)
    copy(result = ExpressionSet(obj))
  }

  /** Sets the current expression.
    *
    * @param expr The current expression.
    * @return The abstract state after changing the current expression with the
    *         given one.
    */
  override def setExpression(expr: ExpressionSet): T = {
    logger.trace("setExpression")
    copy(result = expr)
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access.
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): T = {
    logger.trace("getVariableValue")
    copy(result = ExpressionSet(id))
  }

  /* ------------------------------------------------------------------------- *
   * HELPER FUNCTIONS FOR INFERENCE
   */

  override def specifications: PermissionStack =
    inferred.getOrElse(stack)

  private def saveSpecifications(): T =
    copy(inferred = Some(stack))

  private def setSpecifications(specifications: PermissionStack): T =
    copy(inferred = Some(specifications))

  private def forgetSpecifications(): T =
    copy(inferred = None)

  /** Extracts the path from an expression.
    *
    * @param expression The expression to extract the path from.
    */
  private def path(expression: Expression): AccessPath = expression match {
    case _: Constant => Nil
    case id: VariableIdentifier => List(id)
    case obj: NewObject => List(obj)
    case AccessPathIdentifier(path) => path
    case _ => throw new IllegalArgumentException("Expected an access path identifier")
  }

  private def permission(expression: Expression): Permission = expression match {
    case FractionalPermissionExpression(left, right) => (left, right) match {
      case (Constant(numerator, _), Constant(denominator, _)) =>
        Permission.fractional(numerator.toInt, denominator.toInt)
    }
  }

  /** Adds read permission for all access paths appearing in the specified
    * expression.
    *
    * @param expression The expression to add read permission for.
    */
  private def read(expression: Expression): T = {
    val ids = expression.transform {
      // ignore all current permission expressions
      case CurrentPermission(_, typ) => Constant("ignore", typ)()
      case e => e
    }.ids.getNonTop

    ids.foldLeft(this) {
      case (result, identifier) => identifier match {
        case AccessPathIdentifier(path) => result.read(path) // read permission for path
        case _: VariableIdentifier => result // no permission needed
      }
    }
  }

  /** Adds read permission for the specified path. If the permission is already
    * there nothing happens.
    *
    * @param path The path to add the permission for.
    */
  private def read(path: AccessPath): T =
    access(path, Permission.read)

  /** Adds write permission for the specified path. If the permission is already
    * there nothing happens.
    *
    * @param path The path to add the permission for.
    */
  private def write(path: AccessPath): T =
    copy(stack = stack.updatePaths(_ + path)).access(path, Permission.write)

  /** Adds the specified permission for the specified access path. If the
    * permission is already there nothing happens.
    *
    * @param path       The path to add the permission for.
    * @param permission The amount of permissions to add.
    */
  private def access(path: AccessPath, permission: Permission): T = {
    val p = if (path.nonEmpty && path.last.getName == "[]") path.init else path
    if (p.length < 2)
    // in this case no permission is needed
      this
    else {
      // build permission tree for the wanted permission
      val tree = PermissionTree.create(p, permission)
      copy(stack = stack.updateTree(_ lub tree))
    }
  }

  /** Collects the permission of of all access paths that must alias with but
    * are not equal to the specified access path.
    *
    * @param path The path.
    * @return A lower bound on the amount of permission held for the specified
    *         access path.
    */
  private def collect(path: AccessPath): Permission =
    if (path.length < 2) Permission.none
    else stack.head.tree.fold(Permission.none) { case (permission, (currPath, currTree)) =>
      if (path != currPath && mustBeSame(preAliases, path, currPath)) permission plus currTree.permission
      else permission
    }

  private def assign(left: AccessPath, right: AccessPath): T = {
    if (left.isEmpty || right.isEmpty) this
    else if (right.head.isInstanceOf[NewObject]) {
      val (remainderEntries, extractedEntries) = stack.entries.unzip { entry =>
        val (remainder, extracted) = entry.tree.extract(left)
        (entry.copy(tree = remainder), entry.copy(tree = extracted))
      }
      val remainderStack = PermissionStack(remainderEntries)
      val extractedStack = PermissionStack(extractedEntries)
      copy(stack = remainderStack, inferred = Some(extractedStack))
    } else {
      val updated = stack.mapTrees { tree =>
        val (remainder, extracted) = tree.extract(left)
        remainder.implant(right, extracted)
      }
      copy(stack = updated)
    }
  }

  /* ------------------------------------------------------------------------- *
   * HELPER FUNCTIONS FOR ALIAS ANALYSIS
   */

  /** Returns true if the two given access paths may refer to the same field on
    * the same receiver object.
    *
    * @param aliases The alias information.
    * @param first   The first access path.
    * @param second  The second access path.
    * @return True if the two given access paths may refer to the same field on
    *         the same receiver object.
    */
  private def mayBeSame(aliases: A, first: AccessPath, second: AccessPath): Boolean =
    if (first.length < 2 || second.length < 2) false
    else if (first == second) true
    else aliases.mayAlias(toIdentifier(first.init), toIdentifier(second.init)) && first.last == second.last

  /** Returns true if the two given access paths must refer to the same field on
    * the same receiver object.
    *
    * @param aliases The alias information.
    * @param first   The first access path.
    * @param second  The second access path.
    * @return True if the two given access paths may refer to the same field on
    *         the same receiver object.
    */
  private def mustBeSame(aliases: A, first: AccessPath, second: AccessPath): Boolean =
    if (first.length < 2 || second.length < 2) false
    else if (first == second) true
    else aliases.mustAlias(toIdentifier(first.init), toIdentifier(second.init)) && first.last == second.last

  /* ------------------------------------------------------------------------- *
   *
   */

  def copy(currentPP: ProgramPoint = currentPP,
           result: ExpressionSet = result,
           stack: PermissionStack = stack,
           inferred: Option[PermissionStack] = inferred,
           isBottom: Boolean = isBottom,
           isTop: Boolean = isTop): T


  override def toString: String =
    if (isTop) "⊤"
    else if (isBottom) "⊥"
    else s"inferred:\n ${inferred.getOrElse("none")}\nstack:\n$stack"

  override def ids = IdentifierSet.Top

}

object PermissionAnalysisState {

  case class SimplePermissionAnalysisState(currentPP: ProgramPoint = DummyProgramPoint,
                                           result: ExpressionSet = ExpressionSet(),
                                           stack: PermissionStack = PermissionStack.empty,
                                           inferred: Option[PermissionStack] = None,
                                           isBottom: Boolean = false,
                                           isTop: Boolean = false)
    extends PermissionAnalysisState[SimplePermissionAnalysisState, SimpleAliasAnalysisState, MayAliasGraph, MustAliasGraph] {
    override def copy(currentPP: ProgramPoint,
                      result: ExpressionSet,
                      stack: PermissionStack,
                      inferred: Option[PermissionStack],
                      isBottom: Boolean,
                      isTop: Boolean): SimplePermissionAnalysisState =
      SimplePermissionAnalysisState(currentPP, result, stack, inferred, isBottom, isTop)
  }

}

case class PermissionAnalysisEntryStateBuilder()
  extends SilverEntryStateBuilder[SimplePermissionAnalysisState] {
  override def default: PermissionAnalysisState.SimplePermissionAnalysisState = PermissionAnalysisState.SimplePermissionAnalysisState()
}

case class PermissionAnalysis[T <: PermissionAnalysisState[T, A, May, Must], A <: AliasAnalysisState[A, May, Must], May <: AliasDomain[May, _], Must <: AliasDomain[Must, _]]
(aliasAnalysisStateBuilder: SilverEntryStateBuilder[A],
 permissionAnalysisStateBuilder: SilverEntryStateBuilder[T])
  extends SilverAnalysis[T] {
  override def analyze(program: SilverProgramDeclaration): ProgramResult[T] = {
    // initialize context
    Context.setProgram(program)

    super.analyze(program)
  }

  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[T] = {
    // update context
    Context.setMethod(method.name)

    // first phase: alias analysis
    val aliasEntry = aliasAnalysisStateBuilder.build(program, method)
    val aliasInterpreter = FinalResultForwardInterpreter[A](method.body, aliasEntry)
    val aliasResult = aliasInterpreter.execute()

    // add result of alias analysis to context
    Context.setAliases(aliasResult)

    // second phase: permission analysis
    val permissionEntry = permissionAnalysisStateBuilder.build(program, method)
    val permissionInterpreter = FinalResultBackwardInterpreter[T](method.body, permissionEntry)
    val permissionResult = permissionInterpreter.execute()

    // return result of the permission analysis
    permissionResult
  }
}

trait DebugPermissionInferenceRunner[T <: PermissionAnalysisState[T, A, May, Must], A <: AliasAnalysisState[A, May, Must], May <: AliasDomain[May, _], Must <: AliasDomain[Must, _]]
  extends SilverAnalysisRunner[T] {
  override def main(arguments: Array[String]) {
    // check whether there is a first argument (the path to the file)
    if (arguments.isEmpty) throw new IllegalArgumentException("No file specified")

    // run analysis
    val path = new File(arguments(0)).toPath
    val results = run(Compilable.Path(path))

    println("\n*******************\n* Analysis Result *\n*******************\n")
    for (method <- results.identifiers) {
      println("******************* " + method + "\n")
      results.getResult(method).print()
    }
  }
}

object DebugPermissionInference
  extends DebugPermissionInferenceRunner[SimplePermissionAnalysisState, SimpleAliasAnalysisState, MayAliasGraph, MustAliasGraph] {
  //override val analysis: SilverAnalysis[SimplePermissionAnalysisState] = PermissionAnalysis(AliasAnalysisEntryStateBuilder(), PermissionAnalysisEntryStateBuilder())
  override val analysis: SilverAnalysis[SimplePermissionAnalysisState] = PermissionAnalysis[SimplePermissionAnalysisState, SimpleAliasAnalysisState, MayAliasGraph, MustAliasGraph](AliasAnalysisEntryStateBuilder(), PermissionAnalysisEntryStateBuilder())
}

