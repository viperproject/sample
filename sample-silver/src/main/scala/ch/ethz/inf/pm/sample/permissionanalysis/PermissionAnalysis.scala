/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.oorepresentation.silver.sample.Expression
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState.SimpleAliasAnalysisState
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
  type AccessPath = AliasAnalysisTypes.AccessPath

  type Tuple = (AccessPath, Permission)

  type Tuples = List[Tuple]
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
  override def getName: String = ???

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
  * @tparam T The type of the permission analysis state.
  * @author Jerome Dohrau
  */
trait PermissionAnalysisState[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[A, T]]
  extends SilverState[T]
    with SilverSpecification[PermissionTree]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {
  this: T =>

  // current program point
  def currentPP: ProgramPoint

  // result of the previous statement
  def result: ExpressionSet

  // permission stack
  def stack: PermissionStack

  def inferred: Option[PermissionTree]

  // result of the alias analysis before the current program point
  lazy val preAliases = Context.preAliases[A](currentPP)

  // result of the alias analysis after the current program point
  lazy val postAliases = Context.postAliases[A](currentPP)

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
   * STATE FUNCTIONS
   */

  /**
    * Processes the given precondition.
    *
    * @param expression The expression representing the precondition.
    * @return The state after processing the given precondition.
    */
  override def precondition(expression: Expression): T =
    inhale(expression).saveSpecifications()

  /**
    * Processes the given postcondition.
    *
    * @param expression The expression representing the postcondition.
    * @return The state after processing the postcondition.
    */
  override def postcondition(expression: Expression): T =
    exhale(expression).saveSpecifications()

  /**
    * Processes the given invariant.
    *
    * @param expression The expression representing the invariant.
    * @return The state after processing the invariant.
    */
  override def invariant(expression: Expression): T = {
    // TODO: "assert" invariant rather than exhale it.
    val exhaled = exhale(expression)
    setSpecifications(exhaled.stack.headTree)
  }

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale.
    * @return The abstract state after exhaling the permission.
    */
  def exhale(acc: Expression): T = {
    logger.trace(s"exhale($acc)")
    acc match {
      case BinaryBooleanExpression(left, right, BooleanOperator.&&) =>
        // inhale both sides of the conjunction
        exhale(left).exhale(right)
      case FieldAccessPredicate(identifier, numerator, denominator, _) =>
        // get access path
        val location = path(identifier)
        // get the amount of permission that is exhaled
        val exhaled = permission(numerator, denominator)
        // subtract permission form all paths that may alias
        val updated = stack.mapPermissions { (path, tree) =>
          if (mayBeSame(preAliases, path, location)) {
            if (tree.permission.isSome || tree.isEmpty) tree.permission plus exhaled
            else Permission.read plus exhaled
          }
          else tree.permission
        }
        copy(stack = updated).access(location, exhaled)
      case bool if bool.typ.isBooleanType =>
        // we do not assert boolean conditions since the analysis would fail
        // in all cases where we are not able to prove that something holds.
        this
      case _ =>
        throw new IllegalArgumentException("An exhale must occur via a boolean or a permission expression.")
    }
  }

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale.
    * @return The abstract state after inhaling the permission.
    */
  def inhale(acc: Expression): T = {
    logger.trace(s"inhale($acc)")
    acc match {
      case BinaryBooleanExpression(left, right, BooleanOperator.&&) =>
        inhale(right).inhale(left)
      case FieldAccessPredicate(identifier, numerator, denominator, _) =>
        // get access path
        val location = path(identifier)
        // get the amount of permission that is inhaled
        val inhaled = permission(numerator, denominator)
        // add permission to all paths that must alias
        val updated = stack.mapPermissions { (path, tree) =>
          if (mustBeSame(postAliases, path, location)) tree.permission minus inhaled
          else tree.permission
        }
        copy(stack = updated).read(location.dropRight(1))
      case _ => assume(acc)
    }
  }

  /**
    * This method is invoked to signal the state that a loop is being entered.
    *
    * @return The state after entering the loop.
    */
  override def enterLoop(): T = copy(stack = stack.pop)

  /**
    * This method is invoked to signal the state that a loop is being left.
    *
    * @return The state after leaving the loop.
    */
  override def leaveLoop(): T = copy(stack = stack.push)

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
          assign(leftPath, rightPath).write(leftPath).read(rightPath)
        } else {
          // case 2: assigned variable is not a reference
          // add read permission for all access paths appearing in rhs
          read(right)
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
      case AccessPathIdentifier(leftPath) =>
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
              case (res, path) =>
                postAliases
                if (path == leftPath) res.assign(path, rightPath)
                else if (path.length > 1 && postAliases.pathsMayAlias(path, rightPath))
                  if (postAliases.pathsMustAlias(path, rightPath)) res.assign(path, rightPath)
                  else res lub res.assign(path, rightPath)
                else res
            }
          assigned.write(leftPath).read(rightPath)
        } else {
          // case 2: the assigned field is not a reference
          // add write permission for lhs and write permission for all access paths on rhs
          write(leftPath).read(right)
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
    val constant = Constant(value, typ, pp)
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

  override def specifications: PermissionTree =
    inferred.getOrElse(stack.headTree)

  private def saveSpecifications(): T =
    copy(inferred = Some(stack.headTree))

  private def setSpecifications(specifications: PermissionTree): T =
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

  /** Returns a permission where the amount corresponds to the fraction
    * represented by the specified numerator and denominator.
    *
    * @param numerator   The numerator of the fraction.
    * @param denominator The denominator of the fraction.
    */
  private def permission(numerator: Expression, denominator: Expression): Permission =
    (numerator, denominator) match {
      case (Constant(nValue, _, _), Constant(dValue, _, _)) =>
        Permission.fractional(nValue.toInt, dValue.toInt)
      case (VariableIdentifier("read", _), Constant("1", _, _)) =>
        Permission.read
      case _ => ??? // TODO: support more cases
    }

  /** Adds read permission for all access paths appearing in the specified
    * expression.
    *
    * @param expression The expression to add read permission for.
    */
  private def read(expression: Expression): T = {
    val ids = expression.transform {
      // ignore all current permission expressions
      case CurrentPermission(_, typ) => Constant("ignore", typ)
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
    if (path.length < 2)
    // in this case no permission is needed
      this
    else {
      // build permission tree for the wanted permission
      val want = permission minus collect(path)
      val tree = PermissionTree.create(path, want)

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
      /*val extracted = stack.trees.map(_.extract(left))
      val newStack = extracted.map(_._1)
      val specifications = extracted.map(_._2).reduce(_ lub _)
      copy(stack = newStack, inferred = Some(specifications))*/
      // TODO:
      ???
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
    else aliases.pathsMayAlias(first.init, second.init) && first.last == second.last

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
    else aliases.pathsMustAlias(first.init, second.init) && first.last == second.last

  /* ------------------------------------------------------------------------- *
   *
   */

  def copy(currentPP: ProgramPoint = currentPP,
           result: ExpressionSet = result,
           stack: PermissionStack = stack,
           inferred: Option[PermissionTree] = inferred,
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
                                           inferred: Option[PermissionTree] = None,
                                           isBottom: Boolean = false,
                                           isTop: Boolean = false)
    extends PermissionAnalysisState[SimpleAliasAnalysisState, SimplePermissionAnalysisState] {
    override def copy(currentPP: ProgramPoint,
                      result: ExpressionSet,
                      stack: PermissionStack,
                      inferred: Option[PermissionTree],
                      isBottom: Boolean,
                      isTop: Boolean): SimplePermissionAnalysisState =
      SimplePermissionAnalysisState(currentPP, result, stack, inferred, isBottom, isTop)
  }

}

object PermissionAnalysisEntryState
  extends SimpleEntryStateBuilder[SimplePermissionAnalysisState] {
  override def top: PermissionAnalysisState.SimplePermissionAnalysisState = PermissionAnalysisState.SimplePermissionAnalysisState()
}

case class PermissionAnalysis[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[A, T]]
(aliasAnalysisStateBuilder: AliasAnalysisStateBuilder[A],
 permissionAnalysisStateBuilder: SimpleEntryStateBuilder[T])
  extends SilverAnalysis[T] {
  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[T] = {
    // initialize context
    Context.setProgram(program)

    // first phase: alias analysis
    val aliasEntry = aliasAnalysisStateBuilder.build(program, method)
    val aliasInterpreter = FinalResultForwardInterpreter[A]()
    val aliasResult = aliasInterpreter.execute(method.body, aliasEntry)

    // add result of alias analysis to context
    Context.setAliases(aliasResult)

    // second phase: permission analysis
    val permissionEntry = permissionAnalysisStateBuilder.build(program, method)
    val permissionInterpreter = FinalResultBackwardInterpreter[T]()
    val permissionResult = permissionInterpreter.execute(method.body, permissionEntry)

    // remove result of alias analysis from the context
    Context.clearAliases()

    // return result of the permission analysis
    permissionResult
  }
}

trait DebugPermissionInferenceRunner[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[A, T]]
  extends SilverAnalysisRunner[T] {
  override def main(arguments: Array[String]) {
    // check whether there is a first argument (the path to the file)
    if (arguments.isEmpty) throw new IllegalArgumentException("No file specified")

    // run analysis
    val path = new File(arguments(0)).toPath
    val results = run(Compilable.Path(path)).collect { case x => x }

    val cfgResults = results.map { case (id, cfgResult) => id.name -> cfgResult }
    println("\n*******************\n* Analysis Result *\n*******************\n")
    for ((method, cfgResult) <- cfgResults) {
      println("******************* " + method + "\n")
      cfgResult.print()
    }
  }
}

object DebugPermissionInference
  extends DebugPermissionInferenceRunner[SimpleAliasAnalysisState, SimplePermissionAnalysisState] {
  override val analysis: SilverAnalysis[SimplePermissionAnalysisState] = PermissionAnalysis(AliasAnalysisEntryState, PermissionAnalysisEntryState)
}

