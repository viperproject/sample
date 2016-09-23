/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverAnalysisRunner, SilverInferenceRunner, SilverSpecification}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Statement, Type}
import ch.ethz.inf.pm.sample.permissionanalysis.Permission.Fractional
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

/** Represents a permission.
  *
  * @author Jerome Dohrau
  */
trait Permission
  extends Lattice[Permission]
{
  override def factory(): Permission = top()

  override def top(): Permission = Permission.Top

  override def bottom(): Permission = Permission.Bottom

  override def lub(other: Permission): Permission =
    if (this lessEqual other) other
    else if (other lessEqual this) this
    else top()

  override def glb(other: Permission): Permission =
    if (this lessEqual other) this
    else if (other lessEqual this) other
    else bottom()

  override def widening(other: Permission): Permission =
    if (other lessEqual this) this
    else top()

  /** Returns the sum of this permission and the other permission.
    *
    * @param other The permission to be added.
    */
  def plus(other: Permission): Permission

  /** Returns the difference of this permission and the other permission.
    *
    * @param other The permission to be subtracted.
    */
  def minus(other: Permission): Permission

  /** Returns whether the amount of the permission is strictly greater than zero.
    */
  def isSome: Boolean

  /** Returns whether the amount of the permission is at most zero.
    */
  def isNone: Boolean
}

object Permission {
  /** Returns no permission.
    */
  def none: Permission = fractional(0, 1)

  /** Returns a read permission.
    */
  def read: Permission = fractional(0, 1, read = true)

  /** Returns a write permission.
    */
  def write: Permission = fractional(1, 1)

  /** Returns a permission that is the sum of a fractional permission and a read
    * permission
    *
    * @param numerator   The numerator of the fractional part.
    * @param denominator The denominator of the fractional part.
    * @param read        Indicates whether there is a read part.
    */
  def fractional(numerator: Int, denominator: Int, read: Boolean = false): Permission = {
    val div = gcd(numerator, denominator)
    Fractional(numerator / div, denominator / div, read)
  }

  case object Top extends Permission with Lattice.Top[Permission] {
    override def plus(other: Permission): Permission = top()

    override def minus(other: Permission): Permission = top()

    override def isSome: Boolean = true

    override def isNone: Boolean = false
  }

  case object Bottom extends Permission with Lattice.Bottom[Permission] {
    override def plus(other: Permission): Permission =
      if (other.isTop) top()
      else bottom()

    override def minus(other: Permission): Permission =
      if (other.isBottom) top()
      else bottom()

    override def isSome: Boolean = false

    override def isNone: Boolean = true
  }

  /** A permission that is the sum of a fractional permission and a read
    * permission
    *
    * @param numerator   The numerator of the fractional part.
    * @param denominator The denominator of the fractional part.
    * @param read        Indicates whether there is a read part.
    */
  case class Fractional(numerator: Int, denominator: Int, read: Boolean)
    extends Permission
  {
    override def isBottom: Boolean = false

    override def isTop: Boolean = false

    override def lessEqual(other: Permission): Boolean = other match {
      case Top => true
      case Bottom => false
      case Fractional(oNumerator, oDenominator, oRead) =>
        val x = numerator * oDenominator
        val y = denominator * oNumerator
        x < y || (x == y && (!read || oRead))
    }

    override def plus(other: Permission): Permission = other match {
      case Top => top()
      case Bottom => bottom()
      case Fractional(oNumerator, oDenominator, oRead) =>
        val newNumerator = numerator * oDenominator + denominator * oNumerator
        val newDenominator = denominator * oDenominator
        val newRead = read | oRead
        fractional(newNumerator, newDenominator, newRead)
    }

    override def minus(other: Permission): Permission = other match {
      case Top => bottom()
      case Bottom => top()
      case Fractional(oNumerator, oDenominator, oRead) =>
        val newNumerator = numerator * oDenominator - denominator * oNumerator
        val newDenominator = denominator * oDenominator
        val newRead = read && !oRead
        fractional(newNumerator, newDenominator, newRead)
    }

    override def isSome: Boolean =
      amount > 0 || (amount == 0 && read)

    override def isNone: Boolean =
      amount < 0 || (amount == 0 && !read)

    def amount: Double =
      numerator.toDouble / denominator
  }

  /** Computes the greatest common divisor of the two specified integers.
    *
    * @param a The first integer.
    * @param b The second integer.
    */
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a.abs
    else gcd(b, a % b)
}

/**
  * @param permission amount of permission for the root of the tree
  * @param children   maps fields (identifiers) to subtrees
  */
case class PermissionTree(permission: Permission = Permission.none,
                          children: Map[Identifier, PermissionTree] = Map.empty)
{
  type AccessPath = List[Identifier]

  /** Is true if this tree contains no permission.
    */
  lazy val isEmpty: Boolean =
    permission.isNone && children.forall { case (_, child) => child.isEmpty }

  /** Is true if this tree contains some permission.
    */
  lazy val nonEmpty: Boolean =
    !isEmpty

  /** Returns the least upper bound of this permission tree and the other permission tree.
    *
    * @param other The other permission tree.
    */
  def lub(other: PermissionTree): PermissionTree = {
    // compute lub of permissions
    val newPermission = permission lub other.permission
    // compute child-wise lub of subtrees
    val newChildren = children.foldLeft(other.children) {
      case (map, (id, subtree)) => map.get(id) match {
        case Some(existing) => map + (id -> (subtree lub existing))
        case None => map + (id -> subtree)
      }
    }
    PermissionTree(newPermission, newChildren)
  }

  /**  Returns the greatest lower bound of this permission tree and the other permission tree.
    *
    * @param other The other permission tree.
    */
  def glb(other: PermissionTree): PermissionTree = {
    // computer glb of permissions
    val newPermission = permission glb other.permission
    // compute child-wise glb of subtrees
    val newChildren = children.foldLeft(Map.empty[Identifier, PermissionTree]) {
      case (map, (id, subtree)) => other.children.get(id) match {
        case Some(existing) => map + (id -> (subtree glb existing))
        case None => map
      }
    }
    PermissionTree(newPermission, newChildren)
  }

  /** Returns the widening of this and the other permission tree.
    *
    * @param other The other permission tree.
    * @return The widening of this and the other permission tree.
    */
  def widening(other: PermissionTree): PermissionTree = {
    // TODO: Implement me properly. Currently this is only a point-wise widening
    // compute widening of permissions
    val newPermission = permission widening other.permission
    // compute child-wise widening of subtrees
    val newChildren = children.foldLeft(other.children) {
      case (map, (id, subtree)) => map.get(id) match {
        case Some(existing) => map + (id -> (subtree widening existing))
        case None => map + (id -> subtree)
      }
    }
    PermissionTree(newPermission, newChildren)
  }

  /** Returns whether the amount of permission of this permission tree is less
    * than or equal to the amount of permission  of the other tree.
    *
    * @param other The other permission.
    */
  def lessEqual(other: PermissionTree): Boolean = {
    if (permission lessEqual other.permission) {
      // check for all subtrees whether other has a corresponding subtree that has at least as much permission
      children.forall {
        case (id, subtree) => other.children.get(id) match {
          case Some(existing) => subtree lessEqual existing
          case None => false
        }

      }
    } else false // permission is larger
  }

  /** Returns the amount of permission for the given access path.
    *
    * @param path The access path.
    * @return The amount of permission for the given access path.
    */
  def get(path: AccessPath): Permission = {
    if (path.isEmpty) permission
    else children.get(path.head) match {
      case Some(child) => child.get(path.tail)
      case None => Permission.none
    }
  }

  /**  Extracts the subtree at the specified path and returns the remainder of
    * the tree as well as the extracted subtree.
    *
    * @param path The path to the subtree to be extracted.
    * @return A tuple containing the remainder of the tree and the extracted
    *         subtree.
    */
  def extract(path: AccessPath): (PermissionTree, PermissionTree) = {
    if (path.isEmpty) {
      // base case: extract the entire subtree
      val remainder = PermissionTree(permission)
      val extracted = PermissionTree(Permission.none, children)
      (remainder, extracted)
    } else {
      // recursively extract subtree from child corresponding to head of path
      val id = path.head
      children.get(id) match {
        case Some(child) => {
          val (updated, extracted) = child.extract(path.tail)
          val remainder = PermissionTree(permission, children + (id -> updated))
          (remainder, extracted)
        }
        case None => (this, PermissionTree()) // there is nothing to extract
      }
    }
  }

  /** Implants the specified permission tree at the specified path. If there is
    * already a non-empty subtree at that path the least upper bound is
    * computed.
    *
    * @param path  The tree to be implanted.
    * @param other The path to the place where the permission tree is to be
    *              implanted.
    * @return This permission tree with the other permission tree implanted.
    */
  def implant(path: AccessPath, other: PermissionTree): PermissionTree = {
    if (path.isEmpty) {
      // base case: implant other at root
      this lub other
    } else {
      // recursively implant other into subtree corresponding to head of path
      val id = path.head
      val updated = children.get(id) match {
        case Some(child) => child.implant(path.tail, other)
        case None => {
          // the path does not exist in the tree,
          // thus, we create it and implant te tree there
          path.tail.foldRight(other) {
            case (id, subtree) => PermissionTree(children = Map(id -> subtree))
          }
        }
      }
      PermissionTree(permission, children + (id -> updated))
    }
  }

  /** Applies the specified function to all permissions stored in the tree. The
    * function takes as arguments the current access path and the permission to
    * be modified. At the root of the tree the path is assumed to be the
    * the variable the tree corresponds to.
    *
    * @param path The current access path.
    * @param f    The function to apply to all permissions in the tree.
    */
  def map(path: AccessPath, f: (AccessPath, PermissionTree) => Permission): PermissionTree = {
    val newPermission = f(path, this)
    val newChildren = children.map {
      case (id, tree) => (id, tree.map(path :+ id, f))
    }
    PermissionTree(newPermission, newChildren)
  }

  def fold[R](z: R)(path: AccessPath, f: (R, (AccessPath, PermissionTree)) => R): R =
    children.foldLeft(f(z, (path, this))) { case (res, (id, child)) =>
      child.fold(res)(path :+ id, f)
    }
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
  * @tparam A The type of the alias analysis state.
  * @author Jerome Dohrau
  */
trait PermissionAnalysisState[T <: PermissionAnalysisState[T, A], A <: AliasAnalysisState[A]]
  extends SimpleState[T] with PreviousResult[A, T] with SilverSpecification
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging
{
  this: T =>

  type AccessPath = List[Identifier]

  // current program point
  def currentPP: ProgramPoint

  // result of the alias analysis
  def context: Option[TrackingCFGState[A]]

  // result of the previous statement
  def result: ExpressionSet

  // permission trees for all variables
  def permissions: Map[Identifier, PermissionTree]

  def specification: Seq[sil.Exp]

  def arguments: Seq[sil.LocalVarDecl]

  // result of the alias analysis before the current program point
  lazy val preAliases = preStateAtPP(context.get, currentPP)

  // result of the alias analysis after the current program point
  lazy val postAliases = postStateAtPP(context.get, currentPP)

  // the set of fields
  lazy val fields = context.get.entryState().fields

  // the list of access paths
  lazy val paths: List[AccessPath] =
    fold(List.empty[AccessPath]) { case (list, (path, _)) => path :: list }

  private def tuples(f: (AccessPath, PermissionTree) => Permission): List[(AccessPath, Permission)] =
    fold(List.empty[(AccessPath, Permission)]){
      case (list, (path, tree)) => (path, f(path, tree)) :: list
    }.filter{
      case (path, permission) => path.length > 1 && permission.isSome
    }

  override def addPreviousResult(result: TrackingCFGState[A]): T =
    copy(context = Some(result))

  /** Modifies the list of formal arguments using information stored in the
    * current state.
    *
    * @param existing The list of existing formal arguments.
    * @return The modified list of formal arguments
    */
  override def formalArguments(existing: Seq[sil.LocalVarDecl]): Seq[sil.LocalVarDecl] = {
    val readExists = existing.exists { case sil.LocalVarDecl(name, _) => name == "read" }
    if (!readExists)
      existing ++ arguments
    else
      existing
  }

  /** Modifies the list of preconditions using information stored in the current
    * state.
    *
    * @param existing The list of existing preconditions.
    * @return The modified list of preconditions.
    */
  override def precondition(existing: Seq[sil.Exp]): Seq[sil.Exp] = getSpecification(existing)

  /** Modifies the list of invariants using information stored in the current
    * state.
    *
    * @param existing The list of existing invariants.
    * @return The modified list of invariants.
    */
  override def invariant(existing: Seq[sil.Exp]): Seq[sil.Exp] = getSpecification(existing)

  /** Modifies the list of postconditions using information stored in the
    * current state.
    *
    * @param existing The list of existing postconditions.
    * @return The modified list of postconditions.
    */
  override def postcondition(existing: Seq[sil.Exp]): Seq[sil.Exp] = getSpecification(existing)

  override def command(cmd: Command): T = {
    logger.trace(s"command($cmd)")
    cmd match {
      case InhaleCommand(expression) => unlessBottom(expression, Lattice.bigLub(expression.getNonTop.map(inhale)))
      case ExhaleCommand(expression) => unlessBottom(expression, Lattice.bigLub(expression.getNonTop.map(exhale)))
      case PreconditionCommand(condition) =>
        val expression = condition.getSingle.get
        inhale(expression).setPrecondition(expression)
      case PostconditionCommand(condition) =>
        val expression = condition.getSingle.get
        exhale(expression).setPostcondition(expression)
      case InvariantCommand(condition) =>
        val expression = condition.getSingle.get
        inhale(expression).setInvariant(expression).exhale(expression)
      case _ => super.command(cmd)
    }
  }

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale.
    * @return The abstract state after exhaling the permission.
    */
  private def exhale(acc: Expression): T = {
    logger.trace(s"exhale($acc)")
    acc match {
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, _) =>
        // inhale both sides of the conjunction
        exhale(left).exhale(right)
      case PermissionExpression(identifier, numerator, denominator) =>
        // get access path
        val location = path(identifier)
        // get the amount of permission that is exhaled
        val exhaled = permission(numerator, denominator)
        // subtract permission form all paths that may alias
        map { (path, tree) =>
          if (mayBeSame(preAliases, path, location)) {
            if (tree.permission.isSome || tree.isEmpty) tree.permission plus exhaled
            else Permission.read plus exhaled
          }
          else tree.permission
        }.access(location, exhaled)
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
  private def inhale(acc: Expression): T = {
    logger.trace(s"inhale($acc)")
    acc match {
      case BinaryBooleanExpression(left, right, BooleanOperator.&&, _) =>
        inhale(right).inhale(left)
      case PermissionExpression(identifier, numerator, denominator) => {
        // get access path
        val location = path(identifier)
        // get the amount of permission that is inhaled
        val inhaled = permission(numerator, denominator)

        // add permission to all paths that must alias
        map { (path, tree) =>
          if (mustBeSame(postAliases, path, location)) tree.permission minus inhaled
          else tree.permission
        }.read(location.dropRight(1))
      }
      case _ => assume(acc)
    }
  }

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
      case id: VariableIdentifier => {
        val fieldId = VariableIdentifier(field)(typ)
        val newPath = AccessPathIdentifier(List(id, fieldId))
        copy(result = ExpressionSet(newPath))
      }
      case AccessPathIdentifier(path) => {
        val fieldId = VariableIdentifier(field)(typ)
        val newPath = AccessPathIdentifier(path ++ List(fieldId))
        copy(result = ExpressionSet(newPath))
      }
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
    copy(permissions = permissions - variable)
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

          val accessPaths = paths.sortBy(-_.length) // process long paths before short ones
          val assigned =
            if (rightPath.isEmpty) assign(leftPath, rightPath)
            else accessPaths.foldLeft(this) {
              case (res, path) =>
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
    val constant = new Constant(value, typ, pp)
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

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): T = {
    logger.trace("bottom")
    copy(result = result.bottom(),
      permissions = Map.empty,
      isBottom = true,
      isTop = false)
  }

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: T): T = {
    logger.trace(s"widening(${this.toString}, ${other.toString})")
    // check whether new state is top or bottom
    val newBottom = isBottom && other.isBottom
    val newTop = isTop || other.isTop
    // compute variable-wise widening of permission trees. that is, compute
    // widening for all trees that are in this.permissions and other.permissions
    // and also include trees that are either in this.permissions or
    // other.permissions (but not both)
    val newPermissions =
      if (newBottom || newTop) Map.empty[Identifier, PermissionTree]
      else if (isBottom) other.permissions
      else if (other.isBottom) permissions
      else permissions.foldLeft(other.permissions) {
        case (map, (id, tree)) => map.get(id) match {
          case Some(existing) => map + (id -> (tree widening existing))
          case None => map + (id -> tree)
        }
      }
    // propagate specifications and arguments
    val newSpecification = if (other.specification.nonEmpty) other.specification else specification
    val newArguments = if (other.arguments.nonEmpty) other.arguments else arguments
    // create new state
    copy(
      isBottom = newBottom,
      isTop = newTop,
      permissions = newPermissions,
      specification = newSpecification.distinct,
      arguments = newArguments.distinct)
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: T): Boolean = {
    logger.trace(s"lessEqual(${this.toString}, ${other.toString})")
    // handle cases involving bottom and top
    if (isBottom || other.isTop) true
    else if (other.isBottom || isTop) false
    // compute whether this needs less permissions than other
    else permissions.forall {
      case (id, tree) => other.permissions.get(id) match {
        case Some(existing) => tree lessEqual existing
        case None => tree lessEqual PermissionTree()
      }
    }
  }

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): T = {
    logger.trace("top")
    copy(result = result.top(),
      permissions = Map.empty,
      isBottom = false,
      isTop = true)
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: T): T = {
    logger.trace(s"lub(${this.toString}, ${other.toString})")
    // check whether new state is top or bottom
    val newBottom = isBottom && other.isBottom
    val newTop = isTop || other.isTop
    // compute variable-wise lub of permission trees. that is, compute lub for
    // all trees that are in this.permissions and other.permissions and also
    // include trees that are either in this.permissions or other.permissions
    // (but not both)
    val newPermissions =
      if (newBottom || newTop) Map.empty[Identifier, PermissionTree]
      else if (isBottom) other.permissions
      else if (other.isBottom) permissions
      else permissions.foldLeft(other.permissions) {
        case (map, (id, tree)) => map.get(id) match {
          case Some(existing) => map + (id -> (tree lub existing))
          case None => map + (id -> tree)
        }
      }
    // propagate specifications and arguments
    val newSpecification = if (specification.nonEmpty) specification else other.specification
    val newArguments = if (arguments.nonEmpty) arguments else other.arguments
    // create new state
    copy(
      isBottom = newBottom,
      isTop = newTop,
      permissions = newPermissions,
      specification = newSpecification.distinct,
      arguments = newArguments.distinct)
  }

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): T = {
    logger.trace("factory")
    this
  }

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or
    *         equal to the two arguments, and greater than or equal to any other
    *         lower bound of the two arguments.
    */
  override def glb(other: T): T = {
    logger.trace("glb")
    // check whether new state is bottom or top
    val newBottom = isBottom || other.isBottom
    val newTop = isTop && other.isTop
    // compute variable-vise glb of permission trees. that is, compute glb for
    // all trees that are in this.permissions and other.permissions
    val newPermissions =
      if (newBottom || newTop) Map.empty[Identifier, PermissionTree]
      else if (isTop) other.permissions
      else if (other.isTop) permissions
      else permissions.foldLeft(Map.empty[Identifier, PermissionTree]) {
        case (map, (id, tree)) => other.permissions.get(id) match {
          case Some(existing) => map + (id -> (tree glb existing))
          case None => map
        }
      }
    // propagate specifications and arguments
    val newSpecification = if (specification.nonEmpty) specification else other.specification
    val newArguments = if (arguments.nonEmpty) arguments else other.arguments
    // create new state
    copy(
      isBottom = newBottom,
      isTop = newTop,
      permissions = newPermissions,
      specification = newSpecification,
      arguments = newArguments)
  }

  /* ------------------------------------------------------------------------- *
   * HELPER FUNCTIONS FOR INFERENCE
   */

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
  private def read(expression: Expression): T =
    expression.ids.getNonTop.foldLeft(this) {
      case (result, identifier) => identifier match {
        case AccessPathIdentifier(path) => result.read(path) // read permission for path
        case _: VariableIdentifier => result // no permission needed
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
    access(path, Permission.write)

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
      val (variable :: first :: rest) = path
      val want = permission minus collect(path)
      val subtree = rest.foldRight(PermissionTree(want)) {
        case (field, subtree) => PermissionTree(Permission.none, Map(field -> subtree))
      }
      val tree = PermissionTree(children = Map(first -> subtree))

      // add new permission tree to permissions
      val updated = permissions.get(variable) match {
        case Some(existing) => existing lub tree
        case None => tree
      }
      copy(permissions = permissions + (variable -> updated))
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
    else fold(Permission.none) {
      case (permission, (currPath, currTree)) =>
        if (path != currPath && mustBeSame(preAliases, path, currPath)) permission plus currTree.permission
        else permission
    }

  private def assign(left: AccessPath, right: AccessPath): T = {
    if (left.isEmpty || right.isEmpty) this
    else {
      // split lhs and rhs into pairs of receivers and fields
      val (rcvL :: fldL) = left
      val (rcvR :: fldR) = right

      // get permission trees for lhs
      val treeL = permissions.get(rcvL)

      if (treeL.isEmpty) this // there are no access paths to modify
      else if (rcvR.isInstanceOf[NewObject]) {
        // extract permission that are "transferred" to new object
        // TODO: report that we need these permissions for the new object?
        val (newL, _) = treeL.get.extract(fldL)
        // update permissions
        copy(permissions = permissions + (rcvL -> newL))
      } else if (rcvL == rcvR) {
        // handle case where rcvL == rcvR
        val (temp, extracted) = treeL.get.extract(fldL)
        val newL = temp.implant(fldR, extracted)
        copy(permissions = permissions + (rcvL -> newL))
      } else {
        // handle case where rcvL != rcvR
        // for instance access path a.f.f becomes b.g.f if we assign a.f := b.g
        val treeR = permissions.get(rcvR)
        val (newL, extracted) = treeL.get.extract(fldL)
        val newR = treeR.getOrElse(PermissionTree()).implant(fldR, extracted)
        copy(permissions = permissions +(rcvL -> newL, rcvR -> newR))
      }
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
    else aliases.receiversMayAlias(first, second) && first.last == second.last

  /** Returns true if the two given access paths must refer to the same field on
    * the same receiver object.
    *
    * @param aliases The alias information.
    * @param first   The first access path.
    * @param second  The second access apth.
    * @return True if the two given access apths may refer to the same field on
    *         the same receiver object.
    */
  private def mustBeSame(aliases: A, first: AccessPath, second: AccessPath): Boolean =
    if (first.length < 2 || second.length < 2) false
    else if (first == second) true
    else aliases.receiversMustAlias(first, second) && first.last == second.last

  /* ------------------------------------------------------------------------- *
   * HELPER FUNCTIONS FOR SPECIFICATION
   */

  private def length(expression: sil.Exp): Int = expression match {
    case sil.FieldAccessPredicate(location, _) => length(location)
    case sil.FieldAccess(receiver, _) => length(receiver) + 1
    case _ => 1
  }

  private def getSpecification(existing: Seq[sil.Exp]): Seq[sil.Exp] = {
    val (permissions, unknowns) = extractPermissions(existing)
    val newPermissions = (specification ++ permissions).sortBy(length)
    newPermissions ++ unknowns
  }

  /** Extracts all permissions (field access predicates) from the given sequence
    * of expressions and returns two sequences, where the first one contains all
    * permissions and the second one contains all other expressions.
    *
    * @param expressions The sequence of expressions to process.
    * @return A sequence containing all permissions and a sequence containing
    *         all other expressions.
    */
  private def extractPermissions(expressions: Seq[sil.Exp]): (Seq[sil.Exp], Seq[sil.Exp]) =
    expressions.foldLeft((Seq.empty[sil.Exp], Seq.empty[sil.Exp])) {
      case ((p, u), curr) =>
        val (currP, currU) = extractPermissions(curr)
        (p ++ currP, u ++ currU)
    }

  /** Extracts all permissions (field access predicates) from the given
    * expressions and returns two sequences, where the first one contains all
    * permissions and the second one contains all other (sub) expressions.
    *
    * @param expression The expression to process.
    * @return A sequence containing all permissions and a sequence containing
    *         all other (sub) expressions.
    */
  private def extractPermissions(expression: sil.Exp): (Seq[sil.Exp], Seq[sil.Exp]) =
    expression match {
      case sil.And(left, right) => {
        val (leftP, leftU) = extractPermissions(left)
        val (rightP, rightU) = extractPermissions(right)
        (leftP ++ rightP, leftU ++ rightU)
      }
      case permission: sil.FieldAccessPredicate => (Seq(permission), Seq.empty)
      case unknown => (Seq.empty, Seq(unknown))

    }

  /** Returns a silver field access corresponding to the given access path.
    *
    * @param path The access path.
    * @return A silver field access.
    */
  private def fieldAccess(path: AccessPath): sil.FieldAccess = {
    val receiver =
      if (path.length == 2) sil.LocalVar(path.head.getName)(sil.Ref)
      else fieldAccess(path.init)
    val name = path.last.getName
    val typ = fields.find(_._2 == name).get match {
      case (t, _) if t.isObject => sil.Ref
      case (t, _) if t.isNumericalType => sil.Int
      case (t, _) if t.isBooleanType => sil.Bool
    }
    sil.FieldAccess(receiver, sil.Field(name, typ)())()
  }

  /** Returns a silver field access predicate corresponding to the given access
    * path and permission.
    *
    * @param path       The access path.
    * @param permission The permission.
    * @return A silver field access predicate.
    */
  private def fieldAccessPredicate(path: AccessPath, permission: Permission): sil.Exp = {
    val location = fieldAccess(path)
    permission match {
      case Permission.Top =>
        sil.FalseLit()()
      case Fractional(numerator, denominator, read) =>
        val amount = if (read) {
          val variable = sil.LocalVar("read")(sil.Perm)
          if (numerator == 0) variable
          else if (numerator == denominator) sil.PermAdd(sil.FullPerm()(), variable)()
          else sil.PermAdd(sil.FractionalPerm(sil.IntLit(numerator)(), sil.IntLit(denominator)())(), variable)()
        } else {
          if (numerator == 0) sil.NoPerm()()
          else if (numerator == denominator) sil.FullPerm()()
          else sil.FractionalPerm(sil.IntLit(numerator)(), sil.IntLit(denominator)())()
        }
        sil.FieldAccessPredicate(location, amount)()
    }
  }

  /** Returns the set of access paths (as strings) that are framed by the given
    * expression.
    *
    * @param expression The expression.
    * @return The set of access paths (as strings).
    */
  private def framed(expression: Expression): Set[String] = expression match {
    case PermissionExpression(id, n, _) => n match {
      case Constant(value, _, _) if value.toInt > 0 => Set(id.toString)
      case _ => Set.empty
    }
    case BinaryBooleanExpression(left, right, BooleanOperator.&&, _) => framed(left) ++ framed(right)
    case _ => Set.empty
  }

  private def setPrecondition(existing: Expression): T = {
    val set = framed(existing)
    val tuples = this.tuples { case (path, tree) =>
      if (tree.permission.isNone && tree.nonEmpty && !set.contains(path.mkString("."))) Permission.read
      else tree.permission
    }
    setSpecification(tuples)
  }

  private def setPostcondition(existing: Expression): T = {
    val tuples = this.tuples{ case (_, tree) =>
      if (tree.permission.isNone && tree.nonEmpty) Permission.read
      else Permission.none
    }
    setSpecification(tuples)
  }

  private def setInvariant(existing: Expression): T =
    setPrecondition(existing)

  private def setSpecification(tuples: List[(AccessPath, Permission)]): T = {
    val reading = tuples.exists {
      case (_, Fractional(_, _, true)) => true
      case _ => false
    }

    val prefix = if (reading) {
      val read = sil.LocalVar("read")(sil.Perm)
      Seq(sil.PermGtCmp(read, sil.NoPerm()())())
    } else Seq.empty
    val specification = prefix ++ tuples
      //.filter { case (path, permission) => permission.isSome }
      .map { case (path, permission) => fieldAccessPredicate(path, permission) }
    val arguments = if (reading) Seq(sil.LocalVarDecl("read", sil.Perm)()) else Seq.empty
    copy(specification = specification, arguments = arguments)
  }

  /* ------------------------------------------------------------------------- *
   * GENERAL HELPER FUNCTIONS
   */

  /** Applies the specified function to all permissions.
    *
    * @param f The function to be applied to all permissions.
    */
  def map(f: (AccessPath, PermissionTree) => Permission): T = {
    val newPermissions = permissions.map {
      case (id, tree) => (id, tree.map(List(id), f))
    }
    copy(permissions = newPermissions)
  }

  def fold[R](z: R)(f: (R, (AccessPath, PermissionTree)) => R): R =
    permissions.foldLeft(z) { case (res, (id, tree)) =>
      tree.fold(res)(List(id), f)
    }

  def copy(currentPP: ProgramPoint = currentPP,
           context: Option[TrackingCFGState[A]] = context,
           result: ExpressionSet = result,
           permissions: Map[Identifier, PermissionTree] = permissions,
           specification: Seq[sil.Exp] = specification,
           arguments: Seq[sil.LocalVarDecl] = arguments,
           isBottom: Boolean = isBottom,
           isTop: Boolean = isTop): T

  override def toString: String = s"PermissionAnalysisState(" +
    s"\n\tresult: $result" +
    s"\n\tpermissions: ${
      val strings = tuples{ case (_, tree) => tree.permission }
        .filter(_._2.isSome)
        .map { case (path, permission) =>
          path.map(_.toString).reduce(_ + "." + _) + " " + permission
        }
      if (strings.isEmpty) "none"
      else strings.reduce(_ + ", " + _)
    }" +
    s"\n\tspec: $specification" +
    s"\n\targs: $arguments" +
    s"\n\tisBottom: $isBottom" +
    s"\n\tisTop: $isTop" +
    s"\n)"
}

object PermissionAnalysisState
{
  case class Default(currentPP: ProgramPoint = DummyProgramPoint,
                     context: Option[TrackingCFGState[AliasAnalysisState.Default]] = None,
                     result: ExpressionSet = ExpressionSet(),
                     permissions: Map[Identifier, PermissionTree] = Map.empty,
                     specification: Seq[sil.Exp] = Seq.empty,
                     arguments: Seq[sil.LocalVarDecl] = Seq.empty,
                     isBottom: Boolean = false,
                     isTop: Boolean = false)
    extends PermissionAnalysisState[Default, AliasAnalysisState.Default]
  {
    override def copy(currentPP: ProgramPoint,
                      context: Option[TrackingCFGState[AliasAnalysisState.Default]],
                      result: ExpressionSet,
                      permissions: Map[Identifier, PermissionTree],
                      specification: Seq[sil.Exp],
                      arguments: Seq[sil.LocalVarDecl],
                      isBottom: Boolean,
                      isTop: Boolean): Default =
      Default(currentPP, context, result, permissions, specification, arguments, isBottom, isTop)
  }
}

object PermissionAnalysisEntryState
  extends BackwardEntryStateBuilder[PermissionAnalysisState.Default]
{
  override def topState: PermissionAnalysisState.Default = PermissionAnalysisState.Default()
}

trait DebugPermissionAnalysisRunner[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[T, A]]
  extends SilverAnalysisRunner[T]
{
  override def main(args: Array[String]) {
    val results = run(new File(args(0)).toPath)

    println("\n*******************\n* Analysis Result *\n*******************\n")
    // map of method names to control flow graphs
    val methodNameToCfgState = results.map(result => result.method.name.toString -> result.cfgState).toMap
    for ((m, g) <- methodNameToCfgState) {
      println("******************* " + m + "\n")

      println(g.exitState()) // printing the entry state of the control-flow graph

      val blocks: List[List[Statement]] = g.cfg.nodes // blocks withing the control-flow graph
      // withing each block...
      var i = 0
      for (stmts: List[Statement] <- blocks) {
        if (stmts.isEmpty) {
          val states: List[T] = g.blockStates(i).last // post-states of each statement
          for (s <- states) {
            println("\n******************* \n")
            println(s)
          }
        } else {
          // printing the block pre-state
          println("\n+++++++++++++++++++ BLOCK " + i + "+++++++++++++++++++\n")
          println(g.blockStates(i).last.head)
          val states: List[T] = g.blockStates(i).last.drop(1) // post-states of each statement
          // print statements and corresponding post-states
          for ((c: Statement, s) <- stmts zip states) {
            println("\n******************* " + c + "\n")
            println(s)
          }
        }
        i = i + 1
      }

      println("\n******************* \n")
      println(g.entryState()) // printing the exit state of the control-flow graph
    }
  }
}

object DebugPermissionAnalysis
  extends DebugPermissionAnalysisRunner[AliasAnalysisState.Default, PermissionAnalysisState.Default]
{
  override val analysis =
    SimpleForwardBackwardAnalysis[AliasAnalysisState.Default, PermissionAnalysisState.Default](AliasAnalysisEntryState, PermissionAnalysisEntryState)

  override def toString = "Permission Analysis"
}

trait PermissionAnalysisRunner[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[T, A]]
  extends SilverInferenceRunner[T]

object PermissionAnalysis
  extends PermissionAnalysisRunner[AliasAnalysisState.Default, PermissionAnalysisState.Default]
{
  override val analysis =
    SimpleForwardBackwardAnalysis[AliasAnalysisState.Default, PermissionAnalysisState.Default](AliasAnalysisEntryState, PermissionAnalysisEntryState)

  override def toString = "Permission Analysis"
}