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
import ch.ethz.inf.pm.sample.permissionanalysis.AliasAnalysisState.SimpleAliasAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisState.SimplePermissionAnalysisState
import ch.ethz.inf.pm.sample.permissionanalysis.PermissionAnalysisTypes._
import ch.ethz.inf.pm.sample.permissionanalysis.util.{Permission, PermissionTree}
import ch.ethz.inf.pm.sample.permissionanalysis.util.Permission.Fractional
import ch.ethz.inf.pm.sample.reporting.Reporter
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

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
  extends SimpleState[T]
    with SilverState[T]
    with SilverSpec
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {
  this: T =>

  // current program point
  def currentPP: ProgramPoint

  // the set of fields
  def fields: Set[(String, Type)]

  // result of the previous statement
  def result: ExpressionSet

  // permission trees for all variables
  def permissions: Map[Identifier, PermissionTree]

  def specification: Either[Seq[sil.Exp], Set[sil.Field]]

  def arguments: Seq[sil.LocalVarDecl]

  // result of the alias analysis before the current program point
  lazy val preAliases = Context.preAliases[A](currentPP)

  // result of the alias analysis after the current program point
  lazy val postAliases = Context.postAliases[A](currentPP)

  // the list of access paths
  lazy val paths: List[AccessPath] =
    fold(List.empty[AccessPath]) { case (list, (path, _)) => path :: list }

  private def tuples(f: (AccessPath, PermissionTree) => Permission): Tuples =
    fold(List.empty[Tuple]) {
      case (list, (path, tree)) => (path, f(path, tree)) :: list
    }.filter {
      case (path, permission) => path.length > 1 && permission.isSome
    }

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
  override def preconditions(existing: Seq[sil.Exp]): Seq[sil.Exp] = getSpecification(existing)

  /** Modifies the list of invariants using information stored in the current
    * state.
    *
    * @param existing The list of existing invariants.
    * @return The modified list of invariants.
    */
  override def invariants(existing: Seq[sil.Exp]): Seq[sil.Exp] = getSpecification(existing)

  /** Modifies the list of postconditions using information stored in the
    * current state.
    *
    * @param existing The list of existing postconditions.
    * @return The modified list of postconditions.
    */
  override def postconditions(existing: Seq[sil.Exp]): Seq[sil.Exp] = getSpecification(existing)

  /**
    * Modifies the list of fields of a new statement using information stored in
    * the current state.
    *
    * @param existing The list of existing fields.
    * @return The modified list of fields.
    */
  override def fields(existing: Seq[sil.Field]): Seq[sil.Field] = {
    val inferred = specification match {
      case Left(_) => Seq.empty[sil.Field]
      case Right(fields) => fields.toSeq
    }

    for (field <- existing intersect inferred) {
      val message = s"There might be insufficient permission for the field ${field.name}"
      Reporter.reportGenericWarning(message, currentPP)
    }

    (existing ++ inferred).distinct
  }

  override def command(cmd: Command): T = {
    logger.trace(s"command($cmd)")
    cmd match {
      case InhaleCommand(expression) => unlessBottom(expression, Lattice.bigLub(expression.toSetOrFail.map(inhale)))
      case ExhaleCommand(expression) => unlessBottom(expression, Lattice.bigLub(expression.toSetOrFail.map(exhale)))
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
        map { (path, tree) =>
          if (mustBeSame(postAliases, path, location)) tree.permission minus inhaled
          else tree.permission
        }.read(location.dropRight(1))
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
    val newSpecification = (specification, other.specification) match {
      case (Left(a), Left(b)) => if (b.nonEmpty) Left(b) else Left(a)
      case (Left(a), _) => Left(a)
      case (_, Left(b)) => Left(b)
      case (Right(a), Right(b)) => Right(a ++ b)
    }
    val newArguments = if (other.arguments.nonEmpty) other.arguments else arguments
    // create new state
    copy(
      isBottom = newBottom,
      isTop = newTop,
      permissions = newPermissions,
      specification = newSpecification, ///.distinct),
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
    val newSpecification = (specification, other.specification) match {
      case (Left(a), Left(b)) => if (a.nonEmpty) Left(a) else Left(b)
      case (Left(a), _) => Left(a)
      case (_, Left(b)) => Left(b)
      case (Right(a), Right(b)) => Right(a ++ b)
    }
    //if (specification.nonEmpty) specification else other.specification
    val newArguments = if (arguments.nonEmpty) arguments else other.arguments
    // create new state
    copy(
      isBottom = newBottom,
      isTop = newTop,
      permissions = newPermissions,
      specification = newSpecification,
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
    val newSpecification = (specification, other.specification) match {
      case (Left(a), Left(b)) => if (a.nonEmpty) Left(a) else Left(b)
      case (Left(a), _) => Left(a)
      case (_, Left(b)) => Left(b)
      case (Right(a), Right(b)) => Right(a & b)
    }
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
        // extract permission that are needed for the new object
        val (newL, extracted) = treeL.get.extract(fldL)
        val specification = extracted.fold(Set.empty[sil.Field])(left, {
          case (res, (path, tree)) => path match {
            case (_ :: Nil) =>
              res
            case (_ :: field :: Nil) =>
              val permission = tree.permission
              if (permission.isNone && tree.isEmpty) res
              else {
                if (permission.isInfeasible) {
                  val message = s"There might be insufficient permission for ${path.mkString(".")}"
                  Reporter.reportGenericWarning(message, currentPP)
                }
                res ++ Set(silverField(field))
              }
            case _ =>
              if (tree.nonEmpty) {
                val message = s"Inferred permission for ${path.mkString(".")}"
                Reporter.reportGenericWarning(message, currentPP)
              }
              res
          }
        })
        // update permissions
        copy(permissions = permissions + (rcvL -> newL), specification = Right(specification))
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
        copy(permissions = permissions + (rcvL -> newL, rcvR -> newR))
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
   * HELPER FUNCTIONS FOR SPECIFICATION
   */

  private def length(expression: sil.Exp): Int = expression match {
    case sil.FieldAccessPredicate(location, _) => length(location)
    case sil.FieldAccess(receiver, _) => length(receiver) + 1
    case _ => 1
  }

  private def getSpecification(existing: Seq[sil.Exp]): Seq[sil.Exp] = {
    val (permissions, unknowns) = extractPermissions(existing)
    val newPermissions = specification match {
      case Left(list) => (list ++ permissions).sortBy(length)
      case Right(_) => permissions.sortBy(length)
    }
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
      case sil.And(left, right) =>
        val (leftP, leftU) = extractPermissions(left)
        val (rightP, rightU) = extractPermissions(right)
        (leftP ++ rightP, leftU ++ rightU)
      case permission: sil.FieldAccessPredicate => (Seq(permission), Seq.empty)
      case unknown => (Seq.empty, Seq(unknown))

    }

  /** Returns a silver field access corresponding to the given access path.
    *
    * @param path The access path.
    * @return A silver field access.
    */
  private def silverFieldAccess(path: AccessPath): sil.FieldAccess = {
    val receiver =
      if (path.length == 2) sil.LocalVar(path.head.getName)(sil.Ref)
      else silverFieldAccess(path.init)
    val name = path.last.getName
    val typ = silverType(name)
    sil.FieldAccess(receiver, sil.Field(name, typ)())()
  }

  /**
    * Returns a silver field corresponding to the field with the given name.
    *
    * @param name The name of the field.
    * @return A silver field.
    */
  private def silverField(name: Identifier): sil.Field = {
    val typ = silverType(name.getName)
    sil.Field(name.getName, typ)()
  }

  /** Returns a silver field access predicate corresponding to the given access
    * path and permission.
    *
    * @param path       The access path.
    * @param permission The permission.
    * @return A silver field access predicate.
    */
  private def silverFieldAccessPredicate(path: AccessPath, permission: Permission): sil.Exp = {
    val location = silverFieldAccess(path)
    permission match {
      case Permission.Top =>
        sil.FalseLit()()
      case Fractional(numerator, denominator, read) =>
        val amount = if (read > 0) {
          val variable = if (read == 1) sil.LocalVar("read")(sil.Perm)
          else sil.PermMul(sil.IntLit(read)(), sil.LocalVar("read")(sil.Perm))()
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

  /**
    * Returns a silver type corresponding to the type of the field with the
    * given name.
    *
    * @param name The name of the field.
    * @return A silver type.
    */
  private def silverType(name: String): sil.Type = {
    fields.find(_._1 == name).get match {
      case (_, t) if t.isObject => sil.Ref
      case (_, t) if t.isNumericalType => sil.Int
      case (_, t) if t.isBooleanType => sil.Bool
    }
  }

  /** Returns the set of access paths (as strings) that are framed by the given
    * expression.
    *
    * @param expression The expression.
    * @return The set of access paths (as strings).
    */
  private def framed(expression: Expression): Set[String] = expression match {
    case FieldAccessPredicate(id, n, _, _) => n match {
      case Constant(value, _, _) if value.toInt > 0 => Set(id.toString)
      case _ => Set.empty
    }
    case BinaryBooleanExpression(left, right, BooleanOperator.&&) => framed(left) ++ framed(right)
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
    val tuples = this.tuples { case (_, tree) =>
      if (tree.permission.isNone && tree.nonEmpty) Permission.read
      else Permission.none
    }
    setSpecification(tuples)
  }

  private def setInvariant(existing: Expression): T =
    setPrecondition(existing)

  private def setSpecification(tuples: Tuples): T = {
    val reading = tuples.exists {
      case (_, Fractional(_, _, read)) => read > 0
      case _ => false
    }

    val prefix = if (reading) {
      val read = sil.LocalVar("read")(sil.Perm)
      Seq(sil.PermGtCmp(read, sil.NoPerm()())())
    } else Seq.empty
    val specification = prefix ++ tuples
      //.filter { case (path, permission) => permission.isSome }
      .map { case (path, permission) => silverFieldAccessPredicate(path, permission) }
    val arguments = if (reading) Seq(sil.LocalVarDecl("read", sil.Perm)()) else Seq.empty
    copy(specification = Left(specification), arguments = arguments)
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
           fields: Set[(String, Type)] = fields,
           result: ExpressionSet = result,
           permissions: Map[Identifier, PermissionTree] = permissions,
           specification: Either[Seq[sil.Exp], Set[sil.Field]] = specification,
           arguments: Seq[sil.LocalVarDecl] = arguments,
           isBottom: Boolean = isBottom,
           isTop: Boolean = isTop): T

  override def toString: String = s"PermissionAnalysisState(" +
    s"\n\tresult: $result" +
    s"\n\tpermissions: ${
      val strings = tuples { case (_, tree) => tree.permission }
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

  override def ids = IdentifierSet.Top

}

object PermissionAnalysisState {

  case class SimplePermissionAnalysisState(currentPP: ProgramPoint = DummyProgramPoint,
                                           fields: Set[(String, Type)] = Set.empty,
                                           result: ExpressionSet = ExpressionSet(),
                                           permissions: Map[Identifier, PermissionTree] = Map.empty,
                                           specification: Either[Seq[sil.Exp], Set[sil.Field]] = Left(Seq.empty),
                                           arguments: Seq[sil.LocalVarDecl] = Seq.empty,
                                           isBottom: Boolean = false,
                                           isTop: Boolean = false)
    extends PermissionAnalysisState[SimpleAliasAnalysisState, SimplePermissionAnalysisState] {
    override def copy(currentPP: ProgramPoint,
                      fields: Set[(String, Type)],
                      result: ExpressionSet,
                      permissions: Map[Identifier, PermissionTree],
                      specification: Either[Seq[sil.Exp], Set[sil.Field]],
                      arguments: Seq[sil.LocalVarDecl],
                      isBottom: Boolean,
                      isTop: Boolean): SimplePermissionAnalysisState =
      SimplePermissionAnalysisState(currentPP, fields, result, permissions, specification, arguments, isBottom, isTop)
  }

}

trait PermissionAnalysisStateBuilder[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[A, T]]
  extends SilverEntryStateBuilder[T] {
  override def build(program: SilverProgramDeclaration, method: SilverMethodDeclaration): T = {
    // retrieve the set of fields declared in the program
    val fields = program.fields
      .map(field => (field.variable.toString, field.typ))
      .toSet

    val initial = top.copy(fields = fields)

    method.initializeArgument(initial)
  }
}

object PermissionAnalysisEntryState
  extends PermissionAnalysisStateBuilder[SimpleAliasAnalysisState, SimplePermissionAnalysisState] {
  override def top: PermissionAnalysisState.SimplePermissionAnalysisState = PermissionAnalysisState.SimplePermissionAnalysisState()
}

case class PermissionAnalysis[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[A, T]]
(aliasAnalysisStateBuilder: AliasAnalysisStateBuilder[A],
 permissionAnalysisStateBuilder: PermissionAnalysisStateBuilder[A, T])
  extends SilverAnalysis[T] {
  override def analyze(program: SilverProgramDeclaration, method: SilverMethodDeclaration): CfgResult[T] = {
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

trait PermissionInferenceRunner[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[A, T]]
  extends SilverInferenceRunner[T]

object PermissionInference
  extends PermissionInferenceRunner[SimpleAliasAnalysisState, SimplePermissionAnalysisState] {
  override val analysis: SilverAnalysis[SimplePermissionAnalysisState] = PermissionAnalysis(AliasAnalysisEntryState, PermissionAnalysisEntryState)
}