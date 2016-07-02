package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverAnalysisRunner, SilverInferenceRunner, SilverSpecification}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Statement, Type}
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

/**
  * Placeholder for permissions.
  *
  * @param amount the amount of permission
  * @author Jerome Dohrau
  */
case class Permission(amount: Double) {
  /**
    * Returns the least upper bound of this permission and the other permission.
    *
    * @param other the other permission
    */
  def lub(other: Permission): Permission =
    Permission(math.max(amount, other.amount))

  /**
    * Returns the greatest lower bound of this permission and the other permission.
    *
    * @param other the other permission
    */
  def glb(other: Permission): Permission =
    Permission(math.min(amount, other.amount))

  /**
    * Returns the sum of this permission and the other permission.
    *
    * @param other the other permission
    */
  def plus(other: Permission): Permission =
    Permission(amount + other.amount)

  /**
    * Returns the difference between this permission and the other permission.
    *
    * @param other the other permission
    */
  def minus(other: Permission): Permission =
    Permission(amount - other.amount)

  /**
    * Returns whether this permission is smaller or equal than the other permission.
    *
    * @param other the other permission
    */
  def lessEqual(other: Permission): Boolean =
    amount < other.amount

  override def toString: String =
    if (amount == 0.0) "none"
    else if (amount == 0.1) "read"
    else if (amount == 1.0) "write"
    else s"$amount"
}

object Permission {
  /**
    * Placeholder for no permission
    */
  def none: Permission = Permission(0.0)

  /**
    * Placeholder for read permission
    */
  def read: Permission = Permission(0.1)

  /**
    * Placeholder for write permission
    */
  def write: Permission = Permission(1.0)
}

/**
  * @param permission amount of permission for the root of the tree
  * @param children   maps fields (identifiers) to subtrees
  */
case class PermissionTree(permission: Permission = Permission.none,
                          children: Map[Identifier, PermissionTree] = Map.empty) {

  type AccessPath = List[Identifier]

  /**
    * Returns the least upper bound of this permission tree and the other permission tree.
    *
    * @param other the other permission tree
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

  /**
    * Returns the greatest lower bound of this permission tree and the other permission tree.
    *
    * @param other the other permission tree
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

  /**
    * Returns whether the amount of permission of this permission tree is less
    * than or equal to the amount of permission  of the other tree.
    *
    * @param other the other permission
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

  /**
    * Extracts the subtree at the specified path and returns the remainder of
    * the tree as well as the extracted subtree.
    *
    * @param path the path to the subtree to be extracted
    * @return a tuple containing the remainder of the tree and the extracted
    *         subtree
    */
  def extract(path: List[Identifier]): (PermissionTree, PermissionTree) = {
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

  /**
    * Implants the specified permission tree at the specified path. If there is
    * already a non-empty subtree at that path the least upper bound is
    * computed.
    *
    * @param path  the tree to be implanted
    * @param other the path to the place where the permission tree is to be implanted
    * @return this permission tree with the other permission tree implanted
    */
  def implant(path: List[Identifier], other: PermissionTree): PermissionTree = {
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

  /**
    * Applies the specified function to all permissions stored in the tree. The
    * function takes as arguments the current access path and the permission to
    * be modified. At the root of the tree the path is assumed to be the
    * the variable the tree corresponds to
    *
    * @param path the current access path
    * @param f    the function to apply to all permissions in the tree
    */
  def map(path: AccessPath, f: (AccessPath, Permission) => Permission): PermissionTree = {
    val newPermission = f(path, permission)
    val newChildren = children.map {
      case (id, tree) => (id, tree.map(path :+ id, f))
    }
    PermissionTree(newPermission, newChildren)
  }

  /**
    * Returns a list of all access paths stored in the tree. The access paths
    * are extended with the specified identifier as the receiver.
    *
    * @param identifier the identifier representing the receiver
    */
  def paths(identifier: Identifier): List[AccessPath] =
    paths.map(identifier :: _)

  /**
    * Returns a list of all access paths stored in the tree. The access paths do
    * not include the receiver.
    */
  def paths: List[AccessPath] =
    List(Nil) ++ children.flatMap {
      case (identifier, child) => child.paths.map(identifier :: _)
    }

  /**
    * Returns a list of all permissions stored in the tree. The permissions are
    * represented as tuples of access paths and an amount of permission. The
    * access paths are extended with the specified identifier as the receiver.
    *
    * @param identifier the identifier representing the receiver
    */
  def tuples(identifier: Identifier): List[(AccessPath, Permission)] =
    tuples.map { case (fields, permission) => (identifier :: fields, permission) }

  /**
    * Returns a list of all permissions stored in the tree. The permissions are
    * represented as tuples of access paths and an amount of permission. The
    * access paths do not include the receiver.
    */
  def tuples: List[(AccessPath, Permission)] =
    List((Nil, permission)) ++ children.flatMap {
      case (identifier, child) => child.tuples.map {
        case (path, permission) => (identifier :: path, permission)
      }
    }

  override def toString: String =
    tuples.map { case (path, permission) => path.map(_.toString).reduce(_ + "." + _) + " " + permission }
      .reduce(_ + ", " + _)
}

/**
  * Used to represent new objects in access paths.
  *
  * @param typ the type of the object
  * @param pp  the program point associated with the object
  */
case class NewObject(typ: Type, pp: ProgramPoint = DummyProgramPoint) extends Identifier {
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
  * @tparam T type of the permission analysis state
  * @tparam A type of the alias analysis state
  * @author Jerome Dohrau
  */
trait PermissionAnalysisState[T <: PermissionAnalysisState[T, A], A <: AliasAnalysisState[A]]
  extends SimplePermissionState[T] with PreviousResult[A, T] with SilverSpecification
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {
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

  override def addPreviousResult(result: TrackingCFGState[A]): T = {
    copy(context = Some(result))
  }

  /** Generates a Silver precondition from the current state
    *
    * @return a sequence of sil.Exp
    */
  override def precondition(): Seq[sil.Exp] = Seq[sil.Exp]()

  /** Generates a Silver invariant from the current state
    *
    * @return a sequence of sil.Exp
    */
  override def invariant(): Seq[sil.Exp] = Seq[sil.Exp]()

  /** Generates a Silver postcondition from the current state
    *
    * @return a sequence of sil.Exp
    */
  override def postcondition(): Seq[sil.Exp] = Seq[sil.Exp]()

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  override def exhale(acc: Expression): T = acc match {
    case PermissionExpression(identifier, numerator, denominator) =>
      // get access path
      val access = path(identifier)
      // get the amount of permission that is exhaled
      val exhaled = permission(numerator, denominator)
      // get alias analysis state
      val aliases = preStateBeforePP(context.get, currentPP)

      // subtract permission form all paths that may alias
      map { (path, permission) =>
        if (aliases.receiversMayAlias(path, access)) permission minus exhaled
        else permission
      }
    case _ =>
      // assert
      val asserted = setExpression(ExpressionSet(acc))
      assert(asserted.testFalse() lessEqual bottom())
      assume(acc)
  }

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  override def inhale(acc: Expression): T = acc match {
    case PermissionExpression(identifier, numerator, denominator) => {
      // get access path
      val access = path(identifier)
      // get the amount of permission that is inhaled
      val inhaled = permission(numerator, denominator)

      // get alias analysis state
      val aliases = preStateBeforePP(context.get, currentPP)

      // add permission to all paths that must alias
      map { (path, permission) =>
        if (path == access || aliases.receiversMustAlias(path, access)) permission plus inhaled
        else permission
      }
    }
    case _ => assume(acc)
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the argument
    * @param typ The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): T = {
    logger.trace("createVariableForArgument")
    permissions.get(x) match {
      case Some(_) => this
      case None => copy(permissions = permissions + (x -> PermissionTree()))
    }
  }

  /** Removes a variable.
    *
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be removed
    * @return The abstract state obtained after removing the variable
    */
  override def removeVariable(varExpr: VariableIdentifier): T = {
    logger.trace("removeVariable")
    copy(permissions = permissions - varExpr)
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
    * @param condition The assumed expression
    * @return The abstract state after assuming that the expression holds
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
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    logger.trace("createVariable")
    permissions.get(x) match {
      case Some(existing) => this
      case None => copy(permissions = permissions + (x -> PermissionTree()))
    }
  }

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param left  The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
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
    * Implementations can assume this state is non-bottom
    *
    * @param varExpr The variable to be forgotten
    * @return The abstract state obtained after forgetting the variable
    */
  override def setVariableToTop(varExpr: Expression): T = ???

  /** Assigns an expression to a field of an object.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param obj   the object whose field is assigned
    * @param field the assigned field
    * @param right the assigned expression
    * @return the abstract state after the assignment
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

          // get alias analysis state
          val aliases = preStateBeforePP(context.get, currentPP)

          val accessPaths = paths.sortBy(-_.length) // process long paths before short ones
          val assigned = accessPaths.foldLeft(this) {
              case (res, path) =>
                if (path == leftPath)
                  res.assign(leftPath, rightPath)
                else if (path.last == leftPath.last && aliases.receiversMayAlias(path, leftPath))
                  if (aliases.receiversMustAlias(path, leftPath)) res.assign(path, rightPath)
                  else res lub res.assign(path, rightPath)
                else
                  res
            }
          return assigned.write(leftPath).read(rightPath)
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
    * @param x     The assigned argument
    * @param right The expression to be assigned
    * @return The abstract state after the assignment
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
    * @param t The thrown exception
    * @return The abstract state after the thrown
    */
  override def throws(t: ExpressionSet): T = ??? // ignore

  /** Removes all variables satisfying filter. */
  override def pruneVariables(filter: (VariableIdentifier) => Boolean): T = ??? // ignore

  /** Evaluates a numerical constant.
    *
    * @param value The string representing the numerical constant
    * @param typ   The type of the numerical constant
    * @param pp    The program point that contains the constant
    * @return The abstract state after the evaluation of the constant, that is, the
    *         state that contains an expression representing this constant
    */
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = {
    logger.trace("evalConstant")
    val constant = new Constant(value, typ, pp)
    copy(result = ExpressionSet(constant))
  }

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): T = {
    logger.trace("before")
    copy(currentPP = pp)
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): T = ??? // ignore

  /** Returns the current expression. */
  override def expr: ExpressionSet = {
    logger.trace("expr")
    result
  }

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): T = {
    logger.trace("createObject")
    val obj = NewObject(typ, pp)
    copy(result = ExpressionSet(obj))
  }

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): T = {
    logger.trace("setExpression")
    copy(result = expr)
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access
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
    logger.trace("widening")
    this // TODO: implement me
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: T): Boolean = {
    logger.trace("lessEqual")
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
    logger.trace("lub")
    // compute variable-wise lub of permission trees. that is, compute lub for
    // all trees that are in this.permissions and other.permissions and also
    // include trees that are either in this.permissions or other.permissions
    // (but not both)
    val newPermissions = permissions.foldLeft(other.permissions) {
      case (map, (id, tree)) => map.get(id) match {
        case Some(existing) => map + (id -> (tree lub existing))
        case None => map + (id -> tree)
      }
    }
    val newBottom = isBottom && other.isBottom
    val newTop = isTop || other.isTop
    copy(permissions = newPermissions, isBottom = newBottom, isTop = newTop)
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
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: T): T = {
    logger.trace("glb")
    // compute variable-vise glb of permission trees. that is, compute glb for
    // all trees that are in this.permissions and other.permissions
    val newPermissions = permissions.foldLeft(Map.empty[Identifier, PermissionTree]) {
      case (map, (id, tree)) => other.permissions.get(id) match {
        case Some(existing) => map + (id -> (tree glb existing))
        case None => map
      }
    }
    val newBottom = isBottom || other.isBottom
    val newTop = isTop && other.isTop
    copy(permissions = newPermissions, isBottom = newBottom, isTop = newTop)
  }

  /* ------------------------------------------------------------------------- *
   * HELPER FUNCTIONS
   */

  /**
    * Extracts the path from an expression.
    *
    * @param expression the expression to extract the path from
    */
  private def path(expression: Expression): AccessPath = expression match {
    case _: Constant => Nil
    case id: VariableIdentifier => List(id)
    case obj: NewObject => List(obj)
    case AccessPathIdentifier(path) => path
    case _ => throw new IllegalArgumentException("Expected an access path identifier")
  }

  /**
    * Returns a permission where the amount corresponds to the fraction
    * represented by the specified numerator and denominator.
    *
    * @param numerator   the numerator of the fraction
    * @param denominator the denominator of the fraction
    */
  private def permission(numerator: Expression, denominator: Expression): Permission =
    (numerator, denominator) match {
      case (Constant(nValue, _, _), Constant(dValue, _, _)) =>
        val amount = nValue.toDouble / dValue.toDouble
        Permission(amount)
      case _ => ??? // TODO: support more cases
    }

  /**
    * Adds read permission for all access paths appearing in the specified
    * expression.
    *
    * @param expression the expression to add read permission for
    */
  private def read(expression: Expression): T =
    expression.ids.getNonTop.foldLeft(this) {
      case (result, identifier) => identifier match {
        case AccessPathIdentifier(path) => result.read(path) // read permission for path
        case _: VariableIdentifier => result // no permission needed
      }
    }

  /**
    * Adds read permission for the specified path. If the permission is already
    * there nothing happens.
    *
    * @param path the path to add the permission for
    */
  private def read(path: List[Identifier]): T =
    access(path, Permission.read)

  /**
    * Adds write permission for the specified path. If the permission is already
    * there nothing happens.
    *
    * @param path the path to add the permission for
    */
  private def write(path: List[Identifier]): T =
    access(path, Permission.write)

  /**
    * Adds the specified permission for the specified access path. If the
    * permission is already there nothing happens.
    *
    * @param path       the path to add the permission for
    * @param permission the amount of permissions to add
    */
  private def access(path: List[Identifier], permission: Permission): T = {
    if (path.length < 2) {
      this // no permission needed
    } else {
      // build permission tree for the specified path and permission
      val (receiver :: first :: rest) = path
      val subtree = rest.foldRight(PermissionTree(permission)) {
        case (field, subtree) => PermissionTree(Permission.read, Map(field -> subtree))
      }
      val tree = PermissionTree(children = Map(first -> subtree))
      // add new permission tree to permissions
      val updated = permissions.get(receiver) match {
        case Some(existing) => tree lub existing
        case None => tree
      }
      copy(permissions = permissions + (receiver -> updated))
    }
  }

  private def assign(left: List[Identifier], right: List[Identifier]): T = {
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

  /**
    * Applies the specified function to all permissions.
    *
    * @param f the function to be applied to all permissions
    */
  def map(f: (AccessPath, Permission) => Permission): T = {
    val newPermissions = permissions.map {
      case (id, tree) => (id, tree.map(List(id), f))
    }
    copy(permissions = newPermissions)
  }

  def paths: List[AccessPath] =
    permissions.flatMap { case (identifier, tree) => tree.paths(identifier) }.toList

  def tuples: List[(AccessPath, Permission)] =
    permissions.flatMap { case (identifier, tree) => tree.tuples(identifier) }.toList

  def copy(currentPP: ProgramPoint = currentPP,
           context: Option[TrackingCFGState[A]] = context,
           result: ExpressionSet = result,
           permissions: Map[Identifier, PermissionTree] = permissions,
           isBottom: Boolean = isBottom,
           isTop: Boolean = isTop): T

  override def toString: String = s"PermissionAnalysisState(" +
    s"\n\tresult: $result" +
    s"\n\tpermissions: ${
      val strings = tuples.filter { case (_, permission) =>
        permission.amount > 0
      }.map { case (path, permission) =>
        path.map(_.toString).reduce(_ + "." + _) + " " + permission
      }
      if (strings.isEmpty) "none"
      else strings.reduce(_ + ", " + _)
    }" +
    s"\n\tisBottom: $isBottom" +
    s"\n\tisTop: $isTop" +
    s"\n)"
}

object PermissionAnalysisState {

  case class Default(currentPP: ProgramPoint = DummyProgramPoint,
                     context: Option[TrackingCFGState[AliasAnalysisState.Default]] = None,
                     result: ExpressionSet = ExpressionSet(),
                     permissions: Map[Identifier, PermissionTree] = Map.empty,
                     isBottom: Boolean = false,
                     isTop: Boolean = false)
    extends PermissionAnalysisState[Default, AliasAnalysisState.Default] {

    override def copy(currentPP: ProgramPoint,
                      context: Option[TrackingCFGState[AliasAnalysisState.Default]],
                      result: ExpressionSet,
                      permissions: Map[Identifier, PermissionTree],
                      isBottom: Boolean,
                      isTop: Boolean): Default =
      Default(currentPP, context, result, permissions, isBottom, isTop)
  }

}

object PermissionAnalysisEntryState extends BackwardEntryStateBuilder[PermissionAnalysisState.Default] {
  override def topState: PermissionAnalysisState.Default = PermissionAnalysisState.Default()
}

trait DebugPermissionAnalysisRunner[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[T, A]] extends SilverAnalysisRunner[T] {
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

object DebugPermissionAnalysis extends DebugPermissionAnalysisRunner[AliasAnalysisState.Default, PermissionAnalysisState.Default] {
  override val analysis =
    SimpleForwardBackwardAnalysis[AliasAnalysisState.Default, PermissionAnalysisState.Default](AliasAnalysisEntryState, PermissionAnalysisEntryState)

  override def toString = "Permission Analysis"
}

trait PermissionAnalysisRunner[A <: AliasAnalysisState[A], T <: PermissionAnalysisState[T, A]] extends SilverInferenceRunner[T]

object PermissionAnalysis extends PermissionAnalysisRunner[AliasAnalysisState.Default, PermissionAnalysisState.Default] {
  override val analysis =
    SimpleForwardBackwardAnalysis[AliasAnalysisState.Default, PermissionAnalysisState.Default](AliasAnalysisEntryState, PermissionAnalysisEntryState)

  override def toString = "Permission Analysis"
}