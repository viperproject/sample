package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.execution._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{SilverAnalysisRunner, SilverInferenceRunner, SilverSpecification}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Statement, Type}
import ch.ethz.inf.pm.sample.permissionanalysis.Permission.Inner
import com.typesafe.scalalogging.LazyLogging
import viper.silver.{ast => sil}

/**
  * Represents a permission.
  *
  * @author Jerome Dohrau
  */
trait Permission extends Lattice[Permission] {
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


  override def widening(other: Permission): Permission = ???

  /**
    * Returns the sum of this permission and the other permission.
    *
    * @param other the permission to be added
    */
  def plus(other: Permission): Permission

  /**
    * Returns the difference of this permission and the other permission.
    *
    * @param other the permission to be subtracted
    */
  def minus(other: Permission): Permission

  /**
    * Returns whether the amount of the permission is at most one.
    */
  def isFeasible: Boolean

  /**
    * Returns whether the amount of the permission is greater or equal to one.
    */
  def isWrite: Boolean

  /**
    * Returns whether the amount of the permission is strictly greater than zero.
    */
  def isSome: Boolean

  /**
    * Returns whether the amount of the permission is at most zero.
    */
  def isNone: Boolean
}

object Permission {
  /**
    * Returns no permission.
    */
  def none: Permission = fractional(0, 1)

  /**
    * Returns a read permission.
    */
  def read: Permission = inner(0, 1, true)

  /**
    * Returns a write permission.
    */
  def write: Permission = fractional(1, 1)

  /**
    * Returns a fractional permission.
    *
    * @param numerator   the numerator of the fraction
    * @param denominator the denominator of the fraction
    */
  def fractional(numerator: Int, denominator: Int): Permission =
    inner(numerator, denominator, false)

  /**
    * Returns a permission that is the sum of a fractional permission and a read
    * permission
    *
    * @param numerator   the numerator of the fractional part
    * @param denominator the denominator of the fractional part
    * @param read        indicates whether ther is a read part
    */
  private def inner(numerator: Int, denominator: Int, read: Boolean): Permission = {
    val div = gcd(numerator, denominator)
    Inner(numerator / div, denominator / div, read)
  }

  case object Top extends Permission with Lattice.Top[Permission] {
    override def plus(other: Permission): Permission = top()

    override def minus(other: Permission): Permission = top()

    override def isFeasible: Boolean = false

    override def isWrite: Boolean = true

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

    override def isFeasible: Boolean = true

    override def isWrite: Boolean = false

    override def isSome: Boolean = false

    override def isNone: Boolean = true
  }

  /**
    * A permission that is the sum of a fractional permission and a read
    * permission
    *
    * @param numerator   the numerator of the fractional part
    * @param denominator the denominator of the fractional part
    * @param read        indicates whether ther is a read part
    */
  case class Inner(numerator: Int, denominator: Int, read: Boolean) extends Permission {

    override def isBottom: Boolean = false

    override def isTop: Boolean = false

    override def lessEqual(other: Permission): Boolean = other match {
      case Top => true
      case Bottom => false
      case Inner(oNumerator, oDenominator, oRead) =>
        val x = numerator * oDenominator
        val y = denominator * oNumerator
        x < y || (x == y && (!read || oRead))
    }

    override def plus(other: Permission): Permission = other match {
      case Top => top()
      case Bottom => bottom()
      case Inner(oNumerator, oDenominator, oRead) =>
        val newNumerator = numerator * oDenominator + denominator * oNumerator
        val newDenominator = denominator * oDenominator
        val newRead = read | oRead
        inner(newNumerator, newDenominator, newRead)
    }

    override def minus(other: Permission): Permission = other match {
      case Top => bottom()
      case Bottom => top()
      case Inner(oNumerator, oDenominator, _) =>
        val newNumerator = numerator * oDenominator - denominator * oNumerator
        val newDenominator = denominator * oDenominator
        inner(newNumerator, newDenominator, read)
    }

    override def isFeasible: Boolean =
      amount < 1 || (amount == 1 && !read)

    override def isWrite: Boolean =
      amount >= 1

    override def isSome: Boolean =
      amount > 0 || (amount == 0 && read)

    override def isNone: Boolean =
      amount < 0 || (amount == 0 && !read)

    def amount: Double =
      numerator.toDouble / denominator
  }

  /**
    * Computes the greatest common divisor of the two specified integers.
    *
    * @param a the first integer
    * @param b the second integer
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

  /**
    * Implants the specified permission tree at the specified path. If there is
    * already a non-empty subtree at that path the least upper bound is
    * computed.
    *
    * @param path  the tree to be implanted
    * @param other the path to the place where the permission tree is to be implanted
    * @return this permission tree with the other permission tree implanted
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

  def fold[R](z: R)(path: AccessPath, f: (R, (AccessPath, Permission)) => R): R =
    children.foldLeft(f(z, (path, permission))) { case (res, (id, child)) =>
      child.fold(res)(path :+ id, f)
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
  * @tparam T type of the permission analysis state
  * @tparam A type of the alias analysis state
  * @author Jerome Dohrau
  */
trait PermissionAnalysisState[T <: PermissionAnalysisState[T, A], A <: AliasAnalysisState[A]]
  extends SimpleState[T] with PreviousResult[A, T] with SilverSpecification
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

  // result of the alias analysis before the current program point
  lazy val preAliases = preStateAtPP(context.get, currentPP)

  // result of the alias analysis after the current program point
  lazy val postAliases = postStateAtPP(context.get, currentPP)

  // the set of fields
  lazy val fields = context.get.entryState().fields

  // the list of access paths
  lazy val paths: List[AccessPath] =
    permissions.flatMap { case (identifier, tree) => tree.paths(identifier) }.toList

  // the list of tuples of access paths and their corresponding permissions
  lazy val tuples: List[(AccessPath, Permission)] =
    permissions.flatMap { case (identifier, tree) =>
      tree.tuples(identifier)
    }.toList.filter { case (path, permission) =>
      path.length > 1 && permission.isSome
    }.sortBy { case (path, permission) =>
      path.length
    }

  lazy val reading = tuples.exists {
    case (_, Inner(_, _, true)) => true
    case _ => false
  }

  override def addPreviousResult(result: TrackingCFGState[A]): T = {
    copy(context = Some(result))
  }

  /**
    * Generates a list of additional formal arguments for the method
    *
    * @return a sequence of sil.LocalVarDecl
    */
  override def formalArguments(args: Seq[sil.LocalVarDecl]): Seq[sil.LocalVarDecl] = {
    val existing = args.exists { case sil.LocalVarDecl(name, _) => name == "read" }
    if (reading && !existing) args ++ Seq(sil.LocalVarDecl("read", sil.Perm)())
    else args
  }

  /** Generates a Silver precondition from the current state
    *
    * @return a sequence of sil.Exp
    */
  override def precondition(): Seq[sil.Exp] = {
    val condition = if (reading) {
      val read = sil.LocalVar("read")(sil.Perm)
      Seq(sil.PermGtCmp(read, sil.NoPerm()())())
    } else Seq.empty
    condition ++ tuples.map { case (path, permission) =>
      val obj = sil.LocalVar(path.head.getName)(sil.Ref)
      val loc = path.tail.foldLeft[sil.Exp](obj) { case (rcv, id) =>
        val name = id.getName
        val field = fields.find(_._2 == name).get
        val typ = field match {
          case (t, _) if t.isObject => sil.Ref
          case (t, _) if t.isNumericalType => sil.Int
          case (t, _) if t.isBooleanType => sil.Bool
        }
        sil.FieldAccess(rcv, sil.Field(name, typ)())()
      }.asInstanceOf[sil.FieldAccess]
      permission match {
        case Inner(a, b, read) =>
          val amount = a.toDouble / b
          if (read) {
            val read = sil.LocalVar("read")(sil.Perm)
            val perm = if (amount > 0) {
              val numerator = sil.IntLit(a)()
              val denumerator = sil.IntLit(b)()
              val fractional = sil.FractionalPerm(numerator, denumerator)()
              sil.PermAdd(fractional, read)()
            } else read
            sil.FieldAccessPredicate(loc, perm)()
          } else {
            val perm = if (amount == 1) sil.FullPerm()()
            else {
              val numerator = sil.IntLit(a)()
              val denominator = sil.IntLit(b)()
              sil.FractionalPerm(numerator, denominator)()
            }
            sil.FieldAccessPredicate(loc, perm)()
          }
      }
    }
  }

  /** Generates a Silver invariant from the current state
    *
    * @return a sequence of sil.Exp
    */
  override def invariant(): Seq[sil.Exp] = precondition()

  /** Generates a Silver postcondition from the current state
    *
    * @return a sequence of sil.Exp
    */
  override def postcondition(): Seq[sil.Exp] = Seq[sil.Exp]()

  override def command(cmd: Command): T = cmd match {
    case InhaleCommand(expression) => unlessBottom(expression, Lattice.bigLub(expression.getNonTop.map(inhale)))
    case ExhaleCommand(expression) => unlessBottom(expression, Lattice.bigLub(expression.getNonTop.map(exhale)))
    case PreconditionCommand(condition) => command(InhaleCommand(condition))
    case PostconditionCommand(condition) => command(ExhaleCommand(condition))
    case InvariantCommand(condition) => command(InhaleCommand(condition)).command(ExhaleCommand(condition)) // TODO: improve me
    case _ => super.command(cmd)
  }

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  private def exhale(acc: Expression): T = {
    logger.trace("exhale")
    acc match {
      case PermissionExpression(identifier, numerator, denominator) =>
        // get access path
        val location = path(identifier)
        // get the amount of permission that is exhaled
        val exhaled = permission(numerator, denominator)

        // subtract permission form all paths that may alias
        map { (path, permission) =>
          if (postAliases.pathsMayAlias(path, location)) permission plus exhaled
          else permission
        } lub access(location, exhaled)
      case _ =>
        // we do not assert boolean conditions since the analysis would fail
        // in all cases where we are not able to prove that something holds.
        this
    }
  }

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  private def inhale(acc: Expression): T = {
    logger.trace("inhale")
    acc match {

      case PermissionExpression(identifier, numerator, denominator) => {
        // get access path
        val location = path(identifier)
        // get the amount of permission that is inhaled
        val inhaled = permission(numerator, denominator)

        // add permission to all paths that must alias
        map { (path, permission) =>
          if (path == location || postAliases.pathsMustAlias(path, location)) permission minus inhaled
          else permission
        }
      }
      case _ => assume(acc)
    }
  }

  /** Creates a variable for an argument given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param variable The name of the argument
    * @param typ      The static type of the argument
    * @return The abstract state after the creation of the argument
    */
  override def createVariableForArgument(variable: VariableIdentifier, typ: Type): T = {
    logger.trace("createVariableForArgument")
    this
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
    * @param variable The name of the variable
    * @param typ      The static type of the variable
    * @param pp       The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(variable: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    logger.trace("createVariable")
    copy(permissions = permissions - variable)
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
    lub(other) // TODO: implement me
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
        Permission.fractional(nValue.toInt, dValue.toInt)
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
  private def read(path: AccessPath): T =
    access(path, Permission.read)

  /**
    * Adds write permission for the specified path. If the permission is already
    * there nothing happens.
    *
    * @param path the path to add the permission for
    */
  private def write(path: AccessPath): T =
    access(path, Permission.write)

  /**
    * Adds the specified permission for the specified access path. If the
    * permission is already there nothing happens.
    *
    * @param path       the path to add the permission for
    * @param permission the amount of permissions to add
    */
  private def access(path: AccessPath, permission: Permission): T = {
    val have = collect(path)
    val need = permission minus have
    if (path.length < 2 || need.isNone) {
      // in this case no permission is needed
      this
    } else {
      // build permission tree for the needed permission
      val (variable :: first :: rest) = path
      val subtree = rest.foldRight(PermissionTree(need)) {
        case (field, subtree) => PermissionTree(Permission.read, Map(field -> subtree))
      }
      val tree = PermissionTree(children = Map(first -> subtree))
      // add new permission tree to permissions
      val updated = permissions.get(variable) match {
        case Some(existing) => tree lub existing
        case None => tree
      }
      copy(permissions = permissions + (variable -> updated))
    }
  }

  /**
    * Collects the permission of of all access paths that must alias with the
    * specified access path.
    *
    * @param path
    * @return a lower bound on the amount of permission held for the specified
    *         access path
    */
  private def collect(path: AccessPath): Permission =
    if (path.length < 2) Permission.none
    else {
      val receiver = path.init
      val field = path.last
      fold(Permission.none) { case (permission, (currPath, currPermission)) =>
        val currReceiver = currPath.init
        val currField = currPath.last
        if (path == currPath || (currReceiver.length > 0 && preAliases.pathsMustAlias(receiver, currReceiver) && field == currField)) permission plus currPermission
        else permission
      }
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

  def fold[R](z: R)(f: (R, (AccessPath, Permission)) => R): R =
    permissions.foldLeft(z) { case (res, (id, tree)) =>
      tree.fold(res)(List(id), f)
    }

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
        permission.isSome
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