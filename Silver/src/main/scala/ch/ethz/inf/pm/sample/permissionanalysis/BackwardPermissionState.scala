package ch.ethz.inf.pm.sample.permissionanalysis

import java.io.File

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.execution.{EntryStateBuilder, SimpleBackwardAnalysis}
import ch.ethz.inf.pm.sample.oorepresentation.silver.SilAnalysisRunner
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Statement, Type}
import com.typesafe.scalalogging.LazyLogging

/**
  * @author Jerome Dohrau, Caterina Urban
  */
trait BackwardPermissionState[T <: BackwardPermissionState[T]]
  extends SimplePermissionState[T]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {
  this: T =>

  val permissions: Map[Identifier, PermissionTree]

  def copy(permissions: Map[Identifier, PermissionTree]): T

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  override def inhale(acc: Expression): T = {
    logger.trace("inhale")
    this
  }

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  override def exhale(acc: Expression): T = {
    logger.trace("exhale")
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
  override def createVariableForArgument(x: VariableIdentifier, typ: Type): T = ???

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
    this
  }

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): T = {
    logger.trace("assume")
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
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): T = {
    logger.trace("createVariable")
    this
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
      case variable: VariableIdentifier => {
        // check whether assigned variable is a ref
        if (variable.typ.isObject) {
          val leftPath = List(variable)
          val rightPath = right match {
            case id: VariableIdentifier => List(id)
            case AccessPathIdentifier(path) => path
            case _ => ???
          }
          assign(leftPath, rightPath)
        }
        else ???
      }
      case _ => throw new IllegalArgumentException("A variable assignment must occur via a VariableIdentifier.")
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
    this
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
    this
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
    this
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
    this
  }

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): T = ??? // ignore

  /** Returns the current expression. */
  override def expr: ExpressionSet = {
    logger.trace("expr")
    ExpressionSet()
  }

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): T = {
    logger.trace("createObject")
    this
  }

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): T = {
    logger.trace("setExpression")
    this
  }

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): T = {
    logger.trace("getVariableValue")
    this
  }

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): T = {
    logger.trace("bottom")
    this
  }

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: T): T = {
    logger.trace("widening")
    this
  }

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: T): Boolean = {
    logger.trace("lessEqual")
    permissions.forall {
      case (identifier, tree) => other.permissions.get(identifier) match {
        case Some(existing) => tree.lessThan(existing)
        case None => false
      }
    }
  }

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): T = {
    logger.trace("top")
    this
  }

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: T): T = {
    logger.trace("lub")
    val lubPermissions = permissions.foldLeft(other.permissions) {
      case (accumulated, (identifier, tree)) => {
        accumulated.get(identifier) match {
          case Some(existing) => accumulated.updated(identifier, tree.lub(existing))
          case None => accumulated.updated(identifier, tree)
        }
      }
    }
    copy(permissions = lubPermissions)
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
    val glbPermissions = permissions.foldLeft(Map.empty[Identifier, PermissionTree]) {
      case (accumulated, (identifier, tree)) => {
        other.permissions.get(identifier) match {
          case Some(existing) => accumulated.updated(identifier, tree.glb(existing))
          case None => accumulated
        }
      }
    }
    copy(permissions = glbPermissions)
  }

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return bottom
    */
  override def isBottom: Boolean = {
    logger.trace("isBottom")
    false
  }

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return bottom
    */
  override def isTop: Boolean = false

  // HELPERS

  private def read(path: List[Identifier]): T =
    access(path, Permission.Read)

  private def write(path: List[Identifier]) =
    access(path, Permission.Write)

  private def access(path: List[Identifier], permission: Permission): T = {
    if (path.length < 2) this
    else {
      val (receiver :: first :: others) = path
      val subtree = others.foldRight(PermissionTree(permission)) {
        case (field, accumulated) => PermissionTree(Permission.Read, Map(field -> accumulated))
      }
      val tree = PermissionTree(children = Map(first -> subtree))
      val updated = permissions.get(receiver) match {
        case Some(existing) => existing.lub(tree)
        case None => tree
      }
      copy(permissions + (receiver -> updated))
    }
  }

  private def assign(left: List[Identifier], right: List[Identifier]): T = {
    val (a :: f) = left
    val (b :: g) = right

    val oldA = permissions.get(a)
    val oldB = permissions.get(b)

    if (oldA.isEmpty) this
    else {
      // TODO: improve if a = b
      // a.f = b.g
      // a.f.f -> b.g.f
      val (newA, extracted) = oldA.get.extract(f)
      val newB = oldB.get.implant(extracted, g)

      copy(permissions +(a -> newA, b -> newB)).read(left).write(right)
    }
  }
}

object BackwardPermissionState {

  case class Default(permissions: Map[Identifier, PermissionTree] = Map.empty)
    extends BackwardPermissionState[Default] {

    override def copy(permissions: Map[Identifier, PermissionTree]): Default =
      Default(permissions)

    override def factory(): BackwardPermissionState.Default = {
      BackwardPermissionState.Default()
    }
  }

}


/**
  * @author Jerome Dohrau
  */
case class PermissionTree(permission: Permission = Permission.None,
                          children: Map[Identifier, PermissionTree] = Map.empty) {
  /**
    * Returns the least upper bound of this and the other permission tree.
    */
  def lub(other: PermissionTree): PermissionTree = {
    val lubPermission = permission.lub(other.permission)
    val lubChildren = children.foldLeft(other.children) {
      case (accumulated, (identifier, child)) => {
        accumulated.get(identifier) match {
          case Some(existing) => accumulated.updated(identifier, child.lub(existing))
          case None => accumulated.updated(identifier, child)
        }
      }
    }
    PermissionTree(lubPermission, lubChildren)
  }

  /**
    * Returns the least upper bound of this and the other permission tree.
    */
  def glb(other: PermissionTree): PermissionTree = {
    val glbPermission = permission.glb(other.permission)
    val glbChildren = children.foldLeft(Map.empty[Identifier, PermissionTree]) {
      case (accumulated, (identifier, child)) => {
        other.children.get(identifier) match {
          case Some(existing) => accumulated.updated(identifier, child.glb(existing))
          case None => accumulated
        }
      }
    }
    PermissionTree(glbPermission, glbChildren)
  }

  def lessThan(other: PermissionTree): Boolean = {
    if (permission.lessThan(other.permission)) {
      children.forall {
        case (identifier, child) => other.children.get(identifier) match {
          case Some(existing) => child.lessThan(existing)
          case None => false
        }
      }
    } else false
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
      val remainder = PermissionTree(permission)
      val extracted = PermissionTree(Permission.None, children)
      (remainder, extracted)
    } else children.get(path.head) match {
      case Some(child) => {
        val (updated, extracted) = child.extract(path.tail)
        val remainder = PermissionTree(permission, children.updated(path.head, updated))
        (remainder, extracted)
      }
      case None => (this, PermissionTree())
    }
  }

  /**
    * Implants the specified tree at the specified path. If there is already a
    * non-empty subtree at that path the least upper bound is computed.
    *
    * @param other the tree to be implanted
    * @param path  the path to the place where the tree is implanted
    * @return the tree with the other tree implanted
    */
  def implant(other: PermissionTree, path: List[Identifier]): PermissionTree = {
    if (path.isEmpty) this.lub(other)
    else {
      val updated = children.get(path.head) match {
        case Some(child) => child.implant(other, path.tail)
        case None => path.tail.foldRight(other) {
          case (identifier, accumulated) => PermissionTree(children = Map(identifier -> accumulated))
        }
      }
      PermissionTree(permission, children.updated(path.head, updated))
    }
  }

}

object Permission {
  def None: Permission = Permission(0.0)

  def Read: Permission = Permission(0.1)

  def Write: Permission = Permission(1.0)
}

/**
  * Placeholder for permissions. This will be replaced by the real thing later.
  *
  * @param value the amount of permission
  */
case class Permission(value: Double) {
  def lub(other: Permission): Permission =
    Permission(math.max(value, other.value))

  def glb(other: Permission): Permission =
    Permission(math.min(value, other.value))

  def lessThan(other: Permission): Boolean =
    value <= other.value
}

/** Backward Permission Inference Runner.
  *
  * @tparam S the backward permission state
  * @author Caterina Urban
  */
trait BackwardPermissionRunner[S <: BackwardPermissionState[S]] extends SilAnalysisRunner[S] {

  override def main(args: Array[String]) {
    val results = run(new File(args(0)).toPath)

    println("\n*******************\n* Analysis Result *\n*******************\n")
    // map of method names to control flow graphs
    val methodNameToCfgState = results.map(result => result.method.name.toString -> result.cfgState).toMap
    for ((m, g) <- methodNameToCfgState) {
      println("******************* " + m + "\n")

      println(g.entryState()) // printing the entry state of the control-flow graph

      val blocks: List[List[Statement]] = g.cfg.nodes // blocks withing the control-flow graph
      // withing each block...
      var i = 0
      for (stmts: List[Statement] <- blocks) {
        if (stmts.isEmpty) {
          val states: List[S] = g.blockStates(i).last // post-states of each statement
          for (s <- states) {
            println("\n******************* \n")
            println(s)
          }
        } else {
          // printing the block pre-state
          println("\n+++++++++++++++++++ BLOCK " + i + "+++++++++++++++++++\n")
          println(g.blockStates(i).last.head)
          val states: List[S] = g.blockStates(i).last.drop(1) // post-states of each statement
          // print statements and corresponding post-states
          for ((c: Statement, s) <- stmts zip states) {
            println("\n******************* " + c + "\n")
            println(s)
          }
        }
        i = i + 1
      }

      println("\n******************* \n")
      println(g.exitState()) // printing the exit state of the control-flow graph
    }
  }

  override def toString = "Backward Permission Inference"
}

/** Backward Permission Inference Entry State Builder.
  *
  * @author Caterina Urban
  */
object BackwardPermissionEntryStateBuilder extends EntryStateBuilder[BackwardPermissionState.Default] {
  override def topState: BackwardPermissionState.Default = BackwardPermissionState.Default()
}

/** Backward Permission Inference Runner.
  *
  * @author Caterina Urban
  */
object BackwardPermissionInferenceRunner extends BackwardPermissionRunner[BackwardPermissionState.Default] {
  override val analysis = SimpleBackwardAnalysis[BackwardPermissionState.Default](BackwardPermissionEntryStateBuilder)

  override def toString = "Backward Permission Inference"
}