package ch.ethz.inf.pm.sample.permissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{ExpressionSet, _}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import com.typesafe.scalalogging.LazyLogging

/**
  * @author Jerome Dohrau, Caterina Urban
  */
trait BackwardPermissionState[T <: BackwardPermissionState[T]]
  extends SimplePermissionState[T]
    with StateWithRefiningAnalysisStubs[T]
    with LazyLogging {
  this: T =>

  /** Inhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to inhale
    * @return The abstract state after inhaling the permission
    */
  override def inhale(acc: Expression): T = ???

  /** Exhales permissions.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param acc The permission to exhale
    * @return The abstract state after exhaling the permission
    */
  override def exhale(acc: Expression): T = ???

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
  override def removeVariable(varExpr: VariableIdentifier): T = ???

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
  override def getFieldValue(obj: Expression, field: String, typ: Type): T = ???

  /** Assumes that a boolean expression holds.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param cond The assumed expression
    * @return The abstract state after assuming that the expression holds
    */
  override def assume(cond: Expression): T = ???

  /** Creates a variable given a `VariableIdentifier`.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x   The name of the variable
    * @param typ The static type of the variable
    * @param pp  The program point that creates the variable
    * @return The abstract state after the creation of the variable
    */
  override def createVariable(x: VariableIdentifier, typ: Type, pp: ProgramPoint): T = ???

  /** Assigns an expression to a variable.
    *
    * Implementations can already assume that this state is non-bottom.
    *
    * @param x     The assigned variable
    * @param right The assigned expression
    * @return The abstract state after the assignment
    */
  override def assignVariable(x: Expression, right: Expression): T = ???

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
  override def assignField(obj: Expression, field: String, right: Expression): T = ???

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
  override def removeExpression(): T = ???

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
  override def evalConstant(value: String, typ: Type, pp: ProgramPoint): T = ???

  /** Signals that we are going to analyze the statement at program point `pp`.
    *
    * This is particularly important to eventually partition a state following the specified directives.
    *
    * @param pp The point of the program that is going to be analyzed
    * @return The abstract state eventually modified
    */
  override def before(pp: ProgramPoint): T = this

  /** Performs abstract garbage collection. */
  override def pruneUnreachableHeap(): T = ??? // ignore

  /** Returns the current expression. */
  override def expr: ExpressionSet = ???

  /** Creates an object
    *
    * @param typ The dynamic type of the created object
    * @param pp  The point of the program that creates the object
    * @return The abstract state after the creation of the object
    */
  override def createObject(typ: Type, pp: ProgramPoint): T = ???

  /** Sets the current expression.
    *
    * @param expr The current expression
    * @return The abstract state after changing the current expression with the given one
    */
  override def setExpression(expr: ExpressionSet): T = ???

  /** Gets the value of a variable.
    *
    * @param id The variable to access
    * @return The abstract state obtained after accessing the variable, that is, the state that contains
    *         as expression the symbolic representation of the value of the given variable
    */
  override def getVariableValue(id: Identifier): T = ???

  /** Returns the bottom value of the lattice.
    *
    * @return The bottom value, that is, a value x that is less than or to any other value
    */
  override def bottom(): T = ???

  /** Computes the widening of two elements.
    *
    * @param other The new value
    * @return The widening of `this` and `other`
    */
  override def widening(other: T): T = ???

  /** Returns true if and only if `this` is less than or equal to `other`.
    *
    * @param other The value to compare
    * @return true if and only if `this` is less than or equal to `other`
    */
  override def lessEqual(other: T): Boolean = ???

  /** Returns the top value of the lattice.
    *
    * @return The top value, that is, a value x that is greater than or equal to any other value
    */
  override def top(): T = ???

  /** Computes the least upper bound of two elements.
    *
    * @param other The other value
    * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
    *         and less than or equal to any other upper bound of the two arguments
    */
  override def lub(other: T): T = ???

  /** Returns a new instance of the lattice.
    *
    * @return A new instance of the current object
    */
  override def factory(): T = ???

  /** Computes the greatest lower bound of two elements.
    *
    * @param other The other value
    * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
    *         and greater than or equal to any other lower bound of the two arguments
    */
  override def glb(other: T): T = ???

  /** Checks whether the given domain element is equivalent to bottom.
    *
    * @return bottom
    */
  override def isBottom: Boolean = ???

  /** Checks whether the given domain element is equivalent to top.
    *
    * @return bottom
    */
  override def isTop: Boolean = ???

  /**
    * @author Jerome Dohrau
    */
  case class Tree(permission: Permission = Permission.None,
                  children: Map[Identifier, Tree] = Map.empty[Identifier, Tree]) {
    /**
      * Returns the least upper bound of this and the other permission tree.
      */
    def lub(other: Tree): Tree = {
      val lubPermission = permission.lub(other.permission)
      val lubChildren = children.foldLeft(other.children) {
        case (accumulated, (identifier, child)) => {
          accumulated.get(identifier) match {
            case Some(existing) => accumulated.updated(identifier, child.lub(existing))
            case None => accumulated.updated(identifier, child)
          }
        }
      }
      Tree(lubPermission, lubChildren)
    }

    /**
      * Returns the least upper bound of this and the other permission tree.
      */
    def glb(other: Tree): Tree = {
      val glbPermission = permission.glb(other.permission)
      val glbChildren = children.foldLeft(Map.empty[Identifier, Tree]) {
        case (accumulated, (identifier, child)) => {
          other.children.get(identifier) match {
            case Some(existing) => accumulated.updated(identifier, child.glb(existing))
            case None => accumulated
          }
        }
      }
      Tree(glbPermission, glbChildren)
    }

    /**
      * Extracts the subtree at the specified path and returns the remainder of
      * the tree as well as the extracted subtree.
      *
      * @param path the path to the subtree to be extracted
      * @return a tuple containing the remainder of the tree and the extracted
      *         subtree
      */
    def extract(path: List[Identifier]): (Tree, Tree) = {
      if (path.isEmpty) {
        val remainder = Tree(permission)
        val extracted = Tree(Permission.None, children)
        (remainder, extracted)
      } else children.get(path.head) match {
        case Some(child) => {
          val (updated, extracted) = child.extract(path.tail)
          val remainder = Tree(permission, children.updated(path.head, updated))
          (remainder, extracted)
        }
        case None => (this, Tree())
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
    def implant(other: Tree, path: List[Identifier]): Tree = {
      if (path.isEmpty) this.lub(other)
      else {
        val updated = children.get(path.head) match {
          case Some(child) => child.implant(other, path.tail)
          case None => path.tail.foldRight(other) {
            // TODO: Depending on the implementation of the transformers this should not happen
            case (identifier, accumulated) => Tree(Permission.None, Map(identifier -> accumulated))
          }
        }
        Tree(permission, children.updated(path.head, updated))
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
  }

}
