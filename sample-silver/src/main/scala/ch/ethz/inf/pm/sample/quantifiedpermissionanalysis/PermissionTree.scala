/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.elimination.MaximumElimination
import ch.ethz.inf.pm.sample.oorepresentation.silver.PermType
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree._
import ch.ethz.inf.pm.sample.util.SampleExpressions

/**
  * Represents a non-negative amount of permissions.
  *
  * @author Jerome Dohrau
  * @author Severin Münger
  */
sealed trait PermissionTree {

  import SampleExpressions._
  import MaximumElimination.eliminate

  def assume(condition: Expression) = Conditional(condition, this, Empty)

  def simplify: PermissionTree = transform {
    case Maximum(left, Empty) => left
    case Maximum(Empty, right) => right
    case Maximum(Conditional(c1, Empty, right), Conditional(c2, left, Empty)) if c1 == c2 =>
      Conditional(c1, left, right)
    case Maximum(Conditional(c1, left, Empty), Conditional(c2, Empty, right)) if c1 == c2 =>
      Conditional(c1, left, right)
    case original@Conditional(condition, left, right) =>
      if (left == right) left
      else condition match {
        case Not(argument) => Conditional(argument, right, left)
        case _ => original
      }
    case tree => tree
  }

  def transform(f: PermissionTree => PermissionTree): PermissionTree = this match {
    case Addition(left, right) => f(Addition(left.transform(f), right.transform(f)))
    case Subtraction(left, right) => f(Subtraction(left.transform(f), right.transform(f)))
    case Maximum(left, right) => f(Maximum(left.transform(f), right.transform(f)))
    case Conditional(condition, left, right) => f(Conditional(condition, left.transform(f), right.transform(f)))
    case tree => f(tree)
  }

  def contains(f: PermissionTree => Boolean): Boolean = this match {
    case Empty => f(this)
    case Initial => f(this)
    case Leaf(_, _) => f(this)
    case Addition(left, right) => f(this) || left.contains(f) || right.contains(f)
    case Subtraction(left, right) => f(this) || left.contains(f) || right.contains(f)
    case Maximum(left, right) => f(this) || left.contains(f) || right.contains(f)
    case Conditional(_, left, right) => f(this) || left.contains(f) || right.contains(f)
  }

  def compose(other: PermissionTree): PermissionTree = transform {
    case Initial => other
    case tree => tree
  }

  def forget(variables: Seq[VariableIdentifier], invariant: Expression): PermissionTree = {
    val rewritten = rewrite.map { case (c, p) => (SampleExpressions.simplify(c), p) }
    rewritten.map { case (c, p) =>
      val body = ConditionalExpression(And(c, invariant), p, No)
      val maximum = BigMax(variables, body)
      val eliminated = eliminate(maximum)
      println(s"original: $maximum")
      println(s"eliminated: $eliminated")
      toTree(eliminated)
    }.reduce(Maximum)
  }

  private def rewrite: Set[(Expression, Expression)] = this match {
    case Empty =>
      Set((True, No))
    case Initial =>
      val placeholder = VariableIdentifier("π")(PermType)
      Set((True, placeholder))
    case Leaf(condition, permission) =>
      Set((condition, permission))
    case Addition(left, right) =>
      val rewrittenLeft = left.rewrite
      val rewrittenRight = right.rewrite
      for ((constraint1, permission1) <- rewrittenLeft;
           (constraint2, permission2) <- rewrittenRight)
        yield (Or(constraint1, constraint2), Plus(permission1, permission2))
    case Maximum(left, right) =>
      val rewrittenLeft = left.rewrite
      val rewrittenRight = right.rewrite
      // TODO: Is this correct?
      /*val set = for ((constraint1, permission1) <- rewrittenLeft;
                     (constraint2, permission2) <- rewrittenRight) yield {
        val constraint = And(constraint1, constraint2)
        val expression1: Expression = And(constraint, GreaterEqual(permission1, permission2))
        val expression2: Expression = And(constraint, Less(permission1, permission2))
        Seq((expression1, permission1), (expression2, permission2))
      }
      set.flatten*/
      rewrittenLeft ++ rewrittenRight
    case Conditional(condition, left, right) =>
      val set1 = for ((constraint, permission) <- left.rewrite) yield {
        val expression: Expression = And(constraint, condition)
        (expression, permission)
      }
      val set2 = for ((constraint, permission) <- right.rewrite) yield {
        val expression: Expression = And(constraint, Not(condition))
        (expression, permission)
      }
      set1 ++ set2
    case _ => ???
  }

  private def toTree(expression: Expression): PermissionTree = expression match {
    case ConditionalExpression(condition, left, right) =>
      // TODO: Left is constant
      // TODO: Right is zero
      Leaf(condition, left)
    case Max(left, right) => Maximum(toTree(left), toTree(right))
    case No => Empty
    case _ => ???
  }

  def toExpression: Expression = this match {
    case Empty => No
    case Leaf(condition, permission) => ConditionalExpression(condition, permission, No)
    case Addition(left, right) => Plus(left.toExpression, right.toExpression)
    case Subtraction(left, right) => Bound(Minus(left.toExpression, right.toExpression))
    case Maximum(left, right) => Max(left.toExpression, right.toExpression)
    case Conditional(condition, left, right) => ConditionalExpression(condition, left.toExpression, right.toExpression)
  }
}

object PermissionTree {

  /**
    * Represents no permissions.
    */
  case object Empty
    extends PermissionTree {

    override def toString: String = "none"
  }

  /**
    * Represents a placeholder to some arbitrary initial permissions.
    */
  case object Initial
    extends PermissionTree {

    override def toString: String = "π"
  }

  /**
    * TODO:  Replace receiver by a condition.
    * TODO: Change type of permission to expression.
    *
    * @param condition
    * @param permission
    */
  case class Leaf(condition: Expression, permission: Expression)
    extends PermissionTree {

    override def toString: String = s"(q == $condition ? $permission : none)"
  }

  case class Addition(left: PermissionTree, right: PermissionTree)
    extends PermissionTree {

    override def toString: String = s"($left + $right)"
  }

  /**
    * Represents max(left-right, 0).
    *
    * @param left
    * @param right
    */
  case class Subtraction(left: PermissionTree, right: PermissionTree)
    extends PermissionTree {

    override def toString: String = s"($left - $right)"
  }

  case class Maximum(left: PermissionTree, right: PermissionTree)
    extends PermissionTree {

    override def toString: String = s"max($left, $right)"
  }

  case class Conditional(condition: Expression, left: PermissionTree, right: PermissionTree)
    extends PermissionTree {

    override def toString: String = s"($condition ? $left : $right)"
  }

}
