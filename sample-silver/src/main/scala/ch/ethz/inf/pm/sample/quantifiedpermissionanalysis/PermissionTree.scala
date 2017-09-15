/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain.{Expression, FunctionCallExpression, NegatedBooleanExpression, VariableIdentifier}
import ch.ethz.inf.pm.sample.oorepresentation.silver.PermType
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Permission.Fractional
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

  def bound(): PermissionTree = Maximum(this, Empty)

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
        case NegatedBooleanExpression(argument) => Conditional(argument, right, left)
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

  def compose(other: PermissionTree): PermissionTree = transform {
    case Initial => other
    case tree => tree
  }

  def forget(variable: VariableIdentifier, invariant: Expression): PermissionTree = {
    val rewritten = rewrite.map { case (c, p) => (SampleExpressions.simplify(c), p) }
    println(rewritten)
    ???
  }

  def rewrite: Set[(Expression, Expression)] = this match {
    case Empty =>
      Set((tt, none))
    case Initial =>
      val placeholder = VariableIdentifier("p0")(PermType)
      Set((tt, placeholder))
    case Leaf(receiver, permission) => receiver match {
      case FunctionCallExpression(name, arguments, _, _) =>
        // generate constraint from arguments
        val c = arguments.map { argument =>
          val typ = argument.typ
          val variable = VariableIdentifier(s"q_$typ")(typ)
          equ(variable, argument)
        }.reduce(and)
        // translate permission
        val p = permission match {
          case Fractional(numerator, denominator) =>
            fractional(integer(numerator), integer(denominator))
        }
        Set((c, p))
      case _ => ???
    }
    case Addition(left, right) =>
      val rewrittenLeft = left.rewrite
      val rewrittenRight = right.rewrite
      for ((constraint1, permission1) <- rewrittenLeft;
           (constraint2, permission2) <- rewrittenRight)
        yield (or(constraint1, constraint2), plus(permission1, permission2))
    case Maximum(left, right) =>
      val rewrittenLeft = left.rewrite
      val rewrittenRight = right.rewrite
      val set = for ((constraint1, permission1) <- rewrittenLeft;
                     (constraint2, permission2) <- rewrittenRight) yield {
        val constraint = and(constraint1, constraint2)
        val expression1: Expression = and(constraint, geq(permission1, permission2))
        val expression2: Expression = and(constraint, leq(permission1, permission2))
        Seq((expression1, permission1), (expression2, permission2))
      }
      set.flatten
    case Conditional(condition, left, right) =>
      val set1 = for ((constraint, permission) <- left.rewrite) yield {
        val expression: Expression = and(constraint, condition)
        (expression, permission)
      }
      val set2 = for ((constraint, permission) <- right.rewrite) yield {
        val expression: Expression = and(constraint, not(condition))
        (expression, permission)
      }
      set1 ++ set2
    case _ => ???
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

    override def toString: String = "p_0"
  }

  case class Leaf(receiver: Expression, permission: Permission)
    extends PermissionTree {

    override def toString: String = s"(q == $receiver ? $permission : none)"
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
