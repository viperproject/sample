/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Permission.{Read, Write}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree._
import ch.ethz.inf.pm.sample.util.Maps

/**
  * Maps fields to permission trees.
  *
  * @param map The map from fields to permission trees.
  * @author Jerome Dohrau
  */
case class PermissionRecords(map: Map[Identifier, PermissionTree] = Map.empty) {
  /**
    * The fields for which there is a permission tree.
    *
    * @return The fields.
    */
  def fields: Seq[Identifier] = map.keys.toSeq

  /**
    * Returns the permission tree associated with the given field.
    *
    * @param field The field.
    * @return The permission tree associated with the given field.
    */
  def apply(field: Identifier): PermissionTree = map(field)

  def lub(other: PermissionRecords): PermissionRecords = {
    val updated = Maps.union(map, other.map, Maximum)
    PermissionRecords(updated)
  }

  def assume(condition: Expression): PermissionRecords = {
    val updated = map.mapValues(_.assume(condition))
    PermissionRecords(updated)
  }

  def assignVariable(target: VariableIdentifier, value: Expression): PermissionRecords = transformExpressions {
    case variable: VariableIdentifier if variable == target => value
    case expression => expression
  }

  def assignField(target: FieldAccessExpression, value: Expression): PermissionRecords = transformExpressions {
    case expression: FieldAccessExpression if target.field == expression.field =>
      if (target.receiver == expression.receiver) value
      else {
        val equality = ReferenceComparisonExpression(target.receiver, expression.receiver, ReferenceOperator.==)
        ConditionalExpression(equality: Expression, value, expression)
      }
    case expression => expression
  }

  def inhale(expression: Expression): PermissionRecords = expression match {
    case BinaryBooleanExpression(left, right, operator) => operator match {
      case BooleanOperator.&& => inhale(left).inhale(right)
      case BooleanOperator.|| => inhale(left) lub inhale(right)
    }
    case FieldAccessPredicate(location, numerator, denominator, _) =>
      val FieldAccessExpression(receiver, field) = location
      val permission = Permission.create(numerator, denominator)
      val leaf = Leaf(receiver, permission)
      update(field, Subtraction(_, leaf))
  }

  def exhale(expression: Expression): PermissionRecords = expression match {
    case BinaryBooleanExpression(left, right, operator) => operator match {
      case BooleanOperator.&& => exhale(left).exhale(right)
      case BooleanOperator.|| => exhale(left) lub exhale(right)
    }
    case FieldAccessPredicate(location, numerator, denominator, _) =>
      val FieldAccessExpression(receiver, field) = location
      val permission = Permission.create(numerator, denominator)
      val leaf = Leaf(receiver, permission)
      update(field, Addition(_, leaf))
  }

  def read(expression: Expression): PermissionRecords = access(expression, Read)

  def write(expression: Expression): PermissionRecords = access(expression, Write)

  def access(expression: Expression, permission: Permission): PermissionRecords = expression match {
    case _: Constant => this
    case _: VariableIdentifier => this
    case BinaryBooleanExpression(left, right, _) => read(left).read(right)
    case ReferenceComparisonExpression(left, right, _) => read(left).read(right)
    case BinaryArithmeticExpression(left, right, _) => read(left).read(right)
    case NegatedBooleanExpression(argument) => read(argument)
    case FieldAccessExpression(receiver, field) =>
      val leaf = Leaf(receiver, permission)
      update(field, Maximum(_, leaf)).read(receiver)
    case FunctionCallExpression(_, arguments, _, _) =>
      arguments.foldLeft(this) { case (updated, argument) => updated.read(argument) }
    case FieldAccessPredicate(FieldAccessExpression(receiver, _), _, _, _) => read(receiver)
    case _ => ???
  }

  def update(field: Identifier, f: PermissionTree => PermissionTree): PermissionRecords = {
    val updated = map + (field -> f(map(field)))
    PermissionRecords(updated)
  }

  def transformExpressions(f: Expression => Expression): PermissionRecords = transform {
    case Leaf(receiver, permission) => Leaf(receiver.transform(f), permission)
    case Conditional(condition, left, right) => Conditional(condition.transform(f), left, right)
    case tree => tree
  }

  def transform(f: PermissionTree => PermissionTree): PermissionRecords = {
    val transformed = map.mapValues(_.transform(f))
    PermissionRecords(transformed)
  }
}

sealed trait PermissionTree {

  def bound(): PermissionTree = Maximum(this, Empty)

  def assume(condition: Expression) = Conditional(condition, this, Empty)

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

sealed trait Permission

object Permission {

  def create(numerator: Expression, denominator: Expression): Permission = {
    val a = numerator.asInstanceOf[Constant].constant.toInt
    val b = denominator.asInstanceOf[Constant].constant.toInt
    if (a == 0) Zero
    else if (a == b) Write
    else Fractional(a, b)
  }

  case object Zero
    extends Permission {

    override def toString: String = "none"
  }

  case object Read
    extends Permission {

    override def toString: String = "read"
  }

  case object Write
    extends Permission {

    override def toString: String = "write"
  }

  case class Fractional(numerator: Int, denominator: Int)
    extends Permission {

    override def toString: String = s"$numerator/$denominator"
  }

}