/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Permission.{Fractional, Read}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree._
import ch.ethz.inf.pm.sample.util.{Maps, SampleExpressions}

/**
  * Maps fields to permission trees.
  *
  * @param map      The map from fields to permission trees.
  * @param changing The set of changing variables.
  * @param isTop    The top flag.
  * @param isBottom the bottom flag.
  * @author Jerome Dohrau
  */
case class PermissionRecords(map: Map[Identifier, PermissionTree] = Map.empty,
                             changing: Set[VariableIdentifier] = Set.empty,
                             isTop: Boolean = false,
                             isBottom: Boolean = false)
  extends Lattice[PermissionRecords] {

  override def factory(): PermissionRecords = PermissionRecords()

  override def top(): PermissionRecords = factory().copy(isTop = true)

  override def bottom(): PermissionRecords = factory().copy(isBottom = true)

  override def lub(other: PermissionRecords): PermissionRecords =
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else {
      val newMap = Maps.union(map, other.map, Maximum)
      val newChanging = changing ++ other.changing
      copy(map = newMap, changing = newChanging)
    }

  override def glb(other: PermissionRecords): PermissionRecords = ???

  override def widening(other: PermissionRecords): PermissionRecords = ???

  override def lessEqual(other: PermissionRecords): Boolean = ???

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

  def assume(condition: Expression): PermissionRecords = {
    val updated = map.mapValues(_.assume(condition))
    copy(map = updated)
  }

  def assignVariable(target: VariableIdentifier, value: Expression): PermissionRecords =
    copy(changing = changing + target).transformExpressions {
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

  def write(expression: Expression): PermissionRecords = access(expression, Fractional(1, 1))

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

  def forget(variables: Seq[VariableIdentifier], invariant: Expression): PermissionRecords = {
    // TODO: Don't replace initial with empty
    val updated = map.mapValues(_.transform {
      case Initial => Empty
      case other => other
    }.simplify.forget(variables, invariant))
    copy(map = updated)
  }

  def update(field: Identifier, f: PermissionTree => PermissionTree): PermissionRecords = {
    val updated = map + (field -> f(map(field)))
    copy(map = updated)
  }

  def transformExpressions(f: Expression => Expression): PermissionRecords = transform {
    case Leaf(receiver, permission) => Leaf(receiver.transform(f), permission)
    case Conditional(condition, left, right) => Conditional(condition.transform(f), left, right)
    case tree => tree
  }

  def transform(f: PermissionTree => PermissionTree): PermissionRecords = {
    val transformed = map.mapValues(_.transform(f))
    copy(map = transformed)
  }

  def clear(): PermissionRecords = {
    val emptyMap = map.mapValues(_ => Initial)
    copy(map = emptyMap, changing = Set.empty)
  }

  def copy(map: Map[Identifier, PermissionTree] = map,
           changing: Set[VariableIdentifier] = changing,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): PermissionRecords =
    PermissionRecords(map, changing, isTop, isBottom)
}
