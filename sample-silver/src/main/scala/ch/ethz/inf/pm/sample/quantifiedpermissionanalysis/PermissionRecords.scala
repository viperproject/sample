/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.PermType
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.PermissionTree._
import ch.ethz.inf.pm.sample.util.{Maps, SampleExpressions}

/**
  * Maps fields to permission trees.
  *
  * @param map      The map from fields to permission trees.
  * @param isTop    The top flag.
  * @param isBottom the bottom flag.
  * @author Jerome Dohrau
  */
case class PermissionRecords(map: Map[Identifier, PermissionTree] = Map.empty,
                             isTop: Boolean = false,
                             isBottom: Boolean = false)
  extends Lattice[PermissionRecords] {

  import SampleExpressions._

  override def factory(): PermissionRecords = PermissionRecords()

  override def top(): PermissionRecords = factory().copy(isTop = true)

  override def bottom(): PermissionRecords = factory().copy(isBottom = true)

  override def lub(other: PermissionRecords): PermissionRecords =
    if (isTop || other.isBottom) this
    else if (isBottom || other.isTop) other
    else {
      val newMap = Maps.union(map, other.map, Maximum)
      copy(map = newMap)
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
    transformExpressions {
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
    case True => this
    case False => bottom()
    case BinaryBooleanExpression(left, right, operator) => operator match {
      case BooleanOperator.&& => inhale(left).inhale(right)
      case BooleanOperator.|| => inhale(left) lub inhale(right)
    }
    case FieldAccessPredicate(location, permission) =>
      val FieldAccessExpression(receiver, field) = location
      val condition = receiverToCondition(receiver)
      val leaf = Leaf(condition, permission)
      update(field, Subtraction(_, leaf))
    case _ => assume(expression)
  }

  def exhale(expression: Expression): PermissionRecords = expression match {
    case BinaryBooleanExpression(left, right, operator) => operator match {
      case BooleanOperator.&& => exhale(left).exhale(right)
      case BooleanOperator.|| => exhale(left) lub exhale(right)
    }
    case FieldAccessPredicate(location, permission) =>
      val FieldAccessExpression(receiver, field) = location
      val condition = receiverToCondition(receiver)
      val leaf = Leaf(condition, permission)
      update(field, Addition(_, leaf))
    case _ => this
  }

  def read(expression: Expression): PermissionRecords = {
    val declaration = Context.getReadVariable
    val variable = VariableIdentifier(declaration.name)(PermType)
    access(expression, variable)
  }

  def write(expression: Expression): PermissionRecords = access(expression, Full)

  def access(expression: Expression, permission: Expression): PermissionRecords = expression match {
    case _: Constant => this
    case _: VariableIdentifier => this
    case BinaryBooleanExpression(left, right, _) => read(left).read(right)
    case ReferenceComparisonExpression(left, right, _) => read(left).read(right)
    case UnaryArithmeticExpression(argument, _, _) => read(argument)
    case BinaryArithmeticExpression(left, right, _) => read(left).read(right)
    case NegatedBooleanExpression(argument) => read(argument)
    case FieldAccessExpression(receiver, field) =>
      val condition = receiverToCondition(receiver)
      val leaf = Leaf(condition, permission)
      update(field, Maximum(_, leaf)).read(receiver)
    case FunctionCallExpression(_, arguments, _, _) =>
      arguments.foldLeft(this) { case (updated, argument) => updated.read(argument) }
    case FieldAccessPredicate(FieldAccessExpression(receiver, _), _) => read(receiver)
    case _ => ???
  }

  def receiverToCondition(receiver: Expression): Expression = receiver match {
    case FunctionCallExpression(name, arguments, _, _) =>
      val quantified = Context.getVariables(name)
      val zipped = quantified zip arguments
      AndList(zipped.map { case (q, a) => Equal(q, a) })
  }

  def forget(variables: Seq[VariableIdentifier], invariant: Expression): PermissionRecords = {
    // TODO: Don't replace initial with empty
    val updated = map.map { case (key, tree) =>
      val transformed = tree.transform {
        case Initial => Empty
        case other => other
      }
      val simplified = transformed.simplify
      key -> simplified.forget(variables, invariant)
    }
    copy(map = updated)
  }

  lazy val hasRead: Boolean = {
    val declaration = Context.getReadVariable
    val variable = VariableIdentifier(declaration.name)(PermType)
    contains {
      case PermissionTree.Leaf(_, expression) => expression.contains(_ == variable)
      case _ => false
    }
  }

  def update(field: Identifier, f: PermissionTree => PermissionTree): PermissionRecords = {
    val updated = map + (field -> f(map(field)))
    copy(map = updated)
  }

  def transformExpressions(f: Expression => Expression): PermissionRecords = transform {
    case Leaf(receiver, permission) => Leaf(receiver.transform(f), permission.transform(f))
    case Conditional(condition, left, right) => Conditional(condition.transform(f), left, right)
    case tree => tree
  }

  def transform(f: PermissionTree => PermissionTree): PermissionRecords = {
    val transformed = map.mapValues(_.transform(f))
    copy(map = transformed)
  }

  def contains(f: PermissionTree => Boolean): Boolean =
    map.values.exists(_.contains(f))

  def clear(): PermissionRecords = {
    val emptyMap = map.mapValues(_ => Initial)
    copy(map = emptyMap)
  }

  def copy(map: Map[Identifier, PermissionTree] = map,
           isTop: Boolean = isTop,
           isBottom: Boolean = isBottom): PermissionRecords =
    PermissionRecords(map, isTop, isBottom)
}
