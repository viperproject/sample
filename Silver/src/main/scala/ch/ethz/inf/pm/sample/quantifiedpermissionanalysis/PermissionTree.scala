/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, IntType, PermType}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.ReferenceSetDescription.Inner
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils._
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {

  /**
    * Generates a silver expression that returns the permissions represented by this tree depending on whether the given
    * quantified variable matches the receiver expression(s). This method assumes that the quantified variable is of
    * reference type.
    *
    * @param state The state for which a silver expression should be generated.
    * @param quantifiedVar The quantified reference variable to compare this permission tree against.
    * @return The silver permission expression represented by this tree.
    */
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp

  /**
    * Generates a silver expression that returns the permissions represented by this tree depending on whether the given
    * quantified variable fulfills the respective integer constraints of the subexpressions. This method assumes that
    * the quantified variable is of integer type.
    *
    * @param state The state for which a silver expression should be generated.
    * @param quantifiedVariable The quantified integer variable to compare this permission tree against.
    * @return The silver permission expression represented by this tree, expressed as integer quantification.
    */
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp

  /**
    * Traverses this tree and checks if the whole tree can be expressed by integer quantification instead of reference
    * quantification.
    *
    * @param refSets The set descriptions to check against.
    * @return Whether the permission expression denoted by this tree can be expressed by integer quantification.
    */
  def canBeExpressedByIntegerQuantification(refSets: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean

  def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionAddition(Seq(PermissionLeaf(receiver, permission), this))

  def sub(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionAddition(Seq(PermissionLeaf(receiver, NegativePermission(permission)), this))

  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner]

  def max(other: PermissionTree): PermissionTree = Maximum(Seq(other, this))

  def condition(cond: Expression, elsePermissions: PermissionTree) = Condition(cond, this, elsePermissions)

  def transform(f: (Expression => Expression)): PermissionTree

  def exists(f: (PermissionTree => Boolean)): Boolean

  def foreach(f: (Expression => Unit)): Unit

  /**
    * Removes the last read added to this tree. This method only completes successfully if the last operation on this
    * tree was adding a read access. This method is used to remove read accesses that will be immediately dominated by
    * an exhale or a write to the same location.
    *
    * @return This permission tree with the last read permission removed.
    */
  def undoLastRead: PermissionTree = throw new UnsupportedOperationException("This permission tree does not support undo!")

  /**
    * Lists all possible read amount sums that are reachable starting from this tree. A read amount is a pair of a
    * fractional permission which denotes the sum of all permission amounts encountered along a path without the reads
    * and an integer that denotes the number of encountered occurrences of the symbolic read variable.
    *
    * @return All possible read amounts of this tree.
    */
  def getReadAmounts: Set[(FractionalPermission, Int)]

  /**
    * Performs some basic simplifications on the permission tree.
    *
    * @return The simplified permission tree.
    */
  def simplify: PermissionTree
}

case class ZeroBoundedPermissionTree(child: PermissionTree) extends PermissionTree {
  override def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp = sil.FuncApp(Context.getBoundaryFunction, Seq(child.toSilExpression(state, quantifiedVar)))()
  override def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = sil.FuncApp(Context.getBoundaryFunction, Seq(child.toIntegerQuantification(state, quantifiedVariable)))()
  override def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = child.canBeExpressedByIntegerQuantification(expressions)
  override def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[Inner] = child.getSetDescriptions(expressions)
  override def transform(f: (Expression) => Expression): PermissionTree = ZeroBoundedPermissionTree(child.transform(f))
  override def exists(f: (PermissionTree) => Boolean): Boolean = f(this) || f(child)
  override def foreach(f: (Expression) => Unit): Unit = child.foreach(f)
  override def getReadAmounts: Set[(FractionalPermission, Int)] = child.getReadAmounts
  override def simplify: PermissionTree = child match {
    case ZeroBoundedPermissionTree(grandChild) => ZeroBoundedPermissionTree(grandChild.simplify)
    case other => other
  }
}

case class PermissionLeaf(receiver: ExpressionDescription, permission: Permission) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    sil.CondExp(state.refSets(receiver.key).toSilExpression(state, quantifiedVar), permission.toSilExpression, sil.NoPerm()())()
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    sil.CondExp(state.refSets(receiver.key).toIntegerQuantification(state, quantifiedVariable), permission.toSilExpression, sil.NoPerm()())()
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean =
    expressions(receiver.key).canBeExpressedByIntegerQuantification(expressions)
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] =
    Set(expressions(receiver.key).asInstanceOf[ReferenceSetDescription.Inner])
  def transform(f: (Expression => Expression)) = PermissionLeaf(receiver.transform(f), permission)
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = f(receiver)
  def getReadAmounts: Set[(FractionalPermission, Int)] = permission match {
    case NegativePermission(arg) => arg.getReadPerm match {
      case (FractionalPermission(num, denom), read) => Set((FractionalPermission.createReduced(-num, denom), read))
    }
    case other => Set(other.getReadPerm)
  }
  override def simplify: PermissionTree = this
  override def undoLastRead: PermissionTree = EmptyPermissionTree
}

case class PermissionAddition(permissions: Seq[PermissionTree]) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    permissions.map(_.toSilExpression(state, quantifiedVar)).reduce((left, right) => sil.PermAdd(left, right)())
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    permissions.map(_.toIntegerQuantification(state, quantifiedVariable)).reduce((left, right) => sil.PermAdd(left, right)())
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean =
    permissions.forall(_.canBeExpressedByIntegerQuantification(expressions))
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] =
    permissions.toSet.flatMap((p: PermissionTree) => p.getSetDescriptions(expressions))
  override def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionAddition(PermissionLeaf(receiver, permission) +: permissions)
  override def sub(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionAddition(PermissionLeaf(receiver, NegativePermission(permission)) +: permissions)
  def transform(f: (Expression => Expression)) = PermissionAddition(permissions.map(_.transform(f)))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || permissions.exists(_.exists(f))
  def foreach(f: (Expression => Unit)): Unit = permissions.foreach(_.foreach(f))
  def getReadAmounts: Set[(FractionalPermission, Int)] = Set(permissions.flatMap(_.getReadAmounts).reduceLeft[(FractionalPermission, Int)] {
    case ((FractionalPermission(leftNum, leftDenom), leftRead), (FractionalPermission(rightNum, rightDenom), rightRead)) =>
      (FractionalPermission.createReduced(leftNum * rightDenom + rightNum * leftDenom, leftDenom * rightDenom), leftRead + rightRead)
  })
  override def simplify: PermissionTree = PermissionAddition(permissions.map(_.simplify))
}

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    sil.CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(state, quantifiedVar), right.toSilExpression(state, quantifiedVar))()
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    sil.CondExp(DefaultSampleConverter.convert(cond), left.toIntegerQuantification(state, quantifiedVariable), right.toIntegerQuantification(state, quantifiedVariable))()
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean =
    left.canBeExpressedByIntegerQuantification(expressions) && right.canBeExpressedByIntegerQuantification(expressions)
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] =
    left.getSetDescriptions(expressions) ++ right.getSetDescriptions(expressions)
  def transform(f: (Expression => Expression)) = Condition(cond.transform(f), left.transform(f), right.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || left.exists(f) || right.exists(f)
  def foreach(f: (Expression => Unit)): Unit = {
    f(cond)
    left.foreach(f)
    right.foreach(f)
  }
  def getReadAmounts: Set[(FractionalPermission, Int)] = left.getReadAmounts ++ right.getReadAmounts
  override def simplify: PermissionTree = (left.simplify, right.simplify) match {
    case (l, r) if l == r => l
    case (l, r) => Condition(cond, l, r)
  }
}

case class Maximum(permissions: Seq[PermissionTree])
  extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    permissions.map(_.toSilExpression(state, quantifiedVar)).reduce((left, right) => sil.FuncApp(Context.getMaxFunction, Seq(left, right))())
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    permissions.map(_.toIntegerQuantification(state, quantifiedVariable)).reduce((left, right) => sil.FuncApp(Context.getMaxFunction, Seq(left, right))())
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = permissions.forall(_.canBeExpressedByIntegerQuantification(expressions))
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] = permissions.map(_.getSetDescriptions(expressions)).reduce(_ ++ _)
  def transform(f: (Expression => Expression)) = Maximum(permissions.map(_.transform(f)))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || permissions.exists(_.exists(f))
  override def max(other: PermissionTree): PermissionTree = Maximum(other +: permissions)
  override def undoLastRead: PermissionTree = permissions match {
    case PermissionLeaf(_, SymbolicReadPermission) :: rest => Maximum(rest)
    case _ => throw new IllegalStateException("To undo a read, the last max'ed permission to this tree has to be a symbolic read permission!")
  }
  def foreach(f: (Expression => Unit)): Unit = permissions.foreach(_.foreach(f))
  def getReadAmounts: Set[(FractionalPermission, Int)] = permissions.map(_.getReadAmounts).reduce(_ ++ _)
  def simplify: PermissionTree = Maximum(permissions.distinct.flatMap {
    case Maximum(otherPerms) => otherPerms
    case other => Seq(other)
  })
}

object EmptyPermissionTree extends PermissionTree {
  override def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionLeaf(receiver, permission)
  override def sub(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionLeaf(receiver, NegativePermission(permission))
  override def max(other: PermissionTree): PermissionTree = other
  override def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp = ZeroPerm
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = ZeroPerm
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = true
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] = Set()
  override def transform(f: (Expression) => Expression): PermissionTree = this
  override def exists(f: (PermissionTree) => Boolean): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = {}
  def getReadAmounts: Set[(FractionalPermission, Int)] = Set()
  def simplify: PermissionTree = this
}

trait Permission {
  def toSilExpression: sil.Exp
  def getReadPerm: (FractionalPermission, Int)
}

trait SimplePermission extends Permission

case class NegativePermission(arg: SimplePermission) extends Permission {
  def toSilExpression: sil.Exp = sil.IntPermMul(sil.IntLit(-1)(), arg.toSilExpression)()
  def getReadPerm: (FractionalPermission, Int) = arg.getReadPerm match {
    case (FractionalPermission(num, denom), r) => (FractionalPermission.createReduced(-num, denom), -r)
  }
}

object FractionalPermission {
  def createReduced(numerator: Int, denominator: Int): FractionalPermission = {
    val divisor = gcd(numerator, denominator)
    new FractionalPermission(numerator / divisor, denominator / divisor)
  }
  def apply(numerator: Expression, denominator: Expression): FractionalPermission = (numerator, denominator) match {
    case (Constant(num, IntType | PermType, _), Constant(denom, IntType | PermType, _)) => FractionalPermission.createReduced(num.toInt, denom.toInt)
  }
}

case class FractionalPermission(numerator: Int, denominator: Int) extends SimplePermission {
  if (denominator < 1) throw new IllegalArgumentException("Denominator of a fractional permission must be greater than 0!")
  override def toSilExpression: sil.FractionalPerm = sil.FractionalPerm(sil.IntLit(numerator)(), sil.IntLit(denominator)())()
  def getReadPerm: (FractionalPermission, Int) = (this, 0)
  def <(other: FractionalPermission): Boolean = other match {
    case FractionalPermission(otherNumerator, otherDenominator) => numerator * otherDenominator < denominator * otherNumerator
  }
}

case object SymbolicReadPermission extends SimplePermission {
  override def toSilExpression: sil.Exp = Context.getRdAmountVariable.localVar
  def getReadPerm: (FractionalPermission, Int) = (FractionalPermission(0, 1), 1)
}

case object WritePermission extends SimplePermission {
  override def toSilExpression: sil.Exp = sil.FullPerm()()
  def getReadPerm: (FractionalPermission, Int) = (FractionalPermission(1, 1), 0)
}