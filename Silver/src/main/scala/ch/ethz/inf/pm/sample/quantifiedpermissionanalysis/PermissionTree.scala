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
import viper.silver.ast.{Exp, LocalVar}
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp
  def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean
  def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionAddition(Seq(PermissionLeaf(receiver, permission), this))
  def sub(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionAddition(Seq(PermissionLeaf(receiver, NegativePermission(permission)), this))
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner]
  def max(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = Maximum(Seq(PermissionLeaf(receiver, permission), this))
  def condition(cond: Expression, elsePermissions: PermissionTree) = Condition(cond, this, elsePermissions)
  def transform(f: (Expression => Expression)): PermissionTree
  def exists(f: (PermissionTree => Boolean)): Boolean
  def foreach(f: (Expression => Unit)): Unit
  def undoLastRead: PermissionTree = throw new UnsupportedOperationException("This permission tree does not support undo!")
  def getReadPaths: Set[(FractionalPermission, Int)]
  def simplify: PermissionTree
}

case class ZeroBoundedPermissionTree(child: PermissionTree) extends PermissionTree {
  override def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: LocalVar): Exp = sil.FuncApp(Context.getBoundaryFunction, Seq(child.toSilExpression(state, quantifiedVar)))()
  override def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: LocalVar): Exp = sil.FuncApp(Context.getBoundaryFunction, Seq(child.toParameterQuantification(state, quantifiedVariable)))()
  override def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = child.canBeExpressedByIntegerQuantification(expressions)
  override def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[Inner] = child.getSetDescriptions(expressions)
  override def transform(f: (Expression) => Expression): PermissionTree = ZeroBoundedPermissionTree(child.transform(f))
  override def exists(f: (PermissionTree) => Boolean): Boolean = f(this) || f(child)
  override def foreach(f: (Expression) => Unit): Unit = child.foreach(f)
  override def getReadPaths: Set[(FractionalPermission, Int)] = child.getReadPaths
  override def simplify: PermissionTree = child match {
    case ZeroBoundedPermissionTree(grandChild) => ZeroBoundedPermissionTree(grandChild.simplify)
    case other => other
  }
}

case class PermissionLeaf(receiver: ExpressionDescription, permission: Permission) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    sil.CondExp(state.refSets(receiver.key).toSilExpression(state, quantifiedVar), permission.toSilExpression, sil.NoPerm()())()
  def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    sil.CondExp(state.refSets(receiver.key).toIntegerQuantification(state, quantifiedVariable), permission.toSilExpression, sil.NoPerm()())()
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean =
    expressions(receiver.key).canBeExpressedByIntegerQuantification(expressions)
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] =
    Set(expressions(receiver.key).asInstanceOf[ReferenceSetDescription.Inner])
  def transform(f: (Expression => Expression)) = PermissionLeaf(receiver.transform(f), permission)
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = f(receiver)
  override def undoLastRead = EmptyPermissionTree
  def getReadPaths: Set[(FractionalPermission, Int)] = permission match {
    case NegativePermission(arg) => arg.getReadPerm match {
      case (FractionalPermission(num, denom), read) => Set((FractionalPermission.createReduced(-num, denom), read))
    }
    case other => Set(other.getReadPerm)
  }
  override def simplify: PermissionTree = this
}

case class PermissionAddition(permissions: Seq[PermissionTree]) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    permissions.foldLeft[Option[sil.Exp]](None)((rest, permTree) => rest match {
      case None => Some(permTree.toSilExpression(state, quantifiedVar))
      case Some(silExpression) => Some(sil.PermAdd(silExpression, permTree.toSilExpression(state, quantifiedVar))())
    }).get
  def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    permissions.foldLeft[Option[sil.Exp]](None)((rest, permTree) => rest match {
      case None => Some(permTree.toParameterQuantification(state, quantifiedVariable))
      case Some(silExpression) => Some(sil.PermAdd(silExpression, permTree.toParameterQuantification(state, quantifiedVariable))())
    }).get
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean =
    permissions.forall(tree => tree.canBeExpressedByIntegerQuantification(expressions))
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] =
    permissions.toSet.flatMap((p: PermissionTree) => p.getSetDescriptions(expressions))
  override def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionAddition(PermissionLeaf(receiver, permission) +: permissions)
  override def sub(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionAddition(PermissionLeaf(receiver, NegativePermission(permission)) +: permissions)
  def transform(f: (Expression => Expression)) = PermissionAddition(permissions.map(permissionTree => permissionTree.transform(f)))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || permissions.exists(permissionTree => permissionTree.exists(f))
  override def undoLastRead: PermissionTree = this match {
    case PermissionAddition(_ :: Nil) => EmptyPermissionTree
    case PermissionAddition(perms) => PermissionAddition(perms.init)
  }
  def foreach(f: (Expression => Unit)): Unit = permissions.foreach(tree => tree.foreach(f))
  def getReadPaths: Set[(FractionalPermission, Int)] = Set(permissions.flatMap(tree => tree.getReadPaths).reduceLeft[(FractionalPermission, Int)] {
    case ((FractionalPermission(leftNum, leftDenom), leftRead), (FractionalPermission(rightNum, rightDenom), rightRead)) =>
      (FractionalPermission.createReduced(leftNum * rightDenom + rightNum * leftDenom, leftDenom * rightDenom), leftRead + rightRead)
  })
  override def simplify: PermissionTree = PermissionAddition(permissions.map(permission => permission.simplify))
}

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    sil.CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(state, quantifiedVar), right.toSilExpression(state, quantifiedVar))()
  def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    sil.CondExp(DefaultSampleConverter.convert(cond), left.toParameterQuantification(state, quantifiedVariable), right.toParameterQuantification(state, quantifiedVariable))()
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
  def getReadPaths: Set[(FractionalPermission, Int)] = left.getReadPaths ++ right.getReadPaths
  override def simplify: PermissionTree = if (left == right) left else Condition(cond, left.simplify, right.simplify)
}

case class Maximum(permissions: Seq[PermissionTree])
  extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    permissions.map(permission => permission.toSilExpression(state, quantifiedVar)).reduce((left, right) => sil.FuncApp(Context.getMaxFunction, Seq(left, right))())
  def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    permissions.map(permission => permission.toParameterQuantification(state, quantifiedVariable)).reduce((left, right) => sil.FuncApp(Context.getMaxFunction, Seq(left, right))())
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = permissions.forall(permission => permission.canBeExpressedByIntegerQuantification(expressions))
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] = permissions.map(permission => permission.getSetDescriptions(expressions)).reduce(_ ++ _)
  def transform(f: (Expression => Expression)) = Maximum(permissions.map(permission => permission.transform(f)))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || permissions.exists(_.exists(f))
  override def max(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = Maximum(PermissionLeaf(receiver, permission) +: permissions)
  override def undoLastRead: PermissionTree = if (permissions.size == 2) permissions.last else Maximum(permissions.tail)
  def foreach(f: (Expression => Unit)): Unit = permissions.foreach(_.foreach(f))
  def getReadPaths: Set[(FractionalPermission, Int)] = permissions.map(_.getReadPaths).reduce(_ ++ _)
  def simplify: PermissionTree = Maximum(permissions.distinct)
}

object EmptyPermissionTree extends PermissionTree {
  override def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionLeaf(receiver, permission)
  override def sub(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionLeaf(receiver, NegativePermission(permission))
  override def max(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionLeaf(receiver, permission)
  override def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp = ZeroPerm
  def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = ZeroPerm
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = true
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] = Set()
  override def transform(f: (Expression) => Expression): PermissionTree = this
  override def exists(f: (PermissionTree) => Boolean): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = {}
  def getReadPaths: Set[(FractionalPermission, Int)] = Set()
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

case class SymbolicReadPermission(toSilExpression: sil.Exp = Context.getRdAmountVariable.localVar, getReadPerm: (FractionalPermission, Int) = (FractionalPermission(0, 1), 1)) extends Permission

object WritePermission extends SimplePermission {
  override def toSilExpression: Exp = sil.FullPerm()()
  def getReadPerm: (FractionalPermission, Int) = (FractionalPermission(1, 1), 0)
}

object ZeroPermission extends SimplePermission {
  override def toSilExpression: Exp = sil.NoPerm()()
  def getReadPerm: (FractionalPermission, Int) = (FractionalPermission(0, 1), 0)
}