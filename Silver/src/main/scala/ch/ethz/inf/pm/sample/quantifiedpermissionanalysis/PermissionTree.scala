/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, IntType}
import viper.silver.ast.Exp
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState): sil.Exp
  def add(other: PermissionTree): PermissionTree = other match {
    case leaf: PermissionLeaf => PermissionList(Seq(leaf, this))
    case PermissionList(list) => PermissionList(list :+ this)
    case _ => PermissionList(Seq(other, this))
  }
  def sub(other: PermissionTree): PermissionTree = add(NegativePermissionTree(other))
  def max(other: PermissionTree): PermissionTree = Maximum(other, this)
  def condition(cond: Expression, elsePermissions: PermissionTree) = Condition(cond, this, elsePermissions)
  def transform(f: (Expression => Expression)): PermissionTree
  def exists(f: (PermissionTree => Boolean)): Boolean
  def foreach(f: (Expression => Unit)): Unit
  def undoLastRead: PermissionTree = throw new UnsupportedOperationException("This permission tree does not support undo!")
  def getReadPaths: Set[(FractionalPermission, Int)]
}

case class PermissionLeaf(receiver: ExpressionDescription, permission: Permission) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState): sil.Exp =
    sil.CondExp(state.refSets(receiver.key).toSilExpression(state), permission.toSilExpression, sil.NoPerm()())()
  def transform(f: (Expression => Expression)) = PermissionLeaf(receiver.transform(f), permission.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = f(receiver)
  override def undoLastRead = EmptyPermissionTree
  def getReadPaths: Set[(FractionalPermission, Int)] = permission match {
    case NegatedPermission(arg) => arg.getReadPerm match {
      case (FractionalPermission(num, denom), read) => Set((FractionalPermission.createReduced(-num, denom), read))
    }
    case other => Set(other.getReadPerm)
  }
}

case class PermissionList(permissions: Seq[PermissionTree]) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState): sil.Exp =
    permissions.foldLeft[Option[sil.Exp]](None)((rest, permTree) => rest match {
      case None => Some(permTree.toSilExpression(state))
      case Some(silExpression) => Some(sil.PermAdd(silExpression, permTree.toSilExpression(state))())
    }).get
  override def add(other: PermissionTree): PermissionTree =
    other match {
      case PermissionList(otherPermissions) => PermissionList(otherPermissions ++ permissions)
      case _: PermissionLeaf => PermissionList(other +: permissions)
    }
  def transform(f: (Expression => Expression)) = PermissionList(permissions.map(permissionTree => permissionTree.transform(f)))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || permissions.exists(permissionTree => permissionTree.exists(f))
  override def undoLastRead: PermissionTree = this match {
    case PermissionList(_ :: Nil) => EmptyPermissionTree
    case PermissionList(perms) => PermissionList(perms.init)
  }
  def foreach(f: (Expression => Unit)): Unit = permissions.foreach(tree => tree.foreach(f))
  def getReadPaths: Set[(FractionalPermission, Int)] = Set(permissions.flatMap(tree => tree.getReadPaths).reduceLeft[(FractionalPermission, Int)] {
    case ((FractionalPermission(leftNum, leftDenom), leftRead), (FractionalPermission(rightNum, rightDenom), rightRead)) =>
      (FractionalPermission.createReduced(leftNum * rightDenom + rightNum * leftDenom, leftDenom * rightDenom), leftRead + rightRead)
  })
}

case class NegativePermissionTree(arg: PermissionTree) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState): sil.Exp =
    sil.IntPermMul(sil.IntLit(-1)(), arg.toSilExpression(state))()
  def transform(f: (Expression => Expression)) = NegativePermissionTree(arg.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || arg.exists(f)
  def foreach(f: (Expression => Unit)): Unit = arg.foreach(f)
  def getReadPaths: Set[(FractionalPermission, Int)] = arg.getReadPaths.map {
    case (FractionalPermission(num, denom), read) => (FractionalPermission.createReduced(-num, denom), -read)
  }
}

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState): sil.Exp =
    sil.CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(state), right.toSilExpression(state))()
  def transform(f: (Expression => Expression)) = Condition(cond.transform(f), left.transform(f), right.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || left.exists(f) || right.exists(f)
  def foreach(f: (Expression => Unit)): Unit = {
    f(cond)
    left.foreach(f)
    right.foreach(f)
  }
  def getReadPaths: Set[(FractionalPermission, Int)] = left.getReadPaths ++ right.getReadPaths
}

case class Maximum(left: PermissionTree, right: PermissionTree)
  extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState): sil.Exp =
    sil.FuncApp(Context.getMaxFunction, Seq(left.toSilExpression(state), right.toSilExpression(state)))()
  def transform(f: (Expression => Expression)) = Maximum(left.transform(f), right.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || left.exists(f) || right.exists(f)
  override def undoLastRead: PermissionTree = right
  def foreach(f: (Expression => Unit)): Unit = {
    left.foreach(f)
    right.foreach(f)
  }
  def getReadPaths: Set[(FractionalPermission, Int)] = left.getReadPaths ++ right.getReadPaths
}

object EmptyPermissionTree extends PermissionTree {
  override def add(other: PermissionTree): PermissionTree = other
  override def max(other: PermissionTree): PermissionTree = other
  override def toSilExpression(state: QuantifiedPermissionsState): sil.Exp = ZeroPerm
  override def transform(f: (Expression) => Expression): PermissionTree = this
  override def exists(f: (PermissionTree) => Boolean): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = {}
  def getReadPaths: Set[(FractionalPermission, Int)] = Set()
}

trait Permission {
  def toSilExpression: sil.Exp
  def transform(f: (Expression => Expression)): Permission = this
  def getReadPerm: (FractionalPermission, Int)
}

case class NegatedPermission(arg: Permission) extends Permission {
  def toSilExpression: sil.Exp = sil.IntPermMul(sil.IntLit(-1)(), arg.toSilExpression)()
  override def transform(f: (Expression => Expression)) = NegatedPermission(arg.transform(f))
  def getReadPerm: (FractionalPermission, Int) = arg.getReadPerm match {
    case (FractionalPermission(num, denom), r) => (FractionalPermission.createReduced(-num, denom), -r)
  }
}

object FractionalPermission {
  private def gcd(a: Int, b: Int): Int = b match {
    case 0 => a.abs
    case _ => gcd(b, a % b)
  }
  def createReduced(numerator: Int, denominator: Int): FractionalPermission = {
    val divisor = gcd(numerator, denominator)
    new FractionalPermission(numerator / divisor, denominator / divisor)
  }
  def apply(numerator: Expression, denominator: Expression): FractionalPermission = (numerator, denominator) match {
    case (Constant(num, IntType, _), Constant(denom, IntType, _)) => FractionalPermission.createReduced(num.toInt, denom.toInt)
  }
}

case class FractionalPermission(numerator: Int, denominator: Int) extends Permission {
  if (denominator < 1) throw new IllegalArgumentException("Denominator of a fractional permission must be greater than 0!")
  override def toSilExpression: sil.FractionalPerm = sil.FractionalPerm(sil.IntLit(numerator)(), sil.IntLit(denominator)())()
  override def transform(f: (Expression => Expression)): FractionalPermission = this
  def getReadPerm: (FractionalPermission, Int) = (this, 0)
  def <(other: FractionalPermission): Boolean = other match {
    case FractionalPermission(otherNumerator, otherDenominator) => numerator * otherDenominator < denominator * otherNumerator
  }
}

case class SymbolicReadPermission(toSilExpression: sil.Exp = Context.getRdAmountVariable.localVar, getReadPerm: (FractionalPermission, Int) = (FractionalPermission(0, 1), 1)) extends Permission

object WritePermission extends Permission {
  override def toSilExpression: Exp = sil.FullPerm()()
  def getReadPerm: (FractionalPermission, Int) = (FractionalPermission(1, 1), 0)
}

object ZeroPermission extends Permission {
  override def toSilExpression: Exp = sil.NoPerm()()
  def getReadPerm: (FractionalPermission, Int) = (FractionalPermission(0, 1), 0)
}