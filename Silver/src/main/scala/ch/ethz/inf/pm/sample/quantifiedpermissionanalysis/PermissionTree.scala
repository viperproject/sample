/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, IntType}
import viper.silver.ast.{Exp, PermDiv}
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: sil.LocalVar): sil.Exp
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
  def getMaxPermission: (FractionalPermission, Int)
}

case class PermissionLeaf(receiver: ExpressionDescription, permission: Permission) extends PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: sil.LocalVar): sil.Exp =
    sil.CondExp(expressions(receiver.key).toSilExpression(quantifiedVariable, Context.getSetFor(receiver.key)), permission.toSilExpression, sil.NoPerm()())()
  def transform(f: (Expression => Expression)) = PermissionLeaf(receiver.transform(f), permission.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = f(receiver)
  override def undoLastRead = EmptyPermissionTree
  def getMaxPermission: (FractionalPermission, Int) = permission.toFractional
}

case class PermissionList(permissions: Seq[PermissionTree]) extends PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: sil.LocalVar): sil.Exp =
    permissions.foldLeft[Option[sil.Exp]](None)((rest, permTree) => rest match {
      case None => Some(permTree.toSilExpression(expressions, quantifiedVariable))
      case Some(silExpression) => Some(sil.PermAdd(silExpression, permTree.toSilExpression(expressions, quantifiedVariable))())
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
  def getMaxPermission: (FractionalPermission, Int) =
    permissions.map(perm => perm.getMaxPermission).reduce[(FractionalPermission, Int)] {
      case ((FractionalPermission(num1, denom1), read1), (FractionalPermission(num2, denom2), read2)) => (FractionalPermission(num1 * denom2 + num2 * denom1, denom1 * denom2), read1 + read2)
    }
}

case class NegativePermissionTree(arg: PermissionTree) extends PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: sil.LocalVar): sil.Exp =
    sil.IntPermMul(sil.IntLit(-1)(), arg.toSilExpression(expressions, quantifiedVariable))()
  def transform(f: (Expression => Expression)) = NegativePermissionTree(arg.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || arg.exists(f)
  def foreach(f: (Expression => Unit)): Unit = arg.foreach(f)
  def getMaxPermission: (FractionalPermission, Int) = arg.getMaxPermission match {
    case (FractionalPermission(num, denom), read) => (FractionalPermission(-num, denom), -read)
  }
}

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree) extends PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: sil.LocalVar): sil.Exp =
    sil.CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(expressions, quantifiedVariable), right.toSilExpression(expressions, quantifiedVariable))()
  def transform(f: (Expression => Expression)) = Condition(cond.transform(f), left.transform(f), right.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || left.exists(f) || right.exists(f)
  def foreach(f: (Expression => Unit)): Unit = {
    f(cond)
    left.foreach(f)
    right.foreach(f)
  }
  def getMaxPermission: (FractionalPermission, Int) = (left.getMaxPermission, right.getMaxPermission) match {
    case ((left@FractionalPermission(num1, denom1), read1), (right@FractionalPermission(num2, denom2), read2)) =>
      (if (num1 * denom2 > num2 * denom1) left else right, if (read1 > read2) read1 else read2)
  }
}

case class Maximum(left: PermissionTree, right: PermissionTree)
  extends PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: sil.LocalVar): sil.Exp =
    sil.FuncApp(Context.getMaxFunction, Seq(left.toSilExpression(expressions, quantifiedVariable), right.toSilExpression(expressions, quantifiedVariable)))()
  def transform(f: (Expression => Expression)) = Maximum(left.transform(f), right.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || left.exists(f) || right.exists(f)
  override def undoLastRead: PermissionTree = right
  def foreach(f: (Expression => Unit)): Unit = {
    left.foreach(f)
    right.foreach(f)
  }
  def getMaxPermission: (FractionalPermission, Int) = (left.getMaxPermission, right.getMaxPermission) match {
    case ((left@FractionalPermission(num1, denom1), read1), (right@FractionalPermission(num2, denom2), read2)) =>
      (if (num1 * denom2 > num2 * denom1) left else right, if (read1 > read2) read1 else read2)
  }
}

object EmptyPermissionTree extends PermissionTree {
  override def add(other: PermissionTree): PermissionTree = other
  override def max(other: PermissionTree): PermissionTree = other
  override def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: sil.LocalVar): sil.Exp = ZeroPerm
  override def transform(f: (Expression) => Expression): PermissionTree = this
  override def exists(f: (PermissionTree) => Boolean): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = {}
  def getMaxPermission: (FractionalPermission, Int) = (FractionalPermission(0, 0), 0)
}

trait Permission {
  def toSilExpression: sil.Exp
  def transform(f: (Expression => Expression)): Permission = this
  def toFractional: (FractionalPermission, Int)
}

case class NegatedPermission(arg: Permission) extends Permission {
  def toSilExpression: sil.Exp = sil.IntPermMul(sil.IntLit(-1)(), arg.toSilExpression)()
  override def transform(f: (Expression => Expression)) = NegatedPermission(arg.transform(f))
  def toFractional: (FractionalPermission, Int) = {
    val (argPerm, argRead) = arg.toFractional
    (FractionalPermission(-argPerm.numerator, argPerm.denominator), -argRead)
  }
}

object FractionalPermission {
  def apply(numerator: Expression, denominator: Expression): FractionalPermission = (numerator, denominator) match {
    case (Constant(num, IntType, _), Constant(denum, IntType, _)) => FractionalPermission(num.toInt, denum.toInt)
  }
}

case class FractionalPermission(numerator: Int, denominator: Int) extends Permission {
  override def toSilExpression: PermDiv = sil.PermDiv(sil.IntLit(numerator)(), sil.IntLit(denominator)())()
  override def transform(f: (Expression => Expression)): FractionalPermission = this
  def toFractional: (FractionalPermission, Int) = (this, 0)
}

case class SymbolicReadPermission(toSilExpression: sil.Exp = Context.getRdAmountVariable.localVar, toFractional: (FractionalPermission, Int) = (FractionalPermission(0, 1), 1)) extends Permission

object WritePermission extends Permission {
  override def toSilExpression: Exp = sil.FullPerm()()
  def toFractional: (FractionalPermission, Int) = (FractionalPermission(1, 1), 0)
}

object ZeroPermission extends Permission {
  override def toSilExpression: Exp = sil.NoPerm()()
  def toFractional: (FractionalPermission, Int) = (FractionalPermission(0, 1), 0)
}