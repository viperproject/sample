/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.ProgramPoint
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, PermType}
import viper.silver.ast._

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: LocalVar): Exp
  def add(other: PermissionTree): PermissionTree = other match {
    case leaf: PermissionLeaf => PermissionList(Seq(leaf, this))
    case PermissionList(list) => PermissionList(list :+ this)
    case _ => PermissionList(Seq(other, this))
  }
  def sub(other: PermissionTree): PermissionTree = add(NegativePermissionTree(other))
  def max(other: PermissionTree): PermissionTree = Maximum(other, this)
  def transform(f: (Expression => Expression)): PermissionTree
  def exists(f: (PermissionTree => Boolean)): Boolean
  def foreach(f: (Expression => Unit)): Unit
  def undoLastRead: PermissionTree = throw new UnsupportedOperationException("This permission tree does not support undo!")
}

case class PermissionLeaf(receiver: ExpressionDescription, permission: Permission) extends PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: LocalVar): Exp =
    CondExp(expressions((receiver.pp, receiver.expr)).toSilExpression(quantifiedVariable), permission.toSilExpression, NoPerm()())()
  def transform(f: (Expression => Expression)) = PermissionLeaf(receiver.transform(f), permission.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = f(receiver)
  override def undoLastRead = EmptyPermissionTree
}

case class PermissionList(permissions: Seq[PermissionTree]) extends PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: LocalVar): Exp =
    permissions.foldLeft[Option[Exp]](None)((rest, permTree) => rest match {
      case None => Some(permTree.toSilExpression(expressions, quantifiedVariable))
      case Some(silExpression) => Some(PermAdd(silExpression, permTree.toSilExpression(expressions, quantifiedVariable))())
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
}

case class NegativePermissionTree(arg: PermissionTree) extends PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: LocalVar): Exp =
    IntPermMul(IntLit(-1)(), arg.toSilExpression(expressions, quantifiedVariable))()
  def transform(f: (Expression => Expression)) = NegativePermissionTree(arg.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || arg.exists(f)
  def foreach(f: (Expression => Unit)): Unit = arg.foreach(f)
}

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree) extends PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: LocalVar): Exp =
    CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(expressions, quantifiedVariable), right.toSilExpression(expressions, quantifiedVariable))()
  def transform(f: (Expression => Expression)) = Condition(cond.transform(f), left.transform(f), right.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || left.exists(f) || right.exists(f)
  def foreach(f: (Expression => Unit)): Unit = {
    f(cond)
    left.foreach(f)
    right.foreach(f)
  }
}

case class Maximum(left: PermissionTree, right: PermissionTree)
  extends PermissionTree {
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: LocalVar): Exp =
    FuncApp(Context.getMaxFunction, Seq(left.toSilExpression(expressions, quantifiedVariable), right.toSilExpression(expressions, quantifiedVariable)))()
  def transform(f: (Expression => Expression)) = Maximum(left.transform(f), right.transform(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || left.exists(f) || right.exists(f)
  override def undoLastRead: PermissionTree = right
  def foreach(f: (Expression => Unit)): Unit = {
    left.foreach(f)
    right.foreach(f)
  }
}

object EmptyPermissionTree extends PermissionTree {
  override def add(other: PermissionTree): PermissionTree = other
  override def max(other: PermissionTree): PermissionTree = other
  override def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription], quantifiedVariable: LocalVar): Exp = ZeroPerm
  override def transform(f: (Expression) => Expression): PermissionTree = this
  override def exists(f: (PermissionTree) => Boolean): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = {}
}

trait Permission {
  def toSilExpression: Exp
  def transform(f: (Expression => Expression)): Permission = this
}

case class NegatedPermission(arg: Permission) extends Permission {
  def toSilExpression: Exp = IntPermMul(IntLit(-1)(), arg.toSilExpression)()
  override def transform(f: (Expression => Expression)) = NegatedPermission(arg.transform(f))
}

trait ExpressionPermission extends Permission {
  def expr: Expression
  def toSilExpression: Exp = DefaultSampleConverter.convert(expr)
}

case class SimpleExpressionPermission(expr: Expression) extends ExpressionPermission {
  override def transform(f: (Expression => Expression)) = SimpleExpressionPermission(expr.transform(f))
}

case class FractionalPermission(numerator: Expression, denominator: Expression) extends ExpressionPermission {
  def expr = BinaryArithmeticExpression(numerator, denominator, ArithmeticOperator./, PermType)
  override def transform(f: (Expression => Expression)) = FractionalPermission(numerator.transform(f), denominator.transform(f))
}

case class SymbolicReadPermission(toSilExpression: Exp = Context.getRdAmountVariable.localVar) extends Permission

object WritePermission extends SimpleExpressionPermission(Constant("1", PermType))

object ZeroPermission extends SimpleExpressionPermission(Constant("0", PermType))