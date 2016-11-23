package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, PermType}
import viper.silver.ast._

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp
  def add(other: PermissionTree): PermissionTree = other match {
    case leaf: PermissionLeaf => PermissionList(Seq(leaf, this))
    case PermissionList(list) => PermissionList(list :+ this)
    case _ => PermissionList(Seq(other, this))
  }
  def sub(other: PermissionTree): PermissionTree = add(NegativePermissionTree(other))
  def max(other: PermissionTree): PermissionTree = Maximum(other, this)
  def transform(f: (Expression => Expression)): PermissionTree
  def exists(f: (PermissionTree => Boolean)): Boolean
  def undoLastRead: PermissionTree = throw new UnsupportedOperationException("This permission tree does not support undo!")
}

case class PermissionLeaf(receiver: Expression, permission: Permission) extends PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp =
    CondExp(if (quantifiedVariable.typ.isInstanceOf[SetType]) AnySetContains(DefaultSampleConverter.convert(receiver), quantifiedVariable)() else EqCmp(quantifiedVariable, DefaultSampleConverter.convert(receiver))(), permission.toSilExpression, NoPerm()())()
  def transform(f: (Expression => Expression)) = PermissionLeaf(receiver.transform(f), permission.transform(f))
  def exists(f: (PermissionTree => Boolean)) = f(this)
  override def undoLastRead = EmptyPermissionTree
}

case class PermissionList(permissions: Seq[PermissionTree]) extends PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp =
    permissions.foldLeft[Option[Exp]](None)((rest, permTree) => rest match {
      case None => Some(permTree.toSilExpression(quantifiedVariable))
      case Some(silExpression) => Some(PermAdd(silExpression, permTree.toSilExpression(quantifiedVariable))())
    }).get
  override def add(other: PermissionTree) =
    other match {
      case PermissionList(otherPermissions) => PermissionList(otherPermissions ++ permissions)
      case _: PermissionLeaf => PermissionList(other +: permissions)
    }
  def transform(f: (Expression => Expression)) = PermissionList(permissions.map(permissionTree => permissionTree.transform(f)))
  def exists(f: (PermissionTree => Boolean)) = f(this) || permissions.exists(permissionTree => permissionTree.exists(f))
  override def undoLastRead = this match {
    case PermissionList(onlyElement :: Nil) => EmptyPermissionTree
    case PermissionList(perms) => PermissionList(perms.init)
  }
}

case class NegativePermissionTree(arg: PermissionTree) extends PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp =
    IntPermMul(IntLit(-1)(), arg.toSilExpression(quantifiedVariable))()
  def transform(f: (Expression => Expression)) = NegativePermissionTree(arg.transform(f))
  def exists(f: (PermissionTree => Boolean)) = f(this) || arg.exists(f)
}

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree) extends PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp =
    CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(quantifiedVariable), right.toSilExpression(quantifiedVariable))()
  def transform(f: (Expression => Expression)) = Condition(cond.transform(f), left.transform(f), right.transform(f))
  def exists(f: (PermissionTree => Boolean)) = f(this) || left.exists(f) || right.exists(f)
}

case class Maximum(left: PermissionTree, right: PermissionTree)
  extends PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp =
    FuncApp(Context.getMaxFunction, Seq(left.toSilExpression(quantifiedVariable), right.toSilExpression(quantifiedVariable)))()
  def transform(f: (Expression => Expression)) = Maximum(left.transform(f), right.transform(f))
  def exists(f: (PermissionTree => Boolean)) = f(this) || left.exists(f) || right.exists(f)
  override def undoLastRead = right
}

object EmptyPermissionTree extends PermissionTree {
  override def add(other: PermissionTree): PermissionTree = other
  override def max(other: PermissionTree): PermissionTree = other
  override def toSilExpression(quantifiedVariable: LocalVar): Exp = ZeroPerm
  override def transform(f: (Expression) => Expression): PermissionTree = this
  override def exists(f: (PermissionTree) => Boolean): Boolean = f(this)
}

trait Permission {
  def toSilExpression: Exp
  def transform(f: (Expression => Expression)) = this
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

object SymbolicReadPermission extends SimpleExpressionPermission(ReadPermission)

object WritePermission extends SimpleExpressionPermission(Constant("1", PermType))

object ZeroPermission extends SimpleExpressionPermission(Constant("0", PermType))