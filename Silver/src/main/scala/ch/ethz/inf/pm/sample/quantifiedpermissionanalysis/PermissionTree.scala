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
    case other: PermissionLeaf => PermissionList(Seq(other, this))
    case PermissionList(list) => PermissionList(list :+ this)
    case _ => PermissionList(Seq(other, this))
  }
  def sub(other: PermissionTree): PermissionTree = add(NegativePermissionTree(other))
  def max(other: PermissionTree): PermissionTree = Maximum(other, this)
  def transform(f: (Expression => Expression)): PermissionTree
}

case class PermissionLeaf(receiver: Expression, permission: Permission) extends PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp =
    CondExp(EqCmp(quantifiedVariable, DefaultSampleConverter.convert(receiver))(), permission.toSilExpression, NoPerm()())()
  def transform(f: (Expression => Expression)) = PermissionLeaf(receiver.transform(f), permission.transform(f))
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
      case other: PermissionLeaf => PermissionList(other +: permissions)
    }
  def transform(f: (Expression => Expression)) = PermissionList(permissions.map(permissionTree => permissionTree.transform(f)))
}

case class NegativePermissionTree(arg: PermissionTree) extends PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp =
    IntPermMul(IntLit(-1)(), arg.toSilExpression(quantifiedVariable))()
  def transform(f: (Expression => Expression)) = NegativePermissionTree(arg.transform(f))
}

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree) extends PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp =
    CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(quantifiedVariable), right.toSilExpression(quantifiedVariable))()
  def transform(f: (Expression => Expression)) = Condition(cond.transform(f), left.transform(f), right.transform(f))
}

case class Maximum(left: PermissionTree, right: PermissionTree)
  extends PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp =
    FuncApp(Context.getMaxFunction, Seq(left.toSilExpression(quantifiedVariable), right.toSilExpression(quantifiedVariable)))()
  def transform(f: (Expression => Expression)) = Maximum(left.transform(f), right.transform(f))
}

object EmptyPermissionTree extends PermissionList(Seq()) {
  override def add(other: PermissionTree): PermissionTree = other
  override def max(other: PermissionTree): PermissionTree = other
}

trait Permission {
  def toSilExpression: Exp
  def transform(f: (Expression => Expression)) = this
}

case class NegatedPermission(arg: Permission) extends Permission {
  def toSilExpression: Exp = IntPermMul(IntLit(-1)(), arg.toSilExpression)()
  override def transform(f: (Expression => Expression)): Permission = NegatedPermission(arg.transform(f))
}

case class ExpressionPermission(expr: Expression) extends Permission {
  def toSilExpression: Exp = DefaultSampleConverter.convert(expr)
  def replace(f: (Expression => Expression)): Permission = ExpressionPermission(expr.transform(f))
}

case class FractionalPermission(numerator: Expression, denominator: Expression) extends Permission {
  def toSilExpression: Exp =
    DefaultSampleConverter.convert(BinaryArithmeticExpression(numerator, denominator, ArithmeticOperator./, PermType))
  override def transform(f: (Expression => Expression)) = FractionalPermission(numerator.transform(f), denominator.transform(f))
}

object ReadPermission extends Permission {
  def toSilExpression: Exp = VarRd
}

object WritePermission extends FractionalPermission(Constant("1", PermType), Constant("1", PermType))

object ZeroPermission extends FractionalPermission(Constant("0", PermType), Constant("1", PermType))


object VarRd extends LocalVar("rdAmount")(Perm)