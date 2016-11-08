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
  def max(other: PermissionTree): PermissionTree = Maximum(this, other)
}

case class PermissionLeaf(receiver: Expression, permission: Permission) extends PermissionTree {
  def toSilExpression(quantifiedVariable: LocalVar): Exp =
    CondExp(EqCmp(quantifiedVariable, DefaultSampleConverter.convert(receiver))(), permission.toSilExpression, NoPerm()())()
}

case class PermissionList(permissions: Seq[PermissionTree]) extends PermissionTree {
  override def toSilExpression(quantifiedVariable: LocalVar): Exp =
    permissions.foldLeft[Option[Exp]](None)((rest, permTree) => rest match {
      case None => Some(permTree.toSilExpression(quantifiedVariable))
      case Some(silExpression) => Some(PermAdd(silExpression, permTree.toSilExpression(quantifiedVariable))())
    }).get
  override def add(other: PermissionTree) =
    other match {
      case PermissionList(otherPermissions) => PermissionList(otherPermissions ++ permissions)
      case other: PermissionLeaf => PermissionList(other +: permissions)
    }
}

case class NegativePermissionTree(arg: PermissionTree) extends PermissionTree {
  override def toSilExpression(quantifiedVariable: LocalVar): Exp =
    IntPermMul(IntLit(-1)(), arg.toSilExpression(quantifiedVariable))()
}

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree) extends PermissionTree {
  override def toSilExpression(quantifiedVariable: LocalVar): Exp =
    CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(quantifiedVariable), right.toSilExpression(quantifiedVariable))()
}

case class Maximum(left: PermissionTree, right: PermissionTree)
  extends PermissionTree {
  override def toSilExpression(quantifiedVariable: LocalVar): Exp = {
    FuncApp(MaxFunction, Seq(left.toSilExpression(quantifiedVariable), right.toSilExpression(quantifiedVariable)))()
  }
}

object EmptyPermissionTree extends PermissionList(Seq()) {
  override def add(other: PermissionTree): PermissionTree = other
  override def max(other: PermissionTree): PermissionTree = other
}

trait Permission {
  def toSilExpression: Exp
}

case class NegatedPermission(arg: Permission) extends Permission {
  override def toSilExpression: Exp = IntPermMul(IntLit(-1)(), arg.toSilExpression)()
}

case class ExpressionPermission(expr: Expression) extends Permission {
  override def toSilExpression: Exp = DefaultSampleConverter.convert(expr)
}

case class FractionalPermission(numerator: Expression, denominator: Expression) extends Permission {
  override def toSilExpression: Exp = {
    DefaultSampleConverter.convert(BinaryArithmeticExpression(numerator, denominator, ArithmeticOperator./, PermType))
  }
}

object ReadPermission extends Permission {
  override def toSilExpression: Exp = VarRd
}

object WritePermission extends FractionalPermission(Constant("1", PermType), Constant("1", PermType))

object ZeroPermission extends FractionalPermission(Constant("0", PermType), Constant("1", PermType))

object VarXDecl extends LocalVarDecl("x", Perm)()

object VarX extends LocalVar("x")(Perm)

object VarYDecl extends LocalVarDecl("y", Perm)()

object VarY extends LocalVar("y")(Perm)

object VarRd extends LocalVar("rdAmount")(Perm)

object MaxFunction extends Function("max", Seq(VarXDecl, VarYDecl), Perm, Seq(), Seq(),
  Some(CondExp(PermLeCmp(VarX, VarY)(), VarY, VarX)())
)()

object MinFunction extends Function("min", Seq(VarXDecl, VarYDecl), Perm, Seq(), Seq(),
  Some(CondExp(PermGeCmp(VarX, VarY)(), VarY, VarX)())
)()