package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import viper.silver.ast._

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {
  def toSilExpression(quantifiedVariable: VariableIdentifier): Exp
  def add(other: PermissionTree) = other match {
    case other: PermissionLeaf => PermissionList(Seq(other, this))
    case PermissionList(list) => PermissionList(list :+ this)
    case _ => PermissionList(Seq(other, this))
  }
  def max(other: PermissionTree) = Maximum(this, other)
}

case class PermissionLeaf(expression: Expression, permission: Permission) extends PermissionTree {
  def toSilExpression(quantifiedVariable: VariableIdentifier): Exp =
    CondExp(EqCmp(DefaultSampleConverter.convert(quantifiedVariable), DefaultSampleConverter.convert(expression))(), permission.toSilExpression, NoPerm()())()
}

case class PermissionList(permissions: Seq[PermissionTree]) extends PermissionTree {
  override def toSilExpression(quantifiedVariable: VariableIdentifier): Exp =
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

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree) extends PermissionTree {
  override def toSilExpression(quantifiedVariable: VariableIdentifier): Exp =
    CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(quantifiedVariable), right.toSilExpression(quantifiedVariable))()
}

case class Maximum(left: PermissionTree, right: PermissionTree)
  extends PermissionTree {
  override def toSilExpression(quantifiedVariable: VariableIdentifier): Exp = {
    FuncApp(MaxFunction, Seq(left.toSilExpression(quantifiedVariable), right.toSilExpression(quantifiedVariable)))()
  }
}

object EmptyPermissionTree extends PermissionList(Seq())

trait Permission {
  def toSilExpression: Exp
}

case class NegatedPermission(arg: Permission) extends Permission {
  override def toSilExpression: Exp = Sub(IntLit(0)(), arg.toSilExpression)()
}

case class PermissionExpression(expr: Expression) extends Permission {
  override def toSilExpression: Exp = DefaultSampleConverter.convert(expr)
}

case class FractionalPermission(numerator: Expression, denominator: Expression) extends Permission {
  override def toSilExpression: Exp = PermDiv(DefaultSampleConverter.convert(numerator), DefaultSampleConverter.convert(denominator))()
}

case class ReadPermission() extends Permission {
  override def toSilExpression: Exp = VarRd
}

object WritePermission extends FractionalPermission(Constant("1"), Constant("1"))

object ZeroPermission extends FractionalPermission(Constant("0"), Constant("1"))



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