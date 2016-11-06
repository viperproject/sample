package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import viper.silver.ast._

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {
  def toSilExpression: Exp
  def add(other: PermissionTree) = Addition(other, this)
  def subtract(other: PermissionTree) = Subtraction(this, other)
  def subtractFrom(other: PermissionTree) = Subtraction(other, this)
  def max(other: PermissionTree) = Maximum(other, this)
  def min(other: PermissionTree) = Minimum(other, this)
  def negate(other: PermissionTree) = Negation(this)
}

trait UnaryNode extends PermissionTree {
  def arg: PermissionTree
}

trait BinaryNode extends PermissionTree {
  def left: PermissionTree
  def right: PermissionTree
}

case class Addition(left: PermissionTree, right: PermissionTree) extends BinaryNode {
  override def toSilExpression: Exp = Add(left.toSilExpression, right.toSilExpression)()
}

case class Subtraction(left: PermissionTree, right: PermissionTree) extends BinaryNode {
  override def toSilExpression: Exp = Sub(left.toSilExpression, right.toSilExpression)()
}

case class Maximum(left: PermissionTree, right: PermissionTree) extends BinaryNode {
  override def toSilExpression: Exp = FuncApp(MaxFunction, Seq(left.toSilExpression, right.toSilExpression))()
}

case class Minimum(left: PermissionTree, right: PermissionTree) extends BinaryNode {
  override def toSilExpression: Exp = FuncApp(MinFunction, Seq(left.toSilExpression, right.toSilExpression))()
}

case class Negation(arg: PermissionTree) extends UnaryNode {
  override def toSilExpression: Exp = Sub(IntLit(0)(), arg.toSilExpression)()
}

object VarXDecl extends LocalVarDecl("x", Perm)()

object VarX extends LocalVar("x")(Perm)

object VarYDecl extends LocalVarDecl("y", Perm)()

object VarY extends LocalVar("y")(Perm)

object MaxFunction extends Function("max", Seq(VarXDecl, VarYDecl), Perm, Seq(), Seq(),
  Some(CondExp(PermLeCmp(VarX, VarY)(), VarY, VarX)())
)()

object MinFunction extends Function("min", Seq(VarXDecl, VarYDecl), Perm, Seq(), Seq(),
  Some(CondExp(PermGeCmp(VarX, VarY)(), VarY, VarX)())
)()