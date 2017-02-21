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
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils.ExpressionBuilder._
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

  def toIntegerQuantificationSample(state: QuantifiedPermissionsState, quantifiedVariable: VariableIdentifier): Expression

  /**
    * Traverses this tree and checks if the whole tree can be expressed by integer quantification instead of reference
    * quantification.
    *
    * @param refSets The set descriptions to check against.
    * @return Whether the permission expression denoted by this tree can be expressed by integer quantification.
    */
  def canBeExpressedByIntegerQuantification(refSets: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean

  def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionAddition(Seq(PermissionLeaf(receiver, permission), this))

  def sub(receiver: ExpressionDescription, permission: FractionalPermission): PermissionTree = ZeroBoundedPermissionTree(PermissionAddition(Seq(PermissionLeaf(receiver, NegativePermission(permission)), this)))

  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner]

  def max(other: PermissionTree): PermissionTree = Maximum(Seq(other, this))

  def condition(cond: Expression, elsePermissions: PermissionTree) = Condition(cond, this, elsePermissions)

  def transformExpressions(f: (Expression => Expression)): PermissionTree

  def transform(f: (PermissionTree => PermissionTree)): PermissionTree

  def exists(f: (PermissionTree => Boolean)): Boolean

  def foreach(f: (Expression => Unit)): Unit

  /**
    * Removes the last read added to this tree. This method only completes successfully if the last operation on this
    * tree was adding a read access. This method is used to remove read accesses that will be immediately dominated by
    * an exhale or a write to the same location.
    *
    * @return This permission tree with the last read permission removed.
    */
  def undoLastRead: PermissionTree = throw new UnsupportedOperationException(s"$this does not support undo!")

  def hasRead: Boolean

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
  def simplifySyntactically: PermissionTree

  def simplifySemantically(state: QuantifiedPermissionsState): PermissionTree

  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState): Boolean

  def isEquivalent(other: PermissionTree, state: QuantifiedPermissionsState): Boolean = lessEqual(other,state) && other.lessEqual(this, state)
}

trait SequencePermissionTree extends PermissionTree {
  def permissions: Seq[PermissionTree]
  def create(permissions: Seq[PermissionTree]): SequencePermissionTree
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || permissions.exists(_.exists(f))
  def foreach(f: (Expression => Unit)): Unit = permissions.foreach(_.foreach(f))
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean =
    permissions.forall(_.canBeExpressedByIntegerQuantification(expressions))
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] =
    permissions.toSet.flatMap((p: PermissionTree) => p.getSetDescriptions(expressions))
  def hasRead: Boolean = permissions.exists(_.hasRead)
  override def undoLastRead: PermissionTree = permissions.find(_.hasRead) match {
    case Some(exp) => create((permissions.takeWhile(_ != exp) :+ exp.undoLastRead) ++ permissions.dropWhile(_ != exp).tail)
    case None => throw new IllegalStateException(s"Permission tree $this does not contain a read!")
  }
}

case class ZeroBoundedPermissionTree(child: PermissionTree) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp = sil.FuncApp(Context.getBoundaryFunction, Seq(child.toSilExpression(state, quantifiedVar)))()
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = sil.FuncApp(Context.getBoundaryFunction, Seq(child.toIntegerQuantification(state, quantifiedVariable)))()
  def toIntegerQuantificationSample(state: QuantifiedPermissionsState, quantifiedVariable: VariableIdentifier): Expression = {
    val childExpr = child.toIntegerQuantificationSample(state, quantifiedVariable)
    ConditionalExpression(geq(childExpr, intToConst(0, PermType)), childExpr, intToConst(0, PermType), PermType)
  }
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = child.canBeExpressedByIntegerQuantification(expressions)
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[Inner] = child.getSetDescriptions(expressions)
  def transformExpressions(f: (Expression) => Expression): PermissionTree = ZeroBoundedPermissionTree(child.transformExpressions(f))
  def exists(f: (PermissionTree) => Boolean): Boolean = f(this) || child.exists(f)
  def foreach(f: (Expression) => Unit): Unit = child.foreach(f)
  def getReadAmounts: Set[(FractionalPermission, Int)] = child.getReadAmounts
  def simplifySyntactically: PermissionTree = ZeroBoundedPermissionTree(child.simplifySyntactically)
  def hasRead: Boolean = false
  override def sub(receiver: ExpressionDescription, permission: FractionalPermission): PermissionTree = ZeroBoundedPermissionTree(child.sub(receiver, permission))
  def simplifySemantically(state: QuantifiedPermissionsState): PermissionTree = ZeroBoundedPermissionTree(child.simplifySemantically(state))
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState): Boolean = other match {
    case ZeroBoundedPermissionTree(otherChild) => child.lessEqual(otherChild, state)
    case _ => false
  }
  def transform(f: (PermissionTree => PermissionTree)): PermissionTree = f(ZeroBoundedPermissionTree(child.transform(f)))
}

case class PermissionLeaf(receiver: ExpressionDescription, permission: Permission) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    sil.CondExp(state.refSets(receiver.key).toSilExpression(state, quantifiedVar), permission.toSilExpression, sil.NoPerm()())()
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    sil.CondExp(state.refSets(receiver.key).toIntegerQuantification(state, quantifiedVariable), permission.toSilExpression, sil.NoPerm()())()
  def toIntegerQuantificationSample(state: QuantifiedPermissionsState, quantifiedVariable: VariableIdentifier): Expression = {
    val integerParam = state.refSets(receiver.key).extractIntegerParameterExpression
    ConditionalExpression(ExpressionDescription.tupled(integerParam), permission.toSampleExpression, intToConst(0, PermType), PermType)
  }
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean =
    expressions(receiver.key).canBeExpressedByIntegerQuantification(expressions)
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] =
    Set(expressions(receiver.key).asInstanceOf[ReferenceSetDescription.Inner])
  def transformExpressions(f: (Expression => Expression)) = PermissionLeaf(receiver.transform(f), permission)
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = f(receiver)
  def getReadAmounts: Set[(FractionalPermission, Int)] = permission match {
    case NegativePermission(arg) => arg.getReadPerm match {
      case (FractionalPermission(num, denom), read) => Set((FractionalPermission.createReduced(-num, denom), read))
    }
    case other => Set(other.getReadPerm)
  }
  def simplifySyntactically: PermissionLeaf = this
  def simplifySemantically(state: QuantifiedPermissionsState): PermissionLeaf = this
  def hasRead: Boolean = permission == SymbolicReadPermission
  override def undoLastRead: PermissionTree = permission match {
    case SymbolicReadPermission => EmptyPermissionTree
    case _ => throw new IllegalStateException("To undo a read, the last max'ed permission to this tree has to be a symbolic read permission!")
  }
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState): Boolean = other match {
    case PermissionLeaf(otherReceiver, otherPermission) => state.refSets(receiver.key).lessEqual(state.refSets(otherReceiver.key)) && permission.lessEqual(otherPermission)
    case _ => false
  }
  def transform(f: (PermissionTree => PermissionTree)): PermissionTree = f(this)
}

case class PermissionAddition(permissions: Seq[PermissionTree]) extends SequencePermissionTree {
  def create(permissions: Seq[PermissionTree]): PermissionAddition = PermissionAddition(permissions)
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    permissions.map(_.toSilExpression(state, quantifiedVar)).reduce(sil.PermAdd(_, _)())
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    permissions.map(_.toIntegerQuantification(state, quantifiedVariable)).reduce(sil.PermAdd(_, _)())
  def toIntegerQuantificationSample(state: QuantifiedPermissionsState, quantifiedVariable: VariableIdentifier): Expression =
    permissions.map(_.toIntegerQuantificationSample(state, quantifiedVariable)).reduce(plus)
  override def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionAddition = PermissionAddition(PermissionLeaf(receiver, permission) +: permissions)
  override def sub(receiver: ExpressionDescription, permission: FractionalPermission): PermissionAddition = PermissionAddition(PermissionLeaf(receiver, NegativePermission(permission)) +: permissions)
  def transformExpressions(f: (Expression => Expression)) = PermissionAddition(permissions.map(_.transformExpressions(f)))
  def simplifySyntactically: PermissionAddition = PermissionAddition(permissions.map(_.simplifySyntactically))
  def simplifySemantically(state: QuantifiedPermissionsState): PermissionAddition = PermissionAddition(permissions.map(_.simplifySemantically(state)))
  def getReadAmounts: Set[(FractionalPermission, Int)] = Set(permissions.flatMap(_.getReadAmounts).reduceLeft[(FractionalPermission, Int)] {
    case ((FractionalPermission(leftNum, leftDenom), leftRead), (FractionalPermission(rightNum, rightDenom), rightRead)) =>
      (FractionalPermission.createReduced(leftNum * rightDenom + rightNum * leftDenom, leftDenom * rightDenom), leftRead + rightRead)
  })
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState): Boolean = other match {
    case _ => false
  }
  def transform(f: (PermissionTree => PermissionTree)): PermissionTree = f(PermissionAddition(permissions.map(_.transform(f))))
}

case class Maximum(permissions: Seq[PermissionTree]) extends SequencePermissionTree {
  def create(permissions: Seq[PermissionTree]): Maximum = Maximum(permissions)
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    permissions.map(_.toSilExpression(state, quantifiedVar)).reduce((left, right) => sil.FuncApp(Context.getMaxFunction, Seq(left, right))())
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    permissions.map(_.toIntegerQuantification(state, quantifiedVariable)).reduce((left, right) => sil.FuncApp(Context.getMaxFunction, Seq(left, right))())
  def toIntegerQuantificationSample(state: QuantifiedPermissionsState, quantifiedVariable: VariableIdentifier): Expression =
    MaxExpression(permissions.map(_.toIntegerQuantificationSample(state, quantifiedVariable)), PermType)
  def transformExpressions(f: (Expression => Expression)) = Maximum(permissions.map(_.transformExpressions(f)))
  override def max(other: PermissionTree): PermissionTree = Maximum(other +: permissions)
  override def undoLastRead: PermissionTree = permissions match {
    case PermissionLeaf(_, SymbolicReadPermission) :: rest => Maximum(rest)
    case _ => throw new IllegalStateException("To undo a read, the last max'ed permission to this tree has to be a symbolic read permission!")
  }
  def getReadAmounts: Set[(FractionalPermission, Int)] = permissions.map(_.getReadAmounts).reduce(_ ++ _)
  def simplifySyntactically: PermissionTree = (permissions match {
    case singleElement :: Nil => singleElement.simplifySyntactically
    case _ => Maximum(permissions.distinct.map(_.simplifySyntactically).flatMap {
      case Maximum(otherPerms) => otherPerms
      case other => Seq(other)
    })
  }) match {
    case simplified if simplified == this => this
    case simplified => simplified.simplifySyntactically
  }
  def simplifySemantically(state: QuantifiedPermissionsState): Maximum = Maximum(mergeElements(permissions, (a: PermissionTree, b: PermissionTree) => if (a.lessEqual(b, state)) Some(b) else if (b.lessEqual(a, state)) Some(a) else None))
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState): Boolean = other match {
    case Maximum(otherPermissions) => permissions.forall(permission => otherPermissions.exists(permission.lessEqual(_, state)))
    case _ => false
  }
  def transform(f: (PermissionTree => PermissionTree)): PermissionTree = f(Maximum(permissions.map(_.transform(f))))
}

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    sil.CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(state, quantifiedVar), right.toSilExpression(state, quantifiedVar))()
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp =
    sil.CondExp(DefaultSampleConverter.convert(cond), left.toIntegerQuantification(state, quantifiedVariable), right.toIntegerQuantification(state, quantifiedVariable))()
  def toIntegerQuantificationSample(state: QuantifiedPermissionsState, quantifiedVariable: VariableIdentifier): Expression =
    ConditionalExpression(cond, left.toIntegerQuantificationSample(state, quantifiedVariable), right.toIntegerQuantificationSample(state, quantifiedVariable), PermType)
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean =
    left.canBeExpressedByIntegerQuantification(expressions) && right.canBeExpressedByIntegerQuantification(expressions)
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] =
    left.getSetDescriptions(expressions) ++ right.getSetDescriptions(expressions)
  def transformExpressions(f: (Expression => Expression)) = Condition(cond.transform(f), left.transformExpressions(f), right.transformExpressions(f))
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || left.exists(f) || right.exists(f)
  def foreach(f: (Expression => Unit)): Unit = {
    f(cond)
    left.foreach(f)
    right.foreach(f)
  }
  def getReadAmounts: Set[(FractionalPermission, Int)] = left.getReadAmounts ++ right.getReadAmounts
  def simplifySyntactically: PermissionTree = (Utils.simplifyExpression(cond), left.simplifySyntactically, right.simplifySyntactically) match {
    case (`trueConst`, l, _) => l
    case (`falseConst`, _, r) => r
    case (_, l, r) if l == r => l
    case (simplifiedCond, l, r) => Condition(simplifiedCond, l, r)
  }
  def hasRead: Boolean = left.hasRead || right.hasRead
  def simplifySemantically(state: QuantifiedPermissionsState): PermissionTree = this
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState): Boolean = false
  def toMax: Maximum = Maximum(Seq(left, right))
  def hasToBeForgotten(state: QuantifiedPermissionsState): Boolean = cond.contains {
    case id: VariableIdentifier => state.declaredBelowVars.contains(id) || state.changingVars.contains(id)
    case _ => false
  }
  def transform(f: (PermissionTree => PermissionTree)): PermissionTree = f(Condition(cond, left.transform(f), right.transform(f)))
}

case object EmptyPermissionTree extends PermissionTree {
  override def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionLeaf(receiver, permission)
  override def sub(receiver: ExpressionDescription, permission: FractionalPermission): PermissionTree = ZeroBoundedPermissionTree(PermissionAddition(Seq(PermissionLeaf(receiver, NegativePermission(permission)))))
  override def max(other: PermissionTree): PermissionTree = other
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp = ZeroPerm
  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = ZeroPerm
  def toIntegerQuantificationSample(state: QuantifiedPermissionsState, quantifiedVariable: VariableIdentifier): Expression = intToConst(0, PermType)
  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = true
  def getSetDescriptions(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Set[ReferenceSetDescription.Inner] = Set()
  def transformExpressions(f: (Expression) => Expression): PermissionTree = this
  def exists(f: (PermissionTree) => Boolean): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = {}
  def getReadAmounts: Set[(FractionalPermission, Int)] = Set()
  def simplifySyntactically: PermissionTree = this
  def hasRead: Boolean = false
  def simplifySemantically(state: QuantifiedPermissionsState): EmptyPermissionTree.type = this
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState) = true
  def transform(f: (PermissionTree => PermissionTree)): PermissionTree = f(this)
}

trait Permission {
  def toSilExpression: sil.Exp
  def toSampleExpression: Expression
  def getReadPerm: (FractionalPermission, Int)
  def lessEqual(other: Permission): Boolean
}

trait SimplePermission extends Permission

case class NegativePermission(arg: FractionalPermission) extends Permission {
  def toSilExpression: sil.Exp = sil.IntPermMul(sil.IntLit(-1)(), arg.toSilExpression)()
  def toSampleExpression: Expression = mult(Constant("-1", PermType), arg.toSampleExpression)
  def getReadPerm: (FractionalPermission, Int) = arg.getReadPerm match {
    case (FractionalPermission(num, denom), r) => (FractionalPermission.createReduced(-num, denom), -r)
  }
  def lessEqual(other: Permission): Boolean = (this.arg, other) match {
    case (_, NegativePermission(otherArg)) => otherArg.lessEqual(arg)
    case _ => true
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
  require(denominator >= 1, "Denominator of a fractional permission must be greater than 0!")
  def toSilExpression: sil.FractionalPerm = sil.FractionalPerm(sil.IntLit(numerator)(), sil.IntLit(denominator)())()
  def toSampleExpression: Expression = div(Constant(numerator.toString, PermType), Constant(denominator.toString, PermType))
  def getReadPerm: (FractionalPermission, Int) = (this, 0)
  def <(other: FractionalPermission): Boolean = other match {
    case FractionalPermission(otherNumerator, otherDenominator) => numerator * otherDenominator < denominator * otherNumerator
  }
  def <=(other: FractionalPermission): Boolean = !(other < this)
  def lessEqual(other: Permission): Boolean = this == other || (other match {
    case otherFrac: FractionalPermission => this <= otherFrac
    case NegativePermission(FractionalPermission(0, _)) | SymbolicReadPermission if numerator == 0 => true
    case WritePermission => numerator < denominator
    case _ => false
  })
}

case object SymbolicReadPermission extends SimplePermission {
  def toSilExpression: sil.Exp = Context.getRdAmountVariable.localVar
  def toSampleExpression: Expression = VariableIdentifier(Context.getRdAmountVariable.name)(PermType)
  def getReadPerm: (FractionalPermission, Int) = (FractionalPermission(0, 1), 1)
  def lessEqual(other: Permission): Boolean = this == other || (other match {
    case _: NegativePermission | FractionalPermission(0, _) => false
    case _ => true
  })
}

case object WritePermission extends SimplePermission {
  def toSilExpression: sil.Exp = sil.FullPerm()()
  def toSampleExpression: Expression = intToConst(1, PermType)
  def getReadPerm: (FractionalPermission, Int) = (FractionalPermission(1, 1), 0)
  def lessEqual(other: Permission): Boolean = this == other || (other match {
    case FractionalPermission(numerator, denominator) => numerator > denominator
    case _ => false
  })
}