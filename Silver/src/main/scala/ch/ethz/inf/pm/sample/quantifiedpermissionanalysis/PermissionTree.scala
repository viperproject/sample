/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, IntType, PermType}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.ReferenceSetDescription.Inner
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils._
import viper.silver.{ast => sil}

import scala.collection.Map

/**
  * @author Severin Münger
  *         Added on 31/10/16.
  */

trait PermissionTree {

  def forgottenVariables: Set[Identifier]

  def getReplacementsForForgottenVariables(excludeVariable: Identifier): Map[Identifier, Identifier] = getReplacementsForForgottenVariables(Set(excludeVariable))

  def getReplacementsForForgottenVariables(excludeSet: Set[Identifier]): Map[Identifier, Identifier] = Context.getReplacements(forgottenVariables)

  def replacer(replacements: Map[Identifier, Identifier]): (Identifier) => Identifier = id => replacements.getOrElse(id, id)

  def replaceVariables(expr: Expression, replacements: Map[Identifier, Expression]): Expression = expr.transform {
    case id: Identifier => replacements.getOrElse(id, id)
    case other => other
  }

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
    * Traverses this tree and checks if the whole tree can be expressed by integer quantification instead of reference
    * quantification.
    *
    * @return Whether the permission expression denoted by this tree can be expressed by integer quantification.
    */
  def isIntegerDependent: Boolean = exists {
    case _: IntegerQuantifiedPermissionTree => true
    case FunctionPermissionLeaf(receiver, _, _) => receiver.getIntegerParams.nonEmpty
    case _ => false
  }

  def extractFunction: Option[FunctionExpressionDescription] = {
    var found: Option[FunctionExpressionDescription] = None
    foreach {
      case f: FunctionExpressionDescription => found = Some(f)
      case _ =>
    }
    found
  }

  def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionAddition(Seq(PermissionLeaf(receiver, permission), this))

  def sub(receiver: ExpressionDescription, permission: FractionalPermission): PermissionTree = ZeroBoundedPermissionTree(PermissionAddition(Seq(PermissionLeaf(receiver, NegativePermission(permission)), this)))

  def max(other: PermissionTree): PermissionTree = Maximum(Seq(other, this))

  def condition(cond: Expression, elsePermissions: PermissionTree): PermissionTree = if(isIntegerDependent) Maximum(Seq(transformCondition(cond), elsePermissions.transformCondition(not(cond)))) else Condition(cond, this, elsePermissions)

  def transformForgetVariable(variable: VariableIdentifier): PermissionTree

  def transformAssignVariable(variable: VariableIdentifier): PermissionTree = transform {
    case Condition(cond, left, right, _) if cond.contains {
      case `variable` => true
      case _ => false
    } => Maximum(Seq(left, right))
    case other => other
  }

  def transformAssignField(field: String): PermissionTree = transform {
    case Condition(cond, left, right, _) if cond.contains {
      case FieldExpression(_, `field`, _) => true
      case _ => false
    } => Maximum(Seq(left, right))
    case other => other
  }

  def transformCondition(cond: Expression): PermissionTree = transform {
    case PermissionLeaf(f: FunctionExpressionDescription, permission) => PermissionLeaf(f.transformCondition(cond), permission)
    case other => other
  }

  def transform(f: (PermissionTree => PermissionTree)): PermissionTree

  def exists(f: (PermissionTree => Boolean)): Boolean

  def foreach(f: (Expression => Unit)): Unit

  def forall(f: (PermissionTree => Boolean)): Boolean = !exists(!f(_))

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

  def toIntegerQuantificationConstraints(quantifiedVariable: VariableIdentifier): (Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression])

  def toForgottenTree: IntegerQuantifiedPermissionTree = {
    val quantifiedVariable = VariableIdentifier(Context.getQuantifiedVarDecl(sil.Int).name)(IntType)
    val placeholderFun = Context.getPlaceholderFunction(Context.getQuantifiedVarDecl(sil.Int))
    val permissionPlaceholder = FunctionCallExpression(placeholderFun.name, Seq(quantifiedVariable), PermType)
    val (constraints, invariants, variablesToQuantify, forgottenPermissions) = toIntegerQuantificationConstraints(quantifiedVariable)
    val invariant = invariants.reduceOption(and).getOrElse(trueConst)
    val existsPart = and(invariant, constraints.map { case (constraint, perm) => implies(constraint, equ(permissionPlaceholder, perm)) }.reduce(and))
    val forallPart = implies(invariant, constraints.map { case (constraint, perm) => implies(constraint, geq(permissionPlaceholder, perm)) }.reduce(and))
    IntegerQuantifiedPermissionTree(extractFunction.get, simplifyExpression(and(QuantifierElimination.eliminate(variablesToQuantify, existsPart), not(QuantifierElimination.eliminate(variablesToQuantify, not(forallPart))))), permissionPlaceholder, forgottenPermissions)
  }
}

trait SequencePermissionTree extends PermissionTree {
  def permissions: Seq[PermissionTree]
  def create(permissions: Seq[PermissionTree]): SequencePermissionTree
  def exists(f: (PermissionTree => Boolean)): Boolean = f(this) || permissions.exists(_.exists(f))
  def foreach(f: (Expression => Unit)): Unit = permissions.foreach(_.foreach(f))
  def hasRead: Boolean = permissions.exists(_.hasRead)
  override def undoLastRead: PermissionTree = permissions.find(_.hasRead) match {
    case Some(exp) => create((permissions.takeWhile(_ != exp) :+ exp.undoLastRead) ++ permissions.dropWhile(_ != exp).tail)
    case None => throw new IllegalStateException(s"Permission tree $this does not contain a read!")
  }
}

case class ZeroBoundedPermissionTree(child: PermissionTree, forgottenVariables: Set[Identifier] = Set()) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp = sil.FuncApp(Context.getBoundaryFunction, Seq(child.toSilExpression(state, quantifiedVar)))()
  def exists(f: (PermissionTree) => Boolean): Boolean = f(this) || child.exists(f)
  def foreach(f: (Expression) => Unit): Unit = child.foreach(f)
  def getReadAmounts: Set[(FractionalPermission, Int)] = child.getReadAmounts
  def simplifySyntactically: PermissionTree = child.simplifySyntactically match {
    case PermissionAddition((first@PermissionLeaf(_, NegativePermission(_))) :: ZeroBoundedPermissionTree(otherChild, _) :: rest, _) => ZeroBoundedPermissionTree(PermissionAddition(first +: otherChild +: rest))
    case simplified => ZeroBoundedPermissionTree(simplified)
  }
  def hasRead: Boolean = child.hasRead
  override def sub(receiver: ExpressionDescription, permission: FractionalPermission): PermissionTree = ZeroBoundedPermissionTree(child.sub(receiver, permission))
  def simplifySemantically(state: QuantifiedPermissionsState): PermissionTree = ZeroBoundedPermissionTree(child.simplifySemantically(state))
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState): Boolean = other match {
    case ZeroBoundedPermissionTree(otherChild, _) => child.lessEqual(otherChild, state)
    case _ => false
  }
  def transform(f: (PermissionTree => PermissionTree)): PermissionTree = f(ZeroBoundedPermissionTree(child.transform(f)))
  def toIntegerQuantificationConstraints(quantifiedVariable: VariableIdentifier): (Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression]) = {
    val replacements = getReplacementsForForgottenVariables(quantifiedVariable)
    child.toIntegerQuantificationConstraints(quantifiedVariable) match {
      case (constraints, invariants, forgottenVars, additionalConstraints) => (constraints.flatMap { case (constraint, perm) =>
        val replacedConstraint = replaceVariables(constraint, replacements)
        Set((and(replacedConstraint, geq(perm, noneConst)), perm), (and(replacedConstraint, lt(perm, noneConst)), noneConst))
      }, invariants.map(replaceVariables(_, replacements)), forgottenVars.map(replacer(replacements)), additionalConstraints)
    }
  }
  def transformForgetVariable(variable: VariableIdentifier): PermissionTree = ZeroBoundedPermissionTree(child, forgottenVariables + variable)
}

object PermissionLeaf {
  def apply(expressionDescription: ExpressionDescription, permission: Permission): PermissionLeaf = expressionDescription match {
    case f: FunctionExpressionDescription => FunctionPermissionLeaf(f, permission)
    case s: SimpleExpressionDescription => SimplePermissionLeaf(s, permission)
  }
  def unapply(tree: PermissionTree): Option[(ExpressionDescription, Permission)] = tree match {
    case FunctionPermissionLeaf(receiver, permission, _) => Some(receiver, permission)
    case SimplePermissionLeaf(receiver, permission, _) => Some(receiver, permission)
    case _ => None
  }
}

sealed trait PermissionLeaf extends PermissionTree {
  def receiver: ExpressionDescription
  def permission: Permission
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    sil.CondExp(state.refSets(receiver.key).toSilExpression(state, quantifiedVar), permission.toSilExpression, sil.NoPerm()())()
  def getSetDescriptions(state: QuantifiedPermissionsState): Set[ReferenceSetDescription.Inner] =
    Set(state.refSets(receiver.key).asInstanceOf[ReferenceSetDescription.Inner])
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
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState): Boolean = receiver.isInstanceOf[SimpleExpressionDescription] && (other match {
    case PermissionLeaf(otherReceiver: ExpressionDescription, otherPermission) => state.refSets(receiver.key).lessEqual(state.refSets(otherReceiver.key)) && permission.lessEqual(otherPermission)
    case _ => false
  })
  def transform(f: (PermissionTree => PermissionTree)): PermissionTree = f(this)
}

case class SimplePermissionLeaf(receiver: SimpleExpressionDescription, permission: Permission, forgottenVariables: Set[Identifier] = Set())() extends PermissionLeaf {
  def transformExpressions(f: (Expression => Expression)) = SimplePermissionLeaf(receiver, permission)
  def toIntegerQuantificationConstraints(quantifiedVariable: VariableIdentifier): (Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression]) = throw new UnsupportedOperationException
  def transformForgetVariable(variable: VariableIdentifier): PermissionTree = SimplePermissionLeaf(receiver, permission, forgottenVariables + variable)
}

case class FunctionPermissionLeaf(receiver: FunctionExpressionDescription, permission: Permission, forgottenVariables: Set[Identifier] = Set())() extends PermissionLeaf {
  def transformExpressions(f: (Expression => Expression)) = FunctionPermissionLeaf(receiver, permission)
  def toIntegerQuantificationConstraints(quantifiedVariable: VariableIdentifier): (Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression]) = {
    val intParameter = receiver.getIntegerParams.head
    val expression = intParameter.expr
    val invariants = intParameter.constraints
    val replacements = getReplacementsForForgottenVariables(quantifiedVariable)
    def replacer(expr: Expression) = expr match {
      case v: VariableIdentifier if replacements.contains(v) => replacements(v)
      case other => other
    }
    val replacedExpression = expression.transform(replacer)
    val replacedInvariants = invariants.map(_.transform(replacer))
    (Set((equ(quantifiedVariable, replacedExpression), permission.toSampleExpression), (neq(quantifiedVariable, replacedExpression), noneConst)), replacedInvariants, replacements.values.toSet ++ replacedExpression.ids.toSetOrFail, Map())
  }
  def transformForgetVariable(variable: VariableIdentifier): PermissionTree = FunctionPermissionLeaf(receiver, permission, forgottenVariables + variable)
}

case class PermissionAddition(permissions: Seq[PermissionTree], forgottenVariables: Set[Identifier] = Set()) extends SequencePermissionTree {
  def create(permissions: Seq[PermissionTree]): PermissionAddition = PermissionAddition(permissions)
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    permissions.map(_.toSilExpression(state, quantifiedVar)).reduce(sil.PermAdd(_, _)())
  override def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionAddition = PermissionAddition(PermissionLeaf(receiver, permission) +: permissions)
  override def sub(receiver: ExpressionDescription, permission: FractionalPermission): PermissionAddition = PermissionAddition(PermissionLeaf(receiver, NegativePermission(permission)) +: permissions)
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
  def toIntegerQuantificationConstraints(quantifiedVariable: VariableIdentifier): (Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression]) = {
    val childConstraints = permissions.map(_.toIntegerQuantificationConstraints(quantifiedVariable))
    val replacements = getReplacementsForForgottenVariables(quantifiedVariable)
    childConstraints.reduceRight[(Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression])] { case ((constraints1, invariants1, forgottenVars1, additionalConstraints1), (constraints2, invariants2, forgottenVars2, additionalConstraints2)) =>
      (constraints1.flatMap { case (constraint1, perm1) => constraints2.map { case (constraint2, perm2) => (replaceVariables(and(constraint1, constraint2), replacements), plus(perm1, perm2)) } }, (invariants1 ++ invariants2).map(replaceVariables(_, replacements)), (forgottenVars1 ++ forgottenVars2).map(replacer(replacements)), additionalConstraints1 ++ additionalConstraints2)
    }
  }
  def transformForgetVariable(variable: VariableIdentifier): PermissionTree = PermissionAddition(permissions, forgottenVariables + variable)
}

case class Maximum(permissions: Seq[PermissionTree], forgottenVariables: Set[Identifier] = Set()) extends SequencePermissionTree {
  def create(permissions: Seq[PermissionTree]): Maximum = Maximum(permissions)
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    permissions.map(_.toSilExpression(state, quantifiedVar)).reduce((left, right) => sil.FuncApp(Context.getMaxFunction, Seq(left, right))())
  override def max(other: PermissionTree): PermissionTree = Maximum(other +: permissions)
  override def undoLastRead: PermissionTree = permissions match {
    case PermissionLeaf(_, SymbolicReadPermission) :: rest => Maximum(rest)
    case _ => throw new IllegalStateException("To undo a read, the last max'ed permission to this tree has to be a symbolic read permission!")
  }
  def getReadAmounts: Set[(FractionalPermission, Int)] = permissions.map(_.getReadAmounts).reduce(_ ++ _)
  def simplifySyntactically: PermissionTree = (permissions match {
    case singleElement :: Nil => singleElement.simplifySyntactically
    case _ => Maximum(permissions.distinct.map(_.simplifySyntactically).flatMap {
      case Maximum(otherPerms, _) => otherPerms
      case other => Seq(other)
    })
  }) match {
    case simplified if simplified == this => this
    case simplified => simplified.simplifySyntactically
  }
  def simplifySemantically(state: QuantifiedPermissionsState): Maximum = Maximum(mergeElements(permissions, (a: PermissionTree, b: PermissionTree) => if (a.lessEqual(b, state)) Some(b) else if (b.lessEqual(a, state)) Some(a) else None))
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState): Boolean = other match {
    case Maximum(otherPermissions, _) => permissions.forall(permission => otherPermissions.exists(permission.lessEqual(_, state)))
    case _ => false
  }
  def transform(f: (PermissionTree => PermissionTree)): PermissionTree = f(Maximum(permissions.map(_.transform(f))))
  def toIntegerQuantificationConstraints(quantifiedVariable: VariableIdentifier): (Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression]) = {
    val childConstraints = permissions.map(_.toIntegerQuantificationConstraints(quantifiedVariable))
    val replacements = getReplacementsForForgottenVariables(quantifiedVariable)
    childConstraints.reduceRight[(Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression])] { case ((constraints1, invariants1, forgottenVars1, additionalConstraints1), (constraints2, invariants2, forgottenVars2, additionalConstraints2)) =>
      (constraints1.flatMap { case (constraint1, perm1) => constraints2.flatMap { case (constraint2, perm2) =>
        Set((replaceVariables(and(and(constraint1, constraint2), geq(perm1, perm2)), replacements), perm1),
            (replaceVariables(and(and(constraint2, constraint1), geq(perm2, perm1)), replacements), perm2))
      } }, (invariants1 ++ invariants2).map(replaceVariables(_, replacements)), (forgottenVars1 ++ forgottenVars2).map(replacer(replacements)), additionalConstraints1 ++ additionalConstraints2)
    }
  }
  def transformForgetVariable(variable: VariableIdentifier): PermissionTree = Maximum(permissions, forgottenVariables = forgottenVariables + variable)
}

case class Condition(cond: Expression, left: PermissionTree, right: PermissionTree, forgottenVariables: Set[Identifier] = Set()) extends PermissionTree {
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp =
    sil.CondExp(DefaultSampleConverter.convert(cond), left.toSilExpression(state, quantifiedVar), right.toSilExpression(state, quantifiedVar))()
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
  def transformForgetVariable(variable: VariableIdentifier): PermissionTree = Condition(cond, left, right, forgottenVariables + variable)
  def toIntegerQuantificationConstraints(quantifiedVariable: VariableIdentifier): (Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression]) = {
    val leftConstraints = left.toIntegerQuantificationConstraints(quantifiedVariable)
    val rightConstraints = right.toIntegerQuantificationConstraints(quantifiedVariable)
    val replacements = getReplacementsForForgottenVariables(quantifiedVariable)
    (leftConstraints, rightConstraints) match { case ((constraints1, invariants1, forgottenVars1, additionalConstraints1), (constraints2, invariants2, forgottenVars2, additionalConstraints2)) =>
      (constraints1.map { case (constraint, perm) => (replaceVariables(and(constraint, cond), replacements), perm) } ++ constraints2.map { case (constraint, perm) => (replaceVariables(and(constraint, not(cond)), replacements), perm) }, (invariants1 ++ invariants2).map(replaceVariables(_, replacements)), (forgottenVars1 ++ forgottenVars2).map(replacer(replacements)), additionalConstraints1 ++ additionalConstraints2)
    }
  }
}

case object EmptyPermissionTree extends PermissionTree {
  def forgottenVariables: Set[Identifier] = Set()
  override def add(receiver: ExpressionDescription, permission: SimplePermission): PermissionTree = PermissionLeaf(receiver, permission)
  override def sub(receiver: ExpressionDescription, permission: FractionalPermission): PermissionTree = ZeroBoundedPermissionTree(PermissionAddition(Seq(PermissionLeaf(receiver, NegativePermission(permission)))))
  override def max(other: PermissionTree): PermissionTree = other
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp = ZeroPerm
  def getSetDescriptions(state: QuantifiedPermissionsState): Set[ReferenceSetDescription.Inner] = Set()
  def transformExpressions(f: (Expression) => Expression): PermissionTree = this
  def exists(f: (PermissionTree) => Boolean): Boolean = f(this)
  def foreach(f: (Expression => Unit)): Unit = {}
  def getReadAmounts: Set[(FractionalPermission, Int)] = Set()
  def simplifySyntactically: PermissionTree = this
  def hasRead: Boolean = false
  def simplifySemantically(state: QuantifiedPermissionsState): EmptyPermissionTree.type = this
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState) = true
  def transform(f: (PermissionTree => PermissionTree)): PermissionTree = f(this)
  def toIntegerQuantificationConstraints(quantifiedVariable: VariableIdentifier): (Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression]) = (Set((trueConst, noneConst)), Set(trueConst), Set(), Map())
  def transformForgetVariable(variable: VariableIdentifier): PermissionTree = this
}

case class IntegerQuantifiedPermissionTree(rootExpr: FunctionExpressionDescription, permissionExpression: Expression, permissionPlaceholder: FunctionCallExpression, forgottenPermissions: Map[FunctionCallExpression, Expression]) extends PermissionTree {
  def forgottenVariables: Set[Identifier] = Set()
  def ids: Set[Identifier] = permissionExpression.ids.toSetOrFail
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVar: sil.LocalVar): sil.Exp = throw new UnsupportedOperationException
  def toSilAssertions(quantifiedVarDecl: sil.LocalVarDecl, field: sil.Field): Seq[sil.Exp] = {
    val quantifiedVar = quantifiedVarDecl.localVar
    val funcApp = sil.FuncLikeApp(Context.functions(rootExpr.functionName), rootExpr.parameters.map {
      case Left(e) => DefaultSampleConverter.convert(e.expr)
      case Right(_) => quantifiedVar
    }, scala.Predef.Map[sil.TypeVar, sil.Type]())
    val access = sil.FieldAccessPredicate(sil.FieldAccess(funcApp, field)(), sil.FuncLikeApp(Context.functions(permissionPlaceholder.functionName), Seq(quantifiedVar), scala.Predef.Map()))()
    val permExp = sil.Forall(Seq(quantifiedVarDecl), Seq(), access)()
    var assertions = Seq[sil.Exp](permExp)
    assertions +:= sil.InhaleExhaleExp(sil.Forall(Seq(quantifiedVarDecl), Seq(), DefaultSampleConverter.convert(permissionExpression))(), sil.TrueLit()())()
    assertions ++:= forgottenPermissions.map(assertion => sil.InhaleExhaleExp(sil.Forall(Seq(quantifiedVarDecl), Seq(), DefaultSampleConverter.convert(assertion._2))(), sil.TrueLit()())())
    assertions ++:= forgottenPermissions.map(fun => sil.InhaleExhaleExp(sil.Forall(Seq(quantifiedVarDecl), Seq(), sil.PermGeCmp(sil.FuncLikeApp(Context.functions(fun._1.functionName), Seq(quantifiedVar), scala.Predef.Map()), sil.NoPerm()())())(), sil.TrueLit()())())
    assertions
  }
  def getSetDescriptions(state: QuantifiedPermissionsState): Set[Inner] = Set()
  def transform(f: (PermissionTree) => PermissionTree): PermissionTree = f(this)
  def exists(f: (PermissionTree) => Boolean): Boolean = f(this)
  def foreach(f: (Expression) => Unit): Unit = {
    rootExpr.foreach(f)
    permissionExpression.foreach(f)
  }
  def hasRead: Boolean = permissionExpression.contains {
    case VariableIdentifier(name, _) if name == Context.getRdAmountVariable.name => true
    case _ => false
  } || forgottenPermissions.values.exists(_.contains {
    case VariableIdentifier(name, _) if name == Context.getRdAmountVariable.name => true
    case _ => false
  })
  def getReadAmounts: Set[(FractionalPermission, Int)] = {
    var readAmounts: Set[(FractionalPermission, Int)] = Set()
    permissionExpression.foreach {
      case PermissionExpression(fractionalPermission, reads) => readAmounts += ((fractionalPermission, reads))
      case _ =>
    }
    readAmounts
  }
  def simplifySyntactically: PermissionTree = this
  def simplifySemantically(state: QuantifiedPermissionsState): PermissionTree = this
  def lessEqual(other: PermissionTree, state: QuantifiedPermissionsState): Boolean = false
  def transformForgetVariable(variable: VariableIdentifier): PermissionTree = this
  def toIntegerQuantificationConstraints(quantifiedVariable: VariableIdentifier): (Set[(Expression, Expression)], Set[Expression], Set[Identifier], Map[FunctionCallExpression, Expression]) =
    (Set((trueConst, permissionPlaceholder)), Set(), Set(), Map((permissionPlaceholder, permissionExpression)))
  override def toForgottenTree: IntegerQuantifiedPermissionTree = this
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

object ZeroPermission {
  def apply(): FractionalPermission = FractionalPermission(0, 1)
  def unapply(perm: Permission): Boolean = perm match {
    case FractionalPermission(0, _) => true
    case _ => false
  }
}

case class FractionalPermission(numerator: Int, denominator: Int) extends SimplePermission {
  require(denominator >= 1, "Denominator of a fractional permission must be greater than 0!")
  def toSilExpression: sil.FractionalPerm = sil.FractionalPerm(sil.IntLit(numerator)(), sil.IntLit(denominator)())()
  def toSampleExpression: Expression = FractionalPermissionExpression(numerator, denominator)
  def getReadPerm: (FractionalPermission, Int) = (this, 0)
  def <(other: FractionalPermission): Boolean = other match {
    case FractionalPermission(otherNumerator, otherDenominator) => numerator * otherDenominator < denominator * otherNumerator
  }
  def >(other: FractionalPermission): Boolean = other < this
  def <=(other: Permission): Boolean = this == other || (other match {
    case otherFrac: FractionalPermission => this <= otherFrac
    case NegativePermission(FractionalPermission(0, _)) | SymbolicReadPermission if numerator == 0 => true
    case WritePermission => numerator < denominator
    case _ => false
  })
  def lessEqual(other: Permission): Boolean = this <= other
  def +(other: FractionalPermission): FractionalPermission = other match {
    case FractionalPermission(_numerator, _denominator) => FractionalPermission(numerator * _denominator + _numerator * denominator, denominator * _denominator)
  }
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