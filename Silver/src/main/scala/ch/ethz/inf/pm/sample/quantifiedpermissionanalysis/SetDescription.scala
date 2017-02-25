/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, DomType, IntType, RefType}
import ch.ethz.inf.pm.sample.oorepresentation.{DummyProgramPoint, ProgramPoint, Type}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.QuantifiedPermissionsParameters._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.ReferenceSetDescription.ReferenceSetElementDescriptor.{AddField, Function, RootElement}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils.ExpressionBuilder._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.Utils._
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 03.12.16.
  */
sealed trait SetDescription[S <: SetDescription[S]] extends Lattice[S] {

  this: S =>

  def pp: ProgramPoint

  def expr: Expression

  def key: (ProgramPoint, Expression) = (pp, expr)

  def inner(pp: ProgramPoint, initExpression: Expression): SetDescription[S]

  def silverType: sil.Type

  def transformAssignField(receiver: Expression, field: String, right: Expression): S

  def transformAssignVariable(left: VariableIdentifier, right: Expression): S

  def transformCondition(cond: Expression): S

  def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp

  def isFinite(state: QuantifiedPermissionsState): Boolean

  def isOneElement: Boolean
}

object SetDescription {

  sealed trait Top[S <: SetDescription[S]] extends SetDescription[S] with Lattice.Top[S] {
    this: S =>

    def pp = throw new UnsupportedOperationException

    def expr = throw new UnsupportedOperationException

    def isFinite(state: QuantifiedPermissionsState): Boolean = false

    def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = sil.TrueLit()()

    def isOneElement = false
  }

  sealed trait Bottom[S <: SetDescription[S]] extends SetDescription[S] with Lattice.Bottom[S] {
    this: S =>

    def pp = throw new UnsupportedOperationException

    def expr = throw new UnsupportedOperationException

    def isFinite(state: QuantifiedPermissionsState): Boolean = throw new UnsupportedOperationException()

    def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = throw new UnsupportedOperationException()

    def isOneElement = throw new UnsupportedOperationException()
  }

  sealed trait Inner[S <: SetDescription[S], T <: Inner[S, T]] extends SetDescription[S] with Lattice.Inner[S, T] {
    this: S =>
  }
}

sealed trait ReferenceSetDescription extends SetDescription[ReferenceSetDescription] {

  def silverType: sil.Type = sil.Ref

  def isNullProhibited: Boolean

  def simplify: ReferenceSetDescription

  def factory(): ReferenceSetDescription = top()

  def transformAssignField(receiver: Expression, field: String, right: Expression): ReferenceSetDescription = this

  def transformAssignVariable(left: VariableIdentifier, right: Expression): ReferenceSetDescription = this

  def transformCondition(cond: Expression): ReferenceSetDescription = this

  def isEquivalentDescription(other: ReferenceSetDescription): Boolean

  /**
    * Generates an expression that checks whether a given quantified variable is contained in the set represented by
    * this description.
    *
    * @param quantifiedVariable The quantified variable to compare against.
    * @return A silver expression that checks whether the given quantified variable is in the set.
    */
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp

  def canBeExpressedByIntegerQuantification(state: QuantifiedPermissionsState): Boolean

  def toSetDefinition(state: QuantifiedPermissionsState): Seq[sil.Exp]

  def extractIntegerParameterExpression: (ProgramPoint, Expression)
}

object ReferenceSetDescription {

  trait ReferenceSetElementDescriptor

  object ReferenceSetElementDescriptor {
    case class RootElement(expr: Expression) extends ReferenceSetElementDescriptor

    case class AddField(field: String) extends ReferenceSetElementDescriptor

    case class Function(functionName: String, typ: Type, pp: ProgramPoint, parameters: Seq[(Type, ProgramPoint, Expression)]) extends ReferenceSetElementDescriptor
  }

  object Inner {
    def apply(pp: ProgramPoint, initExpr: Expression, positive: Boolean): ReferenceSetDescription.Inner =
      if (positive) PositiveReferenceSetDescription.Inner(pp, initExpr)
      else NegativeReferenceSetDescription.Inner(pp, initExpr)
  }

  sealed trait Top extends ReferenceSetDescription with SetDescription.Top[ReferenceSetDescription] {
    def isEquivalentDescription(other: ReferenceSetDescription): Boolean = other == top()

    def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = sil.TrueLit()()

    def canBeExpressedByIntegerQuantification(state: QuantifiedPermissionsState): Boolean = false

    def toSetDefinition(state: QuantifiedPermissionsState): Seq[sil.Exp] = Seq(sil.TrueLit()())

    def simplify: ReferenceSetDescription = this

    def extractIntegerParameterExpression: (ProgramPoint, Expression) = (DummyProgramPoint, trueConst)
  }

  sealed trait Bottom extends ReferenceSetDescription with SetDescription.Bottom[ReferenceSetDescription] {
    def isEquivalentDescription(other: ReferenceSetDescription): Boolean = other == bottom()

    def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = throw new UnsupportedOperationException()

    def canBeExpressedByIntegerQuantification(state: QuantifiedPermissionsState): Boolean = throw new UnsupportedOperationException()

    def toSetDefinition(state: QuantifiedPermissionsState): Seq[sil.Exp] = throw new UnsupportedOperationException()

    def simplify: ReferenceSetDescription = this

    def extractIntegerParameterExpression: (ProgramPoint, Expression) = throw new UnsupportedOperationException()
  }

  sealed trait Inner extends ReferenceSetDescription with SetDescription.Inner[ReferenceSetDescription, Inner] {

    def widened: Boolean

    def concreteExpressions: Set[(Expression, Boolean)]

    def copy(pp: ProgramPoint = pp,
             expr: Expression = expr,
             widened: Boolean = widened,
             concreteExpressions: Set[(Expression, Boolean)] = concreteExpressions): Inner

    def copy2(key: (ProgramPoint, Expression),
             widened: Boolean = widened,
             concreteExpressions: Set[(Expression, Boolean)] = concreteExpressions): Inner = copy(key._1, key._2, widened, concreteExpressions)

    override def isNullProhibited: Boolean = concreteExpressions.forall { case (_, b) => b }

    override def isEquivalentDescription(other: ReferenceSetDescription): Boolean = lessEqual(other) && other.lessEqual(this)

    private def isFunctionExprFinite(function: Function, state: QuantifiedPermissionsState): Boolean = function match {
      case Function(_, _, _, parameters) => parameters.forall {
        case ((_: RefType) | (_: DomType), _, expr) => state.refSets((pp, expr)).isFinite(state)
        case (IntType, _, _) => false
      }
    }

    override def isFinite(state: QuantifiedPermissionsState): Boolean = {
      (!widened && abstractExpressions.forall {
        case function: Function => isFunctionExprFinite(function, state)
        case _ => true
      }) || abstractExpressions.forall {
        case _: AddField => false
        case function: Function => isFunctionExprFinite(function, state)
        case _ => true
      }
    }

    override def isOneElement: Boolean = !widened && concreteExpressions.size == 1 && abstractExpressions.forall {
      case _: Function => false
      case _ => true
    }

    def canBeExpressedByIntegerQuantification(state: QuantifiedPermissionsState): Boolean = QuantifiedPermissionsParameters.useIntegerQuantification && !widened && abstractExpressions.forall {
      case Function(functionName, _, _, parameters) => parameters.count {
        case (IntType, _, _) => true
        case _ => false
      } == 1 && parameters.forall {
        case (IntType, _, expr) => Utils.isFunctionInjective(Context.functions(functionName), expr, Context.postNumericalInfo(pp).numDom)
        case (_, pp, expr) => state.refSets((pp, expr)).isOneElement
      }
      case _ => false
    }

    private def expand(state: QuantifiedPermissionsState, setDescription: ReferenceSetDescription): Set[(Expression, Seq[Expression])] = setDescription match {
      case inner: Inner =>
        inner.concreteExpressions.flatMap {
          case (FunctionCallExpression(functionName, parameters, typ, _), isNullGuarded) =>
            expandFunction(state, parameters.map(param => (param.typ, pp, param))).map { arguments =>
              val functionCall = FunctionCallExpression(functionName, arguments.unzip._1, typ, pp)
              val constraints: Seq[Expression] = arguments.unzip._2.flatten
              (functionCall, if (isNullGuarded) constraints :+ functionCall else constraints)
            }
          case (other, isNullGuarded) => Seq((other, if (isNullGuarded) Seq(other) else Seq[Expression]()))
        }
      case _ => throw new IllegalArgumentException()
    }

    private def expand(state: QuantifiedPermissionsState, key: (ProgramPoint, Expression)): Set[(Expression, Seq[Expression])] = expand(state, state.refSets(key))

    private def expandFunction(state: QuantifiedPermissionsState, parameters: Seq[(Type, ProgramPoint, Expression)]): Set[Seq[(Expression, Seq[Expression])]] = {
      val variablesToRemove = state.changingVars ++ state.declaredBelowVars
      if (parameters.isEmpty) Set[Seq[(Expression, Seq[Expression])]](Seq[(Expression, Seq[Expression])]())
      else expandFunction(state, parameters.init).flatMap(seq => parameters.last match {
        case (_: RefType | _: DomType, _, expr) => expand(state, (pp, expr)).map(expr => seq :+ expr)
        case (IntType, _, expr) =>
          val quantifiedVariable = VariableIdentifier(Context.getQuantifiedVarDecl(sil.Int).localVar.name)(IntType)
          val expressionToAssume = BinaryArithmeticExpression(quantifiedVariable, expr, ArithmeticOperator.==)
          Context.postNumericalInfo(pp).numDom.createVariable(quantifiedVariable).assume(expressionToAssume).removeVariables(variablesToRemove).getPossibleConstants(quantifiedVariable).toSetOrFail.map(constant => seq :+ (constant, Seq()))
      })
    }

    override def extractIntegerParameterExpression: (ProgramPoint, Expression) = (pp, key._2.find(_.typ == IntType).get)

    override def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = {
      concreteExpressions.foldLeft[Option[sil.Exp]](None) {
        case (None, (FunctionCallExpression(_, parameters, _, _), _)) =>
          parameters.foldLeft[Option[sil.Exp]](None) {
            case (None, e) if e.typ == IntType =>
              val quantifiedVariableIdentifier = VariableIdentifier(quantifiedVariable.name)(IntType)
//              Utils.toCNFConjuncts(forget(quantifiedVariableIdentifier, e))
//                .toSeq
//                .map(constraint => constraint.transform {
//                  // Reorder subexpressions in the constraints in order to make them more 'natural'. E.g. i >= 0 will be changed to 0 <= i
//                  case BinaryArithmeticExpression(`quantifiedVariableIdentifier`, right, ArithmeticOperator.>=) => BinaryArithmeticExpression(right, quantifiedVariableIdentifier, ArithmeticOperator.<=)
//                  case BinaryArithmeticExpression(`quantifiedVariableIdentifier`, right, ArithmeticOperator.>) => BinaryArithmeticExpression(right, quantifiedVariableIdentifier, ArithmeticOperator.<)
//                  case BinaryArithmeticExpression(left, `quantifiedVariableIdentifier`, ArithmeticOperator.<=) => BinaryArithmeticExpression(quantifiedVariableIdentifier, left, ArithmeticOperator.>=)
//                  case BinaryArithmeticExpression(left, `quantifiedVariableIdentifier`, ArithmeticOperator.<) => BinaryArithmeticExpression(quantifiedVariableIdentifier, left, ArithmeticOperator.>)
//                  case other => other
//                })
//                .sorted (new Ordering[Expression] {
//                  // Reorder the constraints, e.g. [10 >= i, i >= 0] together with the above map will be changed to [0 <= i, i <= 10] which looks like an interval and thus more intuitive
//                  override def compare(x: Expression, y: Expression): Int = (x, y) match {
//                    case (BinaryArithmeticExpression(_, `quantifiedVariableIdentifier`, _), _) => -1
//                    case (BinaryArithmeticExpression(`quantifiedVariableIdentifier`, _, _), _) => 1
//                    case _ => 0
//                  }
//                })
//                .map(constraint => DefaultSampleConverter.convert(constraint))
//                .reduceLeftOption((left, right) => sil.And(left, right)())
//              match {
//                case some: Some[_] => some
//                case None => Some(sil.TrueLit()())
//              }
              Some(DefaultSampleConverter.convert(forget(state, quantifiedVariableIdentifier, e)))
            case (Some(_), e) if e.typ == IntType => throw new IllegalStateException("Encountered two or more int arguments")
            case _ => None
          }
        case _ => throw new IllegalStateException("Encountered a concrete expression that is no function call! Integer quantification can only be applied for functions.")
      } match {
        case Some(exp) => exp
        case _ => throw new IllegalStateException("Integer quantification can only be applied if there is at least one int argument!")
      }
    }

    /**
      * Generates an expression that checks whether a given quantified variable is contained in the set represented by
      * this description.
      *
      * @param quantifiedVariable The quantified variable to compare against.
      * @return A silver expression that checks whether the given quantified variable is in the set.
      */
    override def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = {
      if (isFinite(state))
        expand(state, this).map { case (expr, constraints) =>
          val transformer: Expression => Expression = {
            case FieldExpression(typ, field, receiver) => FunctionCallExpression(Context.getFieldAccessFunction(field).name, Seq(receiver), typ)
            case other => other
          }
          (expr.transform(transformer), constraints.map(_.transform(transformer)))
        }.map {
          case (expr, constraints) => (sil.EqCmp(quantifiedVariable, DefaultSampleConverter.convert(expr))(), constraints.map(DefaultSampleConverter.convert))
        }.map {
          case (e, Nil) => e
          case (e, constraints) => sil.And(constraints.map(constraint => sil.NeCmp(constraint, sil.NullLit()())()).reduce[sil.Exp](sil.And(_, _)()), e)()
        }.reduce[sil.Exp](sil.Or(_, _)())
      else
        sil.AnySetContains(quantifiedVariable, Context.getSetFor(key).localVar)()
    }

    def forget(state: QuantifiedPermissionsState, variable: VariableIdentifier, exprToForget: Expression): Expression = exprToForget match {
      case _: Constant => BinaryArithmeticExpression(variable, exprToForget, ArithmeticOperator.==)
      case _ =>
        val numericalInfo: NumericalDomainType = Context.postNumericalInfo(pp).numDom
        val expressionToAssume = BinaryArithmeticExpression(variable, exprToForget, ArithmeticOperator.==)
        val eliminated =
          if (useQE) {
            val constraints = expressionToAssume +: numericalInfo.getConstraints(exprToForget.ids.toSetOrFail).toSeq
            QuantifierElimination.eliminate((exprToForget.ids.toSetOrFail ++ state.changingVars ++ state.declaredBelowVars).map { case varId: VariableIdentifier => varId }, constraints.reduce(and))
          }
          else None
        eliminated match {
          case None => numericalInfo.createVariable(variable).assume(expressionToAssume).removeVariables(exprToForget.ids.toSetOrFail ++ state.changingVars ++ state.declaredBelowVars).getConstraints(Set(variable)).reduceOption(and) match {
            case Some(exp) => exp
            case None => trueConst
          }
          case Some(exp) => exp
        }
    }

    override def toSetDefinition(state: QuantifiedPermissionsState): Seq[sil.Exp] = {
      val expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription] = state.refSets
      val set = Context.getSetFor(key).localVar
      if (isFinite(state) || canBeExpressedByIntegerQuantification(state)) null
      else {
        var roots: Seq[sil.Exp] = Seq()
        var fields: Seq[sil.Exp] = Seq()
        var functions: Seq[sil.Exp] = Seq()
        val quantifiedVariableForFields = Context.getQuantifiedVarDecl(silverType)
        val quantifiedVariableForFieldsVar = Context.getQuantifiedVarDecl(silverType).localVar
        abstractExpressions.foreach {
          case RootElement(root) =>
            var accessConjuncts: Seq[sil.Exp] = Seq()
            val silRoot = DefaultSampleConverter.convert(root)
            silRoot.foreach {
              case fa: sil.FieldAccess => accessConjuncts +:= sil.PermGtCmp(sil.CurrentPerm(fa)(), sil.NoPerm()())()
              case _ =>
            }
            roots :+= (sil.AnySetContains(silRoot, set)() match {
              case contains if isNullProhibited => sil.Implies((accessConjuncts :+ sil.NeCmp(silRoot, sil.NullLit()())()).reduceLeft(sil.And(_, _)()), contains)()
              case contains if accessConjuncts.nonEmpty => sil.Implies(accessConjuncts.reduceLeft(sil.And(_, _)()), contains)()
              case contains => contains
            })
          case AddField(field) =>
            val fieldAccess = sil.FieldAccess(quantifiedVariableForFieldsVar, sil.Field(field, silverType)())()
            val fieldAccessExpr =
              if (useFieldAccessFunctionsInSetDefinitions) sil.FuncApp(Context.getFieldAccessFunction(field), Seq(quantifiedVariableForFieldsVar))()
              else fieldAccess
            val contains = sil.AnySetContains(quantifiedVariableForFieldsVar, set)()
            var conjuncts: Seq[sil.Exp] = Seq(contains)
            if (QuantifiedPermissionsParameters.addReceiverNullCheckInSetDefinition) conjuncts :+= sil.And(sil.NeCmp(quantifiedVariableForFieldsVar, sil.NullLit()())(), contains)()
            if (!useFieldAccessFunctionsInSetDefinitions && usePermissionCheckInFieldAdd) conjuncts :+= sil.PermGtCmp(sil.CurrentPerm(fieldAccess)(), sil.NoPerm()())()
            if (isNullProhibited) conjuncts :+= sil.NeCmp(fieldAccessExpr, sil.NullLit()())()
            val triggers =
              if (QuantifiedPermissionsParameters.useCustomTriggerGeneration) Seq(sil.Trigger(Seq(contains, fieldAccessExpr))())
              else Seq()
            val forall = sil.Forall(Seq(quantifiedVariableForFields), triggers, sil.Implies(conjuncts.reduce(sil.And(_, _)()), sil.AnySetContains(fieldAccessExpr, set)())())()
            fields :+= forall
          case Function(functionName, _, _, argKeys) =>
            val function = Context.functions(functionName)
            var args: Seq[sil.LocalVarDecl] = Seq()
            var impliesLeftConjuncts: Seq[sil.Exp] = Seq()
            function.formalArgs.zip(argKeys).foreach { case (formalArg, (_, _, argExpr)) =>
              val arg = Context.getQuantifiedVarDecl(formalArg.typ, args.toSet)
              args :+= arg
              formalArg.typ match {
                case sil.Ref | _: sil.DomainType => impliesLeftConjuncts :+= expressions((pp, argExpr)).toSilExpression(state, arg.localVar)
                case sil.Int => DefaultSampleConverter.convert(forget(state, VariableIdentifier(arg.name)(IntType), argExpr))
                case _ => throw new IllegalStateException()
              }
            }
            val funcApp = sil.FuncLikeApp(function, args.map(arg => arg.localVar), Map())
            val setContains = sil.AnySetContains(funcApp, set)()
            if (isNullProhibited)
              impliesLeftConjuncts :+= sil.NeCmp(funcApp, sil.NullLit()())()
            val implies = sil.Implies(impliesLeftConjuncts.reduce((left, right) => sil.And(left, right)()), setContains)()
            functions :+= sil.Forall(args, Seq(), implies)()
        }
        roots ++ fields ++ functions
      }
    }

    private def transformFieldAssignRecursively(field: String, receiver: Expression, right: Expression, expr: Expression): Expression = expr match {
      case ConditionalExpression(cond, thenExpr, elseExpr, typ) => ConditionalExpression(transformFieldAssignRecursively(field, receiver, right, cond), transformFieldAssignRecursively(field, receiver, right, thenExpr), transformFieldAssignRecursively(field, receiver, right, elseExpr), typ)
      case FieldExpression(typ, `field`, rec) =>
        if (receiver.equals(rec)) right
        else ConditionalExpression(BinaryArithmeticExpression(rec, receiver, ArithmeticOperator.==), right, FieldExpression(typ, field, transformFieldAssignRecursively(field, receiver, right, rec)), right.typ)
      case BinaryArithmeticExpression(l, r, op) => BinaryArithmeticExpression(transformFieldAssignRecursively(field, receiver, right, l), transformFieldAssignRecursively(field, receiver, right, r), op)
      case FieldExpression(_, otherField, rec) => FieldExpression(right.typ, otherField, transformFieldAssignRecursively(field, receiver, right, rec))
      case _ => expr
    }

    private def transformVariableAssignRecursively(left: VariableIdentifier, right: Expression, expr: Expression): Expression = expr match {
      case ConditionalExpression(cond, thenExpr, elseExpr, typ) => ConditionalExpression(transformVariableAssignRecursively(left, right, cond), transformVariableAssignRecursively(left, right, thenExpr), transformVariableAssignRecursively(left, right, elseExpr), typ)
      case FieldExpression(typ, field, rec) => FieldExpression(typ, field, transformVariableAssignRecursively(left, right, rec))
      case BinaryArithmeticExpression(l, r, op) => BinaryArithmeticExpression(transformVariableAssignRecursively(left, right, l), transformVariableAssignRecursively(left, right, r), op)
      case varId: VariableIdentifier if varId.equals(left) => right
      case _ => expr
    }

    override def transformAssignField(receiver: Expression, field: String, right: Expression): ReferenceSetDescription = {
      val newConcreteExpressions = concreteExpressions.map { case (expr, b) => (transformFieldAssignRecursively(field, receiver, right, expr), b) }
      copy(concreteExpressions = newConcreteExpressions)
    }

    override def transformAssignVariable(left: VariableIdentifier, right: Expression): ReferenceSetDescription = {
      val newConcreteExpressions = concreteExpressions.map { case (expr, b) => (transformVariableAssignRecursively(left, right, expr), b) }
      copy(concreteExpressions = newConcreteExpressions)
    }

    override def transformCondition(cond: Expression): ReferenceSetDescription = {
      val condCNF = Utils.toCNFConjuncts(cond).filter {
        case ReferenceComparisonExpression(Constant("null", _, _), _, _) => true
        case ReferenceComparisonExpression(_, Constant("null", _, _), _) => true
        case _ => false
      }.map {
        case ReferenceComparisonExpression(n@Constant("null", _, _), right, op) => ReferenceComparisonExpression(right, n, op)
        case other => other
      }
      val newConcreteExpressions: Set[(Expression, Boolean)] = concreteExpressions.map {
        case (expr, b) if !b =>
          if (condCNF.exists {
            case ReferenceComparisonExpression(`expr`, Constant("null", _, _), ReferenceOperator.!=) => true
            case _ => false
          }) (expr, true)
          else (expr, b)
        case other => other
      }
      copy(concreteExpressions = newConcreteExpressions)
    }

    private def extractRules(expr: Expression): Set[ReferenceSetElementDescriptor] = expr match {
      case ConditionalExpression(_, left, right, _) => extractRules(left) ++ extractRules(right)
      case FieldExpression(_, field, receiver) => extractRules(receiver) + AddField(field)
      case id: VariableIdentifier => Set(RootElement(id))
      case FunctionCallExpression(functionName, parameters, returnType, _) => Set(Function(functionName, returnType, pp, parameters.map(expr => (expr.typ, pp, expr))))
    }

    final def abstractExpressions: Set[ReferenceSetElementDescriptor] = concreteExpressions.flatMap { case (concreteExpression, _) => extractRules(concreteExpression) }

    def simplify: Inner = copy(concreteExpressions = concreteExpressions.map { case (expr, b) => (Utils.simplifyExpression(expr), b) })
  }
}

sealed trait NegativeReferenceSetDescription extends ReferenceSetDescription {

  override def top() = NegativeReferenceSetDescription.Top

  override def bottom() = NegativeReferenceSetDescription.Bottom

  override def inner(pp: ProgramPoint, initExpr: Expression) = NegativeReferenceSetDescription.Inner(pp, initExpr)
}

object NegativeReferenceSetDescription {

  case object Top extends ReferenceSetDescription.Top with NegativeReferenceSetDescription {
    override def isNullProhibited = false
  }

  case object Bottom extends ReferenceSetDescription.Bottom with NegativeReferenceSetDescription {
    override def isNullProhibited = true
  }

  object Inner {
    def apply(pp: ProgramPoint, initExpression: Expression) = new Inner(pp, initExpression)
  }

  case class Inner(pp: ProgramPoint, expr: Expression, widened: Boolean = false, concreteExpressions: Set[(Expression, Boolean)] = Set())
      extends ReferenceSetDescription.Inner with NegativeReferenceSetDescription {
    def this(key: (ProgramPoint, Expression)) = this(key._1, key._2, concreteExpressions = Set((key._2, false)))

    override def copy(pp: ProgramPoint = pp,
                      expr: Expression = expr,
                      widened: Boolean = widened,
                      concreteExpressions: Set[(Expression, Boolean)] = concreteExpressions): Inner =
      Inner(pp, expr, widened, concreteExpressions)

    override def isEquivalentDescription(other: ReferenceSetDescription): Boolean =
      other.isInstanceOf[NegativeReferenceSetDescription] && super.isEquivalentDescription(other)

    override def lubInner(other: ReferenceSetDescription.Inner): Inner =
      copy(
        widened = widened && other.widened,
        concreteExpressions = concreteExpressions & other.concreteExpressions
      )

    override def glbInner(other: ReferenceSetDescription.Inner): Inner =
      copy(
        widened = widened || other.widened,
        concreteExpressions = concreteExpressions ++ other.concreteExpressions
      )

    override def wideningInner(other: ReferenceSetDescription.Inner): Inner = glbInner(other).copy(widened = true)

    override def lessEqualInner(other: ReferenceSetDescription.Inner): Boolean =
      other.abstractExpressions.subsetOf(abstractExpressions) && (!isNullProhibited || other.isNullProhibited) &&
        (widened || other.concreteExpressions.subsetOf(concreteExpressions))
  }
}

sealed trait PositiveReferenceSetDescription extends ReferenceSetDescription {

  override def top() = PositiveReferenceSetDescription.Top

  override def bottom() = PositiveReferenceSetDescription.Bottom

  override def inner(pp: ProgramPoint, initExpr: Expression) = PositiveReferenceSetDescription.Inner(pp, initExpr)
}

object PositiveReferenceSetDescription {

  case object Top extends ReferenceSetDescription.Top with PositiveReferenceSetDescription {
    override def isNullProhibited = true
  }

  case object Bottom extends ReferenceSetDescription.Bottom with PositiveReferenceSetDescription {
    override def isNullProhibited = false
  }

  object Inner {
    def apply(pp: ProgramPoint, initExpression: Expression) = new Inner(pp, initExpression)
  }

  case class Inner(pp: ProgramPoint, expr: Expression, widened: Boolean = false, concreteExpressions: Set[(Expression, Boolean)] = Set())
    extends ReferenceSetDescription.Inner with PositiveReferenceSetDescription {

    def this(pp: ProgramPoint, initExpression: Expression) = this(pp, initExpression, concreteExpressions = Set((initExpression, false)))

    override def copy(pp: ProgramPoint = pp,
                      expr: Expression = expr,
                      widened: Boolean = widened,
                      concreteExpressions: Set[(Expression, Boolean)] = concreteExpressions): Inner =
      Inner(pp, expr, widened, concreteExpressions)

    override def isEquivalentDescription(other: ReferenceSetDescription): Boolean =
      other.isInstanceOf[PositiveReferenceSetDescription] && super.isEquivalentDescription(other)

    override def lubInner(other: ReferenceSetDescription.Inner): Inner =
      copy(
        widened = widened || other.widened,
        concreteExpressions = (concreteExpressions ++ other.concreteExpressions).filter {
          case (concreteExpr, true) => !concreteExpressions.contains((concreteExpr, false)) && !other.concreteExpressions.contains((concreteExpr, false))
          case _ => true
        }
      )

    override def glbInner(other: ReferenceSetDescription.Inner): Inner =
      copy(
        widened = widened && other.widened,
        concreteExpressions = concreteExpressions & other.concreteExpressions
      )

    override def wideningInner(other: ReferenceSetDescription.Inner): Inner = lubInner(other).copy(widened = true)

    override def lessEqualInner(other: ReferenceSetDescription.Inner): Boolean =
      abstractExpressions.subsetOf(other.abstractExpressions) && (isNullProhibited || !other.isNullProhibited) &&
        (other.widened || concreteExpressions.subsetOf(other.concreteExpressions))
  }
}

sealed trait IntegerSetDescription extends SetDescription[IntegerSetDescription] {

  def silverType: sil.Type = sil.Int

  def transformAssignField(receiver: Expression, field: String, right: Expression): IntegerSetDescription = this

  def transformAssignVariable(left: VariableIdentifier, right: Expression): IntegerSetDescription = this

  def transformCondition(cond: Expression): IntegerSetDescription = this

}

object IntegerSetDescription {

  sealed trait Top extends IntegerSetDescription with SetDescription.Top[IntegerSetDescription]

  sealed trait Bottom extends IntegerSetDescription with SetDescription.Bottom[IntegerSetDescription]

  sealed trait Inner extends IntegerSetDescription with SetDescription.Inner[IntegerSetDescription, Inner]

}

sealed trait PositiveIntegerSetDescription extends IntegerSetDescription {

  override def top() = PositiveIntegerSetDescription.Top

  override def bottom() = PositiveIntegerSetDescription.Bottom

  override def factory(): IntegerSetDescription = top()

  override def inner(pp: ProgramPoint, initExpr: Expression) = PositiveIntegerSetDescription.Inner(pp, initExpr, Context.preNumericalInfo(pp).numDom.getConstraints(initExpr.ids.toSetOrFail))

}

object PositiveIntegerSetDescription {
  case object Top extends IntegerSetDescription.Top with PositiveIntegerSetDescription

  case object Bottom extends IntegerSetDescription.Bottom with PositiveIntegerSetDescription

  case class Inner(pp: ProgramPoint, expr: Expression, constraints: Set[Expression]) extends IntegerSetDescription.Inner with PositiveIntegerSetDescription {

    def ids: Set[Identifier] = constraints.flatMap(_.ids.toSetOrFail)

    override def transformAssignField(receiver: Expression, field: String, right: Expression): IntegerSetDescription = this

    override def transformAssignVariable(left: VariableIdentifier, right: Expression): IntegerSetDescription = ???

    override def transformCondition(cond: Expression): IntegerSetDescription = ???

    def forget(quantifiedVariable: sil.LocalVar, variablesToForget: Set[VariableIdentifier]): Inner = Inner(pp, expr, QuantifierElimination.eliminate(variablesToForget, constraints).get)

    def toIntegerQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = DefaultSampleConverter.convert(constraints.reduce(and))

    def isFinite(state: QuantifiedPermissionsState): Boolean = isOneElement

    def isOneElement: Boolean = expr match {
      case _: Constant => true
      case _ => false
    }

    def lubInner(other: IntegerSetDescription.Inner): PositiveIntegerSetDescription = Top

    def glbInner(other: IntegerSetDescription.Inner): PositiveIntegerSetDescription = Bottom

    def wideningInner(other: IntegerSetDescription.Inner): PositiveIntegerSetDescription = Top

    def lessEqualInner(other: IntegerSetDescription.Inner): Boolean = false
  }
}