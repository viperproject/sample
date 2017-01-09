/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.{DefaultSampleConverter, DomType, IntType, RefType}
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.ReferenceSetDescription.ReferenceSetElementDescriptor.{AddField, Function, RootElement}
import viper.silver.ast.{Exp, LocalVar}
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 03.12.16.
  */
sealed trait SetDescription[S <: SetDescription[S]] extends Lattice[S] {

  this: S =>

  def silverType: sil.Type

  def transformAssignField(receiver: Expression, field: String, right: Expression): S

  def transformAssignVariable(left: VariableIdentifier, right: Expression): S

  /**
    * Generates an expression that checks whether a given quantified variable is contained in the set represented by
    * this description.
    *
    * @param quantifiedVariable The quantified variable to compare against.
    * @return A silver expression that checks whether the given quantified variable is in the set.
    */
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp

  def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp

  def isFinite(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean

  def isOneElement: Boolean

  def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean

  def toSetDefinition(state: QuantifiedPermissionsState): sil.Exp
}

object SetDescription {

  sealed trait Top[S <: SetDescription[S]] extends SetDescription[S] with Lattice.Top[S] {
    this: S =>

    override def toSetDefinition(state: QuantifiedPermissionsState): Exp = sil.TrueLit()()

    override def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: LocalVar): Exp = sil.TrueLit()()

    override def isFinite(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = false

    override def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = sil.TrueLit()()

    override def isOneElement = false

    def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = false
  }

  sealed trait Bottom[S <: SetDescription[S]] extends SetDescription[S] with Lattice.Bottom[S] {
    this: S =>

    override def toSetDefinition(state: QuantifiedPermissionsState): Exp = throw new UnsupportedOperationException()

    override def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: LocalVar): Exp = throw new UnsupportedOperationException()

    override def isFinite(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = throw new UnsupportedOperationException()

    override def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = throw new UnsupportedOperationException()

    override def isOneElement = throw new UnsupportedOperationException()

    def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = throw new UnsupportedOperationException()
  }

  sealed trait Inner[S <: SetDescription[S], T <: Inner[S, T]] extends SetDescription[S] with Lattice.Inner[S, T] {
    this: S =>
  }
}

sealed trait ReferenceSetDescription extends SetDescription[ReferenceSetDescription] {

  def silverType: sil.Type = sil.Ref

  override def factory(): ReferenceSetDescription = top()

  override def top() = ReferenceSetDescription.Top

  override def bottom() = ReferenceSetDescription.Bottom

  override def transformAssignField(receiver: Expression, field: String, right: Expression): ReferenceSetDescription = this

  override def transformAssignVariable(left: VariableIdentifier, right: Expression): ReferenceSetDescription = this
}

object ReferenceSetDescription {

  trait ReferenceSetElementDescriptor

  object ReferenceSetElementDescriptor {
    case class RootElement(expr: Expression) extends ReferenceSetElementDescriptor

    case class AddField(field: String) extends ReferenceSetElementDescriptor

    case class Function(functionName: String, typ: Type, pp: ProgramPoint, parameters: Seq[(Type, ProgramPoint, Expression)]) extends ReferenceSetElementDescriptor
  }

  case object Top extends ReferenceSetDescription with SetDescription.Top[ReferenceSetDescription]

  case object Bottom extends ReferenceSetDescription with SetDescription.Bottom[ReferenceSetDescription]

  object Inner {
    def apply(pp: ProgramPoint, initExpression: Expression) = new Inner(pp, initExpression)
  }

  case class Inner(key: (ProgramPoint, Expression), widened: Boolean = false, concreteExpressions: Set[Expression] = Set())
    extends ReferenceSetDescription with SetDescription.Inner[ReferenceSetDescription, Inner] {

    def this(pp: ProgramPoint, initExpression: Expression) = this((pp, initExpression), concreteExpressions = Set(initExpression))

    def copy(widened: Boolean = widened, concreteExpressions: Set[Expression] = concreteExpressions): Inner = Inner(key, widened, concreteExpressions)

    private def pp: ProgramPoint = key._1

    private def isFunctionExprFinite(function: Function, expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = function match {
      case Function(_, _, _, parameters) => parameters.forall {
        case (_: RefType, _, expr) => expressions((pp, expr)).isFinite(expressions)
        case (_: DomType, _, expr) => expressions((pp, expr)).isFinite(expressions)
        case (IntType, _, expr) => isIntegerExprFinite(expr, Context.postNumericalInfo(pp).numDom)
      }
    }

    private def isIntegerExprFinite(expr: Expression, numericalInfo: Context.NumericalDomainType): Boolean = {
      val quantifiedVar = VariableIdentifier(Context.getQuantifiedVarDecl(sil.Int).name)(IntType)
      val tempNum = numericalInfo.createVariable(quantifiedVar).assume(BinaryArithmeticExpression(quantifiedVar, expr, ArithmeticOperator.==))
      tempNum.getPossibleConstants(quantifiedVar) match {
        case set if set.isBottom => false
        case set if set.isTop => false
        case _ => true
      }
    }

    override def isFinite(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = {
      (!widened && abstractExpressions.forall {
        case function: Function => isFunctionExprFinite(function, expressions)
        case _ => true
      }) || abstractExpressions.forall {
        case _: AddField => false
        case function: Function => isFunctionExprFinite(function, expressions)
        case _ => true
      }
    }

    override def isOneElement: Boolean = !widened && concreteExpressions.size == 1 && abstractExpressions.forall {
      case _: Function => false
      case _ => true
    }

    def canBeExpressedByIntegerQuantification(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = !widened && abstractExpressions.forall {
      case Function(functionName, _, _, parameters) => parameters.count {
        case (IntType, _, _) => true
        case _ => false
      } == 1 && parameters.forall {
        case (IntType, _, expr) => Utils.isFunctionInjective(Context.functions(functionName), expr, Context.postNumericalInfo(pp).numDom)
        case (_, pp, expr) => expressions((pp, expr)).isOneElement
      }
      case _ => false
    }

    private def expand(state: QuantifiedPermissionsState, setDescription: ReferenceSetDescription): Set[Expression] = setDescription match {
      case Inner(_, _, concreteExprs) =>
        concreteExprs.flatMap {
          case FunctionCallExpression(functionName, parameters, typ, _) =>
            expandFunction(state, parameters.map(param => (param.typ, pp, param))).map(arguments => FunctionCallExpression(functionName, arguments, typ, pp)).toSet[Expression]
          case other => Set(other)
        }
      case _ => throw new IllegalArgumentException()
    }

    private def expand(state: QuantifiedPermissionsState, key: (ProgramPoint, Expression)): Set[Expression] = expand(state, state.refSets(key))

    private def expandFunction(state: QuantifiedPermissionsState, parameters: Seq[(Type, ProgramPoint, Expression)]): Set[Seq[Expression]] = {
      val variablesToRemove = state.changingVars ++ state.declaredBelowVars
      if (parameters.isEmpty) Set(Seq())
      else expandFunction(state, parameters.init).flatMap(seq => parameters.last match {
        case (_: RefType | _: DomType, _, expr) =>
          expand(state, (pp, expr)).map(expr => seq :+ expr)
        case (IntType, _, expr) =>
          val quantifiedVariable = VariableIdentifier(Context.getQuantifiedVarDecl(sil.Int).localVar.name)(IntType)
          val expressionToAssume = BinaryArithmeticExpression(quantifiedVariable, expr, ArithmeticOperator.==)
          Context.postNumericalInfo(pp).numDom.createVariable(quantifiedVariable).assume(expressionToAssume).removeVariables(variablesToRemove).getPossibleConstants(quantifiedVariable).toSetOrFail.map(constant => seq :+ constant)
      })
    }

    override def toParameterQuantification(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar): sil.Exp = {
      concreteExpressions.foldLeft[Option[sil.Exp]](None) {
        case (None, FunctionCallExpression(_, parameters, _, _)) =>
          parameters.foldLeft[Option[sil.Exp]](None) {
            case (None, e) if e.typ == IntType =>
              val numericalInfo = Context.postNumericalInfo(pp).numDom
              val quantifiedVariableIdentifier = VariableIdentifier(quantifiedVariable.name)(IntType)
              val expressionToAssume = BinaryArithmeticExpression(quantifiedVariableIdentifier, e, ArithmeticOperator.==)
              numericalInfo
                .createVariable(quantifiedVariableIdentifier)
                .assume(expressionToAssume)
                .removeVariables(state.changingVars ++ state.declaredBelowVars)
                .getConstraints(Set(quantifiedVariableIdentifier))
                .map(constraint => constraint.transform {
                  // Reorder subexpressions in the constraints in order to make them more 'natural'. E.g. i >= 0 will be changed to 0 <= i
                  case BinaryArithmeticExpression(`quantifiedVariableIdentifier`, right, ArithmeticOperator.>=) => BinaryArithmeticExpression(right, quantifiedVariableIdentifier, ArithmeticOperator.<=)
                  case BinaryArithmeticExpression(`quantifiedVariableIdentifier`, right, ArithmeticOperator.>) => BinaryArithmeticExpression(right, quantifiedVariableIdentifier, ArithmeticOperator.<)
                  case BinaryArithmeticExpression(left, `quantifiedVariableIdentifier`, ArithmeticOperator.<=) => BinaryArithmeticExpression(quantifiedVariableIdentifier, left, ArithmeticOperator.>=)
                  case BinaryArithmeticExpression(left, `quantifiedVariableIdentifier`, ArithmeticOperator.<) => BinaryArithmeticExpression(quantifiedVariableIdentifier, left, ArithmeticOperator.>)
                  case other => other
                })
                .toSeq
                .sorted (new Ordering[Expression] {
                  // Reorder the constraints, e.g. [10 >= i, i >= 0] together with the above map will be changed to [0 <= i, i <= 10]
                  override def compare(x: Expression, y: Expression): Int = (x, y) match {
                    case (BinaryArithmeticExpression(_, `quantifiedVariableIdentifier`, _), _) => -1
                    case (BinaryArithmeticExpression(`quantifiedVariableIdentifier`, _, _), _) => 1
                    case _ => 0
                  }
                })
                .map(constraint => DefaultSampleConverter.convert(constraint))
                .reduceLeftOption((left, right) => sil.And(left, right)())
              match {
                case Some(expr) => Some(expr)
                case None => Some(sil.TrueLit()())
              }
            case (Some(_), e) if e.typ == IntType => throw new IllegalStateException("Encountered two or more int arguments")
            case _ => None
          }
        case _ => throw new IllegalStateException()
      } match {
        case Some(exp) => exp
        case _ => throw new IllegalStateException()
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
      if (isFinite(state.refSets))
        expand(state, this).map (expr => expr.transform {
          case FieldExpression(typ, field, receiver) =>
            if (!Context.fieldAccessFunctions.contains(field)) {
              val fun = sil.Function(Context.createNewUniqueFunctionIdentifier("get_" + field), Seq(sil.LocalVarDecl("x", sil.Ref)()), DefaultSampleConverter.convert(typ), Seq(), Seq(), None)()
              Context.fieldAccessFunctions.put(field, fun)
              Context.auxiliaryFunctions.put(fun.name, fun)
            }
            FunctionCallExpression(Context.fieldAccessFunctions(field).name, Seq(receiver), typ)
          case other => other
        }).map(expr => sil.EqCmp(quantifiedVariable, DefaultSampleConverter.convert(expr))()).reduce[sil.Exp]((left, right) => sil.Or(left, right)())
      else
        sil.AnySetContains(quantifiedVariable, Context.getSetFor(key).localVar)()
    }

    override def toSetDefinition(state: QuantifiedPermissionsState): sil.Exp = {
      val expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription] = state.refSets
      val set = Context.getSetFor(key).localVar
      if (isFinite(expressions) || canBeExpressedByIntegerQuantification(expressions)) null
      else {
        var roots: Set[sil.AnySetContains] = Set()
        var fields: Set[sil.AnySetContains] = Set()
        var functions: Set[sil.Exp] = Set()
        val quantifiedVariableForFields = Context.getQuantifiedVarDecl(silverType)
        abstractExpressions.foreach {
          case RootElement(root) => roots += sil.AnySetContains(DefaultSampleConverter.convert(root), set)()
          case AddField(field) =>
            fields += sil.AnySetContains(sil.FieldAccess(quantifiedVariableForFields.localVar, sil.Field(field, sil.Ref)())(), set)()
          case Function(functionName, _, _, argKeys) =>
            val function = Context.functions(functionName)
            var args: Seq[sil.LocalVarDecl] = Seq()
            var impliesLeftConjuncts: Seq[sil.Exp] = Seq()
            val numericalInfo: Context.NumericalDomainType = Context.postNumericalInfo(pp).numDom
            function.formalArgs.zip(argKeys).foreach { case (formalArg, (_, _, argExpr)) =>
              val arg = Context.getQuantifiedVarDecl(formalArg.typ, args.toSet)
              args :+= arg
              formalArg.typ match {
                case sil.Ref | _: sil.DomainType =>
                  impliesLeftConjuncts :+= expressions((pp, argExpr)).toSilExpression(state, arg.localVar)
                  // TODO: do we want to include this restriction? Consequence: Not necessarily sound anymore!
                  //              if (formalArg.typ.equals(sil.Ref))
                  //                impliesLeftConjuncts :+= sil.NeCmp(arg.localVar, sil.NullLit()())()
                case sil.Int =>
                  val variableIdentifier = VariableIdentifier(arg.name)(IntType)
                  val expressionToAssume = BinaryArithmeticExpression(variableIdentifier, argExpr, ArithmeticOperator.==)
                  val tempNumericalInfo = numericalInfo.createVariable(variableIdentifier).assume(expressionToAssume).removeVariables(state.changingVars ++ state.declaredBelowVars)
                  tempNumericalInfo.getConstraints(Set(variableIdentifier)).map(expr => DefaultSampleConverter.convert(expr)).reduceOption((left, right) => sil.And(left, right)()) match {
                    case Some(exp) => impliesLeftConjuncts :+= exp
                    case None =>
                  }
                case _ => throw new IllegalStateException()
              }
            }
            val funcApp = sil.FuncLikeApp(function, args.map(arg => arg.localVar), Map())
            val setContains = sil.AnySetContains(funcApp, set)()
            val implies = sil.Implies(impliesLeftConjuncts.reduce((left, right) => sil.And(left, right)()), setContains)()
            functions += sil.Forall(args, Seq(), implies)()
        }
        var conjuncts: Seq[sil.Exp] = Seq()
        roots.reduceOption[sil.Exp]((left, right) => sil.And(left, right)()) match {
          case Some(exp) => conjuncts :+= exp
          case None =>
        }
        fields.reduceOption[sil.Exp]((left, right) => sil.And(left, right)()) match {
          case Some(exp) => conjuncts :+= sil.Forall(Seq(quantifiedVariableForFields), Seq(), sil.Implies(sil.AnySetContains(quantifiedVariableForFields.localVar, set)(), exp)())()
          case None =>
        }
        conjuncts ++= functions
        conjuncts.reduce((left, right) => sil.And(left, right)())
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
      val newConcreteExpressions = concreteExpressions.map(expr => transformFieldAssignRecursively(field, receiver, right, expr))
      copy(concreteExpressions = newConcreteExpressions)
    }

    override def transformAssignVariable(left: VariableIdentifier, right: Expression): ReferenceSetDescription = {
      val newConcreteExpressions = concreteExpressions.map(expr => transformVariableAssignRecursively(left, right, expr))
      copy(concreteExpressions = newConcreteExpressions)
    }

    private def abstractExpressions: Set[ReferenceSetElementDescriptor] = {
      concreteExpressions.flatMap(concreteExpression => extractRules(concreteExpression))
    }

    private def extractRules(expr: Expression): Set[ReferenceSetElementDescriptor] = expr match {
      case ConditionalExpression(_, left, right, _) => extractRules(left) ++ extractRules(right)
      case FieldExpression(_, field, receiver) => extractRules(receiver) + AddField(field)
      case id: VariableIdentifier => Set(RootElement(id))
      case FunctionCallExpression(functionName, parameters, returnType, _) => Set(Function(functionName, returnType, pp, parameters.map(expr => (expr.typ, pp, expr))))
    }

    override def lubInner(other: Inner): Inner =
      copy(
        widened = widened || other.widened,
        concreteExpressions = concreteExpressions ++ other.concreteExpressions
      )

    override def glbInner(other: Inner): Inner =
      copy(
        widened = widened && other.widened,
        concreteExpressions = concreteExpressions & other.concreteExpressions
      )

    override def wideningInner(other: Inner): Inner = lubInner(other).copy(widened = true)

    override def lessEqualInner(other: Inner): Boolean =
      abstractExpressions.subsetOf(other.abstractExpressions) &&
        ((widened && other.widened) || concreteExpressions.subsetOf(other.concreteExpressions))
  }
}