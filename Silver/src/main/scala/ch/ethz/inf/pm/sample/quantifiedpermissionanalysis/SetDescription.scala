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
  def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar = Context.getQuantifiedVarDecl(silverType).localVar): sil.Exp

  def isFinite(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean

  def toSetDefinition(state: QuantifiedPermissionsState): sil.Exp
}

object SetDescription {

  sealed trait Top[S <: SetDescription[S]] extends SetDescription[S] with Lattice.Top[S] {
    this: S =>

    override def toSetDefinition(state: QuantifiedPermissionsState): Exp = sil.TrueLit()()

    override def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: LocalVar): Exp = sil.TrueLit()()

    override def isFinite(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = false
  }

  sealed trait Bottom[S <: SetDescription[S]] extends SetDescription[S] with Lattice.Bottom[S] {
    this: S =>

    override def toSetDefinition(state: QuantifiedPermissionsState): Exp = throw new UnsupportedOperationException()

    override def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: LocalVar): Exp = throw new UnsupportedOperationException()

    override def isFinite(expressions: Map[(ProgramPoint, Expression), ReferenceSetDescription]): Boolean = throw new UnsupportedOperationException()
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

    private def isFinite(expr: Expression, numericalInfo: Context.NumType): Boolean = {
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
        case Function(_, _, _, parameters) => parameters.forall {
          case (_: RefType, _, expr) => expressions((pp, expr)).isFinite(expressions)
          case (_: DomType, _, expr) => expressions((pp, expr)).isFinite(expressions)
          case (IntType, _, expr) => isFinite(expr, Context.postNumericalInfo(pp).numDom)
        }
        case _ => true
      }) || abstractExpressions.forall {
        case _: AddField => false
        case Function(_, _, _, parameters) => parameters.forall {
          case (_: RefType, _, expr) => expressions((pp, expr)).isFinite(expressions)
          case (_: DomType, _, expr) => expressions((pp, expr)).isFinite(expressions)
          case (IntType, _, expr) => isFinite(expr, Context.postNumericalInfo(pp).numDom)
        }
        case _ => true
      }
    }

    private def expand(state: QuantifiedPermissionsState, setDescription: ReferenceSetDescription): Set[Expression] = setDescription match {
      case Inner(_, _, expressions) =>
        expressions.flatMap(expr => expr match {
          case FunctionCallDescription(functionName, parameters, typ, _) =>
            expandFunction(state, parameters.map { case (t, e) => (t, pp, e) }).map(arguments => FunctionCallExpression(functionName, arguments, typ, pp)).toSet[Expression]
          case _ => Set(expr)
        })
      case _ => throw new IllegalArgumentException()
    }

    private def expand(state: QuantifiedPermissionsState, key: (ProgramPoint, Expression)): Set[Expression] = expand(state, state.refSets(key))

    private def expandFunction(state: QuantifiedPermissionsState, parameters: Seq[(Type, ProgramPoint, Expression)]): Set[Seq[Expression]] = {
      val changingVars = state.changingVars
      if (parameters.isEmpty) Set(Seq())
      else expandFunction(state, parameters.init).flatMap(seq => parameters.last match {
        case (_: RefType | _: DomType, _, expr) =>
          expand(state, (pp, expr)).map(expr => seq :+ expr)
        case (IntType, _, expr) =>
          val quantifiedVariable = VariableIdentifier(Context.getQuantifiedVarDecl(sil.Int).localVar.name)(IntType)
          val expressionToAssume = BinaryArithmeticExpression(quantifiedVariable, expr, ArithmeticOperator.==)
          Context.postNumericalInfo(pp).numDom.createVariable(quantifiedVariable).assume(expressionToAssume).removeVariables(changingVars).getPossibleConstants(quantifiedVariable).toSetOrFail.map(constant => seq :+ constant)
      })
    }

    /**
      * Generates an expression that checks whether a given quantified variable is contained in the set represented by
      * this description.
      *
      * @param quantifiedVariable The quantified variable to compare against.
      * @return A silver expression that checks whether the given quantified variable is in the set.
      */
    override def toSilExpression(state: QuantifiedPermissionsState, quantifiedVariable: sil.LocalVar = Context.getQuantifiedVarDecl(silverType).localVar): sil.Exp = {
      val expressions = state.refSets
      if (isFinite(expressions))
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
      if (isFinite(expressions)) null
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
            val numericalInfo: Context.NumType = Context.postNumericalInfo(pp).numDom
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
                  val tempNumericalInfo = numericalInfo.createVariable(variableIdentifier).assume(expressionToAssume).removeVariables(state.changingVars)
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

    private def transformAssignmentRecursively(field: String, receiver: Expression, right: Expression, expr: Expression): Expression = expr match {
      case ConditionalExpression(cond, thenExpr, elseExpr, typ) => ConditionalExpression(transformAssignmentRecursively(field, receiver, elseExpr, cond), transformAssignmentRecursively(field, receiver, elseExpr, thenExpr), transformAssignmentRecursively(field, receiver, elseExpr, elseExpr), typ)
      case fieldExpr@FieldExpression(_, `field`, rec) =>
        if (receiver.equals(rec)) right
        else ConditionalExpression(BinaryArithmeticExpression(receiver, rec, ArithmeticOperator.==), right, fieldExpr, right.typ)
      case FieldExpression(_, otherField, rec) => FieldExpression(right.typ, otherField, transformAssignmentRecursively(field, receiver, right, rec))
      case FunctionCallExpression(functionName, params, typ, _) => FunctionCallExpression(functionName, params.map(param => transformAssignmentRecursively(field, receiver, right, param)), typ, pp)
      case _ => expr
    }

    override def transformAssignField(receiver: Expression, field: String, right: Expression): ReferenceSetDescription = {
      val newConcreteExpressions = concreteExpressions.map(expr => transformAssignmentRecursively(field, receiver, right, expr))
      copy(concreteExpressions = newConcreteExpressions)
    }

    override def transformAssignVariable(left: VariableIdentifier, right: Expression): ReferenceSetDescription = {
      val newConcreteExpressions = concreteExpressions.map(expr => expr.transform(e => if (e.equals(left)) right else e))
      copy(concreteExpressions = newConcreteExpressions)
    }

    private def abstractExpressions: Set[ReferenceSetElementDescriptor] = {
      concreteExpressions.flatMap(concreteExpression => extractRules(concreteExpression))
    }

    private def extractRules(expr: Expression): Set[ReferenceSetElementDescriptor] = expr match {
      case ConditionalExpression(_, left, right, _) => extractRules(left) ++ extractRules(right)
      case FieldExpression(_, field, receiver) => extractRules(receiver) + AddField(field)
      case id: VariableIdentifier => Set(RootElement(id))
      case FunctionCallDescription(functionName, parameters, returnType, _) => Set(Function(functionName, returnType, pp, parameters.map { case (typ, argExpr) => (typ, pp, argExpr) } ))
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