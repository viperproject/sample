/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.ReferenceSetDescription.ReferenceSetElementDescriptor.{AddField, Function, RootElement}
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 03.12.16.
  */
sealed trait SetDescription[S <: SetDescription[S]] extends Lattice[S] {

  this: S =>

  def silverType: sil.Type

  def transformAssignField(receiver: Expression, field: String, right: Expression): S = this

  def transformAssignVariable(left: VariableIdentifier, right: Expression): S = this

  /**
    * Generates an expression that checks whether a given quantified variable is contained in the set represented by
    * this description.
    *
    * @param quantifiedVariable The quantified variable to compare against.
    * @return A silver expression that checks whether the given quantified variable is in the set.
    */
  def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription[_]], quantifiedVariable: sil.LocalVar = Context.getQuantifiedVarDecl(silverType).localVar): sil.Exp = throw new UnsupportedOperationException

  def isFinite(expressions: Map[(ProgramPoint, Expression), SetDescription[_]]): Boolean = false

  def toSetDefinition(expressions: Map[(ProgramPoint, Expression), SetDescription[_]]): sil.Exp = throw new UnsupportedOperationException
}

object SetDescription {

  sealed trait Top[S <: SetDescription[S]] extends SetDescription[S] with Lattice.Top[S] {
    this: S =>
  }

  sealed trait Bottom[S <: SetDescription[S]] extends SetDescription[S] with Lattice.Bottom[S] {
    this: S =>
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

    def copy(widened: Boolean = widened, concreteExpressions: Set[Expression] = concreteExpressions): Inner =
      Inner(key, widened, concreteExpressions)

    override def isFinite(expressions: Map[(ProgramPoint, Expression), SetDescription[_]]): Boolean = {
      !widened || abstractExpressions.forall {
        case _: AddField => false
        case Function(_, _, _, parameters) => parameters.forall { case (_, pp, expr) => expressions((pp, expr)).isFinite(expressions) }
        case _ => true
      }
    }

    /**
      * Generates an expression that checks whether a given quantified variable is contained in the set represented by
      * this description.
      *
      * @param quantifiedVariable The quantified variable to compare against.
      * @return A silver expression that checks whether the given quantified variable is in the set.
      */
    override def toSilExpression(expressions: Map[(ProgramPoint, Expression), SetDescription[_]], quantifiedVariable: sil.LocalVar = Context.getQuantifiedVarDecl(silverType).localVar): sil.Exp = {
      if (isFinite(expressions))
        concreteExpressions.map (expr => expr.transform {
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

    override def toSetDefinition(expressions: Map[(ProgramPoint, Expression), SetDescription[_]]): sil.Exp = {
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
            function.formalArgs.zip(argKeys).foreach { case (formalArg, (_, argPP, argExpr)) =>
              val arg = Context.getQuantifiedVarDecl(formalArg.typ, args.toSet)
              args :+= arg
              impliesLeftConjuncts :+= expressions((argPP, argExpr)).toSilExpression(expressions, arg.localVar)
                // TODO: do we want to include this restriction? Consequence: Not necessarily sound anymore!
//              if (formalArg.typ.equals(sil.Ref))
//                impliesLeftConjuncts :+= sil.NeCmp(arg.localVar, sil.NullLit()())()
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
      case FunctionCallExpression(functionName, params, typ, pp) => FunctionCallExpression(functionName, params.map(param => transformAssignmentRecursively(field, receiver, right, param)), typ, pp)
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

    def abstractExpressions: Set[ReferenceSetElementDescriptor] = {
      concreteExpressions.flatMap(concreteExpression => extractRules(concreteExpression))
    }

    private def extractRules(expr: Expression): Set[ReferenceSetElementDescriptor] = expr match {
      case ConditionalExpression(_, left, right, _) => extractRules(left) ++ extractRules(right)
      case FieldExpression(_, field, receiver) => extractRules(receiver) + AddField(field)
      case id: VariableIdentifier => Set(RootElement(id))
      case FunctionCallExpression(functionName, parameters, typ, pp) => Set(Function(functionName, typ, pp, parameters.map(param => (param.typ, pp, param))))
    }

    override def lubInner(other: Inner): Inner =
      copy(
        widened = widened || other.widened,
        concreteExpressions = concreteExpressions ++ other.concreteExpressions
      )

    override def glbInner(other: Inner): Inner = throw new UnsupportedOperationException()

    override def wideningInner(other: Inner): Inner = lubInner(other).copy(widened = true)

    override def lessEqualInner(other: Inner): Boolean =
      abstractExpressions.subsetOf(other.abstractExpressions) &&
        ((widened && other.widened) || concreteExpressions.subsetOf(other.concreteExpressions))
  }
}