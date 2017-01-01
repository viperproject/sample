/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.SetDescription.{Bottom, Top}
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 03.12.16.
  */
object SetDescription {
  case object Top extends SetDescription with Lattice.Top[SetDescription]

  case object Bottom extends SetDescription with Lattice.Bottom[SetDescription]

  object InnerSetDescription {
    def apply(pp: ProgramPoint, initExpression: Expression) = new InnerSetDescription(pp, initExpression)
  }

  case class InnerSetDescription(key: (ProgramPoint, Expression), widened: Boolean = false, concreteExpressions: Set[Expression] = Set()) extends SetDescription {

    def this(pp: ProgramPoint, initExpression: Expression) = this((pp, initExpression), concreteExpressions = Set(initExpression))

    override def silverType: sil.Type = DefaultSampleConverter.convert(key._2.typ)

    override def copy(widened: Boolean = widened,
             concreteExpressions: Set[Expression] = concreteExpressions
            ): InnerSetDescription = InnerSetDescription(key, widened, concreteExpressions)

    override def isFinite: Boolean = {
      !widened || !abstractExpressions.exists {
        case _: AddField => true
        case _ => false
      }
    }

    /**
      * Generates an expression that checks whether a given quantified variable is contained in the set represented by
      * this description.
      *
      * @param quantifiedVariable The quantified variable to compare against.
      * @return A silver expression that checks whether the given quantified variable is in the set.
      */
    override def toSilExpression(quantifiedVariable: sil.LocalVar = Context.getQuantifiedVarDecl(silverType).localVar): sil.Exp = {
      if (isFinite)
        concreteExpressions.map (expr => expr.transform {
          case FieldExpression(typ, field, receiver) =>
            if (!Context.fieldAccessFunctions.contains(field)) {
              val fun = sil.Function(Context.createNewUniqueFunctionIdentifier("get_" + field), Seq(sil.LocalVarDecl("x", sil.Ref)()), DefaultSampleConverter.convert(typ), Seq(), Seq(), None)()
              Context.fieldAccessFunctions.put(field, fun)
              Context.auxiliaryFunctions.put(fun.name, fun)
            }
            FunctionCallExpression(typ, Context.fieldAccessFunctions(field).name, Seq(receiver))
          case other => other
        }).map(expr => sil.EqCmp(quantifiedVariable, DefaultSampleConverter.convert(expr))()).reduce[sil.Exp]((left, right) => sil.Or(left, right)())
      else
        sil.AnySetContains(quantifiedVariable, Context.getSetFor(key).localVar)()
    }

    override def toSetDefinition(expressions: Map[(ProgramPoint, Expression), SetDescription]): sil.Exp = {
      val set = Context.getSetFor(key).localVar
      if (isFinite) null
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
              impliesLeftConjuncts :+= expressions((argPP, argExpr)).toSilExpression(arg.localVar)
              if (formalArg.typ.equals(sil.Ref))
                impliesLeftConjuncts :+= sil.NeCmp(arg.localVar, sil.NullLit()())()
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
          case Some(exp) => conjuncts :+= sil.Forall(Seq(quantifiedVariableForFields), Seq(), exp)()
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
      case FunctionCallExpression(typ, functionName, params, pp) => FunctionCallExpression(typ, functionName, params.map(param => transformAssignmentRecursively(field, receiver, right, param)), pp)
      case _ => expr
    }

    override def transformAssignField(receiver: Expression, field: String, right: Expression): SetDescription = {
      val newConcreteExpressions = concreteExpressions.map(expr => transformAssignmentRecursively(field, receiver, right, expr))
      copy(concreteExpressions = newConcreteExpressions)
    }

    override def transformAssignVariable(left: VariableIdentifier, right: Expression): SetDescription = {
      val newConcreteExpressions = concreteExpressions.map(expr => expr.transform(e => if (e.equals(left)) right else e))
      copy(concreteExpressions = newConcreteExpressions)
    }

    /** Computes the least upper bound of two elements.
      *
      * @param other The other value
      * @return The least upper bound, that is, an element that is greater than or equal to the two arguments,
      *         and less than or equal to any other upper bound of the two arguments
      */
    override def lub(other: SetDescription): SetDescription = other match {
      case Top => Top
      case Bottom => this
      case InnerSetDescription(_, widened_, concreteExpressions_) =>
        copy(
          widened = widened || widened_,
          concreteExpressions = concreteExpressions ++ concreteExpressions_
        )
    }

    /** Computes the greatest lower bound of two elements.
      *
      * @param other The other value
      * @return The greatest upper bound, that is, an element that is less than or equal to the two arguments,
      *         and greater than or equal to any other lower bound of the two arguments
      */
    override def glb(other: SetDescription): SetDescription = throw new UnsupportedOperationException()

    /** Computes the widening of two elements.
      *
      * @param other The new value
      * @return The widening of `this` and `other`
      */
    override def widening(other: SetDescription): SetDescription = {
      lub(other).copy(widened = true)
    }

    def abstractExpressions: Set[SetElementDescriptor] = {
      concreteExpressions.flatMap(concreteExpression => extractRules(concreteExpression))
    }

    private def extractRules(expr: Expression): Set[SetElementDescriptor] = expr match {
      case ConditionalExpression(_, left, right, _) => extractRules(left) ++ extractRules(right)
      case FieldExpression(_, field, receiver) => extractRules(receiver) + AddField(field)
      case id: VariableIdentifier => Set(RootElement(id))
      case FunctionCallExpression(typ, functionName, parameters, pp) => Set(Function(functionName, typ, pp, parameters.map(param => (param.typ, pp, param))))
    }

    /** Returns true if and only if `this` is less than or equal to `other`.
      *
      * @param other The value to compare
      * @return true if and only if `this` is less than or equal to `other`
      */
    override def lessEqual(other: SetDescription): Boolean = other match {
      case Top => true
      case Bottom => false
      case other: InnerSetDescription =>
        (abstractExpressions.subsetOf(other.abstractExpressions)
          && ((widened && other.widened) || concreteExpressions.subsetOf(other.concreteExpressions)))
      case _ => throw new IllegalStateException()
    }

    /** Checks whether the given domain element is equivalent to bottom.
      *
      * @return bottom
      */
    override def isBottom: Boolean = false

    /** Checks whether the given domain element is equivalent to top.
      *
      * @return bottom
      */
    override def isTop: Boolean = false
  }
}

trait SetDescription extends Lattice[SetDescription] {

  def silverType: sil.Type = throw new UnsupportedOperationException

  override def factory(): SetDescription = copy()

  override def top() = Top

  override def bottom() = Bottom

  def copy(widened: Boolean = false, concreteExpressions: Set[Expression] = Set()): SetDescription = this

  def transformAssignField(receiver: Expression, field: String, right: Expression): SetDescription = this

  def transformAssignVariable(left: VariableIdentifier, right: Expression): SetDescription = this

  /**
    * Generates an expression that checks whether a given quantified variable is contained in the set represented by
    * this description.
    *
    * @param quantifiedVariable The quantified variable to compare against.
    * @return A silver expression that checks whether the given quantified variable is in the set.
    */
  def toSilExpression(quantifiedVariable: sil.LocalVar = Context.getQuantifiedVarDecl(silverType).localVar): sil.Exp = throw new UnsupportedOperationException

  def isFinite: Boolean = false

  def toSetDefinition(expressions: Map[(ProgramPoint, Expression), SetDescription]): sil.Exp = throw new UnsupportedOperationException
}

trait SetElementDescriptor

case class RootElement(expr: Expression) extends SetElementDescriptor

case class AddField(field: String) extends SetElementDescriptor

case class Function(functionName: String, typ: Type, pp: ProgramPoint, parameters: Seq[(Type, ProgramPoint, Expression)]) extends SetElementDescriptor