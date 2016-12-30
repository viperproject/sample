/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.{ProgramPoint, Type}
import ch.ethz.inf.pm.sample.oorepresentation.silver.DefaultSampleConverter
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.SetDescription.{Bottom, InnerSetDescription, Top}
import viper.silver.{ast => sil}

/**
  * @author Severin Münger
  *         Added on 03.12.16.
  */
object SetDescription {
  case object Top extends SetDescription with Lattice.Top[SetDescription]

  case object Bottom extends SetDescription with Lattice.Bottom[SetDescription]

  object InnerSetDescription {
    def apply(initExpression: Expression) = new InnerSetDescription(initExpression)
  }

  case class InnerSetDescription(widened: Boolean = false, concreteExpressions: Set[Expression] = Set()) extends SetDescription {

    def this(initExpression: Expression) = this(concreteExpressions = Set(initExpression))

    override def copy(widened: Boolean = widened,
             concreteExpressions: Set[Expression] = concreteExpressions
            ): InnerSetDescription = InnerSetDescription(widened, concreteExpressions)

    override def isFinite: Boolean = {
      !widened || !abstractExpressions.exists {
        case _: AddField => true
        case _ => false
      }
    }

    override def toSilExpression(quantifiedVariable: sil.LocalVar, set: sil.LocalVarDecl): sil.Exp = {
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
        sil.AnySetContains(quantifiedVariable, set.localVar)()
    }

    override def toSetDefinition(quantifiedVariable: sil.LocalVarDecl, set: sil.LocalVarDecl): sil.Exp = {
      if (isFinite) null
      else {
        val roots = abstractExpressions.filter {
          case _: RootElement => true
          case _ => false
        }.map {
          case RootElement(root) => sil.AnySetContains(DefaultSampleConverter.convert(root), set.localVar)()
        }
        val fields = abstractExpressions.filter {
          case _: AddField => true
          case _ => false
        }.map {
          case AddField(field) =>
            val fieldAccess = sil.FieldAccess(quantifiedVariable.localVar, sil.Field(field, sil.Ref)())()
            sil.AnySetContains(fieldAccess, set.localVar)()
        }
        sil.And(roots.reduce[sil.Exp]((left, right) => sil.And(left, right)()), sil.Forall(Seq(quantifiedVariable), Seq(), sil.Implies(sil.And(sil.NeCmp(quantifiedVariable.localVar, sil.NullLit()())(), sil.AnySetContains(quantifiedVariable.localVar, set.localVar)())(), fields.reduce[sil.Exp]((left, right) => sil.And(left, right)()))())())()
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
      case InnerSetDescription(widened_, concreteExpressions_) =>
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
  override def factory() = InnerSetDescription()

  override def top() = Top

  override def bottom() = Bottom

  def copy(widened: Boolean = false, concreteExpressions: Set[Expression] = Set()): SetDescription = this

  def transformAssignField(receiver: Expression, field: String, right: Expression): SetDescription = this

  def transformAssignVariable(left: VariableIdentifier, right: Expression): SetDescription = this

  def toSilExpression(quantifiedVariable: sil.LocalVar, set: sil.LocalVarDecl): sil.Exp = throw new UnsupportedOperationException

  def isFinite: Boolean = false

  def toSetDefinition(quantifiedVariable: sil.LocalVarDecl, set: sil.LocalVarDecl): sil.Exp = throw new UnsupportedOperationException
}

trait SetElementDescriptor

case class RootElement(expr: Expression) extends SetElementDescriptor

case class AddField(field: String) extends SetElementDescriptor

case class Function(functionName: String, typ: Type, pp: ProgramPoint, parameters: Seq[(Type, ProgramPoint, Expression)]) extends SetElementDescriptor