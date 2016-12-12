/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.sample.quantifiedpermissionanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.quantifiedpermissionanalysis.SetDescription.{Bottom, InnerSetDescription, Top}

import scala.collection.mutable

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

  case class InnerSetDescription(widened: Boolean = false, concreteExpressions: Map[Expression, Set[Expression]] = Map()) extends SetDescription {

    def this(initExpression: Expression) = this(concreteExpressions = Map(initExpression -> Set(initExpression)))

    def copy(widened: Boolean = widened,
             concreteExpressions: Map[Expression, Set[Expression]] = concreteExpressions
            ): InnerSetDescription = InnerSetDescription(widened, concreteExpressions)

    private def blubb2(paramSets: Seq[Set[Expression]]): Set[Seq[Expression]] = {
      if (paramSets.isEmpty) Set(Seq())
      else blubb2(paramSets.init).flatMap(seq => paramSets.last.map(expr => seq :+ expr))
    }

    private def blubb(field: String, receiver: Expression, right: Expression, expr: Expression): Set[Expression] = expr match {
      case FieldExpression(_, `field`, rec) =>
        if (receiver.equals(rec)) Set(right)
        else Set(expr, right) ++ blubb(field, receiver, right, rec).map(newReceiver => FieldExpression(right.typ, field, newReceiver))
      case FieldExpression(_, otherField, rec) => blubb(field, receiver, right, rec).map(newReceiver => FieldExpression(right.typ, otherField, newReceiver))
      case FunctionCallExpression(typ, functionName, params, pp) =>
        blubb2(params.map(param => blubb(field, receiver, right, param))).map(newParams => FunctionCallExpression(typ, functionName, newParams, pp))
      case _ => Set(expr)
    }

    override def transformAssignField(receiver: Expression, field: String, right: Expression): SetDescription = {
      val fieldExpression = FieldExpression(right.typ, field, receiver)
      val newConcreteExpressions = concreteExpressions.transform {
        case (_, set) => set.flatMap(expr => blubb(field, receiver, right, expr))
      }
      copy(concreteExpressions = newConcreteExpressions)
    }

    override def transformAssignVariable(left: VariableIdentifier, right: Expression): SetDescription = {
      val newConcreteExpressions = concreteExpressions.transform {
        case (_, set) => set.map(expr => expr.transform(e => if (e.equals(left)) right else e))
      }
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
          concreteExpressions = concreteExpressions ++ concreteExpressions_.transform {
            case (expr, exprSet) => if (concreteExpressions.contains(expr)) concreteExpressions(expr) ++ exprSet else exprSet
          })
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
      copy(widened = true)
    }

    def abstractExpressions: Set[SetElementDescriptor] = {
      val abstractExpressions: mutable.Set[SetElementDescriptor] = mutable.Set()
      concreteExpressions.foreach {
        case (expr, mapsTo) =>
            mapsTo.foreach {
              e =>
                newConcreteExpressions.put(e, Set(e))
                newAbstractExpressions.union(extractRules(e))
            }
        }
    }

    //    override def update: InnerSetDescription = {
    //      val newAbstractExpressions: mutable.Set[SetElementDescriptor] = mutable.Set() ++ abstractExpressions
    //      val newConcreteExpressions: mutable.Map[Expression, Set[Expression]] = mutable.Map()
    //      concreteExpressions.foreach {
    //        case (expr, mapsTo) =>
    //            newConcreteExpressions.put(expr, Set(expr))
    //            mapsTo.foreach {
    //              e =>
    //                newConcreteExpressions.put(e, Set(e))
    //                newAbstractExpressions.union(extractRules(e))
    //            }
    //        }
    //      copy(concreteExpressions = newConcreteExpressions.toMap, abstractExpressions = newAbstractExpressions.toSet)
    //    }

    private def extractRules(expr: Expression): Set[SetElementDescriptor] = expr match {
      case FieldExpression(_, field, receiver) => extractRules(receiver) + AddField(field)
      case id: VariableIdentifier => Set(RootElement(id))
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
          && ((widened && other.widened) || concreteExpressions.forall { case (expr, set) => set.subsetOf(other.concreteExpressions.getOrElse(expr, Set())) }))
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

//  def update: SetDescription = this

  def transformAssignField(receiver: Expression, field: String, right: Expression): SetDescription = this

  def transformAssignVariable(left: VariableIdentifier, right: Expression): SetDescription = this
}

trait SetElementDescriptor

case class RootElement(expr: Expression) extends SetElementDescriptor

case class AddField(field: String) extends SetElementDescriptor