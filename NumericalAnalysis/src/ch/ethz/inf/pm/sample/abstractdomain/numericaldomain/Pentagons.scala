/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Normalizer.Monomial
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.util.Relation

object UpperBoundRelation {

  object Bottom
    extends UpperBoundRelation
    with NumericalDomain.Relational.Bottom[UpperBoundRelation]
    with RelationalDomain.Bottom[UpperBoundRelation] {

    def addBound(id1: Identifier, id2: Identifier): UpperBoundRelation = this

    override def createVariable(variable: Identifier, typ: Type) = this

    override def add(ids: Set[Identifier]) = this

  }

  object Top
    extends UpperBoundRelation
    with NumericalDomain.Relational.Top[UpperBoundRelation]
    with RelationalDomain.Top[UpperBoundRelation] {

    def addBound(id1: Identifier, id2: Identifier): UpperBoundRelation =
      factory(Relation.empty[Identifier].add(id1,id2))

    override def assign(variable: Identifier, expr: Expression) =
      Inner(Relation.empty[Identifier]).assign(variable,expr)

    override def assume(expr: Expression) =
      Inner(Relation.empty[Identifier]).assume(expr)

  }

  case class Inner(override val elements: Relation[Identifier])
    extends UpperBoundRelation
    with NumericalDomain.Relational.Inner[UpperBoundRelation, UpperBoundRelation.Inner]
    with RelationalDomain.Inner[UpperBoundRelation, UpperBoundRelation.Inner]
    with BooleanExpressionSimplifier[UpperBoundRelation]  {

    def addBound(id1: Identifier, id2: Identifier): UpperBoundRelation = {
      if (elements.getLeftOrElse(id2, Set.empty).contains(id1)) return bottom()
      factory(elements.add(id1, id2))
    }

    /**
     * This method assumes that a given expression hold
     *
     * @param expr the expression to be assumed
     * @return the state after this action
     */
    override def assumeSimplified(expr: Expression): UpperBoundRelation =
      expr match {
        case BinaryArithmeticExpression(left, right, ArithmeticOperator.!=) =>
          val newLeft = BinaryArithmeticExpression(left, right, ArithmeticOperator.>)
          val newRight = BinaryArithmeticExpression(left, right, ArithmeticOperator.<)
          val res = assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||))
          res
        case BinaryArithmeticExpression(left, right, ArithmeticOperator.==)
          if left.isInstanceOf[Identifier] && right.isInstanceOf[Identifier] =>
          val l = left.asInstanceOf[Identifier]
          val r = left.asInstanceOf[Identifier]

          var curState:UpperBoundRelation = this
          curState = elements.getLeftOrElse(l, Set.empty).foldLeft(curState)(_.addBound(r, _))
          curState = elements.getLeftOrElse(r, Set.empty).foldLeft(curState)(_.addBound(l, _))
          curState = elements.getRightOrElse(l, Set.empty).foldLeft(curState)(_.addBound(_, r))
          curState = elements.getRightOrElse(r, Set.empty).foldLeft(curState)(_.addBound(_, l))
          curState

        case xp: BinaryArithmeticExpression =>
          Normalizer.conditionalExpressionToMonomial(xp) match {
            case None =>
              this
            case Some(Monomial(weightedIds, constant)) =>
              // Sum of weightedIds + constant >= 0
              // We only cover the case with two variables here
              if (weightedIds.length == 2) {
                val List(first, second) = weightedIds.take(2)
                if (constant < 0) {
                  if (first._1 > 0 && first._1 == -second._1) {
                    // af >= as + c > as
                    addBound(second._2, first._2)
                  } else if (first._1 < 0 && first._1 == -second._1) {
                    // as >= af + c > af
                    addBound(first._2, second._2)
                  } else this
                } else this
              } else this
          }
        case _ => this
      }

    /**
     * This method creates a variable.
     *
     * @param variable the variable to be created
     * @param typ its type
     * @return the state after this action
     */
    override def createVariable(variable: Identifier, typ: Type): UpperBoundRelation =
      this

    /**
     * This method assigns a given variable to the given expression
     *
     * @param variable the variable to be assigned
     * @param expr the expression to be assigned
     * @return the state after this action
     */
    override def assign(variable: Identifier, expr: Expression): UpperBoundRelation =
      factory(elements.setLeft(variable, eval(expr)))

    /**
     * This method returns representing the value of the given identifier
     *
     * @param id the identifier
     * @return the string representing its state
     */
    override def getStringOfId(id: Identifier): String = {
      if (isBottom) return "⊥"
      if (isTop) return "T"

      val left = elements.getLeftOrElse(id, Set.empty)
      val right = elements.getRightOrElse(id, Set.empty)
      if (left.isEmpty && right.isEmpty) return "T"
      (left.map(id + "<" + _) ++ right.map(id + ">" + _)).mkString(",")
    }

    /** We do not implement this */
    override def ids = ???

    /**
     * Returns the upper bounds on the given expression
     */
    def eval(expr: Expression): Set[Identifier] = {
      expr match {
        case id: Identifier => elements.getLeftOrElse(id, Set.empty)
        case xp: BinaryArithmeticExpression =>
          Normalizer.arithmeticExpressionToMonomes(xp) match {
            case None => Set.empty
            case Some(Monomial(weightedIds, constant)) =>
              // Sum of weightedIds + constant.
              // We only cover the case with one variable here
              if (weightedIds.length == 1) {
                val first = weightedIds.head
                if (constant < 0 && first._1 == 1) {
                  // f - a < f < up(f) for a != 0
                  elements.getLeftOrElse(first._2, Set.empty) + first._2
                } else if (constant == 0 && first._1 == 1) {
                  // f < up(f)
                  elements.getLeftOrElse(first._2, Set.empty)
                } else Set.empty
              } else Set.empty
          }
        case _ => Set.empty
      }
    }

    override def glbSameEnvInner(that: Inner) = factory(elements union that.elements)

    override def lessEqualSameEnvInner(that: Inner) = elements subSetOf that.elements

    override def lubSameEnvInner(that: Inner) = factory(elements intersect that.elements)

    override def unifyInner(that: Inner) = factory(elements union that.elements)

    override def wideningSameEnvInner(that: Inner) = factory(elements union that.elements)

    override def getConstraints(ids: Set[Identifier]) = ???

    override def getPossibleConstants(id: Identifier) = ???
  }

}

trait UpperBoundRelation
  extends RelationalDomain[UpperBoundRelation]
  with NumericalDomain.Relational[UpperBoundRelation]
  with SimplifiedSemanticDomain[UpperBoundRelation] {

  override def factory(rel: Relation[Identifier]): UpperBoundRelation =
    UpperBoundRelation.Inner(rel)

  override def bottom(): UpperBoundRelation =
    UpperBoundRelation.Bottom

  override def top(): UpperBoundRelation =
    UpperBoundRelation.Top

  def addBound(id1: Identifier, id2: Identifier): UpperBoundRelation

}

case class Pentagons(_1: BoxedNonRelationalNumericalDomain[IntegerInterval], _2: UpperBoundRelation)
  extends SemanticCartesianProductDomain[BoxedNonRelationalNumericalDomain[IntegerInterval], UpperBoundRelation, Pentagons] {

  override def getStringOfId(id: Identifier) =
    this._1.getStringOfId(id) + ", " + this._2.getStringOfId(id)

  override def factory(a: BoxedNonRelationalNumericalDomain[IntegerInterval], b: UpperBoundRelation) = Pentagons(a, b)

  override def ids = _1.ids

}

case class DoublePentagons(_1: BoxedNonRelationalNumericalDomain[DoubleInterval], _2: UpperBoundRelation)
  extends SemanticCartesianProductDomain[BoxedNonRelationalNumericalDomain[DoubleInterval], UpperBoundRelation, DoublePentagons] {

  override def getStringOfId(id: Identifier) =
    this._1.getStringOfId(id) + ", " + this._2.getStringOfId(id)

  override def factory(a: BoxedNonRelationalNumericalDomain[DoubleInterval], b: UpperBoundRelation) = DoublePentagons(a, b)

  override def ids = _1.ids

}

