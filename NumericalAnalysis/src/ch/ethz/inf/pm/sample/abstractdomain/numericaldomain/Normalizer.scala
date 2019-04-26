/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.abstractdomain._

/**
 * An helper object that perform some transformations to obtain simplified and standard numerical expressions.
 *
 * Eliminate nondeterministic expressions before calling this
 *
 * Works on doubles for generality
 *
 * Ignores overflow anyways
 *
 * @author Pietro Ferrara
 * @since 0.1
 */
object Normalizer {

  case class Monomial(variables:List[(Double, Identifier)], constant:Double)

  /**
   * Transforms the current expression to \sum a_i x_i + c >= 0
   *
   * @param exp The conditional expression to be reduced to monomes
   * @return  None if the given expression cannot be reduced to a linear form, Some(E, c) if it can be reduced to E+c>=0 (where E is \sum a_i x_i)
   */
  def conditionalExpressionToMonomial(exp: Expression): Option[Monomial] = exp match {

    case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op)) =>
      op match {
        //! l>= r => l < r
        case ArithmeticOperator.>= => conditionalExpressionToMonomial(BinaryArithmeticExpression(left, right, ArithmeticOperator.<))
        //! l <= r => l > r
        case ArithmeticOperator.<= => conditionalExpressionToMonomial(BinaryArithmeticExpression(left, right, ArithmeticOperator.>))
        //! l > r => l <= r
        case ArithmeticOperator.> => conditionalExpressionToMonomial(BinaryArithmeticExpression(left, right, ArithmeticOperator.<=))
        //! l < r => l >= r
        case ArithmeticOperator.< => conditionalExpressionToMonomial(BinaryArithmeticExpression(left, right, ArithmeticOperator.>=))

        //== and != abstracted away
        case _ => None
      }

    case BinaryArithmeticExpression(left, right, op) =>
      // TODO: Because x != null is treated as arithmetic and it crashes with NumberFormatException (because of null)
      if (left == null || right == null || left.typ == null || right.typ == null ||
        !left.typ.isNumericalType || !right.typ.isNumericalType)
        return None

      val l: Option[Monomial] = arithmeticExpressionToMonomes(left)
      val r: Option[Monomial] = arithmeticExpressionToMonomes(right)
      if (l.isEmpty || r.isEmpty) return None
      op match {

        case ArithmeticOperator.>= =>
          Some(compactOnTheLeft(l.get, r.get));

        //l <= r => r >= l
        case ArithmeticOperator.<= =>
          Some(compactOnTheLeft(r.get, l.get));

        //l > r => l >= r+1
        case ArithmeticOperator.> =>
          val Monomial(lr, vr) = r.get
          if (left.typ.isFloatingPointType || right.typ.isFloatingPointType)
            Some(compactOnTheLeft(l.get, Monomial(lr, vr + NumericalAnalysisConstants.epsilon)))
          else
            Some(compactOnTheLeft(l.get, Monomial(lr, vr + 1)))

        //l < r => r >= l+1
        case ArithmeticOperator.< =>
          val Monomial(lr, vr) = l.get
          if (left.typ.isFloatingPointType || right.typ.isFloatingPointType)
            Some(compactOnTheLeft(r.get, Monomial(lr, vr + NumericalAnalysisConstants.epsilon)))
          else
            Some(compactOnTheLeft(r.get, Monomial(lr, vr + 1)));

        //== and != abstracted away
        case _ => None
      }
    case _ => None;
  }


  /**
   * Transforms the current expression to \sum a_i x_i + c
   *
   * @param exp The expression to be reduced to monomes
   * @return  None if the given expression cannot be reduced to a linear form, Some(E, c) if it can be reduced to E+c (where E is \sum a_i x_i)
   */
  def arithmeticExpressionToMonomes[I <: HeapIdentifier[I]](exp: Expression): Option[Monomial] = exp match {
    case BinaryArithmeticExpression(left, right, op) =>
      val l: Option[Monomial] = arithmeticExpressionToMonomes(left)
      val r: Option[Monomial] = arithmeticExpressionToMonomes(right)
      if (l.isEmpty || r.isEmpty) return None
      op match {
        case ArithmeticOperator.+ => Some(Monomial(l.get.variables ::: r.get.variables, l.get.constant + r.get.constant))

        case ArithmeticOperator.- => Some(Monomial(l.get.variables ::: transform(r.get.variables, (x: Double) => -x), l.get.constant - r.get.constant))

        case ArithmeticOperator.* =>
          if (r.get.variables.equals(Nil)) Some(Monomial(transform(l.get.variables, (x: Double) => x * r.get.constant), l.get.constant * r.get.constant))
          else if (l.get.variables.equals(Nil)) Some(Monomial(transform(r.get.variables, (x: Double) => x * l.get.constant), l.get.constant * r.get.constant))
          else None;

        case ArithmeticOperator./ =>
          if (r.get.variables.equals(Nil)) Some(Monomial(transform(l.get.variables, (x: Double) => x / r.get.constant), l.get.constant / r.get.constant))
          else None;

        case _ => None

      }

    case UnaryArithmeticExpression(left, op, typ) =>
      val l: Option[Monomial] = arithmeticExpressionToMonomes(left)
      if (l.isEmpty) return None
      op match {
        case ArithmeticOperator.- => Some(Monomial(transform(l.get.variables, (x: Double) => -x), -l.get.constant))

        case _ => None
      }

    case Constant("true", t) => Some(Monomial(Nil,1.0))

    case Constant("false", t) => Some(Monomial(Nil,0.0))

    case Constant(c, t) => try {
      Some(Monomial(Nil, c.toDouble))
    } catch {
      case e: NumberFormatException => None
    }

    case UnitExpression(t, pp) => None;

    case x: AbstractOperator => None;

    case x: BinaryNondeterministicExpression => None;

    case x: Identifier => Some(Monomial((1.0, x) :: Nil, 0.0))

    case x: HeapIdSetDomain[I @unchecked] =>
      if (x.value.size != 1) None
      else Some(Monomial((1.0, x.value.iterator.next()) :: Nil, 0.0))

    case _ => None
  }

  private def compactOnTheLeft(left: Monomial, right: Monomial): Monomial =
    Monomial(left.variables ::: transform(right.variables, (x: Double) => -x), left.constant - right.constant)

  private def transform(monome: List[(Double, Identifier)], f: Double => Double): List[(Double, Identifier)] = monome match {
    case Nil => Nil;
    case (n, v) :: xs => (f(n), v) :: transform(xs, f)
  }


  /**
   * This methods normalizes an arithmetic expression to a form c1*v+c2 where c1 and c2 are constants and v is an identifier.
   * If such a normalization is not possible, null is returned.
   * E.g. 3x+2+x will be normalized to 4x+2 and x+y+1 will return null.
   *
   * @param exp is an expression to be normalized
   * @return None if the expression can not be normalized, otherwise it returns Some(normExp) represeting exp in the form c1*v+c2 where c1 and c2 are constants, v is an identifier and exp == normExp == c1*v+c2.
   */
  def normalizeToCoefVarCost(exp: Expression): Option[Expression] = {
    exp match {
      case x: Constant =>
        Some(exp)
      case _ =>
        Normalizer.arithmeticExpressionToMonomes(exp) match {
          case None =>
            None
          case Some(Monomial(variables, const)) =>
            val constExp = Constant(const.toString, exp.typ)(exp.pp)
            variables.length match {
              case 0 =>
                Some(constExp)
              // TODO: this should be reimplemented - ugly
              case 1 =>
                var result: BinaryArithmeticExpression = null
                for ((coef, id) <- variables) {
                  val coefExp = Constant(coef.toString, exp.typ)(exp.pp)
                  val coefAndVarExp: BinaryArithmeticExpression = new BinaryArithmeticExpression(coefExp, id, ArithmeticOperator.*)
                  result = new BinaryArithmeticExpression(coefAndVarExp, constExp, ArithmeticOperator.+)
                }
                Some(result)
              case _ =>
                None
            }
        }
    }
  }

  /**
   * This methods returns if a given expression contains the given id. Two ids are the same if they have the same name.
   *
   * @param exp is an expression to check
   * @param id is an Identifier that we should check
   * @return true if id among identifiers in exp, false otherwise
   */
  def contains[I <: HeapIdentifier[I]](exp: Expression, id: Identifier): Boolean = {
    exp.contains {
      case x:Identifier if x.getName == id.getName => true
      case x:Any => false
    }
  }

  /**
   * This methods substitutes every occurrence of id in exp for subExp. There is no renaming ids.
   * E.g. substitute(3x+1, x, y+1) == 3(y+1)+1
   * substitute(3x+1, x, x+y) == 3(x+y)+1
   * substitute(3x+1, y, y+1) == 3x+1
   *
   * @param exp is an expression to which we want to substitute id
   * @param id is an id for which we want to substitute subExp in exp
   * @param subExp an expression that we want to substitute exp for id
   * @return an expression in which every id in exp is substituted with subExp
   */
  def substitute(exp: Expression, id: Identifier, subExp: Expression): Expression = {
    exp.transform {
      case x:Identifier if x.getName == id.getName => subExp
      case x:Any => x
    }
  }

}
