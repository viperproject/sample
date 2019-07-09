/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.SystemParameters
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.Normalizer.Monomial
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property.{DivisionByZero, Property, SingleStatementProperty}


case class BoxedNonRelationalNumericalDomain[N <: NonRelationalNumericalDomain[N]](dom: N,
                                                                                   map: Map[Identifier, N] = Map.empty[Identifier, N],
                                                                                   isBottom: Boolean = false,
                                                                                   isTop: Boolean = false)
  extends BoxedDomain[N, BoxedNonRelationalNumericalDomain[N]]
    with NumericalDomain[BoxedNonRelationalNumericalDomain[N]]
    with SimplifiedSemanticDomain[BoxedNonRelationalNumericalDomain[N]] {

  def functionalFactory(_value: Map[Identifier, N] = Map.empty[Identifier, N], _isBottom: Boolean = false, _isTop: Boolean = false): BoxedNonRelationalNumericalDomain[N] =
    new BoxedNonRelationalNumericalDomain[N](dom, _value, _isBottom, _isTop)

  override def createVariable(variable: Identifier, typ: Type): BoxedNonRelationalNumericalDomain[N] = {
    if (variable.typ.isNumericalType) {
      this.add(variable, dom.top())
    } else this
  }

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    var result = Map.empty[Identifier, List[String]]
    result = result + ((variable, path ::: variable.toString :: Nil))
    (this.add(variable, dom.top()), result)
  }

  override def setToTop(variable: Identifier): BoxedNonRelationalNumericalDomain[N] = {
    this.add(variable, dom.top())
  }

  /**
    * Adds [key->value] to the domain
    *
    * @param key   The key
    * @param value The value
    * @return The state of the domain after the assignment
    */
  override def add(key: Identifier, value: N): BoxedNonRelationalNumericalDomain[N] = {
    if (value.isBottom)
      bottom()
    else super.add(key, value)
  }

  override def removeVariable(variable: Identifier): BoxedNonRelationalNumericalDomain[N] = {
    this.remove(variable)
  }

  override def setArgument(variable: Identifier, expr: Expression): BoxedNonRelationalNumericalDomain[N] = this.assign(variable, expr)

  override def assign(variable: Identifier, expr: Expression): BoxedNonRelationalNumericalDomain[N] = {
    if (variable.typ.isNumericalType) {
      if (this.ids.contains(variable)) {
        if (variable.representsSingleVariable)
          this.add(variable, eval(expr))
        else this.add(variable, this.get(variable).lub(eval(expr)))
      } else bottom()
    } else this
  }

  override def backwardAssign(oldPreState: BoxedNonRelationalNumericalDomain[N], variable: Identifier, expr: Expression): BoxedNonRelationalNumericalDomain[N] = {
    ??? // only support for apron domains
  }

  def evalBoolean(expr: Expression): N = {
    // Implicit conversion from boolean types
    val mayBeTrue = if (!this.assume(expr).isBottom) dom.evalConstant(1) else dom.bottom()
    val mayBeFalse = if (!this.assume(NegatedBooleanExpression(expr)).isBottom) dom.evalConstant(0) else dom.bottom()
    mayBeTrue.lub(mayBeFalse)
  }

  def eval(expr: Expression): N = {

    expr match {
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.+) => eval(left).sum(eval(right))
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.*) => eval(left).multiply(eval(right))
      case BinaryArithmeticExpression(left, right, ArithmeticOperator./) => eval(left).divide(eval(right))
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.-) => eval(left).subtract(eval(right))
      case BinaryArithmeticExpression(left, right, op) if ArithmeticOperator.isComparison(op) => evalBoolean(expr)
      case UnaryArithmeticExpression(operand, ArithmeticOperator.+, _) => eval(operand)
      case UnaryArithmeticExpression(operand, ArithmeticOperator.-, _) => dom.evalConstant(0).subtract(eval(operand))
      case BinaryBooleanExpression(left, right, _) => evalBoolean(expr)
      case NegatedBooleanExpression(left) => evalBoolean(expr)
      case BinaryArithmeticExpression(left, right, op) => dom.top()
      case c@Constant("true", typ) => dom.evalConstant(1)
      case c@Constant("false", typ) => dom.evalConstant(0)
      case c@Constant(constant, typ) => dom.evalConstant(c)
      case x: Identifier => this.get(x)
      case xs: HeapIdSetDomain[_] =>
        var result = dom.bottom()
        for (x <- xs.value) result = result.lub(this.get(x))
        result
      case x: Expression =>
        dom.top()
    }
  }

  override def assume(expr: Expression): BoxedNonRelationalNumericalDomain[N] =

    expr match {

      // Boolean constants
      case Constant("true", _) => this
      case Constant("false", _) => this.bottom()
      case NegatedBooleanExpression(Constant("true", _)) => this.bottom()
      case NegatedBooleanExpression(Constant("false", _)) => this
      case BinaryArithmeticExpression(Constant(a, _), Constant(b, _), ArithmeticOperator.==) =>
        if (a == b) this else bottom()

      // Boolean variables
      case x: Identifier =>
        if (SystemParameters.DEBUG) assert(x.typ.isBooleanType)
        val res = assume(BinaryArithmeticExpression(x, Constant("0", x.typ)(x.pp), ArithmeticOperator.!=))
        res

      case NegatedBooleanExpression(x: Identifier) =>
        if (SystemParameters.DEBUG) assert(x.typ.isBooleanType)
        val res = assume(BinaryArithmeticExpression(x, Constant("0", x.typ)(x.pp), ArithmeticOperator.==))
        res

      // And and Or
      case BinaryBooleanExpression(left, right, op) => op match {
        case BooleanOperator.&& => assume(left).assume(right)
        case BooleanOperator.|| =>
          val l = assume(left)
          val r = assume(right)
          val res = l lub r
          res
      }

      // Double-Negation + De-Morgan
      case NegatedBooleanExpression(NegatedBooleanExpression(x)) =>
        assume(x)
      case NegatedBooleanExpression(BinaryBooleanExpression(left, right, op)) =>
        val nl = NegatedBooleanExpression(left)
        val nr = NegatedBooleanExpression(right)
        val nop = op match {
          case BooleanOperator.&& => BooleanOperator.||
          case BooleanOperator.|| => BooleanOperator.&&
        }
        assume(BinaryBooleanExpression(nl, nr, nop))

      // Convert double inequality
      case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, ArithmeticOperator.==)) =>
        val newLeft = BinaryArithmeticExpression(left, right, ArithmeticOperator.>)
        val newRight = BinaryArithmeticExpression(left, right, ArithmeticOperator.<)
        val res = assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||))
        res

      case BinaryArithmeticExpression(left, right, ArithmeticOperator.!=) =>
        val newLeft = BinaryArithmeticExpression(left, right, ArithmeticOperator.>)
        val newRight = BinaryArithmeticExpression(left, right, ArithmeticOperator.<)
        val res = assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||))
        res

      // Inverting of operators
      case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op)) =>
        val res = assume(BinaryArithmeticExpression(left, right, ArithmeticOperator.negate(op)))
        res

      // Handling of monomes
      case _ => Normalizer.conditionalExpressionToMonomial(expr) match {
        case None =>
          expr match {
            case BinaryArithmeticExpression(left, right, op) =>
              if (!left.typ.isNumericalType || !right.typ.isNumericalType) return this

              val l: N = this.eval(left)
              val r: N = this.eval(right)

              val (newLeft, newRight) = op match {
                case ArithmeticOperator.== => (l.glb(r), r.glb(l))
                case ArithmeticOperator.<= => (l.glb(r.valueLEQ), r.glb(l.valueGreater))
                case ArithmeticOperator.>= => (l.glb(r.valueGEQ), r.glb(l.valueLess))
                case ArithmeticOperator.> => (l.glb(r.valueGreater), r.glb(l.valueLEQ))
                case ArithmeticOperator.< => (l.glb(r.valueLess), r.glb(l.valueGEQ))
                case _ => return this
              }

              var curState = this
              left match {
                case lId: Identifier => curState = curState.add(lId, newLeft);
                case _ => ()
              }
              right match {
                case rId: Identifier => curState = curState.add(rId, newRight);
                case _ => ()
              }
              return curState
            case _ => return this
          }
          this
        case Some(Monomial(weightedVariables, constant)) =>
          var stateResult: BoxedNonRelationalNumericalDomain[N] = this

          // Check if it is trivially false, e.g. -1 >= 0
          if (weightedVariables.isEmpty && constant < 0)
            return stateResult.bottom()

          for (weightedVariable <- weightedVariables) {
            val (weight, variable) = weightedVariable
            var result = dom.evalConstant(constant)
            for (weightedVariable1 <- weightedVariables) {
              if (!weightedVariable.equals(weightedVariable1))
                result = result sum dom.evalConstant(weightedVariable1._1).multiply(eval(weightedVariable1._2))
            }
            if (weight >= 0) {
              //k*x+n >= 0 => x >= -(n/k)
              result = result.divide(dom.evalConstant(weight))
              result = dom.evalConstant(0).subtract(result)
              val newValue = get(variable) glb result.valueGEQ
              if (newValue.lessEqual(newValue.bottom()))
                return stateResult.bottom()
              stateResult = stateResult.add(variable, newValue)
            }
            else {
              //-k*x+n >= 0 => x <= n/-k
              result = result.divide(dom.evalConstant(-weight))
              val oldValue = this.get(variable)
              val newRestraint = result.valueLEQ
              val newValue = oldValue.glb(newRestraint)
              if (newValue.lessEqual(newValue.bottom()))
                return stateResult.bottom()
              stateResult = stateResult.add(variable, newValue)
            }
          }
          stateResult
      }
    }

  /** Returns all the knowledge we have on the given identifiers as an expression */
  override def getConstraints(ids: Set[Identifier]): Set[Expression] = {
    (for (id <- ids) yield {
      map.get(id) match {
        case Some(nonrel) => nonrel.asConstraint(id)
        case None => None
      }
    }).flatten
  }

  override def getPossibleConstants(id: Identifier) = get(id).getPossibleConstants

  def get(key: Identifier): N = map.get(key) match {
    case None => dom.bottom()
    case Some(x) => x
  }
}


trait NonRelationalNumericalDomain[N <: NonRelationalNumericalDomain[N]] extends Lattice[N] {
  this: N =>

  def evalConstant(value: Double): N

  def evalConstant(value: Constant): N

  def sum(rightExpr: N): N

  def subtract(rightExpr: N): N

  def multiply(rightExpr: N): N

  def divide(rightExpr: N): N

  /**
    * Returns a value representing (an overapproximation of) all values greater or equal than this value
    */
  def valueGEQ: N

  /**
    * Returns a value representing (an overapproximation of) all values less or equal than this value
    */
  def valueLEQ: N

  /**
    * Returns a value representing (an overapproximation of) all values less than this value
    */
  def valueLess: N

  /**
    * Returns a value representing (an overapproximation of) all values greater than this value
    */
  def valueGreater: N

  /**
    * Returns true if value ranges overlap
    */
  def overlapsWith(value: N): Boolean

  /**
    * Encodes non-relational information as a constraint
    *
    * @param id the identifier this domain restricts
    * @return a constraint, for example 1<id<5 for the interval (1,5), None in case of top or bottom
    */
  def asConstraint(id: Identifier): Option[Expression]

  def getPossibleConstants: SetDomain.Default[Constant]

}

object NonRelationalNumericalDomain {

  trait Bottom[S <: NonRelationalNumericalDomain[S]]
    extends NonRelationalNumericalDomain[S]
      with Lattice.Bottom[S] {
    this: S =>

    def multiply(rightExpr: S) = this

    def divide(rightExpr: S) = this

    def subtract(rightExpr: S) = this

    def sum(rightExpr: S) = this

    def valueGEQ: S = this

    def valueLEQ: S = this

    def valueLess: S = this

    def valueGreater: S = this

    def overlapsWith(value: S) = false

    override def asConstraint(id: Identifier): Option[Expression] = None // TODO: False

    def getPossibleConstants = SetDomain.Default.Top()

  }

  trait Top[S <: NonRelationalNumericalDomain[S]]
    extends NonRelationalNumericalDomain[S]
      with Lattice.Top[S] {
    this: S =>

    /** Gives the element representing exactly Zero */
    def zero: S

    def multiply(rightExpr: S) = if (rightExpr == zero) zero else this

    def divide(rightExpr: S) = if (rightExpr == zero) bottom() else this

    def subtract(rightExpr: S) = this

    def sum(rightExpr: S) = this

    def valueGEQ: S = this

    def valueLEQ: S = this

    def valueLess: S = this

    def valueGreater: S = this

    def overlapsWith(value: S) = true

    override def asConstraint(id: Identifier): Option[Expression] = None

    def getPossibleConstants = SetDomain.Default.Top()

  }

  trait Inner[S <: NonRelationalNumericalDomain[S], I <: Inner[S, I]]
    extends NonRelationalNumericalDomain[S]
      with Lattice.Inner[S, I] {
    this: S =>

  }

}


sealed trait Sign extends NonRelationalNumericalDomain[Sign] {

  final override def factory() = top()

  def top(): Sign = Sign.Top

  def bottom(): Sign = Sign.Bottom

  def evalConstant(value: Constant): Sign = {
    try {
      evalConstant(value.constant.toInt)
    } catch {
      case e: NumberFormatException => top()
    }
  }

  def evalConstant(value: Double): Sign = {
    if (value > 0) Sign.Plus
    else if (value == 0) Sign.Zero
    else Sign.Minus
  }

}

object Sign {

  sealed trait Inner extends Sign with NonRelationalNumericalDomain.Inner[Sign, Inner] {

    def overlapsWith(other: Sign): Boolean = other match {
      case Bottom => false
      case Top => true
      case c: Inner => c == this
    }

    def lubInner(other: Inner): Sign = if (other == this) this else Top

    def glbInner(other: Inner): Sign = if (other == this) this else Top

    def wideningInner(other: Inner): Sign = lub(other)

    def lessEqualInner(other: Inner): Boolean = other == this

  }

  object Top extends Sign with NonRelationalNumericalDomain.Top[Sign] {

    val zero = Zero

  }

  object Bottom extends Sign with NonRelationalNumericalDomain.Bottom[Sign]

  object Plus extends Inner {

    def sum(other: Sign): Sign = other match {
      case Zero => Plus
      case Minus => Top
      case _ => other
    }

    def subtract(other: Sign): Sign = other match {
      case Zero => Plus
      case Plus => Top
      case Minus => Plus
      case _ => other
    }

    def multiply(other: Sign): Sign =
      other

    def divide(other: Sign): Sign = other match {
      case Zero => Bottom // division by zero
      case Plus => Plus
      case _ => other
    }

    def valueGEQ = Plus

    def valueLEQ = Top

    def valueLess = Top

    def valueGreater = Plus

    override def toString = "+"

    override def asConstraint(id: Identifier): Option[Expression] =
      Some(BinaryArithmeticExpression(id, Constant("0", id.typ)(), ArithmeticOperator.>))

    def getPossibleConstants = SetDomain.Default.Top()
  }

  object Minus extends Inner {

    def sum(other: Sign): Sign = other match {
      case Zero => Minus
      case Plus => Top
      case _ => other
    }

    def subtract(other: Sign): Sign = other match {
      case Zero => Minus
      case Plus => Minus
      case Minus => Top
      case _ => other
    }

    def multiply(other: Sign): Sign = other match {
      case Plus => Minus
      case Minus => Plus
      case _ => other
    }

    def divide(other: Sign): Sign = other match {
      case Zero => Bottom // division by zero
      case Plus => Minus
      case Minus => Plus
      case _ => other
    }

    def valueGEQ = Top

    def valueLEQ = Minus

    def valueLess = Minus

    def valueGreater = Top

    override def toString = "-"

    override def asConstraint(id: Identifier): Option[Expression] =
      Some(BinaryArithmeticExpression(id, Constant("0", id.typ)(), ArithmeticOperator.<))

    def getPossibleConstants = SetDomain.Default.Top()

  }

  object Zero extends Inner {

    def sum(other: Sign): Sign = other

    def subtract(other: Sign): Sign = other match {
      case Plus => Minus
      case Minus => Plus
      case _ => other
    }

    def multiply(other: Sign): Sign = other match {
      case Bottom => Bottom
      case _ => Zero
    }

    def divide(other: Sign): Sign = other match {
      case Bottom => Bottom
      case Zero => Bottom // division by zero
      case _ => Zero
    }

    def valueGEQ = Top

    def valueLEQ = Top

    def valueLess = Minus

    def valueGreater = Plus

    override def toString = "0"

    override def asConstraint(id: Identifier): Option[Expression] =
      Some(BinaryArithmeticExpression(id, Constant("0", id.typ)(), ArithmeticOperator.==))

    def getPossibleConstants = SetDomain.Default.Inner(Set(Constant("0", SystemParameters.tm.Int)()))

  }

}

sealed trait IntegerInterval extends NonRelationalNumericalDomain[IntegerInterval] {

  final override def factory() = top()

  def top() = IntegerInterval.Top

  def bottom() = IntegerInterval.Bottom

  def evalConstant(value: Constant): IntegerInterval = {
    try {
      evalConstant(value.constant.toInt)
    } catch {
      case e: NumberFormatException => top()
    }
  }

  def evalConstant(value: Double): IntegerInterval =
    IntegerInterval.Inner(value.toInt, value.toInt)

}

object IntegerInterval {


  case class Inner(left: Int, right: Int)
    extends IntegerInterval
      with NonRelationalNumericalDomain.Inner[IntegerInterval, Inner] {

    if (SystemParameters.DEBUG) {
      assert(left <= right)
      assert(left != Int.MinValue || right != Int.MaxValue)
    }

    def lubInner(other: Inner) = other match {
      case Inner(oLeft, oRight) =>
        factory(Math.min(left, oLeft), Math.max(right, oRight))
    }

    def glbInner(other: Inner) = other match {
      case Inner(oLeft, oRight) =>
        factory(Math.max(left, oLeft), Math.min(right, oRight))
    }

    def wideningInner(other: Inner) = other match {
      case Inner(oLeft, oRight) =>
        val l = if (oLeft < left) Int.MinValue else Math.min(left, oLeft)
        val r = if (oRight > right) Int.MaxValue else Math.max(right, oRight)
        factory(l, r)
    }

    def lessEqualInner(other: Inner): Boolean = other match {
      case Inner(oLeft, oRight) =>
        left >= oLeft && right <= oRight
    }

    def sum(other: IntegerInterval): IntegerInterval = other match {
      case Bottom => Bottom
      case Top => Top
      case Inner(oLeft, oRight) =>
        val newLeft =
          if (left == Int.MinValue || oLeft == Int.MinValue || (left + oLeft).toLong != left.toLong + oLeft.toLong) Int.MinValue
          else left + oLeft
        val newRight =
          if (right == Int.MaxValue || oRight == Int.MaxValue || (right + oRight).toLong != right.toLong + oRight.toLong) Int.MaxValue
          else right + oRight
        factory(newLeft, newRight)
    }

    def subtract(other: IntegerInterval): IntegerInterval = other match {
      case Bottom => Bottom
      case Top => Top
      case Inner(oLeft, oRight) =>
        val newLeft =
          if (left == Int.MinValue || oRight == Int.MaxValue || (left - oRight).toLong != left.toLong - oRight.toLong) Int.MinValue
          else left - oRight
        val newRight =
          if (right == Int.MaxValue || oLeft == Int.MinValue || (right - oLeft).toLong != right.toLong - oLeft.toLong) Int.MaxValue
          else right - oLeft
        factory(newLeft, newRight)
    }

    def multiply(other: IntegerInterval): IntegerInterval = other match {
      case Bottom => Bottom
      case Top => Top
      case Inner(oLeft, oRight) =>
        val a = managedMultiply(left, oLeft)
        val b = managedMultiply(left, oRight)
        val c = managedMultiply(right, oLeft)
        val d = managedMultiply(right, oRight)
        factory(min(a, b, c, d), max(a, b, c, d))
    }

    private def managedMultiply(a: Int, b: Int): Int = {
      val result = a.toLong * b.toLong
      if (result.toInt == result) result.toInt
      else if (result > 0) Int.MaxValue else Int.MinValue
    }

    private def max(a: Int, b: Int, c: Int, d: Int): Int = Math.max(Math.max(a, b), Math.max(c, d))

    private def min(a: Int, b: Int, c: Int, d: Int): Int = Math.min(Math.min(a, b), Math.min(c, d))

    def divide(other: IntegerInterval): IntegerInterval = other match {
      case Bottom => Bottom
      case Top => Top
      case Inner(oLeft, oRight) =>
        if (oLeft < 0 && 0 < oRight) {
          val lower = factory(oLeft, 0)
          val upper = factory(0, oRight)
          (this divide lower) lub (this divide upper)
        } else {
          val a = left / (if (oLeft == 0) 1 else oLeft)
          val b = left / (if (oRight == 0) -1 else oRight)
          val c = right / (if (oLeft == 0) 1 else oLeft)
          val d = right / (if (oRight == 0) -1 else oRight)
          Inner(min(a, b, c, d), max(a, b, c, d))
        }
    }

    def valueGEQ: IntegerInterval = factory(left, Int.MaxValue)

    def factory(newLeft: Int, newRight: Int) = {
      if (newLeft == Int.MinValue && newRight == Int.MaxValue) Top
      else if (newLeft > newRight) Bottom
      else Inner(newLeft, newRight)
    }

    def valueLEQ: IntegerInterval = factory(Int.MinValue, right)

    def valueGreater: IntegerInterval = factory(left + 1, Int.MaxValue)

    def valueLess: IntegerInterval = factory(Int.MinValue, right - 1)

    /** Returns the constraint expressed by this interval on a given identifier, if some (None if unconstrained) */
    override def asConstraint(id: Identifier): Option[Expression] = {
      if (this.isBottom) return None
      if (left == Int.MinValue && right == Int.MaxValue) return None
      val lowerBound = BinaryArithmeticExpression(Constant(left.toString, id.typ)(), id, ArithmeticOperator.<=)
      val upperBound = BinaryArithmeticExpression(id, Constant(right.toString, id.typ)(), ArithmeticOperator.<=)
      if (right == Int.MaxValue) return Some(lowerBound)
      if (left == Int.MinValue) return Some(upperBound)
      Some(BinaryBooleanExpression(lowerBound, upperBound, BooleanOperator.&&))
    }

    def overlapsWith(other: IntegerInterval): Boolean = other match {
      case Bottom => false
      case Top => true
      case Inner(oLeft, oRight) =>
        !(this.right < oLeft || oRight < this.left)
    }

    override def toString: String =
      "[" +
        (if (left == Int.MinValue) "-oo" else left.toString) +
        ".." +
        (if (right == Int.MaxValue) "+oo" else right.toString) +
        "]"

    def getPossibleConstants = {
      if (left == right)
        SetDomain.Default.Inner(Set(Constant(left.toString, SystemParameters.tm.Int)()))
      else
        SetDomain.Default.Top()
    }

  }

  object Top extends IntegerInterval with NonRelationalNumericalDomain.Top[IntegerInterval] {

    override val zero: IntegerInterval = Inner(0, 0)

  }

  object Bottom extends IntegerInterval with NonRelationalNumericalDomain.Bottom[IntegerInterval]

}

sealed trait DoubleInterval extends NonRelationalNumericalDomain[DoubleInterval] {

  def factory() = DoubleInterval.Top

  def top() = DoubleInterval.Top

  def bottom() = DoubleInterval.Bottom

  def evalConstant(value: Constant): DoubleInterval = {
    try {
      evalConstant(value.constant.toDouble)
    } catch {
      case e: NumberFormatException => DoubleInterval.Top
    }
  }

  def evalConstant(value: Double): DoubleInterval =
    if (value == 0) DoubleInterval.Zero
    else DoubleInterval.Inner(value, value)

}

object DoubleInterval {

  val Zero = Inner(0, 0)

  case class Inner(left: Double, right: Double)
    extends DoubleInterval
      with NonRelationalNumericalDomain.Inner[DoubleInterval, Inner] {

    if (SystemParameters.DEBUG) {
      assert {
        left <= right
      }
      assert {
        !right.isPosInfinity || !left.isNegInfinity
      }
      assert {
        !left.isNaN && !right.isNaN
      }
      assert {
        !right.isNegInfinity
      }
      assert {
        !left.isPosInfinity
      }
    }

    def overlapsWith(other: DoubleInterval): Boolean =
      other match {
        case Bottom => false
        case Top => true
        case Inner(oLeft, oRight) =>
          !(this.right < oLeft || oRight < this.left)
      }

    def lubInner(other: Inner) =
      other match {
        case Inner(oLeft, oRight) =>
          factory(Math.min(left, oLeft), Math.max(right, oRight))
      }

    def factory(newLeft: Double, newRight: Double): DoubleInterval = {
      if (newLeft.isNegInfinity && newRight.isPosInfinity) Top
      else if (newLeft > newRight) Bottom
      else Inner(newLeft, newRight)
    }

    def glbInner(other: Inner) =
      other match {
        case Inner(oLeft, oRight) =>
          factory(Math.max(left, oLeft), Math.min(right, oRight))
      }

    def wideningInner(other: Inner) =
      other match {
        case Inner(oLeft, oRight) =>
          val l = if (oLeft < this.left) Double.NegativeInfinity else Math.min(this.left, oLeft)
          val r = if (oRight > this.right) Double.PositiveInfinity else Math.max(this.right, oRight)
          factory(l, r)
      }

    def lessEqualInner(other: Inner): Boolean =
      other match {
        case Inner(oLeft, oRight) =>
          this.left >= oLeft && this.right <= oRight
      }

    def sum(other: DoubleInterval) =
      other match {
        case Bottom => Bottom
        case Top => Top
        case Inner(oLeft, oRight) =>
          factory(left + oLeft, right + oRight)
      }

    def subtract(other: DoubleInterval) =
      other match {
        case Bottom => Bottom
        case Top => Top
        case Inner(oLeft, oRight) =>
          factory(left - oRight, right - oLeft)
      }

    def multiply(other: DoubleInterval) =
      other match {
        case Bottom => Bottom
        case Top => if (this == DoubleInterval.Zero) DoubleInterval.Zero else Top
        case Inner(oLeft, oRight) =>
          if (this == DoubleInterval.Zero || other == DoubleInterval.Zero) DoubleInterval.Zero
          else {
            val a = left * oLeft
            val b = left * oRight
            val c = right * oLeft
            val d = right * oRight
            if (a.isNaN || b.isNaN || c.isNaN || d.isNaN) top() // loses prec.
            else factory(min(a, b, c, d), max(a, b, c, d))
          }
      }

    private def max(a: Double, b: Double, c: Double, d: Double): Double =
      Math.max(Math.max(a, b), Math.max(c, d))

    private def min(a: Double, b: Double, c: Double, d: Double): Double =
      Math.min(Math.min(a, b), Math.min(c, d))

    def divide(other: DoubleInterval): DoubleInterval =
      other match {
        case Bottom => Bottom
        case Top => Top
        case DoubleInterval.Zero => Bottom
        case Inner(oLeft, oRight) =>
          if (oLeft < 0 && 0 < oRight) {
            val lower = factory(oLeft, 0)
            val upper = factory(0, oRight)
            (this divide lower) lub (this divide upper)
          } else {
            val a = if (oLeft == 0) infinitySign(left) else left / oLeft
            val b = if (oRight == 0) invertedInfinitySign(left) else left / oRight
            val c = if (oLeft == 0) infinitySign(right) else right / oLeft
            val d = if (oRight == 0) invertedInfinitySign(right) else right / oRight
            factory(min(a, b, c, d), max(a, b, c, d))
          }
      }

    def valueGEQ = factory(left, Double.PositiveInfinity)

    def valueLEQ = factory(Double.NegativeInfinity, right)

    def valueGreater = factory(left + NumericalAnalysisConstants.epsilon, Double.PositiveInfinity)

    def valueLess = factory(Double.NegativeInfinity, right - NumericalAnalysisConstants.epsilon)

    /** Returns the constraint expressed by this interval on a given identifier, if some (None if unconstrained) */
    override def asConstraint(id: Identifier): Option[Expression] = {
      val lowerBound = BinaryArithmeticExpression(Constant(left.toString, id.typ)(), id, ArithmeticOperator.<=)
      val upperBound = BinaryArithmeticExpression(id, Constant(right.toString, id.typ)(), ArithmeticOperator.<=)
      if (right == Double.PositiveInfinity) return Some(lowerBound)
      if (left == Double.NegativeInfinity) return Some(upperBound)
      Some(BinaryBooleanExpression(lowerBound, upperBound, BooleanOperator.&&))
    }

    def getPossibleConstants = {
      if (left == right)
        SetDomain.Default.Inner(Set(Constant(left.toString, SystemParameters.tm.Int)()))
      else
        SetDomain.Default.Top()
    }

    override def toString: String =
      "[" +
        (if (left == Double.NegativeInfinity) "-oo" else left.toString) +
        ".." +
        (if (right == Double.PositiveInfinity) "+oo" else right.toString) +
        "]"

    private def infinitySign(d: Double): Double =
      if (d > 0) Double.PositiveInfinity else if (d < 0) Double.NegativeInfinity else 0.0

    private def invertedInfinitySign(d: Double): Double =
      if (d < 0) Double.PositiveInfinity else if (d > 0) Double.NegativeInfinity else 0.0

  }

  object Top extends DoubleInterval with NonRelationalNumericalDomain.Top[DoubleInterval] {

    override def zero = DoubleInterval.Zero

  }

  object Bottom extends DoubleInterval with NonRelationalNumericalDomain.Bottom[DoubleInterval]

}


class NonRelationalNumericalAnalysis[D <: NonRelationalNumericalDomain[D]] extends SemanticAnalysis[BoxedNonRelationalNumericalDomain[D]] {
  var domain: NonRelationalNumericalDomain[D] = null

  def getLabel(): String = "Numerical nonrelational analysis"

  def parameters(): List[(String, Any)] = List(("Domain", List("Sign", "Interval", "DoubleInterval")))

  def setParameter(label: String, value: Any) = label match {
    case "Domain" => value match {
      case "Sign" => domain = Sign.Top.asInstanceOf[D]
      case "Interval" => domain = IntegerInterval.Top.asInstanceOf[D]
      case "DoubleInterval" => domain = DoubleInterval.Top.asInstanceOf[D]
    }
  }

  def getInitialState(): BoxedNonRelationalNumericalDomain[D] = ???
    // new BoxedNonRelationalNumericalDomain(domain).asInstanceOf[BoxedNonRelationalNumericalDomain[D]]

  override def reset(): Unit = ()

  def getProperties: List[Property] = List(SingleStatementProperty.Default(DivisionByZero))

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil
}