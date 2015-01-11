package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property.{DivisionByZero, SingleStatementProperty, Property}
import ch.ethz.inf.pm.sample.abstractdomain._

import scala.collection.JavaConverters

trait NonRelationalNumericalDomain[N <: NonRelationalNumericalDomain[N]] extends Lattice[N] {
  this: N =>

  def evalConstant(value: Int): N

  def evalConstant(value: Constant): N

  def sum(leftExpr: N, rightExpr: N): N

  def subtract(leftExpr: N, rightExpr: N): N

  def multiply(leftExpr: N, rightExpr: N): N

  def divide(leftExpr: N, rightExpr: N): N

  def valueGEQ(value: N): N

  def valueLEQ(value: N): N

  def valueLess(value: N): N

  def valueGreater(value: N): N

  def overlapsWith(value: N): Boolean

  /**
   * Encodes non-relational information as a constraint
   * @param id the identifier this domain restricts
   * @return a constraint, for example 1<id<5 for the interval (1,5), None in case of top or bottom
   */
  def asConstraint(id: Identifier): Option[Expression] = ???

}

case class BoxedNonRelationalNumericalDomain[N <: NonRelationalNumericalDomain[N]](dom: N,
                                                                              map: Map[Identifier, N] = Map.empty[Identifier, N],
                                                                              override val isBottom: Boolean = false,
                                                                              isTop: Boolean = false)
  extends BoxedDomain[N, BoxedNonRelationalNumericalDomain[N]]
  with NumericalDomain[BoxedNonRelationalNumericalDomain[N]]
  with SimplifiedSemanticDomain[BoxedNonRelationalNumericalDomain[N]] {

  def functionalFactory(_value: Map[Identifier, N] = Map.empty[Identifier, N], _isBottom: Boolean = false, _isTop: Boolean = false): BoxedNonRelationalNumericalDomain[N] =
    new BoxedNonRelationalNumericalDomain[N](dom, _value, _isBottom, _isTop)

  def get(key: Identifier): N = map.get(key) match {
    case None => dom.bottom()
    case Some(x) => x
  }


  override def createVariable(variable: Identifier, typ: Type): BoxedNonRelationalNumericalDomain[N] = {
    if (variable.typ.isNumericalType) {
      return this.add(variable, dom.top())
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

  override def removeVariable(variable: Identifier): BoxedNonRelationalNumericalDomain[N] = {
    this.remove(variable)
  }

  override def setArgument(variable: Identifier, expr: Expression): BoxedNonRelationalNumericalDomain[N] = this.assign(variable, expr)

  override def assign(variable: Identifier, expr: Expression): BoxedNonRelationalNumericalDomain[N] = {
    if (variable.typ.isNumericalType) {
      if (variable.representsSingleVariable)
        this.add(variable, eval(expr))
      else this.add(variable, this.get(variable).lub(eval(expr)))
    } else this
  }

  override def backwardAssign(oldPreState: BoxedNonRelationalNumericalDomain[N], variable: Identifier, expr: Expression): BoxedNonRelationalNumericalDomain[N] = {
    ??? // only support for apron domains
  }

  override def access(field: Identifier) = this

  override def backwardAccess(field: Identifier) = this

  def eval(expr: Expression): N = expr match {
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.+, typ) => return dom.sum(eval(left), eval(right))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.*, typ) => return dom.multiply(eval(left), eval(right))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator./, typ) => return dom.divide(eval(left), eval(right))
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.-, typ) => return dom.subtract(eval(left), eval(right))
    case BinaryArithmeticExpression(left, right, op, typ) => dom.top()
    case c@Constant(constant, typ, pp) => dom.evalConstant(c)
    case x: Identifier => this.get(x)
    case xs: HeapIdSetDomain[_] =>
      var result = dom.bottom()
      for (x <- xs.value) result = result.lub(this.get(x))
      result
    case x: Expression => dom.top()
  }

  override def assume(expr: Expression): BoxedNonRelationalNumericalDomain[N] =

    expr match {

      // Boolean variables
      case x: Identifier =>
        assert(x.typ.isBooleanType)
        assume(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.!=))
      case NegatedBooleanExpression(x: Identifier) =>
        assume(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.==))

      // And and Or
      case BinaryBooleanExpression(left, right, op, _) => op match {
        case BooleanOperator.&& => assume(left).assume(right)
        case BooleanOperator.|| => assume(left).lub(assume(right))
      }

      // Double-Negation + De-Morgan
      case NegatedBooleanExpression(NegatedBooleanExpression(x)) =>
        assume(x)
      case NegatedBooleanExpression(BinaryBooleanExpression(left, right, op, typ)) =>
        val nl = NegatedBooleanExpression(left)
        val nr = NegatedBooleanExpression(right)
        val nop = op match {
          case BooleanOperator.&& => BooleanOperator.||
          case BooleanOperator.|| => BooleanOperator.&&
        }
        assume(BinaryBooleanExpression(nl, nr, nop, typ))

      // Convert double inequality
      case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, ArithmeticOperator.==, typ)) =>
        val newLeft = BinaryArithmeticExpression(left, right, ArithmeticOperator.>, typ)
        val newRight = BinaryArithmeticExpression(left, right, ArithmeticOperator.<, typ)
        assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||, typ))
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.!=, typ) =>
        val newLeft = BinaryArithmeticExpression(left, right, ArithmeticOperator.>, typ)
        val newRight = BinaryArithmeticExpression(left, right, ArithmeticOperator.<, typ)
        assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||, typ))

      // Handling of monomes
      case _ => Normalizer.conditionalExpressionToMonomes(expr) match {
        case None =>
          expr match {
            case BinaryArithmeticExpression(left, right, op, typ) =>
              val l: N = this.eval(left)
              val r: N = this.eval(right)
              op match {
                case ArithmeticOperator.== => if (!l.overlapsWith(r)) return this.bottom()
                case ArithmeticOperator.<= => if (!l.overlapsWith(l.valueLEQ(r))) return this.bottom()
                case ArithmeticOperator.>= => if (!l.overlapsWith(l.valueGEQ(r))) return this.bottom()
                case ArithmeticOperator.> => if (!l.overlapsWith(l.valueGreater(r))) return this.bottom()
                case ArithmeticOperator.< => if (!l.overlapsWith(l.valueLess(r))) return this.bottom()
                case _ => return this
              }
            case _ => return this
          }
          return this
        case Some((monomes, constant)) =>
          var stateResult: BoxedNonRelationalNumericalDomain[N] = this

          // Check if it is trivially false, e.g. -1 >= 0
          if (monomes.isEmpty && constant < 0)
            return stateResult.bottom()

          for (monome <- monomes) {
            val (index, variable) = monome
            var result = dom.evalConstant(constant)
            for (monome1 <- monomes) {
              if (!monome.equals(monome1))
                result = dom.sum(result, dom.multiply(dom.evalConstant(monome1._1), eval(monome1._2)))
            }
            if (index >= 0) {
              //k*x+n >= 0 => x >= -(n/k)
              result = dom.divide(result, dom.evalConstant(index))
              result = dom.subtract(dom.evalConstant(0), result)
              val newValue = get(variable).glb(dom.valueGEQ(result))
              if (newValue.lessEqual(newValue.bottom()))
                return stateResult.bottom()
              stateResult = stateResult.add(variable, newValue)
            }
            else {
              //-k*x+n >= 0 => x <= n/-k
              result = dom.divide(result, dom.evalConstant(-index))
              val oldValue = this.get(variable)
              val newRestraint = dom.valueLEQ(result)
              val newValue = oldValue.glb(newRestraint)
              if (newValue.lessEqual(newValue.bottom()))
                return stateResult.bottom()
              stateResult = stateResult.add(variable, newValue)
            }
          }
          return stateResult
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

}


class Top extends NonRelationalNumericalDomain[Top] {
  final override def factory() = this

  override def toString = "⊤"

  def top(): Top = this

  def bottom(): Top = this

  def lub(other: Top): Top = this

  def glb(other: Top): Top = this

  def widening(other: Top): Top = this

  def lessEqual(other: Top): Boolean = true

  override def equals(o: Any) = true

  def evalConstant(value: Int): Top = this

  def evalConstant(value: Constant): Top = this

  def sum(leftExpr: Top, rightExpr: Top): Top = this

  def subtract(leftExpr: Top, rightExpr: Top): Top = this

  def multiply(leftExpr: Top, rightExpr: Top): Top = this

  def divide(leftExpr: Top, rightExpr: Top): Top = this

  def valueGEQ(value: Top): Top = this

  def valueLEQ(value: Top): Top = this

  def valueLess(value: Top): Top = this

  def valueGreater(value: Top): Top = this

  def overlapsWith(value: Top): Boolean = true

}


object SignValues extends Enumeration {
  val + = Value("+")
  val ZERO = Value("0")
  val - = Value("-")
  val T = Value("⊤")
  val BOT = Value("⊥")
}

class Sign(val value: SignValues.Value) extends NonRelationalNumericalDomain[Sign] {

  def overlapsWith(value: Sign): Boolean = {
    if (this.value == SignValues.BOT || value.value == SignValues.BOT) return false
    if (this.value == SignValues.T || value.value == SignValues.T) return true
    if (this.value == value.value) return true
    else return false
  }

  final override def factory() = top()

  override def toString = value.toString

  def top(): Sign = new Sign(SignValues.T)

  def bottom(): Sign = new Sign(SignValues.BOT)

  def lub(other: Sign): Sign = {
    if (value == SignValues.T || other.value == SignValues.T) return top()
    if (value == SignValues.BOT) return other
    if (other.value == SignValues.BOT) return this
    if (equals(other)) return this
    return top()
  }

  def glb(other: Sign): Sign = {
    if (value == SignValues.BOT || other.value == SignValues.BOT) return bottom()
    if (value == SignValues.T) return other
    if (other.value == SignValues.T) return this
    if (equals(other)) return this
    return bottom()
  }

  def widening(other: Sign): Sign = lub(other)

  def lessEqual(right: Sign): Boolean = {
    val left: Sign = this
    if (value == SignValues.BOT || right.value == SignValues.T) return true
    if (right.equals(left)) return true
    return false
  }

  override def equals(o: Any) = o match {
    case x: Sign => x.value.equals(value)
    case _ => false
  }

  def sum(leftExpr: Sign, rightExpr: Sign): Sign = {
    if (leftExpr.value == SignValues.BOT || rightExpr.value == SignValues.BOT) return new Sign(SignValues.BOT)
    if (leftExpr.value == SignValues.T || rightExpr.value == SignValues.T) return new Sign(SignValues.T)
    if (leftExpr.value == rightExpr.value) return new Sign(leftExpr.value)
    if (leftExpr.value == SignValues.ZERO) return new Sign(rightExpr.value)
    if (rightExpr.value == SignValues.ZERO) return new Sign(leftExpr.value)
    return new Sign(SignValues.T)
  }

  def subtract(leftExpr: Sign, rightExpr: Sign): Sign = {
    if (leftExpr.value == SignValues.BOT || rightExpr.value == SignValues.BOT) return new Sign(SignValues.BOT)
    if (leftExpr.value == SignValues.T || rightExpr.value == SignValues.T) return new Sign(SignValues.T)
    if (leftExpr.value == SignValues.ZERO) {
      if (rightExpr.value == SignValues.ZERO) return new Sign(SignValues.ZERO)
      else if (rightExpr.value == SignValues.+) return new Sign(SignValues.-)
      else return new Sign(SignValues.+)
    }
    if (leftExpr.value != rightExpr.value && leftExpr.value == SignValues.+) return new Sign(SignValues.+)
    if (leftExpr.value != rightExpr.value && leftExpr.value == SignValues.-) return new Sign(SignValues.-)
    return new Sign(SignValues.T)
  }

  def multiply(leftExpr: Sign, rightExpr: Sign): Sign = {
    if (leftExpr.value == SignValues.BOT || rightExpr.value == SignValues.BOT) return new Sign(SignValues.BOT)
    if (leftExpr.value == SignValues.ZERO || leftExpr.value == SignValues.ZERO) return new Sign(SignValues.ZERO)
    if (leftExpr.value == SignValues.T || rightExpr.value == SignValues.T) return new Sign(SignValues.T)
    if (leftExpr.value == rightExpr.value) return new Sign(SignValues.+)
    return new Sign(SignValues.-)
  }

  def divide(leftExpr: Sign, rightExpr: Sign): Sign = {
    if (leftExpr.value == SignValues.BOT || rightExpr.value == SignValues.BOT || rightExpr.value == SignValues.ZERO) return new Sign(SignValues.BOT)
    if (leftExpr.value == SignValues.ZERO) return new Sign(SignValues.ZERO)
    if (leftExpr.value == SignValues.T || rightExpr.value == SignValues.T) return new Sign(SignValues.T)
    if (leftExpr.value == rightExpr.value) return new Sign(SignValues.+)
    return new Sign(SignValues.-)
  }

  def evalConstant(value: Int): Sign = {
    if (value > 0) return new Sign(SignValues.+)
    else if (value == 0) return new Sign(SignValues.ZERO)
    else return new Sign(SignValues.-)
  }

  def evalConstant(value: Constant): Sign = {
    try {
      val convertedVal: Int = Integer.valueOf(value.constant).intValue()
      evalConstant(convertedVal)
    } catch {
      case e: NumberFormatException => top()
    }
  }

  def valueGEQ(value: Sign): Sign = {
    if (value.value == SignValues.+)
      return new Sign(SignValues.+)
    else return new Sign(SignValues.T)
  }

  def valueLEQ(value: Sign): Sign = {
    if (value.value == SignValues.-)
      return new Sign(SignValues.-)
    else return this
  }

  def valueLess(value: Sign): Sign = {
    if (value.value == SignValues.- || value.value == SignValues.ZERO)
      return new Sign(SignValues.-)
    else return this
  }

  def valueGreater(value: Sign): Sign = {
    if (value.value == SignValues.+ || value.value == SignValues.ZERO)
      return new Sign(SignValues.+)
    else return this
  }

}



case class Interval(left: Int, right: Int) extends NonRelationalNumericalDomain[Interval] {

  final override def factory() = top()

  def top(): Interval = new Interval(Integer.MIN_VALUE, Integer.MAX_VALUE)

  def bottom(): Interval = new Interval(1, 0)

  def isBottom: Boolean = left > right

  /** Returns the constraint expressed by this interval on a given identifier, if some (None if unconstrained) */
  override def asConstraint(id: Identifier): Option[Expression] = {
    if (this.isBottom) return None
    if (left == Integer.MIN_VALUE && right == Integer.MAX_VALUE) return None
    val lowerBound = BinaryArithmeticExpression(Constant(left.toString, id.typ), id, ArithmeticOperator.<=)
    val upperBound = BinaryArithmeticExpression(id, Constant(right.toString, id.typ), ArithmeticOperator.<=)
    if (right == Integer.MAX_VALUE) return Some(lowerBound)
    if (left == Integer.MIN_VALUE) return Some(upperBound)
    Some(BinaryBooleanExpression(lowerBound, upperBound, BooleanOperator.&&))
  }

  def overlapsWith(value: Interval): Boolean = {
    if (this.right < value.left || value.right < this.left) return false
    else return true
  }


  def isTop = left == Integer.MIN_VALUE && right == Integer.MAX_VALUE

  override def toString: String = {
    if (this.isBottom) return "⊥"
    if (this.isTop) return "T"
    var result: String = "["
    if (left == Integer.MIN_VALUE)
      result = result + "-oo"
    else result = result + left.toString
    result = result + ".."
    if (right == Integer.MAX_VALUE)
      result = result + "+oo"
    else result = result + right.toString
    result + "]"
  }


  private def min(left: Int, right: Int) = if (left < right) left else right

  private def max(left: Int, right: Int) = if (left > right) left else right

  def lub(other: Interval): Interval = {
    if (isBottom) return other
    if (other.isBottom) return this
    return new Interval(min(left, other.left), max(right, other.right))
  }


  def glb(other: Interval): Interval = {
    if (isBottom || other.isBottom) return bottom()
    return new Interval(max(left, other.left), min(right, other.right))
  }

  def widening(other: Interval): Interval = {
    var result = lub(other)
    if (other.left < left)
      result = new Interval(Integer.MIN_VALUE, result.right)
    if (other.right > right)
      result = new Interval(result.left, Integer.MAX_VALUE)
    result
  }

  def lessEqual(other: Interval): Boolean = {
    val that: Interval = this
    if (that.isBottom) return true
    if (other.isBottom) return false
    if (that.left >= other.left && that.right <= other.right)
      return true
    else return false
  }

  override def equals(o: Any) = o match {
    case x: Interval => x.left == this.left && x.right == this.right
    case _ => false
  }

  def evalConstant(value: Int): Interval = {
     return new Interval(value, value)
  }

  def evalConstant(value: Constant): Interval = {
    try {
      evalConstant(Integer.valueOf(value.constant).intValue())
    } catch {
      case e: NumberFormatException => top()
    }
  }

  def sum(leftExpr: Interval, rightExpr: Interval): Interval = {
    if (leftExpr.isBottom || rightExpr.isBottom) return new Interval(1, 0)
    var left = leftExpr.left + rightExpr.left
    var right = leftExpr.right + rightExpr.right
    if (leftExpr.left == Integer.MIN_VALUE || rightExpr.left == Integer.MIN_VALUE)
      left = Integer.MIN_VALUE
    if (leftExpr.right == Integer.MAX_VALUE || rightExpr.right == Integer.MAX_VALUE || (rightExpr.right + leftExpr.right) < rightExpr.right) //Last case for overflow
      right = Integer.MAX_VALUE
    return new Interval(left, right)

  }

  def subtract(leftExpr: Interval, rightExpr: Interval): Interval = {
    if (leftExpr.isBottom || rightExpr.isBottom) return new Interval(1, 0)
    var left = leftExpr.left - rightExpr.left
    var right = leftExpr.right - rightExpr.right
    if (leftExpr.left == Integer.MIN_VALUE || rightExpr.left == Integer.MIN_VALUE || (rightExpr.right - leftExpr.right) > rightExpr.right) //Last case for underflow
      left = Integer.MIN_VALUE
    if (leftExpr.right == Integer.MAX_VALUE || rightExpr.right == Integer.MAX_VALUE)
      right = Integer.MAX_VALUE
    return new Interval(left, right)
  }

  private def max(a: Int, b: Int, c: Int, d: Int): Int = max(max(a, b), max(c, d))

  private def min(a: Int, b: Int, c: Int, d: Int): Int = min(min(a, b), min(c, d))

  private def managedMultiply(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) return 0
    var result: Int = a * b
    if (result / a != b) {
      //Overflow
      if (a >= 0 && b >= 0) result = Integer.MAX_VALUE
      else if (a <= 0 && b <= 0) result = Integer.MAX_VALUE
      else result = Integer.MIN_VALUE
    }
    return result
  }

  def multiply(leftExpr: Interval, rightExpr: Interval): Interval = {
    val a = managedMultiply(leftExpr.left, rightExpr.left)
    val b = managedMultiply(leftExpr.left, rightExpr.right)
    val c = managedMultiply(leftExpr.right, rightExpr.left)
    val d = managedMultiply(leftExpr.right, rightExpr.right)
    val result = new Interval(min(a, b, c, d), max(a, b, c, d))
    return result
  }

  def divide(leftExpr: Interval, rightExpr: Interval): Interval = {
    if (rightExpr.left == 0 && rightExpr.right == 0) return leftExpr.bottom()
    val a = leftExpr.left / (if (rightExpr.left == 0) 1 else rightExpr.left)
    val b = leftExpr.left / (if (rightExpr.right == 0) 0 - 1 else rightExpr.right)
    val c = leftExpr.right / (if (rightExpr.left == 0) 1 else rightExpr.left)
    val d = leftExpr.right / (if (rightExpr.right == 0) 0 - 1 else rightExpr.right)
    var result = new Interval(min(a, b, c, d), max(a, b, c, d))
    if (leftExpr.left < 0 && leftExpr.right > 0) //It contains 0
      result = result.lub(new Interval(0, 0))
    if (rightExpr.left < 0 && rightExpr.right > 0) {
      //It contains 0
      if (leftExpr.right > 0)
        result = new Interval(result.left, Integer.MAX_VALUE)
      if (leftExpr.left < 0)
        result = new Interval(Integer.MIN_VALUE, result.right)
    }
    return result
  }

  def valueGEQ(value: Interval): Interval = return new Interval(value.left, Integer.MAX_VALUE)

  def valueLEQ(value: Interval): Interval = new Interval(Integer.MIN_VALUE, value.right)

  def valueGreater(value: Interval): Interval = return new Interval(value.right + 1, Integer.MAX_VALUE)

  def valueLess(value: Interval): Interval = new Interval(Integer.MIN_VALUE, value.left - 1)
}

case class DoubleInterval(left: Double, right: Double) extends NonRelationalNumericalDomain[DoubleInterval] {

  final override def factory() = top()

  def top(): DoubleInterval = new DoubleInterval(Double.MinValue, Double.MaxValue)

  def bottom(): DoubleInterval = new DoubleInterval(1, 0)

  def isBottom: Boolean = left > right

  /** Returns the constraint expressed by this interval on a given identifier, if some (None if unconstrained) */
  override def asConstraint(id: Identifier): Option[Expression] = {
    if (this.isBottom) return None
    if (left == Double.MinValue && right == Double.MaxValue) return None
    val lowerBound = BinaryArithmeticExpression(Constant(left.toString, id.typ), id, ArithmeticOperator.<=)
    val upperBound = BinaryArithmeticExpression(id, Constant(right.toString, id.typ), ArithmeticOperator.<=)
    if (right == Double.MaxValue) return Some(lowerBound)
    if (left == Double.MinValue) return Some(upperBound)
    Some(BinaryBooleanExpression(lowerBound, upperBound, BooleanOperator.&&))
  }

  def overlapsWith(value: DoubleInterval): Boolean = {
    if (this.right < value.left || value.right < this.left) return false
    else return true
  }


  def isTop = left == Double.MinValue && right == Double.MaxValue

  override def toString: String = {
    if (this.isBottom) return "⊥"
    if (this.isTop) return "T"
    var result: String = "["
    if (left == Double.MinValue)
      result = result + "-oo"
    else result = result + left.toString
    result = result + ".."
    if (right == Double.MaxValue)
      result = result + "+oo"
    else result = result + right.toString
    result + "]"
  }


  private def min(left: Double, right: Double) = if (left < right) left else right

  private def max(left: Double, right: Double) = if (left > right) left else right

  def lub(other: DoubleInterval): DoubleInterval = {
    if (isBottom) return other
    if (other.isBottom) return this
    return new DoubleInterval(min(left, other.left), max(right, other.right))
  }


  def glb(other: DoubleInterval): DoubleInterval = {
    if (isBottom || other.isBottom) return bottom()
    return new DoubleInterval(max(left, other.left), min(right, other.right))
  }

  def widening(other: DoubleInterval): DoubleInterval = {
    var result = lub(other)
    if (other.left < left)
      result = new DoubleInterval(Double.MinValue, result.right)
    if (other.right > right)
      result = new DoubleInterval(result.left, Double.MaxValue)
    result
  }

  def lessEqual(other: DoubleInterval): Boolean = {
    val that: DoubleInterval = this
    if (that.isBottom) return true
    if (other.isBottom) return false
    if (that.left >= other.left && that.right <= other.right)
      return true
    else return false
  }

  override def equals(o: Any) = o match {
    case x: DoubleInterval => x.left == this.left && x.right == this.right
    case _ => false
  }

  def evalConstant(value: Int): DoubleInterval = return new DoubleInterval(value, value)

  def evalConstant(value: Double): DoubleInterval = return new DoubleInterval(value, value)

  def evalConstant(value: Constant): DoubleInterval = {
    try {
      val convertedVal: Double = value.constant.toDouble
      evalConstant(convertedVal)
    } catch {
      case e: NumberFormatException => top()
    }
  }

  def sum(leftExpr: DoubleInterval, rightExpr: DoubleInterval): DoubleInterval = {
    if (leftExpr.isBottom || rightExpr.isBottom) return new DoubleInterval(1, 0)
    var left = leftExpr.left + rightExpr.left
    var right = leftExpr.right + rightExpr.right
    if (leftExpr.left == Double.MinValue || rightExpr.left == Double.MinValue)
      left = Double.MinValue
    if (leftExpr.right == Double.MaxValue || rightExpr.right == Double.MaxValue
      || (rightExpr.right + leftExpr.right) < rightExpr.right) // Last case for overflow
      right = Double.MaxValue
    return new DoubleInterval(left, right)

  }

  def subtract(leftExpr: DoubleInterval, rightExpr: DoubleInterval): DoubleInterval = {
    if (leftExpr.isBottom || rightExpr.isBottom) return new DoubleInterval(1, 0)
    var left = leftExpr.left - rightExpr.left
    var right = leftExpr.right - rightExpr.right
    if (leftExpr.left == Double.MinValue || rightExpr.left == Double.MinValue
      || (rightExpr.right - leftExpr.right) > rightExpr.right) //Last case for underflow
      left = Double.MinValue
    if (leftExpr.right == Double.MaxValue || rightExpr.right == Double.MaxValue)
      right = Double.MaxValue
    return new DoubleInterval(left, right)
  }

  private def max(a: Double, b: Double, c: Double, d: Double): Double = max(max(a, b), max(c, d))

  private def min(a: Double, b: Double, c: Double, d: Double): Double = min(min(a, b), min(c, d))

  private def managedMultiply(a: Double, b: Double): Double = {
    if (a == 0 || b == 0) return 0
    var result: Double = a * b
    if (result / a != b) {
      //Overflow
      if (a >= 0 && b >= 0) result = Double.MaxValue
      else if (a <= 0 && b <= 0) result = Double.MaxValue
      else result = Double.MinValue
    }
    return result
  }

  def multiply(leftExpr: DoubleInterval, rightExpr: DoubleInterval): DoubleInterval = {
    val a = managedMultiply(leftExpr.left, rightExpr.left)
    val b = managedMultiply(leftExpr.left, rightExpr.right)
    val c = managedMultiply(leftExpr.right, rightExpr.left)
    val d = managedMultiply(leftExpr.right, rightExpr.right)
    val result = new DoubleInterval(min(a, b, c, d), max(a, b, c, d))
    return result
  }

  def divide(leftExpr: DoubleInterval, rightExpr: DoubleInterval): DoubleInterval = {
    if (rightExpr.left == 0 && rightExpr.right == 0) return leftExpr.bottom()
    val a = leftExpr.left / (if (rightExpr.left == 0) 1 else rightExpr.left)
    val b = leftExpr.left / (if (rightExpr.right == 0) 0 - 1 else rightExpr.right)
    val c = leftExpr.right / (if (rightExpr.left == 0) 1 else rightExpr.left)
    val d = leftExpr.right / (if (rightExpr.right == 0) 0 - 1 else rightExpr.right)
    var result = new DoubleInterval(min(a, b, c, d), max(a, b, c, d))
    if (leftExpr.left < 0 && leftExpr.right > 0) //It contains 0
      result = result.lub(new DoubleInterval(0, 0))
    if (rightExpr.left < 0 && rightExpr.right > 0) {
      //It contains 0
      if (leftExpr.right > 0)
        result = new DoubleInterval(result.left, Double.MaxValue)
      if (leftExpr.left < 0)
        result = new DoubleInterval(Double.MinValue, result.right)
    }
    return result
  }

  def valueGEQ(value: DoubleInterval): DoubleInterval = return new DoubleInterval(value.left, Double.MaxValue)

  def valueLEQ(value: DoubleInterval): DoubleInterval = new DoubleInterval(Double.MinValue, value.right)

  def valueGreater(value: DoubleInterval): DoubleInterval = return new DoubleInterval(value.right + 1, Double.MaxValue)

  def valueLess(value: DoubleInterval): DoubleInterval = new DoubleInterval(Double.MinValue, value.left - 1)

}


class NonRelationalNumericalAnalysis[D <: NonRelationalNumericalDomain[D]] extends SemanticAnalysis[BoxedNonRelationalNumericalDomain[D]] {
  var domain: NonRelationalNumericalDomain[D] = null

  def getLabel(): String = "Numerical nonrelational analysis"

  def parameters(): List[(String, Any)] = List(("Domain", List("Sign", "Interval")))

  def setParameter(label: String, value: Any) = label match {
    case "Domain" => value match {
      case "Sign" => domain = new Sign(SignValues.T).asInstanceOf[D]
      case "Interval" => domain = new Interval(0, 0).asInstanceOf[D]
    }
  }

  def getInitialState(): BoxedNonRelationalNumericalDomain[D] = new BoxedNonRelationalNumericalDomain(domain.asInstanceOf[D])

  override def reset(): Unit = Unit

  def getProperties: List[Property] = List(new SingleStatementProperty(DivisionByZero))

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil
}