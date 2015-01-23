package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property.{DivisionByZero, SingleStatementProperty, Property}
import ch.ethz.inf.pm.sample.abstractdomain._

import scala.collection.JavaConverters

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
   * @param id the identifier this domain restricts
   * @return a constraint, for example 1<id<5 for the interval (1,5), None in case of top or bottom
   */
  def asConstraint(id: Identifier): Option[Expression] = ???

}

case class BoxedNonRelationalNumericalDomain[N <: NonRelationalNumericalDomain[N]](dom: N,
                                                                              map: Map[Identifier, N] = Map.empty[Identifier, N],
                                                                              isBottom: Boolean = false,
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

  def evalBoolean(expr: Expression): N = {
    // Implicit conversion from boolean types
    val mayBeTrue =  if (!this.assume(expr).isBottom) dom.evalConstant(1) else dom.bottom()
    val mayBeFalse = if (!this.assume(NegatedBooleanExpression(expr)).isBottom) dom.evalConstant(0) else dom.bottom()
    return mayBeTrue.lub(mayBeFalse)
  }


  /**
   * Adds [key->value] to the domain
   * @param key The key
   * @param value The value
   * @return The state of the domain after the assignment
   */
  override def add(key: Identifier, value: N): BoxedNonRelationalNumericalDomain[N] = {
    if (value.isBottom)
      bottom()
    else super.add(key,value)
  }

  def eval(expr: Expression): N = {

    expr match {
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.+, typ) => return eval(left).sum(eval(right))
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.*, typ) => return eval(left).multiply(eval(right))
      case BinaryArithmeticExpression(left, right, ArithmeticOperator./, typ) => return eval(left).divide(eval(right))
      case BinaryArithmeticExpression(left, right, ArithmeticOperator.-, typ) => return eval(left).subtract(eval(right))
      case BinaryArithmeticExpression(left, right, op, typ) if ArithmeticOperator.isComparison(op) => evalBoolean(expr)
      case BinaryBooleanExpression(left, right, _, typ) => evalBoolean(expr)
      case NegatedBooleanExpression(left) => evalBoolean(expr)
      case BinaryArithmeticExpression(left, right, op, typ) => dom.top()
      case c@Constant(constant, typ, pp) => dom.evalConstant(c)
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
      case Constant("true",_,_) => this
      case Constant("false",_,_) => this.bottom()
      case NegatedBooleanExpression(Constant("true",_,_)) => this.bottom()
      case NegatedBooleanExpression(Constant("false",_,_)) => this

      // Boolean variables
      case x: Identifier =>
        assert(x.typ.isBooleanType)
        val res = assume(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.!=))
        return res

      case NegatedBooleanExpression(x: Identifier) =>
        assert(x.typ.isBooleanType)
        val res = assume(BinaryArithmeticExpression(x, Constant("0", x.typ, x.pp), ArithmeticOperator.==))
        return res

      // And and Or
      case BinaryBooleanExpression(left, right, op, _) => op match {
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
        val res = assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||, typ))
        return res

      case BinaryArithmeticExpression(left, right, ArithmeticOperator.!=, typ) =>
        val newLeft = BinaryArithmeticExpression(left, right, ArithmeticOperator.>, typ)
        val newRight = BinaryArithmeticExpression(left, right, ArithmeticOperator.<, typ)
        val res = assume(BinaryBooleanExpression(newLeft, newRight, BooleanOperator.||, typ))
        return res

      // Inverting of operators
      case NegatedBooleanExpression(BinaryArithmeticExpression(left, right, op, typ)) =>
        assume(BinaryArithmeticExpression(left,right,ArithmeticOperator.negate(op),typ))

      // Handling of monomes
      case _ => Normalizer.conditionalExpressionToMonomes(expr) match {
        case None =>
          expr match {
            case BinaryArithmeticExpression(left, right, op, typ) =>
              if (!left.typ.isNumericalType || !right.typ.isNumericalType) return this

              val l: N = this.eval(left)
              val r: N = this.eval(right)

              op match {
                case ArithmeticOperator.== =>
                  var curState = this
                  left  match { case lId:Identifier => curState = curState.add(lId,l.glb(r)); case _ => () }
                  right match { case rId:Identifier => curState = curState.add(rId,l.glb(r)); case _ => () }
                  return curState
                case ArithmeticOperator.<= => if (!l.overlapsWith(r.valueLEQ)) return this.bottom()
                case ArithmeticOperator.>= => if (!l.overlapsWith(r.valueGEQ)) return this.bottom()
                case ArithmeticOperator.> => if (!l.overlapsWith(r.valueGreater)) return this.bottom()
                case ArithmeticOperator.< => if (!l.overlapsWith(r.valueLess)) return this.bottom()
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
                result = result sum dom.evalConstant(monome1._1).multiply(eval(monome1._2))
            }
            if (index >= 0) {
              //k*x+n >= 0 => x >= -(n/k)
              result = result.divide(dom.evalConstant(index))
              result = dom.evalConstant(0).subtract(result)
              val newValue = get(variable) glb result.valueGEQ
              if (newValue.lessEqual(newValue.bottom()))
                return stateResult.bottom()
              stateResult = stateResult.add(variable, newValue)
            }
            else {
              //-k*x+n >= 0 => x <= n/-k
              result = result.divide(dom.evalConstant(-index))
              val oldValue = this.get(variable)
              val newRestraint = result.valueLEQ
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

  def isBottom: Boolean = false

  def top(): Top = this

  def bottom(): Top = this

  def lub(other: Top): Top = this

  def glb(other: Top): Top = this

  def widening(other: Top): Top = this

  def lessEqual(other: Top): Boolean = true

  override def equals(o: Any) = true

  def evalConstant(value: Double): Top = this

  def evalConstant(value: Constant): Top = this

  def sum(other: Top): Top = this

  def subtract(other: Top): Top = this

  def multiply(other: Top): Top = this

  def divide(other: Top): Top = this

  def valueGEQ: Top = this

  def valueLEQ: Top = this

  def valueLess: Top = this

  def valueGreater: Top = this

  def overlapsWith(value: Top): Boolean = true

}


object SignValues extends Enumeration {
  val + = Value("+")
  val ZERO = Value("0")
  val - = Value("-")
  val T = Value("⊤")
  val BOT = Value("⊥")
}

case class Sign(value: SignValues.Value) extends NonRelationalNumericalDomain[Sign] {

  def isBottom: Boolean = value == SignValues.BOT

  def overlapsWith(value: Sign): Boolean = {
    if (this.value == SignValues.BOT || value.value == SignValues.BOT) return false
    if (this.value == SignValues.T || value.value == SignValues.T) return true
    if (this.value == value.value) return true
    else return false
  }

  final override def factory() = top()

  override def toString = value.toString

  def top(): Sign = Sign(SignValues.T)

  def bottom(): Sign = Sign(SignValues.BOT)

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

  def sum(other: Sign): Sign = {
    if (value == SignValues.BOT || other.value == SignValues.BOT) return Sign(SignValues.BOT)
    if (value == SignValues.T || other.value == SignValues.T) return Sign(SignValues.T)
    if (value == other.value) return Sign(value)
    if (value == SignValues.ZERO) return Sign(other.value)
    if (other.value == SignValues.ZERO) return Sign(value)
    return Sign(SignValues.T)
  }

  def subtract(other: Sign): Sign = {
    if (value == SignValues.BOT || other.value == SignValues.BOT) return Sign(SignValues.BOT)
    if (value == SignValues.T || other.value == SignValues.T) return Sign(SignValues.T)
    if (value == SignValues.ZERO) {
      if (other.value == SignValues.ZERO) return Sign(SignValues.ZERO)
      else if (other.value == SignValues.+) return Sign(SignValues.-)
      else return Sign(SignValues.+)
    }
    if (value != other.value && value == SignValues.+) return Sign(SignValues.+)
    if (value != other.value && value == SignValues.-) return Sign(SignValues.-)
    return Sign(SignValues.T)
  }

  def multiply(other: Sign): Sign = {
    if (value == SignValues.BOT || other.value == SignValues.BOT) return Sign(SignValues.BOT)
    if (value == SignValues.ZERO || value == SignValues.ZERO) return Sign(SignValues.ZERO)
    if (value == SignValues.T || other.value == SignValues.T) return Sign(SignValues.T)
    if (value == other.value) return Sign(SignValues.+)
    return Sign(SignValues.-)
  }

  def divide(other: Sign): Sign = {
    if (value == SignValues.BOT || other.value == SignValues.BOT || other.value == SignValues.ZERO) return Sign(SignValues.BOT)
    if (value == SignValues.ZERO) return Sign(SignValues.ZERO)
    if (value == SignValues.T || other.value == SignValues.T) return Sign(SignValues.T)
    if (value == other.value) return Sign(SignValues.+)
    return Sign(SignValues.-)
  }

  def evalConstant(value: Double): Sign = {
    if (value > 0) Sign(SignValues.+)
    else if (value == 0) Sign(SignValues.ZERO)
    else Sign(SignValues.-)
  }

  def evalConstant(value: Constant): Sign = {
    try {
      val convertedVal: Int = Integer.valueOf(value.constant).intValue()
      evalConstant(convertedVal)
    } catch {
      case e: NumberFormatException => top()
    }
  }

  def valueGEQ: Sign =
    if (value == SignValues.+) Sign(SignValues.+)
    else Sign(SignValues.T)

  def valueLEQ: Sign =
    if (value == SignValues.-) Sign(SignValues.-)
    else this

  def valueLess: Sign =
    if (value == SignValues.- || value == SignValues.ZERO) Sign(SignValues.-)
    else this

  def valueGreater: Sign =
    if (value == SignValues.+ || value == SignValues.ZERO) Sign(SignValues.+)
    else this

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

  def evalConstant(value: Double): Interval = {
     return new Interval(value.toInt, value.toInt)
  }

  def evalConstant(value: Constant): Interval = {
    try {
      evalConstant(Integer.valueOf(value.constant).intValue())
    } catch {
      case e: NumberFormatException => top()
    }
  }

  def sum(other: Interval): Interval = {
    if (isBottom || other.isBottom) return new Interval(1, 0)
    var newLeft = left + other.left
    var newRight = right + other.right
    if (left == Integer.MIN_VALUE || other.left == Integer.MIN_VALUE)
      newLeft = Integer.MIN_VALUE
    if (right == Integer.MAX_VALUE || other.right == Integer.MAX_VALUE || (other.right + right) < other.right) //Last case for overflow
      newRight = Integer.MAX_VALUE
    return new Interval(newLeft, newRight)

  }

  def subtract(other: Interval): Interval = {
    if (isBottom || other.isBottom) return new Interval(1, 0)
    var newLeft = left - other.left
    var newRight = right - other.right
    if (left == Integer.MIN_VALUE || other.left == Integer.MIN_VALUE || (other.right - right) > other.right) //Last case for underflow
      newLeft = Integer.MIN_VALUE
    if (right == Integer.MAX_VALUE || other.right == Integer.MAX_VALUE)
      newRight = Integer.MAX_VALUE
    return new Interval(newLeft, newRight)
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

  def multiply(other: Interval): Interval = {
    val a = managedMultiply(left, other.left)
    val b = managedMultiply(left, other.right)
    val c = managedMultiply(right, other.left)
    val d = managedMultiply(right, other.right)
    val result = new Interval(min(a, b, c, d), max(a, b, c, d))
    return result
  }

  def divide(other: Interval): Interval = {
    if (other.left == 0 && other.right == 0) return bottom()
    val a = left / (if (other.left == 0) 1 else other.left)
    val b = left / (if (other.right == 0) 0 - 1 else other.right)
    val c = right / (if (other.left == 0) 1 else other.left)
    val d = right / (if (other.right == 0) 0 - 1 else other.right)
    var result = new Interval(min(a, b, c, d), max(a, b, c, d))
    if (left < 0 && right > 0) //It contains 0
      result = result.lub(new Interval(0, 0))
    if (other.left < 0 && other.right > 0) {
      //It contains 0
      if (right > 0)
        result = new Interval(result.left, Integer.MAX_VALUE)
      if (left < 0)
        result = new Interval(Integer.MIN_VALUE, result.right)
    }
    return result
  }

  def valueGEQ: Interval = Interval(left, Integer.MAX_VALUE)

  def valueLEQ: Interval = Interval(Integer.MIN_VALUE, right)

  def valueGreater: Interval = Interval(right + 1, Integer.MAX_VALUE)

  def valueLess: Interval = Interval(Integer.MIN_VALUE, left - 1)
}

case class DoubleInterval(left: Double, right: Double) extends NonRelationalNumericalDomain[DoubleInterval] {

  final override def factory() =
    top()

  def top(): DoubleInterval =
    DoubleInterval(Double.NegativeInfinity, Double.PositiveInfinity)

  def bottom(): DoubleInterval =
    DoubleInterval(1, 0)

  def isBottom: Boolean =
    left > right

  def isTop =
    left == Double.NegativeInfinity && right == Double.PositiveInfinity

  def overlapsWith(value: DoubleInterval): Boolean =
    if (this.right < value.left || value.right < this.left) false
    else true

  def lub(other: DoubleInterval): DoubleInterval =
    if (isBottom) return other
    else if (other.isBottom) return this
    else DoubleInterval(Math.min(left, other.left), Math.max(right, other.right))

  def glb(other: DoubleInterval): DoubleInterval =
    if (isBottom || other.isBottom) return bottom()
    else DoubleInterval(Math.max(left, other.left), Math.min(right, other.right))

  def widening(other: DoubleInterval): DoubleInterval = {
    var result = lub(other)
    if (other.left < left)
      result = new DoubleInterval(Double.NegativeInfinity, result.right)
    if (other.right > right)
      result = new DoubleInterval(result.left, Double.PositiveInfinity)
    result
  }

  def lessEqual(other: DoubleInterval): Boolean =
    if (this.isBottom) true
    else if (other.isBottom) false
    else if (this.left >= other.left && this.right <= other.right) true
    else false

  override def equals(o: Any) = o match {
    case x: DoubleInterval => x.left == this.left && x.right == this.right
    case _ => false
  }

  def evalConstant(value: Double): DoubleInterval =
    DoubleInterval(value, value)

  def evalConstant(value: Constant): DoubleInterval = {
    try {
      evalConstant(value.constant.toDouble)
    } catch {
      case e: NumberFormatException => top()
    }
  }

  def sum(other: DoubleInterval): DoubleInterval =
    if (isBottom || other.isBottom) return bottom()
    else DoubleInterval(left + other.left, right + other.right)

  def subtract(other: DoubleInterval): DoubleInterval =
    if (isBottom || other.isBottom) bottom()
    else DoubleInterval(left - other.right, right - other.left)

  def multiply(other: DoubleInterval): DoubleInterval = {
    val a = left * other.left
    val b = left * other.right
    val c = right * other.left
    val d = right * other.right
    DoubleInterval(min(a, b, c, d), max(a, b, c, d))
  }

  def divide(other: DoubleInterval): DoubleInterval = {
    if (other.left == 0 && other.right == 0) return bottom()
    val a = left / (if (other.left == 0) NumericalAnalysisConstants.epsilon else other.left)
    val b = left / (if (other.right == 0) -NumericalAnalysisConstants.epsilon else other.right)
    val c = right / (if (other.left == 0) NumericalAnalysisConstants.epsilon else other.left)
    val d = right / (if (other.right == 0) -NumericalAnalysisConstants.epsilon else other.right)
    DoubleInterval(min(a, b, c, d), max(a, b, c, d))
  }

  def valueGEQ: DoubleInterval =
    DoubleInterval(left, Double.PositiveInfinity)

  def valueLEQ: DoubleInterval =
    DoubleInterval(Double.NegativeInfinity, right)

  def valueGreater: DoubleInterval =
    DoubleInterval(right + NumericalAnalysisConstants.epsilon, Double.PositiveInfinity)

  def valueLess: DoubleInterval =
    DoubleInterval(Double.NegativeInfinity, left - NumericalAnalysisConstants.epsilon)


  /** Returns the constraint expressed by this interval on a given identifier, if some (None if unconstrained) */
  override def asConstraint(id: Identifier): Option[Expression] = {
    if (this.isBottom) return None
    if (left == Double.NegativeInfinity && right == Double.PositiveInfinity) return None
    val lowerBound = BinaryArithmeticExpression(Constant(left.toString, id.typ), id, ArithmeticOperator.<=)
    val upperBound = BinaryArithmeticExpression(id, Constant(right.toString, id.typ), ArithmeticOperator.<=)
    if (right == Double.PositiveInfinity) return Some(lowerBound)
    if (left == Double.NegativeInfinity) return Some(upperBound)
    Some(BinaryBooleanExpression(lowerBound, upperBound, BooleanOperator.&&))
  }

  override def toString: String = {
    if (this.isBottom) return "⊥"
    if (this.isTop) return "T"
    var result: String = "["
    if (left == Double.NegativeInfinity)
      result = result + "-oo"
    else result = result + left.toString
    result = result + ".."
    if (right == Double.PositiveInfinity)
      result = result + "+oo"
    else result = result + right.toString
    result + "]"
  }

  private def max(a: Double, b: Double, c: Double, d: Double): Double =
    Math.max(Math.max(a, b), Math.max(c, d))

  private def min(a: Double, b: Double, c: Double, d: Double): Double =
    Math.min(Math.min(a, b), Math.min(c, d))

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