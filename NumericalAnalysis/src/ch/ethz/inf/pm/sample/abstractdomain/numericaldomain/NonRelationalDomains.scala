package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.property.{DivisionByZero, SingleStatementProperty, Property}
import ch.ethz.inf.pm.sample.abstractdomain._

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
  def asConstraint(id: Identifier): Option[Expression]

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

trait BottomNonRelationalNumericalDomain[S <: NonRelationalNumericalDomain[S]]
  extends NonRelationalNumericalDomain[S]
  with BottomLattice[S] {
  this:S =>

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

}

trait TopNonRelationalNumericalDomain[S <: NonRelationalNumericalDomain[S]]
  extends NonRelationalNumericalDomain[S]
  with TopLattice[S] {
  this:S =>

  /** Gives the element representing exactly Zero */
  val zero:S

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

}

sealed trait Sign extends NonRelationalNumericalDomain[Sign] {

  final override def factory() = top()

  def top(): Sign = TopSign

  def bottom(): Sign = BottomSign

  def evalConstant(value: Double): Sign = {
    if (value > 0) PlusSign
    else if (value == 0) ZeroSign
    else MinusSign
  }

  def evalConstant(value: Constant): Sign = {
    try {
      evalConstant(value.constant.toInt)
    } catch {
      case e: NumberFormatException => top()
    }
  }

}

object TopSign extends Sign
  with TopNonRelationalNumericalDomain[Sign] {

  val zero = ZeroSign

}

object BottomSign extends Sign
  with BottomNonRelationalNumericalDomain[Sign]

object PlusSign extends InnerSign {

  def sum(other: Sign): Sign = other match {
    case ZeroSign => PlusSign
    case MinusSign => TopSign
    case _ => other
  }

  def subtract(other: Sign): Sign = other match {
    case ZeroSign => PlusSign
    case PlusSign => TopSign
    case MinusSign => PlusSign
    case _ => other
  }

  def multiply(other: Sign): Sign =
    other

  def divide(other: Sign): Sign = other match {
    case ZeroSign => BottomSign // division by zero
    case PlusSign => PlusSign
    case _ => other
  }

  def valueGEQ = PlusSign
  def valueLEQ = TopSign
  def valueLess = TopSign
  def valueGreater = PlusSign
  override def toString = "+"

  override def asConstraint(id: Identifier): Option[Expression] =
    Some(BinaryArithmeticExpression(id, Constant("0", id.typ), ArithmeticOperator.>))

}

object MinusSign extends InnerSign {

  def sum(other: Sign): Sign = other match {
    case ZeroSign => MinusSign
    case PlusSign => TopSign
    case _ => other
  }

  def subtract(other: Sign): Sign = other match {
    case ZeroSign => MinusSign
    case PlusSign => MinusSign
    case MinusSign => TopSign
    case _ => other
  }

  def multiply(other: Sign): Sign = other match {
    case PlusSign => MinusSign
    case MinusSign => PlusSign
    case _ => other
  }

  def divide(other: Sign): Sign = other match {
    case ZeroSign => BottomSign // division by zero
    case PlusSign => MinusSign
    case MinusSign => PlusSign
    case _ => other
  }

  def valueGEQ = TopSign
  def valueLEQ = MinusSign
  def valueLess = MinusSign
  def valueGreater = TopSign
  override def toString = "-"

  override def asConstraint(id: Identifier): Option[Expression] =
    Some(BinaryArithmeticExpression(id, Constant("0", id.typ), ArithmeticOperator.<))

}

object ZeroSign extends InnerSign {

  def sum(other: Sign): Sign = other

  def subtract(other: Sign): Sign = other match {
    case PlusSign => MinusSign
    case MinusSign => PlusSign
    case _ => other
  }

  def multiply(other: Sign): Sign = other match {
    case BottomSign => BottomSign
    case _ => ZeroSign
  }

  def divide(other: Sign): Sign = other match {
    case BottomSign => BottomSign
    case ZeroSign => BottomSign // division by zero
    case _ => ZeroSign
  }

  def valueGEQ = TopSign
  def valueLEQ = TopSign
  def valueLess = MinusSign
  def valueGreater = PlusSign
  override def toString = "0"

  override def asConstraint(id: Identifier): Option[Expression] =
    Some(BinaryArithmeticExpression(id, Constant("0", id.typ), ArithmeticOperator.==))

}

sealed trait InnerSign extends Sign {

  val isTop = false
  val isBottom = false

  def overlapsWith(other: Sign): Boolean = other match {
    case BottomSign => false
    case TopSign => true
    case c:InnerSign => c == this
  }

  def lub(other: Sign): Sign = other match {
    case BottomSign => this
    case TopSign => other
    case c:InnerSign => if (c == this) this else TopSign
  }

  def glb(other: Sign): Sign = other match {
    case BottomSign => other
    case TopSign => this
    case c:InnerSign => if (c == this) this else TopSign
  }

  def widening(other: Sign): Sign = lub(other)

  def lessEqual(other: Sign): Boolean = other match {
    case BottomSign => false
    case TopSign => true
    case c:InnerSign => c == this
  }

}

trait IntegerInterval extends NonRelationalNumericalDomain[IntegerInterval] {

  final override def factory() = top()

  def top() = TopIntegerInterval

  def bottom() = BottomIntegerInterval

  def evalConstant(value: Double): IntegerInterval =
    InnerIntegerInterval(value.toInt, value.toInt)

  def evalConstant(value: Constant): IntegerInterval = {
    try {
      evalConstant(value.constant.toInt)
    } catch {
      case e: NumberFormatException => top()
    }
  }

}

object TopIntegerInterval extends IntegerInterval
  with TopNonRelationalNumericalDomain[IntegerInterval]  {

  override val zero: IntegerInterval = InnerIntegerInterval(0,0)

}

object BottomIntegerInterval extends IntegerInterval
  with BottomNonRelationalNumericalDomain[IntegerInterval]

case class InnerIntegerInterval(left: Int, right: Int) extends IntegerInterval {

  def isBottom = false
  def isTop = false

  def factory(newLeft: Int, newRight:Int) = {
    if (newLeft == Int.MinValue && newRight == Int.MaxValue) TopIntegerInterval
    else if (newLeft > newRight) BottomIntegerInterval
    else InnerIntegerInterval(left,right)
  }

  def lub(other: IntegerInterval) = other match {
    case BottomIntegerInterval => this
    case TopIntegerInterval => other
    case InnerIntegerInterval(oLeft,oRight) =>
      factory(Math.min(left, oLeft), Math.max(right, oRight))
  }

  def glb(other: IntegerInterval) = other match {
    case BottomIntegerInterval => other
    case TopIntegerInterval => this
    case InnerIntegerInterval(oLeft,oRight) =>
      factory(Math.max(left, oLeft), Math.min(right, oRight))
  }

  def widening(other: IntegerInterval) = other match {
    case BottomIntegerInterval => this
    case TopIntegerInterval => other
    case InnerIntegerInterval(oLeft,oRight) =>
      val l = if (oLeft < left) Int.MinValue else Math.min(left, oLeft)
      val r = if (oRight > right) Int.MaxValue else Math.max(right, oRight)
      factory(l,r)
  }

  def lessEqual(other: IntegerInterval): Boolean = other match {
    case BottomIntegerInterval => false
    case TopIntegerInterval => true
    case InnerIntegerInterval(oLeft, oRight) =>
      left >= oLeft && right <= oRight
  }

  def sum(other: IntegerInterval): IntegerInterval = other match {
    case BottomIntegerInterval => BottomIntegerInterval
    case TopIntegerInterval => TopIntegerInterval
    case InnerIntegerInterval(oLeft, oRight) =>
      val newLeft =
        if (left == Int.MinValue || oLeft == Int.MinValue || (left + oLeft).toLong != left.toLong + oLeft.toLong) Int.MinValue
        else left + oLeft
      val newRight = 
        if (right == Int.MaxValue || oRight == Int.MaxValue || (right + oRight).toLong != right.toLong + oRight.toLong) Int.MaxValue
        else right + oRight
      factory(newLeft, newRight)
  }

  def subtract(other: IntegerInterval): IntegerInterval = other match {
    case BottomIntegerInterval => BottomIntegerInterval
    case TopIntegerInterval => TopIntegerInterval
    case InnerIntegerInterval(oLeft, oRight) =>
      val newLeft =
        if (left == Int.MinValue || oRight == Int.MaxValue || (left - oRight).toLong != left.toLong - oRight.toLong) Int.MinValue
        else left - oRight
      val newRight =
        if (right == Int.MaxValue || oLeft == Int.MinValue || (right - oLeft).toLong != right.toLong - oLeft.toLong) Int.MaxValue
        else right - oLeft
      factory(newLeft, newRight)
  }


  def multiply(other: IntegerInterval): IntegerInterval = other match {
    case BottomIntegerInterval => BottomIntegerInterval
    case TopIntegerInterval => TopIntegerInterval
    case InnerIntegerInterval(oLeft, oRight) =>
      val a = managedMultiply(left, oLeft)
      val b = managedMultiply(left, oRight)
      val c = managedMultiply(right, oLeft)
      val d = managedMultiply(right, oRight)
      factory(min(a, b, c, d), max(a, b, c, d))
  }

  def divide(other: IntegerInterval): IntegerInterval = other match {
    case BottomIntegerInterval => BottomIntegerInterval
    case TopIntegerInterval => TopIntegerInterval
    case InnerIntegerInterval(oLeft, oRight) =>
      val a = left /  (if (oLeft == 0)   1 else oLeft)
      val b = left /  (if (oRight == 0) -1 else oRight)
      val c = right / (if (oLeft == 0)   1 else oLeft)
      val d = right / (if (oRight == 0) -1 else oRight)
      InnerIntegerInterval(min(a, b, c, d), max(a, b, c, d))
  }

  def valueGEQ: IntegerInterval = factory(left, Int.MaxValue)

  def valueLEQ: IntegerInterval = factory(Int.MinValue, right)

  def valueGreater: IntegerInterval = factory(left + 1, Int.MaxValue)

  def valueLess: IntegerInterval = factory(Int.MinValue, right - 1)

  /** Returns the constraint expressed by this interval on a given identifier, if some (None if unconstrained) */
  override def asConstraint(id: Identifier): Option[Expression] = {
    if (this.isBottom) return None
    if (left == Int.MinValue && right == Int.MaxValue) return None
    val lowerBound = BinaryArithmeticExpression(Constant(left.toString, id.typ), id, ArithmeticOperator.<=)
    val upperBound = BinaryArithmeticExpression(id, Constant(right.toString, id.typ), ArithmeticOperator.<=)
    if (right == Int.MaxValue) return Some(lowerBound)
    if (left == Int.MinValue) return Some(upperBound)
    Some(BinaryBooleanExpression(lowerBound, upperBound, BooleanOperator.&&))
  }

  def overlapsWith(other: IntegerInterval): Boolean = other match {
    case BottomIntegerInterval => false
    case TopIntegerInterval => true
    case InnerIntegerInterval(oLeft, oRight) =>
      return !(this.right < oLeft || oRight < this.left)
  }

  override def toString: String =
    "[" +
      (if (left == Int.MinValue) "-oo" else left.toString) +
      ".." +
      (if (right == Int.MaxValue) "+oo" else right.toString) +
      "]"

  private def managedMultiply(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) return 0
    var result: Int = a * b
    if (result / a != b) {
      //Overflow
      if (a >= 0 && b >= 0) result = Int.MaxValue
      else if (a <= 0 && b <= 0) result = Int.MaxValue
      else result = Int.MinValue
    }
    return result
  }

  private def max(a: Int, b: Int, c: Int, d: Int): Int = Math.max(Math.max(a, b), Math.max(c, d))

  private def min(a: Int, b: Int, c: Int, d: Int): Int = Math.min(Math.min(a, b), Math.min(c, d))

}

object DoubleInterval {

  val Zero = InnerDoubleInterval(0,0)

}

sealed trait DoubleInterval extends NonRelationalNumericalDomain[DoubleInterval] {

  def isTop:Boolean

  def factory() = TopDoubleInterval
  def top() = TopDoubleInterval
  def bottom() = BottomDoubleInterval

  def evalConstant(value: Double) =
    if (value == 0) DoubleInterval.Zero
    else InnerDoubleInterval(value, value)

  def evalConstant(value: Constant) = {
    try {
      evalConstant(value.constant.toDouble)
    } catch {
      case e: NumberFormatException => TopDoubleInterval
    }
  }

}

object TopDoubleInterval extends DoubleInterval
   with TopNonRelationalNumericalDomain[DoubleInterval] {

  override val zero = DoubleInterval.Zero

}

object BottomDoubleInterval extends DoubleInterval
   with BottomNonRelationalNumericalDomain[DoubleInterval] {

}

case class InnerDoubleInterval(left: Double, right: Double)
  extends DoubleInterval {

  assert {left <= right}
  assert {!right.isPosInfinity || !left.isPosInfinity}
  assert {!left.isNaN && !right.isNaN}
  assert {!right.isNegInfinity}
  assert {!left.isPosInfinity}

  def factory(newLeft:Double, newRight: Double):DoubleInterval = {
    if (newLeft.isPosInfinity && newRight.isNegInfinity) TopDoubleInterval
    else if (newLeft > newRight) BottomDoubleInterval
    else InnerDoubleInterval(newLeft,newRight)
  }

  def isBottom: Boolean = false
  def isTop = false

  def overlapsWith(other: DoubleInterval): Boolean =
    other match {
      case BottomDoubleInterval => false
      case TopDoubleInterval => true
      case InnerDoubleInterval(oLeft, oRight) =>
        return !(this.right < oLeft || oRight < this.left)
    }

  def lub(other: DoubleInterval) =
    other match {
      case BottomDoubleInterval => this
      case TopDoubleInterval => TopDoubleInterval
      case InnerDoubleInterval(oLeft, oRight) =>
        factory(Math.min(left, oLeft), Math.max(right, oRight))
    }

  def glb(other: DoubleInterval) =
    other match {
      case BottomDoubleInterval => BottomDoubleInterval
      case TopDoubleInterval => this
      case InnerDoubleInterval(oLeft, oRight) =>
        factory(Math.max(left, oLeft), Math.min(right, oRight))
    }

  def widening(other: DoubleInterval) =
    other match {
      case BottomDoubleInterval => this
      case TopDoubleInterval => TopDoubleInterval
      case InnerDoubleInterval(oLeft, oRight) =>
        val l = if (oLeft < this.left) Double.NegativeInfinity else Math.min(this.left, oLeft)
        val r = if (oRight > this.right) Double.PositiveInfinity else Math.max(this.right, oRight)
        factory(l, r)
    }

  def lessEqual(other: DoubleInterval): Boolean =
    other match {
      case BottomDoubleInterval => false
      case TopDoubleInterval => true
      case InnerDoubleInterval(oLeft, oRight) =>
        this.left >= oLeft && this.right <= oRight
    }

  def sum(other: DoubleInterval) =
    other match {
      case BottomDoubleInterval => BottomDoubleInterval
      case TopDoubleInterval => TopDoubleInterval
      case InnerDoubleInterval(oLeft, oRight) =>
        factory(left + oLeft, right + oRight)
    }

  def subtract(other: DoubleInterval) =
    other match {
      case BottomDoubleInterval => BottomDoubleInterval
      case TopDoubleInterval => TopDoubleInterval
      case InnerDoubleInterval(oLeft, oRight) =>
        factory(left - oRight, right - oLeft)
    }

  def multiply(other: DoubleInterval) =
    other match {
      case BottomDoubleInterval => BottomDoubleInterval
      case TopDoubleInterval =>
        if (this == DoubleInterval.Zero) DoubleInterval.Zero else TopDoubleInterval
      case InnerDoubleInterval(oLeft, oRight) =>
        if (this == DoubleInterval.Zero || other == DoubleInterval.Zero) DoubleInterval.Zero
        else {
          val a = left * oLeft
          val b = left * oRight
          val c = right * oLeft
          val d = right * oRight
          factory(min(a, b, c, d), max(a, b, c, d))
        }
    }

  def divide(other: DoubleInterval) =
    other match {
      case BottomDoubleInterval => BottomDoubleInterval
      case TopDoubleInterval => TopDoubleInterval
      case DoubleInterval.Zero => BottomDoubleInterval
      case InnerDoubleInterval(oLeft, oRight) =>
        val a = if (oLeft == 0)  infinitySign(left)  else left / oLeft
        val b = if (oRight == 0) invertedInfinitySign(left)  else left / oRight
        val c = if (oLeft == 0)  infinitySign(right) else right / oLeft
        val d = if (oRight == 0) invertedInfinitySign(right) else right / oRight
        factory(min(a, b, c, d), max(a, b, c, d))
    }

  def valueGEQ = factory(left, Double.PositiveInfinity)
  def valueLEQ = factory(Double.NegativeInfinity, right)
  def valueGreater = factory(left + NumericalAnalysisConstants.epsilon, Double.PositiveInfinity)
  def valueLess = factory(Double.NegativeInfinity, right - NumericalAnalysisConstants.epsilon)

  /** Returns the constraint expressed by this interval on a given identifier, if some (None if unconstrained) */
  override def asConstraint(id: Identifier): Option[Expression] = {
    val lowerBound = BinaryArithmeticExpression(Constant(left.toString, id.typ), id, ArithmeticOperator.<=)
    val upperBound = BinaryArithmeticExpression(id, Constant(right.toString, id.typ), ArithmeticOperator.<=)
    if (right == Double.PositiveInfinity) return Some(lowerBound)
    if (left == Double.NegativeInfinity) return Some(upperBound)
    Some(BinaryBooleanExpression(lowerBound, upperBound, BooleanOperator.&&))
  }

  override def toString: String =
    "[" +
      (if (left == Double.NegativeInfinity) "-oo" else left.toString) +
    ".." +
      (if (right == Double.PositiveInfinity) "+oo" else right.toString) +
    "]"

  private def max(a: Double, b: Double, c: Double, d: Double): Double =
    Math.max(Math.max(a, b), Math.max(c, d))

  private def min(a: Double, b: Double, c: Double, d: Double): Double =
    Math.min(Math.min(a, b), Math.min(c, d))

  private def infinitySign(d: Double): Double =
    if (d > 0) Double.PositiveInfinity else if (d < 0) Double.NegativeInfinity else 0.0

  private def invertedInfinitySign(d: Double): Double =
    if (d < 0) Double.PositiveInfinity else if (d > 0) Double.NegativeInfinity else 0.0

}


class NonRelationalNumericalAnalysis[D <: NonRelationalNumericalDomain[D]] extends SemanticAnalysis[BoxedNonRelationalNumericalDomain[D]] {
  var domain: NonRelationalNumericalDomain[D] = null

  def getLabel(): String = "Numerical nonrelational analysis"

  def parameters(): List[(String, Any)] = List(("Domain", List("Sign", "Interval")))

  def setParameter(label: String, value: Any) = label match {
    case "Domain" => value match {
      case "Sign" => domain = TopSign.asInstanceOf[D]
      case "Interval" => domain = TopIntegerInterval.asInstanceOf[D]
      case "DoubleInterval" => domain = TopDoubleInterval.asInstanceOf[D]
    }
  }

  def getInitialState(): BoxedNonRelationalNumericalDomain[D] = new BoxedNonRelationalNumericalDomain(domain.asInstanceOf[D])

  override def reset(): Unit = ()

  def getProperties: List[Property] = List(new SingleStatementProperty(DivisionByZero))

  def getNativeMethodsSemantics(): List[NativeMethodSemantics] = Nil
}