package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.ToStringUtilities
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain

trait StringDomain[X <: StringDomain[X]] extends SimplifiedSemanticDomain[X] { this: X => }

/**
 * Represents a non-relational string domain.
 *
 * @param dom An instance of the value domain. Only for factory purposes
 * @tparam T The type of the value domain
 */
case class NonrelationalStringDomain[T <:StringValueSetDomain[T]](dom:T,
                                                          map:Map[Identifier, T] = Map.empty[Identifier, T],
                                                          override val isBottom:Boolean = false,
                                                          isTop:Boolean = false)
  extends BoxedDomain[T,NonrelationalStringDomain[T]]
  with StringDomain[NonrelationalStringDomain[T]] {

  def functionalFactory(_value:Map[Identifier, T] = Map.empty[Identifier, T],
                        _isBottom:Boolean = false,
                        _isTop:Boolean = false) : NonrelationalStringDomain[T] =
    new NonrelationalStringDomain[T](dom,_value,_isBottom,_isTop)

  def get(key : Identifier) : T = map.get(key) match {
    case None => dom.bottom()
    case Some(x) => x
  }

  override def createVariable(variable: Identifier, typ: Type): NonrelationalStringDomain[T] = this

  override def createVariableForArgument(variable: Identifier, typ: Type, path: List[String]) = {
    var result = Map.empty[Identifier, List[String]]
    result = result + ((variable, path ::: variable.toString :: Nil))
    (this.add(variable, dom.top()), result)
  }

  override def setToTop(variable: Identifier): NonrelationalStringDomain[T] = {
    this.add(variable, dom.top())
  }

  override def removeVariable(variable: Identifier): NonrelationalStringDomain[T]  = {
    this.remove(variable)
  }

  override def setArgument(variable: Identifier, expr: Expression): NonrelationalStringDomain[T]  = this.assign(variable, expr)

  override def assign(variable: Identifier, expr: Expression): NonrelationalStringDomain[T]  = {
    if (variable.typ.isStringType) {
      val res = eval(expr)
      //if (res.isBottom) bottom()
      val ret = if (variable.representsSingleVariable) this.add(variable, res)
                else this.add(variable, get(variable).lub(res))
      ret
    } else this
  }

  override def backwardAssign(oldPreState: NonrelationalStringDomain[T], variable: Identifier, expr: Expression): NonrelationalStringDomain[T]  = this

  override def access(field: Identifier) = this

  override def backwardAccess(field: Identifier) = this

  private def eval(expr: Expression): T = expr match {
    case Constant(constant, typ, pp) =>
      dom.singleton(constant)
    case AbstractOperator(left,List(right),Nil,AbstractOperatorIdentifiers.stringConcatenation,_) =>
      eval(left).concat(eval(right))
    case x: Identifier =>
      this.get(x)
    case x: Expression => dom.top()
  }

  /**
   * This is imprecise, but sound
   */
  override def assume(expr: Expression): NonrelationalStringDomain[T] = {

    if (isBottom)
      return this

    // Check if we assume something about non-numerical values - if so, return
    val ids = expr.ids
    ids match {
      case IdentifierSet.Top => return this
      case IdentifierSet.Inner(v) =>
        for (id <- v) {
          if (!id.typ.isStringType) {
            return this
          }
        }
      case IdentifierSet.Bottom => ()
    }

    expr match {

      // DOUBLE NEGATION
      case NegatedBooleanExpression(NegatedBooleanExpression(n)) =>
        assume(n)

      // Comparison
      case BinaryArithmeticExpression(a:Expression, b:Expression, ArithmeticOperator.==, _) =>

        val left = eval(a)
        val right = eval(b)

        val intersection = dom.intersect(left,right)

        if (intersection.isBottom)
          bottom()
        else (a,b) match {
          case (x:Identifier, y:Identifier) =>
            val res = this.restrict(x,intersection).restrict(y,intersection)
            res
          case (x:Identifier, y:Expression) =>
            val res = this.restrict(x,intersection)
            res
          case (y:Expression, x:Identifier) =>
            val res = this.restrict(x,intersection)
            res
          case _ =>
            this
        }

      // Negated comparison
      case NegatedBooleanExpression(BinaryArithmeticExpression(a:Expression, b:Expression, ArithmeticOperator.==, _)) =>

        val left = eval(a)
        val right = eval(b)

        val diff = dom.diff(left,right)

        // We can only assume anything about inequalities if left or right is a singleton
        var curState = this
        if (a.isInstanceOf[Identifier] && right.isSingleton) curState = curState.restrict(a.asInstanceOf[Identifier],diff)
        if (b.isInstanceOf[Identifier] && left.isSingleton) curState = curState.restrict(b.asInstanceOf[Identifier],diff)
        if (right.isSingleton && left.isSingleton && diff.isBottom) curState = curState.bottom()
        curState

      // DE MORGAN
      case NegatedBooleanExpression(BinaryBooleanExpression(left,right,op,typ)) => op match {
        case BooleanOperator.|| => assume(NegatedBooleanExpression(left)).assume(NegatedBooleanExpression(right))
        case BooleanOperator.&& => assume(NegatedBooleanExpression(left)).lub(assume(NegatedBooleanExpression(right)))
      }

      // AND, OR
      case BinaryBooleanExpression(left,right,op,typ) => op match {
        case BooleanOperator.&& => assume(left).assume(right)
        case BooleanOperator.|| => assume(left) lub assume(right)
      }

      case _ =>
        this
    }

  }

  private def restrict(id:Identifier,a:T): NonrelationalStringDomain[T] = {
    copy(map = map + (id -> (map.getOrElse(id,dom.bottom()) glb a)))
  }

}

trait StringValueDomain[T <: StringValueDomain[T]] extends Lattice[T] { this: T =>

  def isBottom:Boolean

  def isTop:Boolean

  def concat(right:T):T

}

trait StringValueSetDomain[T <: StringValueSetDomain[T]] extends StringValueDomain[T] { this: T =>

  def isSingleton:Boolean

  def intersect(a:T,b:T):T

  def diff(a:T,b:T):T

  def singleton(a:String):T

}

trait StringKSetDomain extends SetDomain.Bounded[String, StringKSetDomain]
  with StringValueSetDomain[StringKSetDomain] {

  val k:Int

  def bottom() = StringKSetDomain.Bottom(k)
  def top() = StringKSetDomain.Top(k)
  def factory(v:Set[String]) = if (v.isEmpty) bottom() else StringKSetDomain.Inner(k,v)

  def diff(a: StringKSetDomain, b: StringKSetDomain): StringKSetDomain = (a -- b) lub (b -- a)

  def intersect(a: StringKSetDomain, b: StringKSetDomain): StringKSetDomain =
    a glb b

  def singleton(a: String): StringKSetDomain = bottom().+(a)

  def concat(other:StringKSetDomain):StringKSetDomain

  def isSingleton:Boolean

}

object StringKSetDomain {

  case class Top(k:Int) extends StringKSetDomain with SetDomain.Bounded.Top[String,StringKSetDomain] {
    def isSingleton = false
    def concat(other:StringKSetDomain) = if (other.isBottom) bottom() else this
  }

  case class Bottom(k:Int) extends StringKSetDomain with SetDomain.Bounded.Bottom[String,StringKSetDomain] {
    def isSingleton = false
    def concat(other:StringKSetDomain) = this
  }

  case class Inner(k:Int, value: Set[String] = Set.empty[String]) extends StringKSetDomain
    with SetDomain.Bounded.Inner[String,StringKSetDomain,Inner] {

    def isSingleton = value.size == 1

    def concat(other:StringKSetDomain):StringKSetDomain = other match {
      case Bottom(_) => bottom()
      case Top(_) => top()
      case Inner(_,oValue) =>
        var ret:StringKSetDomain = bottom()
        for (left <- value)
          for (right <- oValue)
            ret = ret.+(left + right)
        ret
    }

    override def cap: StringKSetDomain = if (value.size > k) top() else this

  }

}

trait NumericWithStringDomain[
    N <: NumericalDomain[N],
    S <: StringDomain[S],
    T <: NumericWithStringDomain[N, S, T]]
  extends SemanticCartesianProductDomain[N, S, T]
  with NumericalDomain[T] { this: T =>

  override def _2canHandle(id: Identifier) = id.typ.isStringType

  def initialNum: N = _1

  def initialStr: S = _2

  override def toString = "Numeric:\n"+ToStringUtilities.indent(_1.toString)+"\nString:\n"+ToStringUtilities.indent(_2.toString)

  override def getConstraints(ids: Set[Identifier]) = ???
}