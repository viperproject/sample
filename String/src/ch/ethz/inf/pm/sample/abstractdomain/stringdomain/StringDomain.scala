package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation.Type
import ch.ethz.inf.pm.sample.ToStringUtilities
import ch.ethz.inf.pm.sample.abstractdomain.numericaldomain.NumericalDomain

trait StringDomain[T <: StringValueDomain[T],X <: StringDomain[T,X]] extends SimplifiedSemanticDomain[X] { this: X => }

/**
 * Represents a non-relational string domain.
 *
 * @param dom An instance of the value domain. Only for factory purposes
 * @tparam T The type of the value domain
 */
class NonrelationalStringDomain[T <:StringValueSetDomain[T]](dom:T,
                                                          val map:Map[Identifier, T] = Map.empty[Identifier, T],
                                                          val isBottom:Boolean = false,
                                                          val isTop:Boolean = false)
  extends BoxedDomain[T,NonrelationalStringDomain[T]]
  with StringDomain[T,NonrelationalStringDomain[T]] {

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
      if (variable.representsSingleVariable()) this.add(variable, res)
      else this.add(variable, get(variable).lub(res))
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
    case xs: HeapIdSetDomain[_] =>
      var result = dom.bottom()
      for (x <- xs.value) result = result.lub(get(x))
      result
    case x: Expression => dom.top()
  }

  /**
   * This is imprecise, but sound
   */
  override def assume(expr: Expression): NonrelationalStringDomain[T] = {

    if (isBottom) return this

    // Check if we assume something about non-numerical values - if so, return
    val ids = Normalizer.getIdsForExpression(expr)
      for (id <- ids) {
      if (!id.typ.isStringType) {
        return this
      }
    }
    expr match {

      // Comparison
      case BinaryArithmeticExpression(a:Expression, b:Expression, ArithmeticOperator.==, _) =>

        val left = eval(a)
        val right = eval(b)

        val intersection = dom.intersect(left,right)

        if (intersection.isBottom) bottom()
        else (a,b) match {
          case (x:Identifier, y:Identifier) =>
            this.add(x,intersection).add(y,intersection)
          case (x:Identifier, y:Expression) =>
            this.add(x,intersection)
          case (y:Expression, x:Identifier) =>
            this.add(x,intersection)
          case _ => this
        }

      // Negated comparison
      case NegatedBooleanExpression(BinaryArithmeticExpression(a:Expression, b:Expression, ArithmeticOperator.==, _)) =>

        val left = eval(a)
        val right = eval(b)

        val diff = dom.diff(left,right)

        // We can only assume anything about inequalities if left or right is a singleton
        var curState = this
        if (a.isInstanceOf[Identifier] && right.isSingleton) curState = curState.add(a.asInstanceOf[Identifier],diff)
        if (b.isInstanceOf[Identifier] && left.isSingleton) curState = curState.add(b.asInstanceOf[Identifier],diff)
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
        case BooleanOperator.|| => assume(left).lub(assume(right))
      }

      case _ => this
    }

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

case class StringKSetDomain(
    K: Int,
    value: Set[String] = Set.empty[String],
    isTop: Boolean = false,
    isBottom: Boolean = false)
  extends KSetDomain[String, StringKSetDomain]
  with StringValueSetDomain[StringKSetDomain] {

  def setFactory(
      value: Set[String] = Set.empty[String],
      isTop: Boolean = false,
      isBottom: Boolean = false) =
    StringKSetDomain(K, value, isTop, isBottom)

  def isSingleton:Boolean = !isBottom && !isTop && value.size == 1

  def diff(a: StringKSetDomain, b: StringKSetDomain): StringKSetDomain = {
    a.remove(b).lub(b.remove(a))
  }

  def intersect(a: StringKSetDomain, b: StringKSetDomain): StringKSetDomain = a.glb(b)

  def singleton(a: String): StringKSetDomain = factory().add(a)

  def concat(other:StringKSetDomain):StringKSetDomain = {
    var ret = factory()
    for (left <- this.value)
      for (right <- other.value)
        ret = ret.add(left + right)
    ret
  }
}

trait NumericWithStringDomain[
    N <: NumericalDomain[N],
    V <: StringValueDomain[V],
    S <: StringDomain[V, S],
    T <: NumericWithStringDomain[N, V, S, T]]
  extends SemanticCartesianProductDomain[N, S, T]
  with NumericalDomain[T] { this: T =>

  def initialNum: N = _1

  def initialStr: S = _2

  override def toString = "Numeric:\n"+ToStringUtilities.indent(_1.toString)+"\nString:\n"+ToStringUtilities.indent(_2.toString)
}