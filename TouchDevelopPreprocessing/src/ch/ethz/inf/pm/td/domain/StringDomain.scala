package ch.ethz.inf.pm.td.domain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.td.analysis.TouchAnalysisParameters
import ch.ethz.inf.pm.sample.oorepresentation.Type
import numericaldomain.NumericalDomain
import ch.ethz.inf.pm.sample.ToStringUtilities

trait StringDomain[T <: StringValueDomain[T],X <: StringDomain[T,X]] extends SimplifiedSemanticDomain[X]

/**
 * Represents a non-relational string domain.
 *
 * @param dom An instance of the value domain. Only for factory purposes
 * @tparam T The type of the value domain
 */
class NonrelationalStringDomain[T <:StringValueDomain[T]](dom:T)
  extends BoxedDomain[T,NonrelationalStringDomain[T]]
  with StringDomain[T,NonrelationalStringDomain[T]] {

  override def factory() = new NonrelationalStringDomain[T](dom)

  def get(key : Identifier) : T = value.get(key) match {
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
    if (variable.getType().isStringType()) {
      val res = eval(expr)
      if (res.isBottom) bottom()
      else if (variable.representSingleVariable()) this.add(variable, res)
      else this.add(variable, dom.lub(this.get(variable), res))
    } else this
  }

  override def backwardAssign(variable: Identifier, expr: Expression): NonrelationalStringDomain[T]  = this

  override def access(field: Identifier) = this

  override def backwardAccess(field: Identifier) = this

  private def eval(expr: Expression): T = expr match {
    case Constant(constant, typ, pp) =>
      dom.singleton(constant)
    case x: Identifier =>
      this.get(x)
    case xs: HeapIdSetDomain[_] =>
      var result = dom.bottom()
      for (x <- xs.value) result = result.lub(result, this.get(x))
      result
    case x: Expression => dom.top()
  }

  /**
   * This is imprecise, but sound
   */
  override def assume(expr: Expression): NonrelationalStringDomain[T] = expr match {

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

      if (diff.isBottom) bottom()
      else (a,b) match {
        case (x:Identifier, y:Identifier) =>
          this.add(x,diff).add(y,diff)
        case (x:Identifier, y:Expression) =>
          this.add(x,diff)
        case (y:Expression, x:Identifier) =>
          this.add(x,diff)
        case _ => this
      }

    // AND, OR
    case BinaryBooleanExpression(left,right,op,typ) => op match {
      case BooleanOperator.&& => this.assume(left).assume(right)
      case BooleanOperator.|| => this.lub(this.assume(left),this.assume(right))
    }

    case _ => this
  }

}

trait StringValueDomain[T <: StringValueDomain[T]] extends Lattice[T] {

  def isBottom:Boolean

  def isTop:Boolean

  def intersect(a:T,b:T):T

  def diff(a:T,b:T):T

  def singleton(a:String):T

}

class StringKSetDomain extends KSetDomain[String,StringKSetDomain] with StringValueDomain[StringKSetDomain] {

  def factory(): StringKSetDomain = new StringKSetDomain

  def getK: Int = TouchAnalysisParameters.stringRepresentationBound

  def diff(a: StringKSetDomain, b: StringKSetDomain): StringKSetDomain = lub(a.remove(b),b.remove(a))

  def intersect(a: StringKSetDomain, b: StringKSetDomain): StringKSetDomain = glb(a,b)

  def singleton(a: String): StringKSetDomain = factory().add(a)

}

abstract class NumericWithStringDomain[N <: NumericalDomain[N], V <: StringValueDomain[V], S <: StringDomain[V,S], T <: NumericWithStringDomain[N,V,S,T]](val num:N,val str:S)
  extends SemanticCartesianProductDomain[N,S,T](num,str)
  with NumericalDomain[T] {

  override def toString() = "Numeric:\n"+ToStringUtilities.indent(d1.toString())+"\nString:\n"+ToStringUtilities.indent(d2.toString)
}