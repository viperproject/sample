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
    case Constant("valid", typ, pp) => // FIXME: This will break once somebody has a string constant valid
      dom.top()
    case Constant("invalid", typ, pp) => // FIXME: This will break once somebody has a string constant invalid
      dom.top() // FIXME: Wouldn't bottom be better?
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

  override def isCovered(i:Identifier):Boolean = i.getType().isStringType()

  /**
   * This is imprecise, but sound
   */
  override def assume(expr: Expression): NonrelationalStringDomain[T] = {

    if (isBottom) return this

    // Check if we assume something about non-numerical values - if so, return
    val ids = Normalizer.getIdsForExpression(expr)
      for (id <- ids) {
      if (!id.getType().isStringType()) {
        return this
      }
    }

    // Check if we just assume if something is invalid - we dont know that here
    // TODO: Filter everything with valid or invalid
    expr match {
      case BinaryArithmeticExpression(_,Constant("invalid",_,_),_,_) => return this
      case _ => ()
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
        case BooleanOperator.|| => this.assume(NegatedBooleanExpression(left)).assume(NegatedBooleanExpression(right))
        case BooleanOperator.&& => this.lub(this.assume(NegatedBooleanExpression(left)),this.assume(NegatedBooleanExpression(right)))
      }

      // AND, OR
      case BinaryBooleanExpression(left,right,op,typ) => op match {
        case BooleanOperator.&& => this.assume(left).assume(right)
        case BooleanOperator.|| => this.lub(this.assume(left),this.assume(right))
      }

      case _ => this
    }

  }

}

trait StringValueDomain[T <: StringValueDomain[T]] extends Lattice[T] {

  def isBottom:Boolean

  def isTop:Boolean

  def isSingleton:Boolean

  def intersect(a:T,b:T):T

  def diff(a:T,b:T):T

  def singleton(a:String):T

}

class StringKSetDomain extends KSetDomain[String,StringKSetDomain] with StringValueDomain[StringKSetDomain] {

  def factory(): StringKSetDomain = new StringKSetDomain

  def isSingleton:Boolean = !isBottom && !isTop && value.size == 1

  def getK: Int = TouchAnalysisParameters.stringRepresentationBound

  def diff(a: StringKSetDomain, b: StringKSetDomain): StringKSetDomain = {
    lub(a.remove(b),b.remove(a))
  }

  def intersect(a: StringKSetDomain, b: StringKSetDomain): StringKSetDomain = glb(a,b)

  def singleton(a: String): StringKSetDomain = factory().add(a)

}

abstract class NumericWithStringDomain[N <: NumericalDomain[N], V <: StringValueDomain[V], S <: StringDomain[V,S], T <: NumericWithStringDomain[N,V,S,T]](val initialNum:N,val initialStr:S)
  extends SemanticCartesianProductDomain[N,S,T](initialNum,initialStr)
  with NumericalDomain[T] {

  override def toString() = "Numeric:\n"+ToStringUtilities.indent(d1.toString())+"\nString:\n"+ToStringUtilities.indent(d2.toString)
}