package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

trait CharacterSet extends SetDomain[Char, CharacterSet] {
  override def factory(value: Set[Char]): CharacterSet = InnerCharacterSet(value)
  override def bottom(): CharacterSet = BottomCharacterSet
  override def top(): CharacterSet = TopCharacterSet
}
object BottomCharacterSet extends CharacterSet with SetDomain.Bottom[Char, CharacterSet]
object TopCharacterSet extends CharacterSet with SetDomain.Top[Char, CharacterSet]
case class InnerCharacterSet(value:Set[Char]) extends CharacterSet with SetDomain.Inner[Char, CharacterSet, InnerCharacterSet]


class MaybeContainedCharacters
(val map: Map[Identifier, CharacterSet] = Map.empty[Identifier, CharacterSet], override val isBottom: Boolean = false, val isTop: Boolean = false)
  extends BoxedDomain[CharacterSet, MaybeContainedCharacters]
  with SimplifiedSemanticDomain[MaybeContainedCharacters] {

  def functionalFactory(_value: Map[Identifier, CharacterSet] = Map.empty[Identifier, CharacterSet], _isBottom: Boolean = false, _isTop: Boolean = false): MaybeContainedCharacters =
    new MaybeContainedCharacters(_value, _isBottom, _isTop)

  /*
   * The following methods are already defined by BoxedDomain
   * def top() : ContainedCharacters
   * def bottom() : ContainedCharacters
   * def lub(left : ContainedCharacters, right : ContainedCharacters) : ContainedCharacters=this  
   * def glb(left : ContainedCharacters, right : ContainedCharacters) : ContainedCharacters=this  
   * def widening(left : ContainedCharacters, right : ContainedCharacters) : ContainedCharacters
   * def lessEqual(r : ContainedCharacters) : Boolean=true
   * def getStringOfId(id : Identifier) : String="";
   */

  def get(variable: Identifier) = map.get(variable) match {
    case Some(x) => x;
    case None => TopCharacterSet
  }

  def setToTop(variable: Identifier): MaybeContainedCharacters = this.remove(variable)

  def assign(variable: Identifier, expr: Expression): MaybeContainedCharacters = this.add(variable, this.eval(expr))

  def assume(expr: Expression): MaybeContainedCharacters = expr match {
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, typeParameters,
    AbstractOperatorIdentifiers.stringIndexof, returnTyp), Constant("0", typ2, pp), ArithmeticOperator.>=, typ) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.apply(0) match {
        case Constant(s, _, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          return this.add(thisExpr, this.get(thisExpr).add(c));
        case _ => this;
      }
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, typeParameters,
    AbstractOperatorIdentifiers.stringLastindexof, returnTyp), Constant("0", typ2, pp), ArithmeticOperator.>=, typ) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.apply(0) match {
        case Constant(s, _, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          return this.add(thisExpr, this.get(thisExpr).add(c));
        case _ => this;
      }
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, typeParameters,
    AbstractOperatorIdentifiers.stringIndexof, returnTyp), Constant("0", typ2, pp), ArithmeticOperator.<, typ) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.apply(0) match {
        case Constant(s, _, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          return this.add(thisExpr, this.get(thisExpr).remove(c));
        case _ => this;
      }
    case BinaryArithmeticExpression(AbstractOperator(thisExpr: Identifier, parameters, typeParameters,
    AbstractOperatorIdentifiers.stringLastindexof, returnTyp), Constant("0", typ2, pp), ArithmeticOperator.<, typ) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.apply(0) match {
        case Constant(s, _, _) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          return this.add(thisExpr, this.get(thisExpr).remove(c));
        case _ => this;
      }
    case AbstractOperator(thisExpr: Identifier, parameters, typeParameters, AbstractOperatorIdentifiers.stringContains, returnTyp) =>
      val l: List[Expression] = parameters
      if (l.size != 1) return this
      l.apply(0) match {
        case Constant(s, typ2, pp) =>
          val c = Integer.decode(s).intValue().asInstanceOf[Char]
          return this.add(thisExpr, this.get(thisExpr).add(c));
        case _ => this;
      }
    case _ => this;
  }

  def createVariable(variable: Identifier, typ: Type): MaybeContainedCharacters = this

  def removeVariable(variable: Identifier): MaybeContainedCharacters = this.remove(variable)

  private def eval(expr: Expression): CharacterSet = expr match {
    case x: Identifier => this.get(x)
    case x: Constant if x.constant.isInstanceOf[String] =>
      var result:CharacterSet = InnerCharacterSet(Set.empty)
      for (c <- x.constant.toCharArray)
        result = result.add(c)
      return result;
    case AbstractOperator(thisExpr, parameters, typeParameters, AbstractOperatorIdentifiers.stringConcatenation, returnTyp) =>
      parameters match {
        case p1 :: Nil =>
          val left = this.eval(thisExpr)
          val right = this.eval(p1)
          left.lub(right)
        case _ => TopCharacterSet
      }
    case AbstractOperator(thisExpr, parameters, typeParameters, AbstractOperatorIdentifiers.stringSubstring, returnTyp) =>
      return this.eval(thisExpr);
    case _ => TopCharacterSet
  }
}