package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class SetCharacters(_value: Set[Char] = Set.empty[Char], _isTop: Boolean = false, _isBottom: Boolean = false)
  extends SetDomain[Char, SetCharacters](_value,_isTop,_isBottom) {

  def setFactory (_value: Set[Char] = Set.empty[Char], _isTop: Boolean = false, _isBottom: Boolean = false): SetCharacters
    = new SetCharacters(_value,_isTop,_isBottom)
}

class MaybeContainedCharacters
  (_value:Map[Identifier, SetCharacters] = Map.empty[Identifier, SetCharacters],_isBottom:Boolean = false,_isTop:Boolean = false)
  extends BoxedDomain[SetCharacters, MaybeContainedCharacters](_value,_isBottom,_isTop)
  with SimplifiedSemanticDomain[MaybeContainedCharacters] {

  def functionalFactory(_value:Map[Identifier, SetCharacters] = Map.empty[Identifier, SetCharacters],_isBottom:Boolean = false,_isTop:Boolean = false) : MaybeContainedCharacters =
    new MaybeContainedCharacters(_value,_isBottom,_isTop)

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
  
  def get(variable : Identifier) = value.get(variable) match {
    case Some(x) => x;
    case None => new SetCharacters().top();
  }
  
  def setToTop(variable : Identifier) : MaybeContainedCharacters = this.remove(variable);
  
  def assign(variable : Identifier, expr : Expression) : MaybeContainedCharacters = this.add(variable, this.eval(expr));
  
  def assume(expr : Expression) : MaybeContainedCharacters = expr match {
	case BinaryArithmeticExpression(AbstractOperator(thisExpr : Identifier, parameters, typeparameters, 
      AbstractOperatorIdentifiers.stringIndexof, returntyp), Constant("0", typ2, pp), ArithmeticOperator.>=, typ) =>
	    	  val l : List[Expression] = parameters;
	    	  if(l.size != 1) return this;
	    	  l.apply(0) match {
	    	    case Constant(s, typ2, pp) =>
	    	      val c = Integer.decode(s).intValue().asInstanceOf[Char];
	    	      return this.add(thisExpr, this.get(thisExpr).add(c));
	    	    case _ => this;
	    	  }
	case BinaryArithmeticExpression(AbstractOperator(thisExpr : Identifier, parameters, typeparameters, 
      AbstractOperatorIdentifiers.stringLastindexof, returntyp), Constant("0", typ2, pp), ArithmeticOperator.>=, typ) =>
	    	  val l : List[Expression] = parameters;
	    	  if(l.size != 1) return this;
	    	  l.apply(0) match {
	    	    case Constant(s, typ2, pp) =>
	    	      val c = Integer.decode(s).intValue().asInstanceOf[Char];
	    	      return this.add(thisExpr, this.get(thisExpr).add(c));
	    	    case _ => this;
	    	  }
	case BinaryArithmeticExpression(AbstractOperator(thisExpr : Identifier, parameters, typeparameters, 
      AbstractOperatorIdentifiers.stringIndexof, returntyp), Constant("0", typ2, pp), ArithmeticOperator.<, typ) =>
	    	  val l : List[Expression] = parameters;
	    	  if(l.size != 1) return this;
	    	  l.apply(0) match {
	    	    case Constant(s, typ2, pp) =>
	    	      val c = Integer.decode(s).intValue().asInstanceOf[Char];
	    	      return this.add(thisExpr, this.get(thisExpr).remove(c));
	    	    case _ => this;
	    	  }
	case BinaryArithmeticExpression(AbstractOperator(thisExpr : Identifier, parameters, typeparameters, 
      AbstractOperatorIdentifiers.stringLastindexof, returntyp), Constant("0", typ2, pp), ArithmeticOperator.<, typ) =>
	    	  val l : List[Expression] = parameters;
	    	  if(l.size != 1) return this;
	    	  l.apply(0) match {
	    	    case Constant(s, typ2, pp) =>
	    	      val c = Integer.decode(s).intValue().asInstanceOf[Char];
	    	      return this.add(thisExpr, this.get(thisExpr).remove(c));
	    	    case _ => this;
	    	  }
	case AbstractOperator(thisExpr : Identifier, parameters, typeparameters, AbstractOperatorIdentifiers.stringContains, returntyp) =>
	    	  val l : List[Expression] = parameters;
	    	  if(l.size != 1) return this;
	    	  l.apply(0) match {
	    	    case Constant(s, typ2, pp) =>
	    	      val c = Integer.decode(s).intValue().asInstanceOf[Char];
	    	      return this.add(thisExpr, this.get(thisExpr).add(c));
	    	    case _ => this;
	    	  }    
	case _ => this;
  }
  
  def createVariable(variable : Identifier, typ : Type) : MaybeContainedCharacters = this;
  
  def removeVariable(variable : Identifier) : MaybeContainedCharacters = this.remove(variable);
  
  private def eval(expr : Expression) : SetCharacters = expr match {
    case x : Identifier => this.get(x)
    case x : Constant if x.constant.isInstanceOf[String] => 
      var result=new SetCharacters(); 
      for(c <- x.constant.asInstanceOf[String].toCharArray())
        result=result.add(c);
      return result;
    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, returntyp) =>
        parameters match {
        	case p1 :: Nil => 
	        	val left = this.eval(thisExpr);
	        	val right = this.eval(p1);
	        	left.lub(right)
        	case _ =>
            new SetCharacters().top()
      	}
    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringSubstring, returntyp) =>
      return this.eval(thisExpr);
    case _ => return new SetCharacters().top();
  }  
}