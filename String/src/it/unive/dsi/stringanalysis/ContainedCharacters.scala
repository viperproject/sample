package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class InverseSetCharacters extends InverseSetDomain[Char, InverseSetCharacters] {
  def factory() : InverseSetCharacters = new InverseSetCharacters();
}

class SurelyContainedCharacters extends SimplifiedSemanticDomain[SurelyContainedCharacters] 
     with BoxedDomain[InverseSetCharacters, SurelyContainedCharacters] {

  def factory() : SurelyContainedCharacters = new SurelyContainedCharacters ();
  
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
    case None => new InverseSetCharacters().top();
  }
  
  def setToTop(variable : Identifier) : SurelyContainedCharacters = this.remove(variable);
  
  def assign(variable : Identifier, expr : Expression) : SurelyContainedCharacters = this.add(variable, this.eval(expr));
  
  def assume(expr : Expression) : SurelyContainedCharacters = expr match {    
	case BinaryArithmeticExpression(AbstractOperator(thisExpr : Identifier, parameters, typeparameters, 
      AbstractOperatorIdentifiers.stringIndexof, returntyp), Constant("0", typ2, pp), ArithmeticOperator.>=, typ) =>
	    	  if(parameters.size!=1) return this;
	    	  val l : List[Expression] = parameters.head;
	    	  if(l.size != 1) return this;
	    	  l.apply(0) match {
	    	    case Constant(s, typ2, pp) =>
	    	      val c = Integer.decode(s).intValue().asInstanceOf[Char];
	    	      return this.add(thisExpr, this.get(thisExpr).add(c));
	    	    case _ => this;
	    	  }
	case BinaryArithmeticExpression(AbstractOperator(thisExpr : Identifier, parameters, typeparameters, 
      AbstractOperatorIdentifiers.stringLastindexof, returntyp), Constant("0", typ2, pp), ArithmeticOperator.>=, typ) =>
	    	  if(parameters.size!=1) return this;
	    	  val l : List[Expression] = parameters.head;
	    	  if(l.size != 1) return this;
	    	  l.apply(0) match {
	    	    case Constant(s, typ2, pp) =>
	    	      val c = Integer.decode(s).intValue().asInstanceOf[Char];
	    	      return this.add(thisExpr, this.get(thisExpr).add(c));
	    	    case _ => this;
	    	  }
	case BinaryArithmeticExpression(AbstractOperator(thisExpr : Identifier, parameters, typeparameters, 
      AbstractOperatorIdentifiers.stringIndexof, returntyp), Constant("0", typ2, pp), ArithmeticOperator.<, typ) =>
	    	  if(parameters.size!=1) return this;
	    	  val l : List[Expression] = parameters.head;
	    	  if(l.size != 1) return this;
	    	  l.apply(0) match {
	    	    case Constant(s, typ2, pp) =>
	    	      val c = Integer.decode(s).intValue().asInstanceOf[Char];
	    	      return this.add(thisExpr, this.get(thisExpr).remove(c));
	    	    case _ => this;
	    	  }
	case BinaryArithmeticExpression(AbstractOperator(thisExpr : Identifier, parameters, typeparameters, 
      AbstractOperatorIdentifiers.stringLastindexof, returntyp), Constant("0", typ2, pp), ArithmeticOperator.<, typ) =>
	    	  if(parameters.size!=1) return this;
	    	  val l : List[Expression] = parameters.head;
	    	  if(l.size != 1) return this;
	    	  l.apply(0) match {
	    	    case Constant(s, typ2, pp) =>
	    	      val c = Integer.decode(s).intValue().asInstanceOf[Char];
	    	      return this.add(thisExpr, this.get(thisExpr).remove(c));
	    	    case _ => this;
	    	  }
	case AbstractOperator(thisExpr : Identifier, parameters, typeparameters, AbstractOperatorIdentifiers.stringContains, returntyp) =>
    		  if(parameters.size!=1) return this;
	    	  val l : List[Expression] = parameters.head;
	    	  if(l.size != 1) return this;
	    	  l.apply(0) match {
	    	    case Constant(s, typ2, pp) =>
	    	      val c = Integer.decode(s).intValue().asInstanceOf[Char];
	    	      return this.add(thisExpr, this.get(thisExpr).add(c));
	    	    case _ => this;
	    	  }
    case _ => this;
  }
  
  def createVariable(variable : Identifier, typ : Type) : SurelyContainedCharacters = this;
  
  def removeVariable(variable : Identifier) : SurelyContainedCharacters = this.remove(variable);
  
  private def eval(expr : Expression) : InverseSetCharacters = expr match {
    case x : Identifier => this.get(x)
    case x : Constant if x.constant.isInstanceOf[String] => 
      var result=new InverseSetCharacters(); 
      for(c <- x.constant.asInstanceOf[String].toCharArray())
        result=result.add(c);
      return result;
    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, returntyp) =>
      if(parameters.size == 1)
        parameters.head match {
        	case p1 :: Nil => 
	        	val left = this.eval(thisExpr);
	        	val right = this.eval(p1);
	        	return left.glb(left, right);
        	case _ => return new InverseSetCharacters().top();
      	}
      else return new InverseSetCharacters().top();
    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringSubstring, returntyp) => 
      return new InverseSetCharacters().top();
    case _ => return new InverseSetCharacters().top();
  }  
}