package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class SuffixDomain extends Lattice[SuffixDomain]
{
  var isBottom : Boolean = false;
  var isTop : Boolean = false;
  var stringValue : String = "";
  
  override def factory() : SuffixDomain = new SuffixDomain();
  
  def top() : SuffixDomain = {
    val result : SuffixDomain = this.factory();
    result.isTop = true;
    result.isBottom = false; 
    result
  }
  
  def bottom() : SuffixDomain = { 
	  val result : SuffixDomain = this.factory();
	  result.isBottom = true;
      result.isTop = false;
	  result
  }
  
  def foo(cnt : Int, lengthLeft : Int, left : String, lengthRight : Int, right : String) : Int = {
      if(cnt >= lengthLeft || cnt >= lengthRight)
        return cnt;
      else if(left.charAt(lengthLeft - 1 - cnt).equals(right.charAt(lengthRight - 1 - cnt)))
		return foo(cnt + 1, lengthLeft, left, lengthRight, right);
	  else
		return cnt;
  }
  
  def lub(left : SuffixDomain, right : SuffixDomain) : SuffixDomain = {
	  val leftLength = left.stringValue.length();
	  val rightLength = right.stringValue.length();
	  if(leftLength == 0 || rightLength == 0)
		  return top();
	  val cnt : Int = foo(0, leftLength, left.stringValue, rightLength, right.stringValue);
   	  if(cnt <= 0)
   		  return top();
      else {
    	 val result = this.factory();
    	 //Console.println("left: " + left.stringValue + "; right: " + right.stringValue + "; leftlength: "  + leftLength + "; cnt: " + cnt);
    	 result.stringValue = left.stringValue.substring(leftLength - cnt, leftLength);
    	 return result
      }
  }
  
  def glb(left : SuffixDomain, right : SuffixDomain) : SuffixDomain = {
	  if(left.lessEqual(right))
		  left;
	  else if(right.lessEqual(left))
		  right;
	  else
		  bottom();
  }
  
  def widening(left : SuffixDomain, right : SuffixDomain) : SuffixDomain = {
	  lub(left,right);
  }
  
  def lessEqual(r : SuffixDomain) : Boolean = {
	  if(this.stringValue.endsWith(r.stringValue))
		  true;
	  else
		  false;
  }  
  
  override def toString() : String = {
    if(this.isBottom) 
      return "_|_"; 
    else if(this.isTop)
      return "_T_";
    else
      return stringValue;
  }
}

class Suffix extends SimplifiedSemanticDomain[Suffix] with BoxedDomain[SuffixDomain, Suffix] 
{
   def factory() : Suffix = new Suffix();
   def setToTop(variable : Identifier) : Suffix = this.remove(variable);
   def assign(variable : Identifier, expr : Expression) : Suffix = this.add(variable, this.eval(expr));
   def assume(expr : Expression) : Suffix = this;
   def createVariable(variable : Identifier, typ : Type) : Suffix = this;
   def removeVariable(variable : Identifier) : Suffix = this.remove(variable);
 
   def get(variable : Identifier) = value.get(variable) match {
    case Some(x) => x;
    case None => new SuffixDomain().top();
   }
   override def getStringOfId(id : Identifier) : String = {
     get(id).toString();
   }
   
   private def eval(expr : Expression) : SuffixDomain = expr match {
    case x : Identifier => this.get(x)
    case x : Constant if x.constant.isInstanceOf[String] => {
      //Console.println("costruzione string: " + x.constant.asInstanceOf[String]);
      var result = new SuffixDomain(); 
      result.stringValue = x.constant.asInstanceOf[String];
      return result;
    }
    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, returntyp) =>
      if(parameters.size == 1)
        parameters.head match {
        	case p1 :: Nil => return this.eval(p1);
        	case _ => new SuffixDomain().top();
         }
      else
          return new SuffixDomain().top();
    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringSubstring, returntyp) =>
      return new SuffixDomain().top();
    case _ => return new SuffixDomain().top();
  }
}