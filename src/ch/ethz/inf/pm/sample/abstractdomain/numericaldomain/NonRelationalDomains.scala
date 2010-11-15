package ch.ethz.inf.pm.sample.abstractdomain.numericaldomain
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._
import scala.Math


trait NonRelationalNumericalDomain[N <: NonRelationalNumericalDomain[N]] extends Lattice[N] {
	def evalConstant(value : Int) : N;
	def sum(leftExpr : N, rightExpr : N) : N;
	def subtract(leftExpr : N, rightExpr : N) : N;
	def multiply(leftExpr : N, rightExpr : N) : N;
	def divide(leftExpr : N, rightExpr : N) : N;
	def valueGEQ(value : N) : N;
	def valueLEQ(value : N) : N;
}

class BoxedNonRelationalNumericalDomain[N <: NonRelationalNumericalDomain[N]](dom : N) extends BoxedDomain[N, BoxedNonRelationalNumericalDomain[N]]() with NumericalDomain[BoxedNonRelationalNumericalDomain[N]] {
  final def factory() = new BoxedNonRelationalNumericalDomain[N](dom.factory());
    
  def get(key : Identifier) : N = value.get(key) match {
    case None => dom.bottom();
    case Some(x) => x
  }
  
  
  override def createVariable(variable : Identifier, typ : Type) : BoxedNonRelationalNumericalDomain[N] = {
    return this.add(variable, dom.bottom());
  }
  override def createVariableForParameter(variable : Identifier, typ : Type, path : List[String]) = {
    var result = Map.empty[Identifier, List[String]];
    result=result+((variable, path ::: variable.toString() :: Nil));
    (this.add(variable, dom.top()), result);
  }
  
  override def setToTop(variable : Identifier) : BoxedNonRelationalNumericalDomain[N] = {
    this.add(variable, dom.top());
  }
  
  override def removeVariable(variable : Identifier) : BoxedNonRelationalNumericalDomain[N] = {
    this.remove(variable);
  }
  
  override def setParameter(variable : Identifier, expr : Expression) : BoxedNonRelationalNumericalDomain[N]= this.assign(variable, expr);
  
  override def assign(variable : Identifier, expr : Expression) : BoxedNonRelationalNumericalDomain[N]= {
    if(variable.representSingleVariable)
    	this.add(variable, eval(expr));
    else this.add(variable, this.get(variable).lub(this.get(variable), eval(expr)))
  }
  
  override def backwardAssign(variable : Identifier, expr : Expression) : BoxedNonRelationalNumericalDomain[N]= this
  
  override def access(field : Identifier)=this;
  
  override def backwardAccess(field : Identifier)=this;
  
  private def eval(expr : Expression) : N = expr match {
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.+, typ) => return dom.sum(eval(left), eval(right));
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.*, typ) => return dom.multiply(eval(left), eval(right));
    case BinaryArithmeticExpression(left, right, ArithmeticOperator./, typ) => return dom.divide(eval(left), eval(right));
    case BinaryArithmeticExpression(left, right, ArithmeticOperator.-, typ) => return dom.subtract(eval(left), eval(right));
    case BinaryArithmeticExpression(left, right, op, typ) => dom.top() //TODO: implement it!!! 
    case Constant(constant, typ) => try { return dom.evalConstant(Integer.valueOf(constant).intValue());} catch {case _ => return dom.top();}
    case x : Identifier => this.get(x)
  }
    
  override def assume(expr : Expression) : BoxedNonRelationalNumericalDomain[N]= Normalizer.conditionalExpressionToMonomes(expr) match {
    case None => return this;
    case Some((monomes, constant)) =>
      var stateResult : BoxedNonRelationalNumericalDomain[N] = this;
      for(monome <- monomes) {
        val (index, variable) = monome;
        var result = dom.evalConstant(constant)
      	for(monome1 <- monomes) {
      	  if(! monome.equals(monome1))
      		  result=dom.sum(result, dom.multiply(dom.evalConstant(monome1._1), eval(monome1._2)))
      	}
       if(index >= 0) {
         //k*x+n >= 0 => x >= -(n/k)
    	   result=dom.divide(result, dom.evalConstant(index))
    	   result=dom.subtract(dom.evalConstant(0), result)
    	   stateResult=stateResult.add(variable, dom.glb(this.get(variable), dom.valueGEQ(result)))
        }
       else {
         //-k*x+n >= 0 => x <= n/-k
    	   result=dom.divide(result, dom.evalConstant(-index))
    	   val oldvalue=this.get(variable)
    	   val newRestraint=dom.valueLEQ(result)
    	   val newValue=dom.glb(oldvalue, newRestraint)
    	   stateResult=stateResult.add(variable, newValue)
        }
      }
      return stateResult;
  }
  
}



class Top extends NonRelationalNumericalDomain[Top] {
  final override def factory() = this;
  override def toString() = "T";
  def top() : Top = this;
  def bottom() : Top = this;
  def lub(left : Top, right : Top) : Top = this
  def glb(left : Top, right : Top) : Top = this;
  def widening(left : Top, right : Top) : Top = this;
  def lessEqual(right : Top) : Boolean = true
  override def equals(o : Any) = true
  def evalConstant(value : Int) : Top = this;
  def sum(leftExpr : Top, rightExpr : Top) : Top = this;
  def subtract(leftExpr : Top, rightExpr : Top) : Top = this;
  def multiply(leftExpr : Top, rightExpr : Top) : Top = this;
  def divide(leftExpr : Top, rightExpr : Top) : Top = this;
  def valueGEQ(value : Top) : Top = this;
  def valueLEQ(value : Top) : Top = this;
}



object SignValues extends Enumeration {
  val + = Value("+");
  val ZERO = Value("0");
  val - = Value("-");
  val T = Value("T");
  val BOT = Value("_|_");
}

class Sign(val value : SignValues.Value) extends NonRelationalNumericalDomain[Sign] {

  final override def factory() = top();
  
  override def toString() = value.toString;
  
  def top() : Sign = new Sign(SignValues.T);
  
  def bottom() : Sign = new Sign(SignValues.BOT);
  
  def lub(left : Sign, right : Sign) : Sign = {
    if(left.value==SignValues.T || right.value==SignValues.T) return top();
    if(left.value==SignValues.BOT) return right;
    if(right.value==SignValues.BOT) return left;
    if(left.equals(right)) return left;
    return top();
  }
  
  def glb(left : Sign, right : Sign) : Sign = {
    if(left.value==SignValues.BOT || right.value==SignValues.BOT) return bottom();
    if(left.value==SignValues.T) return right;
    if(right.value==SignValues.T) return left;
    if(left.equals(right)) return left;
    return bottom();
  }
  
  def widening(left : Sign, right : Sign) : Sign = this.lub(left, right);
  
  def lessEqual(right : Sign) : Boolean = {
    val left : Sign = this;
    if(left.value==SignValues.BOT || right.value==SignValues.T) return true;
    if(right.equals(left)) return true;
    return false;
  }
  
  override def equals(o : Any) = o match {
    case x : Sign => x.value.equals(value);
    case _ => false;
  }
  
  def sum(leftExpr : Sign, rightExpr : Sign) : Sign = {
      if(leftExpr.value == SignValues.BOT || rightExpr.value == SignValues.BOT) return new Sign(SignValues.BOT)
      if(leftExpr.value == SignValues.T || rightExpr.value == SignValues.T) return new Sign(SignValues.T)
      if(leftExpr.value == rightExpr.value) return new Sign(leftExpr.value);
      if(leftExpr.value == SignValues.ZERO) return new Sign(rightExpr.value);
      if(rightExpr.value == SignValues.ZERO) return new Sign(leftExpr.value);
      return new Sign(SignValues.T);
  }
  
  def subtract(leftExpr : Sign, rightExpr : Sign) : Sign = {
      if(leftExpr.value == SignValues.BOT || rightExpr.value == SignValues.BOT) return new Sign(SignValues.BOT)
      if(leftExpr.value == SignValues.T || rightExpr.value == SignValues.T) return new Sign(SignValues.T)
      if(leftExpr.value == SignValues.ZERO) {
        if(rightExpr.value == SignValues.ZERO) return new Sign(SignValues.ZERO);
        else if(rightExpr.value == SignValues.+) return new Sign(SignValues.-);
        else return new Sign(SignValues.+);
      }
      if(leftExpr.value != rightExpr.value && leftExpr.value==SignValues.+) return new Sign(SignValues.+);
      if(leftExpr.value != rightExpr.value && leftExpr.value==SignValues.-) return new Sign(SignValues.-);
      return new Sign(SignValues.T);
  }

  def multiply(leftExpr : Sign, rightExpr : Sign) : Sign = {
      if(leftExpr.value == SignValues.BOT || rightExpr.value == SignValues.BOT) return new Sign(SignValues.BOT)
      if(leftExpr.value == SignValues.ZERO || leftExpr.value == SignValues.ZERO) return new Sign(SignValues.ZERO);
      if(leftExpr.value == SignValues.T || rightExpr.value == SignValues.T) return new Sign(SignValues.T)
      if(leftExpr.value == rightExpr.value) return new Sign(SignValues.+);
      return new Sign(SignValues.-);
  }

  def divide(leftExpr : Sign, rightExpr : Sign) : Sign = {
      if(leftExpr.value == SignValues.BOT || rightExpr.value == SignValues.BOT || rightExpr.value == SignValues.ZERO) return new Sign(SignValues.BOT)
      if(leftExpr.value == SignValues.ZERO) return new Sign(SignValues.ZERO);
      if(leftExpr.value == SignValues.T || rightExpr.value == SignValues.T) return new Sign(SignValues.T)
      if(leftExpr.value == rightExpr.value) return new Sign(SignValues.+);
      return new Sign(SignValues.-);
  }
  
  def evalConstant(value : Int) : Sign = { 
      if (value > 0) return new Sign(SignValues.+) 
      else if (value == 0) return new Sign(SignValues.ZERO) 
      else return new Sign(SignValues.-)
  }
  
  def valueGEQ(value : Sign) : Sign = {
    if(value.value==SignValues.+)
      return new Sign(SignValues.+);
    else return new Sign(SignValues.T);
  }
  
  def valueLEQ(value : Sign) : Sign = {
    if(value.value==SignValues.-)
      return new Sign(SignValues.-);
    else return this;
  }
}

class Interval(val left : Int, val right: Int) extends NonRelationalNumericalDomain[Interval] {
  final override def factory() = top();
  
  override def toString() : String = {
    if(this.isBottom) return "_|_"
    var result : String = "[";
    if(left==Math.MIN_INT)
      result=result+"-oo"
    else result=result+left.toString();
    result=result+"..";
    if(right==Math.MAX_INT)
      result=result+"+oo"
    else result=result+right.toString();
    result+"]";
  }
  
  def top() : Interval = new Interval(Math.MIN_INT, Math.MAX_INT);
  
  def bottom() : Interval = new Interval(1, 0);
  
  def isBottom() : Boolean = left > right 
  private def min(left : Int, right : Int)=if(left < right) left else right
  private def max(left : Int, right : Int)=if(left > right) left else right
  
  def lub(left : Interval, right : Interval) : Interval  = {
    if(left.isBottom) return right;
    if(right.isBottom) return left;
    return new Interval(min(left.left, right.left), max(left.right, right.right))
  }
  
  
  def glb(left : Interval, right : Interval) : Interval= {
    if(left.isBottom || right.isBottom) return bottom();
    return new Interval(max(left.left, right.left), min(left.right, right.right))
  }
  
  def widening(left : Interval, right : Interval) : Interval = {
    var result=this.lub(left, right);
    if(right.left < left.left)
      result=new Interval(Math.MIN_INT, result.right)
    if(right.right > left.right)
      result=new Interval(result.left, Math.MAX_INT)
    result;
  }
  
  def lessEqual(right : Interval) : Boolean = {
    val left : Interval = this;
    if(left.isBottom()) return true;
    if(right.isBottom()) return false;
    if(left.left >= right.left && left.right<=right.right)
      return true;
    else return false;
  }
  
  override def equals(o : Any) = o match {
    case x : Interval => x.left==this.left && x.right==this.right;
    case _ => false;
  }
  
  def evalConstant(value : Int) : Interval = { 
      return new Interval(value, value)
  }
  
  def sum(leftExpr : Interval, rightExpr : Interval) : Interval = {
    if(leftExpr.isBottom || rightExpr.isBottom) return new Interval(1,0);
    var left=leftExpr.left+rightExpr.left;
  	var right=leftExpr.right+rightExpr.right;
    if(leftExpr.left==Math.MIN_INT || rightExpr.left==Math.MIN_INT)
      left=Math.MIN_INT;
    if(leftExpr.right==Math.MAX_INT || rightExpr.right==Math.MAX_INT || (rightExpr.right+leftExpr.right)<rightExpr.right ) //Last case for overflow
      right=Math.MAX_INT;
    return new Interval(left, right);
    
  }
  
  def subtract(leftExpr : Interval, rightExpr : Interval) : Interval = {
    if(leftExpr.isBottom || rightExpr.isBottom) return new Interval(1,0);
    var left=leftExpr.left-rightExpr.left;
  	var right=leftExpr.right-rightExpr.right;
    if(leftExpr.left==Math.MIN_INT || rightExpr.left==Math.MIN_INT || (rightExpr.right-leftExpr.right)>rightExpr.right ) //Last case for underflow
      left=Math.MIN_INT;
    if(leftExpr.right==Math.MAX_INT || rightExpr.right==Math.MAX_INT)
      right=Math.MAX_INT;
    return new Interval(left, right);
  }
  
  private def max(a : Int, b : Int, c : Int, d : Int) : Int = max(max(a, b), max(c, d))
  
  private def min(a : Int, b : Int, c : Int, d : Int) : Int = min(min(a, b), min(c, d))
  
  private def managedMultiply(a : Int, b : Int) : Int = {
    if(a==0||b==0) return 0;
    var result : Int =a*b;
    if(result/a != b) { //Overflow
      if(a>=0 && b >=0) result=Math.MAX_INT;
      else if(a<=0 && b <=0) result=Math.MAX_INT;
      else result=Math.MIN_INT;
    }
    return result;
  }

  def multiply(leftExpr : Interval, rightExpr : Interval) : Interval = {
    val a = managedMultiply(leftExpr.left, rightExpr.left);
    val b = managedMultiply(leftExpr.left, rightExpr.right);
    val c = managedMultiply(leftExpr.right, rightExpr.left);
    val d = managedMultiply(leftExpr.right, rightExpr.right);
    var result=new Interval(min(a, b, c, d), max(a, b, c, d));
    return result;
  }
  
  def divide(leftExpr : Interval, rightExpr : Interval) : Interval = {
    val a = leftExpr.left/rightExpr.left;
    val b = leftExpr.left/rightExpr.right
    val c = leftExpr.right/rightExpr.left
    val d = leftExpr.right/rightExpr.right
    var result=new Interval(min(a, b, c, d), max(a, b, c, d));
    if(leftExpr.left<0 && leftExpr.right>0) //It contains 0
      result=result.lub(result, new Interval(0, 0))
    if(rightExpr.left<0 && rightExpr.right>0) { //It contains 0
      if(leftExpr.right > 0)
        result=new Interval(result.left, Math.MAX_INT)
      if(leftExpr.left < 0)
        result=new Interval(Math.MIN_INT, result.right);
    }
    return result;
  }
    
  
  def valueGEQ(value : Interval) : Interval = return new Interval(value.left, Math.MAX_INT);
  
  def valueLEQ(value : Interval) : Interval = new Interval(Math.MIN_INT, value.right);
}
