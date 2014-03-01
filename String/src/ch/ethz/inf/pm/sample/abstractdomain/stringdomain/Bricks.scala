package ch.ethz.inf.pm.sample.abstractdomain.stringdomain

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class Brick(protected var m : Int, protected var M : Int, protected var s : Set[String]) extends Lattice[Brick]
{
  var strings : Set[String] = s
  var min : Int = m
  var max : Int = M
  var isTop = false
  var isBottom = false
  var ks = 3
  var ki = 3

  def factory() : Brick = new Brick(m,M,s)
  
  def lub(other : Brick) : Brick =
  {
	if(this.isTop || other.isTop)
		return new Brick(0,0,Set.empty).top()
	if(this.isBottom)
		return other.factory()
	if(other.isBottom)
		return factory()
	
    val newMin = this.min.min(other.min)
    val newMax = this.max.max(other.max)
    val newStrings = this.strings.toList.union(other.strings.toList)
    var newStringsSet = Set[String]()
    for(s <- newStrings)
      newStringsSet += s
    
    new Brick(newMin, newMax, newStringsSet)
  }
  
  def glb(other : Brick) : Brick =
  {
    val newMin = this.min.max(other.min)
    val newMax = this.max.min(other.max)
    val newStrings = this.strings.intersect(other.strings)
    new Brick(newMin, newMax, newStrings)
  }
  
  def widening(other : Brick) : Brick = {
    if(this.isTop || other.isTop || (this.strings ++ other.strings).size > ks)
      return top()
    val m = this.min.min(other.min)
    val M = this.max.max(other.max)
    if(M - m > ki)
      new Brick(0, Int.MaxValue, this.strings ++ other.strings)
    else
      new Brick(m, M, this.strings ++ other.strings)
  }

  def top() : Brick = {
    val res = new Brick(0,0,Set.empty); //this brick should correspond to empty string,
    //but we override its meaning with the label isTop set to true
    res.isTop = true
    res
  }

  def bottom() : Brick ={
    val res = new Brick(0,0,Set.empty); //this brick should correspond to empty string,
    //but we override its meaning with the label isBottom set to true
    res.isBottom = true
    res
  }
  
  def lessEqual(right : Brick) : Boolean =
  {
    if(this.min >= right.min && this.max <= right.max && this.strings.subsetOf(right.strings))
      true
    else
      false
  }  
  
  override def toString : String = {
    if(this.isBottom)
    	"⊥"
    else if(this.isTop)
    	"⊤"
    else
    	"[{" + this.strings.toString + "}, " + this.min + ", " + this.max + "]"
  }
}



class BricksDomain extends StringValueDomain[BricksDomain]
{
  var isBottom : Boolean = false
  var isTop : Boolean = false
  var bricksList : List[Brick] = Nil
  val kl = 20
  
  override def factory() : BricksDomain = new BricksDomain()

  def emptyBrick = new Brick(0,0,Set.empty)
  
  def top() : BricksDomain = {
    val result : BricksDomain = factory()
    result.isTop = true
    result.isBottom = false
    result.bricksList = List(new Brick(0,0,Set.empty).top())
    result
  }
  
  def bottom() : BricksDomain = { 
	  val result : BricksDomain = factory()
	  result.isBottom = true
    result.isTop = false
    result.bricksList = Nil
	  result
  }

  /**
   * Takes two BricksDomains and appends the empty brick to the shorter list of bricks until
   * both domains have a brick list of equal length
   */
  private def appendEmptyBricks(left : BricksDomain, right : BricksDomain) : Unit = {
    if(left.bricksList.length < right.bricksList.length)
      for(i <- 0 to (right.bricksList.length - left.bricksList.length - 1))
        left.bricksList = left.bricksList :+ emptyBrick
    else if(right.bricksList.length < left.bricksList.length)
      for(i <- 0 to (left.bricksList.length - right.bricksList.length - 1))
        right.bricksList = right.bricksList :+ emptyBrick
  }
  
  def lub(other : BricksDomain) : BricksDomain = {
     if(this.isTop || other.isTop)
    	 return top()
     
     val newLeft : BricksDomain = this.factory()
     val newRight : BricksDomain = this.factory()
  	 newLeft.bricksList = Nil ::: this.bricksList
  	 newRight.bricksList = Nil ::: other.bricksList
     
     appendEmptyBricks(newLeft, newRight)
     
     val result : BricksDomain = factory()
     result.bricksList = Nil
     val lengthLeft = newLeft.bricksList.length
     for(i <- 0 to lengthLeft - 1)
     {
       val newBrick = newLeft.bricksList(lengthLeft - 1 - i).lub(newRight.bricksList(lengthLeft - 1 - i))
       result.bricksList = newBrick :: result.bricksList
     }
	  result
  }
  
  def glb(other : BricksDomain) : BricksDomain = {
   if(this.isBottom || other.isBottom)
     return bottom()

   val newLeft : BricksDomain = factory()
   val newRight : BricksDomain = factory()
   newLeft.bricksList = Nil ::: this.bricksList
   newRight.bricksList = Nil ::: other.bricksList

   appendEmptyBricks(newLeft, newRight)

   val result : BricksDomain = factory()
   val lengthLeft = newLeft.bricksList.length
   for(i <- 0 to lengthLeft - 1)
   {
     val newBrick = newLeft.bricksList(lengthLeft - 1 - i).glb(newRight.bricksList(lengthLeft - 1 - i))
     if(newBrick.min > newBrick.max)
       return bottom()
     result.bricksList = newBrick :: result.bricksList
   }
     
	 result
  }

  /**
   * From the paper <b>Static Analysis of String Values</b>:
   *
   * The widening operator (BR × BR) -> BR is defined as follows:
   *
   * widening(L1,L2) = Top, if (not (L1 ≤ L2) ∧ not (L2 ≤ L1)) ∨ (∃i ∈ {1,2} : len(Li) > kl),
   * widening(L1,L2) = w(L1,L2), otherwise
   *
   * where w(L1,L2) = [B1new(L1[1],L2[1]);B2new(L1[2],L2[2]);...;Bnnew(L1[n],L2[n])] with n
   * being the size of the bigger list and Binew(L1[i],L2[i]) is defined by:
   *
   * Binew([S1i]^(m1i,M1i),[S2i]^(m2i,M2i)) = ⊤, if |S1i ∪ S2i| > kS
   * Binew([S1i]^(m1i,M1i),[S2i]^(m2i,M2i)) = [S1i ∪ S2i]^(0,∞), if (M-m) > kI
   * Binew([S1i]^(m1i,M1i),[S2i]^(m2i,M2i)) = [S1i ∪ S2i]^(m,M), otherwise
   *
   * where m = min(m1i,m2i) and M = max(M1i,M2i)
   *
   * @param other The new value
   * @return The widening of <code>left</code> and <code>right</code>
   */

  def widening(other : BricksDomain) : BricksDomain = {

	 if((!this.lessEqual(other) && !other.lessEqual(this)) ||
     this.bricksList.length > kl || other.bricksList.length > kl ||
     this.isTop || other.isTop)
        return top()

   val newLeft : BricksDomain = factory()
	 val newRight : BricksDomain = factory()
	 newLeft.bricksList = Nil ::: this.bricksList
	 newRight.bricksList = Nil ::: other.bricksList
	 
	 appendEmptyBricks(newLeft, newRight)

   val result : BricksDomain = factory()
   val n = newLeft.bricksList.length
   for(i <- 0 to n - 1)
   {
     val newBrick = newLeft.bricksList.apply(n - 1 - i).widening(newRight.bricksList.apply(n - 1 - i))
     result.bricksList = newBrick :: result.bricksList
   }
	 result
  }

  /**
   * Compute whether this BricksDomain is partially ordered below the domain in the argument.
   * According to the paper <b>Static Analysis of String Values</b> this is the case if either:
   *  - <code>this</code> is bottom
   *  - <code>right</code>
   *  - ∀i <code>this(i)</code> is partially ordered below <code>right(i)</code>
   *
   * @param right The BricksDomain to which we want to evaluate the partial order
   * @return true iff <code>this</code> is less or equal than <code>r</code>
   */
  def lessEqual(right : BricksDomain) : Boolean = {
	 if(this.isBottom || right.isTop)
		  return true

   def compute(x : List[Brick], y : List[Brick]) : Boolean =
     (x,y) match {
       case (Nil,Nil) => true
       case (Nil,yy::ys) => emptyBrick.lessEqual(yy) && compute(Nil,ys)
       case (xx::xs,Nil) => xx.lessEqual(emptyBrick) && compute(xs,Nil)
       case (xx::xs,yy::ys) => xx.lessEqual(yy) && compute(xs,ys)
     }

   compute(this.bricksList,right.bricksList)
  }
  
  override def toString : String = {
    if(this.isTop)
      "⊤"
    else if(this.isBottom || this.bricksList.length == 0) 
      "⊥"
    else {
      var returnVal = ""
      for(b <- this.bricksList)
    	  returnVal += b.toString; //"[{" + b.strings.toString + "}, " + b.min + ", " + b.max + "]";
      returnVal
    }
  }
  
  def isNormalized : Boolean = {
	//RULE 1
    if(this.bricksList.exists(b => !b.isTop &&
    		b.min == 0 && b.max == 0 && b.strings == Set.empty))
		return false
    
    //RULE 3
    if(this.bricksList.exists(b => b.min > 1 && b.max == b.min))
      return false
    
    //RULE 5
    if(this.bricksList.exists(b => b.min >= 1 && b.max > b.min))
      return false

    for(i <- 0 to this.bricksList.length - 2)
    {
      val first = this.bricksList.apply(i)
      val second = this.bricksList.apply(i+1)
      
      //RULE 2
      if(first.min == 1 && first.max == 1 && second.min == 1 && second.max == 1)
        return false
      
      //RULE 4
	  if(first.strings.subsetOf(second.strings) && second.strings.subsetOf(first.strings))
		  return false
    }
    
    true
  }

  def concat(right : BricksDomain) : BricksDomain = {
    val result = factory()
    if(right.isTop && this.isTop )
      return result.top()
    result.bricksList = (this.bricksList ++ right.bricksList).toList
    val normalizedResult = result.normalize()
    normalizedResult
  }
  
  def stringConcatenation(left : Set[String], other : Set[String]) : Set[String] = {
    var newSet : Set[String] = Set.empty;
    left.foreach(s1 => other.foreach(s2 => newSet = newSet + s1.concat(s2)));
    return newSet;
  }
  def stringConcatenation(set : Set[String], times : Int) : Set[String] = {
    var left : Set[String] = set;
    var other : Set[String] = set;
    var temp : Set[String] = Set.empty;
    for(i <- 0 to times-2) {
    	left.foreach(s1 => other.foreach(s2 => temp = temp + s1.concat(s2)));
    	left = temp;
    	temp = Set.empty;
    }
    return left;
  }
  
  def normalize() : BricksDomain = {
    while(!this.isNormalized) {
    	
    	//RULE 1
    	this.bricksList = this.bricksList.filter(b => !(b.isTop == false &&
    			b.min == 0 && b.max == 0 && b.strings == Set.empty))
    	if(this.bricksList.length == 0) {
    		this.isBottom = true;
    		return this;
		}
    	
    	//RULE 3
    	this.bricksList = this.bricksList.map(b => {
    			if(b.min > 1 && b.max == b.min)
    				new Brick(1,1, stringConcatenation(b.strings, b.min));
    			else
    				b;
           });
     
    	var newBricksList : List[Brick] = Nil;
    	var length = this.bricksList.length;
    	var i = 0;
    	var skipNext = false;
	    for(i <- 0 to length - 2)
	    {
	      if(!skipNext) {
		      var first = this.bricksList.apply(length - 1 - i);
		      var second = this.bricksList.apply(length - 2 - i);
		     
		      if(first.min == 1 && first.max == 1 && second.min == 1 && second.max == 1) {
	        	  //RULE 2
		    	  newBricksList = new Brick(1,1, stringConcatenation(second.strings, first.strings)) :: newBricksList;
		    	  skipNext = true;
		      } else if(first.strings.subsetOf(second.strings) && second.strings.subsetOf(first.strings)) {
		          //RULE 4
		    	  newBricksList = new Brick(first.min + second.min, first.max + second.max, first.strings) :: newBricksList;
		    	  skipNext = true;		        
		      } else
      			  newBricksList = first :: newBricksList;
	      } else
	    	  skipNext = false;
	    }
	    if(!skipNext)
	    	newBricksList = this.bricksList.apply(0) :: newBricksList;
     
	    this.bricksList = newBricksList;
     
    	//RULE 5
	    length = this.bricksList.length;
	    newBricksList = Nil;
	    for(i <- 0 to length - 1)
	    {
	      var b = this.bricksList.apply(length - 1 - i);
	      if(b.min >= 1 && b.max > b.min) {
	    	  newBricksList = new Brick(1, 1, stringConcatenation(b.strings, b.min)) :: 
	    		  	(new Brick(0, b.max - b.min, b.strings) :: newBricksList);
	      } else
	    	  newBricksList = b :: newBricksList;
	    }
     
	    this.bricksList = newBricksList;
    }
    return this;
  }
}


class Bricks (dom:BricksDomain, val map:Map[Identifier, BricksDomain] = Map.empty[Identifier, BricksDomain], override val isBottom:Boolean = false, val isTop:Boolean = false)
  extends BoxedDomain[BricksDomain, Bricks] with StringDomain[BricksDomain, Bricks]
{
   def this() = this(new BricksDomain().top())
   def functionalFactory(_value:Map[Identifier, BricksDomain] = Map.empty[Identifier, BricksDomain],_isBottom:Boolean = false,_isTop:Boolean = false) : Bricks =
    new Bricks(dom,_value,_isBottom,_isTop)

   def setToTop(variable : Identifier) : Bricks = this.remove(variable)

   def assign(variable : Identifier, expr : Expression) : Bricks =
     if(variable.typ.isStringType) this.add(variable, this.eval(expr))
     else this

   def assume(expr : Expression) : Bricks = {
     if(isBottom || !expr.typ.isStringType)return this
     // Check if we assume something about non-numerical values - if so, return
     val ids = Normalizer.getIdsForExpression(expr)
     for (id <- ids) {
       if (!id.typ.isStringType) {
         return this
       }
     }

     expr match {
       // Comparison
       case BinaryArithmeticExpression(thisExpr,thatExpr,ArithmeticOperator.==,_) =>
         val left = eval(thisExpr)
         val right = eval(thatExpr)
         val glb = left.glb(right)
         if(glb.isBottom)bottom()
         else
           (thisExpr, thatExpr) match {
             case(x: Identifier, y: Identifier) =>
               this.add(x, glb).add(y, glb)
             case(x: Identifier, _) =>
               this.add(x, glb)
             case(_, y: Identifier) =>
               this.add(y, glb)
             case _ => this
           }

       // negated comparison
       case NegatedBooleanExpression(BinaryArithmeticExpression(thisExpr, thatExpr, ArithmeticOperator.==, _)) =>
         val left = eval(thisExpr)
         val right = eval(thatExpr)
         (left, right) match {
           case _ => this
         }

       // double negation
       case NegatedBooleanExpression(NegatedBooleanExpression(thisExpr)) =>
         assume(thisExpr)

       case _ => this
     }
   }
   def createVariable(variable : Identifier, typ : Type) : Bricks = this
   def removeVariable(variable : Identifier) : Bricks = this.remove(variable)
 
   def get(variable : Identifier) = map.get(variable) match {
	    case Some(x) => x
	    case None => dom.top()
   }
   override def getStringOfId(id : Identifier) : String = {
	     get(id).toString
   }
   
   private def eval(expr : Expression) : BricksDomain = expr match {
	   	case x : Identifier => this.get(x)
	    case Constant(x,_,_) =>
	      val result = new BricksDomain()
	      result.bricksList = List(new Brick(1, 1, Set(x)))
	      result
	    case AbstractOperator(thisExpr, parameters, _, AbstractOperatorIdentifiers.stringConcatenation, _) =>
	        parameters match {
	        	case p1 :: Nil => 
		        	val left = this.eval(thisExpr)
		        	val right = this.eval(p1)
              left.concat(right)
	        	case _ => dom.top()
          }
	    case AbstractOperator(thisExpr, parameters, _, AbstractOperatorIdentifiers.stringSubstring, _) =>
	      val l : List[Expression] = parameters
	      if(l.size != 2) return new BricksDomain().top();
	      l.apply(0) match {
    	    case Constant(s1, _, _) =>
		      l.apply(1) match {
		    	    case Constant(s2, _, _) =>
		    	    	  val beginIndex = Integer.decode(s1).intValue()
	    	    	  	  val endIndex = Integer.decode(s2).intValue()
					      val left = this.eval(thisExpr)
					      if(left.bricksList.length >= 1 && left.bricksList.apply(0).min == 1 && left.bricksList.apply(0).max == 1) {
					    	  val firstBrick = left.bricksList.apply(0)
					    	  if(firstBrick.strings.forall(s => s.length() >= endIndex)) {
					    	    val result = new BricksDomain()
				    	    	var substrings : Set[String] = Set.empty
					    	    firstBrick.strings.foreach(s => substrings = substrings + s.substring(beginIndex,endIndex));
				    	    	result.bricksList = List(new Brick(1, 1, substrings));
					    	    result
					    	  } else
                    dom.top()
					      } else
                  dom.top()
		    	    case _ => dom.top()
		      }
    	    case _ => dom.top()
	    }
	    case _ => dom.top()
   }
}