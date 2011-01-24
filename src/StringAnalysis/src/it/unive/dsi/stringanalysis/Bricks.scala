package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

class Brick(protected var m : Int, protected var M : Int, protected var s : Set[String])
{
  var strings : Set[String] = s;
  var min : Int = m;
  var max : Int = M;
  var isTop = false;
  var isBottom = false;
  var ks = 3;
  var ki = 3;
  
  def lub(left : Brick, right : Brick) : Brick =
  {
    val newMin = left.min.min(right.min);
    val newMax = left.max.max(right.max);
    val newStrings = left.strings.toList.union(right.strings.toList);
    var newStringsSet = Set[String]();
    for(s <- newStrings)
      newStringsSet += s;
    
    return new Brick(newMin, newMax, newStringsSet);
  }
  
  def glb(left : Brick, right : Brick) : Brick =
  {
    val newMin = left.min.max(right.min);
    val newMax = left.max.min(right.max);
    val newStrings = left.strings.intersect(right.strings);
    return new Brick(newMin, newMax, newStrings);
  }
  
  def widening(left : Brick, right : Brick) : Brick = {
    if(left.isTop || right.isTop || (left.strings ++ right.strings).size > ks)
      return top();
    val m = left.min.min(right.min);
    val M = left.max.max(right.max);
    if(M - m > ki)
      return new Brick(0, Int.MaxValue, left.strings ++ right.strings);
    else
      return new Brick(m, M, left.strings ++ right.strings);
  }

  def top() : Brick = {
    var res = new Brick(0,0,Set.empty); //this brick should correspond to empty string, 
    //but we override its meaning with the label isTop set to true
    res.isTop = true;
    return res;
  }
  
  def lessEqual(right : Brick) : Boolean =
  {
    if(this.min >= right.min && this.max <= right.max && this.strings.subsetOf(right.strings))
      return true;
    return false;
  }  
  
  override def toString() : String = {
    if(this.isBottom)
    	return "_|_";
    if(this.isTop)
    	return "_T_";
    else
    	return "[{" + this.strings.toString() + "}, " + this.min + ", " + this.max + "]";
  }
}



class BricksDomain extends Lattice[BricksDomain]
{
  var isBottom : Boolean = false;
  var isTop : Boolean = false;
  var bricksList : List[Brick] = Nil;
  val kl = 3;
  
  override def factory() : BricksDomain = new BricksDomain();
  
  def top() : BricksDomain = {
    val result : BricksDomain = this.factory();
    result.isTop = true;
    result.isBottom = false; 
    result
  }
  
  def bottom() : BricksDomain = { 
	  val result : BricksDomain = this.factory();
	  result.isBottom = true;
      result.isTop = false;
      result.bricksList = Nil;
	  result
  }
  
  def lub(left : BricksDomain, right : BricksDomain) : BricksDomain = {
     if(left.isTop || right.isTop)
    	 return top();
    
     var newLeft : BricksDomain = this.factory();
     var newRight : BricksDomain = this.factory();
  	 newLeft.bricksList = left.bricksList;
  	 newRight.bricksList = right.bricksList;
     
     if(left.bricksList.length < right.bricksList.length) {
        for(i <- 0 to (right.bricksList.length - left.bricksList.length - 1))
        	newLeft.bricksList = newLeft.bricksList ::: List(new Brick(0,0,Set.empty));
     }
     if(right.bricksList.length < left.bricksList.length) {
        for(i <- 0 to (left.bricksList.length - right.bricksList.length - 1))
        	newRight.bricksList = newRight.bricksList ::: List(new Brick(0,0,Set.empty));
     }
     
     var result : BricksDomain = this.factory();
     result.bricksList = Nil;
     var lengthLeft = newLeft.bricksList.length;
     for(i <- 0 to lengthLeft - 1)
       {
         val newBrick = newLeft.bricksList(lengthLeft - 1 - i).lub(
           newLeft.bricksList(lengthLeft - 1 - i), newRight.bricksList(lengthLeft - 1 - i));
         result.bricksList = newBrick :: result.bricksList; 
       }
     
	 return result;
  }
  
  def glb(left : BricksDomain, right : BricksDomain) : BricksDomain = {
     if(left.isBottom || right.isBottom)
    	 return bottom();

     var newLeft : BricksDomain = this.factory();
     var newRight : BricksDomain = this.factory();
  	 newLeft.bricksList = left.bricksList;
  	 newRight.bricksList = right.bricksList;
     
     if(left.bricksList.length < right.bricksList.length) {
        for(i <- 0 to (right.bricksList.length - left.bricksList.length - 1))
        	newLeft.bricksList = newLeft.bricksList ::: List(new Brick(0,0,Set.empty));
     }
     if(right.bricksList.length < left.bricksList.length) {
        for(i <- 0 to (left.bricksList.length - right.bricksList.length - 1))
        	newRight.bricksList = newRight.bricksList ::: List(new Brick(0,0,Set.empty));
     }
     
     var result : BricksDomain = this.factory();
     //result.bricksList = Nil;
     var lengthLeft = newLeft.bricksList.length;
     for(i <- 0 to lengthLeft - 1)
       {
         val newBrick = newLeft.bricksList(lengthLeft - 1 - i).glb(
           newLeft.bricksList(lengthLeft - 1 - i), newRight.bricksList(lengthLeft - 1 - i));
         if(newBrick.min > newBrick.max)
           return bottom();
         result.bricksList = newBrick :: result.bricksList; 
       }
     
	 return result;
  }
  
  def widening(left : BricksDomain, right : BricksDomain) : BricksDomain = {
     if(!left.lessEqual(right) && !right.lessEqual(left))
        return top();
     if(left.bricksList.length > kl || right.bricksList.length > kl)
        return top();
      
     var newLeft : BricksDomain = this.factory();
	 var newRight : BricksDomain = this.factory();
	 newLeft.bricksList = left.bricksList;
	 newRight.bricksList = right.bricksList;
	 
	 if(left.bricksList.length < right.bricksList.length) {
	    for(i <- 0 to (right.bricksList.length - left.bricksList.length - 1))
	    	newLeft.bricksList = newLeft.bricksList ::: List(new Brick(0,0,Set.empty));
	 }
	 if(right.bricksList.length < left.bricksList.length) {
	    for(i <- 0 to (left.bricksList.length - right.bricksList.length - 1))
	    	newRight.bricksList = newRight.bricksList ::: List(new Brick(0,0,Set.empty));
	 }

     var result : BricksDomain = this.factory();
     var lengthLeft = newLeft.bricksList.length;
     for(i <- 0 to lengthLeft - 1)
       {
         val newBrick = newLeft.bricksList.apply(lengthLeft - 1 - i).widening(
           newLeft.bricksList.apply(lengthLeft - 1 - i), newRight.bricksList.apply(lengthLeft - 1 - i));
         result.bricksList = newBrick :: result.bricksList; 
       }
     
	 return result;
  }
  
  def lessEqual(right : BricksDomain) : Boolean = {
	 if(this.isBottom || right.isTop)
		  return true;
   
  	 var newLeftList = this.bricksList;
  	 var newRightList = right.bricksList;
     
     if(this.bricksList.length < right.bricksList.length) {
        for(i <- 0 to (right.bricksList.length - this.bricksList.length - 1))
        	newLeftList = newLeftList ::: List(new Brick(0,0,Set.empty));
     }
     if(right.bricksList.length < this.bricksList.length) {
        for(i <- 0 to (this.bricksList.length - right.bricksList.length - 1))
        	newRightList = newRightList ::: List(new Brick(0,0,Set.empty));
     }
     for(i <- 0 to (newLeftList.length-1))
       if(!newLeftList(i).lessEqual(newRightList(i)))
          return false;
          
      return true;
  }
  
  override def toString() : String = {
    if(this.isTop)
      return "_T_";
    else if(this.isBottom || this.bricksList.length == 0) 
      return "_|_"; 

    else {
      var returnVal = "";
      for(b <- this.bricksList)
    	  returnVal += b.toString(); //"[{" + b.strings.toString() + "}, " + b.min + ", " + b.max + "]";
      return returnVal;
    }
  }
  
  def isNormalized() : Boolean = {
	//RULE 1
    if(this.bricksList.exists(b => b.min == 0 && b.max == 0 && b.strings == Set.empty))
		return false;
    
    //RULE 3
    if(this.bricksList.exists(b => b.min > 1 && b.max == b.min))
      return false;
    
    //RULE 5
    if(this.bricksList.exists(b => b.min >= 1 && b.max > b.min))
      return false;
	
    var i = 0;
    for(i <- 0 to this.bricksList.length - 2)
    {
      var first = this.bricksList.apply(i);
      var second = this.bricksList.apply(i+1);
      
      //RULE 2
      if(first.min == 1 && first.max == 1 && second.min == 1 && second.max == 1)
        return false;
      
      //RULE 4
	  if(first.strings.subsetOf(second.strings) && second.strings.subsetOf(first.strings))
		  return false;
    }
    
    return true;
  }
  
  def stringConcatenation(left : Set[String], right : Set[String]) : Set[String] = {
    var newSet : Set[String] = Set.empty;
    left.foreach(s1 => right.foreach(s2 => newSet = newSet + s1.concat(s2)));
    return newSet;
  }
  def stringConcatenation(set : Set[String], times : Int) : Set[String] = {
    var left : Set[String] = set;
    var right : Set[String] = set;
    var temp : Set[String] = Set.empty;
    for(i <- 0 to times-2) {
    	left.foreach(s1 => right.foreach(s2 => temp = temp + s1.concat(s2)));
    	left = temp;
    	temp = Set.empty;
    }
    return left;
  }
  
  def normalize() : BricksDomain = {
    while(!this.isNormalized()) {
    	//RULE 1
    	this.bricksList = this.bricksList.remove(b => b.min == 0 && b.max == 0 && b.strings == Set.empty);
     
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


class Bricks extends SimplifiedSemanticDomain[Bricks] with BoxedDomain[BricksDomain, Bricks] 
{
   def factory() : Bricks = new Bricks();
   
   def setToTop(variable : Identifier) : Bricks = this.remove(variable);
   def assign(variable : Identifier, expr : Expression) : Bricks = this.add(variable, this.eval(expr));
   def assume(expr : Expression) : Bricks = this;
   def createVariable(variable : Identifier, typ : Type) : Bricks = this;
   def removeVariable(variable : Identifier) : Bricks = this.remove(variable);
 
   def get(variable : Identifier) = value.get(variable) match {
	    case Some(x) => x;
	    case None => new BricksDomain().top();
   }
   override def getStringOfId(id : Identifier) : String = {
	     get(id).toString();
   }
   
   private def eval(expr : Expression) : BricksDomain = expr match {
	   	case x : Identifier => this.get(x)
	    case x : Constant if x.constant.isInstanceOf[String] => {
	      var result = new BricksDomain(); 
	      result.bricksList = List(new Brick(1, 1, Set(x.constant.asInstanceOf[String])));
	      return result;
	    }
	    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, returntyp) =>
	      if(parameters.size == 1)
	        parameters.elements.next match {
	        	case p1 :: Nil => 
		        	val left = this.eval(thisExpr);
		        	val right = this.eval(p1);
	        		val result = new BricksDomain();
	        		result.bricksList = (left.bricksList ++ right.bricksList).toList;
		        	return result.normalize();
	        	case _ => return new BricksDomain().top();
	      	}
	      else return new BricksDomain().top();	
	    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringSubstring, returntyp) =>
	      if(parameters.size!=1) return new BricksDomain().top();
	      val l : List[Expression] = parameters.elements.next();
	      if(l.size != 2) return new BricksDomain().top();
	      l.apply(0) match {
    	    case Constant(s1, _) =>
		      l.apply(1) match {
		    	    case Constant(s2, _) =>
		    	    	  val beginIndex = Integer.decode(s1).intValue();
	    	    	  	  val endIndex = Integer.decode(s2).intValue();
					      val left = this.eval(thisExpr);
					      if(left.bricksList.length >= 1 && left.bricksList.apply(0).min == 1 && left.bricksList.apply(0).max == 1) {
					    	  val firstBrick = left.bricksList.apply(0);
					    	  if(firstBrick.strings.forall(s => s.length() >= endIndex)) {
					    	    val result = new BricksDomain();
				    	    	var substrings : Set[String] = Set.empty; 
					    	    firstBrick.strings.foreach(s => substrings = substrings + s.substring(beginIndex,endIndex));
				    	    	result.bricksList = List(new Brick(1, 1, substrings));
					    	    return result;
					    	  } else
					    		  return new BricksDomain().top();
					      } else
					    	  return new BricksDomain().top();
		    	    case _ => return new BricksDomain().top();
		      }
    	    case _ => return new BricksDomain().top();
	    }
	    case _ => return new BricksDomain().top();
   }
}