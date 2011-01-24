package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

/*package foo {

object FunctorType extends Enumeration {
  val or = Value("or");
  val concat = Value("concat");
}
object NodeIndexGenerator {
  var index : Int = -1;
  def getNextIndex() : Int = {
    index = index + 1;
    return index;
  }
}

class Node {
  var id : Int = NodeIndexGenerator.getNextIndex();
  def toString(alreadyDrawnNodes : List[Int]) : String = "";
}
class SimpleNode(c : Char) extends Node {
  var character : Char = c;
  var isBottom : Boolean = false;
  var isTop : Boolean = false;
  
  override def toString(alreadyDrawnNodes : List[Int]) : String = {
	if(this.isBottom)
		return "";
	else if(this.isTop)
		return "*";
	else
		return "Simple(" + character + ")";
  }
  
  def equals(right : SimpleNode) : Boolean = {
    if(this.isBottom && right.isBottom)
      return true;
    else if(this.isTop && right.isTop)
      return true;
    else if(this.isTop || right.isTop || this.isBottom || right.isBottom)
      return false;
    else if(this.character == right.character)
      return true;
    else
      return false;
  }
}
class FunctorNode(ftype : FunctorType.Value) extends Node {
  var functortype : FunctorType.Value = ftype;
  var nodeSons : List[Node] = Nil;
  
  override def toString(alreadyDrawnNodes : List[Int]) : String = {
    var res : String = "n_" + this.id + "=";
    if(functortype == FunctorType.or)
      res += "OR";
    else
      res += "Concat";
    
	res += "[ ";
	nodeSons.foreach(n => {
        if(alreadyDrawnNodes.contains(n.id))
           res += "n_"+ n.id; 
       else 
         res += n.toString(this.id :: alreadyDrawnNodes);
       res += ",";
    });
	res += " ]";
	return res;
  }
}

class StringGraphsDomain extends Lattice[StringGraphsDomain]
{
  var isBottom : Boolean = false;
  var isTop : Boolean = false;
  var root : Node = new Node();
  
  override def factory() : StringGraphsDomain = new StringGraphsDomain();
  
  def top() : StringGraphsDomain = {
    val result : StringGraphsDomain = this.factory();
    result.isTop = true;
    result.isBottom = false; 
    result
  }
  
  def bottom() : StringGraphsDomain = {
    val result : StringGraphsDomain = this.factory();
    result.isTop = false;
    result.isBottom = true; 
    result    
  }
  
  def lub(left : StringGraphsDomain, right : StringGraphsDomain) : StringGraphsDomain = {
	var result : StringGraphsDomain = this.factory();
  	result.root = new FunctorNode(FunctorType.or)
  	result.root.asInstanceOf[FunctorNode].nodeSons = List(left.root, right.root);
    return result.normalize();
  }
  
  def glb(left : StringGraphsDomain, right : StringGraphsDomain) : StringGraphsDomain = {
	//TODO
     return left;
  }
  
  def widening(left : StringGraphsDomain, right : StringGraphsDomain) : StringGraphsDomain = {
	//TODO
    return left;
  }
  
  def prnd(n : Node) : Set[Node] = {
    if(n.isInstanceOf[FunctorNode] && n.asInstanceOf[FunctorNode].functortype == FunctorType.or)
    {
    	var s : Set[Node] = Set.empty;
     	var nOr = n.asInstanceOf[FunctorNode];
     	var k = nOr.nodeSons.length;
     	for(i <- 0 to k-1)
     		s = s ++ prnd(nOr.nodeSons.apply(i));
     	return s;
    } else
      Set(n);
  }
  def same_label(n : Node, m : Node) : Boolean = {
    return true;
  }
  
  def lessEqualHelper(n : Node, m : Node, sc : Set[Pair[Node,Node]]) : Boolean = {
    //CASE 1
	if(sc.contains(Pair(n, m)))
		return true;
    //CASE 2
	else if(m.isInstanceOf[SimpleNode] && m.asInstanceOf[SimpleNode].isTop)
		return true;
    //CASE 3
	else if(n.isInstanceOf[FunctorNode] && m.isInstanceOf[FunctorNode] && 
           n.asInstanceOf[FunctorNode].functortype == FunctorType.concat && 
           m.asInstanceOf[FunctorNode].functortype == FunctorType.concat &&
           n.asInstanceOf[FunctorNode].nodeSons.length == m.asInstanceOf[FunctorNode].nodeSons.length && 
           n.asInstanceOf[FunctorNode].nodeSons.length > 0) {
		var nFunc = n.asInstanceOf[FunctorNode];
    	var mFunc = m.asInstanceOf[FunctorNode];
    	var nSons = nFunc.nodeSons.length;
    	var mSons = mFunc.nodeSons.length;
    	for(i <- 0 to nSons-1) {
		    if(!lessEqualHelper(nFunc.nodeSons.apply(i), mFunc.nodeSons.apply(i), sc + Pair(nFunc,mFunc)))
		    	return false;
		}
    	return true;
    //CASE 4
	} else if(n.isInstanceOf[FunctorNode] && m.isInstanceOf[FunctorNode] && 
             n.asInstanceOf[FunctorNode].functortype == FunctorType.or && 
             m.asInstanceOf[FunctorNode].functortype == FunctorType.or) {
		var nFunc = n.asInstanceOf[FunctorNode];
		var mFunc = m.asInstanceOf[FunctorNode];
		var k = nFunc.nodeSons.length;;
		for(i <- 0 to k-1) {
    	    if(!lessEqualHelper(nFunc.nodeSons.apply(i), mFunc, sc + Pair(nFunc,mFunc)))
    	    	return false;
		}
		return true;
	//CASE 5
	} else if(m.isInstanceOf[FunctorNode] && 
             m.asInstanceOf[FunctorNode].functortype == FunctorType.or && 
             prnd(m).exists(md => same_label(md,n))) {
		var mFunc = m.asInstanceOf[FunctorNode];
		var mD : Option[Node] = prnd(m).find(md => same_label(md,n));
		mD match {
		  case Some(x) => return lessEqualHelper(n, x, sc + Pair(n,m)); 
		  case None => return false;  
		}
	//CASE 6
	} else if(n.isInstanceOf[SimpleNode] && m.isInstanceOf[SimpleNode]) //oppure else return same_label(n,m);?
		return n.asInstanceOf[SimpleNode].equals(m.asInstanceOf[SimpleNode]);
	else
		return false;
  }
  
  def lessEqual(right : StringGraphsDomain) : Boolean = {
    return lessEqualHelper(this.root, right.root, Set.empty);
  }
  
  override def toString() : String = {
	if(this.isTop)
		return "_T_";
	else if(this.isBottom)
		return "_|_";
	else
		return root.toString(Nil);
  }
  
  def normalize() : StringGraphsDomain = {
    return this;
  }
}

class StringGraphs extends SimplifiedSemanticDomain[StringGraphs] with BoxedDomain[StringGraphsDomain, StringGraphs] 
{
   def factory() : StringGraphs = new StringGraphs();
   
   def setToTop(variable : Identifier) : StringGraphs = this.remove(variable);
   def assign(variable : Identifier, expr : Expression) : StringGraphs = this.add(variable, this.eval(expr));
   def assume(expr : Expression) : StringGraphs = this;
   def createVariable(variable : Identifier, typ : Type) : StringGraphs = this;
   def removeVariable(variable : Identifier) : StringGraphs = this.remove(variable);
 
   def get(variable : Identifier) = value.get(variable) match {
	    case Some(x) => x;
	    case None => new StringGraphsDomain().top();
   }
   override def getStringOfId(id : Identifier) : String = {
	     get(id).toString();
   }
   
   private def eval(expr : Expression) : StringGraphsDomain = expr match {
	   	case x : Identifier => this.get(x)
	    case x : Constant if x.constant.isInstanceOf[String] => {
	      var result = new StringGraphsDomain();
	      var charList = x.constant.asInstanceOf[String].toArray.toList;
	      if(charList.length > 1) { 
	    	  result.root = new FunctorNode(FunctorType.concat);
	    	  result.root.asInstanceOf[FunctorNode].nodeSons = charList.map(c => new SimpleNode(c));
	      } else if(charList.length == 1)
	    	  result.root = new SimpleNode(charList.apply(0));
	      else {
	    	  var simple = new SimpleNode('a');
	    	  simple.isBottom = true;
	    	  result.root = simple;
	      }
          return result.normalize();
	    }
	    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, returntyp) =>
	      if(parameters.size == 1)
	        parameters.elements.next match {
	        	case p1 :: Nil => 
		        	val left = this.eval(thisExpr);
		        	val right = this.eval(p1);
	        		val result = new StringGraphsDomain();
	        		result.root = new FunctorNode(FunctorType.concat);
	        		result.root.asInstanceOf[FunctorNode].nodeSons = List(left.root, right.root);
		        	return result.normalize();
	        	case _ => return new StringGraphsDomain().top();
	      	}
	      else return new StringGraphsDomain().top();	
	    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringSubstring, returntyp) =>
	      if(parameters.size!=1) return new StringGraphsDomain().top();
	      val l : List[Expression] = parameters.elements.next();
	      if(l.size != 2) return new StringGraphsDomain().top();
	      l.apply(0) match {
    	    case Constant(s1, _) =>
		      l.apply(1) match {
		    	    case Constant(s2, _) =>
		    	    	  val beginIndex = Integer.decode(s1).intValue();
	    	    	  	  val endIndex = Integer.decode(s2).intValue();
					      val left = this.eval(thisExpr);
					      //TODO
				    	  return new StringGraphsDomain().top();
		    	    case _ => return new StringGraphsDomain().top();
		      }
    	    case _ => return new StringGraphsDomain().top();
	    }
	    case _ => return new StringGraphsDomain().top();
   }
}
}*/