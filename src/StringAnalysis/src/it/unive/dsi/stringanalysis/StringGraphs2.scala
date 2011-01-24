package it.unive.dsi.stringanalysis

import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.oorepresentation._

object NodeType extends Enumeration {
  val or = Value("or");
  val concat = Value("concat");
  val simple = Value("simple");
  val top = Value("top");
  val bottom = Value("bottom");
}
object NodeIndexGenerator {
  var index : Int = -1;
  def getNextIndex() : Int = {
    index = index + 1;
    return index;
  }
}

class Node(ntype : NodeType.Value)
{
  var id : String = "n_" + NodeIndexGenerator.getNextIndex();
  var nodeType : NodeType.Value = ntype;
  var character : Char = 'a';
  var sons : List[String] = Nil;
  
  def label() : String = {
    nodeType match {
      case NodeType.or => return "or";
      case NodeType.concat => return "concat";
      case NodeType.bottom => return "bottom";
      case NodeType.top => return "top";
      case NodeType.simple => return character.toString();
    }
  }
  
  /*def tostring() : String = {
	  return "";
  }*/
  
  def equals(otherNode : Node) : Boolean = {
	nodeType match {
      case NodeType.or => return otherNode.nodeType == NodeType.or;
      case NodeType.concat => return otherNode.nodeType == NodeType.concat;
      case NodeType.bottom => return otherNode.nodeType == NodeType.bottom;
      case NodeType.top => return otherNode.nodeType == NodeType.top;
      case NodeType.simple => return otherNode.nodeType == NodeType.simple && otherNode.character == this.character;
    }
  }
  
  override def clone() : Node = {
	  var newNode = new Node(this.nodeType);
	  newNode.character = this.character;
	  for(s <- this.sons) {
	    newNode.sons = newNode.sons ::: List(s);  
	  }
	  //newNode.sons = this.sons;
	  return newNode;
  }
  
  override def toString() : String = {
    return "(" + id + ", " + label + ")";
  }
}

class StringGraph(rId : String, isEmptyStringGraph : Boolean)
{
  var nodes : Map[String,Node] = Map.empty;
  var rootID : String = rId;
  var forwardArcs : Set[Pair[String,String]] = Set.empty;
  var backwardArcs : Set[Pair[String,String]] = Set.empty;
  var isEmpty = isEmptyStringGraph || (rId=="");
  
  override def clone() : StringGraph = {
	  var newStringGraph = new StringGraph(this.rootID,this.isEmpty);
      newStringGraph.nodes = this.nodes;
      newStringGraph.forwardArcs = this.forwardArcs;
      newStringGraph.backwardArcs = this.backwardArcs;
      return newStringGraph;
  }
  
  //removes the specified node, all its arcs and repeats the procedure for its sons
  def removeNodeRecursive(node : Node) : StringGraph = {
	  var nodeSons = sons(node);
	  for(n <- nodeSons) {
	    if(!this.backwardArcs.exists(a => a._1 == node.id && a._2 == n.id))
	    	this.removeNodeRecursive(n);
	  }
	  this.forwardArcs = this.forwardArcs.filter(fa => fa._1 != node.id && fa._2 != node.id);
	  this.backwardArcs = this.backwardArcs.filter(fa => fa._1 != node.id && fa._2 != node.id);
	  this.nodes = this.nodes - node.id;
	  return this;
  }
  
  def normalizeHelper() : StringGraph = {
	  if(this.isNormalized())
		  return this;

	  //RULE 1
	  if(nodes.values.exists(n => n.nodeType == NodeType.bottom && n.id != rootID)) {
		  //Console.println("applied rule 1");
		  var res = new StringGraph("", true);
		  var bottom = new Node(NodeType.bottom);
		  res.rootID = bottom.id;
		  res.addNode(bottom);
		  return res;
	  }
   
   	  //RULE 2, part 1
	  var orNodes = nodes.values.filter(n => n.nodeType == NodeType.or).toList;
   	  var invalidOrNodes = orNodes.filter(n => outdegree(n) < 2);
      if(invalidOrNodes.length > 0) {
	      var newStringGraph = this.clone();
	      var n = invalidOrNodes.apply(0);
    	  
	      if(outdegree(n) == 1) {
    	    var onlySon = newStringGraph.sons(n).apply(0);
    	    if(newStringGraph.backwardArcs.contains(Pair(n.id,onlySon.id))) {
    	    	var nParent = newStringGraph.forwardArcs.find(a => a._2 == n.id);
    	    	nParent match {
    	    	  case Some((parent,child)) => {
  	    	    	newStringGraph.backwardArcs = newStringGraph.backwardArcs - Pair(n.id,onlySon.id);
  	    	    	newStringGraph.backwardArcs = newStringGraph.backwardArcs + Pair(parent, onlySon.id);
  	    	    	newStringGraph.forwardArcs = newStringGraph.forwardArcs - Pair(parent,child); 
  	    	    	if(newStringGraph.nodes(parent).nodeType == NodeType.concat) {
  	    	    		newStringGraph.nodes(parent).sons = newStringGraph.nodes(parent).sons.map(s => if(s == n.id) onlySon.id else s); 
  	    	    	}
    	    	  };
    	    	  case _ => ;
    	    	}
    	    } else {
	    	    newStringGraph.forwardArcs = newStringGraph.forwardArcs.map(a => {
	    	    	if(a._2 == n.id) {
	    	    	    if(newStringGraph.nodes(a._1).nodeType == NodeType.concat) {
	    	    	    	newStringGraph.nodes(a._1).sons = newStringGraph.nodes(a._1).sons.map(s => if(s == n.id) onlySon.id else s); 
  	    	    		}
    	    			(a._1, onlySon.id);
	    	    	} else
	    				a
	    	    });
	    	    if(newStringGraph.forwardArcs.contains(Pair(n.id,onlySon.id)))
	    	    	newStringGraph.forwardArcs = newStringGraph.forwardArcs - Pair(n.id,onlySon.id);
	    	    newStringGraph.backwardArcs = newStringGraph.backwardArcs.map(a => {
	    	    	if(a._2 == n.id) {
	    	    	    if(newStringGraph.nodes(a._1).nodeType == NodeType.concat) {
	    	    	    	newStringGraph.nodes(a._1).sons = newStringGraph.nodes(a._1).sons.map(s => if(s == n.id) onlySon.id else s); 
  	    	    		}
    	    			(a._1, onlySon.id);
	    	    	} else
	    				a
	    	    });
    	    }
    	    if(newStringGraph.rootID == n.id)
    	    	newStringGraph.rootID = onlySon.id;
    	    newStringGraph.nodes = newStringGraph.nodes - n.id; 
    	  } else { //outdegree(n) = 0
    	    var nParent = newStringGraph.forwardArcs.find(a => a._2 == n.id);
    	  	nParent match {
    	  	  case Some((parent,child)) => {
    	  		  newStringGraph.forwardArcs -= Pair(parent,child);
    	  		  if(newStringGraph.nodes(parent).nodeType == NodeType.concat)
    	  			  newStringGraph.nodes(parent).sons = newStringGraph.nodes(parent).sons.remove(s => s == n.id);
    	  	    };
    	  	  case None => if(newStringGraph.rootID == n.id) {
    	  		  	newStringGraph.isEmpty = true;
    	  		  	nodes = Map.empty;
    	  		  	forwardArcs = Set.empty;
    	  		  	backwardArcs = Set.empty;
    	  		  	rootID = "";
    	  	    }; 
    	  	}
    	  }
	      //Console.println("applied rule 2.1");
	      return newStringGraph.normalize();
      }
      
      //RULE 2, part 2
      invalidOrNodes = orNodes.filter(n => sons(n).exists(n2 => n2.nodeType == NodeType.top));
	  if(invalidOrNodes.length > 0) {
		  var newStringGraph = this.clone();
	      var n = invalidOrNodes.apply(0);
	      var maxSon = newStringGraph.sons(n).filter(n2 => n2.nodeType == NodeType.top).apply(0);
	      
	      newStringGraph.forwardArcs = newStringGraph.forwardArcs.map(a => {
	    	  if(a._2 == n.id) {
		    	    if(newStringGraph.nodes(a._1).nodeType == NodeType.concat) {
		    	    	newStringGraph.nodes(a._1).sons = newStringGraph.nodes(a._1).sons.map(s => if(s == n.id) maxSon.id else s); 
		    		}
					(a._1, maxSon.id);
	    	  } else
    				a
	      });
	      if(newStringGraph.rootID == n.id)
	    	  newStringGraph.rootID = maxSon.id;
	             
	      //eliminazione nodi superflui
	      var otherSons = newStringGraph.sons(n).filter(
	        n2 => n2.id != maxSon.id && !newStringGraph.backwardArcs.exists(a => a._1 == n.id && a._2 == n2.id));
	      for(s <- otherSons)
	    	  newStringGraph = newStringGraph.removeNodeRecursive(s);
       
   	      //eliminazione di tutto ció che era connesso al nodo OR
	      newStringGraph.forwardArcs = newStringGraph.forwardArcs.filter(a => !(a._1 == n.id || a._2 == n.id));
	      newStringGraph.backwardArcs = newStringGraph.backwardArcs.filter(a => !(a._1 == n.id || a._2 == n.id));
	      newStringGraph.nodes = newStringGraph.nodes - n.id;

	      //Console.println("applied rule 2.2");
	      return newStringGraph.normalize();
	  }
      
	  //RULE 3
	  invalidOrNodes = orNodes.filter(n => sons(n).exists(n2 => 
	    forwardArcs.contains(Pair(n.id,n2.id)) && n2.nodeType == NodeType.or && indegree(n2) == 1));
	  if(invalidOrNodes.length > 0) {
		  var newStringGraph = this.clone();
	      var n = invalidOrNodes.apply(0);
	      var orSon = sons(n).filter(n2 => forwardArcs.contains(Pair(n.id,n2.id)) && n2.nodeType == NodeType.or && indegree(n2) == 1).apply(0);
	      var orSonSons = sons(orSon);
	      
	      newStringGraph.forwardArcs -= Pair(n.id, orSon.id);
	      for(s <- orSonSons) {
	    	  if(newStringGraph.forwardArcs.contains(Pair(orSon.id,s.id))) {
		    	  newStringGraph.forwardArcs += Pair(n.id, s.id);
		    	  newStringGraph.forwardArcs -= Pair(orSon.id, s.id);
	    	  }
	      }
	      newStringGraph.backwardArcs = newStringGraph.backwardArcs.map(a => {
	    	  if(a._1 == orSon.id) {
					(n.id, a._2)
	    	  } else
    				a
	      });
       
	      newStringGraph.nodes -= orSon.id;
	      
	      //Console.println("applied rule 3");
	      return newStringGraph.normalize();
	  }
   
  	  //RULE 4
	  var invalidBackwardArcs = 
		  backwardArcs.filter(b => (b._1 == b._2) || (!path(nodes(b._2), nodes(b._1)).exists(n => n.nodeType == NodeType.concat)));
  	  if(invalidBackwardArcs.size > 0) {
		  var newStringGraph = this.clone();
	      for(a <- invalidBackwardArcs) {
	    	  newStringGraph.backwardArcs -= a;
	    	  if(nodes(a._1).nodeType == NodeType.concat)
	    		  nodes(a._1).sons = nodes(a._1).sons.remove(s => s == a._2); 
	      }
	      //Console.println("applied rule 4");
	      return newStringGraph.normalize();
      }
     
  	  //RULE 5
  	  //principal label restriction
  	  invalidOrNodes = orNodes.filter(n => sons(n).exists(n1 => sons(n).exists(n2 => (n1 != n2) && (prlb(n1).intersect(prlb(n2)).size > 0))));
      if(invalidOrNodes.length > 0) {
		  var or = invalidOrNodes.apply(0);
		  var invalidSon = sons(or).find(n1 => sons(or).exists(n2 => (n1 != n2) && (prlb(n1).intersect(prlb(n2)).size > 0)));
		  invalidSon match {
		    case Some(n1) => {
		    	var otherInvalidSon = sons(or).find(n2 => (n1 != n2) && (prlb(n1).intersect(prlb(n2)).size > 0));
		    	otherInvalidSon match {
		    		case Some(n2) => {
		    			//or is the OR node; n1 and n2 are the two sons of n such that their principal labels sets have some intersection
	    				var newStringGraph = this.clone();
		    			
		    			if(
		    			  (n1.nodeType == NodeType.simple && n2.nodeType == NodeType.simple && n1.character == n2.character) ||
		    			  (n1.nodeType == NodeType.top && n2.nodeType == NodeType.top) ||
		    			  (n1.nodeType == NodeType.bottom && n2.nodeType == NodeType.bottom)
		    			) {    				
		    				newStringGraph.forwardArcs -= Pair(or.id,n2.id);
		    				newStringGraph.nodes -= n2.id;
	    				} else if( (n1.nodeType == NodeType.simple || n1.nodeType == NodeType.top || n1.nodeType == NodeType.bottom) 
	    						&& n2.nodeType == NodeType.or) {
	    					newStringGraph.forwardArcs -= Pair(or.id,n1.id);
		    				newStringGraph.nodes -= n1.id;
	    				} else if( (n2.nodeType == NodeType.simple || n2.nodeType == NodeType.top || n2.nodeType == NodeType.bottom) 
	    						&& n1.nodeType == NodeType.or) {
	    					newStringGraph.forwardArcs -= Pair(or.id,n2.id);
		    				newStringGraph.nodes -= n2.id;
	    				} else if(n1.nodeType == NodeType.concat && n2.nodeType == NodeType.concat &&
	    					n1.sons.length == n2.sons.length) {
	    					var newConcatNode = new Node(NodeType.concat);
	    					for(i <- 0 to n1.sons.length-1) {
	    						var newOrNode = new Node(NodeType.or);
	    						if(newStringGraph.forwardArcs.exists(a => a._1 == n1.id && a._2 == n1.sons.apply(i))) {
	    							newStringGraph.forwardArcs += Pair(newOrNode.id, n1.sons.apply(i));
	    							newStringGraph.forwardArcs -= Pair(n1.id, n1.sons.apply(i));
	    						} else {
	    							newStringGraph.backwardArcs += Pair(newOrNode.id, n1.sons.apply(i));
	    							newStringGraph.backwardArcs -= Pair(n1.id, n1.sons.apply(i));
	    						}
	    						if(newStringGraph.forwardArcs.exists(a => a._1 == n2.id && a._2 == n2.sons.apply(i))) { 
	    							newStringGraph.forwardArcs += Pair(newOrNode.id, n2.sons.apply(i));
	    							newStringGraph.forwardArcs -= Pair(n2.id, n2.sons.apply(i));
	    						} else {
	    							newStringGraph.backwardArcs += Pair(newOrNode.id, n2.sons.apply(i));
	    							newStringGraph.backwardArcs -= Pair(n2.id, n2.sons.apply(i));
	    						}
	    						newConcatNode.sons = newConcatNode.sons ::: List(newOrNode.id);
    							newStringGraph.forwardArcs += Pair(newConcatNode.id, newOrNode.id);
	    						newStringGraph.nodes += Pair(newOrNode.id, newOrNode);
    						}
	    					newStringGraph.nodes += Pair(newConcatNode.id, newConcatNode);
	    					newStringGraph.forwardArcs += Pair(or.id, newConcatNode.id);
	    					newStringGraph.forwardArcs -= Pair(or.id, n1.id);
	    					newStringGraph.forwardArcs -= Pair(or.id, n2.id);
	    					newStringGraph.nodes -= n1.id;
	    					newStringGraph.nodes -= n2.id;
	    				} else if(n1.nodeType == NodeType.concat && n2.nodeType == NodeType.or) {
	    					newStringGraph.forwardArcs += Pair(n2.id, n1.id);
	    					newStringGraph.forwardArcs -= Pair(or.id, n1.id);
	    				} else if(n2.nodeType == NodeType.concat && n1.nodeType == NodeType.or) {
	    					newStringGraph.forwardArcs += Pair(n1.id, n2.id);
	    					newStringGraph.forwardArcs -= Pair(or.id, n2.id);
	    				} else if(n1.nodeType == NodeType.or && n2.nodeType == NodeType.or) {
	    					var newOrNode = new Node(NodeType.or);
	    					
	    					newStringGraph.forwardArcs = newStringGraph.forwardArcs.map(a => {
	    						if(a._1 == n1.id || a._1 == n2.id)
	    							(newOrNode.id, a._2)
    							else if(a._2 == n1.id || a._2 == n2.id)
    								(a._1, newOrNode.id)
								else
									a
	    					});
	    					newStringGraph.backwardArcs = newStringGraph.backwardArcs.map(a => {
	    						if(a._1 == n1.id || a._1 == n2.id)
	    							(newOrNode.id, a._2)
    							else if(a._2 == n1.id || a._2 == n2.id)
    								(a._1, newOrNode.id)
								else
									a
	    					});
          
	    					var concatNodes = newStringGraph.nodes.values.filter(n => n.nodeType == NodeType.concat);
	    					for(n <- concatNodes) {
	    						n.sons = n.sons.map(s => if(s==n1.id || s==n2.id) newOrNode.id else s);  
	    					}
          
	    					newStringGraph.nodes += Pair(newOrNode.id,newOrNode);
	    					newStringGraph.nodes -= n1.id;
	    					newStringGraph.nodes -= n2.id;
	    				}

		    			//Console.println("applied rule 5");
		    			//Console.println(newStringGraph.toString());
		    			return newStringGraph.normalize();
		    		}
		    		case None => ;
		    	}
		    }
		    case None => ;
		  }
      }
   
  	  //MY_RULE 1
  	  //un nodo concat deve avere almeno due figli altrimenti viene sostituito dal suo unico figlio
  	  var concatNodes = nodes.values.filter(n => n.nodeType == NodeType.concat).toList;
  	  var invalidConcatNodes = concatNodes.filter(n => n.sons.length == 1); 
  	  if(invalidConcatNodes.length > 0) {
  		  var newStringGraph = this.clone();
  		  var n = invalidConcatNodes.apply(0);
  		  var son = newStringGraph.nodes(n.sons.apply(0));
      
  		  //the concat node has one sons, but the arc between them could be forward or backward
  		  newStringGraph.backwardArcs -= Pair(n.id, son.id);
  		  newStringGraph.forwardArcs -= Pair(n.id, son.id);
  		  
  	      newStringGraph.backwardArcs = newStringGraph.backwardArcs.map(a => {
	    	  if(a._2 == n.id) {
					(a._1, son.id)
	    	  } else
    				a
	      });

  	      if(newStringGraph.rootID == n.id) {
  	    	  newStringGraph.rootID = son.id;
  	      } else {
  	        var nParent = newStringGraph.forwardArcs.find(a => a._2 == n.id);
  	        nParent match {
  	        	case Some((parent,child)) => {
  	        		newStringGraph.forwardArcs -= Pair(parent,child);
  	        		newStringGraph.forwardArcs += Pair(parent,son.id);
  	        		
  	        		if(newStringGraph.nodes(parent).nodeType == NodeType.concat) 
  	        			newStringGraph.nodes(parent).sons = newStringGraph.nodes(parent).sons.map(s => if(s == n.id) son.id else s);
  	        	}
  	        	case None => ;
  	        }
  	      }
         
  	      newStringGraph.nodes -= n.id;
      
  	      //Console.println("applied rule M1");
  	      return newStringGraph.normalize();
  	  }
     
  	  //MY_RULE 2
  	  //un nodo concat i cui figli sono tutti nodi top diventa un unico nodo top
  	  invalidConcatNodes = concatNodes.filter(n => n.sons.forall(n2 => nodes(n2).nodeType == NodeType.top));
  	  if(invalidConcatNodes.length > 0) {
  	    var n = invalidConcatNodes.apply(0);
  	    var topNode = new Node(NodeType.top);
       
  	    if(this.rootID == n.id) { 
  	    	var result = new StringGraph(topNode.id, false);
  	    	result.addNode(topNode);
            //Console.println("applied rule M2");
  	    	return result;
  	    } 
       
  	    var newStringGraph = this.clone();
  	    var nParent = newStringGraph.forwardArcs.find(a => a._2 == n.id);
        nParent match {
        	case Some((parent,child)) => {
		  	    newStringGraph.forwardArcs = newStringGraph.forwardArcs.map(a => {
		  	    	if(a._1 == parent && a._2 == n.id)
		  	    		Pair(parent, topNode.id)
	  	    		else
	  	    			a
		  	    });  
        		if(newStringGraph.nodes(parent).nodeType == NodeType.concat) 
        			newStringGraph.nodes(parent).sons = newStringGraph.nodes(parent).sons.map(s => if(s == n.id) topNode.id else s);
        	}
        	case None => ;
        }
        newStringGraph = newStringGraph.removeNodeRecursive(n);
        newStringGraph.nodes += Pair(topNode.id,topNode);
       
    	//Console.println("applied rule M2");
  	    return newStringGraph.normalize();       
  	  }
     
  	  //MY_RULE 4
  	  //un nodo concat non puó avere come figlio un ulteriore nodo concat se il suo indegree é pari a 1
  	  invalidConcatNodes = concatNodes.filter(n => n.sons.exists(n2 => nodes(n2).nodeType == NodeType.concat && indegree(nodes(n2)) == 1));
  	  if(invalidConcatNodes.length > 0) {
  		  var n = invalidConcatNodes.apply(0);
  		  var concatSon = n.sons.find(n2 => nodes(n2).nodeType == NodeType.concat && indegree(nodes(n2)) == 1);
  		  concatSon match {
  		    case Some(n2) => {
  		    		var newStringGraph = this.clone();
  		    		newStringGraph.forwardArcs -= Pair(n.id, n2);
  		    		newStringGraph.forwardArcs = newStringGraph.forwardArcs.map(a => if (a._1 == n2) Pair(n.id,a._2) else a);
	    			newStringGraph.backwardArcs = newStringGraph.backwardArcs.map(a => if (a._1 == n2) Pair(n.id,a._2) else a);
	    			
	    			var newSonsOfN : List[String] = Nil;
	    			for(s <- n.sons) {
	    				if(s != n2)
	    					newSonsOfN = newSonsOfN ::: List(s);
	    				else
	    					newSonsOfN = newSonsOfN ::: newStringGraph.nodes(n2).sons;
	    			}
	    			n.sons = newSonsOfN;
        
	    			newStringGraph.nodes -= n2;
	    			
	    			//Console.println("applied rule M4");
	    			return newStringGraph.normalize();
  		      }
  		    case None => ;
  		  }
  	  }
     
  	  //MY_RULE XXX
  	  //two consecutive MAX sons of a concat node will be merged into one
  	  invalidConcatNodes = concatNodes.filter(n => {
  		  var result = false;
  		  for(i <- 0 to n.sons.length - 2) 
  			  if(nodes(n.sons.apply(i)).nodeType == NodeType.top && nodes(n.sons.apply(i+1)).nodeType == NodeType.top)
  				  result = true
  		  result
  	  });
  	  if(invalidConcatNodes.length > 0) {
  		  var n = invalidConcatNodes.apply(0);
  		  var newStringGraph = this.clone();
  		  var newSons : List[String] = Nil;
  		  var i = 0;
  		  for(i <- 0 to n.sons.length - 2) {
  			  if(nodes(n.sons.apply(i)).nodeType == NodeType.top && nodes(n.sons.apply(i+1)).nodeType == NodeType.top) {
  				  newStringGraph.forwardArcs -= Pair(n.id, n.sons.apply(i));
  				  newStringGraph.nodes -= n.sons.apply(i);
  			  } else
  				  newSons = newSons ::: List(n.sons.apply(i));
  		  }
  		  newSons = newSons ::: List(n.sons.apply(n.sons.length-1));
  		  n.sons = newSons;
  		  //Console.println("applied rule MX");
  		  return newStringGraph.normalize();
  	  }
   
	  return this;
  }
  
  def normalize() : StringGraph = {
	  return normalizeHelper();
  }
  
  /*
   * Check if the string graph is normal, that is it satisfies 
   * the conditions defined in the paper of Janssens and Bruynooghe
   * (page 20) plus the conditions specific for strings defined in 
   * my MD thesis
  */
  def isNormalized() : Boolean = {
    //RULE 1
	if(nodes.values.exists(n => n.nodeType == NodeType.bottom && n.id != rootID))
		return false;
 
    //RULE 2
    var orNodes = nodes.values.filter(n => n.nodeType == NodeType.or).toList;
    if(orNodes.exists(n => outdegree(n) < 2 || sons(n).exists(n2 => n2.nodeType == NodeType.top)))
    	return false;
    
    //RULE 3
    var ff = forwardArcs.filter(a => nodes(a._1).nodeType == NodeType.or && nodes(a._2).nodeType == NodeType.or);
    if(ff.exists(a => indegree(nodes(a._2)) < 2))
    	return false;
    
    //RULE 4
    for(b <- backwardArcs) {
    	if(b._1 == b._2)
    		return false;
    	if(!path(nodes(b._2), nodes(b._1)).exists(n => n.nodeType == NodeType.concat))
    		return false;
    }
    
    //RULE 5
    for(n <- orNodes) {
    	var orSons = sons(n);
    	for(n1 <- orSons)
    		for(n2 <- orSons) {
    			if(n1.id != n2.id) {
    				if(prlb(n1).intersect(prlb(n2)).size > 0)
						return false;
				}
    		}
    }
    
    //MY_RULE 1 && 2
    var concatNodes = nodes.values.filter(n => n.nodeType == NodeType.concat).toList;
    if(concatNodes.exists(n => sons(n).length < 2 || sons(n).forall(n => n.nodeType == NodeType.top)))
    	return false;

    //MY_RULE 3 sembra ridondante rispetto alla regola 4
    
    //MY_RULE 4
    for(n <- concatNodes) {
      var nSons = sons(n);
      for(son <- nSons) {
    	  if(son.nodeType != NodeType.simple && son.nodeType != NodeType.top && son.nodeType != NodeType.or && 
            !(son.nodeType == NodeType.concat && indegree(son) > 1))
    		  return false;
      }
    }
    
	//MY_RULE XXX
    for(n <- concatNodes) {
    	for(i <- 0 to n.sons.length - 2) 
    		if(nodes(n.sons.apply(i)).nodeType == NodeType.top && nodes(n.sons.apply(i+1)).nodeType == NodeType.top)
			{
    			return false
			}
    }
	
    return true;
  }
  
  /*
   * Check if the string graph satisfies the five conditions
   * defined in the paper of Janssens and Bruynooghe (page 16)
  */
  def validate() : Boolean = {
    if(isEmpty)
      return true;
    
    for(n <- nodes.values) {
    	var incomingForwardArcs = forwardArcs.filter(p => p._2 == n.id).size;
    	//RULE P1
    	if(n.id == rootID && incomingForwardArcs != 0)
    		return false;
    	//RULE P2
    	if(incomingForwardArcs != 1 && n.id != rootID)
    		return false;
    	//RULE P3
    	if(n.id != rootID && !tanc(n).contains(nodes(rootID)))
    		return false;
    	//RULE P4
    	if(backwardArcs.exists(a => !tanc(nodes(a._1)).contains(nodes(a._2))))
    		return false;
    	//RULE P5.1
    	if(n.nodeType == NodeType.simple || n.nodeType == NodeType.top || n.nodeType == NodeType.bottom)
    		if(sons(n) != Nil)
    			return false;
    	//RULE P5.2
    	if(n.nodeType == NodeType.or)
    		if(sons(n).length < 0)
    			return false;
    }
    
    return true;
  }
  
  def addNode(node : Node) = {
	  nodes += node.id -> node;
	  isEmpty = false;
  }
  
  def indegree(node : Node) : Int = {
	  var incomingForwardArcs = forwardArcs.filter(p => p._2 == node.id);
	  var incomingBackwardArcs = backwardArcs.filter(p => p._2 == node.id);
	  return incomingForwardArcs.size + incomingBackwardArcs.size;
  }
  
  def outdegree(node : Node) : Int = {
	  var outgoingForwardArcs = forwardArcs.filter(p => p._1 == node.id);
	  var outgoingBackwardArcs = backwardArcs.filter(p => p._1 == node.id);
	  return outgoingForwardArcs.size + outgoingBackwardArcs.size;
  }
  
  def sons(node : Node) : List[Node] = {
	  if(isEmpty)
		  return Nil;
   
	  if(node.nodeType == NodeType.concat)
		  return node.sons.map(n => nodes(n));
	  else {
		  var forwardSons = forwardArcs.filter(p => p._1 == node.id).map(p => nodes(p._2));
		  var backwardSons = backwardArcs.filter(p => p._1 == node.id).map(p => nodes(p._2));
		  return (forwardSons.toList ++ backwardSons.toList).sort((s1,s2) => s1.label() <= s2.label());
	  }
  }
  
  def prnd(node : Node) : Set[Node] = {
	if(node.nodeType == NodeType.or)
    {
    	var s : Set[Node] = Set.empty;
     	var ns = sons(node);
     	var k = ns.length;
     	for(i <- 0 to k-1)
     		s = s ++ prnd(ns.apply(i));
     	return s;
    } else
      Set(node);
  }
  
  def prlb(node : Node) : Set[String] = {
	  return prnd(node).map(n => {
	    if(n.nodeType == NodeType.concat)
	    	n.label() + "/" + sons(n).length.toString()
	    else
	    	n.label()
	  }); 
  }
  
  def tanc(node : Node) : Set[Node] = {
	  var incomingForwardParents = forwardArcs.filter(p => p._2 == node.id).map(p => nodes(p._1));
	  var ancestors : Set[Node] = Set.empty;
	  for(parent <- incomingForwardParents)
		ancestors = ancestors ++ tanc(parent); 
	  return ancestors ++ incomingForwardParents;
  }
  
  def path(from : Node, to : Node) : List[Node] = {
	  if(from.id == to.id)
		  return List(to);
	  if(!tanc(to).contains(from))
		  return Nil;
   
	  var pred = forwardArcs.filter(a => a._2 == to.id).map(a => a._1);
	  if(pred.size != 1)
		  return Nil; //invalid string graph or TO is the root
	  var p = pred.toList.apply(0);
   
	  if(p == from.id)
		  return List(from,to);
   
	  return path(from,nodes(p)) ::: List(to);
  }
  
  def depth(node : Node) : Int = {
	  var pathFromRoot = path(nodes(rootID),node);
	  return pathFromRoot.length;
  }
  
  def sameDepth(n1 : Node, sg1 : StringGraph, n2 : Node, sg2 : StringGraph) : Boolean = {
	  var d1 = sg1.depth(n1);
	  var d2 = sg2.depth(n2);
	  return d1 == d2;
  }
  
  def samepf(n1 : Node, sg1 : StringGraph, n2 : Node, sg2 : StringGraph) : Boolean = {
	  var pf1 = sg1.prlb(n1);
	  var pf2 = sg2.prlb(n2);
	
	  if(pf1.subsetOf(pf2) && pf2.subsetOf(pf1))
		  return true;
   
	  return false;
  }
  
  def replaceEdge(edgeToRemove : Pair[String,String], edgeToInsert : Pair[String,String]) : StringGraph = {
	  var newStringGraph = this.clone();
   
	  newStringGraph.forwardArcs -= edgeToRemove;
	  newStringGraph.backwardArcs -= edgeToRemove;
   
	  if(newStringGraph.nodes(edgeToRemove._1).nodeType == NodeType.concat) {
		  if(edgeToRemove._1 == edgeToInsert._1)
			  newStringGraph.nodes(edgeToRemove._1).sons = 
				  	newStringGraph.nodes(edgeToRemove._1).sons.map(s => if(s==edgeToRemove._2) edgeToInsert._2 else s);
		  else {
			  newStringGraph.nodes(edgeToRemove._1).sons = newStringGraph.nodes(edgeToRemove._1).sons.remove(s => s == edgeToRemove._2);
			  if(newStringGraph.nodes(edgeToInsert._1).nodeType == NodeType.concat) {
				  newStringGraph.nodes(edgeToInsert._1).sons = newStringGraph.nodes(edgeToInsert._1).sons ::: List(edgeToInsert._2); 
			  }
		  }
	  }
   
	  if(newStringGraph.tanc(newStringGraph.nodes(edgeToInsert._1)).contains(newStringGraph.nodes(edgeToInsert._2)))
		  newStringGraph.backwardArcs += edgeToInsert;
	  else
		  newStringGraph.forwardArcs += edgeToInsert;
	  
	  var root = newStringGraph.nodes(rootID);
	  //remove unconnected nodes
	  var unconnectedNodes = newStringGraph.nodes.values.filter(n => newStringGraph.path(root,n) == Nil).toList;
	  for(un <- unconnectedNodes)
		  newStringGraph.nodes -= un.id;
	  //remove unconnected arcs
	  newStringGraph.forwardArcs = newStringGraph.forwardArcs.filter(a => 
	    newStringGraph.nodes.contains(a._1) && newStringGraph.nodes.contains(a._2));
	  newStringGraph.backwardArcs = newStringGraph.backwardArcs.filter(a => 
	    newStringGraph.nodes.contains(a._1) && newStringGraph.nodes.contains(a._2));   
	  
	  Console.println("replaced edge");
	  return newStringGraph.normalize();
  }
  
  //used for widening
  def replaceNode(n : Node, n1 : Node) : StringGraph = {
	  var newStringGraph = this.clone();
	  var max = new Node(NodeType.top);
	  newStringGraph.addNode(max);
	  newStringGraph.forwardArcs = newStringGraph.forwardArcs.map(a => if(a._2 == n.id) Pair(a._1,max.id) else a);
	  for(node <- newStringGraph.nodes.values) {
		  if(node.nodeType == NodeType.concat)
			  node.sons = node.sons.map(s => if(s == n.id) max.id else s);
	  }
	  newStringGraph = newStringGraph.removeNodeRecursive(n);
	  if(newStringGraph.rootID == n.id)
		  newStringGraph.rootID = max.id;
   
	  Console.println("replaced node");
	  return newStringGraph.normalize();
  }
  
  def toStringHelper(currNode : Node) : String = {
    currNode.nodeType match {
      case NodeType.concat => { 
    	  var str = "";
    	  if(backwardArcs.exists(a => a._2 == currNode.id))
    		  str += currNode.id + " = ";
    	  str += "CONCAT[";
    	  var sons = currNode.sons;
    	  for(s <- sons) {
    		  if(!backwardArcs.contains((currNode.id,s)))
    			  str += toStringHelper(nodes(s)) + ", ";
    		  else
    			  str += s + ", ";
    	  }
    	  str += "]";
    	  return str;
        }
      case NodeType.simple => return "'" + currNode.character.toString() + "'";
      case NodeType.top => return "max";
      case NodeType.bottom => return "_|_";
      case NodeType.or => {
    	  var properSons = forwardArcs.filter(p => p._1 == currNode.id);
    	  var improperSons = backwardArcs.filter(p => p._1 == currNode.id);
    	  var str = ""; 
    	  if(backwardArcs.exists(a => a._2 == currNode.id))
    		  str += currNode.id + " = ";
    	  str += "OR[";
    	  for(ps <- properSons)
    		  str += toStringHelper(nodes(ps._2)) + ", ";
    	  for(is <- improperSons)
    		  str += is._2 + ", ";
    	  str += "]";
    	  return str;
        }
    }
  }
  
  override def toString() : String = {
	  if(!isEmpty)
		  return toStringHelper(nodes(rootID));
	  else 
		  return "EMPTY";
  }
  
  def equalsHelper(currNodeLeft : Node, sgLeft : StringGraph, currNodeRight : Node, sgRight : StringGraph) : Boolean = {
	  if(currNodeLeft.equals(currNodeRight)) {
	    var sonsLeft = currNodeLeft.sons;
	    var sonsRight = currNodeRight.sons;
	    var sonsLeftLength = sonsLeft.length;
	    var sonsRightLength = sonsRight.length;
	    if(sonsLeftLength != sonsRightLength)
	    	return false;
	    else {
	      for(i <- 0 to sonsLeftLength-1) {
	    	var curr1 = sonsLeft.apply(i);
	    	var curr2 = sonsRight.apply(i);
      
    	  	if(!sgLeft.nodes(curr1).equals(sgRight.nodes(curr2)))
    	  		return false;
	      }
	      return true;
	    }
	  } else
		  return false;
  }
  
  def equals(right : StringGraph) : Boolean = {
	  if(this.isEmpty || right.isEmpty) {
		  if(this.isEmpty && right.isEmpty)
			  return true;
		  else 
			  return false;
	  }
   
	  return equalsHelper(this.nodes(this.rootID), this, right.nodes(right.rootID), right);
  }
  
  //replace a node with an equivalent one but with different id
  def replaceNodeId(n1 : Node, n2 : Node) : StringGraph = {
	  var newStringGraph = this.clone();
   
	  newStringGraph.forwardArcs = newStringGraph.forwardArcs.map(a => { 
	  	if(a._1 == n1.id)
	  		Pair(n2.id, a._2)
	  	else if(a._2 == n1.id)
	  		Pair(a._1, n2.id)
	  	else
	  		a
	  });
   	  newStringGraph.backwardArcs = newStringGraph.backwardArcs.map(a => { 
	  	if(a._1 == n1.id)
	  		Pair(n2.id, a._2)
	  	else if(a._2 == n1.id)
	  		Pair(a._1, n2.id)
	  	else
	  		a
	  });
      
      if(newStringGraph.rootID == n1.id)
    	  newStringGraph.rootID = n2.id;
      
      newStringGraph.nodes -= n1.id;
      newStringGraph.nodes += Pair(n2.id, n2);
      
      /*for(n <- newStringGraph.nodes.values) {
    	  if(n.nodeType == NodeType.concat)
    	    n.sons = n.sons.map(s => if(s==n1.id) n2.id else s);
      }*/
   
	  return newStringGraph;
  }
}

class StringGraphsDomain extends Lattice[StringGraphsDomain]
{
  //var isBottom : Boolean = false; 
  //var isTop : Boolean = false;
  var sg : StringGraph = new StringGraph("", true);
  
  override def factory() : StringGraphsDomain = new StringGraphsDomain();
  
  def isTop() : Boolean = {
	  if(!sg.isEmpty && sg.forwardArcs == Set.empty && sg.backwardArcs == Set.empty && sg.nodes.keySet.size == 1 && 
	  	sg.nodes(sg.rootID).nodeType == NodeType.top)
		  return true;
   
	  return false;
  }
  
  def top() : StringGraphsDomain = {
    val result : StringGraphsDomain = this.factory();
    //result.isTop = true;
    //result.isBottom = false; 
	var topNode = new Node(NodeType.top);
	result.sg.rootID = topNode.id;
	result.sg.addNode(topNode);
    result
  }

  def isBottom() : Boolean = {
	  if(!sg.isEmpty && sg.forwardArcs == Set.empty && sg.backwardArcs == Set.empty && sg.nodes.keySet.size == 1 && 
	  	sg.nodes(sg.rootID).nodeType == NodeType.bottom)
		  return true;
   
	  return false;
  }

  def bottom() : StringGraphsDomain = {
    val result : StringGraphsDomain = this.factory();
//    result.isTop = false;
//    result.isBottom = true; 
	var bottomNode = new Node(NodeType.bottom);
	result.sg.rootID = bottomNode.id;
	result.sg.addNode(bottomNode);
    result    
  }

  def lub(left : StringGraphsDomain, right : StringGraphsDomain) : StringGraphsDomain = {
		  //return lub2(left,right);
		  return widening(left,right);
  }
  
  def lub2(left : StringGraphsDomain, right : StringGraphsDomain) : StringGraphsDomain = {
	if(left.sg.isEmpty)
		return right;
	if(right.sg.isEmpty)
		return left;
	if(left.sg.equals(right.sg))
		return left;

	if(left.sg.nodes.keys.exists(k => right.sg.nodes.keys.exists(k2 => k==k2))) {
		//Console.println("ATTENZIONE: ci sono chiavi uguali nei due SG!");
		var keysInCommon = left.sg.nodes.keys.filter(k => right.sg.nodes.keys.exists(k2 => k==k2)).toList;
		var idChanges : Map[String,String] = Map.empty;
		for(k <- keysInCommon) {
			var node = left.sg.nodes(k);
			var newNode = node.clone();
			left.sg = left.sg.replaceNodeId(node,newNode);
			idChanges += node.id -> newNode.id;
		}
		for(n <- left.sg.nodes.values) {
    	  if(n.nodeType == NodeType.concat)
    	    n.sons = n.sons.map(s => if(idChanges.keySet.contains(s)) idChanges(s) else s);
		}
	}
	/*
	if(left.sg.nodes.keys.exists(k => right.sg.nodes.keys.exists(k2 => k==k2)))
		Console.println("ATTENZIONE: ci sono ANCORA chiavi uguali nei due SG!");
	else {
		Console.println("OK: NON ci sono piú chiavi uguali nei due SG!");
		Console.println("LEFT: " + left.toString);
		Console.println("RIGHT: " + right.toString);
	}
	*/
 
    //return left;
    //Console.print("iniziato calcolo lub...");
	var result = new StringGraphsDomain();
	val orNode = new Node(NodeType.or);
	result.sg.forwardArcs += Pair(orNode.id, left.sg.rootID);
	result.sg.forwardArcs += Pair(orNode.id, right.sg.rootID);
	result.sg.rootID = orNode.id;
	result.sg.addNode(orNode);
	for(n <- left.sg.nodes.values) { result.sg.addNode(n); }
	for(n <- right.sg.nodes.values) { result.sg.addNode(n); }
	for(a <- left.sg.forwardArcs) { result.sg.forwardArcs += a; }
	for(a <- right.sg.forwardArcs) { result.sg.forwardArcs += a; }
	for(a <- left.sg.backwardArcs) { result.sg.backwardArcs += a; }
	for(a <- right.sg.backwardArcs) { result.sg.backwardArcs += a; }
	result = result.normalize();
	//Console.println("finito!");
	return result;
  }
  
  def correspondenceSet(left : StringGraphsDomain, right : StringGraphsDomain) : Set[Pair[Node,Node]] = {
	 if(left.sg.isEmpty || right.sg.isEmpty)
		 return Set.empty; //sicuri? o ritornare qualcos'altro???? check!!!
   
	 var cSet = Set(Pair(left.sg.nodes(left.sg.rootID), right.sg.nodes(right.sg.rootID)));
	 var prevSize = 0;
	 while(cSet.size > prevSize) {
		 prevSize = cSet.size; 
		 for(Pair(v1,v2) <- cSet) {
			 if(left.sg.sameDepth(v1, left.sg, v2, right.sg) && left.sg.samepf(v1, left.sg, v2, right.sg)) {
			 	for(i <- 0 to left.sg.sons(v1).length-1)
			 		cSet += Pair(left.sg.sons(v1).apply(i), right.sg.sons(v2).apply(i));
			 }
		 }
	 }
	 return cSet;
  }
  
  def topologicalClashes(left : StringGraphsDomain, right : StringGraphsDomain) : Set[Pair[Node,Node]] = {
	  if(left.lessEqual(right)) {
		  var corrSet = correspondenceSet(left,right);
		  return corrSet.filter(v => !(left.sg.sameDepth(v._1, left.sg, v._2, right.sg) && left.sg.samepf(v._1, left.sg, v._2, right.sg)));
	  }
	  return Set.empty;
  }
  
  def wideningClashes(left : StringGraphsDomain, right : StringGraphsDomain) : Set[Pair[Node,Node]] = {
	  var topClashes = topologicalClashes(left, right);
	  return topClashes.filter(v => right.sg.prlb(v._2) != Set.empty && 
                       ( (!left.sg.samepf(v._1, left.sg, v._2, right.sg) && left.sg.sameDepth(v._1, left.sg, v._2, right.sg)) || 
                             (left.sg.depth(v._1) < right.sg.depth(v._2)) ));
  }
  
  //cycle introduction rule
  def CI(leftD : StringGraphsDomain, rightD : StringGraphsDomain) : Set[Pair[Pair[Node,Node],Pair[Node,Node]]] = {
	  var left = leftD.sg;
	  var right = rightD.sg;
	  var wc = wideningClashes(leftD,rightD);
	  var ci : Set[Pair[Pair[Node,Node],Pair[Node,Node]]] = Set.empty;
	  
	  for(clash <- wc) {
		  var v0 = clash._1;
		  var vn = clash._2;
		  var possibleVAs = right.tanc(vn).filter(anc => 
		    rightD.lessEqualNodes(vn, right, anc, right) && 
		    left.depth(v0) >= right.depth(anc));
		  for(va <- possibleVAs) {
			  if(vn.id != right.rootID) {
				  var possibleParent = right.forwardArcs.find(a => a._2 == vn.id);
				  possibleParent match {
				    case Some(Pair(parent,child)) => { 
				    		var v = right.nodes(parent);
		    				ci += Pair(Pair(v, vn), Pair(v, va));
                       }
				    case _ => ;
				  }
			  }
		  }
	  }
   
	  return ci;
  }

  def TRi(leftD : StringGraphsDomain, rightD : StringGraphsDomain) : StringGraphsDomain = {
	  var ci = CI(leftD,rightD);
	  if(ci != Set.empty) {
		  var edgePair = ci.toList.apply(0);
		  var edge1 = edgePair._1;
		  var edge2 = edgePair._2;
		  rightD.sg = rightD.sg.replaceEdge(Pair(edge1._1.id, edge1._2.id), Pair(edge2._1.id, edge2._2.id));
	  }
	  return rightD;
  }

  //replacement rule
  def CR(leftD : StringGraphsDomain, rightD : StringGraphsDomain) : Set[Pair[Node,Node]] = {
	  var left = leftD.sg;
	  var right = rightD.sg;
	  var wc = wideningClashes(leftD,rightD);
	  var cr : Set[Pair[Node,Node]] = Set.empty;
    
	  for(clash <- wc) {
		  var v0 = clash._1;
		  var vn = clash._2;
		  var possibleVAs = right.tanc(vn).filter(anc => 
		    !rightD.lessEqualNodes(vn, right, anc, right) && 
		    left.depth(v0) >= right.depth(anc) &&
		  	(right.prlb(vn).subsetOf(right.prlb(anc)) || (left.depth(v0) < right.depth(vn)))
		  );
		  for(va <- possibleVAs)
			  cr += Pair(vn,va);
	  }
	  
	  return cr;
  }
  
  def TRr(leftD : StringGraphsDomain, rightD : StringGraphsDomain) : StringGraphsDomain = {
	  var cr = CR(leftD,rightD);
	  if(cr != Set.empty) {
		  var nodePair = cr.toList.apply(0);
		  var nodeN = nodePair._1;
		  var nodeA = nodePair._2;
		  rightD.sg = rightD.sg.replaceNode(nodeA, nodeN);
	  }
	  return rightD;
  }

  def widen(left : StringGraphsDomain, right : StringGraphsDomain) : StringGraphsDomain = {
	  //Console.println("widen");
	  if(CI(left,right) != Set.empty)
		  return widen(left, TRi(left,right));
	  else if(CR(left,right) != Set.empty)
		  return widen(left, TRr(left,right));
	  else
		  right;
  }
  
  def widening(left : StringGraphsDomain, right : StringGraphsDomain) : StringGraphsDomain = {
	//if(!left.isTop() && !right.isTop() && !left.lessEqual(right) && !right.lessEqual(left))
	  //if(! left.equals(right))
		Console.println("widening between " + left.toString + " and " + right.toString);
	var result : StringGraphsDomain = left;
	if(right.lessEqual(left)) {
		//return left;
		result = left;
		Console.println("right <= left");
	}
	else
		//return widen(left, left.lub2(left,right));
		result = widen(left, left.lub2(left,right));
	//if(!left.isTop() && !right.isTop() && !left.lessEqual(right) && !right.lessEqual(left))
		Console.println("... result: " + result.toString);
	return result;
  }

  def glb(left : StringGraphsDomain, right : StringGraphsDomain) : StringGraphsDomain = {
	//TODO
    if(left.equals(left.top())) return right;
    if(right.equals(left.top())) return left;
    if(left.lessEqual(right)) return left;
    if(right.lessEqual(left)) return right;
    throw new Exception("TODO");
  }
  
  def lessEqualHelper(n : Node, nStringGraph : StringGraph, m : Node, mStringGraph : StringGraph, sc : Set[Pair[Node,Node]]) : Boolean = {
    //CASE 1
	if(sc.contains(Pair(n, m)))
		return true;
    //CASE 2
	else if(m.nodeType == NodeType.top)
		return true;
    //CASE 3
	else if(n.nodeType == NodeType.concat && m.nodeType == NodeType.concat &&
           nStringGraph.sons(n).length == mStringGraph.sons(m).length && 
           nStringGraph.sons(n).length > 0) {
		var nSons = nStringGraph.sons(n).length;
    	var mSons = mStringGraph.sons(m).length;
    	for(i <- 0 to nSons-1) {
		    if(!lessEqualHelper(nStringGraph.sons(n).apply(i), nStringGraph, mStringGraph.sons(m).apply(i), mStringGraph, sc + Pair(n,m)))
		    	return false;
		}
    	return true;
    //CASE 4
	} else if(n.nodeType == NodeType.or && m.nodeType == NodeType.or) {
		var k = nStringGraph.sons(n).length;
		for(i <- 0 to k-1) {
    	    if(!lessEqualHelper(nStringGraph.sons(n).apply(i), nStringGraph, m, mStringGraph, sc + Pair(n,m)))
    	    	return false;
		}
		return true;
	//CASE 5
	} else if(m.nodeType == NodeType.or && mStringGraph.prnd(m).exists(md => md.label() == n.label())) {
		var mD : Option[Node] = mStringGraph.prnd(m).find(md => md.label() == n.label());
		mD match {
		  case Some(x) => return lessEqualHelper(n, nStringGraph, x, mStringGraph, sc + Pair(n,m)); 
		  case None => return false;  
		}
	//CASE 6
	} else if(n.nodeType == NodeType.simple && m.nodeType == NodeType.simple) //oppure else return same_label(n,m);?
		return n.character == m.character;
	else
		return false;
  }
  
  def lessEqual(right : StringGraphsDomain) : Boolean = {
	if(right.isTop || this.sg.isEmpty)
		return true;
	if(this.isBottom)
		return true;
	if(right.sg.isEmpty)
		return false;
	if(this.equals(right))
		return true;
    val b = lessEqualHelper(this.sg.nodes(this.sg.rootID), this.sg, right.sg.nodes(right.sg.rootID), right.sg, Set.empty);
    return b;
  }
  
  def lessEqualNodes(left : Node, leftSG : StringGraph, right : Node, rightSG : StringGraph) : Boolean = {
		  return lessEqualHelper(left, leftSG, right, rightSG, Set.empty);
  }
  
  override def toString() : String = {
	if(this.isTop)
		return "_T_";
	else if(this.isBottom)
		return "_|_";
	else
		return sg.toString();
  }
  
  def normalize() : StringGraphsDomain = {
	if(this.isBottom || this.isTop)
		return this;
	
	var result = new StringGraphsDomain();
	result.sg = this.sg.normalize();
    return result;
  }
  
  def concatenate(left : StringGraphsDomain, right: StringGraphsDomain) : StringGraphsDomain = {
    if(left.sg.isEmpty)
    	return right;
    if(right.sg.isEmpty)
    	return left;
    
	if(left.sg.nodes.keys.exists(k => right.sg.nodes.keys.exists(k2 => k==k2)))
		Console.println("ATTENZIONE: ci sono chiavi uguali nei due SG!");

    var result = new StringGraphsDomain();
	val concatNode = new Node(NodeType.concat);
	concatNode.sons = concatNode.sons ::: left.sg.rootID :: Nil;
	concatNode.sons = concatNode.sons ::: right.sg.rootID :: Nil;
	result.sg.forwardArcs += Pair(concatNode.id, left.sg.rootID);
	result.sg.forwardArcs += Pair(concatNode.id, right.sg.rootID);
	result.sg.rootID = concatNode.id;
	result.sg.addNode(concatNode);
	for(n <- left.sg.nodes.values) { result.sg.addNode(n); }
	for(n <- right.sg.nodes.values) { result.sg.addNode(n); }
	for(a <- left.sg.forwardArcs) { result.sg.forwardArcs += a; }
	for(a <- right.sg.forwardArcs) { result.sg.forwardArcs += a; }
	for(a <- left.sg.backwardArcs) { result.sg.backwardArcs += a; }
	for(a <- right.sg.backwardArcs) { result.sg.backwardArcs += a; }
	result = result.normalize(); 
	return result;
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
	    	  var concatNode = new Node(NodeType.concat);
	    	  result.sg.rootID = concatNode.id;
	    	  for(c <- charList)
		  		{
		  		  var charNode = new Node(NodeType.simple);
		  		  charNode.character = c;
		  		  result.sg.addNode(charNode);
		  		  concatNode.sons = concatNode.sons ::: charNode.id :: Nil;
		  		  result.sg.forwardArcs += Pair(concatNode.id, charNode.id);
    		    }
	    	  result.sg.addNode(concatNode);
	    	  return result.normalize();
	      } else if(charList.length == 1) {
	    	  var simpleNode = new Node(NodeType.simple);
	      	  simpleNode.character = charList.apply(0);
      		  result.sg.rootID = simpleNode.id;
      		  result.sg.addNode(simpleNode);
      		  return result.normalize();
	      } else if(charList.length == 0) {
	    	  return result.normalize();
	      } else 
	    	  return new StringGraphsDomain().top();
	    }
	    case AbstractOperator(thisExpr, parameters, typeparameters, AbstractOperatorIdentifiers.stringConcatenation, returntyp) =>
	      if(parameters.size == 1)
	        parameters.elements.next match {
	        	case p1 :: Nil => 
		        	val left = this.eval(thisExpr);
		        	val right = this.eval(p1);
		        	//Console.println("concatenating " + left.toString + " that is " + thisExpr.toString + " and " + 
                    //         right.toString + " that is " + p1.toString);
		        	//Console.println("while the state is... " + value.toString);
        			return left.concatenate(left,right);
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
