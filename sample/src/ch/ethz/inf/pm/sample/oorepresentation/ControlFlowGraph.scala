package ch.ethz.inf.pm.sample.oorepresentation

//import scala.collection.mutable._

import ch.ethz.inf.pm.sample._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.SystemParameters

/** 
 * This class represents an oriented weighted graph.
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
trait WeightedGraph[T, W] {
  var nodes : List[T] = Nil
  var edges : Set[(Int, Int, Option[W])] = Set.empty//new HashSet[(Int, Int, Option[W])]
  val weightLabel = "weight"
  
  
  def removeNode(node : T) = {
    val index : Int = this.addNodeIfNotExisting(node);
    var newEdges : Set[(Int, Int, Option[W])] = Set.empty
    for((i1, i2, w) <- edges ) {
      val index1=if(i1>index) i1-1 else i1;
      val index2=if(i2>index) i2-1 else i2;
      if(index!=i1 && index!=i2)
        newEdges=newEdges+((index1, index2, w)); 
    }
    edges=newEdges;
    nodes=remove(nodes, index);
  }
  
  private def remove[T1](list : List[T1], index : Int) : List[T1] = list match {
    case x :: x1 if index!=0 => x :: remove(x1, index-1);
    case x :: x1 if index==0 => remove(x1, index-1);
    case Nil => Nil
  }
  
      /** 
       * Add a node to the current graph
	   *
	   * @param the node to be added
	   * @return the index of the inserted node 
	   */
  def addNode(node : T) : Int = {nodes=nodes ::: node :: Nil; nodes.lastIndexOf(node)}
  
      /** 
       * Add a node to the current graph iff it is not yet in the graph. Otherwise, it returns the index
       * of the existing node.
	   *
	   * @param the node to be added if it does not exist
	   * @return the index of the(evantually inserted) node 
	   */
  def addNodeIfNotExisting(node : T) : Int = {
    for(i <- 0 to nodes.length-1)
      if(nodes.apply(i).equals(node)) return i;
    nodes=nodes ::: node :: Nil; 
    return nodes.lastIndexOf(node)
  }
  
      /** 
       * Replace a node with the given one
	   *
	   * @param index the index of the node to be replaced
	   * @param  node the node to be inserted 
	   */
  def setNode(index : Int, node : T) : Unit = {
    //val array=nodes.toArray
    //array.update(index, node);
    //nodes=array.toList;
    //For Scala 2.8
    nodes=nodes.updated(index, node)
  }
  
  /**
   * Add an edge to the current graph
   *
   * @param from the node from which the edge starts
   * @param to the node to which point the edge
   * @param the weight of the edge or None if this edge is not linked to a weight
   */
  def addEdge(from : Int, to : Int, weight : Option[W]) : Unit = edges=edges.+((from, to, weight))
  
  /**
   * Return the nodes without sorting edges
   *
   * @return the leaves of the graph
   */
  def getLeaves() : Set[T] = {
    var notLeaves : Set[Int] = Set.empty;
    //var connected : Set[Int] = Set.empty;
    for((i1, i2, w) <- edges) //{
      notLeaves=notLeaves+i1;
      //connected=connected+i2;
    //}
    var result : Set[T] = Set.empty;
    for(i <- 0 to nodes.length-1)
      if((! notLeaves.contains(i)))// && connected.contains(i))
        result=result+nodes.apply(i);
    return result;
  }
  
  def entryEdges(index : Int) : Set[(Int, Int, Option[W])] = {
    var edgesSet : Set[(Int, Int, Option[W])]=Set.empty;
    for((i1, i2, w) <- edges)
      if(i2==index)
        edgesSet=edgesSet+((i1, i2, w));
    return edgesSet;
  }
  
  def exitEdges(index : Int) : Set[(Int, Int, Option[W])] = {
    var edgesSet : Set[(Int, Int, Option[W])]=Set.empty;
    for((i1, i2, w) <- edges)
      if(i1==index)
        edgesSet=edgesSet+((i1, i2, w));
    return edgesSet;
  }
  
  def initialBlockInLoop(index : Int) : Boolean = {
    if(entryEdges(index).size<=1) return false;
    var prevEdges : Set[(Int, Int, Option[W])] = this.exitEdges(index);
    var nextEdges : Set[(Int, Int, Option[W])] = prevEdges;
    
    while(nextEdges.size>0) {
    	prevEdges=nextEdges;
    	nextEdges=Set.empty;
	    for((i1, i2, w) <- prevEdges) {
	      if(i2>i1)
	        nextEdges=nextEdges++this.exitEdges(i2);
	      if(i2==index)
	        return true;
	    }
    }
    return false;
  }
  
  
  override def toString() : String = {
    var result : String = ""
    var i : Int = 0;
    while(i < nodes.size) {
      val node : T = nodes.apply(i)
      result=result+
        "Node n."+i+"\n-----------------\n"+
        "Entry node:\n"+entryNodesToString(i)+
        "\nExit node:\n"+exitNodesToString(i)+
        "\nBlock:\n"+nodeToString(node)+"\n-----------------\n";
      i=i+1;
    }
    result
  }
  
  /**
   * It adds a node containing the given statements to the control flow graph.
   * It adds an edge from each entry point to the fresh node (related with the given condition).
   * It returns the index of the fresh node. 
  */
  def addSummaryNode(statements : T, entryPoints : List[(Int, Option[W])]) : Int = {
	val index : Int = this.addNode(statements)
	for(entryindex : (Int, Option[W]) <- entryPoints)
	  this.addEdge(entryindex _1, index, entryindex _2);
    index;
  }
  
  def getEdgesExitingFrom(index : Int) : Set[Int] = {
    var result : Set[Int]=Set.empty;
    for(edge : (Int, Int, Option[W]) <- edges ) {
      if(edge._1.equals(index))
        result=result.+(edge._2);
    }
    result;
  }
  
  protected def nodeToString(node : T) = node.toString();
  
  private def weightToString(weight : Option[W]) : String = weight match {
    case None => ""
    case null => ""
    case Some(x) => x.toString();
  }
  
  protected def entryNodesToString(i : Int) : String = {
    var result : String = "";
    for(edge : (Int, Int, Option[W]) <- edges ) {
      if(edge._2 == i)
        result=result+edge._1+", "+weightLabel+":"+weightToString(edge._3)+"\n";
    }
    result
  }
  
  protected def exitNodesToString(i : Int) : String = {
    var result : String = "";
    for(edge : (Int, Int, Option[W]) <- edges ) {
      if(edge._1 == i)
        result=result+edge._2+", "+weightLabel+":"+weightToString(edge._3)+"\n";
    }
    result
  }
}

/** 
 * This class represents a control flow graph, i.e. a weighted graph where
 * - nodes are list of statements (a sequential piece of code)
 * - the weight of edges are (possible) boolean values, meaning in which way
 *   (<code>true</code> or <code>false</code>) we have to evaluate the condition
 * 	 or if we have always to follow such edge
 * 
 * Thus from each node we can have one exiting edge with no weight, or two edgeds
 * (one <code>true</code> and the other <code>false</code>)
 * 
 * This class extends <code>Statement</code> in order to be as much generic as
 * possible. For instance, consider a statement like <code>variable=if(B) then 1 else 2</code>.
 * Such assignments are allowed by some programming languages (e.g. <code>Scala</code>).
 * Since we want to be as generic as possible, we allow that a statement may be also
 * a structured control flow graph.
 *
 * @author Pietro Ferrara
 * @version 0.1
 */
class ControlFlowGraph(val programpoint : ProgramPoint) extends Statement(programpoint) with WeightedGraph[List[Statement], Boolean] {
  override val weightLabel="condition";
  
  def forwardSemantics[S <: State[S]](state : S) : S= new ControlFlowGraphExecution[S](this, state).forwardSemantics(state).exitState()
  
  def backwardSemantics[S <: State[S]](state : S) : S= new ControlFlowGraphExecution[S](this, state).definiteBackwardSemantics(state).entryState()


  /**
   * Return an ordered list of indexes that could be used for the computation of
   * fixpoints over CFG.
   * There is not guarantee that this sequence will give better results than others
   * since it is based on some heuristics and should be deeply tested.
   *
   * @return A sequence of all the indexes of the CFG
   */
  def getIterativeSequence() : List[Int] = this.getIterativeSequence(Nil, Nil, 0);

  private def getIterativeSequence(alreadyVisited : List[Int], pendingBlocks : List[Int], next : Int) : List[Int] = {
    //We consider the next blocks that have not yet been visited
    def exitNodes = this.getEdgesExitingFrom(next);
    def exitNodesNotVisited = exitNodes.--(alreadyVisited).-(next);
    if(! exitNodesNotVisited.isEmpty) {
      //We take the minimum since we expect to be the optimal one (heuristic)
      val min = this.getMin(exitNodesNotVisited);
      return next :: getIterativeSequence(alreadyVisited:::next::Nil, pendingBlocks++(exitNodesNotVisited-min), min);
    }
    else {
      def pendingNodesNotVisited = pendingBlocks.--(alreadyVisited).-(next);
      if(pendingNodesNotVisited.isEmpty) {
        //We are at the end!
        //assert(this.nodes.size==alreadyVisited.size+1);
        return next :: Nil;
      }
      else  {
        //Otherwise we consider the minimum of the pendingBlocks
        val min = this.getMin(pendingNodesNotVisited.toSet);
        return next :: getIterativeSequence(alreadyVisited:::next::Nil, pendingBlocks++(exitNodesNotVisited-min), min);
      }
    }
  }

  private def getMin(l : Set[Int]) : Int = {
    assert(! l.isEmpty)
    var min : Int = 0-1;
    for(el <- l)
      if(min==0-1 || el<min) min=el;
    assert(min>=0);
    return min;
  }

  override protected def nodeToString(node : List[Statement]) = ToStringUtilities.listToDotCommaRepresentationSingleLine(node);

  override def addNode(st : scala.collection.immutable.List[Statement]) = super.addNode(st);//Work-around for Java interfacing
  
  override def toSingleLineString() : String = {
    if(this.nodes.size!=1) return toString();
    var result : String="";
    for(st <- this.nodes.apply(0))
      result=result+st.toSingleLineString()
    result;
  }
}

class ControlFlowGraphExecution[S <: State[S]](val cfg : ControlFlowGraph, val state : S) extends WeightedGraph[List[S], Boolean] {
  
  override protected def nodeToString(node : List[S]) = if(node!=null) ToStringUtilities.listToNewLineRepresentation[S](node) else "BOTTOM";

  def combinedSemantics(initialState : S, exitState : S) : ControlFlowGraphExecution[S] = {
    val forwardResult=this.semantics(initialState, None, forwardsingleIteration);
    val backwardResult=this.definitesemantics(exitState, Some(forwardResult), definiteBackwardsingleIteration);
    return backwardResult;
    //return forwardResult.glb2(forwardResult, backwardResult, state)
  }
  
  def glb2(left : ControlFlowGraphExecution[S], right : ControlFlowGraphExecution[S], state : S) : ControlFlowGraphExecution[S] = {
    if(! left.cfg.equals(right.cfg))
      throw new CFGSemanticException("It is not possible to compute the glb of the analysis of two different control flow graphs");
    if(left.nodes.length!=right.nodes.length)
      throw new CFGSemanticException("It is not possible to compute the glb of executions with a different nodes in the control flow graph");
    var result : ControlFlowGraphExecution[S]= new ControlFlowGraphExecution[S](left.cfg, state);
    for(i <- 0 to left.nodes.length-1) {
      result.addNode(this.glbOnListOfStates(left.nodes.apply(i), right.nodes.apply(i)));
    }
    return result;
  }

   def glb(left : ControlFlowGraphExecution[S], right : ControlFlowGraphExecution[S]) : ControlFlowGraphExecution[S] = {
    if(! left.cfg.equals(right.cfg))
      throw new CFGSemanticException("It is not possible to compute the glb of the analysis of two different control flow graphs");
    //if(left.nodes.length!=right.nodes.length)
    //  throw new CFGSemanticException("It is not possible to compute the glb of executions with a different nodes in the control flow graph");
    var result : ControlFlowGraphExecution[S]= new ControlFlowGraphExecution[S](left.cfg, state);
    for(i <- 0 to Math.max(left.nodes.length-1, right.nodes.length-1)) {
      if(i>=left.nodes.length)
    	  result.addNode(right.nodes.apply(i));
      else if(i>=right.nodes.length)
    	  result.addNode(left.nodes.apply(i));
      else
    	  result.addNode(this.glbOnListOfStates(left.nodes.apply(i), right.nodes.apply(i)));
    }
    return result;
  }
  
  private def glbOnListOfStates(left : List[S], right : List[S]) : List[S] = left match {
    case x :: xs => right match {
      case y :: ys => x.glb(x, y) :: this.glbOnListOfStates(xs, ys);
      case Nil => x :: this.glbOnListOfStates(xs, Nil); //throw new CFGSemanticException("I cannot make the glb of lists of different length")
    }
    case Nil => right match {
      case Nil => Nil
      case y :: ys => y :: this.glbOnListOfStates(ys, Nil); //throw new CFGSemanticException("I cannot make the glb of lists of different length")
    }
  }
  
  def forwardSemantics(initialState : S) : ControlFlowGraphExecution[S] =
  this.semantics(initialState, None, forwardOptimizedSingleIteration);
  //this.semantics(initialState, None, forwardsingleIteration);
  
  def definiteBackwardSemantics(exitState : S) : ControlFlowGraphExecution[S] = this.semantics(exitState, None, definiteBackwardsingleIteration);
  
  private def semantics(initialState : S, lastresult : Option[ControlFlowGraphExecution[S]], singleIteration : (ControlFlowGraphExecution[S], Option[ControlFlowGraphExecution[S]], S) => ControlFlowGraphExecution[S] ) : ControlFlowGraphExecution[S] = {
    var iteration : Int = 1;
    var prev : ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](cfg, initialState);
    prev.nodes = Nil
    //System.out.println("Iteration n."+iteration);
    var next : ControlFlowGraphExecution[S]=singleIteration(prev, lastresult, initialState).lub(prev);
    while(! next.lessEqual(prev)) {
      prev=next;
      iteration=iteration+1;
      //System.out.println("Iteration n."+iteration);
      if(iteration > SystemParameters.wideningLimit) {
    	  val result=singleIteration(prev, lastresult, initialState)
    	  next=prev.widening(result);
      }
      else next=singleIteration(prev, lastresult, initialState).lub(prev);
    }
    //System.out.println("End of the analysis");
    next.edges=cfg.edges; //TODO: This should be in the constructor and not here!
    next
  }
  
  private def definitesemantics(initialState : S, lastresult : Option[ControlFlowGraphExecution[S]], singleIteration : (ControlFlowGraphExecution[S], Option[ControlFlowGraphExecution[S]], S) => ControlFlowGraphExecution[S] ) : ControlFlowGraphExecution[S] = {
    var iteration : Int = 1;
    var prev : ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](cfg, initialState);
    prev.nodes = Nil
    //var next : ControlFlowGraphExecution[S]=prev.glb(prev, singleIteration(prev, lastresult, initialState));
    prev=prev.glb(prev, singleIteration(prev, lastresult, initialState));
    var next : ControlFlowGraphExecution[S]=prev.glb(prev, singleIteration(prev, lastresult, initialState));
    //while(! next.lessEqual(prev)) {
    while(! prev.lessEqual(next)) {
      prev=next;
      iteration=iteration+1;
      if(iteration > SystemParameters.wideningLimit)
        next=prev.glb(prev, singleIteration(prev, lastresult, initialState));//I should use the narrowing!
      else next=prev.glb(prev, singleIteration(prev, lastresult, initialState));
    }
    next.edges=cfg.edges; //TODO: This should be in the constructor and not here!
    next
  }
  
  def exitState() : S = {
    var result : S = state.bottom();
    for(i <- 0 to nodes.size-1) {
      var isExitPoint : Boolean = true;
      for((from, to, weight) <- cfg.edges) {
      if(from equals(i))
        isExitPoint=false;
      }
      if(isExitPoint) this.getExecution(i) match {
        case Nil =>
        case x =>result=result.lub(result, this.getExecution(i).last);

      }
    }
    result;
  }
  
  def entryState() : S = nodes.apply(0).apply(0);
  
  private def getExecution(i : Int) : List[S] = {
    if( i>=0 && i<nodes.size) nodes.apply(i)
    else null
  } 
  
  private def forwardsingleIteration(prev : ControlFlowGraphExecution[S], lastresult : Option[ControlFlowGraphExecution[S]], initialState : S) : ControlFlowGraphExecution[S] = {
    var next : ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](cfg, state);
    for(i <- 0 to cfg.nodes.size-1) {
      var entry : S = 
        if(i==0)
          initialState;
        else entryState(prev, i);
      entry = lastresult match {
        case Some(x) => x.getExecution(i) match {
          case null => entry
          case l => entry.glb(l.first, entry)
        }
        case None => entry
      }
      val res : List[S] = if(! entry.equals(state.bottom())) {
        lastresult match {
          case Some(x) => x.getExecution(i) match {
            case null => this.forwardBlockSemantics(entry, None, cfg.nodes.apply(i));
            case l => this.forwardBlockSemantics(entry, Some(l), cfg.nodes.apply(i));
          }
          case None => this.forwardBlockSemantics(entry, None, cfg.nodes.apply(i));
        }
      }
      else entry :: Nil;
      next.nodes=next.nodes ::: res :: Nil
    }
    next
  }

  private def getList[S](size : Int, el : S) : List[S] = size match {
    case 0 => Nil
    case _ => el :: getList(size-1, el);
  }

  private def forwardOptimizedSingleIteration(prev : ControlFlowGraphExecution[S], lastresult : Option[ControlFlowGraphExecution[S]], initialState : S) : ControlFlowGraphExecution[S] = {
    var next : ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](cfg, state);
    next.nodes = getList[List[S]](this.cfg.nodes.size, Nil);
    val l = cfg.getIterativeSequence();
    for(j <- 0 to l.size-1) {
      val i : Int = l.apply(j);
      //System.out.println("Semantics of block n."+i);
      var entry : S =
        if(i==0)
          initialState;
        else optimizedEntryState(prev, next, i);
      entry = lastresult match {
        case Some(x) => x.getExecution(i) match {
          case null => entry
          case l => entry.glb(l.first, entry)
        }
        case None => entry
      }
      val res : List[S] = if(! entry.equals(state.bottom())) {
        lastresult match {
          case Some(x) => x.getExecution(i) match {
            case null => this.forwardBlockSemantics(entry, None, cfg.nodes.apply(i));
            case l => this.forwardBlockSemantics(entry, Some(l), cfg.nodes.apply(i));
          }
          case None => this.forwardBlockSemantics(entry, None, cfg.nodes.apply(i));
        }
      }
      else entry :: Nil;
      next.setNode(i, res);
    }
    next
  }

  private def optimizedEntryState(prev : ControlFlowGraphExecution[S], current: ControlFlowGraphExecution[S], index : Int) : S = {
    var result : S = state.bottom();
    for((from, to, weight) <- cfg.edges) {
      if(to equals(index)) {
        //Try to see if we have already computed the given block...
        var pointedBy : List[S] = current.getExecution(from);
        //..and if not we take the results of the previous iteration
        if(pointedBy==null || pointedBy==Nil)
          pointedBy=prev.getExecution(from);
        if(pointedBy!=null && pointedBy!=Nil){
          if(weight==None)
            result=result.lub(result, pointedBy.last)
          else if(weight.get == true)
            result=result.lub(result, pointedBy.last.testTrue())
          else
              result=result.lub(result, pointedBy.last.testFalse())
        }
      }
    }
    result;
  }

  private def definiteBackwardsingleIteration(prev : ControlFlowGraphExecution[S], lastresult : Option[ControlFlowGraphExecution[S]], endingState : S) : ControlFlowGraphExecution[S] = {
    var next : ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](cfg, state);
    for(i <- 0 to cfg.nodes.size-1) {
      var exit : S = 
        if(isExitBlock(i))
          endingState;
        else definiteExitState(prev, i);
      exit = lastresult match {
        case Some(x) => x.getExecution(i) match {
          case null => exit
          case l => exit.glb(l.last, exit)
        }
        case None => exit
      }
      val res : List[S] = if(! exit.equals(state.bottom())) {
        lastresult match {
          case Some(x) => x.getExecution(i) match {
            case null => this.backwardBlockSemantics(exit, None, cfg.nodes.apply(i));
            case l => this.backwardBlockSemantics(exit, Some(l), cfg.nodes.apply(i));
          }
          case None => this.backwardBlockSemantics(exit, None, cfg.nodes.apply(i));
        }
      }
      else exit :: Nil;
      next.nodes=next.nodes ::: res :: Nil
    }
    next
  }
  
  private def isExitBlock(index : Int) : Boolean = {
    for((from, to, weight) <- cfg.edges)
      if(from.equals(index)) return false;
    return true;
  }
  
  private def definiteExitState(prev : ControlFlowGraphExecution[S], index : Int) : S = {
    var result : S = state.top();
    for((from, to, weight) <- cfg.edges) {
      if(from equals(index)) {
        val pointedBy : List[S] = prev.getExecution(to);
        //TODO: add looking at weight and conditions true and false!!!
        if(pointedBy!=null && pointedBy!=Nil){
            result=result.glb(result, pointedBy.first)
        }
      }
    }
    result;
  }
  
  private def entryState(prev : ControlFlowGraphExecution[S], index : Int) : S = {
    var result : S = state.bottom();
    for((from, to, weight) <- cfg.edges) {
      if(to equals(index)) {
        val pointedBy : List[S] = prev.getExecution(from); 
        if(pointedBy!=null && pointedBy!=Nil){
          if(weight==None)
            result=result.lub(result, pointedBy.last)
          else if(weight.get == true)
            result=result.lub(result, pointedBy.last.testTrue())
          else
              result=result.lub(result, pointedBy.last.testFalse())
        }
      }
    }
    result;
  }
  
  private def backwardBlockSemantics(exitState : S, prev : Option[List[S]], block : List[Statement]) : List[S] = block match {
    case Nil => prev match {
      case None => exitState :: Nil
      case Some(y) => exitState.glb(exitState, y.last) :: Nil
    }
    case x : List[Statement] => prev match {
      case None => backwardBlockSemantics(x.last.backwardSemantics[S](exitState), None, x.dropRight(1)) ::: (exitState :: Nil)
      case Some(y) => {
        val state=exitState.glb(exitState, y.last);
        val newResult=x.last.backwardSemantics[S](state)
        return backwardBlockSemantics(newResult, Some(y.dropRight(1)), x.dropRight(1)) ::: (exitState :: Nil)
        }
    }
  }
  
  private def forwardBlockSemantics(entryState : S, prev : Option[List[S]], block : List[Statement]) : List[S] = block match {
    case x :: xs => prev match {
      case None =>
					val modifiedState = entryState.before(identifyingPP(x))
					modifiedState :: forwardBlockSemantics(x.forwardSemantics(modifiedState), None, xs)
      case Some(y :: ys) => entryState ::forwardBlockSemantics(x.forwardSemantics[S](entryState.glb(entryState, y)), Some(ys), xs)
    }
    case Nil => entryState :: Nil
  }

	/**
	 * Returns a program point uniquely identifying a single statement as used in
	 * the block semantics (!). The computation finds the leftmost involved program
	 * point.
	 *
	 * @param s A statement as used in the block semantics
	 * @return The leftmost involved program point
	 */
	private def identifyingPP(s: Statement): ProgramPoint = s match {
		case Assignment(pp, l, r) => (pp :: identifyingPP(l) :: identifyingPP(r) :: Nil).min

		case MethodCall(pp, m, _, p, _) => (pp :: identifyingPP(m) :: p.map(identifyingPP)).min
		case VariableDeclaration(pp, v, _, r) => (pp :: identifyingPP(v) :: identifyingPP(r) :: Nil).min
		case FieldAccess(pp, s, _, _) => (pp :: s.map(identifyingPP)).min
		case _ => s.getPC
	}

	private implicit val programPointOrdering = new Ordering[ProgramPoint] {
		def compare(p1: ProgramPoint, p2: ProgramPoint): Int = p1.getLine.compare(p2.getLine) match {
			case -1 => -1
			case 0 => p1.getColumn.compare(p2.getColumn)
			case 1 => 1
		}
	}

  def lessEqual(right : ControlFlowGraphExecution[S]) : Boolean = lessEqualOnLists[List[S]](this.nodes, right.nodes, checkBlockLessEqual)
  
  private def checkBlockLessEqual(left : List[S], right : List[S]) : Boolean = lessEqualOnLists[S](left, right, lessEqualOnStates)
  
  private def lessEqualOnStates(left : S, right : S) : Boolean = left.lessEqual(right) 
  
  private def lessEqualOnLists[T](left : List[T], right : List[T], operator : (T, T) => Boolean) : Boolean = {
    for(i <- 0 to left.size-1) {
      val state : T = left.apply(i); 
      if(state!=null) {
        if(right.size<=i) return false;
        val rightState : T = right.apply(i);
        if(rightState==null) return false;
        if(! operator(state, rightState)) return false;
      }
    }
    true
  }
  
  def lub(right : ControlFlowGraphExecution[S]) : ControlFlowGraphExecution[S] = {
    if(! this.cfg.equals(right.cfg)) throw new CFGSemanticException("Cannot work with executions of different Control Flow Graphs");
    val result : ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](this.cfg, this.state);
    result.nodes=this.lubListListStates(this.nodes, right.nodes);
    result
  }
  
  private def lubListListStates(left : List[List[S]], right : List[List[S]]) : List[List[S]] = {
    upperBoundsOnLists[List[S]](left, right, lubListStates)
  }
  
  private def lubListStates(left : List[S], right : List[S]) : List[S] = {
    upperBoundsOnLists[S](left, right, lubOnStates)
  }
  
  private def lubOnStates(left : S, right : S) : S = {
    left.lub(left, right)
  }  
    
  def widening(right : ControlFlowGraphExecution[S]) : ControlFlowGraphExecution[S] = {
    if(! this.cfg.equals(right.cfg)) throw new CFGSemanticException("Cannot work with executions of different Control Flow Graphs");
    val result : ControlFlowGraphExecution[S] = new ControlFlowGraphExecution[S](this.cfg, this.state);
    result.nodes=this.wideningListListStates(this.nodes, right.nodes);
    result
  }
  
  private def wideningListListStates(left : List[List[S]], right : List[List[S]]) : List[List[S]] = {
    upperBoundsOnLists[List[S]](left, right, wideningListStates)
  }
  
  private def wideningListStates(left : List[S], right : List[S]) : List[S] = {
    upperBoundsOnLists[S](left, right, wideningOnStates)
  }
  
  private def wideningOnStates(left : S, right : S) : S= {
    left.widening(left, right)
  }  
  
  private def upperBoundsOnLists[T](left : List[T], right : List[T], operator : (T, T) => T) : List[T] = {
    val maxSize=if(right.size>left.size) right.size else left.size;
    var result : List[T] = Nil;//new List[T](maxSize);
    for(i <- 0 to maxSize-1) {
      val elLeft : Option[T] = safeApply(left, i);
      val elRight : Option[T] = safeApply(right, i);
      if(elLeft.equals(None) && elRight.equals(None))
        result=result ::: null.asInstanceOf[T] :: Nil
      else if(elLeft.equals(None))
        result=result ::: elRight.get :: Nil
      else if(elRight.equals(None))
        result=result ::: elLeft.get :: Nil
      else result=result ::: operator(elLeft.get, elRight.get) :: Nil
    }
    return result.toList;
  }
  
  private def safeApply[T](list : List[T], index : Int) : Option[T] = {
    if(index<0 || index >= list.size) return None;
    else return Some(list.apply(index));
  } 
  
  
  override def toString() : String = {
    var result : String = ""
    for(i <- 0 to nodes.size-1) {
      val node : List[S] = nodes.apply(i)
      result=result+
        "Node n."+i+"\n-----------------\n"+
        "Entry node:\n"+entryNodesToString(i)+
        "\nExit node:\n"+exitNodesToString(i)+"\n"
      for(j <- 0 to node.size-1) {
        result=result+node.apply(j).toString()+"\n";
      	if(j < node.size-1)
         result=result+"| "+cfg.nodes.apply(i).apply(j).toSingleLineString+"\nV\n";
      }
    }
    result
  }
}

class CFGSemanticException(message : String) extends Exception(message)