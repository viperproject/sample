package semper.sample.multithreading

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.SystemParameters



class CounterLoopIdentifier(loopPC : ProgramPoint, currentPC : ProgramPoint)
  extends VariableIdentifier("_loopCounter("+loopPC.getLine()+")", SystemParameters.typ.top(), currentPC)

object CFGPreProcessing {
     def augmentCFG(c: ControlFlowGraph) : ControlFlowGraph = {
       var result = new ControlFlowGraph(c.programpoint);
       val loops = this.loops(c);
       var pc = c.programpoint;
       var loopsToPC : Map[Int, ProgramPoint] = Map.empty;
       for(i <- 0 to c.nodes.size-1) {
         var modifiedblock = c.nodes.apply(i);
         val newpc = if (c.nodes.apply(i).size>0) c.nodes.apply(i).last.getPC() else pc;
         loopsToPC = loopsToPC+((i, newpc));
         if(loops.keySet.contains(i)) {
           if (c.nodes.apply(i).size>0) pc = c.nodes.apply(i).head.getPC()
           val newStatement = new Assignment(pc, new Variable(pc, new CounterLoopIdentifier(pc, pc)), new NumericalConstant(pc, "0", SystemParameters.typ.top()))
           modifiedblock = (newStatement :: Nil) ::: modifiedblock
         }
         for(id <- getExitLoopBlock(loops, i)) {
           val counterid = new Variable(newpc, new CounterLoopIdentifier(loopsToPC.apply(id), newpc))
           val increment = new MethodCall(newpc, new FieldAccess(newpc, counterid::Nil, "+", SystemParameters.typ.top()), Nil, new NumericalConstant(newpc, "1", SystemParameters.typ.top())::Nil, SystemParameters.typ.top())
           val newStatement = new Assignment(newpc, counterid, increment);
           modifiedblock = modifiedblock:::newStatement::Nil
         }
         result.nodes=result.nodes ::: (modifiedblock :: Nil)
       }
       result.edges=c.edges
       return result;
     }

  def getExitLoopBlock(loops : Map[Int, Int], block : Int) : Set[Int] = {
    var result : Set[Int]=Set.empty;
    for(i <- loops.keySet)
      if(loops.apply(i)==block)
        result=result+i;
    return result;
  }

  def loops(c : ControlFlowGraph) : Map[Int, Int] = {
    var result : Map[Int, Int] = Map.empty;
     for(edge <- c.edges ) {
       if (edge._3 != None && edge._3.get==false) {
         val edges = c.exitEdges(edge._1);
         if(edges.size==2) {
           var newEdge = (edges - edge).head;
           if(newEdge._3!=None && ! newEdge._3.get.equals(edge._3.get)) {
             findLoop(c, edge._1) match {
               case Some(n) => result=result+((edge._1, n))
               case None =>
             }
           }
         }
       }
     }
    return result;
  };



  def findLoop(c : ControlFlowGraph, block : Int) : Option[Int] = {
    if(c.getEdgesExitingFrom(block).size != 2) throw new SemanticException("This method should never called in this context");
    if(c.initialBlockInLoop(block)) {
      val blocksAfter = c.indexesafterBlockWithouthCurrentBlock(block);
      var result : Option[Int] = None;
      for((i1, i2, b) <- c.entryEdges(block))
        if(b==None && blocksAfter.contains(i1))
          result match {
            case None => result= Some(i1)
            case Some(s) => if(Math.abs(block-i1) > Math.abs(block-s)) result=Some(i1)
              //I need that in order to find "minimal" loops - TODO: there are other ways to do that a lot better!
          }
      result match {
        case Some(s) => return result;
        case None => throw new SemanticException("This should not happen");
      }
    }
    else return None;
  }
}

