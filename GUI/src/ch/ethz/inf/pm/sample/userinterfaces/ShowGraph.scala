package ch.ethz.inf.pm.sample.userinterfaces


import javax.swing.JFrame;
import ch.ethz.inf.pm.sample._;
import com.mxgraph.view._
import com.mxgraph.swing._
import com.mxgraph.model._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.awt.geom.Rectangle2D;
import javax.swing._

private class Show extends JFrame {
  def this(g : JComponent, exitonclose : Boolean, height : Int, width : Int) = {
    this()
    val scrollBar : JScrollPane=new JScrollPane(g,ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
    this.add(scrollBar);
    if(exitonclose) this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    if( (height > 500 || width > 500) || height <= 0 || width <= 0)
	    //(2|4) is a workaround for JFrame.MAXIMIZED_BOTH, since it does not see this final constant field in class Frame
	    this.setExtendedState(this.getExtendedState() | (2|4) )
    else
      this.setSize(width+100, height+100);
	this.setVisible(true);
	this.validate();
  }
  def this(g : mxGraph, exitonclose : Boolean, height : Int, width : Int) = {
    this(new mxGraphComponent(g), exitonclose, height, width)
  }
}

object ShowGraph extends Property
{
	private val ygap : Int = 20;
	private val leftspace : Int = 40;
	private val singleLine : Int = 19;
	private val emptySpace : Int = 20;
  	private val spaceSingleCharacter : Int = 7;
  
  	def getLabel() : String = "Show CFG";
  	def check[S <: State[S]](className : Type, methodName : String, result : ControlFlowGraphExecution[S], printer : OutputCollector) = Show(result);
  	def finalizeChecking() : Unit = Unit;
   	def Show[S <: State[S]](a : Any) : Unit = a match {
   	  case graph: ControlFlowGraphExecution[S] => new ShowControlFlowGraphExecution(graph)
      case graph: ControlFlowGraph => new Show(ShowGraph.ControlFlowGraphJGraph(graph), true, -1, -1);
      case state : S => ShowGraph.stateToGraph(state); 
      case _ => System.out.println("I do not know how to visualize this!")
   	}
   
    def ShowControlFlowGraph(graph : ControlFlowGraph) : Unit = new Show(ShowGraph.ControlFlowGraphJGraph(graph), false, -1, -1);
    
	private def defaultGraphSettings() : mxGraph = {
	  val model : mxGraphModel = new mxGraphModel();
      val graph : mxGraph = new mxGraph(model);
      graph.setAllowDanglingEdges(false);
      graph.setCellsEditable(false)
	  graph
	}
  
	private def createVertix(node : Any, index : Int, x : Double, y : Double, graph : mxGraph, dashed : Boolean, shape : String) : (Object, Double) = { 
			var label : String = null;
			//Workaround: if I check node.isInstanceOf[List[SingleLineRepresentation] it returns always true because of type erasure
			if(node.isInstanceOf[List[Any]]) {
				if(node.asInstanceOf[List[Any]].length > 0 && node.asInstanceOf[List[Any]].apply(0).isInstanceOf[SingleLineRepresentation])
					label = ToStringUtilities.listToNewLineRepresentationSingleLine(node.asInstanceOf[List[SingleLineRepresentation]]);
				else label = ToStringUtilities.listToNewLineRepresentation(node.asInstanceOf[List[Any]])
			}
			else 
				if(node.isInstanceOf[SingleLineRepresentation])
					label = node.asInstanceOf[SingleLineRepresentation].toSingleLineString
				else label = node.toString 
			if(label.equals(""))
				label="#empty node#";
			val w : Double = maxLineLength(label)*spaceSingleCharacter
			val h : Double = singleLine*countLines(label);
   			val cell : Object = if(dashed)
   				graph.insertVertex(graph.getDefaultParent(), index.toString, label, x, y, w, h, "ROUNDED;shape="+shape+";strikeColor=white;fillColor=black;fontColor=white");
            else graph.insertVertex(graph.getDefaultParent(), index.toString, label, x, y, w, h, "ROUNDED;shape="+shape+";strikeColor=black;fillColor=white");
			(cell, h)
    }
   
	private def countLines(s : String ) : Int = {
	  if(s.indexOf('\n')>=0)
		  return countLines(s.substring(s.indexOf('\n')+1))+1
	  else return 1;
	} 
 
 	private def createEdge(from : Int, to : Int, label : Any, vertixes : List[Object], graph : mxGraph) : Unit = { 
		  var l : String = null;
		  if(label.isInstanceOf[SingleLineRepresentation])
		    	l=label.asInstanceOf[SingleLineRepresentation].toSingleLineString;
		  else l=label.toString;
		  if(from < to)
			  graph.insertEdge(graph.getDefaultParent(), "("+from+","+to+")", l, vertixes.apply(from), vertixes.apply(to), "edgeStyle=elbowEdgeStyle");
		  else graph.insertEdge(graph.getDefaultParent(), "("+from+","+to+")", l, vertixes.apply(from), vertixes.apply(to), "edgeStyle=entityRelationEdgeStyle");
    }
 
	private class ShowControlFlowGraphExecution[S <: State[S]] {
	  def this(g : ControlFlowGraphExecution[S]) = {
	    this()
	  	val (graph, vertixes) : (mxGraph, List[Object])= ShowGraph.ControlFlowGraphExecutiontoJGraph[S](g);
	   	val graphComponent : mxGraphComponent = new mxGraphComponent(graph);
	    graphComponent.getGraphControl().addMouseListener(new MouseAdapter()
			{
			
				override def mouseReleased(e : MouseEvent)
				{
					val cell : Object = graphComponent.getCellAt(e.getX(), e.getY());
					if (cell != null)
					{
					  val castedcell = cell.asInstanceOf[mxCell];
					  var i : Int=0;
					  while(i<vertixes.size) {
					    if(vertixes.apply(i)==cell)
					    	new ShowCFGBlock(g.cfg.nodes.apply(i), g.nodes.apply(i));
					    i=i+1;
					  }
					}
				}
			});
	    new Show(graphComponent, false, -1, -1);
	  }
	}
  
 	private class ShowCFGBlock[S <: State[S]] {
	  def this(st : List[Statement], s : List[S]) = {
	    this()
	  	val (graph, vertixes) : (mxGraph, List[Object])= ShowGraph.listToJGraph(s, st);
	   	val graphComponent : mxGraphComponent = new mxGraphComponent(graph);
	    graphComponent.getGraphControl().addMouseListener(new MouseAdapter()
			{
			
				override def mouseReleased(e : MouseEvent)
				{
					val cell : Object = graphComponent.getCellAt(e.getX(), e.getY());
					if (cell != null)
					{
					  val castedcell = cell.asInstanceOf[mxCell];
					  var i : Int = 0;
					  while(i < vertixes.length) {
					    if(cell==vertixes.apply(i)) {
					      ShowGraph.stateToGraph(s.apply(i))
					    }
					    i=i+1;
					  }
					  
					}
				}
			});
	    new Show(graphComponent, false, -1, -1);
	  }
	}

   	private class ShowNonRelationalHeapState[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: NonRelationalHeapIdentifier[I]] {
	  def this(state : GenericAbstractState[N, H, I]) = {
	    this()
     
	    //var addresses : Set[I]= state.getHeap().getAddresses;
	  	val (graph, idToVertix) : (mxGraph, Map[Identifier, Object])= ShowGraph.nonRelationalHeapStateToGraph[I, N](state.getHeap().asInstanceOf[NonRelationalHeapDomain[I]], state.getSemanticDomain());
	   	val graphComponent : mxGraphComponent = new mxGraphComponent(graph);
	    graphComponent.getGraphControl().addMouseListener(new MouseAdapter()
			{
			
				override def mouseReleased(e : MouseEvent)
				{
					val cell : Object = graphComponent.getCellAt(e.getX(), e.getY());
					if (cell != null)
					{
					  val castedcell = cell.asInstanceOf[mxCell];
					  var i : Int = 0;
					  val vertixes = idToVertix.values;
					  for(vertix <- vertixes) {
					    if(cell==vertix) {
					      val ids = idToVertix.keySet
					      for(id <- ids)
					    	  if(idToVertix.apply(id)==cell) {
					    		  val label=state.getStringOfId(id); 
					    		  new Show(new JLabel(label), false, singleLine*countLines(label), maxLineLength(label)*spaceSingleCharacter)
					    	  }
					    }
					    i=i+1;
					  }
					  
					}
				}
			});
	    new Show(graphComponent, false, -1, -1);
	  }
	}
  
    private def ControlFlowGraphExecutiontoJGraph[S <: State[S]](wgraph : ControlFlowGraphExecution[S]) : (mxGraph, List[Object]) = {
	    val graph : mxGraph = defaultGraphSettings();
		var vertixes : List[Object] = Nil;
		var yposition : Double = ygap;
		var xposition : Int = leftspace
		try {
			var index : Int = 0;
			for(node <- wgraph.cfg.nodes) {
				val (vertix, h) = createVertix(node, index, xposition, yposition+ygap, graph, false, "rectangle")
				vertixes=vertixes ::: vertix :: Nil
				yposition=yposition+ygap+h;
				index=index+1;
			}
			for(edge <- wgraph.cfg.edges) 
			  createEdge(edge._1, edge._2, ToStringUtilities.optionToString(edge._3), vertixes, graph)
		}
		finally
		{
			graph.getModel().endUpdate();
		}
		(graph, vertixes)
  }

    
    
  private def ControlFlowGraphJGraph[S <: State[S]](cfg : ControlFlowGraph) : mxGraph = {
	    val graph : mxGraph = defaultGraphSettings();
		var vertixes : List[Object] = Nil;
		var yposition : Double = ygap;
		var xposition : Int = leftspace
		try {
			var index : Int = 0;
			for(node <- cfg.nodes) {
				val (vertix, h) = createVertix(node, index, xposition, yposition+ygap, graph, false, "rectangle")
				vertixes=vertixes ::: vertix :: Nil
				yposition=yposition+ygap+h;
				index=index+1;
			}
			for(edge <- cfg.edges) 
			  createEdge(edge._1, edge._2, ToStringUtilities.optionToString(edge._3), vertixes, graph) 
			
		}
		finally
		{
			graph.getModel().endUpdate();
		}
		graph
  }
    
  private def listToJGraph(list : List[Any], listEdgesLabel : List[Any]) : (mxGraph, List[Object])= {
    val graph : mxGraph = defaultGraphSettings();
	var vertixes : List[Object] = Nil;
		var yposition : Double = ygap;
	try {
		var index : Int = 0;
		for(node <- list) {
			val (vertix, h) = createVertix(toSimplifiedString(node), index, leftspace, yposition+ygap, graph, false, "rectangle")
			vertixes=vertixes ::: vertix :: Nil
			if(index > 0) {
   		  		var l : Any = listEdgesLabel.apply(index-1);
				if(l.isInstanceOf[SingleLineRepresentation])
					l=l.asInstanceOf[SingleLineRepresentation].toSingleLineString;
				else l=l.toString;
				graph.insertEdge(graph.getDefaultParent(), index.toString(), l.toString, vertixes.apply(index-1), vertixes.apply(index), "edgeStyle=elbowEdgeStyle");
			}
			yposition=yposition+ygap*2+h;
			index=index+1;
		}
	}
	finally
	{
		graph.getModel().endUpdate();
	}
   	(graph, vertixes)
  }

  private def toSimplifiedString[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](a : Any) : String = a match {
    case s : GenericAbstractState[N, H, I] => "Click here to see the abstract heap structure\n\nExpression:\n"+s.getExpression  
    case _ => return a.toString();
  }
  
  private def maxLineLength(s : String) : Int = {
    var max : Int = 0;
    var str : String = s;
    while(str.indexOf('\n') != -1 ) {
      if(str.indexOf('\n') > max)
        max=str.indexOf('\n');
      str=str.substring(str.indexOf('\n')+1)
    }
    if(str.length > max)
        max=str.length;
    max
  }
 
  private def nonRelationalHeapStateToGraph[I <: NonRelationalHeapIdentifier[I], N <: SemanticDomain[N]](heap : NonRelationalHeapDomain[I], s : N) : (mxGraph, Map[Identifier, Object]) = {
    val graph : mxGraph = defaultGraphSettings();
	var vertixes : List[Object] = Nil;
	var yposition : Double = ygap;
    var idToVertix : Map[Identifier, Object] = Map.empty[Identifier, Object];
	try {
		var index : Int = 0;
		val variables = heap.getVariables;
		val addresses = 
			if(s.isInstanceOf[AddressedDomain[I]]) 
				s.asInstanceOf[AddressedDomain[I]].getAddresses()++heap.getAddresses 
			else heap.getAddresses;   
  		//Create the nodes for variables
		for(node <- variables) {
			val (vertix, h) = createVertix(node, index, leftspace, yposition+ygap, graph, ! node.representSingleVariable, "ellipse")
			idToVertix+=((node, vertix));
			yposition=yposition+ygap*2+h;
			index=index+1;
		}
		yposition=ygap;
		val xposition : Int = leftspace+200;
		//Create the nodes for abstract addresses
		for(node <- addresses) {
			val (vertix, h) = createVertix(node, index, xposition, yposition+ygap, graph, ! node.representSingleVariable, "ellipse")
			yposition=yposition+ygap*2+h;
			idToVertix+=((node, vertix));
			index=index+1;
		}
		for(variable <- variables) {
			val res : HeapIdAndSetDomain[I] = heap.get(variable);
			val from = idToVertix.apply(variable);
			for(add <- res.value) {
			  val to = idToVertix.apply(add);
			  graph.insertEdge(graph.getDefaultParent(), "("+from+","+to+")", "", from, to, "edgeStyle=elbowEdgeStyle");
			}
		}
		for(add2 <- addresses) {
			val res : HeapIdAndSetDomain[I] = heap.get(heap.cod.convert(add2));
			val from = idToVertix.apply(add2);
			for(add3 <- res.value) {
			  val to = idToVertix.apply(add3);
			  graph.insertEdge(graph.getDefaultParent(), "("+from+","+to+")", "", from, to, "edgeStyle=elbowEdgeStyle");
			}
		}
	}
	finally
	{
		graph.getModel().endUpdate();
	}
	(graph, idToVertix)
  }
  
  
  private def stateToGraph[S <: State[S], N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: NonRelationalHeapIdentifier[I]](state : S) = state match {
    case s : GenericAbstractState[N, H, I] => genericStateToGraph(s); 
    case _ => new Show(stateToString(state), false, -1, -1);
  }
  
  private def genericStateToGraph[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: NonRelationalHeapIdentifier[I]](state : GenericAbstractState[N, H, I]) = state match {
    case _ if state.getHeap().isInstanceOf[NonRelationalHeapDomain[I]] => new ShowNonRelationalHeapState(state.asInstanceOf[GenericAbstractState[N, H,I]]) 
    case _ => new Show(stateToString(state), false, -1, -1);
  }
  
  private def stateToString[S <: State[S]](state : S) : JComponent = {
      var s : String="";
      if(state.isInstanceOf[SingleLineRepresentation])
        s=state.asInstanceOf[SingleLineRepresentation].toSingleLineString ; 
      else s=state.toString;
      new JLabel(s);
  }
}
