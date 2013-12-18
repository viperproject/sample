package ch.ethz.inf.pm.sample.userinterfaces

import graph._
import scala.collection.immutable._
import ch.ethz.inf.pm.sample._
import com.mxgraph.view._
import com.mxgraph.swing._
import com.mxgraph.model._
import oorepresentation._
import abstractdomain._
import abstractdomain.heapanalysis._

import javax.swing._
import java.awt.event._
import tracepartitioning._
import java.awt.{GridLayout, Dimension}
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import scala.Some
import ch.ethz.inf.pm.sample.tracepartitioning.Node
import ch.ethz.inf.pm.sample.abstractdomain.VariableIdentifier
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis.FieldAndProgramPoint
import ch.ethz.inf.pm.sample.tracepartitioning.Leaf

private class Show extends JFrame {
  def this(g: JComponent, exitonclose: Boolean, height: Int, width: Int) = {
    this()
    g.setPreferredSize(new Dimension((g.getPreferredSize.getWidth * 1.1).toInt, (g.getPreferredSize.getHeight * 1.1).toInt));
    val scrollBar: JScrollPane = new JScrollPane(g, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
    this.add(scrollBar);
    if (exitonclose) this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    if (height <= 0 || width <= 0) this.setSize(1800, 1000);
    else this.setSize(width + 100, height + 100);
    this.setVisible(true);
    this.validate();
  }

  def this(g: mxGraph, exitonclose: Boolean, height: Int, width: Int) = {
    this(new mxGraphComponent(g), exitonclose, height, width)
  }
}

object ShowGraph {
  private val ygap: Int = 20;
  private val leftspace: Int = 40;
  private val singleLine: Int = 19;
  private val emptySpace: Int = 20;
  private val spaceSingleCharacter: Int = 8;
  var exitOnClose: Boolean = false;

  def Show[S <: State[S]](a: Any): Unit = a match {
    case results: List[(Type, MethodDeclaration, ControlFlowGraphExecution[S])] => new ShowControlFlowGraphExecutions(results, exitOnClose)
    case graph: ControlFlowGraph => new Show(ShowGraph.ControlFlowGraphJGraph(graph), true, -1, -1);
    case state: S => ShowGraph.stateToGraph(state);
    case _ => System.out.println("I do not know how to visualize this!")
  }

  def ShowControlFlowGraph(graph: ControlFlowGraph): Unit = new Show(ShowGraph.ControlFlowGraphJGraph(graph), false, -1, -1);

  private def defaultGraphSettings(): mxGraph = {
    val model: mxGraphModel = new mxGraphModel();
    val graph: mxGraph = new mxGraph(model);
    graph.setAllowDanglingEdges(false);
    graph.setCellsEditable(false)
    graph
  }

  private def createVertex(node: Any, index: Int, x: Double, y: Double, graph: mxGraph, dashed: Boolean, shape: String): (Object, Double) = {
    var label: String = null;
    //Workaround: if I check node.isInstanceOf[List[SingleLineRepresentation] it returns always true because of type erasure
    if (node.isInstanceOf[List[Any]]) {
      if (node.asInstanceOf[List[Any]].length > 0 && node.asInstanceOf[List[Any]].apply(0).isInstanceOf[SingleLineRepresentation])
        label = ToStringUtilities.listToNewLineRepresentationSingleLine(node.asInstanceOf[List[SingleLineRepresentation]]);
      else label = ToStringUtilities.listToNewLineRepresentation(node.asInstanceOf[List[Any]])
    }
    else
    if (node.isInstanceOf[SingleLineRepresentation])
      label = node.asInstanceOf[SingleLineRepresentation].toSingleLineString
    else label = node.toString
    if (label.equals(""))
      label = "#empty node#"
    val w: Double = maxLineLength(label) * spaceSingleCharacter
    val h: Double = singleLine * countLines(label)
    val cell: Object = if (dashed)
      graph.insertVertex(graph.getDefaultParent(), index.toString, label, x, y, w, h, "ROUNDED;shape=" + shape + ";strikeColor=white;fillColor=black;fontColor=white");
    else graph.insertVertex(graph.getDefaultParent(), index.toString, label, x, y, w, h, "ROUNDED;shape=" + shape + ";strikeColor=black;fillColor=white;fontFamily=Monospaced");
    (cell, h)
  }

  private def countLines(s: String): Int = {
    if (s.indexOf('\n') >= 0)
      return countLines(s.substring(s.indexOf('\n') + 1)) + 1
    else return 1
  }

  private def createEdge(from: Int, to: Int, label: Any, vertixes: List[Object], graph: mxGraph): Unit = {
    var l: String = null
    if (label.isInstanceOf[SingleLineRepresentation])
      l = label.asInstanceOf[SingleLineRepresentation].toSingleLineString;
    else l = label.toString
    if (from < to)
      graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", l, vertixes.apply(from), vertixes.apply(to), "edgeStyle=elbowEdgeStyle");
    else graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", l, vertixes.apply(from), vertixes.apply(to), "edgeStyle=entityRelationEdgeStyle");
  }

  private class ShowControlFlowGraphExecutions[S <: State[S]] {
    def this(gs: List[(Type, MethodDeclaration, ControlFlowGraphExecution[S])], exitOnClose: Boolean) = {
      this()
      val components = for ((c, m, g) <- gs) yield {
        val (graph, vertixes): (mxGraph, List[Object]) = ShowGraph.ControlFlowGraphExecutiontoJGraph[S](g)
        val graphComponent: mxGraphComponent = new mxGraphComponent(graph)
        graphComponent.getGraphControl().addMouseListener(new MouseAdapter() {
          override def mouseReleased(e: MouseEvent) {
            val cell: Object = graphComponent.getCellAt(e.getX(), e.getY())
            if (cell != null) {
              val castedcell = cell.asInstanceOf[mxCell]
              var i: Int = 0
              while (i < vertixes.size) {
                if (vertixes.apply(i) == cell)
                  new ShowCFGBlock(g.cfg.nodes.apply(i), g.nodes.apply(i))
                i = i + 1
              }
            }
          }
        })
        (graphComponent, c.toString()+"."+m.name)
      }

      val tabbedPane = new JTabbedPane()
      for ((component, s) <- components) {
        tabbedPane.addTab(s, null, component, "")
      }
      new Show(tabbedPane, exitOnClose, -1, -1)
    }
  }

  private class ShowCFGBlock[S <: State[S]] {
    def this(st: List[Statement], s: List[S]) = {
      this()
      val (graph, vertixes): (mxGraph, List[Object]) = ShowGraph.listToJGraph(s, st);
      val graphComponent: mxGraphComponent = new mxGraphComponent(graph);
      graphComponent.getGraphControl().addMouseListener(new MouseAdapter() {

        override def mouseReleased(e: MouseEvent) {
          val cell: Object = graphComponent.getCellAt(e.getX(), e.getY());
          if (cell != null) {
            val castedcell = cell.asInstanceOf[mxCell];
            var i: Int = 0;
            while (i < vertixes.length) {
              if (cell == vertixes.apply(i))
                ShowGraph.stateToGraph(s.apply(i))
              i = i + 1;
            }
          }
        }
      });
      new Show(graphComponent, false, -1, -1);
    }
  }

  private class ShowNonRelationalHeapState[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: NonRelationalHeapIdentifier[I]] {
    def this(state: AbstractState[N, H, I]) = {
      this()

      val (graph, idToVertix): (mxGraph, Map[Identifier, Object]) = ShowGraph.nonRelationalHeapStateToGraph[I, N, H](state.getHeapDomain, state.getSemanticDomain);
      val graphComponent: mxGraphComponent = new mxGraphComponent(graph);
      graphComponent.getGraphControl().addMouseListener(new MouseAdapter() {

        override def mouseReleased(e: MouseEvent) {
          val cell: Object = graphComponent.getCellAt(e.getX(), e.getY());
          if (cell != null) {
            val castedcell = cell.asInstanceOf[mxCell];
            var i: Int = 0;
            val vertixes = idToVertix.values;
            for (vertix <- vertixes) {
              if (cell == vertix) {
                val ids = idToVertix.keySet
                for (id <- ids)
                  if (idToVertix.apply(id) == cell) {
                    val label = state.getStringOfId(id);
                    new Show(new JLabel("<HTML>" + label.replace(">", "&gt;").replace("<", "&lt;").replace("\n", "<BR>") + "<HTML>"), false, singleLine * countLines(label), maxLineLength(label) * spaceSingleCharacter)
                  }
              }
              i = i + 1;
            }

          }
        }
      });
      new Show(graphComponent, false, -1, -1);
    }
  }

  private class ShowValueDrivenHeapState[N <: SemanticDomain[N]](state: ValueDrivenHeapState[N]) {
    val (graph, idToVertix, idToEdges): (mxGraph, Map[Vertex, Object], Map[EdgeWithState[N], Object]) = valueDrivenHeapStateToGraph[N](state)
    val graphComponent: mxGraphComponent = new mxGraphComponent(graph);
    graphComponent.getGraphControl().addMouseListener(new MouseAdapter() {

      override def mouseReleased(e: MouseEvent) {
        val cell: Object = graphComponent.getCellAt(e.getX(), e.getY());
        if (cell != null) {
          val castedcell = cell.asInstanceOf[mxCell];
          var i: Int = 0;
          val vertixes = idToVertix.values;
          //          for (vertix <- vertixes) {
          //            if (cell == vertix) {
          //              val ids = idToVertix.keySet
          //              for (id <- ids)
          //                if (idToVertix.apply(id) == cell) {
          //                  val label = state.toString
          //                  new Show(new JLabel("<HTML>" + label.replace(">", "&gt;").replace("<", "&lt;").replace("\n", "<BR>") + "<HTML>"), false, singleLine * countLines(label), maxLineLength(label) * spaceSingleCharacter)
          //                }
          //            }
          //            i = i + 1;
          //          }
          val edges = idToEdges.values
          for (edge <- edges) {
            if (cell == edge) {
              for (id <- idToEdges.keySet) {
                if (idToEdges.apply(id) == cell) {
                  val label = id.state.toString// + "\n" + id
                  new Show(new JLabel("<HTML>" + label.replace(">", "&gt;").replace("<", "&lt;").replace("\n", "<BR>") + "<HTML>"), false, singleLine * countLines(label), maxLineLength(label) * spaceSingleCharacter)
                }
              }
            }
          }
        }
      }
    });
    new Show(graphComponent, false, -1, -1);
  }

  private def ControlFlowGraphExecutiontoJGraph[S <: State[S]](wgraph: ControlFlowGraphExecution[S]): (mxGraph, List[Object]) = {
    val graph: mxGraph = defaultGraphSettings();
    var vertixes: List[Object] = Nil;
    var yposition: Double = ygap;
    var xposition: Int = leftspace
    try {
      var index: Int = 0;
      for (node <- wgraph.cfg.nodes) {
        val (vertix, h) = createVertex(node, index, xposition, yposition + ygap, graph, false, "rectangle")
        vertixes = vertixes ::: vertix :: Nil
        yposition = yposition + ygap + h;
        index = index + 1;
      }
      for (edge <- wgraph.cfg.edges)
        createEdge(edge._1, edge._2, ToStringUtilities.optionToString(edge._3), vertixes, graph)
      new mxHierarchicalLayout(graph).execute(graph.getDefaultParent())
    }
    finally {
      graph.getModel().endUpdate();
    }
    (graph, vertixes)
  }


  private def ControlFlowGraphJGraph[S <: State[S]](cfg: ControlFlowGraph): mxGraph = {
    val graph: mxGraph = defaultGraphSettings();
    var vertixes: List[Object] = Nil;
    var yposition: Double = ygap;
    var xposition: Int = leftspace
    try {
      var index: Int = 0;
      for (node <- cfg.nodes) {
        val (vertix, h) = createVertex(node, index, xposition, yposition + ygap, graph, false, "rectangle")
        vertixes = vertixes ::: vertix :: Nil
        yposition = yposition + ygap + h;
        index = index + 1;
      }
      for (edge <- cfg.edges)
        createEdge(edge._1, edge._2, ToStringUtilities.optionToString(edge._3), vertixes, graph)
      new mxHierarchicalLayout(graph).execute(graph.getDefaultParent())
    }
    finally {
      graph.getModel().endUpdate();
    }
    graph
  }

  private def listToJGraph(list: List[Any], listEdgesLabel: List[Any]): (mxGraph, List[Object]) = {
    val graph: mxGraph = defaultGraphSettings();
    var vertixes: List[Object] = Nil;
    var yposition: Double = ygap;
    try {
      var index: Int = 0;
      for (node <- list) {
        val (vertix, h) = createVertex(toSimplifiedString(node), index, leftspace, yposition + ygap, graph, false, "rectangle")
        vertixes = vertixes ::: vertix :: Nil
        if (index > 0) {
          var l: Any = listEdgesLabel.apply(index - 1);
          if (l.isInstanceOf[SingleLineRepresentation])
            l = l.asInstanceOf[SingleLineRepresentation].toSingleLineString;
          else l = l.toString;
          graph.insertEdge(graph.getDefaultParent(), index.toString(), l.toString, vertixes.apply(index - 1), vertixes.apply(index), "edgeStyle=elbowEdgeStyle");
        }
        yposition = yposition + ygap * 2 + h;
        index = index + 1;
      }
      new mxHierarchicalLayout(graph).execute(graph.getDefaultParent())
    }
    finally {
      graph.getModel().endUpdate();
    }
    (graph, vertixes)
  }

  private def toSimplifiedString[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]](a: Any): String = a match {
    case s: AbstractState[N, H, I] => "Click here to see the abstract heap structure\n\nExpression:\n" + s._2
    case _ => return a.toString();
  }

  private def maxLineLength(s: String): Int = {
    var max: Int = 0;
    var str: String = s;
    while (str.indexOf('\n') != -1) {
      if (str.indexOf('\n') > max)
        max = str.indexOf('\n');
      str = str.substring(str.indexOf('\n') + 1)
    }
    if (str.length > max)
      max = str.length;
    max
  }

  private def nonRelationalHeapStateToGraph[I <: NonRelationalHeapIdentifier[I], N <: SemanticDomain[N], H <: HeapDomain[H, I]](heap: H, s: N): (mxGraph, Map[Identifier, Object]) = {
    val graph: mxGraph = defaultGraphSettings()
    var vertixes: List[Object] = Nil
    var yposition: Double = ygap
    var idToVertix: Map[Identifier, Object] = Map.empty[Identifier, Object]
    try {
      var index: Int = 0
      val sIds = s.getIds()
      val heapIds = heap.getIds()
      val ids = s.getIds() ++ heap.getIds()

      //Create the nodes for variables
      for (node <- ids) {
        if (node.isInstanceOf[VariableIdentifier]) {
          val (vertix, h) = createVertex(node, index, leftspace, yposition + ygap, graph, !node.representsSingleVariable, "ellipse")
          idToVertix += ((node, vertix))
          yposition = yposition + ygap * 2 + h
          index = index + 1
        }
      }
      yposition = ygap
      val xposition: Int = leftspace + 200

      // Create the nodes for abstract addresses
      for (node <- ids) {
        if (!node.isInstanceOf[VariableIdentifier]) {
          val (vertix, h) = createVertex(node, index, xposition, yposition + ygap, graph, !node.representsSingleVariable, "ellipse")
          yposition = yposition + ygap * 2 + h
          idToVertix += ((node, vertix))
          index = index + 1
        }
      }

      // Create edges between heap ids
      for (id <- ids) {
        id match {

          case v:VariableIdentifier =>
            val res: HeapIdSetDomain[I] = heap.get(v)
            val from = idToVertix.apply(v)
            for (add <- res.value) {
              val to = idToVertix.apply(add)
              graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", "", from, to, "edgeStyle=elbowEdgeStyle");
            }

          case _ =>
            val res: HeapIdSetDomain[I] = heap.get(id.asInstanceOf[I])
            val from = idToVertix.apply(id)
            for (add <- res.value) {
              val to = idToVertix.apply(add)
              graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", "", from, to, "edgeStyle=elbowEdgeStyle");
            }

        }
      }

      // Special handling for fields.
      for (id <- ids) {

        id match {

          case FieldAndProgramPoint(p1,field,_, _) =>
            //if (idToVertix.contains(p1)){
              val from = idToVertix.apply(p1)
              val to = idToVertix.apply(id)
              graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", "", from, to, "edgeStyle=elbowEdgeStyle;dashed=true");
            //}
          case _ => ()

        }

      }

      val a = new java.util.LinkedList[Object]
      for (id <- ids) {
        val vertex = idToVertix(id)
        if(graph.getIncomingEdges(vertex).isEmpty) {
          a.add(vertex)
        }
      }

      // Layout the graph
      val layout = new mxHierarchicalLayout(graph,SwingConstants.WEST)
      layout.execute(graph.getDefaultParent(),a)
    }
    finally {
      graph.getModel().endUpdate()
    }
    (graph, idToVertix)
  }

  private def valueDrivenHeapStateToGraph[N <: SemanticDomain[N]](state: ValueDrivenHeapState[N]) = {
    val graph: mxGraph = defaultGraphSettings()
    var yposition: Double = ygap
    var idToVertix = Map.empty[Vertex, Object]
    var idToEdge = Map.empty[EdgeWithState[N], Object]

    try {
      var index: Int = 0
      //      val ids = s.getIds() ++ heap.getIds()
      val vertices = state.abstractHeap.vertices
      //Create the nodes for variables
      for (node <- vertices) {
        if (node.isInstanceOf[LocalVariableVertex]) {
          val (vertex, h) = createVertex(node.name, index, leftspace, yposition + ygap, graph, !node.label.equals(VertexConstants.DEFINITE), "rectangle")
          idToVertix += ((node, vertex))
          yposition = yposition + ygap * 4 + h
          index = index + 1
        }
      }
      yposition = ygap
      val xposition: Int = leftspace + 200

      // Create the nodes for abstract addresses
      for (node <- vertices) {
        if (!node.isInstanceOf[LocalVariableVertex]) {
          val (vertix, h) = createVertex(node.name, index, xposition, yposition + ygap, graph, !node.label.equals(VertexConstants.DEFINITE), "ellipse")
          yposition = yposition + ygap * 4 + h
          idToVertix += ((node, vertix))
          index = index + 1
        }
      }

      for (edge <- state.abstractHeap.edges) {
        val from = idToVertix.apply(edge.source)
        val to = idToVertix.apply(edge.target)

        val addedEdge = graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + "," + edge.field + ")" + index, edge.field, from, to, "edgeStyle=elbowEdgeStyle")
        index = index + 1
        idToEdge += ((edge, addedEdge))
      }

      val a = new java.util.LinkedList[Object]
      for (id <- vertices) {
        val vertex = idToVertix(id)
        if(graph.getIncomingEdges(vertex).isEmpty) {
          a.add(vertex)
        }
      }

      // Layout the graph
      val layout = new mxHierarchicalLayout(graph,SwingConstants.WEST)
      layout.execute(graph.getDefaultParent(),a)
    }
    finally {
      graph.getModel().endUpdate()
    }
    (graph, idToVertix, idToEdge)
  }

  private def stateToGraph[S <: State[S], N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: NonRelationalHeapIdentifier[I]](state: S) = state match {
    case s: AbstractState[N, H, I] => genericStateToGraph(s)
    case s: PartitionedState[_] => partitionedStateToJComponent(s)
    case s: ValueDrivenHeapState[N] => new ShowValueDrivenHeapState[N](s)
    case _ => new Show(stateToString(state), false, -1, -1)
  }

  private def genericStateToGraph[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: NonRelationalHeapIdentifier[I]](state: AbstractState[N, H, I]) = state match {
    case _ if state.getHeapDomain.isInstanceOf[NonRelationalMayAndMustHeapDomain[I]] => new ShowNonRelationalHeapState(state.asInstanceOf[AbstractState[N, H, I]])
    case _ if state.getHeapDomain.isInstanceOf[NonRelationalHeapDomain[I]] => new ShowNonRelationalHeapState(state.asInstanceOf[AbstractState[N, H, I]])
    case _ if state.getHeapDomain.isInstanceOf[NonRelationalSummaryCollectionHeapDomain[I]] => new ShowNonRelationalHeapState(state)
    //case _ if state.getHeap().isInstanceOf[ArrayHeapDomain] => new ShowArrayAnalysisHeapState(state.asInstanceOf[GenericAbstractState[N, ArrayHeapDomain, ArrayHeapID]], false, null)
    //case _ if state.getHeap().isInstanceOf[TVSHeap] => new ShowTVSHeapState(state.asInstanceOf[GenericAbstractState[N, H, I]])
    case _ => new Show(stateToString(state), false, -1, -1)
  }


  private def partitionedStateToJComponent[S <: State[S]](state: PartitionedState[S]) = new ShowPartitionedState(state)

  private class ShowPartitionedState[S <: State[S]] {
    def this(state: PartitionedState[S]) = {
      this()

      val leaves: List[(List[Directive[S]], Option[S], String)] = flatPartitioning(state.partitioning, Nil)
      var result: JPanel = new JPanel()
      result.setLayout(new GridLayout(leaves.size, 1))
      for ((d, l, s) <- leaves) {
        var b: JButton = new JButton("Directive:" + d.mkString(", "))
        b.addActionListener(new ActionListener() {
          def actionPerformed(e: ActionEvent) {
            {
              if (e.getSource() == b)
                if (l != None)
                  stateToGraph(l.get)
                else new Show(new JLabel(s), false, -1, -1)
            }
          }
        })
        result.add(b)
      }
      new Show(result, false, -1, -1)
    }

    private def flatPartitioning[S <: State[S]](p: Partitioning[S], l: List[Directive[S]]): List[(List[Directive[S]], Option[S], String)] = p match {
      case Leaf(s) => (l, Some(s), "") :: Nil;
      case Node(d, children) =>
        var result: List[(List[Directive[S]], Option[S], String)] = Nil
        for (c <- children)
          result = result ::: flatPartitioning(c, l ::: d :: Nil)
        return result
      case x: Supremum[S] => (l, None, x.toString()) :: Nil
    }
  }

  private def stateToString[S <: State[S]](state: S): JComponent = {
    var s: String = ""
    if (state.isInstanceOf[SingleLineRepresentation])
      s = state.asInstanceOf[SingleLineRepresentation].toSingleLineString;
    else s = state.toString
    new JLabel(s)
  }

}