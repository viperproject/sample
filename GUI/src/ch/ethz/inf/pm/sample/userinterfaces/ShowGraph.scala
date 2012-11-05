package ch.ethz.inf.pm.sample.userinterfaces


import ch.ethz.inf.pm.sample._;
import com.mxgraph.view._
import com.mxgraph.swing._
import com.mxgraph.model._
import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import java.awt.geom.Rectangle2D;
import javax.swing._
import java.awt.event._
import tracepartitioning._
import com.mxgraph.util.mxConstants
import java.awt.{Color, GridLayout, Dimension, Toolkit}
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import ch.ethz.inf.pm.td.domain.EnvironmentIdentifier


private class Show extends JFrame {
	def this(g: JComponent, exitonclose: Boolean, height: Int, width: Int) = {
		this ()
		g.setPreferredSize(new Dimension((g.getPreferredSize.getWidth * 1.1).toInt, (g.getPreferredSize.getHeight * 1.1).toInt));
		val scrollBar: JScrollPane = new JScrollPane(g, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
		this.add(scrollBar);
		if (exitonclose) this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		//if( (height > 500 || width > 500) || height <= 0 || width <= 0)
		//(2|4) is a workaround for JFrame.MAXIMIZED_BOTH, since it does not see this final constant field in class Frame
		//  this.setExtendedState(this.getExtendedState() | (2|4) )
		//else
		if (height <= 0 || width <= 0) this.setSize(1800, 1000);
		else this.setSize(width + 100, height + 100);
		this.setVisible(true);
		this.validate();
	}

	def this(g: mxGraph, exitonclose: Boolean, height: Int, width: Int) = {
		this (new mxGraphComponent(g), exitonclose, height, width)
	}
}

object ShowGraph extends Property {
	private val ygap: Int = 20;
	private val leftspace: Int = 40;
	private val singleLine: Int = 19;
	private val emptySpace: Int = 20;
	private val spaceSingleCharacter: Int = 8;
  var exitOnClose : Boolean=false;

	def getLabel(): String = "Show CFG";

	def check[S <: State[S]](className: Type, methodName: String, result: ControlFlowGraphExecution[S], printer: OutputCollector) = Show(result);

	def finalizeChecking(printer: OutputCollector): Unit = Unit;

	def Show[S <: State[S]](a: Any): Unit = a match {
		case graph: ControlFlowGraphExecution[S] => new ShowControlFlowGraphExecution(graph, exitOnClose)
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

	private def createVertix(node: Any, index: Int, x: Double, y: Double, graph: mxGraph, dashed: Boolean, shape: String): (Object, Double) = {
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
			label = "#empty node#";
		val w: Double = maxLineLength(label) * spaceSingleCharacter
		val h: Double = singleLine * countLines(label);
		val cell: Object = if (dashed)
			graph.insertVertex(graph.getDefaultParent(), index.toString, label, x, y, w, h, "ROUNDED;shape=" + shape + ";strikeColor=white;fillColor=black;fontColor=white");
		else graph.insertVertex(graph.getDefaultParent(), index.toString, label, x, y, w, h, "ROUNDED;shape=" + shape + ";strikeColor=black;fillColor=white;fontFamily=Monospaced");
		(cell, h)
	}

	private def countLines(s: String): Int = {
		if (s.indexOf('\n') >= 0)
			return countLines(s.substring(s.indexOf('\n') + 1)) + 1
		else return 1;
	}

	private def createEdge(from: Int, to: Int, label: Any, vertixes: List[Object], graph: mxGraph): Unit = {
		var l: String = null;
		if (label.isInstanceOf[SingleLineRepresentation])
			l = label.asInstanceOf[SingleLineRepresentation].toSingleLineString;
		else l = label.toString;
		if (from < to)
			graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", l, vertixes.apply(from), vertixes.apply(to), "edgeStyle=elbowEdgeStyle");
		else graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", l, vertixes.apply(from), vertixes.apply(to), "edgeStyle=entityRelationEdgeStyle");
	}

	private class ShowControlFlowGraphExecution[S <: State[S]] {
		def this(g: ControlFlowGraphExecution[S], exitOnClose : Boolean) = {
			this ()
			val (graph, vertixes): (mxGraph, List[Object]) = ShowGraph.ControlFlowGraphExecutiontoJGraph[S](g);
			val graphComponent: mxGraphComponent = new mxGraphComponent(graph);
			graphComponent.getGraphControl().addMouseListener(new MouseAdapter() {

				override def mouseReleased(e: MouseEvent) {
					val cell: Object = graphComponent.getCellAt(e.getX(), e.getY());
					if (cell != null) {
						val castedcell = cell.asInstanceOf[mxCell];
						var i: Int = 0;
						while (i < vertixes.size) {
							if (vertixes.apply(i) == cell)
								new ShowCFGBlock(g.cfg.nodes.apply(i), g.nodes.apply(i));
							i = i + 1;
						}
					}
				}
			});
			new Show(graphComponent, exitOnClose, -1, -1);
		}
	}

	private class ShowCFGBlock[S <: State[S]] {
		def this(st: List[Statement], s: List[S]) = {
			this ()
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
			this ()

			val (graph, idToVertix): (mxGraph, Map[Identifier, Object]) = ShowGraph.nonRelationalHeapStateToGraph[I, N](state.getHeapDomain().asInstanceOf[NonRelationalHeapDomain[I]], state.getSemanticDomain());
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
										new Show(new JLabel("<HTML>"+label.replace("\n","<BR>")+"<HTML>"), false, singleLine * countLines(label), maxLineLength(label) * spaceSingleCharacter)
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

  /*private class ShowArrayAnalysisHeapState[N <: SemanticDomain[N]] {
		def this(state: GenericAbstractState[N, ArrayHeapDomain, ArrayHeapID], showSingleArr: Boolean, heapId: ArrayHeapID) = {
			this ()

      try {
        if (!showSingleArr){
          val (graph, idToVertix): (mxGraph, Map[Identifier, Object]) = ShowGraph.arrayHeapStateToGraph[N](state.getHeap().asInstanceOf[ArrayHeapDomain], state.getSemanticDomain());
          val graphComponent: mxGraphComponent = new mxGraphComponent(graph);
          graphComponent.getGraphControl().addMouseListener(new MouseAdapter() {

            override def mouseReleased(e: MouseEvent) {
              val cell: Object = graphComponent.getCellAt(e.getX(), e.getY());
              if (cell != null) {
                val vertixes = idToVertix.values;
                for (vertix <- vertixes) {
                  if (cell == vertix) {
                    val ids = idToVertix.keySet
                    for (id <- ids)
                      if (idToVertix.apply(id) == cell) {
                        if (id.isInstanceOf[ArrayHeapID] && id.asInstanceOf[ArrayHeapID].n == 0) {
                          val label = state.getHeap().store.apply(id.asInstanceOf[ArrayHeapID]).toString()
  //                        new Show(new JLabel(label), false, singleLine * countLines(label), maxLineLength(label) * spaceSingleCharacter)
                          new ShowArrayAnalysisHeapState(state, true, id.asInstanceOf[ArrayHeapID])
                        }
                      }
                  }
                }

              }
            }
          });
          new Show(graphComponent, false, -1, -1);
        } else {
          val (graph, idToVertix, boundsToVertex): (mxGraph, Map[Identifier, Object], Map[MySegmentBounds, Object]) = ShowGraph.arraySingleArrayToGraph[N](state.getHeap().asInstanceOf[ArrayHeapDomain], state.getSemanticDomain(), heapId);
          val graphComponent: mxGraphComponent = new mxGraphComponent(graph);
          graphComponent.getGraphControl().addMouseListener(new MouseAdapter() {

            override def mouseReleased(e: MouseEvent) {
              val cell: Object = graphComponent.getCellAt(e.getX(), e.getY());
              if (cell != null) {
                val vertixes = idToVertix.values ++ boundsToVertex.values;
                for (vertix <- vertixes) {
                  if (cell == vertix) {
                    val ids = idToVertix.keySet
                    val bounds = boundsToVertex.keySet
                    for (id <- ids) {
                      if (idToVertix.apply(id) == cell) {
                        if (id.isInstanceOf[ArrayHeapID] && id.asInstanceOf[ArrayHeapID].n > 0) {
//                          val label = state.getSemanticDomainState().getStringOfId(id)
                          val label = state.getSemanticDomain().getStringOfId(id)
                          new Show(new JLabel(label), false, singleLine * countLines(label), maxLineLength(label) * spaceSingleCharacter)
                        }
                      }
                    }
                    for (bound <- bounds)  {
                      if (boundsToVertex.apply(bound) == cell) {
                        if (bound.isInstanceOf[MySegmentBounds]) {
//                          val label = state.getSemanticDomainState().getStringOfId(id)
                          var seenIds = Set.empty[Identifier]
                          for (exp <- bound.getExpressions()) {
                            for (expId <- Normalizer.getIdsForExpression(exp)) {
                              seenIds += expId
                            }
                          }
                          var label: String = "";
                          for (expId <- seenIds) {
                            label += "\n" + state.getSemanticDomain().getStringOfId(expId)
                          }
                          new Show(new JLabel(label), false, singleLine * countLines(label), maxLineLength(label) * spaceSingleCharacter)
                        }
                      }
                    }
                  }
                }

              }
            }
          });
          new Show(graphComponent, false, -1, -1);
        }
      } catch {
        case e: Exception => new Show(stateToString(state), false, -1, -1)
      }

		}
	}

           */

	private def ControlFlowGraphExecutiontoJGraph[S <: State[S]](wgraph: ControlFlowGraphExecution[S]): (mxGraph, List[Object]) = {
		val graph: mxGraph = defaultGraphSettings();
		var vertixes: List[Object] = Nil;
		var yposition: Double = ygap;
		var xposition: Int = leftspace
		try {
			var index: Int = 0;
			for (node <- wgraph.cfg.nodes) {
				val (vertix, h) = createVertix(node, index, xposition, yposition + ygap, graph, false, "rectangle")
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
				val (vertix, h) = createVertix(node, index, xposition, yposition + ygap, graph, false, "rectangle")
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
				val (vertix, h) = createVertix(toSimplifiedString(node), index, leftspace, yposition + ygap, graph, false, "rectangle")
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

	private def nonRelationalHeapStateToGraph[I <: NonRelationalHeapIdentifier[I], N <: SemanticDomain[N]](heap: NonRelationalHeapDomain[I], s: N): (mxGraph, Map[Identifier, Object]) = {
		val graph: mxGraph = defaultGraphSettings();
		var vertixes: List[Object] = Nil;
		var yposition: Double = ygap;
		var idToVertix: Map[Identifier, Object] = Map.empty[Identifier, Object];
		try {
			var index: Int = 0;
			val ids = s.getIds() ++ heap.getIds()
			//Create the nodes for variables
			for (node <- ids) {
				if (node.isInstanceOf[VariableIdentifier]) {
					val (vertix, h) = createVertix(node, index, leftspace, yposition + ygap, graph, !node.representSingleVariable, "ellipse")
					idToVertix += ((node, vertix));
					yposition = yposition + ygap * 2 + h;
					index = index + 1;
				}
			}
			yposition = ygap;
			val xposition: Int = leftspace + 200;
			//Create the nodes for abstract addresses
			for (node <- ids) {
				if (!node.isInstanceOf[VariableIdentifier]) {
					val (vertix, h) = createVertix(node, index, xposition, yposition + ygap, graph, !node.representSingleVariable, "ellipse")
					yposition = yposition + ygap * 2 + h;
					idToVertix += ((node, vertix));
					index = index + 1;
				}
			}
			for (variable <- ids) {
				if (variable.isInstanceOf[VariableIdentifier]) {
					val res: HeapIdSetDomain[I] = heap.get(variable.asInstanceOf[VariableIdentifier]);
					val from = idToVertix.apply(variable);
					for (add <- res.value) {
						val to = idToVertix.apply(add);
						graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", "", from, to, "edgeStyle=elbowEdgeStyle");
					}
				}
			}
			for (add2 <- ids) {
				if (!add2.isInstanceOf[VariableIdentifier]) {
					val res: HeapIdSetDomain[I] = heap.get(add2.asInstanceOf[I]);
					val from = idToVertix.apply(add2);
					for (add3 <- res.value) {
						val to = idToVertix.apply(add3);
						graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", "", from, to, "edgeStyle=elbowEdgeStyle");
					}
				}
			}
		}
		finally {
			graph.getModel().endUpdate();
		}
		(graph, idToVertix)
	}

//  private def arrayHeapVariablesToGraph[N <: SemanticDomain[N]]

  /*private def arrayHeapStateToGraph[N <: SemanticDomain[N]](heap: ArrayHeapDomain, s: N): (mxGraph, Map[Identifier, Object]) = {
		val graph: mxGraph = defaultGraphSettings();
		var vertixes: List[Object] = Nil;
		var yposition: Double = ygap;
		var idToVertix: Map[Identifier, Object] = Map.empty[Identifier, Object];
		try {
			var index: Int = 0;
			val ids = s.getIds() ++ heap.getVarIds()
			//Create the nodes for variables
			for (node <- ids) {
				if (node.isInstanceOf[VariableIdentifier]) {
					val (vertix, h) = createVertix(node, index, leftspace, yposition + ygap, graph, !node.representSingleVariable, "ellipse")
					idToVertix += ((node, vertix));
					yposition = yposition + ygap * 2 + h;
					index = index + 1;
				}
			}
			yposition = ygap;
			val xposition: Int = leftspace + 200;
			//Create the nodes for abstract addresses
			for (node <- ids) {
				if (!node.isInstanceOf[VariableIdentifier]) {
					val (vertix, h) = createVertix(node, index, xposition, yposition + ygap, graph, !node.representSingleVariable, "ellipse")
					yposition = yposition + ygap * 2 + h;
					idToVertix += ((node, vertix));
					index = index + 1;
				}
			}
			for (variable <- ids) {
				if (variable.isInstanceOf[VariableIdentifier]) {
					val res: HeapIdSetDomain[ArrayHeapID] = heap.get(variable.asInstanceOf[VariableIdentifier]);
					val from = idToVertix.apply(variable);
					for (add <- res.value) {
//						val to = heap.store.apply(add);
            if (!idToVertix.keySet.contains(add)) {
              val (vertix, h) = createVertix(add, index, xposition, yposition + ygap, graph, !add.representSingleVariable, "ellipse")
              yposition = yposition + ygap * 2 + h;
              idToVertix += ((add, vertix));
              index = index + 1;
            }
            graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + idToVertix.apply(add)  + ")", "", from, idToVertix.apply(add), "edgeStyle=elbowEdgeStyle");
					}
				}
			}
			for (add2 <- ids) {
//				if (!add2.isInstanceOf[VariableIdentifier]) {
//					val res: HeapIdSetDomain[ArrayHeapID] = heap.get(add2.asInstanceOf[ArrayHeapID]);
//					val from = idToVertix.apply(add2);
//					for (add3 <- res.value) {
//						val to = idToVertix.apply(add3);
//						graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", "", from, to, "edgeStyle=elbowEdgeStyle");
//					}
//				}
			}
		}
		finally {
			graph.getModel().endUpdate();
		}
		return (graph, idToVertix)
	}
  private def arraySingleArrayToGraph[N <: SemanticDomain[N]](heap: ArrayHeapDomain, s: N, heapId: ArrayHeapID): (mxGraph, Map[Identifier, Object], Map[MySegmentBounds, Object]) = {
		val graph: mxGraph = defaultGraphSettings();
    var xposition: Double = leftspace;
		var idToVertix: Map[Identifier, Object] = Map.empty[Identifier, Object];
    var boundToVertex: Map[MySegmentBounds, Object] = Map.empty[MySegmentBounds, Object]
		try {

      heap.store.get(heapId) match {
        case None => {
          throw new Exception("Heap ID does not have any single array in store.")
        }
        case Some(sa) => {
          // We render the single array representation.
          var index = 0;
          val boundsIt = sa.bounds.iterator
          val idsIt = sa.ids.iterator
          while (boundsIt.hasNext || idsIt.hasNext) {
            if (boundsIt.hasNext) {
              val bound = boundsIt.next()
              val label = bound.toString()
              if (bound.hasQuestionmark()) {
                val (vertix, h) = createVertix(bound, index, xposition, ygap, graph, false, mxConstants.STYLE_DASHED)
                boundToVertex += ((bound, vertix));
            } else {
                val (vertix, h) = createVertix(bound, index, xposition, ygap, graph, false, mxConstants.SHAPE_RECTANGLE)
                boundToVertex += ((bound, vertix));
              }
              xposition = xposition + maxLineLength(label) * spaceSingleCharacter + 5
              index = index + 1;
            }
            if (idsIt.hasNext) {
              val id = idsIt.next()
              val label = id.toString()
              val (vertix, h) = createVertix(id, index, xposition, ygap, graph, false, "ellipse")
              idToVertix += ((id, vertix))
              xposition = xposition + maxLineLength(label) * spaceSingleCharacter + 5
              index = index + 1;
            }
          }
        }
      }
		}
		finally {
			graph.getModel().endUpdate();
		}
		return (graph, idToVertix, boundToVertex)
	}


  /**
   * ShowTVSHeapState displays a graphical representation of the TVSHeap domain's state.
   * For every three-valued structure it draws a graph using jgraphx
   *
   * @author: Raphael Fuchs
   */
  private class ShowTVSHeapState[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]] {
    def this(state: GenericAbstractState[N, H, I]) = {
      this ()

      val heap = state.getHeap().asInstanceOf[TVSHeap]
      val b: Box = new Box(BoxLayout.Y_AXIS)

      for (struct <- heap.structures) {
        val (graph, idToVertex): (mxGraph, Map[Identifier, Object]) = TVSStructureToGraph(heap, struct, state.getSemanticDomain());
        val graphComponent: mxGraphComponent = new mxGraphComponent(graph);
        graphComponent.getGraphControl.addMouseListener(new ClickIdentifierListener(graphComponent, state, idToVertex))
        graphComponent.setBorder(BorderFactory.createLineBorder(Color.black))
        b.add(graphComponent)
      }

      new Show(b, false, -1, -1)
    }


    /**
     * Listener for clicking on the identifiers in the heap graph. Brings up a small windows with the associated
     * semantic state.
     */
      private class ClickIdentifierListener[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: HeapIdentifier[I]]
                                                    (graph: mxGraphComponent,
                                                     state: GenericAbstractState[N, H, I],
                                                     idToVertix: Map[Identifier, Object]) extends MouseAdapter {

        override def mouseReleased(e: MouseEvent) = {
          val cell: Object = graph.getCellAt(e.getX(), e.getY());
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
                    new Show(new JLabel(label), false, singleLine * countLines(label), maxLineLength(label) * spaceSingleCharacter)
                  }
              }
              i = i + 1;
            }

          }
        }
      }

    /**
     *  TVSStructureToGraph visualizes one particular three-valued structure (tvs) as a graph
     */
    private def TVSStructureToGraph[N <: SemanticDomain[N],T <: NodeName](heap: TVSHeap, tvs: TVS[T], s: N): (mxGraph, Map[Identifier, Object]) = {
      val graph: mxGraph = defaultGraphSettings()
      var idToVertix: Map[Identifier, Object] = Map.empty[Identifier, Object];
      var xposition: Int = leftspace
      var yposition: Int = ygap
      var index: Int = 0;

      // program variables
      val numericalVars = s.getIds().collect {case v: VariableIdentifier if(!v.typ.isObject) => v }
      for (v <- (heap.variables ++ heap.tempVariables ++ numericalVars)) {
          val label = v.toString()
          val w: Double = 1.5 * maxLineLength(label) * spaceSingleCharacter
          val h: Double = 1.5 * singleLine * countLines(label);
          val cell = graph.insertVertex(graph.getDefaultParent(), index.toString, label, xposition, yposition, w, h,
            "shape=rectangle;fillColor=#FFFFFF;strokeWidth=1;strokeColor=#000000");
          yposition = yposition + ygap * 2 + h.toInt;
          idToVertix += ((v, cell));
          index = index + 1;
      }
      xposition += 200
      yposition = ygap

      // heap nodes
      for (node <- tvs.nodes) {
          val label = node.toString
          val w: Double = 1.5 * maxLineLength(label) * spaceSingleCharacter
          val h: Double = 1.5 * singleLine * countLines(label);
          var style = "shape=ellipse;fillColor=#FFFFFF;strokeWidth=1;strokeColor=#000000"
          if (tvs.summarization.values.contains(node))
            style += ";dashed=1"
          val cell = graph.insertVertex(graph.getDefaultParent(), index.toString, label, xposition, yposition, w, h, style);
          yposition = yposition + ygap * 2 + h.toInt;
          idToVertix += ((node, cell));
          index = index + 1;
      }

      // edges from program variables
      for (v <- (heap.variables ++ heap.tempVariables)) {
        val from = idToVertix.apply(v)
        tvs.programVariables(v.toString).value match {
          case Some(n) =>
            val to = idToVertix(n)
            graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", "", from, to, "edgeStyle=elbowEdgeStyle");
          case None =>
        }
       }

      // edges between nodes (field references)
      for ((fname,fp) <- tvs.fields) {
        for ((l, (r, truth)) <- fp.values) {
          val from = idToVertix(l)
          val to = idToVertix(r)
          val decodedFieldName = fname.split('_')
          val fieldlabel = if (decodedFieldName.size == 2) decodedFieldName(1) + " field" else fname
          var style = "edgeStyle=elbowEdgeStyle"
          if (truth == Kleene.Unknown)
            style += ";dashed=1"
          graph.insertEdge(graph.getDefaultParent(), "(" + from + "," + to + ")", fieldlabel, from, to, style);
        }
       }

      graph.getModel().endUpdate();
      (graph, idToVertix)
    }
  }
   */



	private def stateToGraph[S <: State[S], N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: NonRelationalHeapIdentifier[I]](state: S) = state match {
		case s: AbstractState[N, H, I] => genericStateToGraph(s);
		case s: PartitionedState[_] => partitionedStateToJComponent(s)
		case _ => new Show(stateToString(state), false, -1, -1);
	}

	private def genericStateToGraph[N <: SemanticDomain[N], H <: HeapDomain[H, I], I <: NonRelationalHeapIdentifier[I]](state: AbstractState[N, H, I]) = state match {
		case _ if state.getHeapDomain().isInstanceOf[NonRelationalHeapDomain[I]] => new ShowNonRelationalHeapState(state.asInstanceOf[AbstractState[N, H, I]])
		//case _ if state.getHeap().isInstanceOf[ArrayHeapDomain] => new ShowArrayAnalysisHeapState(state.asInstanceOf[GenericAbstractState[N, ArrayHeapDomain, ArrayHeapID]], false, null)
    //case _ if state.getHeap().isInstanceOf[TVSHeap] => new ShowTVSHeapState(state.asInstanceOf[GenericAbstractState[N, H, I]])
		case _ => new Show(stateToString(state), false, -1, -1);
	}


	private def partitionedStateToJComponent[S <: State[S]](state: PartitionedState[S]) = new ShowPartitionedState(state)

	private class ShowPartitionedState[S <: State[S]] {
		def this(state: PartitionedState[S]) = {
			this ()

			val leaves: List[(List[Directive[S]], Option[S], String)] = flatPartitioning(state.partitioning, Nil);
			var result: JPanel = new JPanel();
			result.setLayout(new GridLayout(leaves.size, 1));
			for ((d, l, s) <- leaves) {
				var b: JButton = new JButton("Directive:" + d.mkString(", "));
				b.addActionListener(new ActionListener() {
					def actionPerformed(e: ActionEvent) {
						{
							if (e.getSource() == b)
								if (l != None)
									stateToGraph(l.get);
								else new Show(new JLabel(s), false, -1, -1);
						}
					}
				});
				result.add(b);
			}
			new Show(result, false, -1, -1);
		}

		private def flatPartitioning[S <: State[S]](p: Partitioning[S], l: List[Directive[S]]): List[(List[Directive[S]], Option[S], String)] = p match {
			case Leaf(s) => (l, Some(s), "") :: Nil;
			case Node(d, children) =>
				var result: List[(List[Directive[S]], Option[S], String)] = Nil;
				for (c <- children)
					result = result ::: flatPartitioning(c, l ::: d :: Nil);
				return result;
			case x: Supremum[S] => (l, None, x.toString()) :: Nil
		}
	}

	private def stateToString[S <: State[S]](state: S): JComponent = {
		var s: String = "";
		if (state.isInstanceOf[SingleLineRepresentation])
 			s = state.asInstanceOf[SingleLineRepresentation].toSingleLineString;
		else s = state.toString;
		new JLabel(s);
	}

}
