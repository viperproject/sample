/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.oorepresentation.WeightedGraph

/**
  *
  * Dumps an execution into an HTML/Javascript based graph.
  *
  * @author Lucas Brutschy
  */
object DumpGraph {

  def dumpToFile[Node <: S, Weight <: T, S, T](
      name: String,
      graph: WeightedGraph[Node, Weight],
      renderer: GraphRenderer[S, T]
  ): String = {

    FileSystemExporter.export(name + ".html", getString(graph, renderer))

  }

  /** Returns a string to a file visualizing the given graph structure */
  def getString[Node <: S, Weight <: T, S, T](
      graph: WeightedGraph[Node,Weight],
      renderer: GraphRenderer[S, T]
  ): String = {

    val nodeMap = graph.nodes.zipWithIndex.toMap[S, Int]

    val nodeStr = (graph.nodes.zipWithIndex map { case (node,id) =>
      val name = renderer.name(node).replace("'", "\"")
      val clazz = renderer.clazz(node)
      val partition = renderer.partitioning(node).map { x => ", parent: '"+nodeMap(x)+"'" }.getOrElse("")
      s"{ data: { id: '$id', name: '$name' $partition }, classes: '$clazz' }"
    }).mkString(",\n")

    val edgeStr = (graph.edges map { case (source,target,weight) =>
      val label = weight.map(renderer.label).getOrElse("")
      val id = source + "to" + target + label
      s"{ data: { id: '$id', source: '$source', target: '$target', label: '$label' }, classes: '$label' }"
    }).mkString(",\n")

      s"""
         |<!DOCTYPE html>
         |<html>
         |<head>
         |<meta charset=utf-8 />
         |<meta name='viewport' content='user-scalable=no, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, minimal-ui'>
         |<title>Sessions</title>
         |<style>
         |body {
         |  font: 14px helvetica neue, helvetica, arial, sans-serif;
         |}
         |
         |#cy {
         |  height: 100%;
         |  width: 100%;
         |  position: absolute;
         |  left: 0;
         |  top: 0;
         |}
         |</style>
         |<script src='http://ajax.googleapis.com/ajax/libs/jquery/1/jquery.min.js'></script>
         |<script src='http://cytoscape.github.io/cytoscape.js/api/cytoscape.js-latest/cytoscape.min.js'></script>
         |<script>
         |
         |$$(function(){ // on dom ready
         |
         |var cy = cytoscape({
         |  container: document.getElementById('cy'),
         |
         |  style: [
         |    {
         |      selector: 'node',
         |      css: {
         |        'content': 'data(name)',
         |        'text-valign': 'center',
         |        'text-halign': 'center',
         |        'background-color': 'lightgray'
         |      }
         |    },
         |    {
         |      selector: '$$node > node',
         |      css: {
         |        'padding-top': '3px',
         |        'padding-left': '10px',
         |        'padding-bottom': '3px',
         |        'padding-right': '10px',
         |        'text-valign': 'top',
         |        'text-halign': 'center'
         |      }
         |    },
         |    {
         |      selector: 'edge',
         |      css: {
         |        'target-arrow-shape': 'triangle',
         |        'background-color': 'black',
         |        'line-color': 'black',
         |        'target-arrow-color': 'black',
         |        'source-arrow-color': 'black',
         |        'content': 'data(label)'
         |      }
         |    },
         |    {
         |      selector: '.coral',
         |      css: {
         |        'background-color': 'lightcoral'
         |      }
         |    },
         |    {
         |      selector: '.blue',
         |      css: {
         |        'background-color': 'lightblue'
         |      }
         |    },
         |    {
         |      selector: '.white',
         |      css: {
         |        'background-color': 'white'
         |      }
         |    },
         |    {
         |      selector: ':selected',
         |      css: {
         |        'background-color': 'yellow',
         |        'line-color': 'yellow',
         |        'target-arrow-color': 'yellow',
         |        'source-arrow-color': 'yellow'
         |      }
         |    }
         |  ],
         |
         |  elements: {
         |    nodes: [
         |      $nodeStr
         |    ], edges: [
         |      $edgeStr
         |    ]
         |  },
         |
         |  layout: {
         |    name: 'grid',
         |    position: function(node) {
         |      return { row:node.data('row'), column:node.data('column') };
         |    },
         |    padding: 1
         |  }
         |});
         |
         |}); // on dom ready
         |</script>
         |
         |</head>
         |<body>
         |
         |<div id='cy'></div>
         |
         |</body>
         |</html>
    """.stripMargin

  }

  trait GraphRenderer[Node, Weight] {

    def clazz(node: Node): String

    def name(node: Node): String

    def label(value: Weight): String

    def partitioning(value: Node): Option[Node]

  }

  object SimpleGraphRenderer extends GraphRenderer[Any, Any] {

    def clazz(node: Any): String = "node"

    def name(node: Any): String = node.toString

    def label(value: Any): String = value.toString

    def partitioning(value: Any) = None

  }

}
