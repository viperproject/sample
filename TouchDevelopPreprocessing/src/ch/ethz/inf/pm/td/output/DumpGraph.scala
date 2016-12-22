/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package ch.ethz.inf.pm.td.output

import ch.ethz.inf.pm.sample.oorepresentation.LabeledGraph

/**
  *
  * Dumps an execution into an HTML/Javascript based graph.
  *
  * @author Lucas Brutschy
  */
object DumpGraph {

  def dumpToFile[Node <: S, Weight <: T, S, T](
      name: String,
      graph: LabeledGraph[Node, Weight]
  ): String = {

    FileSystemExporter.export(name + ".html", getString(graph))

  }

  /** Returns a string to a file visualizing the given graph structure */
  def getString[Node <: S, Weight <: T, S, T](
      graph: LabeledGraph[Node, Weight]
  ): String = {

    val nodeMap = graph.nodes.zipWithIndex.toMap[S, Int]

    val nodeStr = (graph.nodes.zipWithIndex map { case (node,id) =>
      val name = graph.getNodeLabel(node).replace("'", "\"")
      val clazz = graph.getNodeClass(node)
      val partition = graph.getPartition(node).map { x => ", parent: '" + nodeMap(x) + "'" }.getOrElse("")
      s"{ data: { id: '$id', name: '$name' $partition }, classes: '$clazz' }"
    }).mkString(",\n")

    val edgeStr = (graph.edges map { case (source,target,weight) =>
      val label = weight.map(graph.getEdgeLabel).getOrElse("")
      val id = source + "to" + target + label
      s"{ data: { id: '$id', source: '$source', target: '$target', label: '$label' }, classes: 'edge' }"
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
         |<script src="https://cdn.rawgit.com/cpettitt/dagre/v0.7.4/dist/dagre.min.js"></script>
         |<script src="https://cdn.rawgit.com/cytoscape/cytoscape.js-dagre/1.1.2/cytoscape-dagre.js"></script>
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
         |        'background-color': 'lightgray',
         |        'width': 'label',
         |        'height': 'label',
         |        'padding': '5px',
         |        'shape': 'roundrectangle'
         |      }
         |    },
         |    {
         |      selector: '$$node > node',
         |      css: {
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
         |        'content': 'data(label)',
         |        'curve-style': 'bezier'
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
         |	layout: {
         |		name: 'dagre'
         |	}
         |  //layout: {
         |  //  name: 'grid',
         |  //  position: function(node) {
         |  //    return { row:node.data('row'), column:node.data('column') };
         |  //  },
         |  //  padding: 1
         |  //}
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

}
