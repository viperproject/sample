/** Performs the layouting and rendering of a graph.
  *
  * Nodes are defined as HTML elements in the 'graph' container.
  * However, edges are added via JavaScript using the `graph` property
  * of this `GraphView` object.
  *
  * In addition to a 'source' and 'target' property, each edge may also
  * have a 'label' property as well as an 'onConnected' hook that is called
  * when the edge has been rendered.
  *
  * @todo Make it more OO.
  */
function GraphView() {
    // By default, nodes are laid out from left to right
    this.rankDir = "LR";
    this.nodeSep = 75;
    this.rankSep = 75;

    var graph = new dagre.Digraph();
    this.graph = graph;

    // Add the nodes represented by HTML DIVs to the `dgre.Digraph`
    $('#graph .node').each(function(index, node) {
        graph.addNode($(node).attr('id'), {
            width: $(node).width(),
            height: $(node).height()
        });
    });
};

/** Use `dagre` to position each node in the graph. */
GraphView.prototype.layout = function() {
    var layout = dagre.layout();
    layout = layout.nodeSep(this.nodeSep);
    layout = layout.rankSep(this.rankSep);
    layout = layout.rankDir(this.rankDir);
    layout = layout.run(this.graph);

    layout.eachNode(function(id, value) {
        var elem = $('#' + id);
        elem.css('top', (value['y'] + 50) + 'px');
        elem.css('left', value['x'] + 'px');
    });

    this.layout = layout;
};

/** Use `jsPlumb` to draw the edges of the graph and make it interactive. */
GraphView.prototype.display = function() {
    this.layout();

    var graph = this.graph;
    // Initialize the jsPlumb with general settings
    var instance = jsPlumb.getInstance({
        Endpoint: ["Blank", {}],
        HoverPaintStyle: {strokeStyle:"#1e8151"},
        ConnectionOverlays: [
            ["Arrow", {
                location: 1,
                id: "arrow",
                length: 10,
                foldback: 0.8
            }],
            ["Label", {
                id: "label",
                cssClass: "edgeLabel"
            }]
        ],
        Container: "graph"
    });
    this.instance = instance;

    // Make all nodes draggable
    var vertices = jsPlumb.getSelector("#graph .node");
    instance.draggable(vertices);

    // Draw all edges
    instance.doWhileSuspended(function() {
        instance.makeSource(vertices, {
            anchor: "Continuous",
            connector: ["StateMachine", {
                curviness: 10,
                margin: 0,
                loopbackRadius: 15
            }],
            connectorStyle: {
                strokeStyle: "#5c96bc",
                lineWidth: 2,
                outlineColor: "transparent",
                outlineWidth: 4
            }
        });

        instance.makeTarget(vertices, {
            anchor: "Continuous"
        });

        graph.eachEdge(function(edge, source, target, value) {
            var options = {
                source: source,
                target: target,
                anchors: ["Continuous", "Continuous"]
            }

            // Display a label on an edge only if its 'label' attribute
            // is defined
            if (value != undefined && value["label"]) {
                options["overlays"] = [
                    ["Label", {
                        label: value["label"],
                        id: "label",
                        cssClass: "edgeLabel"
                    }]
                ]
            }

            // If defined, call the 'onConnected' callback
            var con = instance.connect(options);
            if (value != undefined && value["onConnected"]) {
                value["onConnected"](con);
            }
        });
    });
};