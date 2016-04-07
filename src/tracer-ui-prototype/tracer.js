/* A simple call-graph viewer. */
/* Licensed under GPL3 */
/* Derivative of: https://bl.ocks.org/mbostock/4339083 */
/* Written by: Justin Pombrio (2016) */

"use strict";

var MARGIN = {top: 20, right: 120, bottom: 20, left: 120};
var WIDTH = 960 - MARGIN.right - MARGIN.left;
var HEIGHT = 800 - MARGIN.top - MARGIN.bottom;
var DURATION = 750;

var ROOT;
var NEXT_ID = 0;

var TREE_LAYOUT = d3.layout.tree()
    .size([HEIGHT, WIDTH]);

var DIAGONAL = d3.svg.diagonal()
    .projection(function(d) { return [d.y, d.x]; });

var SVG = d3.select("body").append("svg")
    .attr("width", WIDTH + MARGIN.right + MARGIN.left)
    .attr("height", HEIGHT + MARGIN.top + MARGIN.bottom)
    .append("g")
    .attr("transform", "translate(" + MARGIN.left + "," + MARGIN.top + ")");


function load_log() {
  return [
    { type: "CALL",   data: "fib(3)" },
    { type: "CALL",   data: "fib(2)" },
    { type: "CALL",   data: "fib(1)" },
    { type: "RETURN", data: "1"      },
    { type: "CALL",   data: "fib(0)" },
    { type: "RETURN", data: "1"      },
    { type: "RETURN", data: "2"      },
    { type: "CALL",   data: "fib(1)" },
    { type: "RETURN", data: "1"      },
    { type: "RETURN", data: "3"      }
  ]
}

function load_data() {
  return [
    { call_string: "fib(3)",
      return_string: "3",
      children: [
        { call_string: "fib(2)",
          return_string: "2",
          children: [
            { call_string: "fib(1)", return_string: "1"},
            { call_string: "fib(0)", return_string: "1"}
          ]},
        { call_string: "fib(1)", return_string: "1"}]}
  ];
}

function init() {

  function log_to_tree(root, log) {
    var tree = root;
    log.forEach(function(event) {
      if (event.type == "CALL") {
        var child = { parent: tree, call_string: event.data };
        if (tree.children) {
          tree.children.push(child);
        } else {
          tree.children = [child];
        }
        tree = child;
      } else if (event.type == "RETURN") {
        tree.return_string = event.data;
        tree.label = tree.call_string + " -> " + tree.return_string;
        var parent = tree.parent;
        delete tree.parent;
        delete tree.call_string;
        delete tree.return_string;
        tree = parent;
      } else {
        throw "Tracer: invalid tracing log";
      }
    });
    return root;
  }

  var log = load_log();
  ROOT = log_to_tree({label: "Program"}, log);
  console.log(ROOT);
  
  function data_to_tree(data) {
    var label = data.call_string + " → " + data.return_string;
    if (data.children) {
      var children = data_to_tree_list(data.children);
      return {
        label: label,
        children: children
      }
    } else {
      return {label: label}
    }
  }
  
  function data_to_tree_list(data_list) {
    return data_list.map(function(data) {
      return data_to_tree(data);
    });
  }

  ROOT.x0 = HEIGHT / 2;
  ROOT.y0 = 0;

  function collapse(d) {
    if (d.children) {
      d._children = d.children;
      d._children.forEach(collapse);
      d.children = null;
    }
  }

//  ROOT.children.forEach(collapse);
  update(ROOT);

};
init();

d3.select(self.frameElement).style("height", "800px");

function update(source) {

  // Compute the new tree layout.
  var nodes = TREE_LAYOUT.nodes(ROOT).reverse(),
      links = TREE_LAYOUT.links(nodes);

  // Normalize for fixed-depth.
  nodes.forEach(function(d) { d.y = d.depth * 180; });

  // Update the nodes…
  var node = SVG.selectAll("g.node")
      .data(nodes, function(d) { return d.id || (d.id = ++NEXT_ID); });

  // Enter any new nodes at the parent's previous position.
  var nodeEnter = node.enter().append("g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + source.y0 + "," + source.x0 + ")"; })
      .on("click", click);

  nodeEnter.append("circle")
      .attr("r", 1e-6)
      .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; });

  nodeEnter.append("text")
      .attr("x", function(d) { return d.children || d._children ? -10 : 10; })
      .attr("dy", ".35em")
      .attr("text-anchor", function(d) { return d.children || d._children ? "end" : "start"; })
      .text(function(d) { return d.label; })
      .style("font-family", "Monospace")
      .style("fill-opacity", 1e-6);

  // Transition nodes to their new position.
  var nodeUpdate = node.transition()
      .duration(DURATION)
      .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; });

  nodeUpdate.select("circle")
      .attr("r", 4.5)
      .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; });

  nodeUpdate.select("text")
      .style("fill-opacity", 1);

  // Transition exiting nodes to the parent's new position.
  var nodeExit = node.exit().transition()
      .duration(DURATION)
      .attr("transform", function(d) { return "translate(" + source.y + "," + source.x + ")"; })
      .remove();

  nodeExit.select("circle")
      .attr("r", 1e-6);

  nodeExit.select("text")
      .style("fill-opacity", 1e-6);

  // Update the links…
  var link = SVG.selectAll("path.link")
      .data(links, function(d) { return d.target.id; });

  // Enter any new links at the parent's previous position.
  link.enter().insert("path", "g")
      .attr("class", "link")
      .attr("d", function(d) {
        var o = {x: source.x0, y: source.y0};
        return DIAGONAL({source: o, target: o});
      });

  // Transition links to their new position.
  link.transition()
      .duration(DURATION)
      .attr("d", DIAGONAL);

  // Transition exiting nodes to the parent's new position.
  link.exit().transition()
      .duration(DURATION)
      .attr("d", function(d) {
        var o = {x: source.x, y: source.y};
        return DIAGONAL({source: o, target: o});
      })
      .remove();

  // Stash the old positions for transition.
  nodes.forEach(function(d) {
    d.x0 = d.x;
    d.y0 = d.y;
  });
}

// Toggle children on click.
function click(d) {
  if (d.children) {
    d._children = d.children;
    d.children = null;
  } else {
    d.children = d._children;
    d._children = null;
  }
  update(d);
}
