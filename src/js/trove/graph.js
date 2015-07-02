define(["js/runtime-util", "js/js-numbers", "trove/image-lib", "trove/image-structs",
        "trove/d3-lib", "trove/graph-structs",
        "../../../node_modules/d3/d3.min", "../../../node_modules/d3-tip/index"],
       function(util, jsnums, imageLib, imageStructs, clib, graphStructs, d3, d3tipLib){

  var libs = clib(d3);
  var libData =    libs.libData,
    libNum =       libs.libNum,
    libJS =        libs.libJS,
    libColor =     libs.libColor,
    libCheck =     libs.libCheck,
    getMargin =    libs.d3common.getMargin,
    getDimension = libs.d3common.getDimension,
    svgTranslate = libs.d3common.svgTranslate,
    createDiv =    libs.d3common.createDiv,
    createCanvas = libs.d3common.createCanvas,
    callBigBang =  libs.d3common.callBigBang,
    stylizeTip =   libs.d3common.stylizeTip,
    assert =       libs.assert,
    d3tip =        libs.d3common.d3tipBuilder(d3tipLib);

  return function (rt, namespace) {
    var IMAGE = imageLib(rt, rt.namespace);
    var gf = rt.getField;

    var tostring = function (val) {
      return rt.toReprJS(val, rt.ReprMethods._tostring)
    };

    return rt.loadModulesNew(namespace, [imageStructs, graphStructs],
      function(IMAGESTRUCTS, STRUCTS) {

      function valFromStructs(name){
  			return gf(gf(STRUCTS, "values"), name);
  		}

  		function typeFromStructs(name){
  			return gf(STRUCTS, "types")[name];
  		}

      var PyretGray = gf(gf(IMAGESTRUCTS, "values"), "gray")

      function forceLayout(rawGraph) {
        /*
         * This function is adapted from:
         *
         * http://jsfiddle.net/zhanghuancs/a2QpA/
         * http://emptypipes.org/2015/02/15/selectable-force-directed-graph/
         */
        rt.checkArity(1, arguments, "force-layout");
        rt.checkObject(rawGraph);
        // TODO: use brander to check if it is a graph

        var marginType = "none",
          margin = getMargin(marginType),
          dimension = getDimension(margin),
          width = dimension.width,
          height = dimension.height;

        var colorConverter = libColor.convertColor(rt, IMAGE);

        var graphOptions = gf(rawGraph, "options");
        var vertexPrinter = gf(graphOptions, "vertex-printer").app;
        var edgePrinter = gf(graphOptions, "edge-printer").app;

        var arrayOfNodes = rt.ffi.toArray(gf(rawGraph, "vertices"));
        var nodes = arrayOfNodes.map(
          function(e, i){
            // temporarily mutate the object
            // to make node finding O(1)
            // will remove this additional field afterwards
            e['_index'] = i;
            var options = gf(e, "options");
            return {
              'name': vertexPrinter(e),
              'index': i,
              'color': gf(options, "color"),
            };
          });

        var totalLinks = {};
        var links = rt.ffi.toArray(
          gf(rawGraph, "edges")).map(
            function (e) {
              var options = gf(e, "options");
              return {
                'source': nodes[gf(e, "source")['_index']],
                'target': nodes[gf(e, "target")['_index']],
                'linkindex': 0,
                'name': edgePrinter(e),
                'color': gf(options, "color"),
                'directed': gf(options, "directed")
              };
            });

        links.sort(function (a, b) {
          assert(a.source.index !== undefined);
          if (a.source.index == b.source.index) {
            return a.target.index - b.target.index;
          }
          return a.source.index - b.source.index;
        });

        function idEdge(d) {
          return d.source.index + "_" + d.target.index + "_" +
             d.linkindex;
        }

        links.forEach(function (v, i) {
          assert(links[i].source.index !== undefined);
          if (i > 0 &&
            links[i].source.index == links[i - 1].source.index &&
            links[i].target.index == links[i - 1].target.index) {

            links[i].linkindex = links[i - 1].linkindex + 1;
          }
          var combinedName = links[i].source.index + ',' + links[i].target.index;
          totalLinks[combinedName] = links[i].linkindex + 1;
        });

        var detached = createDiv();
        var canvas = createCanvas(detached, margin, "top-left");

        detached.select('.divsvg').attr("tabindex", 1)
          .on("keydown.brush", keydown)
          .on("keyup.brush", keyup)
          .each(function () { this.focus(); });

        var shiftKey, hideLabels = false;

        var xScale = d3.scale.linear()
            .domain([0,width]).range([0,width]);
        var yScale = d3.scale.linear()
            .domain([0,height]).range([0,height]);

        var zoomer = d3.behavior.zoom().
          scaleExtent([0.5,5]).
          x(xScale).
          y(yScale).
          on("zoomstart", zoomstart).
          on("zoom", redraw);

        function zoomstart() {
          node.each(function (d) {
            d.selected = false;
            d.previouslySelected = false;
          });
          node.classed("selected", false);
          update();
        }

        function redraw() {
          vis.attr("transform",
               svgTranslate(d3.event.translate) +
               " scale(" + d3.event.scale + ")");
          update();
        }

        var brusher = d3.svg.brush()
            .x(xScale)
            .y(yScale)
            .on("brushstart", function (d) {
              node.each(function (d) {
                d.previouslySelected = shiftKey && d.selected;
              });
              update();
            })
            .on("brush", function () {
              var extent = d3.event.target.extent();

              node.classed("selected", function (d) {
                return d.selected =
                  d.previouslySelected ^
                  (extent[0][0] <= d.x &&
                    d.x < extent[1][0] &&
                    extent[0][1] <= d.y &&
                    d.y < extent[1][1]);
              });
              update();
            })
            .on("brushend", function () {
              d3.event.target.clear();
              d3.select(this).call(d3.event.target);
              update();
            });

        var svg_graph = canvas.append('g')
            .call(zoomer);
        //.call(brusher)

        var brush = svg_graph.append("g")
            .datum(function () {
              return {
                selected: false,
                previouslySelected: false
              };
            })
            .attr("class", "brush");

        function strokeLinkPre(d){
          return colorConverter(d.color);
        }

        function strokeLink(d) {
          return libColor.changeColor(
            IMAGE.colorRed(d.color),
            IMAGE.colorGreen(d.color),
            IMAGE.colorBlue(d.color), -20);
        }

        var vis = svg_graph.append("g");

        vis.attr('opacity', 0.4)
          .attr('id', 'vis');

        brush.call(brusher)
          .on("mousedown.brush", null)
          .on("touchstart.brush", null)
          .on("touchmove.brush", null)
          .on("touchend.brush", null);

        brush.select('.background').style('cursor', 'auto');

        var markers = vis.append("defs");

        var subMarkers = markers.selectAll("marker")
          .data(links).enter()
          .append("marker")
          .attr("id", function (d) { return "arrow_" + idEdge(d); })
          .attr("class", "link")
          .attr("viewBox", "-5 -5 10 10")
          .attr("refX", 17)
          .attr("markerWidth", 3)
          .attr("markerHeight", 3)
          .attr("orient", "auto")
          .append("path")
          .attr("d", "M 0,0 m -5,-5 L 5,0 L -5,5 Z")
          .attr('fill', strokeLinkPre);

        var tipVertex = d3tip(detached)
            .attr('class', 'd3-tip')
            .direction('e')
            .offset([0, 20])
            .html(function (d) { return d.name; });

        var tipEdge = d3tip(detached)
            .attr('class', 'd3-tip')
            .direction('e')
            .offset([0, 20])
            .html(function (d) { return d.name; });

        canvas.call(tipVertex);
        canvas.call(tipEdge);

        function setLink(cls){
          return vis.append("g")
            .attr("class", cls)
            .selectAll("path")
            .data(links).enter()
            .append("path")
            .classed(function(d) { return cls + '_' + idEdge(d); })
            .attr("marker-end", function(d) {
              if(d.directed) {
                return "url(#arrow_" + idEdge(d) + ")";
              } else {
                return "";
              }
            })
            .attr('stroke-linecap', 'round');
        }

        var linkPre = setLink('linkPre')
          .attr("stroke", strokeLinkPre);

        var link = setLink('link')
          .classed({'active': false, 'inactive': true})
          .on('mouseover', function(d) {
            var aClass = {
              'active': true,
              'inactive': false
            };
            d3.select(this).classed(aClass);
            d3.select('#text_' + idEdge(d)).classed(aClass);
            update();
            tipEdge.show(d);
          })
          .on('mouseout', function(d) {
            var aClass = {
              'active': false,
              'inactive': true
            };
            d3.select(this).classed(aClass);
            d3.select('#text_' + idEdge(d)).classed(aClass);
            update();
            tipEdge.hide(d);
          })
          .on('mousedown', tipEdge.hide)
          .attr("stroke", strokeLink);

        var node = vis.append("g")
            .attr("class", "node")
            .selectAll("circle")
            .data(nodes).enter()
            .append("circle")
            .attr("r", 8)
            .attr("fill", function (d) {
              return colorConverter(d.color);
            })
            .on("dblclick", function (d) {
              d3.event.stopPropagation();
            })
            .on("click", function (d) {
              if (d3.event.defaultPrevented) return;
              if (!shiftKey) {
                //if the shift key isn't down,
                // unselect everything
                canvas.selectAll(".selected").classed(
                  'selected', function (k) {
                    k.selected = false;
                    k.previouslySelected = false;
                    return false;
                  });
              }
              // always select this node
              d3.select(this).classed(
                "selected", d.selected = !d.previouslySelected);
              update();
            })
            .on("mouseup", function (d) {
              //if (d.selected && shiftKey) d3.select(this).classed("selected", d.selected = false);
            })
            .on("mouseover", tipVertex.show)
            .on("mouseout", tipVertex.hide)
            .on("mousedown", tipVertex.hide)
            .call(d3.behavior.drag()
                .on("dragstart", dragstarted)
                .on("drag", dragged)
                .on("dragend", dragended));

        // TODO: add this back and make it works
        /*
        var textlink = vis.append('g')
            .selectAll(".link_label")
            .data(links).enter()
            .append('text')
            .attr('class', 'label link_label')
            .attr('id', function(d) { return 'text_' + idEdge(d); })
            .append('textPath')
            .attr('startOffset', '50%')
            .attr("text-anchor", "middle")
            .attr("xlink:href", function (d) {
              return "#link_" + idEdge(d);
            })
            .text(function (d) { return d.name; });
        */

        var textnode = vis.append('g')
            .selectAll('g')
            .data(nodes).enter()
            .append('g')

        var DXTEXTNODE = 14;
        var DYTEXTNODE = 4;

        textnode.append('text')
            .attr('x', DXTEXTNODE)
            .attr('y', DYTEXTNODE)
            .attr("class", "shadow label")
            .attr('stroke', function (d) {
              return libColor.getContrast(
                IMAGE.colorRed(d.color),
                IMAGE.colorGreen(d.color),
                IMAGE.colorBlue(d.color));
            })
            .text(function(d) { return d.name; });

        textnode.append('text')
            .attr('x', DXTEXTNODE)
            .attr('y', DYTEXTNODE)
            .attr('class', 'label')
            .attr('fill', function (d) {
              return colorConverter(d.color);
            })
            .text(function (d) { return d.name; });

        var force = d3.layout.force()
            .charge(-120)
            .linkDistance(200)
            .nodes(nodes)
            .links(links)
            .gravity(0.2)
            .size([width, height])
            .start();

        function updateLink(l){
          l.attr("d", function (d) {
            var dx = d.target.x - d.source.x,
              dy = d.target.y - d.source.y,
              numOfLinks =
                totalLinks[
                  d.source.index + "," + d.target.index] ||
                totalLinks[
                  d.target.index + "," + d.source.index],
              dr = 0.8 * Math.sqrt(dx * dx + dy * dy) /
                (1 + d.linkindex / numOfLinks);

            return "M" + d.source.x + "," + d.source.y +
              "A" + dr + "," + dr + " 0 0,1" +
              d.target.x + "," + d.target.y;
          });
        }

        function tick() {
          updateLink(link);
          updateLink(linkPre);

          node.attr('transform',
                function (d) { return svgTranslate(d.x, d.y); });

          textnode.attr('transform',
                function (d) { return svgTranslate(d.x, d.y); });

          update();
        };

        force.on("tick", tick);


        var center_view = function () {
          // Center the view on the molecule(s) and scale it so
          // that everything fits in the window

          //no molecules, nothing to do
          if (nodes.length === 0) return;

          // Get the bounding box
          var min_x = d3.min(nodes.map(function (d) { return d.x; })),
            max_x = d3.max(nodes.map(function (d) { return d.x; })),
            min_y = d3.min(nodes.map(function (d) { return d.y; })),
            max_y = d3.max(nodes.map(function (d) { return d.y; }));

          // The width and the height of the graph
          var mol_width = max_x - min_x,
            mol_height = max_y - min_y;

          // how much larger the drawing area is than the width and
          // the height
          var width_ratio = width / mol_width,
            height_ratio = height / mol_height;

          // we need to fit it in both directions, so we scale
          // according to the direction in which we need to
          // shrink the most
          var min_ratio = Math.min(width_ratio, height_ratio) * 0.6;

          // the new dimensions of the molecule
          var new_mol_width = mol_width * min_ratio,
            new_mol_height = mol_height * min_ratio;

          // translate so that it's in the center of the window
          var x_trans = -(min_x) * min_ratio +
              (width - new_mol_width) / 2,
            y_trans = -(min_y) * min_ratio +
              (height - new_mol_height) / 2;


          // do the actual moving
          vis.attr("transform",
               svgTranslate(x_trans, y_trans) +
                 " scale(" + min_ratio + ")");

          // tell the zoomer what we did so that next we zoom,
          // it uses the transformation we entered here
          zoomer.translate([x_trans, y_trans ]);
          zoomer.scale(min_ratio);
          update();
        };

        function dragended(d) {
          //d3.select(self).classed("dragging", false);
          node.filter(function (d) { return d.selected; })
            .each(function (d) { d.fixed &= ~6; });
          update();
        }

        function dragstarted(d) {
          d3.event.sourceEvent.stopPropagation();
          if (!d.selected && !shiftKey) {
            // if this node isn't selected,
            // then we have to unselect every other node
            node.classed("selected", function (k) {
              return k.selected = k.previouslySelected = false;
            });
          }

          d3.select(this).classed("selected", function () {
            d.previouslySelected = d.selected;
            return d.selected = true;
          });

          node.filter(function (d) { return d.selected; })
            .each(function (d) { d.fixed |= 2; });
          update();
        }

        function dragged(d) {
          node.filter(function (d) { return d.selected; })
            .each(function (d) {
              d.x += d3.event.dx;
              d.y += d3.event.dy;
              d.px += d3.event.dx;
              d.py += d3.event.dy;
            });
          force.resume();
          update();
        }

        function keydown() {
          shiftKey = d3.event.shiftKey || d3.event.metaKey;

          //console.log('d3.event', d3.event);

          if (d3.event.keyCode == 67) {   //the 'c' key
            center_view();
          }

          if (shiftKey) {
            svg_graph.call(zoomer)
              .on("mousedown.zoom", null)
              .on("touchstart.zoom", null)
              .on("touchmove.zoom", null)
              .on("touchend.zoom", null);

            //svg_graph.on('zoom', null);
            vis.selectAll('g.gnode')
              .on('mousedown.drag', null);

            brush.select('.background').style(
              'cursor', 'crosshair');
            brush.call(brusher);
          }
          update();
        }

        function keyup() {
          shiftKey = d3.event.shiftKey || d3.event.metaKey;

          brush.call(brusher)
            .on("mousedown.brush", null)
            .on("touchstart.brush", null)
            .on("touchmove.brush", null)
            .on("touchend.brush", null);

          brush.select('.background').style('cursor', 'auto');
          svg_graph.call(zoomer);
          update();
        }

        function update() {
          canvas.selectAll('.link, .linkPre').style({
            'fill': 'none',
            'stroke-width': 2
          });
          node.style({
            'stroke': '#fff',
            'stroke-width': '1.5px'
          });
          canvas.selectAll('.selected').style({
            'stroke': 'black'
          });
          canvas.selectAll('.brush .extent').style({
            'fill-opacity': '0.1',
            'stoke': '#fff',
            'shape-rendering': 'crispEdges'
          });
          canvas.selectAll('text').style({
            'font-family': "sans-serif",
            'user-select': 'none',
            'cursor': 'default'
          });
          /*
          textlink.style({
            'font-size': '6px',
            'stroke-width': 0,
            'fill': 'red'
          });
          */
          textnode.style({
            'font-size': '10px',
            'stroke-width': 0,
            'fill': 'green'
          });
          canvas.selectAll('.shadow').style({
            'stroke-width': "3px",
            'stroke-opacity': "0.9"
          })

          detached.selectAll('.divsvg').style({
            'outline': 'none'
          });

          canvas.selectAll('.active').style({
            'stroke-opacity': 1
          });

          canvas.selectAll('.inactive').style({
            'stroke-opacity': 0
          });

          canvas.selectAll('marker').style({
            'stroke-opacity': 1
          });

          stylizeTip(detached);
        }

        for(var i = 0; i < 100; ++i){ force.tick(); }
        center_view();

        detached.append('button')
          .attr('class', 'd3btn')
          .text('Hide labels')
          .on('click', function () {
            if (hideLabels) {
              canvas.selectAll('.label').style({
                'opacity': '100'
              });
              d3.select(this).text('Hide labels');
            } else {
              canvas.selectAll('.label').style({
                'opacity': 0
              });
              d3.select(this).text('Show labels');
            }
            hideLabels = !hideLabels;
          });

        arrayOfNodes.forEach(function(e){
          delete e['_index'];
        });
        callBigBang(rt, detached);

        return rawGraph;
      }

      function treeDiagram(rootNode, treeOptions) {
        /*
         * Part of this function is adapted from:
         * http://www.d3noob.org/2014/01/tree-diagrams-in-d3js_11.html
         */
        rt.checkArity(2, arguments, "tree-diagram");
        rt.checkObject(rootNode);
        rt.checkObject(treeOptions);

        var printer = gf(treeOptions, 'node-printer').app;

        // TODO: use brander to check if it is a tree-diagram. (how?)

        function parseTree(node) {
          return {
            'name': printer(node),
            'children': rt.ffi.toArray(
              gf(node, "children")).map(parseTree)
          };
        }

        var margin = MARGIN,
          dimension = getDimension(margin),
          width = dimension.width,
          height = dimension.height;

        var tree = d3.layout.tree().size([height, width]);
        var diagonal = d3.svg.diagonal()
            .projection(function (d) { return [d.x, d.y]; });

        var root = parseTree(rootNode);

        var detached = createDiv();
        var canvas = createCanvas(detached, margin, "top-left");

        root.x0 = height / 2;
        root.y0 = 0;

        var i = 0, duration = 750;

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

        function update(source) {
          // Compute the new tree layout.
          var nodes = tree.nodes(root).reverse(),
            links = tree.links(nodes);

          // Normalize for fixed-depth.
          nodes.forEach(function (d) { d.y = d.depth * 40; });

          // Update the
          var node = canvas.selectAll("g.node")
              .data(nodes, function (d) {
                return d.id || (d.id = ++i);
              });

          // Enter any new nodes at the parent's previous position.
          var nodeEnter = node.enter().append("g")
              .attr("class", "node")
              .attr("transform", function (d) {
                return svgTranslate(source.x0, source.y0);
              })
              .on("click", click);

          nodeEnter.append("circle")
            .attr('class', 'circlemain')
            .attr("r", 1e-6)
            .style("fill", function (d) {
              return d._children ? "lightsteelblue" : "#fff";
            });

          nodeEnter.append("circle")
            .attr('class', 'circleminor')
            .attr('r', 1e-6)

          nodeEnter.append("text")
            .attr("dy", ".35em")
            .attr("text-anchor", "middle")
            .text(function (d) { return d.name; })
            .style("fill-opacity", 1e-6);

          // Transition nodes to their new position.
          var nodeUpdate = node.transition()
              .duration(duration)
              .attr("transform", function (d) {
                return svgTranslate(d.x, d.y);
              });

          nodeUpdate.select(".circlemain")
            .attr("r", 10)
            .style("fill", function (d) {
              return d._children ? "lightsteelblue" : "#fff";
            });

          nodeUpdate.select(".circleminor")
            .attr('r', function(d) {
              return d._children ? 4 : 1e-6
            })
            .attr('cy', 15)


          nodeUpdate.select("text")
            .style("fill-opacity", 1);

          // Transition exiting nodes to the parent's new position.
          var nodeExit = node.exit().transition()
              .duration(duration)
              .attr("transform", function (d) {
                return svgTranslate(source.y, source.x);
              })
              .remove();

          nodeExit.select("circle").attr("r", 1e-6);

          nodeExit.select("text")
            .style("fill-opacity", 1e-6);

          // Update the links

          var link = canvas.selectAll("path.link")
              .data(links, function (d) { return d.target.id; });

          // Enter any new links at the parent's previous position.
          link.enter().insert("path", "g")
            .attr("class", "link")
            .attr("d", function (d) {
              var o = {x: source.x0, y: source.y0};
              return diagonal({source: o, target: o});
            });

          // Transition links to their new position.
          link.transition()
            .duration(duration)
            .attr("d", diagonal);

          // Transition exiting nodes to the parent's new position.
          link.exit().transition()
            .duration(duration)
            .attr("d", function (d) {
              var o = {x: source.x, y: source.y};
              return diagonal({source: o, target: o});
            })
            .remove();

          // Stash the old positions for transition.
          nodes.forEach(function (d) {
            d.x0 = d.x;
            d.y0 = d.y;
          });

          canvas.selectAll(".node").style("cursor", "pointer");

          canvas.selectAll(".node circle").style({
            'fill': '#fff',
            'opacity': 0.5,
            'stroke': 'blue',
            'stroke-width': "1.5px"
          });

          canvas.selectAll(".node text").style({
            'font-size': '13px'
          });

          canvas.selectAll(".link").style({
            'fill': 'none',
            'stroke': '#ccc',
            'stroke-width': '2px'
          });
        }

        update(root);
        callBigBang(rt, detached);

        return rootNode;
      }

      return util.makeModuleReturn(rt, {
        Vertex: typeFromStructs("Vertex"),
        Edge: typeFromStructs("Edge"),
        Graph: typeFromStructs("Graph"),
        Tree: typeFromStructs("Tree")
      }, {
        "show-tree": rt.makeFunction(treeDiagram),
        "show-graph": rt.makeFunction(forceLayout),
        "graph": valFromStructs("graph"),
        "node": valFromStructs("node"),
        "vertex": valFromStructs("vertex"),
        "edge": valFromStructs("edge"),
        "graph-options": rt.makeObject({
          "vertex-printer": rt.makeFunction(function(vertex){
            // TODO: check if it's a vertex
            return tostring(gf(vertex, "value"))
          }),
          "edge-printer": rt.makeFunction(function(edge){
            // TODO: check if it's an edge
            return tostring(gf(edge, "value"))
          })
        }),
        "vertex-options": rt.makeObject({
          "color": PyretGray
        }),
        "edge-options": rt.makeObject({
          "color": PyretGray,
          "directed": rt.pyretTrue,
          "size": 5 // TODO: put proper value
        }),
        "tree-options": rt.makeObject({
          "node-printer": rt.makeFunction(function(tree){
            // TODO: check if it's a tree
            return tostring(gf(tree, "value"));
          })
        })
      });
    });
  };
});
