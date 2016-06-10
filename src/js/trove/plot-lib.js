define(["js/runtime-util", "js/js-numbers", "trove/option", "trove/image-lib", "trove/image-structs",
        "trove/d3-lib",
        "../../../node_modules/d3/d3.min",
        "../../../node_modules/d3-tip/index"],
        function(util, jsnums, optionLib, imageLib, imageStructs,
                 clib, d3, d3tipLib) {

  var HISTOGRAM_N = 100;
  var libs = clib(d3);
  var libData =    libs.libData,
    libNum =       libs.libNum,
    libJS =        libs.libJS,
    libColor =     libs.libColor,
    getMargin =    libs.d3common.getMargin,
    getDimension = libs.d3common.getDimension,
    svgTranslate = libs.d3common.svgTranslate,
    createDiv =    libs.d3common.createDiv,
    createCanvas = libs.d3common.createCanvas,
    callBigBang =  libs.d3common.callBigBang,
    stylizeTip =   libs.d3common.stylizeTip;
    d3tip =        libs.d3common.d3tipBuilder(d3tipLib);


  function appendAxis(xMin, xMax, yMin, yMax, width, height, canvas) {
    /*
     * Appends axes to canvas
     *
     * @param {jsnums} xMin
     * @param {jsnums} xMax
     * @param {jsnums} yMin
     * @param {jsnums} yMax
     * @param {fixnum} width
     * @param {fixnum} height
     * @param {d3 selection} canvas
     */

    function getAxisConf(aMin, aMax) {
      var conf = {},
        scaler = libNum.scaler(aMin, aMax, 0, 1, false),
        pos = jsnums.toFixnum(scaler(0));

      if (0 <= pos && pos <= 1) {
        conf.bold = true;
        conf.pos = pos;
      } else if (pos > 1) {
        conf.bold = false;
        conf.pos = 1;
      } else if (pos < 0) {
        conf.bold = false;
        conf.pos = 0;
      }
      return conf;
    }

    var xAxisConf = getAxisConf(yMin, yMax),
        yAxisConf = getAxisConf(xMin, xMax);
    xAxisConf.pos = 1 - xAxisConf.pos;

    var tickNum = 11;

    var xAxisScaler = d3.scale.linear()
        .domain([0, tickNum - 1]).range([0, width - 1]),
      yAxisScaler = d3.scale.linear()
        .domain([0, tickNum - 1]).range([height - 1, 0]);

    var allValues = d3.range(0, tickNum);

    var xAxisDisplayScaler = libNum.scaler(0, tickNum - 1, xMin, xMax),
      yAxisDisplayScaler = libNum.scaler(0, tickNum - 1, yMin, yMax);

    var prettyNumToStringDigits7 = libNum.getPrettyNumToStringDigits(7);

    var xAxis = d3.svg.axis().scale(xAxisScaler)
        .orient((xAxisConf.pos === 0) ? "top" : "bottom")
        .tickValues(allValues).tickFormat(
          function (d, i) {
            return prettyNumToStringDigits7(xAxisDisplayScaler(i));
          });

    canvas.append("g")
      .attr("class", "x axis").attr(
        "transform",
        svgTranslate(0, xAxisConf.pos * (height - 1)))
      .call(xAxis);

    var yAxis = d3.svg.axis().scale(yAxisScaler)
        .orient((yAxisConf.pos === 1) ? "right" : "left")
        .tickValues(allValues).tickFormat(
          function (d, i) {
            return prettyNumToStringDigits7(yAxisDisplayScaler(i));
          });

    canvas.append("g")
      .attr("class", "y axis").attr(
        "transform",
        svgTranslate(yAxisConf.pos * (width - 1), 0))
      .call(yAxis);

    canvas.selectAll('.x.axis path').style({
      'stroke': 'black',
      'stroke-width': xAxisConf.bold ? 2 : 0,
      'fill': 'none'
    });
    canvas.selectAll('.y.axis path').style({
      'stroke': 'black',
      'stroke-width': yAxisConf.bold ? 2 : 0,
      'fill': 'none'
    });

    canvas.selectAll("g.y.axis g.tick line")
      .attr("x1", -yAxisConf.pos * (width - 1))
      .attr("x2", (1 - yAxisConf.pos) * (width - 1));
    canvas.selectAll("g.x.axis g.tick line")
      .attr("y1", -xAxisConf.pos * (height - 1))
      .attr("y2", (1 - xAxisConf.pos) * (height - 1));

    canvas.selectAll('.axis').style({'shape-rendering': 'crispEdges'});
    canvas.selectAll('.axis text').style({'font-size': '10px'});
    canvas.selectAll('.axis line').style({
      'stroke': 'lightgray',
      'opacity': 0.6
    });
  }

  function plotBar(xMin, xMax, yMin, yMax, width, height, data, histogramn, detached, canvas) {

    /*
     * Plot a histogram
     *
     * Part of this function is adapted from
     * http://www.frankcleary.com/making-an-interactive-histogram-in-d3-js/
     */

    var x = d3.scale.linear()
        .domain([0, histogramn])
        .range([0, width]);

    var y = d3.scale.linear()
        .domain([0, d3.max(data, function (d) { return d.y; })])
        .range([height, 0]);

    var prettyNumToStringDigits7 = libNum.getPrettyNumToStringDigits(7);

    var tip = d3tip(detached)
        .attr('class', 'd3-tip')
        .direction('e')
        .offset([0, 20])
        .html(function (d) {
          var maxVal = prettyNumToStringDigits7(d.reduce(rt.num_max), 6);
          var minVal = prettyNumToStringDigits7(d.reduce(rt.num_min), 6);
          return "min: " + minVal.toString() + "<br />" +
                 "max: " + maxVal.toString() + "<br />" +
                 "freq: " + d.y;
        });

    canvas.call(tip);

    var bar = canvas.selectAll(".bar")
        .data(data)
        .enter().append("g")
        .attr("class", "bar")
        .on("mouseover", tip.show)
        .on("mouseout", tip.hide);

    bar.append("rect")
      .attr("x", function (d) { return x(d.x); })
      .attr("y", function (d) { return y(d.y); })
      .attr("width", x(data[0].dx) - 1)
      .attr("height", function (d) { return height - y(d.y); });

    canvas.selectAll('.bar rect')
      .style({
        'fill': 'steelblue',
        'fill-opacity': '0.8',
        'shape-rendering': 'crispEdges'
      })
      .on('mouseover', function (d) {
        d3.select(this).style('fill', "black");
      })
      .on('mouseout', function (d) {
        d3.select(this).style('fill', "steelblue");
      });

    stylizeTip(detached);
  }

  return function(rt, namespace) {

    var gf = rt.getField;
    var IMAGE = imageLib(rt, rt.namespace);
    var colorConverter = libColor.convertColor(rt, IMAGE);

    function genericPlot(scatterPlots, linePlots, windowOptions) {
      var xMin = gf(windowOptions, "x-min");
      var xMax = gf(windowOptions, "x-max");
      var yMin = gf(windowOptions, "y-min");
      var yMax = gf(windowOptions, "y-max");

      var marginType = "normal",
        margin = getMargin(marginType),
        dimension = getDimension(margin),
        width = dimension.width,
        height = dimension.height;

      var detached = createDiv();
      var detachedJQuery = $(detached.node());

      var divSvg = detached
              .append('div')
              .attr('class', 'divsvg'),
          canvas = divSvg
              .append("svg")
              .attr("width", libs.constants.width)
              .attr("height", libs.constants.height)
              .append("g")
              .attr('class', 'maing')
              .append('g'),
          panel = detached.append('div').style({top: '65px', left: '660px'}),
          controller = $(panel.append('div').style({top: "90px"}).node()),
          coordDisplay = panel.append('div').style({top: "0px", left: "0px", 'font-size': '12px', width: '500px'}),
          numSamplesDisplay = panel.append('div').style({top: "295px", left: "50px", 'font-size': '18px', width: '500px'}).text('Number of Samples:'),
          rectangleElement = canvas
            .append('rect')
            .attr('class', 'selection')
            .style({
              "stroke"          : "gray",
              "stroke-width"    : "1px",
              "stroke-dasharray": "4px",
              "stroke-opacity"  : "0.5",
              "fill"            : "gray",
              "opacity"         : "0.3",
            });

      canvas.attr("transform", svgTranslate(110, 40));

      var xMinC = $('<input/>', {
        type: "text",
        placeholder: "x-min",
        style: "left: 0px; top: 70px",
      }).attr('size', '8');
      var xMaxC = $('<input/>', {
        type: "text",
        placeholder: "x-max",
        style: "left: 180px; top: 70px",
      }).attr('size', '8');
      var yMinC = $('<input/>', {
        type: "text",
        placeholder: "y-min",
        style: "left: 90px; top: 140px",
      }).attr('size', '8');
      var yMaxC = $('<input/>', {
        type: "text",
        placeholder: "y-max",
        style: "left: 90px; top: 0px",
      }).attr('size', '8');
      var numSamplesC = $('<input/>', {
        type: "text",
        placeholder: "num-samples",
        style: "left: 90px; top: 240px",
      }).attr('size', '8');

      controller
        .append(xMinC)
        .append(xMaxC)
        .append(yMinC)
        .append(yMaxC)
        .append(numSamplesC);

      var prettyNumToStringDigits20 = libNum.getPrettyNumToStringDigits(20);
      var prettyNumToStringDigits9 = libNum.getPrettyNumToStringDigits(9);

      function setDefault() {
        xMinC.val(prettyNumToStringDigits20(xMin));
        xMaxC.val(prettyNumToStringDigits20(xMax));
        yMinC.val(prettyNumToStringDigits20(yMin));
        yMaxC.val(prettyNumToStringDigits20(yMax));
      }

      numSamplesC.val(rt.num_to_string(gf(windowOptions, "num-samples")));

      setDefault();

      function getNewWindow() {
        var ret = rt.ffi.cases(rt.ffi.isOption, "Option", rt.string_to_number(xMinC.val()), {
          none: function() {
            xMinC.addClass('error-bg');
            xMinC.removeClass('ok-bg');
            return null;
          },
          some: function(xMin_val) {
            xMinC.removeClass('error-bg');
            xMinC.addClass('ok-bg');
            return rt.ffi.cases(rt.ffi.isOption, "Option", rt.string_to_number(xMaxC.val()), {
              none: function() {
                xMaxC.addClass('error-bg');
                xMaxC.removeClass('ok-bg');
                return null;
              },
              some: function(xMax_val) {
                xMaxC.removeClass('error-bg');
                xMaxC.addClass('ok-bg');

                if(jsnums.greaterThanOrEqual(xMin_val, xMax_val)) {
                  xMinC.addClass('error-bg');
                  xMaxC.addClass('error-bg');
                  xMinC.removeClass('ok-bg');
                  xMaxC.removeClass('ok-bg');
                  return null;
                }

                return rt.ffi.cases(rt.ffi.isOption, "Option", rt.string_to_number(yMinC.val()), {
                  none: function() {
                    yMinC.addClass('error-bg');
                    yMinC.removeClass('ok-bg');
                    return null;
                  },
                  some: function(yMin_val) {
                    yMinC.removeClass('error-bg');
                    yMinC.addClass('ok-bg');

                    return rt.ffi.cases(rt.ffi.isOption, "Option", rt.string_to_number(yMaxC.val()), {
                      none: function() {
                        yMaxC.addClass('error-bg');
                        yMaxC.removeClass('ok-bg');
                        return null;
                      },
                      some: function(yMax_val) {
                        yMaxC.removeClass('error-bg');
                        yMaxC.addClass('ok-bg');

                        if(jsnums.greaterThanOrEqual(xMin_val, xMax_val)) {
                          yMinC.addClass('error-bg');
                          yMaxC.addClass('error-bg');
                          yMinC.removeClass('ok-bg');
                          yMaxC.removeClass('ok-bg');
                          return null;
                        }

                        return rt.ffi.cases(rt.ffi.isOption, "Option", rt.string_to_number(numSamplesC.val()), {
                          none: function() {
                            numSamplesC.addClass('error-bg');
                            numSamplesC.removeClass('ok-bg');
                            return null;
                          },
                          some: function(numSamples_val) {
                            numSamplesC.removeClass('error-bg');
                            numSamplesC.addClass('ok-bg');

                            if (rt.isPyretFalse(rt.num_is_integer(numSamples_val)) ||
                                jsnums.lessThanOrEqual(numSamples_val, 1)) {
                              numSamplesC.addClass('error-bg');
                              numSamplesC.removeClass('ok-bg');
                              return null;
                            }

                            return {
                              "x-min": xMin_val,
                              "x-max": xMax_val,
                              "y-min": yMin_val,
                              "y-max": yMax_val,
                              "num-samples": numSamples_val,
                              "infer-bounds": rt.pyretTrue,
                            };
                          }
                        });
                      }
                    });
                  }
                });
              }
            });
          }
        });

        detached.selectAll('.error-bg').style({'background-color': '#FF9494'});
        detached.selectAll('.ok-bg').style({'background-color': '#FFFFFF'});
        return ret;
      }

      controller.append($('<button/>', {
        text: '⇦',
        style: "left: 100px; top: 70px",
      }).addClass('xMinGo d3btn').click(function() {
        if (rectangleElement.attr('style').indexOf('visible') >= 0) {
          rectangleElement.style({visibility: 'hidden'});
        }
        var newWindow = getNewWindow();
        if (newWindow === null) { return; }
        var xMin_val = newWindow['x-min'];
        var xMax_val = newWindow['x-max'];
        var move = jsnums.divide(jsnums.subtract(xMax_val, xMin_val), 10);
        xMinC.val(prettyNumToStringDigits20(jsnums.subtract(xMin_val, move)));
        xMaxC.val(prettyNumToStringDigits20(jsnums.subtract(xMax_val, move)));
      }));
      controller.append($('<button/>', {
        text: '⇨',
        style: "left: 140px; top: 70px",
      }).addClass('xMaxGo d3btn').click(function() {
        if (rectangleElement.attr('style').indexOf('visible') >= 0) {
          rectangleElement.style({visibility: 'hidden'});
        }
        var newWindow = getNewWindow();
        if (newWindow === null) { return; }
        var xMin_val = newWindow['x-min'];
        var xMax_val = newWindow['x-max'];
        var move = jsnums.divide(jsnums.subtract(xMax_val, xMin_val), 10);
        xMinC.val(prettyNumToStringDigits20(jsnums.add(xMin_val, move)));
        xMaxC.val(prettyNumToStringDigits20(jsnums.add(xMax_val, move)));
      }));
      controller.append($('<button/>', {
        text: '⇩',
        style: "left: 120px; top: 105px",
      }).addClass('yMinGo d3btn').click(function() {
        if (rectangleElement.attr('style').indexOf('visible') >= 0) {
          rectangleElement.style({visibility: 'hidden'});
        }
        var newWindow = getNewWindow();
        if (newWindow === null) { return; }
        var yMin_val = newWindow['y-min'];
        var yMax_val = newWindow['y-max'];
        var move = jsnums.divide(jsnums.subtract(yMax_val, yMin_val), 10);
        yMinC.val(prettyNumToStringDigits20(jsnums.subtract(yMin_val, move)));
        yMaxC.val(prettyNumToStringDigits20(jsnums.subtract(yMax_val, move)));
      }));
      controller.append($('<button/>', {
        text: '⇧',
        style: "left: 120px; top: 35px",
      }).addClass('yMaxGo d3btn').click(function() {
        if (rectangleElement.attr('style').indexOf('visible') >= 0) {
          rectangleElement.style({visibility: 'hidden'});
        }
        var newWindow = getNewWindow();
        if (newWindow === null) { return; }
        var yMin_val = newWindow['y-min'];
        var yMax_val = newWindow['y-max'];
        var move = jsnums.divide(jsnums.subtract(yMax_val, yMin_val), 10);
        yMinC.val(prettyNumToStringDigits20(jsnums.add(yMin_val, move)));
        yMaxC.val(prettyNumToStringDigits20(jsnums.add(yMax_val, move)));
      }));

      var redraw = $('<button/>', {
        text: 'Redraw', style: 'left: 95px; top: 295px'
      });

      controller.append(redraw);

      $(panel.node())
        .css('position', 'absolute')
        .children()
        .css('position', 'absolute')
        .children()
        .css('position', 'absolute');

      appendAxis(xMin, xMax, yMin, yMax, width, height, canvas);

      var xToPixel = libNum.scaler(xMin, xMax, 0, width - 1, true),
          yToPixel = libNum.scaler(yMin, yMax, height - 1, 0, true),
          pixelToX = libNum.scaler(0, width - 1, xMin, xMax, false),
          pixelToY = libNum.scaler(height - 1, 0, yMin, yMax, false);;

      // from http://jsfiddle.net/dirtyd77/4Qm6A/7/

      var rectData, isDown = false;

      function updateRect() {  ;
        rectangleElement.attr({
          x: rectData[1].x - rectData[0].x > 0 ? rectData[0].x :  rectData[1].x,
          y: rectData[1].y - rectData[0].y > 0 ? rectData[0].y :  rectData[1].y,
          width: Math.abs(rectData[1].x - rectData[0].x),
          height: Math.abs(rectData[1].y - rectData[0].y)
        });
      }

      canvas
        .append('rect')
        .attr('width', width)
        .attr('height', height)
        .attr('class', 'overlay')
        .on("click", function() {
          if (!d3.event.shiftKey) { return; }

          var coord = d3.mouse(this);
          var cx = pixelToX(coord[0]);
          var radiusX = jsnums.subtract(xMax, xMin);
          var cy = pixelToY(coord[1]);
          var radiusY = jsnums.subtract(yMax, yMin);

          xMinC.val(prettyNumToStringDigits20(jsnums.subtract(cx, radiusX)));
          xMaxC.val(prettyNumToStringDigits20(jsnums.add(cx, radiusX)));
          yMinC.val(prettyNumToStringDigits20(jsnums.subtract(cy, radiusY)));
          yMaxC.val(prettyNumToStringDigits20(jsnums.add(cy, radiusY)));

        })
        .on("mousedown", function() {
          if (isDown) { return; }
          if (d3.event.shiftKey) { return; }

          // prevent bad dragging; disable to make canvas focusable
          // d3.event.preventDefault();

          var m1 = d3.mouse(this);
          rectData = [ { x: m1[0], y: m1[1] }, { x: m1[0], y: m1[1] } ];
          updateRect();
          rectangleElement.style({visibility: 'visible'});
          isDown = true;
        })
        .on("mousemove", function(){

          var coord = d3.mouse(this);
          var vX = pixelToX(coord[0]);
          var vY = pixelToY(coord[1]);

          coordDisplay.html('x: ' + prettyNumToStringDigits20(vX) + '<br/><br/>' +
                            'y: ' + prettyNumToStringDigits20(vY));

          if(isDown) {
            rectData[1] = { x: coord[0], y: coord[1] };
            updateRect();
          }
        })
        .on("mouseup", function() {
          if (rectData[0].x == rectData[1].x &&
              rectData[0].y == rectData[1].y &&
              rectangleElement.attr('style').indexOf('visible') >= 0) {
            setDefault();
            rectangleElement.style({visibility: 'hidden'});
          } else {
            xMinC.val(prettyNumToStringDigits20(pixelToX(Math.min(rectData[0].x, rectData[1].x))));
            xMaxC.val(prettyNumToStringDigits20(pixelToX(Math.max(rectData[0].x, rectData[1].x))));
            yMinC.val(prettyNumToStringDigits20(pixelToY(Math.max(rectData[0].y, rectData[1].y))));
            yMaxC.val(prettyNumToStringDigits20(pixelToY(Math.min(rectData[0].y, rectData[1].y))));
          }
          isDown = false;
        });

      function plotLine(plot) {
        /*
         * Graph a line
         *
         * Part of this function is adapted from
         * http://jsfiddle.net/christopheviau/Hwpe3/
         */

        var options = plot.options;
        var points = plot.line;

        var line = d3.svg.line()
          .x(function (d) { return xToPixel(d[0]); })
          .y(function (d) { return yToPixel(d[1]); });

        canvas
          .append("path")
          .attr("d", line(points))
          .style({'stroke': options.color, 'stroke-width': 1, 'fill': 'none'});
      }

      function plotPoints(points) {
        /*
         * Plot data points (scatter plot)
         *
         * Part of this function is adapted from
         * http://alignedleft.com/tutorials/d3/making-a-scatterplot
         */

        var tip = d3tip(detached)
            .attr('class', 'd3-tip')
            .direction('e')
            .offset([0, 20])
            .html(function (d) {
              var x = prettyNumToStringDigits9(d[0]);
              var y = prettyNumToStringDigits9(d[1]);
              return "x: " + x.toString() + "<br />" +
                     "y: " + y.toString() + "<br />";
            });

        canvas.call(tip);

        canvas
          .selectAll("circle")
          .data(points)
          .enter()
          .append("circle")
          .attr("cx", function (d) { return xToPixel(d[0]); })
          .attr("cy", function (d) { return yToPixel(d[1]); })
          .attr("r", function(d) { return d[2].size })
          .style('fill', function(d) { return d[2].color; })
          .style('opacity', function(d) { return d[2].opacity; })
          .on("mouseover", function(d) {
            if (d[2].tip) {
              tip.show.apply(this, arguments);
            }
          })
          .on("mouseout", function(d) {
            if (d[2].tip) {
              tip.hide.apply(this, arguments);
            }
          });

      }


      plotPoints(scatterPlots);
      linePlots.forEach(plotLine);

      stylizeTip(detached);

      callBigBang(rt, 970, 650, detached, rt.ffi.makeNone(), function(restarter) {
        redraw.click(function() {
          var newWindow = getNewWindow();
          if (newWindow === null) { return; }
          var toRet = rt.ffi.makeSome(rt.makeObject(newWindow));
          rt.getParam("remove-d3-port")();
          restarter.resume(toRet);
        });
      });

    }

    function plotMulti(lstOfScatterPlots, lstOfLinePlots, windowOptions) {
      var xMin = gf(windowOptions, "x-min");
      var xMax = gf(windowOptions, "x-max");
      var yMin = gf(windowOptions, "y-min");
      var yMax = gf(windowOptions, "y-max");

      function inBound(p) {
        return jsnums.lessThanOrEqual(xMin, p[0]) &&
               jsnums.lessThanOrEqual(p[0], xMax) &&
               jsnums.lessThanOrEqual(yMin, p[1]) &&
               jsnums.lessThanOrEqual(p[1], yMax);
      }

      function dist(a, b) {
        return jsnums.add(
          jsnums.sqr(jsnums.subtract(a[0], b[0])),
          jsnums.sqr(jsnums.subtract(a[1], b[1])));
      }

      function nearest(candidates, origin) {
        var ans = null;
        var optimal = null;
        candidates.forEach(function(candidate) {
          var distance = dist(candidate, origin);
          if (optimal === null || jsnums.lessThan(distance, optimal)) {
            optimal = distance;
            ans = candidate;
          }
        });
        return ans;
      }

      function equal(a, b) {
        return jsnums.lessThanOrEqual(a, b) && jsnums.lessThanOrEqual(b, a);
      }

      function findPointOnEdge(near, far) {
        /*
        Find a Posn on the border and on the line between `near` and `far`. If there are many,
        pick the one closest to `near`.

        Precondition: at least one of `near` or `far` is not in the border.
        */

        var pxMax = rt.num_min(rt.num_max(near[0], far[0]), xMax);
        var pxMin = rt.num_max(rt.num_min(near[0], far[0]), xMin);
        var pyMax = rt.num_min(rt.num_max(near[1], far[1]), yMax);
        var pyMin = rt.num_max(rt.num_min(near[1], far[1]), yMin);

        var candidates = [];
        if (equal(near[0], far[0])) {
          candidates = [
            [near[0], yMin],
            [near[0], yMax]
          ];
        } else {
          /*
          y = m * x + c           [3]
          y2 = m * x2 + c         [3.1]
          y - y2 = m * (x - x2)   [5]   [by 3 - 3.1]
          m = (y - y2) / (x - x2) [1]   [rewrite 5]
          c = y - m * x           [2]   [rewrite 3]
          x = (y - c) / m         [4]   [rewrite 3]
          */

          var m = jsnums.divide(jsnums.subtract(near[1], far[1]), jsnums.subtract(near[0], far[0]));
          var c = jsnums.subtract(near[1], jsnums.multiply(m, near[0]));

          function f(x) {
            return jsnums.add(jsnums.multiply(m, x), c);
          }

          function g(y) {
            return jsnums.divide(jsnums.subtract(y, c), m);
          }

          candidates = [
            [xMin, f(xMin)],
            [xMax, f(xMax)]
          ];

          if (!equal(m, 0)) {
            candidates = candidates.concat([
              [g(yMin), yMin],
              [g(yMax), yMax]
            ]);
          }
        }

        return nearest(candidates.filter(inBound), near);
      }

      function pointEqual(x, y) {
        return equal(x[0], y[0]) && equal(x[1], y[1]);
      }

      function toJSOptions(options) {
				return {
          color:   colorConverter(gf(options, "color")),
          size:    jsnums.toFixnum(gf(options, "size")),
          opacity: jsnums.toFixnum(gf(options, "opacity")),
          tip:     rt.isPyretTrue(gf(options, "tip")),
        };
			}

      var scatterPoints = [];
      rt.ffi.toArray(lstOfScatterPlots).forEach(function(scatterPlot) {
        var points = gf(scatterPlot, "points");
        var options = toJSOptions(gf(scatterPlot, "options"));
        points.forEach(function(point) {
          if (inBound(point)) {
            scatterPoints.push(point.concat([options]));
          }
        });
      });

      var linePlots = [];

      rt.ffi.toArray(lstOfLinePlots).forEach(function(linePlot) {
        var points = gf(linePlot, "points");
        var options = toJSOptions(gf(linePlot, "options"));

        var segments = [];
        for (var i = 0; i < points.length - 1; i++) {
          var start = points[i];
          var stop = points[i + 1];

          if (inBound(start)) {
            if (inBound(stop)) {
              segments.push([start, stop]);
            } else {
              segments.push([start, findPointOnEdge(start, stop)]);
            }
          } else {
            if (inBound(stop)) {
              segments.push([findPointOnEdge(start, stop), stop]);
            } else {
              var result = findPointOnEdge(start, stop);
              if (result !== null) {
                var result2 = findPointOnEdge(stop, start);
                segments.push([result, result2]);
              }
            }
          }
        }

        var combined = [segments[0]];
        for (var i = 1; i < segments.length; i++) {
          var currentSegment = segments[i];
          var lastSegment = combined[combined.length - 1]
          var lastPoint = lastSegment[lastSegment.length - 1];
          if (pointEqual(lastPoint, currentSegment[0])) {
            lastSegment.push(currentSegment[1]);
          } else {
            combined.push(currentSegment);
          }
        }

        combined.forEach(function(segment) {
          linePlots.push({
            line: segment,
            options: options
          })
        });
      });

      return genericPlot(scatterPoints, linePlots, windowOptions);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    function histogram(tab, n) {
      var data = tab.map(function(row){ return row[0]; });
      var xMin = data.reduce(rt.num_min);
      var xMax = data.reduce(rt.num_max);
      var dataScaler = libNum.scaler(xMin, xMax, 0, HISTOGRAM_N, false);

      var histogramData = d3.layout.histogram()
          .bins(n).value(function (val) {
            return jsnums.toFixnum(dataScaler(val));
          })(data);

      var yMax = d3.max(histogramData, function (d) { return d.y; });

      var marginType = "normal",
        margin = getMargin(marginType),
        dimension = getDimension(margin),
        width = dimension.width,
        height = dimension.height;

      var detached = createDiv();
      var canvas = createCanvas(detached, margin, "top-left");

      appendAxis(xMin, xMax, 0, yMax, width, height, canvas);

      plotBar(xMin, xMax, 0, yMax, width, height,
          histogramData, HISTOGRAM_N, detached, canvas);

      callBigBang(rt, undefined, undefined, detached, tab, function(){});
    }

    function pieChart(tab) {
      /*
       * Part of this function is adapted from:
       * http://bl.ocks.org/mbostock/3887235
       *
       * row[0] => label, row[1] => value
       */

      if (tab.length === 0) {
        rt.throwMessageException("There must be at least " +
                                 "one entry in the list.");
      }

      var sum = tab.map(function (row) { return row[1]; }).reduce(jsnums.add);
      var scaler = libNum.scaler(0, sum, 0, 100, true);

      var marginType = "normal",
        margin = getMargin(marginType),
        dimension = getDimension(margin),
        width = dimension.width,
        height = dimension.height;

      var radius = Math.min(width, height) / 2;
      var color = d3.scale.category20();
      var arc = d3.svg.arc()
          .outerRadius(radius)
          .innerRadius(0);

      var pie = d3.layout.pie()
          .sort(null)
          .value(function (row) { return row[1]; });

      var detached = createDiv();

      var prettyNumToStringDigits9 = libNum.getPrettyNumToStringDigits(9);

      var tip = d3tip(detached)
          .attr('class', 'd3-tip')
          .direction('e')
          .offset([0, 20])
          .html(function (d) {
            return "value: <br />" + prettyNumToStringDigits9(d.data[1]) + "<br />" +
                   "percent: <br />" + prettyNumToStringDigits9(scaler(d.data[1])) + "%";
          });

      var canvas = createCanvas(detached, margin, "center");
      canvas.call(tip);

      var g = canvas.selectAll(".arc")
          .data(pie(tab))
          .enter().append("g")
          .attr("class", "arc");

      g.append("path").attr("class", "path").attr("d", arc);

      g.append("text")
        .attr("transform", function (d) {
          return "translate(" + arc.centroid(d) + ")";
        })
        .attr("dy", ".35em")
        .style({
          "text-anchor": "middle"
        })
        .text(function (d) { return d.data[0]; });

      g.append("path").attr("class", "transparent").attr("d", arc);

      stylizeTip(detached);

      canvas.selectAll(".arc path")
        .style({
          "fill": function (d, i) { return color(i); }
        })
        .on("mouseover", function (e) {
          d3.select(this.parentNode)
            .selectAll(".path")
            .style('opacity', '0.4');
          tip.show(e);
        })
        .on("mouseout", function (e) {
          d3.select(this.parentNode)
            .selectAll(".path")
            .style('opacity', '0.9');
          tip.hide(e);
        });

      canvas.selectAll(".transparent").style('opacity', '0');
      canvas.selectAll('text').style({'font-size': '15px'});
      callBigBang(rt, undefined, undefined, detached, tab, function(){});
    }

    return util.makeModuleReturn(rt, {}, {
      "histogram": rt.makeFunction(histogram),
      "pie-chart": rt.makeFunction(pieChart),
      "plot-multi": rt.makeFunction(plotMulti)
    });
  };
});
