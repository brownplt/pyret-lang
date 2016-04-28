function Plot(type, points, f, option) {
  this.type = type;
  this.points = points;
  this.f = f;
  this.option = option;
}

Plot.LINE =  0;
Plot.SCATTER = 1;
Plot.XY = 2;

define(["js/runtime-util", "js/js-numbers", "trove/either", "trove/option",
        "trove/string-dict", "trove/image-lib", "trove/image-structs",
        "trove/d3-lib", "trove/plot-structs",
        "../../../node_modules/d3/d3.min",
        "../../../node_modules/d3-tip/index"],
        function(util, jsnums, eitherLib, optionLib, sdLib, imageLib, imageStructs,
                 clib, plotStructs, d3, d3tipLib) {

  var HISTOGRAM_N = 100;
  var CError = {
    "RANGE": "x-min and y-min must be strictly less than " +
    "x-max and y-max respectively."
  };
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

    var tickNum = 7;

    var xAxisScaler = d3.scale.linear()
        .domain([0, tickNum - 1]).range([0, width - 1]),
      yAxisScaler = d3.scale.linear()
        .domain([0, tickNum - 1]).range([height - 1, 0]);

    var allValues = d3.range(0, tickNum);

    var xAxisDisplayScaler = libNum.scaler(0, tickNum - 1, xMin, xMax),
      yAxisDisplayScaler = libNum.scaler(0, tickNum - 1, yMin, yMax);

    var xAxis = d3.svg.axis().scale(xAxisScaler)
        .orient((xAxisConf.pos === 0) ? "top" : "bottom")
        .tickValues(allValues).tickFormat(
          function (d, i) {
            return libNum.format(xAxisDisplayScaler(i), 10);
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
            return libNum.format(yAxisDisplayScaler(i), 10);
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

    var tip = d3tip(detached)
        .attr('class', 'd3-tip')
        .direction('e')
        .offset([0, 20])
        .html(function (d) {
          var maxVal = libNum.format(d.reduce(libNum.max), 6);
          var minVal = libNum.format(d.reduce(libNum.min), 6);
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

  function putLabel(label, width, height, detached, margin) {
    var supportedTags = [
      ["<sup>", "<tspan baseline-shift='super'>"],
      ["</sup>", "</tspan>"],
      ["<sub>", "<tspan baseline-shift='sub'>"],
      ["</sub>", "</tspan>"]
    ];

    var processedLabel = supportedTags.reduce(function (prev, current) {
      return prev.replace(
        libJS.htmlspecialchars(current[0]), current[1]);
    }, libJS.htmlspecialchars(label));

    var legend = detached.select('.maing')
        .append("text")
        .attr("x", (margin.left + width + margin.right) / 2)
        .attr("y", height + margin.top + 30)
        .html(processedLabel);

    legend.style({
      'position': 'absolute',
      'font-size': '8pt',
      'text-anchor': 'middle'
    });
  }

  function inferBounds(arrOfPlot) {
    var dataPoints = libData.flatten(
      arrOfPlot
        .filter(function(plot){ return plot.type != Plot.XY; })
        .map(function(plot){ return plot.points; }));

    var xMin, xMax, yMin, yMax;

    if (dataPoints.length === 0) {
      xMin = -10;
      xMax = 10;
      yMin = -10;
      yMax = 10;
    } else {
      xMin = dataPoints
        .map(function (d) { return d.x; })
        .reduce(libNum.min);
      xMax = dataPoints
        .map(function (d) { return d.x; })
        .reduce(libNum.max);
      yMin = dataPoints
        .map(function (d) { return d.y; })
        .reduce(libNum.min);
      yMax = dataPoints
        .map(function (d) { return d.y; })
        .reduce(libNum.max);

      var blockPortion = 10;
      var xOneBlock = jsnums.divide(jsnums.subtract(xMax, xMin),
                      blockPortion);
      var yOneBlock = jsnums.divide(jsnums.subtract(yMax, yMin),
                      blockPortion);

      xMin = jsnums.subtract(xMin, xOneBlock);
      xMax = jsnums.add(xMax, xOneBlock);
      yMin = jsnums.subtract(yMin, yOneBlock);
      yMax = jsnums.add(yMax, yOneBlock);

      // Plotting 1 point should be possible
      // but we need a wider range
      if (jsnums.equals(xMin, xMax)) {
        xMin = jsnums.subtract(xMin, 1);
        xMax = jsnums.add(xMax, 1);
      }
      if (jsnums.equals(yMin, yMax)) {
        yMin = jsnums.subtract(yMin, 1);
        yMax = jsnums.add(yMax, 1);
      }
    }
    return {xMin: xMin, xMax: xMax, yMin: yMin, yMax: yMax};
  }

  function isInBoundGenerator(xMin, xMax, yMin, yMax){
    return function(point) {
      return jsnums.lessThanOrEqual(xMin, point.x) &&
             jsnums.lessThanOrEqual(point.x, xMax) &&
             jsnums.lessThanOrEqual(yMin, point.y) &&
             jsnums.lessThanOrEqual(point.y, yMax);
    };
  }

  function adjustLinePlot(points, option, xMin, xMax, yMin, yMax) {
    var isPointInBound = points.map(isInBoundGenerator(xMin, xMax, yMin, yMax));
    var newPoints = [[]];

    for(var i = 0; i < points.length; i++) {
      var subArr = libData.lastElement(newPoints);
      if (i > 0 && isPointInBound[i - 1] && !isPointInBound[i]) {
        subArr.push(libNum.calcPointOnEdge(points[i - 1], points[i],
                                           xMin, xMax, yMin, yMax));
        newPoints.push([]);
      } else if (i > 0 && !isPointInBound[i - 1] && isPointInBound[i]) {
        subArr.push(libNum.calcPointOnEdge(points[i], points[i - 1],
                                           xMin, xMax, yMin, yMax));
        subArr.push(points[i]);
      } else if (isPointInBound[i]) {
        subArr.push(points[i]);
      }
    }

    return newPoints.map(function(pts) {
      return new Plot(Plot.LINE, pts, undefined, option);
    });
  }

  return function(rt, namespace) {

  var gf = rt.getField;
  var IMAGE = imageLib(rt, rt.namespace);
  var colorConverter = libColor.convertColor(rt, IMAGE);

  return rt.loadModulesNew(namespace, [sdLib, eitherLib, optionLib, imageStructs, plotStructs],
    function(SD, EITHER, OPTION, IMAGESTRUCTS, STRUCTS) {

    function valFromStructs(name){
      return gf(gf(STRUCTS, "values"), name);
    }

    function typeFromStructs(name){
      return gf(STRUCTS, "types")[name];
    }

    var PyretEither = gf(gf(EITHER, "values"), "is-Either");
    var PyretBlue = gf(gf(IMAGESTRUCTS, "values"), "blue")
    var PyretPlot = valFromStructs("is-Plot");
    var TypePlotWindowOptions = typeFromStructs("PlotWindowOptions");

    function toJSPoints(points) {
      return rt.ffi.toArray(points).map(
        function (e) { return {x: gf(e, "x"), y: gf(e, "y")}; }
      );
    }

    function toJSOption(option) {
      return { color: colorConverter(gf(option, "color")) }
    }

    function genericPlot(arrayOfPlot, windowOption) {
      var xMin = windowOption.xMin;
      var xMax = windowOption.xMax;
      var yMin = windowOption.yMin;
      var yMax = windowOption.yMax;

      var isInBound = isInBoundGenerator(xMin, xMax, yMin, yMax);

      var marginType = "normal",
        margin = getMargin(marginType),
        dimension = getDimension(margin),
        width = dimension.width,
        height = dimension.height;

      var detached = createDiv();
      var canvas = createCanvas(detached, margin, "top-left");
      appendAxis(xMin, xMax, yMin, yMax, width, height, canvas);

      var xToPixel = libNum.scaler(xMin, xMax, 0, width - 1, true),
          yToPixel = libNum.scaler(yMin, yMax, height - 1, 0, true);


      function plotLine(plot) {
        /*
         * Graph a line
         *
         * Part of this function is adapted from
         * http://jsfiddle.net/christopheviau/Hwpe3/
         */


        var line = d3.svg.line()
            .x(function (d) { return xToPixel(d.x); })
            .y(function (d) { return yToPixel(d.y); });

        canvas.append("path")
          .attr("d", line(plot.points))
          .style({'stroke': plot.option.color, 'stroke-width': 1, 'fill': 'none'});
      }

      function plotPoint(plot) {
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
              var x = libNum.format(d.x, 6);
              var y = libNum.format(d.y, 6);
              return "x: " + x.toString() + "<br />" +
                     "y: " + y.toString() + "<br />";
            });

        canvas.call(tip);

        canvas.selectAll("circle")
          .data(plot.points.filter(isInBound))
          .enter()
          .append("circle")
          .attr("cx", function (d) { return xToPixel(d.x); })
          .attr("cy", function (d) { return yToPixel(d.y); })
          .attr("r", 2)
          .style({'fill': plot.option.color})
          .on("mouseover", tip.show)
          .on("mouseout", tip.hide);
      }

      arrayOfPlot.forEach(function (plot) {
        switch (plot.type) {
        case Plot.LINE:
          plotLine(plot)
          break;
        case Plot.SCATTER:
          plotPoint(plot)
          break;
        }
      });

      putLabel(windowOption.label, width, height, detached, margin);
      stylizeTip(detached);
      callBigBang(rt, detached);
    }

    function generateXY(f, option, isSafe) {
      var xMin = option.xMin;
      var xMax = option.xMax;
      var yMin = option.yMin;
      var yMax = option.yMax;

      var marginType = "normal",
        margin = getMargin(marginType),
        dimension = getDimension(margin),
        width = dimension.width,
        height = dimension.height,
        K = 702,       // TODO: optimal?
        DELTA = 0.001; // TODO: not a good treshold

      var inputScaler = libNum.scaler(
          0, width - 1, xMin, xMax, false),

        outputScaler = libNum.scaler(
          yMin, yMax, height - 1, 0, false);

      var xToPixel = libNum.scaler(xMin, xMax, 0, width - 1, true);
      var yToPixel = libNum.scaler(yMin, yMax, height - 1, 0, true);

      logtable = [];
      for(var i = 0; i < width; ++i) {
        logtable.push(new libData.LogTable(height));
      }

      function occupy(pt) {
        logtable[Math.floor(pt.px)].occupy(Math.floor(pt.py));
      }

      function isAllOccupied(left, right) {
        return (Math.floor(left.px) == Math.floor(right.px)) &&
                logtable[Math.floor(left.px)].isRangedOccupied(
                  Math.floor(left.py), Math.floor(right.py)
                );
      }

      function closeEnough(coordA, coordB) {
        return ((Math.abs(coordA.py - coordB.py) <= 1) &&
               (coordB.px - coordA.px <= 1));
      }

      function tooClose(coordA, coordB) {
        return jsnums.roughlyEquals(coordA.px, coordB.px, DELTA);
      }

      if (isSafe) {

        // safe version

        function makePoint(x, done) {
          // This function create a point `pt`. It then returns done(pt).
          // x :: Number
          // done :: (Number -> Any)
          var pt = {y: NaN, py: NaN, x: x, px: xToPixel(x)};
          return rt.safeCall(function() {
            return rt.execThunk({ app: function(){ return f.app(x); }});
          }, function(result) {
            rt.ffi.cases(PyretEither, "Either", result, {
              left: function(val){
                if (jsnums.isReal(val) &&
                    jsnums.lessThanOrEqual(yMin, val) &&
                    jsnums.lessThanOrEqual(val, yMax)) {

                    pt.y = val;
                    pt.py = yToPixel(val);
                }
                return 0;
              },
              right: function(_){ return 0; }
            });
            return done(pt)
          });
        }

        function makePoints(i, scalerSubinterval, done) {
          if(i == -1) return done([]);
          return makePoint(scalerSubinterval(i), function(pt) {
            return makePoints(i - 1, scalerSubinterval, function(points) {
              points.push(pt);
              return done(points);
            });
          })
        }

        function makeIntervals(i, points, left, right, done) {
          if(i == -1) return done([]);
          if(Number.isNaN(points[i].y) && Number.isNaN(points[i + 1].y)) {
            return makeIntervals(i - 1, points, left, right, done);
          }
          return rt.safeCall(function() {
            return divideSubinterval(points[i], points[i + 1])
          }, function(divided) {
            if (isAllOccupied(left, right)) return [[left, right]];
            return makeIntervals(i - 1, points, left, right, function(interv) {
              interv.push(divided);
              return done(interv);
            });
          });
        }

        function divideSubinterval(left, right) {
          // Input: two X values
          // Output: list of [2-length long list of points]
          // Note: invalid for two ends is still okay
          // invalid for K points indicate that it should not be plotted!

          occupy(left);
          occupy(right);

          if (closeEnough(left, right)) return [[left, right]];
          if (tooClose(left, right)) return [];
          if (isAllOccupied(left, right)) return [[left, right]];

          return makePoints(K - 1, libNum.scaler(0, K - 1, left.x, right.x, false), function(points) {
            return makeIntervals(K - 2, points, left, right, libData.flatten);
          });
        }

        return makePoint(xMin, function(leftPoint) {
          return makePoint(xMax, function(rightPoint) {
            return divideSubinterval(leftPoint, rightPoint);
          });
        });

      } else {

        // unsafe version

        function PointCoord(x) {
          this.x = x;
          this.px = xToPixel(x);
          this.y = NaN;
          this.py = NaN;

          var y;

          try {
            y = f.app(x);
            if (jsnums.isReal(y) &&
                jsnums.lessThanOrEqual(yMin, y) &&
                jsnums.lessThanOrEqual(y, yMax)) {
              this.y = y;
              this.py = yToPixel(y);
            }
          } catch(e) {
            // ignore error
          }
          return this;
        }

        function divideSubintervalUnsafe(left, right, depth) {
          /*
          Input: two X values
          Output: list of [2-length long list of points]
          Note: invalid for two ends is still okay
          invalid for K points indicate that it should not be plotted!
          */

          occupy(left);
          occupy(right);

          if (closeEnough(left, right)) return [[left, right]];
          if (tooClose(left, right)) return [];
          if (isAllOccupied(left, right)) return [[left, right]];

          var scalerSubinterval = libNum.scaler(
            0, K - 1, left.x, right.x, false);

          var points = libData.range(0, K).map(function (i) {
            return new PointCoord(scalerSubinterval(i));
          });

          var intervals = [];
          for (var v = 0; v < K - 1; v++) {
            if (Number.isNaN(points[v].y) && Number.isNaN(points[v + 1].y)) {
              continue;
            }
            intervals.push(divideSubintervalUnsafe(
              points[v], points[v + 1], depth + 1
            ));
            if (isAllOccupied(left, right)) return [[left, right]];
          }
          return libData.flatten(intervals);
        }

        return divideSubintervalUnsafe(new PointCoord(xMin),
                                       new PointCoord(xMax), 0);

      }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    function plotMulti(pyretLstOfPlot, pyretWinOptions) {
      rt.checkArity(2, arguments, "plot-multi");
      rt.checkList(pyretLstOfPlot);

      var checkPlotWindowOptions = function(v) {
        rt._checkAnn(["PlotWindowOptions"], TypePlotWindowOptions, v);
      };

      checkPlotWindowOptions(pyretWinOptions);

      var checkListofPlot = libCheck.checkListGenerator(
        "Plot", PyretPlot.app, rt);
      checkListofPlot(pyretLstOfPlot);

      var xMin = gf(pyretWinOptions, "x-min");
      var xMax = gf(pyretWinOptions, "x-max");
      var yMin = gf(pyretWinOptions, "y-min");
      var yMax = gf(pyretWinOptions, "y-max");

      if (jsnums.greaterThanOrEqual(xMin, xMax) ||
          jsnums.greaterThanOrEqual(yMin, yMax)) {
        rt.throwMessageException(CError.RANGE);
      }

      var colorConverter = libColor.convertColor(rt, IMAGE);

      var arrOfArrOfPlot = rt.ffi.toArray(pyretLstOfPlot)
        .map(function(pyretPlot) {
          return rt.ffi.cases(PyretPlot, "Plot", pyretPlot, {
            'line-plot': function(points, option){
              return adjustLinePlot(toJSPoints(points), toJSOption(option),
                                    xMin, xMax, yMin, yMax);
            },
            'scatter-plot': function(points, option) {
              return [new Plot(Plot.SCATTER, toJSPoints(points), undefined, toJSOption(option))];
            },
            'xy-plot': function(f, option){
              return [new Plot(Plot.XY, undefined, f, toJSOption(option))];
            }
          });
        });

      var arrOfPlot = libData.flatten(arrOfArrOfPlot);

      var winOptions = {
        xMin: gf(pyretWinOptions, "x-min"),
        xMax: gf(pyretWinOptions, "x-max"),
        yMin: gf(pyretWinOptions, "y-min"),
        yMax: gf(pyretWinOptions, "y-max"),
        inferBound: rt.isPyretTrue(gf(pyretWinOptions, "infer-bounds")),
        label: gf(pyretWinOptions, "label"),
        safe: gf(pyretWinOptions, "safe")
      };

      if (rt.isPyretTrue(winOptions.inferBound)) {
        bound = inferBound(arrOfPlot);
        winOptions.xMin = bound.xMin;
        winOptions.xMax = bound.xMax;
        winOptions.yMin = bound.yMin;
        winOptions.yMax = bound.yMax;
      }

      var nonXY = arrOfPlot.filter(function(plot){ return plot.type != Plot.XY; });
      var XY =    arrOfPlot.filter(function(plot){ return plot.type == Plot.XY; });

      function makeXYs(i, done) {
        if(i === -1) {
          return done([]);
        }
        return rt.safeCall(function(){
          return generateXY(XY[i].f, winOptions, winOptions.safe);
        }, function(plot) {
          return makeXYs(i - 1, function(XYPlots) {
            XYPlots.push(plot);
            return done(XYPlots);
          });
        });
      }

      return makeXYs(XY.length - 1, function(XYPlots) {
        var newXYPlots = XYPlots.map(function(plot, i) {
          return plot.map(function(points){
            return new Plot(Plot.LINE, points, undefined, XY[i].option);
          });
        });
        genericPlot(libData.flatten(newXYPlots).concat(nonXY), winOptions);
        // xy-plot should be drawn first (so that scatter-plot is visible)
        return pyretLstOfPlot;
      });
    }

    function histogram(lst, n) {
      rt.checkArity(2, arguments, "histogram");
      rt.checkList(lst);
      rt.checkNumber(n);

      var checkListofNumber = libCheck.checkListGenerator(
        "Number", rt.isNumber, rt);
      checkListofNumber(lst);

      if ((!jsnums.isInteger(n)) ||
        (n < 1) ||
        (n > HISTOGRAM_N)) {
        rt.throwMessageException(
          "n must be an interger between 1 and " + HISTOGRAM_N.toString());
      }

      var data = rt.ffi.toArray(lst);

      if (data.length === 0) {
        rt.throwMessageException("There must be at least " +
                                 "one Number in the list.");
      }

      var xMin = data.reduce(libNum.min);
      var xMax = data.reduce(libNum.max);
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

      callBigBang(rt, detached);
      return lst;
    }

    function pieChart(sdValue) {
      /*
       * Part of this function is adapted from:
       * http://bl.ocks.org/mbostock/3887235
       */
      rt.checkArity(1, arguments, "pie-chart");

      var annImmutable = gf(SD, "types").StringDict;

      var checkISD = function (v) {
        rt._checkAnn(["string-dict"], annImmutable, v);
      };

      checkISD(sdValue);
      // TODO: check if all are numbers
      // Pyret currently doesn't have a good way to check this

      var keys = rt.ffi.toArray(gf(sdValue, "keys-list").app());

      if (keys.length === 0) {
        rt.throwMessageException("There must be at least " +
                                 "one entry in the list.");
      }

      var data = keys.map(function (k) {
        return {
          'label': k,
          'value': gf(sdValue, "get-value").app(k)
        };
      });

      var sum = data.map(function (e) { return e.value; })
          .reduce(jsnums.add);

      var scaler = libNum.scaler(0, sum, 0, 100);

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
          .value(function (d) { return d.value; });

      var detached = createDiv();

      var tip = d3tip(detached)
          .attr('class', 'd3-tip')
          .direction('e')
          .offset([0, 20])
          .html(function (d) {
            return "value: <br />" +
              libNum.format(d.data.value, 10) + "<br />" +
              "percent: <br />" +
              libNum.format(
                jsnums.toFixnum(
                  scaler(d.data.value)), 7) + "%";
          });

      var canvas = createCanvas(detached, margin, "center");
      canvas.call(tip);

      var g = canvas.selectAll(".arc")
          .data(pie(data))
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
        .text(function (d) { return d.data.label; });

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
      callBigBang(rt, detached);
      return sdValue;
    }

    return util.makeModuleReturn(rt, {
      Posn: typeFromStructs("Posn"),
      Plot: typeFromStructs("Plot"),
      PlotOptions: typeFromStructs("PlotOptions"),
      PlotWindowOptions: typeFromStructs("PlotWindowOptions")
    }, {
      // TODO: provide is-...?
      "plot-multi": rt.makeFunction(plotMulti),
      "histogram": rt.makeFunction(histogram),
      "pie-chart": rt.makeFunction(pieChart),
      "plot-window-options": rt.makeObject({
        "x-min": -10,
        "x-max": 10,
        "y-min": -10,
        "y-max": 10,
        "infer-bounds": rt.pyretFalse,
        "label": "",
        "safe": rt.pyretTrue
      }),
      "plot-options": rt.makeObject({
        "color": PyretBlue
      }),
      "posn": valFromStructs("posn"),
      "line-plot": valFromStructs("line-plot"),
      "scatter-plot": valFromStructs("scatter-plot"),
      "xy-plot": valFromStructs("xy-plot")
    });
  });

  };
});
