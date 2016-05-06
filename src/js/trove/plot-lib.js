define(["js/runtime-util", "js/js-numbers", "trove/either", "trove/option",
        "trove/string-dict", "trove/image-lib", "trove/image-structs",
        "trove/d3-lib",
        "../../../node_modules/d3/d3.min",
        "../../../node_modules/d3-tip/index"],
        function(util, jsnums, eitherLib, optionLib, sdLib, imageLib, imageStructs,
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

  return function(rt, namespace) {

  var gf = rt.getField;
  var IMAGE = imageLib(rt, rt.namespace);
  var colorConverter = libColor.convertColor(rt, IMAGE);

  return rt.loadModulesNew(namespace, [optionLib, sdLib],
    function(OPTION, SD) {

    function genericPlot(scatterPlots, linePlots, windowOptions) {
      
      var xMin = gf(windowOptions, "x-min");
      var xMax = gf(windowOptions, "x-max");
      var yMin = gf(windowOptions, "y-min");
      var yMax = gf(windowOptions, "y-max");
      
			function toJSPoints(points) {
				return rt.ffi.toArray(points).map(
					function (e) { return {x: gf(e, "x"), y: gf(e, "y")}; }
				);
			}
      
			function toJSOptions(options) {
				return {
          color:   colorConverter(gf(options, "color")),
          size:    jsnums.toFixnum(gf(options, "size")),
          opacity: jsnums.toFixnum(gf(options, "opacity")),
          tip:     rt.isPyretTrue(gf(options, "tip")),
        };
			}

      var marginType = "normal",
        margin = getMargin(marginType),
        dimension = getDimension(margin),
        width = dimension.width,
        height = dimension.height;

      var detached = createDiv();
      
      var divSvg = detached
              .append('div')
              .attr('class', 'divsvg'),
          canvas = divSvg
              .append("svg")
              .attr("width", 601)
              .attr("height", 476)
              .append("g")
              .attr('class', 'maing')
              .append('g'),
          controller = $(detached.append('div').attr('class', 'plot-controller').node());

      canvas.attr("transform", svgTranslate(100, 30));
      
      controller.css({left: "580px", top: "155px"});

      controller.append($('<input/>', {
        type: "text",
        placeholder: "x-min",
        value: rt.num_to_string(xMin),
        style: "left: 0px; top: 70px",
      }).attr('size', '8').addClass('xMin'));
      controller.append($('<input/>', {
        type: "text",
        placeholder: "x-max",
        value: rt.num_to_string(xMax),
        style: "left: 180px; top: 70px",
      }).attr('size', '8').addClass('xMax'));
      controller.append($('<input/>', {
        type: "text",
        placeholder: "y-min",
        value: rt.num_to_string(yMin),
        style: "left: 90px; top: 140px",
      }).attr('size', '8').addClass('yMin'));
      controller.append($('<input/>', {
        type: "text",
        placeholder: "y-max",
        value: rt.num_to_string(yMax),
        style: "left: 90px; top: 0px",
      }).attr('size', '8').addClass('yMax'));
      controller.append($('<input/>', {
        type: "text",
        placeholder: "num-samples",
        value: rt.num_to_string(gf(windowOptions, "num-samples")),
        style: "left: 90px; top: 240px",
      }).attr('size', '8').addClass('numSamples'));
      
      function getNewWindow() {
        var elem1 = $('.xMin').last();
        var ret = rt.ffi.cases(rt.ffi.isOption, "Option", rt.string_to_number(elem1.val()), {
          none: function() {
            elem1.addClass('error-bg');
            elem1.removeClass('ok-bg');
            return null;
          },
          some: function(xMin_val) {
            elem1.removeClass('error-bg');
            elem1.addClass('ok-bg');
            var elem2 = $('.xMax').last();
            return rt.ffi.cases(rt.ffi.isOption, "Option", rt.string_to_number(elem2.val()), {
              none: function() {
                elem2.addClass('error-bg');
                elem2.removeClass('ok-bg');
                return null;
              },
              some: function(xMax_val) {  
                elem2.removeClass('error-bg');
                elem2.addClass('ok-bg');
                
                if(jsnums.greaterThanOrEqual(xMin_val, xMax_val)) {
                  elem1.addClass('error-bg');
                  elem2.addClass('error-bg');
                  elem1.removeClass('ok-bg');
                  elem2.removeClass('ok-bg');
                  return null;
                }
                
                var elem3 = $('.yMin').last();
                return rt.ffi.cases(rt.ffi.isOption, "Option", rt.string_to_number(elem3.val()), {
                  none: function() {
                    elem3.addClass('error-bg');
                    elem3.removeClass('ok-bg');
                    return null;
                  },
                  some: function(yMin_val) {
                    elem3.removeClass('error-bg');
                    elem3.addClass('ok-bg');
                    
                    var elem4 = $('.yMax').last();
                    return rt.ffi.cases(rt.ffi.isOption, "Option", rt.string_to_number(elem4.val()), {
                      none: function() {
                        elem4.addClass('error-bg');
                        elem4.removeClass('ok-bg');
                        return null;
                      },
                      some: function(yMax_val) {
                        elem4.removeClass('error-bg');
                        elem4.addClass('ok-bg');
                        
                        if(jsnums.greaterThanOrEqual(xMin_val, xMax_val)) {
                          elem3.addClass('error-bg');
                          elem4.addClass('error-bg');
                          elem3.removeClass('ok-bg');
                          elem4.removeClass('ok-bg');
                          return null;
                        }
                        
                        var elem5 = $('.numSamples').last();
                        return rt.ffi.cases(rt.ffi.isOption, "Option", rt.string_to_number(elem5.val()), {
                          none: function() {
                            elem5.addClass('error-bg');
                            elem5.removeClass('ok-bg');
                            return null;
                          },
                          some: function(numSamples_val) {
                            elem5.removeClass('error-bg');
                            elem5.addClass('ok-bg');
                            
                            if (rt.isPyretFalse(rt.num_is_integer(numSamples_val))) {
                              elem5.addClass('error-bg');
                              elem5.removeClass('ok-bg');
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
        var newWindow = getNewWindow();
        if (newWindow === null) { return; }
        var xMin_val = newWindow['x-min'];
        var xMax_val = newWindow['x-max'];
        var move = jsnums.divide(jsnums.subtract(xMax_val, xMin_val), 10);
        $('.xMin').last().val(rt.num_to_string(jsnums.subtract(xMin_val, move)));
        $('.xMax').last().val(rt.num_to_string(jsnums.subtract(xMax_val, move)));
        
      }));
      controller.append($('<button/>', {
        text: '⇨',
        style: "left: 140px; top: 70px",
      }).addClass('xMaxGo d3btn').click(function() {
        var newWindow = getNewWindow();
        if (newWindow === null) { return; }
        var xMin_val = newWindow['x-min'];
        var xMax_val = newWindow['x-max'];
        var move = jsnums.divide(jsnums.subtract(xMax_val, xMin_val), 10);
        $('.xMin').last().val(rt.num_to_string(jsnums.add(xMin_val, move)));
        $('.xMax').last().val(rt.num_to_string(jsnums.add(xMax_val, move)));
      }));
      controller.append($('<button/>', {
        text: '⇩',
        style: "left: 120px; top: 105px",
      }).addClass('yMinGo d3btn').click(function() {        
        var newWindow = getNewWindow();
        if (newWindow === null) { return; }
        var yMin_val = newWindow['y-min'];
        var yMax_val = newWindow['y-max'];
        var move = jsnums.divide(jsnums.subtract(yMax_val, yMin_val), 10);
        $('.yMin').last().val(rt.num_to_string(jsnums.subtract(yMin_val, move)));
        $('.yMax').last().val(rt.num_to_string(jsnums.subtract(yMax_val, move)));
      }));
      controller.append($('<button/>', {
        text: '⇧',
        style: "left: 120px; top: 35px",
      }).addClass('yMaxGo d3btn').click(function() {
        var newWindow = getNewWindow();
        if (newWindow === null) { return; }
        var yMin_val = newWindow['y-min'];
        var yMax_val = newWindow['y-max'];
        var move = jsnums.divide(jsnums.subtract(yMax_val, yMin_val), 10);
        $('.yMin').last().val(rt.num_to_string(jsnums.add(yMin_val, move)));
        $('.yMax').last().val(rt.num_to_string(jsnums.add(yMax_val, move)));
      }));
      
      appendAxis(xMin, xMax, yMin, yMax, width, height, canvas);

      var xToPixel = libNum.scaler(xMin, xMax, 0, width - 1, true),
          yToPixel = libNum.scaler(yMin, yMax, height - 1, 0, true),
          pixelToX = libNum.scaler(0, width - 1, xMin, xMax, false),
          pixelToY = libNum.scaler(height - 1, 0, yMin, yMax, false);;
      
      var display = detached.append('div').style({top: "470px", left: "60px"});
      var xDisplay = display.append('div').style({top: "0px", left: "0px"});
      var yDisplay = display.append('div').style({top: "20px", left: "0px"});
      
      var displayJQ = $(display.node())
      displayJQ.css("position", 'absolute');
      displayJQ.children().css({position: 'absolute', 'font-size': '12px', width: '500px'});
      
      var rectData, isDown = false;

      function updateRect() {  ;
          rectangleElement.attr({
              x: rectData[1].x - rectData[0].x > 0 ? rectData[0].x :  rectData[1].x,
              y: rectData[1].y - rectData[0].y > 0 ? rectData[0].y :  rectData[1].y,
              width: Math.abs(rectData[1].x - rectData[0].x),
              height: Math.abs(rectData[1].y - rectData[0].y)
          });   
      }
      
      var rectangleElement = canvas
        .append('rect')
        .attr('class', 'selection')
        .style({
          "stroke"          : "gray",
          "stroke-width"    : "1px",
          "stroke-dasharray": "4px",
          "stroke-opacity"  : "0.5",
          "fill"            : "transparent"
        });
      
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
          
          $('.xMin').last().val(rt.num_to_string(jsnums.subtract(cx, radiusX)));
          $('.xMax').last().val(rt.num_to_string(jsnums.add(cx, radiusX)));
          $('.yMin').last().val(rt.num_to_string(jsnums.subtract(cy, radiusY)));
          $('.yMax').last().val(rt.num_to_string(jsnums.add(cy, radiusY)));

        })
        .on("mousedown", function() {
          if (isDown) { return; }
          if (d3.event.shiftKey) { return; }
          
          d3.event.preventDefault();
          
          var m1 = d3.mouse(this);
          rectData = [ { x: m1[0], y: m1[1] }, { x: m1[0], y: m1[1] } ];
          rectangleElement.style({visibility: 'visible'})
          updateRect();  
          isDown = true;
        })
        .on("mousemove", function(){
          var coord = d3.mouse(this);
          var vX = pixelToX(coord[0]);
          var vY = pixelToY(coord[1]);

          xDisplay.text("x: " + rt.num_tostring_digits(vX, 5) + ' (' + rt.num_to_string(vX) + ')');
          yDisplay.text("y: " + rt.num_tostring_digits(vY, 5) + ' (' + rt.num_to_string(vY) + ')');
          
          if(isDown) {
            rectData[1] = { x: coord[0], y: coord[1] };
            updateRect();            
          }
        })
        .on("mouseup", function() {
          $('.xMin').last().val(rt.num_to_string(pixelToX(rectData[0].x)));
          $('.xMax').last().val(rt.num_to_string(pixelToX(rectData[1].x)));
          $('.yMin').last().val(rt.num_to_string(pixelToY(rectData[1].y)));
          $('.yMax').last().val(rt.num_to_string(pixelToY(rectData[0].y)));
          
          rectangleElement.style({visibility: 'hidden'});
          isDown = false;
        });

      function plotLine(plot) {
        /*
         * Graph a line
         *
         * Part of this function is adapted from
         * http://jsfiddle.net/christopheviau/Hwpe3/
         */

        var options = toJSOptions(gf(plot, "options"));
        var points = toJSPoints(gf(plot, "points"));

        var line = d3.svg.line()
            .x(function (d) { return xToPixel(d.x); })
            .y(function (d) { return yToPixel(d.y); });

        canvas.append("path")
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
        
        console.log(points);

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
        
        console.log('asdasdsadsadsa');

        canvas
          .selectAll("circle")
          .data(points)
          .enter()
          .append("circle")
          .attr("cx", function (d) { return xToPixel(d.x); })
          .attr("cy", function (d) { return yToPixel(d.y); })
          .attr("r", function(d) { console.log(d); return d.options.size })
          .style('fill', function(d) { console.log(d); return d.options.color; })
          .style('opacity', function(d) { console.log(d); return d.options.opacity; })
          .on("mouseover", function(d) {
            console.log(d);
            if (d.options.tip) {
              tip.show.apply(this, arguments);
            }
          })
          .on("mouseout", function(d) {
            console.log(d);
            if (d.options.tip) {
              tip.hide.apply(this, arguments);
            }
          });
          
      }

      var scatterPoints = []
      rt.ffi.toArray(scatterPlots).forEach(function(plot) {
        var options = toJSOptions(gf(plot, "options"))
        var result = toJSPoints(gf(plot, "points")).map(function(p) {
          p.options = options;
          return p;
        })
        scatterPoints = scatterPoints.concat(result);
      });
      
      plotPoints(scatterPoints);
      
      rt.ffi.toArray(linePlots).forEach(function(plot) {
        plotLine(plot);
      });
      
      stylizeTip(detached);
    
      callBigBang(rt, 900, 585, detached, rt.ffi.makeNone(), function(restarter) {
        controller.append(
          $('<button/>', {
            text: 'Redraw', style: 'left: 95px; top: 295px'
          }).click(function() {
            
            var newWindow = getNewWindow();
            if (newWindow === null) { return; }
            var toRet = rt.ffi.makeSome(rt.makeObject(newWindow));
            rt.getParam("remove-d3-port")();
            restarter.resume(toRet);
          }));
        controller.css('position', 'absolute');
        controller.children().css('position', 'absolute');
      });
        
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////

    function histogram(lst, n) {
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

      callBigBang(rt, undefined, undefined, detached, lst, function(){});
    }

    function pieChart(sdValue) {
      /*
       * Part of this function is adapted from:
       * http://bl.ocks.org/mbostock/3887235
       */

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
      callBigBang(rt, undefined, undefined, detached, sdValue, function(){});
    }

    return util.makeModuleReturn(rt, {}, {
      "histogram": rt.makeFunction(histogram),
      "pie-chart": rt.makeFunction(pieChart),
      "generic-plot": rt.makeFunction(genericPlot),
    });
  });
  };
});
