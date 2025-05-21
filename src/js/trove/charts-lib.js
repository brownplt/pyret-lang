({
  requires: [
    { 'import-type': 'builtin', 'name': 'image-lib' },
  ],
  nativeRequires: [
    'pyret-base/js/js-numbers',
    'vegaMin',
    'canvas',
  ],
  provides: {
    values: {
      'pie-chart': "tany",
      'bar-chart': "tany",
      'multi-bar-chart': "tany",
      'histogram': "tany",
      'box-plot': "tany",
      'plot': "tany",
    }
  },
  theModule: function (RUNTIME, NAMESPACE, uri, IMAGELIB, jsnums, vega, canvasLib) {
    'use strict';

    
    // Default Google Chart Colors for sequential series (Like Multi Bar Charts and Pie Charts) from 
    // https://stackoverflow.com/a/75264589

    // Google chart colors
    var default_colors = ['#3366cc','#dc3912','#ff9900','#109618','#990099',
                          '#0099c6','#dd4477','#66aa00','#b82e2e','#316395',
                          '#994499','#22aa99','#aaaa11','#6633cc','#e67300',
                          '#8b0707','#651067','#329262','#5574a6','#3b3eac',
                          '#b77322','#16d620','#b91383','#f4359e','#9c5935',
                          '#a9c413','#2a778d','#668d1c','#bea413','#0c5922','#743411'];
    
    function notImp(name) {
      return RUNTIME.makeFunction(() => {
        throw new RUNTIME.makeMessageException(name + " not available.")
      })
    }
    if(!vega) {
      return RUNTIME.makeModuleReturn(
        {
          'pie-chart': notImp('pie-chart'),
          'bar-chart': notImp('bar-chart'),
          'multi-bar-chart': notImp('multi-bar-chart'),
          'histogram': notImp('histogram'),
          'box-plot': notImp('box-plot'),
          'plot': notImp('plot'),
        }, 
        { }
      )
    }

    const isTrue = RUNTIME.isPyretTrue;
    const get = RUNTIME.getField;
    const toFixnum = jsnums.toFixnum;
    const cases = RUNTIME.ffi.cases;

    var IMAGE = get(IMAGELIB, "internal");

    const ann = function(name, pred) {
      return RUNTIME.makePrimitiveAnn(name, pred);
    };

    var checkListWith = function(checker) {
      return function(val) {
        if (!RUNTIME.ffi.isList(val)) return false;
        var cur = val;
        var gf = RUNTIME.getField;
        while (RUNTIME.unwrap(RUNTIME.ffi.isLink(cur))) {
          var f = gf(cur, "first");
          if (!checker(f)) {
            return false;
          }
          cur = gf(cur, "rest");
        }
        return true;
      }
    }

    var checkOptionWith = function(checker) {
      return function(val) {
        if (!(RUNTIME.ffi.isNone(val) || RUNTIME.ffi.isSome(val))) return false;
        var gf = RUNTIME.getField;
        if (RUNTIME.unwrap(RUNTIME.ffi.isSome(val))) {
          var f = gf(val, "value");
          if (!checker(f)) {
            return false;
          }
        }
        return true;
      }
    }
    
    //////////////////////////////////////////////////////////////////////////////

    function getPrettyNumToStringDigits(d) {
      // this accepts Pyret num
      return n =>
      jsnums.toStringDigits(n, d, RUNTIME.NumberErrbacks).replace(/\.?0*$/, '');
    }

    const prettyNumToStringDigits5 = getPrettyNumToStringDigits(5);

    function convertColor(v) {
      function p(pred, name) {
        return val => {
          RUNTIME.makeCheckType(pred, name)(val);
          return val;
        };
      }

      const colorDb = IMAGE.colorDb;
      const _checkColor = p(IMAGE.isColorOrColorString, 'Color');

      function checkColor(val) {
        let aColor = _checkColor(val);
        if (colorDb.get(aColor)) {
          aColor = colorDb.get(aColor);
        }
        return aColor;
      }

      function rgb2hex(rgb){
        // From http://jsfiddle.net/Mottie/xcqpF/1/light/
        rgb = rgb.match(/^rgba?[\s+]?\([\s+]?(\d+)[\s+]?,[\s+]?(\d+)[\s+]?,[\s+]?(\d+)[\s+]?/i);
        return (rgb && rgb.length === 4) ? "#" +
          ("0" + parseInt(rgb[1],10).toString(16)).slice(-2) +
          ("0" + parseInt(rgb[2],10).toString(16)).slice(-2) +
          ("0" + parseInt(rgb[3],10).toString(16)).slice(-2) : '';
      }
      return rgb2hex(IMAGE.colorString(checkColor(v)));
    }

    function convertPointer(p) {
      return {value: toFixnum(get(p, 'value')) , label: get(p, 'label')}
    }


    //////////////////////////////////////////////////////////////////////////////

    function numSignificantDigits(n) {
      let ns = n.toString();
      let fracPart = ns.replace(/^.*\./, '');
      let fracPartLength = fracPart.length;
      if (fracPartLength === ns.length) {
        return 0;
      }
      return fracPartLength;
    }

    function fourSig(n, targetSd = 4) {
      if (targetSd > 4) { targetSd = 4; }
      let sd = numSignificantDigits(n);
      if (sd === 0) { return n; }
      if (sd > targetSd) { n = n.toFixed(targetSd); }
      let ns = n.toString();
      ns = ns.replace(/0*$/, '');
      return Number(ns);
    }

    function saneSubtract(m, n) {
      let sd = Math.max(numSignificantDigits(m), numSignificantDigits(n));
      return fourSig(m - n, sd);
    }

    //////////////////////////////////////////////////////////////////////////////

    function getNewWindow(xMinC, xMaxC, yMinC, yMaxC, numSamplesC) {
      const raw = {
        'x-min': xMinC,
        'x-max': xMaxC,
        'y-min': yMinC,
        'y-max': yMaxC,
        'num-samples': numSamplesC
      }
      const ret = {}
      for (const rawVal of raw) {
        const num = RUNTIME.string_tonumber(raw[rawVal].val());
        if (RUNTIME.isNothing(num)) {
          raw[rawVal].addClass('error-bg');
          raw[rawVal].removeClass('ok-bg');
          return null;
        } else {
          ret[rawVal] = num;
        }
      }
      if (jsnums.greaterThanOrEqual(ret['x-min'], ret['x-max'], RUNTIME.NumberErrbacks)) {
        xMinC.addClass('error-bg');
        xMaxC.addClass('error-bg');
        xMinC.removeClass('ok-bg');
        xMaxC.removeClass('ok-bg');
        return null;
      }
      if (jsnums.greaterThanOrEqual(ret['y-min'], ret['y-max'], RUNTIME.NumberErrbacks)) {
        yMinC.addClass('error-bg');
        yMaxC.addClass('error-bg');
        yMinC.removeClass('ok-bg');
        yMaxC.removeClass('ok-bg');
        return null;
      }
      if (!isTrue(RUNTIME.num_is_integer(ret['num-samples'])) ||
          jsnums.lessThanOrEqual(ret['num-samples'], 1, RUNTIME.NumberErrbacks)) {
        numSamplesC.addClass('error-bg');
        numSamplesC.removeClass('ok-bg');
        return null;
      }

      return {
        'x-min': RUNTIME.ffi.makeSome(ret['x-min']),
        'x-max': RUNTIME.ffi.makeSome(ret['x-max']),
        'y-min': RUNTIME.ffi.makeSome(ret['y-min']),
        'y-max': RUNTIME.ffi.makeSome(ret['y-max']),
        'num-samples': ret['num-samples']
      };
    }

    //////////////////////////////////////////////////////////////////////////////

    /**
     * Adds multiple columns with the given properties and values after data
     * columns
     * 
     * For example, if given:
     * colProperties:
     *   {type: 'string', role: 'style'}
     * colValues:
     *   [
     *     [['red', 'black'], ['white', 'blue'], ['green', 'purple']],
     *     []
     *   ]
     * addNSpecialColumns will add 2 style columns after the first data column
     * and no columns after the second data column.
     * 
     * The number of columns added after a particular data column do not have to
     * agree. It is possible to add one special value on one row and two
     * special values on another row.
     * 
     * https://jsfiddle.net/eyanje/u83kaf92/
     * 
     * @param {DataTable} table a table to expand
     * @param {object} colProperties an object specifying column properties
     * @param {Array<Array<*>>>} colValues rows of groups of values to insert
     */
    function addNSpecialColumns(table, colProperties, colValues) {
      let dataColNums = [];
      let nDataCols;
      let groupWidths;
      for (let i = 1; i < table.getNumberOfColumns(); i++) {
        const role = table.getColumnRole(i);
        if (role === '' || role === 'data') {
          dataColNums.push(i);
        }
      }
      nDataCols = dataColNums.length;
      // Check column count
      // Should never run -- Pyret checks all column counts properly
      // This should be somewhat caught in the try-catch around setup(restarter),
      // unless it's been moved
      colValues.forEach((row, rowN) => {
        if (row.length !== nDataCols) {
          throw new Error(`Incorrect column count in row ${rowN}.`
                          + ` Expected ${nDataCols}, given ${row.length}.`);
        }
      });
      // Tally columns needed for each group
      groupWidths = dataColNums.map(() => 0);
      colValues.forEach(row => {
        row.forEach((group, groupN) => {
          groupWidths[groupN] = Math.max(group.length, groupWidths[groupN]);
        });
      });
      // Add columns in reverse order
      for (let groupIndex = nDataCols - 1; groupIndex >= 0; groupIndex--) {
        for (let i = 0; i < groupWidths[groupIndex]; i++) {
          table.insertColumn(dataColNums[groupIndex] + 1, colProperties);
        }
      }
      // Adjust dataColNums to match expanded table
      let sum = 0;
      dataColNums.forEach((dataColNum, i) => {
        dataColNums[i] += sum;
        sum += groupWidths[i];
      });
      // Add columns in reverse order to avoid extra calculations
      colValues.forEach((row, rowN) => {
        row.forEach((group, groupN) => {
          group.forEach((val, i) => {
            table.setValue(rowN, dataColNums[groupN] + i + 1, val);
          });
        })
      });
    }

    /**
     * Adds columns with the given properties and values after data columns
     * 
     * For example, you may use this function to add columns with properties
     * {type: 'string', role: 'style'} and values
     * [['red', 'black'] ['white', 'blue'], ['green', 'purple']], to add
     * two style columns to a table with 3 rows and 2 columns.
     * 
     * @param {DataTable} table a table to expand
     * @param {object} colProperties an object specifying column properties
     * @param {Array<Array<*>>>} colValues rows of values to insert
     */
    function addSpecialColumns(table, colProperties, colValues) {
      addNSpecialColumns(table, colProperties,
                         colValues.map(r => r.map(c => [c])));
    }

    function addAnnotations(data, rawData) {
      const rawAnnotations = get(rawData, 'annotations').map(row =>
        row.map(col =>
          cases(RUNTIME.ffi.isOption, 'Option', col, {
            none: function () {},
            some: function (annotation) { return annotation; }
          })
        )
      );
      const colProperties = { type: 'string', role: 'annotation' };
      addSpecialColumns(table, colProperties, rawAnnotations);
    }

    function addIntervals(table, rawData) {
      const colProperties = {type: 'number', role: 'interval'};
      addNSpecialColumns(table, colProperties, get(rawData, 'intervals'));
    }

    function selectMultipleMutator(options, globalOptions, _) {
      const multiple = get(globalOptions, 'multiple');
      if (multiple) {
        $.extend(options, {selectionMode: 'multiple'});
      } else {
        $.extend(options, {selectionMode: 'single'});
      }
    }

    function backgroundMutator(options, globalOptions, _) {
      const backgroundColor = cases(RUNTIME.ffi.isOption, 'Option', get(globalOptions, 'backgroundColor'), {
        none: function () {
          return 'transparent';
        },
        some: function (color) {
          return convertColor(color);
        }
      });
      const borderColor = cases(RUNTIME.ffi.isOption, 'Option', get(globalOptions, 'borderColor'), {
        none: function () {
          return '#666';
        },
        some: function (color) {
          return convertColor(color);
        }
      });
      const borderSize = toFixnum(get(globalOptions, 'borderSize'))
      $.extend(options, {
        backgroundColor: {
          fill: backgroundColor,
          strokeWidth: borderSize,
          stroke: borderColor,
        }
      });
    }
    function axesNameMutator(options, globalOptions, _) {
      const hAxis = ('hAxis' in options) ? options.hAxis : {};
      const vAxis = ('vAxis' in options) ? options.vAxis : {};
      hAxis.title = get(globalOptions, 'x-axis');
      vAxis.title = get(globalOptions, 'y-axis');
      $.extend(options, {hAxis: hAxis, vAxis: vAxis});
    }

    function gridlinesMutator(options, globalOptions, _) {
      const hAxis = ('hAxis' in options) ? options.hAxis : {};
      const vAxis = ('vAxis' in options) ? options.vAxis : {};

      const gridlineColor = cases(RUNTIME.ffi.isOption, 'Option', get(globalOptions, 'gridlineColor'), {
        none: function () {
          return '#aaa';
        },
        some: function (color) {
          return convertColor(color);
        }
      });

      const minorGridlineColor = cases(RUNTIME.ffi.isOption, 'Option', get(globalOptions, 'minorGridlineColor'), {
        none: function () {
          return '#ddd';
        },
        some: function (color) {
          return convertColor(color);
        }
      });

      const minorGridlineMinspacing = toFixnum(get(globalOptions, 'minorGridlineMinspacing'))

      hAxis.gridlines = {color: gridlineColor};
      vAxis.gridlines = {color: gridlineColor};

      cases(RUNTIME.ffi.isOption, 'Option', get(globalOptions, 'gridlineMinspacing'), {
        none: function () {
          hAxis.gridlines.count = 5;
        },
        some: function (minspacing) {
          hAxis.gridlines.minSpacing = toFixnum(minspacing);
        }
      });


      if (get(globalOptions, 'show-minor-grid-lines')) {
        hAxis.minorGridlines = {color: minorGridlineColor, minSpacing: minorGridlineMinspacing};
        vAxis.minorGridlines = {color: minorGridlineColor, minSpacing: minorGridlineMinspacing};
      } else {
        hAxis.minorGridlines = {count: 0};
        vAxis.minorGridlines = {count: 0};
      }
      $.extend(options, {hAxis: hAxis, vAxis: vAxis});
    }

    function yAxisRangeMutator(options, globalOptions, _) {
      const vAxis = ('vAxis' in options) ? options.vAxis : {};
      const viewWindow = ('viewWindow' in vAxis) ? vAxis.viewWindow : {};

      cases(RUNTIME.ffi.isOption, 'Option', get(globalOptions, 'y-min'), {
        none: function () {},
        some: function (minValue) {
          const v = toFixnum(minValue);
          vAxis.minValue = v;
          viewWindow.min = v;
        }
      });
      cases(RUNTIME.ffi.isOption, 'Option', get(globalOptions, 'y-max'), {
        none: function () {},
        some: function (maxValue) {
          const v = toFixnum(maxValue);
          vAxis.maxValue = v;
          viewWindow.max = v;
        }
      });
      vAxis.viewWindow = viewWindow;
      $.extend(options, {vAxis: vAxis});
    }

    function xAxisRangeMutator(options, globalOptions, _) {
      const hAxis = ('hAxis' in options) ? options.hAxis : {};
      const viewWindow = ('viewWindow' in hAxis) ? hAxis.viewWindow : {};

      const minValue = get(globalOptions, 'x-min');
      const maxValue = get(globalOptions, 'x-max');

      cases(RUNTIME.ffi.isOption, 'Option', minValue, {
        none: function () {},
        some: function (realMinValue) {
          hAxis.minValue = toFixnum(realMinValue);
          viewWindow.min = toFixnum(realMinValue);
        }
      });
      cases(RUNTIME.ffi.isOption, 'Option', maxValue, {
        none: function () {},
        some: function (realMaxValue) {
          hAxis.maxValue = toFixnum(realMaxValue);
          viewWindow.max = toFixnum(realMaxValue);
        }
      });

      hAxis.viewWindow = viewWindow;
      $.extend(options, {hAxis: hAxis});
    }

    //////////////////////////////////////////////////////////////////////////////

    function pieChart(globalOptions, rawData) {
      const table = get(rawData, 'tab');
      const default_colors = ['#3366CC', '#DC3912', '#FF9900', '#109618', '#990099',
                              '#3B3EAC', '#0099C6', '#DD4477', '#66AA00', '#B82E2E',
                              '#316395', '#994499', '#22AA99', '#AAAA11', '#6633CC',
                              '#E67300', '#8B0707', '#329262', '#5574A6', '#3B3EAC']
      var colors_list = get_colors_list(rawData);

      if (colors_list.length < default_colors.length) {
        default_colors.splice(0, colors_list.length, ...colors_list);
        colors_list = default_colors;
        colors_list = colors_list.slice(0, table.length);
      }
      const new_colors_list = table.map(row => colors_list[row[3]])
      colors_list = new_colors_list
      
      const threeD = get(rawData, 'threeD');
      const piehole = toFixnum(get(rawData, 'piehole'));
      const startingAngle = toFixnum(get(rawData, 'startingAngle'));
      const collapseThreshold = toFixnum(get(rawData, 'collapseThreshold'));

      // ASSERT: if we're using custom images, the third column will be an object
      const hasImage = typeof table[0][3] == 'object';

      const data = new google.visualization.DataTable();
      data.addColumn('string', 'Label');
      data.addColumn('number', 'Value');
      data.addRows(table.map(row => [row[0], toFixnum(row[1])]));

      return {
        data: data,
        options: {
          slices: table.map((row, i) => ({
            color: hasImage? "transparent" : colors_list[i], 
            offset: toFixnum(row[2])
          })),
          legend: {
            alignment: 'end'
          },
          is3D: threeD,
          pieHole: piehole,
          pieStartAngle: startingAngle,
          sliceVisibilityThreshold: collapseThreshold,
        },
        chartType: google.visualization.PieChart,
        onExit: defaultImageReturn,
        mutators: [backgroundMutator],
        overlay: (overlay, restarter, chart, container) => {
          // If we don't have images, our work is done!
          if(!hasImage) { return; }
          
          // if custom images are defined, use the image at that location
          // and overlay it atop each dot
          google.visualization.events.addListener(chart, 'ready', function () {
            // HACK(Emmanuel): 
            // The only way to hijack marker events is to walk the DOM here
            // If Google changes the DOM, these lines will likely break
            const svgRoot = chart.container.querySelector('svg');

            // The order of SVG slices is *not* the order of the rows in the table!!
            //   - 1 or 2 slices: drawn in reverse order
            //   - More than 2 slices: the first row in the table is the first SVG 
            //       slice, but the rest are in reverse order
            let slices;
            if(table.length <= 2) {
              slices = Array.prototype.slice.call(svgRoot.children, 2, -1).reverse();
            } else {
              slices = Array.prototype.slice.call(svgRoot.children, 3, -1).reverse();
              slices.unshift(svgRoot.children[2]);
            }
            const defs = svgRoot.children[0];
            const legendImgs = svgRoot.children[1].querySelectorAll('g[column-id]');

            // remove any labels that have previously been drawn
            $('.__img_labels').each((idx, n) => $(n).remove());

            // Render each slice under the old ones, using the image as a pattern
            table.forEach((row, i) => {            
              const oldDot = legendImgs[i].querySelector('circle');
              const oldSlice = slices[i];
              
              // render the image to an img tag
              const imgDOM = row[3].val.toDomNode();
              row[3].val.render(imgDOM.getContext('2d'), 0, 0);
              
              // make an SVGimage element from the img tag, and make it the size of the slice
              const sliceBox = oldSlice.getBoundingClientRect();
              const imageElt = document.createElementNS("http://www.w3.org/2000/svg", 'image');
              imageElt.classList.add('__img_labels'); // tag for later garbage collection
              imageElt.setAttributeNS(null, 'href', imgDOM.toDataURL());
              imageElt.setAttribute('width',  Math.max(sliceBox.width, sliceBox.height));

              // create a pattern from that image
              const patternElt = document.createElementNS("http://www.w3.org/2000/svg", 'pattern');
              patternElt.setAttribute(  'x',    0);
              patternElt.setAttribute(  'y',    0);
              patternElt.setAttribute('width',  1);
              patternElt.setAttribute('height', 1);
              patternElt.setAttribute( 'id',   'pic'+i);

              // make a new slice, copy elements from the old slice, and fill with the pattern
              const newSlice = document.createElementNS("http://www.w3.org/2000/svg", 'path');
              Object.assign(newSlice, oldSlice); // we should probably not steal *everything*...
              newSlice.setAttribute(  'd',       oldSlice.firstChild.getAttribute('d'));
              newSlice.setAttribute( 'fill',         'url(#pic'+i+')');

              // add the image to the pattern and the pattern to the defs
              patternElt.appendChild(imageElt);
              defs.append(patternElt);

              // insert the new slice before the now-transparent old slice
              oldSlice.parentNode.insertBefore(newSlice, oldSlice)
              
              // make a new dot, then set size and position of dot to replace the old dot
              const newDot = imageElt.cloneNode(true);
              const radius = oldDot.r.animVal.value;
              newDot.setAttribute('x',       oldDot.cx.animVal.value - radius);
              newDot.setAttribute('y',       oldDot.cy.animVal.value - radius);
              newDot.setAttribute('width',   radius * 2);
              newDot.setAttribute('height',  radius * 2);
              oldDot.parentNode.replaceChild(newDot, oldDot);
            });
          });
        }
      }
    }

    //////////// Bar Chart Getter Functions /////////////////

    function getColorOrDefault(optColor, defaultColor) {
      return cases(RUNTIME.ffi.isOption, 'Option', optColor, {
        none: () => defaultColor,
        some: convertColor
      });
    }
    
    function get_colors_list(rawData) {
      // Sets up the color list [Each Bar Colored Individually]
      return cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'colors'), {
        none: function () {
          return [];
        },
        some: function (colors) {
          return colors.map(convertColor);
        }
      });
    }

    function get_default_color(rawData) {
      if (!RUNTIME.hasField(rawData, 'color')) return "";
      
      // Sets up the default color [Default Bar Color if not specified in color_list]
      return cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'color'), {
        none: function () {
          return "";
        },
        some: function (color) {
          return convertColor(color);
        }
      });
    }

    function get_pointers_list(rawData) {
      // Sets up the pointers list [Coloring each group memeber/stack]
      return cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'pointers'), {
        none: function () {
          return [];
        },
        some: function (pointers) {
          return pointers.map(convertPointer);
        }
      });
    }

    function get_pointer_color(rawData) { 
      // Sets up the pointer color
      return cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'pointer-color'), {
        none: function () {
          return 'black';
        },
        some: function (color) {
          return convertColor(color);
        }
      });
    }

    function get_axis(rawData) {
      // Sets up the calculated axis properties/data
      return cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'axisdata'), {
        none: function () {
          return undefined;
        },
        some: function (axisdata) {
          const pointers = get(axisdata, 'ticks').map(convertPointer);
          return {
            domainMax : toFixnum(get(axisdata, 'axisTop')), 
            domainMin : toFixnum(get(axisdata, 'axisBottom')),
            domainRaw : pointers.map(p => p.value),
            labels: pointers.map(p => p.label)
          };
        }
      });
    }

    function get_interval_color(rawData) { 
      // Sets up the default interval color
      return cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'default-interval-color'), {
        none: function () {
          return 'black';
        },
        some: function (color) {
          return convertColor(color);
        }
      });
    }

    /////////////////////////////////////////////////////////
    const dimensions = {
      vertical: {
        primary: {
          dir: 'x',
          range: 'width',
          type: 'band',
          axes: 'bottom',
        },
        secondary: {
          dir: 'y',
          range: 'height',
          type: 'linear',
          axes: 'left',
        },
        images: {
          anchorProp: 'baseline',
          anchor: 'bottom',
        },
        pointers: {
          rangeIndex: 1,
          offsetDir: 'dx',
          offsetValue: 10,
          align: 'left',
          baseline: 'middle'
        },
        annotations: {
          align: 'center',
          baseline: 'top',
          offset: 5
        }
      },
      horizontal: {
        primary: {
          dir: 'y',
          range: 'height',
          type: 'band',
          axes: 'left',
        },
        secondary: {
          dir: 'x',
          range: 'width',
          type: 'linear',
          axes: 'bottom',
        },
        images: {
          anchorProp: 'align',
          anchor: 'right',
        },
        pointers: {
          rangeIndex: 0,
          offsetDir: 'dy',
          offsetValue: -5,
          align: 'center',
          baseline: 'bottom'
        },
        annotations: {
          align: 'right',
          baseline: 'middle',
          offset: -5
        }
      }

    };

    const verticalStackedBarChart = {
      "$schema": "https://vega.github.io/schema/vega/v6.json",
      "description": "A basic stacked bar chart example.",
      "width": 500,
      "height": 200,
      "padding": 5,

      "data": [
        {
          "name": "table",
          "values": [
            {"x": 0, "y": 28, "c": 0}, {"x": 0, "y": 55, "c": 1},
            {"x": 1, "y": 43, "c": 0}, {"x": 1, "y": 91, "c": 1},
            {"x": 2, "y": 81, "c": 0}, {"x": 2, "y": 53, "c": 1},
            {"x": 3, "y": 19, "c": 0}, {"x": 3, "y": 87, "c": 1},
            {"x": 4, "y": 52, "c": 0}, {"x": 4, "y": 48, "c": 1},
            {"x": 5, "y": 24, "c": 0}, {"x": 5, "y": 49, "c": 1},
            {"x": 6, "y": 87, "c": 0}, {"x": 6, "y": 66, "c": 1},
            {"x": 7, "y": 17, "c": 0}, {"x": 7, "y": 27, "c": 1},
            {"x": 8, "y": 68, "c": 0}, {"x": 8, "y": 16, "c": 1},
            {"x": 9, "y": 49, "c": 0}, {"x": 9, "y": 15, "c": 1}
          ],
          "transform": [
            {
              "type": "stack",
              "groupby": ["x"],
              "sort": {"field": "c"},
              "field": "y"
            }
          ]
        }
      ],

      "scales": [
        {
          "name": "x",
          "type": "band",
          "range": "width",
          "domain": {"data": "table", "field": "x"}
        },
        {
          "name": "y",
          "type": "linear",
          "range": "height",
          "nice": true, "zero": true,
          "domain": {"data": "table", "field": "y1"}
        },
        {
          "name": "color",
          "type": "ordinal",
          "range": "category",
          "domain": {"data": "table", "field": "c"}
        }
      ],

      "axes": [
        {"orient": "bottom", "scale": "x", "zindex": 1},
        {"orient": "left", "scale": "y", "zindex": 1}
      ],

      "marks": [
        {
          "type": "rect",
          "from": {"data": "table"},
          "encode": {
            "enter": {
              "x": {"scale": "x", "field": "x"},
              "width": {"scale": "x", "band": 1, "offset": -1},
              "y": {"scale": "y", "field": "y0"},
              "y2": {"scale": "y", "field": "y1"},
              "fill": {"scale": "color", "field": "c"}
            },
            "update": {
              "fillOpacity": {"value": 1}
            },
            "hover": {
              "fillOpacity": {"value": 0.5}
            }
          }
        }
      ]
    };
    const verticalGroupedBarChart = {
      "$schema": "https://vega.github.io/schema/vega/v6.json",
      "description": "A basic grouped bar chart example.",
      "width": 300,
      "height": 240,
      "padding": 5,

      "data": [
        {
          "name": "table",
          "values": [
            {"category":"A", "position":0, "value":0.1},
            {"category":"A", "position":1, "value":0.6},
            {"category":"A", "position":2, "value":0.9},
            {"category":"A", "position":3, "value":0.4},
            {"category":"B", "position":0, "value":0.7},
            {"category":"B", "position":1, "value":0.2},
            {"category":"B", "position":2, "value":1.1},
            {"category":"B", "position":3, "value":0.8},
            {"category":"C", "position":0, "value":0.6},
            {"category":"C", "position":1, "value":0.1},
            {"category":"C", "position":2, "value":0.2},
            {"category":"C", "position":3, "value":0.7}
          ]
        }
      ],

      "scales": [
        {
          "name": "yscale",
          "type": "band",
          "domain": {"data": "table", "field": "category"},
          "range": "height",
          "padding": 0.2
        },
        {
          "name": "xscale",
          "type": "linear",
          "domain": {"data": "table", "field": "value"},
          "range": "width",
          "round": true,
          "zero": true,
          "nice": true
        },
        {
          "name": "color",
          "type": "ordinal",
          "domain": {"data": "table", "field": "position"},
          "range": {"scheme": "category20"}
        }
      ],

      "axes": [
        {"orient": "left", "scale": "yscale", "tickSize": 0, "labelPadding": 4, "zindex": 1},
        {"orient": "bottom", "scale": "xscale"}
      ],

      "marks": [
        {
          "type": "group",

          "from": {
            "facet": {
              "data": "table",
              "name": "facet",
              "groupby": "category"
            }
          },

          "encode": {
            "enter": {
              "y": {"scale": "yscale", "field": "category"}
            }
          },

          "signals": [
            {"name": "height", "update": "bandwidth('yscale')"}
          ],

          "scales": [
            {
              "name": "pos",
              "type": "band",
              "range": "height",
              "domain": {"data": "facet", "field": "position"}
            }
          ],

          "marks": [
            {
              "name": "bars",
              "from": {"data": "facet"},
              "type": "rect",
              "encode": {
                "enter": {
                  "y": {"scale": "pos", "field": "position"},
                  "height": {"scale": "pos", "band": 1},
                  "x": {"scale": "xscale", "field": "value"},
                  "x2": {"scale": "xscale", "value": 0},
                  "fill": {"scale": "color", "field": "position"}
                }
              }
            },
            {
              "type": "text",
              "from": {"data": "bars"},
              "encode": {
                "enter": {
                  "x": {"field": "x2", "offset": -5},
                  "y": {"field": "y", "offset": {"field": "height", "mult": 0.5}},
                  "fill": [
                    {"test": "contrast('white', datum.fill) > contrast('black', datum.fill)", "value": "white"},
                    {"value": "black"}
                  ],
                  "align": {"value": "right"},
                  "baseline": {"value": "middle"},
                  "text": {"field": "datum.value"}
                }
              }
            }
          ]
        }
      ]
    }


    function chooseColor(colorsList, defaultColor, seriesIndex, itemIndex, numSeries) {
      const relevantIndex = (numSeries === 1) ? itemIndex : seriesIndex;
      if (relevantIndex < colorsList.length) { return colorsList[relevantIndex]; }
      if (defaultColor) { return defaultColor; }
      return seriesIndex;
    }

    function imageToCanvas(img) {
      const canvas = canvasLib.createCanvas(img.getWidth(), img.getHeight());
      const ctx = canvas.getContext('2d');
      img.render(ctx);
      return canvas;
    }

    /*
      globalOptions and rawData: as in other functions
      marksSource: either "table" or "facet", depending on whether it's grouped
      dataTable: outparameter array to store the data values
      marks: outparam array to store the marks
     */
    function constructDataTable(globalOptions, rawData, marksSource, heightField, primaryScale, tooltip, dataTable, marks) {
      const horizontal = isTrue(get(rawData, 'horizontal'));
      const axesConfig = dimensions[horizontal ? 'horizontal' : 'vertical']
      const table = get(rawData, 'tab');
      const colors_list = get_colors_list(rawData);
      const default_color = get_default_color(rawData);

      // Adds each row of bar data and bar_color data
      // allAnnotations : Option<String>[][], where allAnnotations[label][series] is the optional annotation
      // for the value of a given axis label and a given data-series within that label
      const allAnnotations = get(rawData, 'annotations') || [];
      // allAnnotations : num[][], where allAnnotations[label][series] is the list of interval data
      // for the value of a given axis label and a given data-series within that label
      const allIntervals = get(rawData, 'intervals') || []
      const NONEann = RUNTIME.ffi.makeNone();
      
      table.forEach(function (row, i) {
        if (!(row[1] instanceof Array)) {
          row[1] = [row[1]];
        }
        const seriesAnnotations = allAnnotations[i] || [];
        row[1].forEach(function (value, series) {
          const annotationOpt = seriesAnnotations[series] || NONEann;
          const annotation = cases(RUNTIME.ffi.isOption, 'Option', annotationOpt, {
            none: () => undefined,
            some: (v) => v
          });
          let intervals = (allIntervals[i] || [])[series]
          if (intervals && intervals.length === 0) {
            intervals = undefined;
          } else {
            intervals = intervals.map(toFixnum);
          }
          
          dataTable.values.push({
            label: row[0],
            rawValue: toFixnum(value),
            color: chooseColor(colors_list, default_color, series, i, row[1].length),
            image: (row[2] && row[2].val && IMAGE.isImage(row[2].val)) ? imageToCanvas(row[2].val) : undefined,
            annotation,
            series,
            intervals
          });
        });
      });
      marks.push({
        "type": "rect",
        "name": "bars",
        "from": {"data": marksSource},
        "encode": {
          "enter": {
            [axesConfig.primary.dir]: {"scale": primaryScale, "field": heightField},
            [axesConfig.primary.range]: {"scale": primaryScale, "band": 1, "offset": -1},
            [axesConfig.secondary.dir]: {"scale": "secondary", "field": "value0"},
            [axesConfig.secondary.dir + '2']: {"scale": "secondary", "field": "value1"},
            "fill": [
              { test: 'isValid(datum.image)', value: 'transparent' },
              { scale: 'color', field: 'series' }
            ],
            "tooltip": tooltip,
            strokeWidth: { value: 2 },
            stroke: [
              { test: 'contrast("white", scale("color", datum.series)) > contrast("black", scale("color", datum.series))', value: 'white' },
              { value: 'black' }
            ]
          },
          "update": {
            strokeOpacity: { signal: "hoveredSeries == datum.series ? 1 : 0" }
          },
          "hover": {
            strokeOpacity: { value: 1 },
          }
        }
      });
    }

    function addImages(globalOptions, rawData, primaryScale, tooltip, data, marks) {
      const horizontal = isTrue(get(rawData, 'horizontal'));
      const axesConfig = dimensions[horizontal ? 'horizontal' : 'vertical']
      const imagesTable = {
        name: 'images',
        source: 'table',
        transform: [ { type: 'filter', expr: 'isValid(datum.image)' } ]
      };
      data.push(imagesTable);
      marks.push({
        "type": "image",
        "from": {"data": "images"},
        "encode": {
          "enter": {
            [axesConfig.primary.dir]: {"scale": primaryScale, "field": "label"},
            [axesConfig.secondary.dir]: {
              "signal": "max(scale('secondary', datum.value0), scale('secondary', datum.value1))"
            },
            [axesConfig.primary.range]: {"scale": primaryScale, "band": 1, "offset": -1},
            [axesConfig.secondary.range]: {"signal": "abs(scale('secondary', datum.value1) - scale('secondary', datum.value0))"},
            "image": {"field": "image"},
            "stroke": {"value": "#666666"},
            "strokeWidth": {"value": 10},
            "strokeOpacity": {"value": 1},
            "aspect": {"value": false},
            [axesConfig.images.anchorProp]: {"value": axesConfig.images.anchor},
            "tooltip": tooltip
          }
        }
      });
    }

    function addPointers(globalOptions, rawData, primaryScale, data, marks) {
      const horizontal = isTrue(get(rawData, 'horizontal'));
      const axesConfig = dimensions[horizontal ? 'horizontal' : 'vertical']
      const pointersTable = {
        name: 'pointers',
        values: [],
      };
      data.push(pointersTable);
      const pointers_list = get_pointers_list(rawData);
      const pointer_color = get_pointer_color(rawData);
      pointers_list.forEach((pointer) => {
        pointersTable.values.push(pointer);
      });
      marks.push(
        {
          type: "rule",
          from: { data: "pointers" },
          encode: {
            enter: {
              [axesConfig.secondary.dir]: { scale: 'secondary', field: 'value' },
              [axesConfig.secondary.dir + '2']: { scale: 'secondary', field: 'value' },
              [axesConfig.primary.dir]: { signal: `range('${primaryScale}')[0]`},
              [axesConfig.primary.dir + '2']: { signal: `range('${primaryScale}')[1]`},
              stroke: { value: pointer_color },
              strokeWidth: { value: 1 },
              opacity: { value: 1 }
            }
          }
        },
        {
          type: 'text',
          from: { data: 'pointers' },
          encode: {
            enter: {
              [axesConfig.secondary.dir]: { scale: 'secondary', field: 'value' },
              [axesConfig.primary.dir]: { signal: `range('${primaryScale}')[${axesConfig.pointers.rangeIndex}]` },
              [axesConfig.pointers.offsetDir]: { value: axesConfig.pointers.offsetValue },
              align: { value: axesConfig.pointers.align },
              baseline: { value: axesConfig.pointers.baseline },
              text: { field: 'label' },
              fill: { value: pointer_color }
            }
          }
        }
      );
    }
    
    function addAnnotations(globalOptions, rawData, tableSource, primaryField, primaryScale, data, marks) {
      const horizontal = isTrue(get(rawData, 'horizontal'));
      const axesConfig = dimensions[horizontal ? 'horizontal' : 'vertical']
      const annotationsTable = {
        name: 'annotations',
        source: tableSource,
        transform: [ { type: 'filter', expr: 'isValid(datum.annotation)' } ]
      };
      data.push(annotationsTable);
      marks.push({
        type: "text",
        from: { data: "annotations"},
        encode: {
          enter: {
            [axesConfig.secondary.dir]: {
              // NOTE(Ben): Scaling *after* choosing the greater *value-space* endpoint,
              // to choose rightmost and topmost
              signal: 'max(datum.value0, datum.value1)',
              scale: 'secondary',
              offset: axesConfig.annotations.offset
            },
            [axesConfig.primary.dir]: {
              scale: primaryScale, field: primaryField, offset: { scale: primaryScale, band: 0.5 }
            },
            fill: [
              // NOTE(Ben): fillColor is computed by the transform above
              { test: 'contrast("white", scale("color", datum.series)) > contrast("black", scale("color", datum.color))',
                value: "white" },
              { value: "black" }
            ],
            align: { value: axesConfig.annotations.align },
            baseline: { value: axesConfig.annotations.baseline },
            text: { field: "annotation" }
          }
        }
      });
    }

    function addIntervals(globalOptions, rawData, tableSource, primaryField, primaryScale, data, marks) {
      const horizontal = isTrue(get(rawData, 'horizontal'));
      const axesConfig = dimensions[horizontal ? 'horizontal' : 'vertical']
      const interval_color = get_interval_color(rawData); 
      const intervalsTable = {
        name: 'intervals',
        source: tableSource,
        transform: [
          { type: 'filter', expr: 'isArray(datum.intervals)' },
          { type: 'formula', as: 'intervalExtent', expr: 'extent(datum.intervals)' }
        ]
      };
      data.push(intervalsTable);
      const intervalTicksTable = {
        name: 'intervalTicks',
        source: 'intervals',
        transform: [
          { type: 'flatten', fields: ['intervals'], as: ['intervalTick'] }
        ]
      };
      data.push(intervalTicksTable);
      marks.push(
        {
          type: 'rule',
          from: { data: 'intervals' }, 
          encode: {
            enter: {
              interactive: false,
              [axesConfig.secondary.dir]: { scale: 'secondary', field: 'intervalExtent[0]' },
              [axesConfig.secondary.dir + '2']: { scale: 'secondary', field: 'intervalExtent[1]' },
              [axesConfig.primary.dir]: {
                scale: primaryScale, field: primaryField, offset: { scale: primaryScale, band: 0.5 }
              },
              [axesConfig.primary.dir + '2']: {
                scale: primaryScale, field: primaryField, offset: { scale: primaryScale, band: 0.5 }
              },
              stroke: { value: interval_color },
              strokeWidth: { value: 1 },
              opacity: { value: 1 }
            }
          }
        },
        {
          type: 'rule',
          from: { data: 'intervalTicks' },
          encode: {
            enter: {
              interactive: false,
              [axesConfig.secondary.dir]: { scale: 'secondary', field: 'intervalTick' },
              [axesConfig.primary.dir]: {
                scale: primaryScale, field: primaryField,
                offset: {
                  scale: primaryScale, band: 0.5,
                  offset: { signal: `-0.5 * min(bandwidth('${primaryScale}'), 10)` },
                }
              },
              [axesConfig.primary.dir + 2]: {
                scale: primaryScale, field: primaryField,
                offset: {
                  scale: primaryScale, band: 0.5,
                  offset: { signal: `0.5 * min(bandwidth('${primaryScale}'), 10)` },
                }
              },
              stroke: { value: interval_color },
              strokeWidth: { value: 1 },
              opacity: { value: 1 }
            }
          }
        }
      );
    }      
    
    function barChart(globalOptions, rawData) {
      // Variables and constants 
      const horizontal = isTrue(get(rawData, 'horizontal'));
      const axisloc = horizontal ? 'hAxes' : 'vAxes';
      const title = get(globalOptions, 'title');
      const width = get(globalOptions, 'width');
      const height = get(globalOptions, 'height');
      const background = getColorOrDefault(get(globalOptions, 'backgroundColor'), 'transparent');
      const axesConfig = dimensions[horizontal ? 'horizontal' : 'vertical']
      const axis = get_axis(rawData);

      const data = [];
      

      const dataTable = {
        name: 'table',
        values: [],
        transform: [
          {
            type: "formula",
            as: "color",
            expr: "isString(datum.color) ? datum.color : scale('color', datum.series)"
          },
          { type: "formula", as: "value", expr: "datum.rawValue" },
          {
            "type": "stack",
            "groupby": ["label"],
            "sort": {"field": "series"},
            "field": "value",
            "as": ["value0", "value1"],
          }
        ]
      };
      data.push(dataTable);
      const signals = [];
      const scales = [
        {
          "name": "primary",
          "type": "band",
          "range": axesConfig.primary.range,
          "domain": {"data": "table", "field": "label"}
        },
        {
          "name": "secondary",
          "type": "linear",
          "range": axesConfig.secondary.range,
          "nice": true, "zero": true,
          "domain": {"data": "table", "field": "value"}
        },
        {
          "name": "color",
          "type": "ordinal",
          "domain": Array.from(Array(default_colors.length).keys()),
          "range": { scheme: "google" }
        }
      ];
      if (axis) {
        scales.push({
          name: "secondaryLabels",
          type: "ordinal",
          domain: axis.domainRaw,
          range: axis.labels
        });
      }
      // TODO: AXES
      const axes = [
        { orient: axesConfig.primary.axes, scale: 'primary', zindex: 1 },
        { orient: axesConfig.secondary.axes, scale: 'secondary', zindex: 1, grid: false },
        // redraw the axis just for its gridlines, but beneath everything else in z-order
        { orient: axesConfig.secondary.axes, scale: 'secondary', zindex: 0, grid: true, ticks: false, labels: false }
      ];
      if (axis) {
        axes[1].values = axis.domainRaw;
        axes[1].encode = {
          labels: { update: { text: { signal: "scale('secondaryLabels', datum.value)" } } }
        };
      }
      const marks = [];
      const tooltips = [
        {
          test: "isArray(datum.intervals) && datum.intervals.length > 0",
          signal: "{title: datum.label, Values: datum.value, Intervals: datum.intervals}"
        },
        {
          signal: "{title: datum.label, Values: datum.value}"
        }
      ];
      constructDataTable(globalOptions, rawData, "table", "label", "primary", tooltips, dataTable, marks);

      addImages(globalOptions, rawData, "primary", tooltips, data, marks);

      addPointers(globalOptions, rawData, "primary", data, marks);

      addAnnotations(globalOptions, rawData, "table", "label", "primary", data, marks);

      addIntervals(globalOptions, rawData, "table", "label", "primary", data, marks);
      
      // addIntervals(data, rawData);

      return {
        "$schema": "https://vega.github.io/schema/vega/v6.json",
        description: title,
        title: title ? { text: title } : '',
        width,
        height,
        padding: 0,
        autosize: "fit",
        background,
        data,
        signals,
        scales,
        axes,
        marks,
        onExit: defaultImageReturn,
        gChartOverlay: (overlay, restarter, chart, container) => {

          if (!hasImage && !dotChartP) return;

          // if custom images are defined, use the image at that location
          // and overlay it atop each dot


          google.visualization.events.addListener(chart, 'ready', function () {

            if (dotChartP) {
              table.forEach(function (row, i) {
                // console.log('row', i, '=', row);
                const rect = rects[i];
                // console.log('rect', i, '=', rect);
                const num_elts = row[1];
                const rect_x = Number(rect.getAttribute('x'));
                const rect_y = Number(rect.getAttribute('y'));
                const rect_height = Number(rect.getAttribute('height'));
                const unit_height = rect_height/num_elts;
                const rect_width = Number(rect.getAttribute('width'));
                const rect_fill = rect.getAttribute('fill');
                // const rect_fill_opacity = Number(rect.getAttribute('fill-opacity'));
                // const rect_stroke = rect.getAttribute('stroke');
                // const rect_stroke_width = Number(rect.getAttribute('stroke-width'));
                rect.setAttribute('stroke-width', 0);
                for (let j = 0; j < num_elts; j++) {
                  const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
                  circle.classList.add('__img_labels');
                  circle.setAttribute('r', rect_width/8);
                  circle.setAttribute('cx', rect_x + rect_width/2);
                  circle.setAttribute('cy', rect_y + (num_elts - j - 0.5)*unit_height);
                  circle.setAttribute('fill', rect_fill);
                  // circle.setAttribute('fill-opacity', rect_fill_opacity);
                  // circle.setAttribute('stroke', rect_stroke);
                  // circle.setAttribute('stroke-width', rect_stroke_width);
                  // console.log('adding circle elt', i, j, '=', circle);
                  svgRoot.appendChild(circle);
                }
              });
            }

          });

        }
      };
    }


    function multiBarChart(globalOptions, rawData) {
      // Variables and constants
      const horizontal = isTrue(get(rawData, 'horizontal'));
      const axisloc = horizontal ? 'hAxes' : 'vAxes';
      const title = get(globalOptions, 'title');
      const width = get(globalOptions, 'width');
      const height = get(globalOptions, 'height');
      const background = getColorOrDefault(get(globalOptions, 'backgroundColor'), 'transparent');
      const axesConfig = dimensions[horizontal ? 'horizontal' : 'vertical']
      const colors_list = get_colors_list(rawData);
      const axis = get_axis(rawData);
      const legendsList = get(rawData, 'legends');
      const stackType = get(rawData, 'is-stacked');
      const isStacked = stackType !== 'none';
      const isNotFullStacked = (stackType !== 'relative') && (stackType !== 'percent');

      const data = [];

      const dataTable = {
        name: 'table',
        values: [],
        transform: [
          {
            type: "formula",
            as: "desc",
            expr: "isString(datum.annotation) ? datum.annotation : scale('legends', datum.series)"
          }
        ]
      };
      data.push(dataTable);
      if (isStacked) {
        if (stackType !== 'absolute') {
          dataTable.transform.push(
            {
              type: "joinaggregate",
              groupby: ["label"],
              ops: ["sum"],
              fields: ["rawValue"],
              as: ["totalValue"]
            },
            {
              type: "formula",
              as: "value",
              expr: "datum.rawValue/datum.totalValue",
            }
          );
        } else {
          dataTable.transform.push({ type: 'formula', as: 'value', expr: 'datum.rawValue' });
        }            
        dataTable.transform.push(
          {
            "type": "stack",
            "groupby": ["label"],
            "sort": {"field": "series"},
            "field": "value",
            "as": ["value0", "value1"],
          }
        );
      } else {
        dataTable.transform.push(
          { type: "formula", as: "value", expr: "datum.rawValue" },
          // Since there is no stacking happening, each bar goes from 0 to the value
          { type: 'formula', as: 'value0', expr: '0' },
          { type: 'formula', as: 'value1', expr: 'datum.value' },
        );
      }
      const signals = [
        {
          "name": "hoveredSeries",
          "value": "null",
          "on": [
            {
              "events": [
                { "markname": "legend-labels", "type": "mouseover" },
                { "markname": "legend-symbols", "type": "mouseover"}
              ],
              "force": true,
              "update": "datum.value"
            },
            {
              "events": [
                { "markname": "legend-labels", "type": "mouseout" },
                { "markname": "legend-symbols", "type": "mouseout"}
              ],
              "force": true,
              "update": "null"
            },
          ]
        }
      ];
      const primaryScale = {
        name: "primary",
        type: "band",
        range: axesConfig.primary.range,
        domain: {"data": "table", "field": "label"},
        padding: 0.2
      };
      const secondaryScale = {
        name: "secondary",
        type: "linear",
        range: axesConfig.secondary.range,
        nice: true, "zero": true,
        domain: {"data": "table", "field": "value1"}
      };
      const scales = [
        primaryScale,
        secondaryScale,
        {
          name: "color",
          type: "ordinal",
          domain: {data: "table", field: "series"},
          range: [...colors_list, ...default_colors]
        },
        {
          name: "legends",
          type: "ordinal",
          domain: Array.from(Array(legendsList.length).keys()),
          range: legendsList
        }
      ];
      if (axis) {
        scales.push({
          name: "secondaryLabels",
          type: "ordinal",
          domain: axis.domainRaw,
          range: axis.labels
        });
      }
      const axes = [
        { orient: axesConfig.primary.axes, scale: 'primary', zindex: 1 },
        { orient: axesConfig.secondary.axes, scale: 'secondary', zindex: 1,
          grid: false, ticks: isNotFullStacked, labels: isNotFullStacked },
        // redraw the axis just for its gridlines, but beneath everything else in z-order
        { orient: axesConfig.secondary.axes, scale: 'secondary', zindex: 0,
          grid: true, ticks: !isNotFullStacked, labels: !isNotFullStacked }
      ];
      if (axis) {
        axes[1].values = axis.domainRaw;
        axes[1].encode = {
          labels: { update: { text: { signal: "scale('secondaryLabels', datum.value)" } } }
        };
      }
      if (stackType === 'percent') {
        axes[1].format = axes[2].format = '.2%';
      }
      // These labels are *specifically directional*, not *logical* -- if a graph is flipped
      // from horizontal to vertical, the labels don't also get flipped.
      const xAxisLabel = get(globalOptions, 'x-axis');
      const yAxisLabel = get(globalOptions, 'y-axis');
      if (horizontal) {
        axes[isNotFullStacked ? 1 : 2].title = yAxisLabel;
        axes[0].title = xAxisLabel;
      } else {
        axes[isNotFullStacked ? 1 : 2].title = xAxisLabel;
        axes[0].title = yAxisLabel;
      }

      const marks = [];

      const groupMark = {
        type: 'group',
        from: {
          facet: {
            data: 'table',
            name: 'facet',
            groupby: 'label'
          }
        },
        data: [],
        encode: {
          enter: {
            [axesConfig.primary.dir]: { scale: 'primary', field: 'label' }
          }
        },
        signals: [
          { name: axesConfig.primary.range, update: "bandwidth('primary')" }
        ],
        scales: [
          {
            name: 'primaryGrouped',
            type: 'band',
            range: axesConfig.primary.range,
            domain: { data: 'facet', field: 'series' }
          }
        ],
        marks: []
      };
      let tooltipValue = "datum.Value";
      if (!isStacked) {
        marks.push(groupMark);
      } else {
        tooltipValue = `(datum.rawValue + ' (' + format(datum.value, '.2${stackType === 'percent' ? '%' : ''}') + ')')`;
      }


      const primaryScaleName = (isStacked ? "primary" : "primaryGrouped");
      const tooltips = [
        {
          test: "isArray(datum.intervals) && datum.intervals.length > 0",
          signal: `{title: datum.label, Series: datum.desc, Value: ${tooltipValue}, Intervals: datum.intervals}`
        },
        {
          signal: `{title: datum.label, Series: datum.desc, Value: ${tooltipValue}}`
        }
      ];
      constructDataTable(globalOptions, rawData,
                         (isStacked ? "table" : "facet"),
                         (isStacked ? "label" : "series"),
                         primaryScaleName,
                         tooltips,
                         dataTable,
                         (isStacked ? marks : groupMark.marks));

      // NOTE(Ben): Multi-bar charts don't support images per bar, though I suppose we could.
      // addImages(globalOptions, rawData, primaryScaleName, tooltips, data, (isStacked ? marks : groupMark.marks));

      // always use the ungrouped primary scale, so it stretches across the whole chart
      addPointers(globalOptions, rawData, "primary", data, marks);

      addAnnotations(globalOptions, rawData,
                     (isStacked ? "table" : "facet"),
                     (isStacked ? "label" : "series"),
                     (isStacked ? "primary" : "primaryGrouped"),
                     (isStacked ? data : groupMark.data),
                     (isStacked ? marks : groupMark.marks));

      if (!isStacked) {
        addIntervals(globalOptions, rawData,
                     (isStacked ? "table" : "facet"),
                     (isStacked ? "label" : "series"),
                     (isStacked ? "primary" : "primaryGrouped"),
                     (isStacked ? data : groupMark.data),
                     (isStacked ? marks : groupMark.marks));
      }
      
      const legends = [
        {
          direction: horizontal ? 'vertical' : 'horizontal',
          orient: horizontal ? 'right' : 'top',
          columns: horizontal ? 1 : 4,
          type: 'symbol',
          fill: 'color',
          symbolType: 'square',
          encode: {
            labels: {
              name: 'legend-labels',
              interactive: true,
              update: {
                text: { scale: 'legends', field: 'value' },
                cursor: { value: 'pointer' },
              }
            },
            symbols: {
              name: 'legend-symbols',
              interactive: true,
              update: { cursor: { value: 'pointer' } }
            }
          }
        }
      ];
      
      return {
        "$schema": "https://vega.github.io/schema/vega/v6.json",
        description: title,
        title: title ? { text: title } : '',
        width,
        height,
        padding: 0,
        autosize: "fit",
        background,
        data,
        signals,
        scales,
        axes,
        marks,
        legends,
        onExit: defaultImageReturn,
        gChartOverlay: (overlay, restarter, chart, container) => {

          if (!hasImage && !dotChartP) return;

            if (dotChartP) {
              table.forEach(function (row, i) {
                // console.log('row', i, '=', row);
                const rect = rects[i];
                // console.log('rect', i, '=', rect);
                const num_elts = row[1];
                const rect_x = Number(rect.getAttribute('x'));
                const rect_y = Number(rect.getAttribute('y'));
                const rect_height = Number(rect.getAttribute('height'));
                const unit_height = rect_height/num_elts;
                const rect_width = Number(rect.getAttribute('width'));
                const rect_fill = rect.getAttribute('fill');
                // const rect_fill_opacity = Number(rect.getAttribute('fill-opacity'));
                // const rect_stroke = rect.getAttribute('stroke');
                // const rect_stroke_width = Number(rect.getAttribute('stroke-width'));
                rect.setAttribute('stroke-width', 0);
                for (let j = 0; j < num_elts; j++) {
                  const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
                  circle.classList.add('__img_labels');
                  circle.setAttribute('r', rect_width/8);
                  circle.setAttribute('cx', rect_x + rect_width/2);
                  circle.setAttribute('cy', rect_y + (num_elts - j - 0.5)*unit_height);
                  circle.setAttribute('fill', rect_fill);
                  // circle.setAttribute('fill-opacity', rect_fill_opacity);
                  // circle.setAttribute('stroke', rect_stroke);
                  // circle.setAttribute('stroke-width', rect_stroke_width);
                  // console.log('adding circle elt', i, j, '=', circle);
                  svgRoot.appendChild(circle);
                }
              });
            }

          });

        }
      }
    }

    function boxPlot(globalOptions, rawData) {
      let table = get(rawData, 'tab');
      const dimension = toFixnum(get(rawData, 'height'));
      // TODO: are these two supposed to be on ChartWindow or DataSeries?
      const horizontal = get(rawData, 'horizontal');
      const showOutliers = get(rawData, 'show-outliers');
      const axisName = horizontal ? 'hAxis' : 'vAxis';
      const chartType = horizontal ? google.visualization.BarChart : google.visualization.ColumnChart;
      const data = new google.visualization.DataTable();

      const color = cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'color'), {
        none: function () {
          return "#777";
        },
        some: function (color) {
          return convertColor(color);
        }
      });


      const intervalOptions = {
        lowNonOutlier: {
          style: 'bars',
          fillOpacity: 1,
          color: color
        },
        highNonOutlier: {
          style: 'bars',
          fillOpacity: 1,
          color: color
        }
      };

      data.addColumn('string', 'Label');
      data.addColumn('number', 'Total');
      data.addColumn({id: 'firstQuartile', type: 'number', role: 'interval'});
      data.addColumn({id: 'median', type: 'number', role: 'interval'});
      data.addColumn({id: 'thirdQuartile', type: 'number', role: 'interval'});
      data.addColumn({id: 'highNonOutlier', type: 'number', role: 'interval'});
      data.addColumn({id: 'lowNonOutlier', type: 'number', role: 'interval'});
      data.addColumn({type: 'string', role: 'tooltip', 'p': {'html': true}});

      // NOTE(joe & emmanuel, Aug 2019): With the current chart library, it seems
      // like we can only get outliers to work as a variable-length row if we
      // have a single row of data. It's an explicit error to mix row lengths.
      // Since the main use case where outliers matter is for single-column
      // box-plots, this maintains existing behavior (if anyone was relying on
      // multiple series), while adding the ability to render outliers for BS:DS.
      if(table.length === 1 && showOutliers) {
        var extraCols = table[0][8].length + table[0][9].length;
        for(var i = 0; i < extraCols; i += 1) {
          data.addColumn({id: 'outlier', type: 'number', role: 'interval'});
        }
        intervalOptions['outlier'] = { 'style':'points', 'color':'grey', 'pointSize': 10, 'lineWidth': 0, 'fillOpacity': 0.3 };
      }
      else {
        // NOTE(joe & emmanuel, Aug 2019 cont.): This forces the low and high
        // whiskers to be equal to the min/max when there are multiple rows since we
        // won't be able to render the outliers, and the whiskers need to cover
        //  the whole span of data.
        table = table.map(function(row) {
          row = row.slice(0, row.length);
          // force whisker to be max/min
          row[7] = row[2];
          row[6] = row[1];
          // empty outliers
          row[9] = [];
          row[8] = [];
          return row;
        });
      }

      const rowsToAdd = table.map(row => {
        const summaryValues = row.slice(3, 8).map(n => toFixnum(n));
        let tooltip = `<p><b>${row[0]}</b></p>
            <p>minimum: <b>${row[2]}</b></p>
            <p>maximum: <b>${row[1]}</b></p>
            <p>first quartile: <b>${summaryValues[0]}</b></p>
            <p>median: <b>${summaryValues[1]}</b></p>
            <p>third quartile: <b>${summaryValues[2]}</b></p>`;
        // ONLY if we're showing outliers, add whiskers to the tooltip
        // (otherwise, the min/max ARE the bottom/top whiskers)
        if(table.length == 1 && showOutliers) {
          tooltip += 
            ` <p>bottom whisker: <b>${summaryValues[4]}</b></p>
            <p>top whisker: <b>${summaryValues[3]}</b></p>`;
        }
        return [row[0], toFixnum(dimension)]
          .concat(summaryValues)
          .concat([tooltip])
          .concat(row[9]).concat(row[8]);
      });

      data.addRows(rowsToAdd);
      const options = {
        tooltip: {isHtml: true},
        legend: {position: 'none'},
        lineWidth: 0,
        intervals: {
          barWidth: 0.25,
          boxWidth: 0.8,
          lineWidth: 2,
          color: color,
          style: 'boxes'
        },
        interval: intervalOptions,
        dataOpacity: 0,
      };

      /* NOTE(Oak): manually set the default max to coincide with bar charts' height
       * so that the bar charts are concealed (the automatic value from Google
       * is likely to screw this up)
       */
      const axisOpts = {
        maxValue: dimension,
        viewWindow: {
          max: dimension
        },      
      };
      /* NOTE(Emmanuel): if min and max are set, override these defaults
       * 
       */
      cases(RUNTIME.ffi.isOption, 'Option', get(globalOptions, 'min'), {
        none: function () {},
        some: function (min) {
          axisOpts.viewWindow.min = toFixnum(min);
        }
      });
      cases(RUNTIME.ffi.isOption, 'Option', get(globalOptions, 'max'), {
        none: function () {},
        some: function (max) {
          axisOpts.viewWindow.max = toFixnum(max);
        }
      });
      options[axisName] = axisOpts;

      return {
        data: data,
        options: options,
        chartType: chartType,
        onExit: defaultImageReturn,
        mutators: [backgroundMutator, axesNameMutator],
      };
    }

    function histogram(globalOptions, rawData) {
      const table = get(rawData, 'tab');
      const data = new google.visualization.DataTable();

      data.addColumn('string', 'Label');
      data.addColumn('number', '');
      
      var max, min;
      var val = null;
      var hasAtLeastTwoValues = false;
      data.addRows(table.map(row => {
        var valfix = toFixnum(row[1]);
        if(val !== null && val !== valfix) { hasAtLeastTwoValues = true; }
        if(val === null) { val = valfix; }
        if(max === undefined) { max = valfix; }
        if(min === undefined) { min = valfix; }
        if(valfix > max) { max = valfix; }
        if(valfix < min) { min = valfix; }
        return [row[0], valfix];
      }));

      // ASSERT: if we're using custom images, there will be a 4th column
      const hasImage = table[0].length == 3;

      // set legend to none because there's only one data set
      const options = {
        legend: {position: 'none'}, 
        histogram: {},
        series : {
          0 : { dataOpacity : hasImage? 0 : 1.0 }
        }
      };

      cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'bin-width'), {
        none: function () {},
        some: function (binWidth) {
          // NOTE(joe, aug 2019): The chart library has a bug for histograms with
          // a single unique value (https://jsfiddle.net/L0y64fbo/2/), so thisi
          // hackaround makes it so this case can't come up.
          if(hasAtLeastTwoValues) {
            options.histogram.bucketSize = toFixnum(binWidth);
          }
        }
      });

      cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'max-num-bins'), {
        none: function () {},
        some: function (maxNumBins) {
          options.histogram.maxNumBuckets = toFixnum(maxNumBins);
        }
      });

      cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'min-num-bins'), {
        none: function () {
          if(options.histogram.bucketSize !== undefined) {
            options.histogram.minNumBuckets = Math.floor((max - min) / options.histogram.bucketSize) + 1; 
          }
        },
        some: function (minNumBins) {
          options.histogram.minNumBuckets = toFixnum(minNumBins);
        }
      });

      cases(RUNTIME.ffi.isOption, 'Option', get(rawData, 'color'), {
        none: function () {},
        some: function (color) {
          options.colors = [convertColor(color)];
        }
      });
      
      return {
        data: data,
        options: options,
        chartType: google.visualization.Histogram,
        onExit: defaultImageReturn,
        mutators: [backgroundMutator, axesNameMutator, yAxisRangeMutator, xAxisRangeMutator],
        overlay: (overlay, restarter, chart, container) => {
          if(!hasImage) return;

          // if custom images are defined, use the image at that location
          // and overlay it atop each dot
          google.visualization.events.addListener(chart, 'ready', function () {
            // HACK(Emmanuel): 
            // The only way to hijack rect events is to walk the DOM here
            // If Google changes the DOM, these lines will likely break
            const svgRoot = chart.container.querySelector('svg');
            const rectRoot = svgRoot.children[1].children[1].children[1];
            const rects = rectRoot.children;

            // remove any labels that have previously been drawn
            $('.__img_labels').each((idx, n) => $(n).remove());

            // sort the table in value-order, so the images are in the same
            // order as the data used to draw the rects
            table.sort((r1,r2) => (toFixnum(r1[1]) < toFixnum(r2[1]))? -1 : 0)

            // walk the table and swap in the images for the rects
            table.forEach(function (row, i) {
              const rect = rects[i];
              // make an image element for the img, from the SVG namespace
              const imgDOM = row[2].val.toDomNode();
              row[2].val.render(imgDOM.getContext('2d'), 0, 0);
              let imageElt = document.createElementNS("http://www.w3.org/2000/svg", 'image');
              imageElt.classList.add('__img_labels'); // tag for later garbage collection
              imageElt.setAttributeNS(null, 'href', imgDOM.toDataURL());
              // position it using the position of the corresponding rect
              imageElt.setAttribute('preserveAspectRatio', 'none');
              imageElt.setAttribute('x', rects[i].getAttribute('x'));
              imageElt.setAttribute('y', rects[i].getAttribute('y'));
              imageElt.setAttribute('width', rects[i].getAttribute('width'));
              imageElt.setAttribute('height', rects[i].getAttribute('height'));
              Object.assign(imageElt, rects[i]); // we should probably not steal *everything*...
              rectRoot.appendChild(imageElt);
            });
          })
        }
      };
    }

    function plot(globalOptions, rawData) {
      const scatters = get(rawData, 'scatters');
      const lines = get(rawData, 'lines');
      const intervals = get(rawData, 'intervals');
      const minIntervalIndex = scatters.length + lines.length;
      const data = new google.visualization.DataTable();
      data.addColumn('number', 'X');
      const combined = scatters.concat(lines).concat(intervals);
      const legends = [];
      let cnt = 1;
      const legendEnabled = combined.length > 1;
      combined.forEach((p, i) => {
        let legend = get(p, 'legend');
        if (legend === '') {
          legend = `Plot ${cnt}`;
          cnt++;
        }
        legends.push(legend);
        data.addColumn('number', legend);
        data.addColumn({type: 'string', role: 'tooltip', 'p': {'html': true}});
        data.addColumn({id: 'i0', type: 'number', role: 'interval'});
        data.addColumn({id: 'i1', type: 'number', role: 'interval'});
      });

      combined.forEach((p, i) => {
        /* 

           combined.length = number of charts (regardless of kind).
           total length of a row = 1 + 4 × (combined.length).
           a row looks like:

           x | aaaa aaaa aaaa | yyyy | bbbb bbbb bbbb

           each chart contributes as many rows as it has x-y mappings.
           each such row has first column = x and 
           a 4-tuple y₁y₂y₃y₄ in the appropriately staggered y-slots, where:
           y₁ = y
           y₂ = label
           y₃ = y (again, only for interval charts)
           y₄ = y-predicted (only for interval charts)

           all other slots in a row are null.
        */

        const rowTemplate = new Array(combined.length * 4 + 1).fill(null);
        const intervalP = (i >= minIntervalIndex);
        const dotChartP = Boolean(get(p, 'dot-chart'));

        if(dotChartP) {
          data.addRows(get(p, 'ps').map(row => {
            const currentRow = rowTemplate.slice();
            if (row.length != 0) {
              currentRow[0] = toFixnum(row[0]);
              currentRow[4*i + 1] = toFixnum(row[1]);
              let labelRow = null;
              if (row.length >= 3 && row[2] !== '') {
                labelRow = `<p>label: <b>${row[2]}</b></p>`;
              } else {
                labelRow = '';
              }
              currentRow[4*i + 2] = `<p>${legends[i]}</p>
<p>x: <b>${currentRow[0]}</b></p>
${labelRow}`;
            }
            return currentRow;
          }));
        } else if(intervalP) {
          data.addRows(get(p, 'tab').map(row => {
            const currentRow = rowTemplate.slice();
            if (row.length != 0) {
              const r0 = toFixnum(row[0]);
              const r1 = toFixnum(row[1]);
              const r2 = toFixnum(row[2]);

              currentRow[0] = r0;
              currentRow[4*i + 1] = r1;
              const labelRow = `<p>label: <b>${r2}</b></p>`;
              currentRow[4*i + 2] = `<p>${legends[i]}</p>
<p>x: <b>${r0}</b></p>
<p>y: <b>${fourSig(r1)}</b></p>
<p>ŷ: <b>${fourSig(r2)}</b></p>
<p>y - ŷ: <b>${saneSubtract(r1, r2)}</b></p>`;
              currentRow[4*i + 3] = r1;
              currentRow[4*i + 4] = r2;
            }
            return currentRow;
          }));
        } else {
          data.addRows(get(p, 'ps').map(row => {
            const currentRow = rowTemplate.slice();
            if (row.length != 0) {
              currentRow[0] = toFixnum(row[0]);
              currentRow[4*i + 1] = toFixnum(row[1]);
              let labelRow = null;
              if (row.length >= 3 && row[2] !== '') {
                labelRow = `<p>label: <b>${row[2]}</b></p>`;
              } else {
                labelRow = '';
              }
              currentRow[4*i + 2] = `<p>${legends[i]}</p>
<p>x: <b>${currentRow[0]}</b></p>
<p>y: <b>${currentRow[4*i + 1]}</b></p>
${labelRow}`;
              // leave currentRow[4*i + 3] and [4*i + 4] null
            }
            return currentRow;
          }));
        }
      });

      // ASSERT: if we're using custom images, *every* series will have idx 3 defined
      const hasImage = combined.every(p => get(p, 'ps').filter(p => p[3]).length > 0);
      const dotChartP = combined.some(p => get(p, 'dot-chart'));
      const replaceDefaultSVG = (hasImage || dotChartP);

      const options = {
        tooltip: {isHtml: true},
        series: combined.map((p, i) => {
          
          // scatters and then lines
          const seriesOptions = {};

          cases(RUNTIME.ffi.isOption, 'Option', get(p, 'color'), {
            none: function () {},
            some: function (color) {
              seriesOptions.color = convertColor(color);
            }
          });
          // If we have our own image, make the point small and transparent
          if (i < scatters.length) {
            $.extend(seriesOptions, {
              pointSize: replaceDefaultSVG ? 0.1 : toFixnum(get(p, 'point-size')),
              lineWidth: 0,
              dataOpacity: replaceDefaultSVG ? 0 : 1,
            });
          } else if (i - scatters.length < lines.length) {
            $.extend(seriesOptions, {
              pointSize: hasImage ? 0.1 : toFixnum(get(p, 'point-size')),
              dataOpacity: hasImage ? 0 : 1,
            });
          } else if (i - scatters.length - lines.length < intervals.length) {

            let intervalStyle = get(p, 'style');
            let intervalStickColor = get_default_color(p);
            let intervalStickWidth = toFixnum(get(p, 'stick-width'));
            let intervalFillOpacity = ((intervalStyle == 'boxes') ? 0 : 1);
            let intervalPointColor = get_pointer_color(p);
            let intervalPointSize = toFixnum(get(p, 'point-size'));

            $.extend(seriesOptions, {
              pointSize: intervalPointSize,
              dataOpacity: 1,

              curveType: 'function',
              lineWidth: 0,
              intervals: { 
                style: 'sticks',
                lineWidth: 2,
              },
              interval: {
                'i0': {
                  'style': intervalStyle,
                  'color': intervalStickColor,
                  'lineWidth': intervalStickWidth,
                  'barWidth': 0,
                  'pointSize': 0,
                  'fillOpacity': intervalFillOpacity,
                },
                'i1': {
                  'style': intervalStyle,
                  'color': intervalPointColor,
                  'pointSize': intervalPointSize,
                  'barWidth': 0,
                  'lineWidth': 4,
                  'fillOpacity': intervalFillOpacity,
                },
              }

            });
          }
          return seriesOptions;
        }),
        legend: {position: legendEnabled ? 'bottom' : 'none'},
        crosshair: {trigger: 'selection'}
      };

      if (lines.length != 0) {
        const line0 = lines[0];
        const curveType = get(line0, 'curved');
        const lineWidth = toFixnum(get(line0, 'lineWidth'));

        
        const dashedLine = get(line0, 'dashedLine');
        const dashlineStyle = get(line0, 'dashlineStyle');
        const pointSize = toFixnum(get(line0, 'point-size'));
        

        options['curveType'] = curveType;
        options['lineWidth'] = lineWidth;
        options['pointSize'] = pointSize;
        
        if (dashedLine) {
          options['lineDashStyle'] = dashlineStyle;
        }
      }

      const ser0 = combined[0];

      const trendlineType = cases(RUNTIME.ffi.isOption, 'Option', get(ser0, 'trendlineType'), {
        none: function () {
          return null;
        },
        some: function (type) {
          return type;
        }
      });

      const trendlineColor = cases(RUNTIME.ffi.isOption, 'Option', get(ser0, 'trendlineColor'), {
        none: function () {
          return 'green';
        },
        some: function (color) {
          return convertColor(color);
        }
      });

      const trendlineWidth = toFixnum(get(ser0, 'trendlineWidth'));
      const trendlineOpacity = toFixnum(get(ser0, 'trendlineOpacity'));
      const trendlineDegree = toFixnum(get(ser0, 'trendlineDegree'));

      if (trendlineType != null) {
        options['trendlines'] = {
          0: {
            type: trendlineType,
            color: trendlineColor,
            lineWidth: trendlineWidth,
            opacity: trendlineOpacity,
            showR2: true,
            visibleInLegend: true
          }
        }
      }
      if (trendlineType == "polynomial") {
        options['trendlines'][0]['degree'] = trendlineDegree;
      }

      // by default, dotPlotAxesMutator does nothing
      let dotPlotAxesMutator = (options, globalOptions, _) => {return false};
      if (dotChartP) {
        // for the hAxis, we need to custom-calculate the tick marks
        // copied from erison.blogspot.com/2011/07/algorithm-for-optimal-scaling-on-chart.html
        dotPlotAxesMutator = (options, globalOptions, _) => {
          const xValues = combined.map(p => get(p, 'ps').map(r => toFixnum(r[0]))).flat();
          const xMin = Math.min(...xValues);
          const xMax = Math.max(...xValues);
          const maxTicks = 8;

          function niceNum(range, round) {
            const exponent = Math.floor(Math.log10(range));
            const fraction = range / Math.pow(10, exponent);
            let niceFraction;

            if (round) {
              if(fraction < 1.5)     niceFraction =  1;
              else if(fraction < 3)  niceFraction =  2;
              else if(fraction < 7)  niceFraction =  5;
              else                   niceFraction = 10;
            } else {
              if (fraction <= 1)     niceFraction =  1;
              else if(fraction <= 2) niceFraction =  2;
              else if(fraction <= 5) niceFraction =  5;
              else                   niceFraction = 10;
            }

            return niceFraction * Math.pow(10, exponent);
          }

          const range       = niceNum(xMax - xMin, false);
          const tickSpacing = niceNum(range / (maxTicks - 1), true);
          let niceMin     = Math.floor(xMin / tickSpacing) * tickSpacing;
          let niceMax     = Math.ceil(xMax / tickSpacing) * tickSpacing;
          // add an extra tick if the data falls exactly on the boundaries
          if(xMin == niceMin) niceMin = niceMin - tickSpacing;
          if(xMax == niceMax) niceMax = niceMax + tickSpacing;      let hTicks        = [niceMin]; // start at the bottom and add
          while(hTicks.slice(-1) < niceMax) { hTicks.push(Number(hTicks.slice(-1)) + tickSpacing); }

          //console.log('niceMin', niceMin, 'niceMax', niceMax, 'tickSpacing', tickSpacing, 'ticks', hTicks);

          // ticks [] as we don't want horizontal grid lines;
          // maxValue must be set to something as otherwise having
          // all dots at y=0 causes vAxis to be centered at 0;
          // we don't want any chart real estate below x-axis
          options['vAxis'] = {
            ...options['vAxis'],
            ...{ticks: [], viewWindow: {min: 0, max: 10}}
          };
          options['hAxis'] = {
            ...options['hAxis'],
            ...{viewWindow: {min: niceMin, max: niceMax}}
          };
        }
      }

      const pointshapeType = get(ser0, 'pointshapeType');
      const pointshapeSides = toFixnum(get(ser0, 'pointshapeSides'));
      const pointshapeDent = toFixnum(get(ser0, 'pointshapeDent'));
      const pointshapeRotation = toFixnum(get(ser0, 'pointshapeRotation'));
      const apothem = Math.cos(Math.PI / pointshapeSides)
      
      if (pointshapeType != 'circle') {
        options['pointShape'] = {
          type: 'star',
          sides: pointshapeSides, 
          dent: (pointshapeDent + 1) * apothem + 0.01,
          rotation: pointshapeRotation,
        }
      }

      if (isTrue(get(globalOptions, 'interact'))) {
        $.extend(options, {
          chartArea: {
            left: '12%',
            width: dotChartP? '76%' : '56%',
          }
        });
      }

      return {
        data: data,
        options: options,
        chartType: google.visualization.LineChart,
        onExit: (restarter, result) => {
          let svg = result.chart.container.querySelector('svg');
          let svg_xml = (new XMLSerializer()).serializeToString(svg);
          let dataURI = "data:image/svg+xml;base64," + btoa(unescape(encodeURIComponent(svg_xml)));
          imageReturn(
            dataURI,
            restarter,
            RUNTIME.ffi.makeRight)
        },
        mutators: [axesNameMutator,
                   yAxisRangeMutator,
                   xAxisRangeMutator,
                   gridlinesMutator,
                   backgroundMutator, 
                   selectMultipleMutator,
                   dotPlotAxesMutator],
        overlay: (overlay, restarter, chart, container) => {
          if(!dotChartP) {
            overlay.css({
              width: '30%',
              position: 'absolute',
              right: '0px',
              top: '50%',
              transform: 'translateY(-50%)',
            });

            const controller = $('<div/>');

            overlay.append(controller);

            const inputSize = 16;

            const xMinC = $('<input/>', {
              'class': 'controller',
              type: 'text',
              placeholder: 'x-min',
            }).attr('size', inputSize);
            const xMaxC = $('<input/>', {
              'class': 'controller',
              type: 'text',
              placeholder: 'x-max',
            }).attr('size', inputSize);
            const yMinC = $('<input/>', {
              'class': 'controller',
              type: 'text',
              placeholder: 'y-min',
            }).attr('size', inputSize);
            const yMaxC = $('<input/>', {
              'class': 'controller',
              type: 'text',
              placeholder: 'y-max',
            }).attr('size', inputSize);
            const numSamplesC = $('<input/>', {
              'class': 'controller',
              type: 'text',
              placeholder: '#samples',
            }).attr('size', inputSize).val('2');
            // dummy value so that a new window can be constructed correctly
            // when numSamplesC is not used. The value must be at least 2

            const redrawC = $('<button/>', {
              'class': 'controller',
              text: 'Redraw',
            }).click(() => {
              const newWindow = getNewWindow(xMinC, xMaxC, yMinC, yMaxC, numSamplesC);
              if (newWindow === null) return;
              const toRet = RUNTIME.ffi.makeLeft(
                RUNTIME.extendObj(
                  RUNTIME.makeSrcloc('dummy location'),
                  globalOptions,
                  newWindow
                )
              );
              RUNTIME.getParam('remove-chart-port')();
              restarter.resume(toRet);
            });

            function getBoundControl(control, name) {
              control.val(prettyNumToStringDigits5(
                get(get(globalOptions, name), 'value')));
              return $('<p/>')
                .append($('<label/>', {'class': 'controller', text: name + ': '}))
                .append(control);
            }

            const xMinG = getBoundControl(xMinC, 'x-min');
            const xMaxG = getBoundControl(xMaxC, 'x-max');
            const yMinG = getBoundControl(yMinC, 'y-min');
            const yMaxG = getBoundControl(yMaxC, 'y-max');
            const redrawG = $('<p/>').append(redrawC);

            if (isTrue(get(globalOptions, 'is-show-samples'))) {
              numSamplesC.val(RUNTIME.num_to_string(get(globalOptions, 'num-samples')));
              const numSamplesG = $('<p/>')
                    .append($('<label/>', {'class': 'controller', text: '#samples: '}))
                    .append(numSamplesC);
              controller
                .append(xMinG)
                .append(xMaxG)
                .append(yMinG)
                .append(yMaxG)
                .append(numSamplesG)
                .append(redrawG);
            } else {
              controller
                .append(xMinG)
                .append(xMaxG)
                .append(yMinG)
                .append(yMaxG)
                .append(redrawG);
            }
          }

          if (!replaceDefaultSVG) { return; } // If we don't have images, our work is done!
          
          // if custom images are defined, use the image at that location
          // and overlay it atop each dot
          google.visualization.events.addListener(chart, 'ready', function () {
            // HACK(Emmanuel): 
            // The only way to hijack marker events is to walk the DOM here
            // If Google changes the DOM, these lines will likely break
            // NOTE(joe, April 2022): It sort of happened. When we made the legend
            // sometimes not show (autohiding on single series), it shifted the
            // index. So this would only work if .title() was set. Use
            // legendEnabled to decided which index to look up.
            // This is brittle and needs to be revisited
            const svgRoot = chart.container.querySelector('svg');
            const layout = chart.getChartLayoutInterface();
            // remove any labels that have previously been drawn
            $('.__img_labels').each((idx, n) => $(n).remove());

            let markers;
            if(legendEnabled) {
              markers = svgRoot.children[2].children[2].children;
            } else {
              markers = svgRoot.children[1].children[2].children;
            }
            if (hasImage) {

              // for each point, (1) find the x,y location, (2) render the SVGImage,
              // (3) center it on the datapoint, (4) steal all the events
              // and (5) add it to the chart
              combined.forEach((p, i) => {
                get(p, 'ps').filter(p => p[3]).forEach((p, i) => {
                  const xPos = layout.getXLocation(data.getValue(i, 0));
                  const yPos = layout.getYLocation(data.getValue(i, 1));
                  const imgDOM = p[3].val.toDomNode();
                  p[3].val.render(imgDOM.getContext('2d'), 0, 0);
                  // make an image element from the SVG namespace
                  let imageElt = document.createElementNS("http://www.w3.org/2000/svg", 'image');
                  imageElt.classList.add('__img_labels'); // tag for later garbage collection
                  imageElt.setAttributeNS(null, 'href', imgDOM.toDataURL());
                  imageElt.setAttribute('x', xPos - imgDOM.width/2);  // center the image
                  imageElt.setAttribute('y', yPos - imgDOM.height/2); // center the image
                  Object.assign(imageElt, markers[i]); // we should probably not steal *everything*...
                  svgRoot.appendChild(imageElt);
                });
              });
            }

            if (dotChartP) {
              // get bounding box of graph boundaries, and set the
              // chartCeiling to reserve the top 20% of the graph area
              const graphBounds  = svgRoot.children[1].children[0].getBoundingClientRect();
              let   chartCeiling = graphBounds.top - svgRoot.getBoundingClientRect().top;
              chartCeiling      += 0.2 * graphBounds.height;
              const usableHeight = 0.8 * graphBounds.height

              const circles = [...markers].filter(m => m.nodeName == 'circle');
              const diameter = toFixnum(get(combined[0], 'point-size'));
              const circleR = diameter / 2;
              let prevDotArray = [];
              function tooClose(x, y) {
                return prevDotArray.some( n => 
                  (Math.abs(x - n[0]) < diameter) && (Math.abs(y - n[1]) < diameter)
                );
              }

              // compute circleY, and add a new SVG to the graph
              circles.forEach( (circle, i) => {
                const circleX = Number(circle.getAttribute('cx'));
                
                // Shift the circle up by r+1, so it sits on the x-axis
                let   circleY = Number(circle.getAttribute('cy')) - (circleR + 1);
                
                // If it's too close to any existing circle, shift up by 1 diameter
                while (tooClose(circleX, circleY)) { circleY -= diameter;}
                
                // If the new circleY goes above the ceiling, place it randomly
                // within the first 80% of the vHeight, using (1-random^2) to
                // bias the randomness towards low portions of the graph
                if(circleY < chartCeiling) {
                  const randomVHeight = (1 - Math.random()**2) * usableHeight;
                  circleY = randomVHeight + chartCeiling - circleR;
                }
                // save the location of the new dot along with all the others
                prevDotArray.push([circleX, circleY]);

                const circleElt = circle.cloneNode(false);
                circleElt.classList.add('__img_labels'); // tag for later gc
                circleElt.setAttribute('cy', circleY);
                circleElt.setAttribute('r', circleR);
                circleElt.setAttribute('stroke', 'white');
                circleElt.setAttribute('stroke-width', '1');
                circleElt.setAttribute('fill-opacity', '0.8');
                Object.assign(circleElt, circle); // we should probably not steal *everything*...
                svgRoot.appendChild(circleElt);
              });

            }
          });
        },
      };
    }

    //////////////////////////////////////////////////////////////////////////////


    function onExitRetry(resultGetter, restarter) {
      const result = resultGetter();
      if (result !== null) {
        result.onExit(restarter, result);
      } else {
        setTimeout(onExitRetry, 100, resultGetter, restarter);
      }
    }


    function imageDataReturn(imageData, restarter, hook) {
      restarter.resume(
        hook(
          RUNTIME.makeOpaque(
            IMAGE.makeImageDataImage(imageData),
            IMAGE.imageEquals
          )
        )
      );
    }

    function defaultImageReturn(restarter, result) {
      /*
        We in fact should put imageReturn(...) inside

        google.visualization.events.addListener(result.chart, 'ready', () => {
        ...
        });

        However, somehow this event is never triggered, so we will just call
        it here to guarantee that it will return.
      */

      // serialize the whole SVG element, in case of custom image overlays
      // then pass the URI to imageReturn`
      result.view.toSVG().then((svg) => {
        let dataURI = "data:image/svg+xml;base64," + btoa(unescape(encodeURIComponent(svg)));
        const rawImage = new Image();
        rawImage.onload = () => {
          restarter.resume(
            RUNTIME.makeOpaque(
              IMAGE.makeFileImage(url, rawImage),
              IMAGE.imageEquals
            )
          );
        };
        rawImage.onerror = e => {
          restarter.error(
            RUNTIME.ffi.makeMessageException(
              'unable to load the image: ' + e.message));
        };
        rawImage.src = dataURI;
      })
    }

    function renderStaticImage(processed, globalOptions, rawData) {
      return RUNTIME.pauseStack(restarter => {
        try {
          console.log(JSON.stringify(processed, (k, v) => (v && IMAGE.isImage(v)) ? v.ariaText : v, 2));
          const width = toFixnum(get(globalOptions, 'width'));
          const height = toFixnum(get(globalOptions, 'height'));
          const canvas = canvasLib.createCanvas(width, height);
          const view = new vega.View(vega.parse(processed));
          const externalContext = canvas.getContext('2d');
          view.width(width).height(height).resize();
          view.runAsync()
            // NOTE(Ben): this externalContext *should* be unnecessary, but for some reason,
            // vega-as-bundled-in-a-jarr doesn't seem to notice NodeCanvas correctly
            .then(() => view.toCanvas(1, { externalContext }))
            .then(() => {
              imageDataReturn(externalContext.getImageData(0, 0, canvas.width, canvas.height), restarter, x => x);
            });
        } catch(e) {
          return restarter.error(e);
        }
      });
    }

    function renderInteractiveChart(processed, globalOptions, rawData) {
      return RUNTIME.pauseStack(restarter => {
          const root = $('<div/>');
          const overlay = $('<div/>', {style: 'position: relative'});

          const width = toFixnum(get(globalOptions, 'width'));
          const height = toFixnum(get(globalOptions, 'height'));
          const vegaTooltipHandler = new vegaTooltip.Handler();
          const view = new vega.View(vega.parse(processed), {
            container: overlay[0],
            renderer: 'svg',
            hover: true,
            tooltip: vegaTooltipHandler.call
          });
          view.width(width).height(height).resize();

          var tmp = processed;
          tmp.view = view;
          const options = {
            backgroundColor: {fill: 'transparent'},
            title: (get(globalOptions, 'title') || {}).text,
          };

          tmp.options = $.extend({}, options, 'options' in tmp ? tmp.options : {});

          delete tmp.width;
          delete tmp.height;

          // only mutate result when everything is setup
          const result = tmp;
          // this draw will have a wrong width / height, but do it for now so
          // that overlay works
        try {
          view.runAsync()
            .then(() => {
              // return true;
              RUNTIME.getParam('chart-port')({
                root: root[0],
                onExit: () => {
                  // In case the tooltip was currently being shown while the window was being closed
                  $("#vg-tooltip-element").removeClass("visible");
                  onExitRetry(() => result, restarter);
                },
                draw: () => {
                  // must append the overlay _after_ drawing to make the overlay appear
                  // correctly
                  view.runAsync().then(() => root.append(overlay))
                },
                windowOptions: {  },
                isInteractive: true,
                getImageURI: () => view.toImageURI('png'),
              });
            });
        } catch(e) {
          return restarter.error(e);
        }
      });
    }
    
    function makeFunction(f) {
      return RUNTIME.makeFunction((globalOptions, rawData) => {
        const isInteractive = isTrue(get(globalOptions, 'interact'));
        if (isInteractive) {
          if (RUNTIME.hasParam('chart-port')) {
            return renderInteractiveChart(f(globalOptions, rawData), globalOptions, rawData);
          } else {
            return RUNTIME.ffi.throwMessageException("Cannot display interactive charts headlessly");
          }
        } else {
          return renderStaticImage(f(globalOptions, rawData), globalOptions, rawData);
        }
      });
    }

    return RUNTIME.makeModuleReturn(
      {
        'pie-chart': notImp('pie-chart'), //makeFunction(pieChart),
        'bar-chart': makeFunction(barChart),
        'multi-bar-chart': makeFunction(multiBarChart),
        'histogram': notImp('histogram'), //makeFunction(histogram),
        'box-plot': notImp('box-plot'), //makeFunction(boxPlot),
        'plot': notImp('plot'), //makeFunction(plot),
      }, 
      {
        "LoC": ann("List<Color>", checkListWith(IMAGE.isColorOrColorString)),
        "LoS": ann("List<String>", checkListWith(RUNTIME.isString)), 
        "LoN": ann("List<Number>", checkListWith(RUNTIME.isNumber)),
        "LoI": ann("List<Image>", checkListWith(v => RUNTIME.isOpaque(v) && IMAGE.isImage(v.val))),
        "LoLoN": ann("List<List<Number>>", checkListWith(checkListWith(RUNTIME.isNumber))),
        "LoLoLoN": ann("List<List<List<Number>>>", checkListWith(checkListWith(checkListWith(RUNTIME.isNumber)))),
        "LoOoS": ann("List<Option<String>>", checkListWith(checkOptionWith(RUNTIME.isString))),
        "LoLoOoS": ann("List<List<Option<String>>>", checkListWith(checkListWith(checkOptionWith(RUNTIME.isString)))),
        "LoNi": ann("List<NumInteger>", checkListWith(v => RUNTIME.isNumber(v) && RUNTIME.num_is_integer(v))),
      }
    )
  }
})
