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
      'dot-chart': "tany",
      'categorical-dot-chart': "tany",
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
      return { value: toFixnum(get(p, 'value')) , label: get(p, 'label') }
    }

    //////////////////////////////////////////////////////////////////////////////

    function getNewWindow(raw) {
      const ret = {}
      for (const rawVal in raw) {
        const num = RUNTIME.string_tonumber(raw[rawVal].val());
        if (RUNTIME.isNothing(num)) {
          raw[rawVal].addClass('error-bg');
          raw[rawVal].removeClass('ok-bg');
          return null;
        } else {
          ret[rawVal] = num;
        }
      }
      if (jsnums.greaterThanOrEqual(ret.xMinValue, ret.xMaxValue, RUNTIME.NumberErrbacks)) {
        raw.xMinValue.addClass('error-bg');
        raw.xMaxValue.addClass('error-bg');
        raw.xMinValue.removeClass('ok-bg');
        raw.xMaxValue.removeClass('ok-bg');
        return null;
      }
      if (jsnums.greaterThanOrEqual(ret.yMinValue, ret.yMaxValue, RUNTIME.NumberErrbacks)) {
        raw.yMinValue.addClass('error-bg');
        raw.yMaxValue.addClass('error-bg');
        raw.yMinValue.removeClass('ok-bg');
        raw.yMaxValue.removeClass('ok-bg');
        return null;
      }
      if (!isTrue(RUNTIME.num_is_integer(ret.numSamples)) ||
          jsnums.lessThanOrEqual(ret.numSamples, 1, RUNTIME.NumberErrbacks)) {
        raw.numSamples.addClass('error-bg');
        raw.numSamples.removeClass('ok-bg');
        return null;
      }

      return ret;
    }

    //////////////////////////////////////////////////////////////////////////////

    // TODO(Ben): support these options?
    function getGridlines(options, globalOptions) {
      const hAxis = options.hAxis ??= {};
      const vAxis = options.vAxis ??= {};

      const showGridlines = isTrue(globalOptions['show-grid-lines']);
      const gridlineColor = getColorOrDefault(globalOptions['gridlineColor'], undefined);

      const minorGridlineColor = getColorOrDefault(globalOptions['minorGridlineColor'], undefined);

      const minorGridlineMinspacing = toFixnum(globalOptions['minorGridlineMinspacing'])

      hAxis.gridlines = {show: showGridlines, color: gridlineColor};
      vAxis.gridlines = {show: showGridlines, color: gridlineColor};

      cases(RUNTIME.ffi.isOption, 'Option', globalOptions['gridlineMinspacing'], {
        none: function () {
          hAxis.gridlines.count = 5;
        },
        some: function (minspacing) {
          hAxis.gridlines.minSpacing = toFixnum(minspacing);
        }
      });


      if (globalOptions['show-minor-grid-lines']) {
        hAxis.minorGridlines = {color: minorGridlineColor, minSpacing: minorGridlineMinspacing};
        vAxis.minorGridlines = {color: minorGridlineColor, minSpacing: minorGridlineMinspacing};
      } else {
        hAxis.minorGridlines = {count: 0};
        vAxis.minorGridlines = {count: 0};
      }
      return options;
    }

    //////////////////////////////////////////////////////////////////////////////

    function pieChart(globalOptions, rawData) {
      /*
        Note: Most of the complexity here is due to supporting the "collapsed" wedge of values,
        which means giving it a gray color, and putting it last in the slice order and the legend order.
        But, there are lots of colors in the color scale, and we don't know how many pie wedges will
        precede the "collapsed" wedge, so we don't know what index it will have,
        so we can't easily just put gray into the color scale.

        The solution is to stagger the dataflow a few steps:
        0. Construct a rawData table, and compute the sum of the value column
        1. Partition the rawData into two tables, largeEnough and collapsed, based on whether their 
           value is less than the collapseThreshold fraction of the computed total sum.
           In the collapsed table, aggregate all rows into one summed row.
        2. Create the color scale and base it only on the largeEnough values.
        3. Concatenate the largeEnough and collapsed tables, and compute a new color field
        4. Create a sortedColor scale whose domain and range come from the concatenated table
        5. Use the sortedColor scale for the ordering of the legend.
       */
      const table = get(rawData, 'tab');
      const title = globalOptions['title'];
      const width = globalOptions['width'];
      const height = globalOptions['height'];
      const background = getColorOrDefault(globalOptions['backgroundColor'], 'transparent');
      const COLLAPSED_ID = -1;
      
      const data = [];
      const signals = [];
      const marks = [];
      const colors_list = get_colors_list(rawData);
      const scales = [
        {
          name: 'color',
          type: 'ordinal',
          domain: { data: 'largeEnough', field: 'id' },
          range: [...colors_list, ...default_colors]
        },
        {
          name: 'sortedColor',
          type: 'ordinal',
          domain: { data: 'table', field: 'id' },
          range: { data: 'table', field: 'color' }
        },
        {
          name: 'legends',
          type: 'ordinal',
          domain: [COLLAPSED_ID, ...Array(table.length).keys()],
          range: ['Other', ...table.map((row) => row[0])]
        }
      ];
      
      const threeD = get(rawData, 'threeD');
      const piehole = toFixnum(get(rawData, 'piehole'));
      const startingAngle = toFixnum(get(rawData, 'startingAngle'));
      const collapseThreshold = toFixnum(get(rawData, 'collapseThreshold'));

      signals.push(
        { name: 'centerX', update: 'width / 2' },
        { name: 'centerY', update: 'height / 2' },
        { name: 'outerRadius', update: 'min(width / 2, height / 2)' },
        { name: 'innerRadius', update: `outerRadius * ${piehole}` },
        { name: 'cornerRadius', update: '0' }, // allows for rounded corners on each wedge
        { name: 'startAngle', update: '0' }, // start and end angle support rendering only
        { name: 'endAngle', update: `${2 * Math.PI}` },   // a wedge of a pie, rather than the whole circle
        { name: 'padAngle', update: '0' }, // supports gaps between wedges
        { name: 'rotation', update: `${startingAngle * Math.PI / 180}` }, // in radians
        { name: 'collapseThreshold', update: `${collapseThreshold}` },
        {
          name: 'hoveredId',
          value: 'null',
          on: [
            {
              events: [
                { markname: 'legend-labels', type: 'mouseover' },
                { markname: 'legend-symbols', type: 'mouseover' }
              ],
              force: true,
              update: 'datum.value'
            },
            {
              events: [
                { markname: 'legend-labels', type: 'mouseout' },
                { markname: 'legend-symbols', type: 'mouseout' }
              ],
              force: true,
              update: 'null'
            },
          ]
        }
      );

      const dataTable = {
        name: 'rawTable',
        values: table.map((row, i) => ({
          id: i,
          label: row[0],
          value: toFixnum(row[1]),
          offset: toFixnum(row[2]),
          // TODO: image would be from row[3], if we could support it
        })),
        transform: [
          { type: 'joinaggregate', ops: ['sum'], fields: ['value'], as: ['total'] }
        ]
      };
      data.push(dataTable);
      const largeEnough = {
        name: 'largeEnough',
        source: 'rawTable',
        transform: [
          { type: 'filter', expr: 'datum.value >= (collapseThreshold * datum.total)' }
        ]
      }
      data.push(largeEnough);
      const collapsed = {
        name: 'collapsed',
        source: 'rawTable',
        transform: [
          { type: 'filter', expr: 'datum.value < (collapseThreshold * datum.total)' },
          { type: 'aggregate', ops: ['sum', 'min'], fields: ['value', 'total'], as: ['value', 'total'] },
          { type: 'formula', as: 'label', expr: '"Other"' },
          { type: 'formula', as: 'id', expr: '-1' },
          { type: 'formula', as: 'offset', expr: '0' }
        ]
      };
      data.push(collapsed);
      const filtered = {
        name: 'table',
        source: ['largeEnough', 'collapsed'],
        transform: [
          {
            type: 'pie',
            field: 'value',
            startAngle: { signal: 'startAngle' },
            endAngle: { signal: 'endAngle' },
          },
          {
            type: 'formula',
            as: 'color',
            expr: `datum.id == ${COLLAPSED_ID} ? 'gray' : scale('color', datum.id)`
          },
          { type: 'formula', as: 'startAngle', expr: 'datum.startAngle + rotation' },
          { type: 'formula', as: 'endAngle', expr: 'datum.endAngle + rotation' },
          {
            type: 'formula',
            as: 'midAngle',
            expr: '(datum.startAngle + datum.endAngle) / 2'
          },
          // NOTE: angle 0 points *upward*, not *rightward*, so sin and cos
          // are out of phase and therefore swapped from what might be expected.
          { type: 'formula', as: 'offsetX', expr: 'datum.offset * sin(datum.midAngle)' },
          { type: 'formula', as: 'offsetY', expr: 'datum.offset * cos(datum.midAngle)' },
          { type: 'formula', as: 'textX', expr: '(datum.offset + 0.9 * outerRadius) * sin(datum.midAngle)' },
          { type: 'formula', as: 'textY', expr: '(datum.offset + 0.9 * outerRadius) * cos(datum.midAngle)' },
        ]
      };
      data.push(filtered);

      const tooltip = {
        signal: '{ title: datum.label, Value: datum.value + " (" + format(datum.value / datum.total, ".2%") + ")" }'
      };
      marks.push(
        {
          type: 'arc',
          name: 'arcs',
          from: { data: 'table' },
          encode: {
            enter: {
              fill: { field: 'color' },
              strokeWidth: { value: 2 },
              stroke: [
                { test: 'contrast("white", datum.color) > contrast("black", datum.color)', value: 'white' },
                { value: 'black' }
              ]
            },
            update: {
              id: { signal: 'datum.id' },
              x: { signal: 'centerX + (datum.offsetX * outerRadius)' },
              y: { signal: 'centerY - (datum.offsetY * outerRadius)' },
              startAngle: { field: 'startAngle' },
              endAngle: { field: 'endAngle' },
              padAngle: { signal: 'padAngle' },
              innerRadius: { signal: 'innerRadius' },
              outerRadius: { signal: 'outerRadius' },
              cornerRadius: { signal: 'cornerRadius' },
              strokeOpacity: { signal: 'hoveredId == datum.id ? 1 : 0' },
              tooltip,
            },
            hover: {
              strokeOpacity: { value: 1 },
            }
          }
        },
        {
          type: 'text',
          from: { data: 'table' },
          encode: {
            update: {
              fill: [
                {
                  test: 'contrast("white", datum.color) > contrast("black", datum.color)',
                  value: 'white'
                },
                { value: 'black' }
              ],
              text: { signal: 'format(datum.value / datum.total, ".2%")' },
              xc: { signal: 'centerX + datum.textX' },
              yc: { signal: 'centerY - datum.textY' },
              align: { value: 'center' },
              baseline: { value: 'middle' }
            }
          }
        });

      const legends = [
        {
          type: 'symbol',
          fill: 'sortedColor',
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
        autosize: 'fit',
        background,
        data,
        signals,
        scales,
        marks,
        legends,
        onExit: defaultImageReturn,
      };
    }

    function getOrDefault(opt, defaultVal) {
      return cases(RUNTIME.ffi.isOption, 'Option', opt, {
        none: () => defaultVal,
        some: (v) => v
      });
    }
    
    function getNumOrDefault(optNum, defaultNum) {
      return cases(RUNTIME.ffi.isOption, 'Option', optNum, {
        none: () => defaultNum,
        some: toFixnum
      });
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
      return getColorOrDefault(get(rawData, 'color'), '');
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
      return getColorOrDefault(get(rawData, 'pointer-color'), 'black');
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
      return getColorOrDefault(get(rawData, 'default-interval-color'), 'black');
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
          const annotation = getOrDefault(annotationOpt, undefined);
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
        type: 'rect',
        name: 'bars',
        from: { data: marksSource },
        encode: {
          enter: {
            [axesConfig.primary.dir]: { scale: primaryScale, field: heightField },
            [axesConfig.primary.range]: { scale: primaryScale, band: 1, offset: -1 },
            [axesConfig.secondary.dir]: { scale: 'secondary', field: 'value0' },
            [axesConfig.secondary.dir + '2']: { scale: 'secondary', field: 'value1' },
            fill: [
              { test: 'isValid(datum.image)', value: 'transparent' },
              { test: 'isValid(datum.color)', field: 'color' },
              { scale: 'color', field: 'series' }
            ],
            tooltip,
            strokeWidth: { value: 2 },
            stroke: [
              { test: 'contrast("white", scale("color", datum.series)) > contrast("black", scale("color", datum.series))', value: 'white' },
              { value: 'black' }
            ]
          },
          update: {
            strokeOpacity: { signal: 'hoveredSeries == datum.series ? 1 : 0' }
          },
          hover: {
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
        type: 'image',
        from: { data: 'images' },
        encode: {
          enter: {
            [axesConfig.primary.dir]: { scale: primaryScale, field: 'label' },
            [axesConfig.secondary.dir]: {
              signal: 'max(scale("secondary", datum.value0), scale("secondary", datum.value1))'
            },
            [axesConfig.primary.range]: { scale: primaryScale, band: 1, offset: -1 },
            [axesConfig.secondary.range]: { signal: 'abs(scale("secondary", datum.value1) - scale("secondary", datum.value0))' },
            image: { field: 'image' },
            stroke: { value: '#666666' },
            strokeWidth: { value: 10 },
            strokeOpacity: { value: 1 },
            aspect: { value: false },
            [axesConfig.images.anchorProp]: { value: axesConfig.images.anchor },
            tooltip
          }
        }
      });
    }

    function addPointers(globalOptions, rawData, primaryScale, data, marks) {
      const horizontal = isTrue(get(rawData, 'horizontal'));
      const axesConfig = dimensions[horizontal ? 'horizontal' : 'vertical']
      data.push({
        name: 'pointers',
        values: [...get_pointers_list(rawData)],
        transform: [{
          type: 'filter',
          expr: 'inrange(datum.value, domain("secondary"))'
        }]
      })
      const pointer_color = get_pointer_color(rawData);
      marks.push(
        {
          type: 'rule',
          from: { data: 'pointers' },
          encode: {
            enter: {
              [axesConfig.secondary.dir]: { scale: 'secondary', field: 'value' },
              [axesConfig.secondary.dir + '2']: { scale: 'secondary', field: 'value' },
              [axesConfig.primary.dir]: { signal: `range('${primaryScale}')[0]` },
              [axesConfig.primary.dir + '2']: { signal: `range('${primaryScale}')[1]` },
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
      const textOffset = (horizontal
                          ? { signal: 'bandwidth("primary") * (1 + (datum.series % 2)) / 3' }
                          : { scale: primaryScale, band: 0.5 });
      marks.push({
        type: 'text',
        from: { data: 'annotations' },
        encode: {
          enter: {
            [axesConfig.secondary.dir]: {
              // NOTE(Ben): Scaling *after* choosing the greater *value-space* endpoint,
              // to choose rightmost and topmost
              signal: 'max(datum.value0, datum.value1)',
              scale: 'secondary',
              offset: axesConfig.annotations.offset
            },
            [axesConfig.primary.dir]: { scale: primaryScale, field: primaryField, offset: textOffset },
            fill: [
              // NOTE(Ben): fillColor is computed by the transform above
              { test: 'contrast("white", scale("color", datum.series)) > contrast("black", scale("color", datum.series))',
                value: 'white' },
              { value: 'black' }
            ],
            align: { value: axesConfig.annotations.align },
            baseline: { value: axesConfig.annotations.baseline },
            text: { field: 'annotation' }
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
      const fromBottom = { scale: 'secondary', field: 'value0', offset: { scale: 'secondary', value: 0, mult: -1 } }
      marks.push(
        {
          type: 'rule',
          from: { data: 'intervals' }, 
          encode: {
            enter: {
              interactive: false,
              [axesConfig.secondary.dir]: { scale: 'secondary', field: 'intervalExtent[0]', offset: fromBottom },
              [axesConfig.secondary.dir + '2']: { scale: 'secondary', field: 'intervalExtent[1]', offset: fromBottom },
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
              [axesConfig.secondary.dir]: { scale: 'secondary', field: 'intervalTick', offset: fromBottom },
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
      const title = globalOptions['title'];
      const width = globalOptions['width'];
      const height = globalOptions['height'];
      const background = getColorOrDefault(globalOptions['backgroundColor'], 'transparent');
      const axesConfig = dimensions[horizontal ? 'horizontal' : 'vertical']
      const axis = get_axis(rawData);

      const data = [];
      

      const dataTable = {
        name: 'table',
        values: [],
        transform: [
          {
            type: 'formula',
            as: 'color',
            expr: 'isString(datum.color) ? datum.color : scale("color", datum.series)'
          },
          { type: 'formula', as: 'value', expr: 'datum.rawValue' },
          {
            type: 'stack',
            groupby: ['label'],
            sort: { field: 'series' },
            field: 'value',
            as: ['value0', 'value1'],
          }
        ]
      };
      data.push(dataTable);
      const signals = [
        { name: 'hoveredSeries', update: 'null' }
      ];
      const scales = [
        {
          name: 'primary',
          type: 'band',
          range: axesConfig.primary.range,
          domain: { data: 'table', field: 'label' }
        },
        {
          name: 'secondary',
          type: 'linear',
          range: axesConfig.secondary.range,
          nice: true, zero: true,
          domain: axis ? { signal: 'extent(domain("secondaryLabels"))' } : { data: 'table', field: 'value' }
        },
        {
          name: 'color',
          type: 'ordinal',
          domain: Array.from(Array(default_colors.length).keys()),
          range: default_colors
        }
      ];
      if (axis) {
        scales.push({
          name: 'secondaryLabels',
          type: 'ordinal',
          domain: axis.domainRaw,
          range: axis.labels
        });
      }

      // These labels are *specifically directional*, not *logical* -- if a graph is flipped
      // from horizontal to vertical, the labels don't also get flipped.
      const axisLabels = {
        x: globalOptions['x-axis'],
        y: globalOptions['y-axis']
      };
      const axes = [
        { orient: axesConfig.primary.axes, scale: 'primary', zindex: 1, title: axisLabels[axesConfig.primary.dir] },
        { orient: axesConfig.secondary.axes, scale: 'secondary', zindex: 1,
          grid: false, title: axisLabels[axesConfig.secondary.dir] },
        // redraw the axis just for its gridlines, but beneath everything else in z-order
        { orient: axesConfig.secondary.axes, scale: 'secondary', zindex: 0,
          grid: true, ticks: false, labels: false }
      ];

      if (axis) {
        axes[1].values = axis.domainRaw;
        axes[1].encode = {
          labels: { update: { text: { signal: 'scale("secondaryLabels", datum.value)' } } }
        };
      }
      const marks = [];
      const tooltips = [
        {
          test: 'isArray(datum.intervals) && datum.intervals.length > 0',
          signal: '{ title: datum.label, Values: datum.value, Intervals: datum.intervals }'
        },
        {
          signal: '{ title: datum.label, Values: datum.value }'
        }
      ];
      constructDataTable(globalOptions, rawData, 'table', 'label', 'primary', tooltips, dataTable, marks);

      addImages(globalOptions, rawData, 'primary', tooltips, data, marks);

      addPointers(globalOptions, rawData, 'primary', data, marks);

      addAnnotations(globalOptions, rawData, 'table', 'label', 'primary', data, marks);

      addIntervals(globalOptions, rawData, 'table', 'label', 'primary', data, marks);
      
      return {
        "$schema": "https://vega.github.io/schema/vega/v6.json",
        description: title,
        title: title ? { text: title } : '',
        width,
        height,
        padding: 0,
        autosize: 'fit',
        background,
        data,
        signals,
        scales,
        axes,
        marks,
        onExit: defaultImageReturn,
      };
    }


    function multiBarChart(globalOptions, rawData) {
      // Variables and constants
      const horizontal = isTrue(get(rawData, 'horizontal'));
      const axisloc = horizontal ? 'hAxes' : 'vAxes';
      const title = globalOptions['title'];
      const width = globalOptions['width'];
      const height = globalOptions['height'];
      const background = getColorOrDefault(globalOptions['backgroundColor'], 'transparent');
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
            type: 'formula',
            as: 'desc',
            expr: 'isString(datum.annotation) ? datum.annotation : scale("legends", datum.series)'
          }
        ]
      };
      data.push(dataTable);
      if (isStacked) {
        if (stackType !== 'absolute') {
          dataTable.transform.push(
            {
              type: 'joinaggregate',
              groupby: ['label'],
              ops: ['sum'],
              fields: ['rawValue'],
              as: ['totalValue']
            },
            {
              type: 'formula',
              as: 'value',
              expr: 'datum.rawValue/datum.totalValue',
            }
          );
        } else {
          dataTable.transform.push({ type: 'formula', as: 'value', expr: 'datum.rawValue' });
        }            
        dataTable.transform.push(
          {
            type: 'stack',
            groupby: ['label'],
            sort: { field: 'series' },
            field: 'value',
            as: ['value0', 'value1'],
          }
        );
      } else {
        dataTable.transform.push(
          { type: 'formula', as: 'value', expr: 'datum.rawValue' },
          // Since there is no stacking happening, each bar goes from 0 to the value
          { type: 'formula', as: 'value0', expr: '0' },
          { type: 'formula', as: 'value1', expr: 'datum.value' },
        );
      }
      const signals = [
        {
          name: 'hoveredSeries',
          value: 'null',
          on: [
            {
              events: [
                { markname: 'legend-labels', type: 'mouseover' },
                { markname: 'legend-symbols', type: 'mouseover' }
              ],
              force: true,
              update: 'datum.value'
            },
            {
              events: [
                { markname: 'legend-labels', type: 'mouseout' },
                { markname: 'legend-symbols', type: 'mouseout' }
              ],
              force: true,
              update: 'null'
            },
          ]
        }
      ];
      const primaryScale = {
        name: 'primary',
        type: 'band',
        range: axesConfig.primary.range,
        domain: { data: 'table', field: 'label' },
        padding: 0.2
      };
      const secondaryScale = {
        name: 'secondary',
        type: 'linear',
        range: axesConfig.secondary.range,
        nice: true, zero: true,
        domain: (axis && isNotFullStacked) ? { signal: 'extent(domain("secondaryLabels"))' } : { data: 'table', field: 'value1' }
      };
      const scales = [
        primaryScale,
        secondaryScale,
        {
          name: 'color',
          type: 'ordinal',
          domain: { data: 'table', field: 'series' },
          range: [...colors_list, ...default_colors]
        },
        {
          name: 'legends',
          type: 'ordinal',
          domain: Array.from(Array(legendsList.length).keys()),
          range: legendsList
        }
      ];
      if (axis) {
        scales.push({
          name: 'secondaryLabels',
          type: 'ordinal',
          domain: axis.domainRaw,
          range: axis.labels
        });
      }
      // These labels are *specifically directional*, not *logical* -- if a graph is flipped
      // from horizontal to vertical, the labels don't also get flipped.
      const axisLabels = {
        x: globalOptions['x-axis'],
        y: globalOptions['y-axis']
      };
      const axes = [
        { orient: axesConfig.primary.axes, scale: 'primary', zindex: 1, title: axisLabels[axesConfig.primary.dir] },
        { orient: axesConfig.secondary.axes, scale: 'secondary', zindex: 1,
          grid: false, ticks: isNotFullStacked, labels: isNotFullStacked },
        // redraw the axis just for its gridlines, but beneath everything else in z-order
        { orient: axesConfig.secondary.axes, scale: 'secondary', zindex: 0,
          grid: true, ticks: !isNotFullStacked, labels: !isNotFullStacked }
      ];
      // set the axis with the ticks to have the title, so they don't overlap
      axes[isNotFullStacked ? 1 : 2].title = axisLabels[axesConfig.secondary.dir];
      
      if (axis) {
        axes[1].values = axis.domainRaw;
        axes[1].encode = {
          labels: { update: { text: { signal: 'scale("secondaryLabels", datum.value)' } } }
        };
      }
      if (stackType === 'percent') {
        axes[1].format = axes[2].format = '.2%';
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
          { name: axesConfig.primary.range, update: 'bandwidth("primary")' }
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
      let tooltipValue = 'datum.Value';
      if (!isStacked) {
        marks.push(groupMark);
      } else {
        if (stackType === 'percent') {
          tooltipValue = `(datum.rawValue + ' (' + format(datum.value, '.2%') + ')')`;
        } else if (stackType === 'relative') {
          tooltipValue = `(datum.rawValue + ' (' + format(datum.value, '.4') + ')')`;
        } else {
          tooltipValue = 'datum.rawValue';
        }
      }


      const primaryScaleName = (isStacked ? 'primary' : 'primaryGrouped');
      const tooltips = [
        {
          test: 'isArray(datum.intervals) && datum.intervals.length > 0',
          signal: `{ title: datum.label, Series: datum.desc, Value: ${tooltipValue}, Intervals: datum.intervals }`
        },
        {
          signal: `{ title: datum.label, Series: datum.desc, Value: ${tooltipValue} }`
        }
      ];
      constructDataTable(globalOptions, rawData,
                         (isStacked ? 'table' : 'facet'),
                         (isStacked ? 'label' : 'series'),
                         primaryScaleName,
                         tooltips,
                         dataTable,
                         (isStacked ? marks : groupMark.marks));

      // NOTE(Ben): Multi-bar charts don't support images per bar, though I suppose we could.
      // addImages(globalOptions, rawData, primaryScaleName, tooltips, data, (isStacked ? marks : groupMark.marks));

      // always use the ungrouped primary scale, so it stretches across the whole chart
      addPointers(globalOptions, rawData, 'primary', data, marks);

      addAnnotations(globalOptions, rawData,
                     (isStacked ? 'table' : 'facet'),
                     (isStacked ? 'label' : 'series'),
                     (isStacked ? 'primary' : 'primaryGrouped'),
                     (isStacked ? data : groupMark.data),
                     (isStacked ? marks : groupMark.marks));

      if (isNotFullStacked) {
        addIntervals(globalOptions, rawData,
                     (isStacked ? 'table' : 'facet'),
                     (isStacked ? 'label' : 'series'),
                     (isStacked ? 'primary' : 'primaryGrouped'),
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
        autosize: 'fit',
        background,
        data,
        signals,
        scales,
        axes,
        marks,
        legends,
        onExit: defaultImageReturn,
      }
    }

    function boxPlot(globalOptions, rawData) {
      const horizontal = isTrue(get(rawData, 'horizontal'));
      const axesConfig = dimensions[horizontal ? 'horizontal' : 'vertical']
      const table = get(rawData, 'tab');
      const showOutliers = isTrue(get(rawData, 'show-outliers'));
      const axisName = horizontal ? 'hAxis' : 'vAxis';
      const color = getColorOrDefault(get(rawData, 'color'), default_colors[0]);

      const title = globalOptions['title'];
      const width = globalOptions['width'];
      const height = globalOptions['height'];
      const background = getColorOrDefault(globalOptions['backgroundColor'], 'transparent');
      const min = getNumOrDefault(globalOptions['min'], undefined);
      const max = getNumOrDefault(globalOptions['max'], undefined);

      const data = [
        {
          name: 'table',
          values: table.map((boxInfo) => ({
            label: get(boxInfo, 'label'),
            maxVal: toFixnum(get(boxInfo, 'max-val')),
            minVal: toFixnum(get(boxInfo, 'min-val')),
            firstQuartile: toFixnum(get(boxInfo, 'first-quartile')),
            median: toFixnum(get(boxInfo, 'median')),
            thirdQuartile: toFixnum(get(boxInfo, 'third-quartile')),
            highWhisker: toFixnum(get(boxInfo, 'high-whisker')),
            lowWhisker: toFixnum(get(boxInfo, 'low-whisker')),
            highOutliers: get(boxInfo, 'high-outliers').map(toFixnum),
            lowOutliers: get(boxInfo, 'low-outliers').map(toFixnum),
          })),
        },
        {
          name: 'highOutliers',
          source: 'table',
          transform: [
            { type: 'filter', expr: `${showOutliers}` },
            { type: 'flatten', fields: ['highOutliers'], as: ['highOutlier'] }
          ]
        },
        {
          name: 'lowOutliers',
          source: 'table',
          transform: [
            { type: 'filter', expr: `${showOutliers}` },
            { type: 'flatten', fields: ['lowOutliers'], as: ['lowOutlier'] }
          ]
        },        
      ]

      const signals = [
        { name: 'minValue',
          update: 'extent(pluck(data("table"), "minVal"))[0]' },
        { name: 'maxValue',
          update: 'extent(pluck(data("table"), "maxVal"))[1]' }
      ];
      const outlierTooltip = `, 'bottom whisker': datum.lowWhisker, 'top whisker': datum.highWhisker`;
      const tooltip = `{
        title: datum.label,
        minimum: datum.minVal,
        maximum: datum.maxVal,
        'first quartile': datum.firstQuartile,
        median: datum.median,
        'third quartile': datum.thirdQuartile
        ${showOutliers ? outlierTooltip : ''}
      }`;
      
      // Abbreviations for orientation-independent property names
      const P = axesConfig.primary.dir;
      const P2 = P + '2';
      const PC = P + 'c';
      const S = axesConfig.secondary.dir;
      const S2 = S + '2';
      const SC = S + 'c';
      const marks = [
        {
          type: 'group',
          name: 'clip',
          clip: true,
          encode: {
            // Use this clipping rectangle to ensure that no marks extend outside the chart area, even
            // if the user specified an overlay-small chart region
            enter: {
              [P]: { signal: 'range("primary")[0]' },
              [P2]: { signal: 'range("primary")[1]' },
              [S]: { signal: 'range("secondary")[0]' },
              [S2]: { signal: 'range("secondary")[1]' },
            }
          },
          marks: [
            {
              type: 'rect',
              from: {  data: 'table' },
              name: 'whiskers',
              encode: {
                enter: {
                  fill: { value: color },
                  [axesConfig.primary.range]: { value: 1 }
                },
                update: {
                  [PC]: { scale: 'primary', field: 'label', offset: { scale: 'primary', band: 0.5 } },
                  [S]: { scale: 'secondary',  field: 'lowWhisker' },
                  [S2]: { scale: 'secondary', field: 'highWhisker' },
                  tooltip: { signal: tooltip }
                },
              }
            },
            {
              type: 'rect',
              from: { data: 'table' },
              name: 'minTicks',
              encode: {
                enter: {
                  fill: { value: color },
                  [axesConfig.secondary.range]: { value: 2 }
                },
                update: {
                  [PC]: { scale: 'primary', field: 'label', offset: { scale: 'primary', band: 0.5 } },
                  [axesConfig.primary.range]: { scale: 'primary', band: 0.25 },
                  [S]: { scale: 'secondary', field: 'lowWhisker' },
                  tooltip: { signal: tooltip }
                }
              }
            },
            {
              type: 'rect',
              from: { data: 'table' },
              name: 'maxTicks',
              encode: {
                enter: {
                  fill: { value: color },
                  [axesConfig.secondary.range]: { value: 2 }
                },
                update: {
                  [PC]: { scale: 'primary', field: 'label', offset: { scale: 'primary', band: 0.5 } },
                  [axesConfig.primary.range]: { scale: 'primary', band: 0.25 },
                  [S]: { scale: 'secondary', field: 'highWhisker' },
                  tooltip: { signal: tooltip }
                }
              }
            },
            {
              type: 'rect',
              from: { data: 'table' },
              name: 'IQRs',
              encode: {
                enter: {
                  fill: { value: color },
                  fillOpacity: { value: 0.5 },
                  stroke: { value: color },
                  strokeWidth: { value: 2 },
                  cornerRadius: { value: 4 }
                },
                update: {
                  [PC]: { scale: 'primary', field: 'label', offset: { scale: 'primary', band: 0.5 } },
                  [axesConfig.primary.range]: { scale: 'primary', band: 0.5 },
                  [S]: { scale: 'secondary', field: 'firstQuartile' },
                  [S2]: { scale: 'secondary', field: 'thirdQuartile' },
                  tooltip: { signal: tooltip }
                }
              }
            },
            {
              type: 'rect',
              from: { data: 'table' },
              name: 'medians',
              encode: {
                enter: {
                  fill: { value: color },
                  [axesConfig.secondary.range]: { value: 2 }
                },
                update: {
                  [PC]: { scale: 'primary', field: 'label', offset: { scale: 'primary', band: 0.5 } },
                  [axesConfig.primary.range]: { scale: 'primary', band: 0.5 },
                  [S]: { scale: 'secondary', field: 'median' },
                  tooltip: { signal: tooltip }
                }
              }
            },
            {
              type: 'symbol',
              from: { data: 'lowOutliers' },
              name: 'lowOutlierMarks',
              encode: {
                enter: {
                  shape: { value: 'M -1 -1 L 1 1 M 1 -1 L -1 1' },
                  fill: { value: color },
                  fillOpacity: { value: 0.25 },
                  stroke: { value: color },
                  strokeWidth: { value: 2 },
                  size: { scale: 'primary', band: 1 }
                },
                update: {
                  [PC]: { scale: 'primary', field: 'label', offset: { scale: 'primary', band: 0.5 } },
                  [SC]: { scale: 'secondary', field: 'lowOutlier' },
                  tooltip: { signal: tooltip }
                }
              },
            },
            {
              type: 'symbol',
              from: { data: 'highOutliers' },
              name: 'highOutlierMarks',
              encode: {
                enter: {
                  shape: { value: 'M -1 -1 L 1 1 M 1 -1 L -1 1' }, // Draws an X shape
                  fill: { value: color },
                  fillOpacity: { value: 0.25 },
                  stroke: { value: color },
                  strokeWidth: { value: 2 },
                  size: { scale: 'primary', band: 1 }
                },
                update: {
                  [PC]: { scale: 'primary', field: 'label', offset: { scale: 'primary', band: 0.5 } },
                  [SC]: { scale: 'secondary', field: 'highOutlier' },
                  tooltip: { signal: tooltip }
                }
              },
            }
          ]
        }
      ];
      const scales = [
        {
          name: 'primary',
          type: 'band',
          range: axesConfig.primary.range,
          domain: { data: 'table', field: 'label' },
          padding: 0.2
        },
        {
          name: 'secondary',
          type: 'linear',
          range: axesConfig.secondary.range,
          nice: true, zero: false,
          domain: [{ signal: 'minValue' },{ signal: 'maxValue' }],
          domainMin: min,
          domainMax: max,
        },
      ];
      // These labels are *specifically directional*, not *logical* -- if a graph is flipped
      // from horizontal to vertical, the labels don't also get flipped.
      const axisLabels = {
        x: globalOptions['x-axis'],
        y: globalOptions['y-axis']
      };
      const axes = [
        { orient: axesConfig.primary.axes, scale: 'primary', zindex: 1, title: axisLabels[axesConfig.primary.dir] },
        { orient: axesConfig.secondary.axes, scale: 'secondary', zindex: 1, grid:
          false, title: axisLabels[axesConfig.secondary.dir] },
        // redraw the axis just for its gridlines, but beneath everything else in z-order
        { orient: axesConfig.secondary.axes, scale: 'secondary', zindex: 0, grid:
          true, ticks: false, labels: false }
      ];
      
      return {
        "$schema": "https://vega.github.io/schema/vega/v6.json",
        description: title,
        title: title ? { text: title } : '',
        width,
        height,
        padding: 0,
        autosize: 'fit',
        background,
        data,
        signals,
        scales,
        axes,
        marks,
        onExit: defaultImageReturn,
      };
    }

    function histogram(globalOptions, rawData) {
      const table = get(rawData, 'tab');
      const binWidth = getNumOrDefault(get(rawData, 'bin-width'), undefined);

      const maxNumBins = getNumOrDefault(get(rawData, 'max-num-bins'), undefined);

      const title = globalOptions['title'];
      const width = globalOptions['width'];
      const height = globalOptions['height'];
      const background = getColorOrDefault(globalOptions['backgroundColor'], 'transparent');

      const data = [
        {
          name: 'rawTable',
          values: table.map((row, i) => ({
            label: row[0],
            value: toFixnum(row[1]),
            image: (row[2] && row[2].val && IMAGE.isImage(row[2].val)) ? imageToCanvas(row[2].val) : undefined,
          })),
          transform: [
            {
              type: 'extent',
              field: 'value',
              signal: 'dataRange'
            },
            {
              type: 'bin',
              field: 'value',
              extent: { signal: 'dataRange' },
              maxbinx: maxNumBins,
              step: binWidth ? { signal: 'binWidth' } : undefined,
              nice: true
            },
            {
              type: 'stack',
              groupby: ['bin0', 'bin1'],
              sort: { field: 'value' },
              offset: 'zero',
              as: ['y0', 'y1']
            },
          ]
        },
        {
          name: 'summaryTable',
          source: 'rawTable',
          transform: [
            // ONLY draw summary boxes if the datum boxes are too small
            { type: 'filter', expr: '!showIndividualBoxes' },
            {
              type: 'aggregate',
              groupby: ['bin0', 'bin1'],
              ops: ['min', 'max', 'count'],
              fields: ['y0', 'y1', 'y0'],
              as: ['y0', 'y1', 'count']
            }
          ]
        },
        {
          name: 'table',
          source: 'rawTable',
          transform: [ { type: 'filter', expr: 'showIndividualBoxes' } ]
        },
        {
          name: 'images',
          source: 'table',
          transform: [ { type: 'filter', expr: 'isValid(datum.image)' } ]
        }
      ];
      const signals = [
        { name: 'boxHeight', update: 'height / abs(domain("countScale")[0] - domain("countScale")[1])' },
        { name: 'showIndividualBoxes', update: 'boxHeight >= 5' },
        { name: 'binWidth', update: `${binWidth ?? 0}` }
      ];

      const rangeFormatStr = '"[" + trim(format(datum.bin0, "5~f")) + ", " + trim(format(datum.bin1, "5~f")) + "]"';
      const perItemTooltip = `{ title: datum.label, Range: ${rangeFormatStr}, Value: datum.value }`;
      const summaryTooltip = `{
        title: ['Too many items to', 'display separately'],
        Range: ${rangeFormatStr},
        Items: datum.count
      }`;
      const color = getColorOrDefault(get(rawData, 'color'), default_colors[0])
      
      const marks = [
        {
          type: 'rect',
          name: 'blocks',
          from: { data: 'table' },
          encode: {
            enter: {
              x: { scale: 'binScale', field: 'bin0', offset: 0.5 },
              x2: { scale: 'binScale', field: 'bin1', offset: -0.5 },
              y: { scale: 'countScale', field: 'y0', offset: -0.5 },
              y2: { scale: 'countScale', field: 'y1', offset: 0.5 },
              fill: [
                { test: 'isValid(datum.image)', value: 'transparent' },
                { value: color }
              ],
              tooltip: { signal: perItemTooltip }
            }
          }
        },
        {
          type: 'image',
          from: { data: 'images' },
          encode: {
            enter: {
              x: { scale: 'binScale', field: 'bin0' },
              x2: { scale: 'binScale', field: 'bin1' },
              y: { scale: 'countScale', field: 'y0' },
              y2: { scale: 'countScale', field: 'y1' },
              image: { field: 'image' },
              aspect: { value: false },
              tooltip: { signal: perItemTooltip }
            }
          }
        },
        {
          type: 'rect',
          name: 'summaryBlocks',
          from: { data: 'summaryTable' },
          encode: {
            enter: {
              x: { scale: 'binScale', field: 'bin0', offset: 0.5 },
              x2: { scale: 'binScale', field: 'bin1', offset: -0.5 },
              y: { scale: 'countScale', field: 'y0', offset: -0.5 },
              y2: { scale: 'countScale', field: 'y1', offset: 0.5 },
              fill: { value: color },
              tooltip: { signal: summaryTooltip }
            }
          }
        }
      ];

      const scales = [
        {
          name: 'binScale',
          type: 'linear',
          range: 'width',
          domain: { data: 'rawTable', field: 'bin1' }
        },
        {
          name: 'countScale',
          type: 'linear',
          range: 'height',
          domain: { data: 'rawTable', field: 'y1' }
        }
      ];

      const xAxisLabel = globalOptions['x-axis'];
      const yAxisLabel = globalOptions['y-axis'];
      const axes = [
        { orient: 'bottom', scale: 'binScale', zindex: 1, title: xAxisLabel,
          format: '5~r',
          values: (binWidth
                   ? { signal: `sequence(range('binScale')[0], range('binScale')[1] + binWidth, binWidth)` }
                   : undefined) },
        { orient: 'left', scale: 'countScale', grid: true, title: yAxisLabel }
      ];
      

      return {
        "$schema": "https://vega.github.io/schema/vega/v6.json",
        description: title,
        title: title ? { text: title } : '',
        width,
        height,
        padding: 0,
        autosize: 'fit',
        background,
        data,
        signals,
        scales,
        axes,
        marks,
        onExit: defaultImageReturn,
      };
    }

    function dotChart(globalOptions, rawData) {
      const defaultColor = default_colors[0];
      const color = getColorOrDefault(get(rawData, 'color'), defaultColor);
      const legend = get(rawData, 'legend') || '';
      const pointSize = toFixnum(get(rawData, 'point-size'));
      const autosizeImage = isTrue(get(rawData, 'useImageSizes'));

      const points = RUNTIME.ffi.toArray(get(rawData, 'ps'));

      const title = globalOptions['title'];
      const width = globalOptions['width'];
      const height = globalOptions['height'];
      const xAxisLabel = globalOptions['x-axis'];
      const yAxisLabel = globalOptions['y-axis'];
      const background = getColorOrDefault(globalOptions['backgroundColor'], 'transparent');

      const data = [
        {
          name: 'rawTable',
          values: points.map((p) => ({
            label: get(p, 'label'),
            value: toFixnum(get(p, 'value')),
            image: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => imageToCanvas(opaqueImg.val)
            }),
            imageOffsetX: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => opaqueImg.val.getPinholeX() / opaqueImg.val.getWidth()
            }),
            imageOffsetY: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => opaqueImg.val.getPinholeY() / opaqueImg.val.getHeight()
            }),
          })),
          transform: [
            { type: 'extent', field: 'value', signal: 'dataRange' }
          ]
        },
        {
          name: 'binnedRawTable',
          source: 'rawTable',
          transform: [
            { type: 'formula', as: 'binNum', expr: 'floor((datum.value - dataRange[0]) / binSize)' },
            { type: 'formula', as: 'bin0', expr: 'datum.binNum * binSize + dataRange[0]' },
            { type: 'formula', as: 'bin1', expr: 'datum.bin0 + binSize' },
            { type: 'stack', groupby: ['binNum'], offset: 'zero', as: ['y0', 'y1'] }
          ]
        },
        {
          name: `binnedTable`,
          source: `binnedRawTable`,
          transform: [ { type: 'filter', expr: '!isValid(datum.image)' } ]
        },
        {
          name: `binnedImages`,
          source: `binnedRawTable`,
          transform: [ { type: 'filter', expr: 'isValid(datum.image)' } ]
        },
      ];
      const signals = [
        { name: 'dotSize', value: pointSize },
        { name: 'binSize', update: 'invert("binScale", dotSize)' },
        { name: 'actualDotSize', update: 'scale("dotScale", 0) - scale("dotScale", 1)' },
        { name: 'headspace', value: '0.25' },
        { name: 'wrapMaxY', update: 'floor(domain("dotScale")[1] * (1 - headspace))' }
      ];
      const scales = [
        {
          name: 'binScale',
          type: 'linear',
          range: { signal: '[0, width - dotSize / 2]' },
          domain: { data: 'rawTable', field: 'value' }
        },
        {
          name: 'dotScale',
          type: 'linear',
          range: { signal: '[height, dotSize / 2]' },
          domain: { signal: '[0, floor(height / dotSize)]' }
        }
      ];
      const axes = [
        { orient: 'bottom', scale: 'binScale', zindex: 1, title: xAxisLabel, format: '5~r' },
        { orient: 'bottom', scale: 'binScale', zindex: 0, grid: true, ticks: false, labels: false },
        { orient: 'left', scale: 'dotScale', grid: false, ticks: false, labels: false, title: yAxisLabel, zindex: 1 }
      ];
      const marks = [
        // {
        //   type: 'rule',
        //   name: 'binLines',
        //   from: { data: 'binnedTable' },
        //   encode: {
        //     update: {
        //       x: { scale: 'binScale', field: 'bin0' },
        //       y: { signal: 'range("dotScale")[0]' },
        //       y2: { signal: 'range("dotScale")[1]' },
        //       strokeWidth: { value: 1 },
        //       stroke: { value: 'black' }
        //     }
        //   }
        // },
        {
          type: 'symbol',
          name: 'dots',
          from: { data: 'binnedTable' },
          encode: {
            enter: {
              shape: { value: 'circle' },
              fillOpacity: { value: 0.5 },
              size: { signal: 'dotSize * dotSize' },
              fill: { value: color },
              stroke: { value: 'white' },
              strokeWidth: { value: 0.25 },
              tooltip: { signal: '{ title: datum.label, Value: datum.value }' }
            },
            update: {
              xc: { scale: 'binScale', field: 'value' },
              yc: { signal: 'scale("dotScale", 0) - actualDotSize * (0.5 + datum.y0 % wrapMaxY)' }
            }
          }
        },
        {
          type: 'image',
          from: { data: `binnedImages` },
          name: `ImageMarks`,
          encode: {
            enter: {
              width: autosizeImage ? undefined : { value: pointSize },
              height: autosizeImage ? undefined : { value: pointSize },
              image: { field: 'image' },
              tooltip: { signal: '{ title: datum.label, Value: datum.value }' }
            },
            update: {
              xc: { scale: 'binScale', field: 'value' },
              yc: { signal: 'scale("dotScale", 0) - actualDotSize * (0.5 + datum.y0 % wrapMaxY)' },
              align: { value: 'center' },
              baseline: { value: 'middle' }
            },
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
        autosize: 'fit',
        background,
        data,
        signals,
        scales,
        axes,
        marks,
        onExit: defaultImageReturn,
      };
    }

    function categoricalDotChart(globalOptions, rawData) {
      const defaultColor = default_colors[0];
      const color = getColorOrDefault(get(rawData, 'color'), defaultColor);
      const legend = get(rawData, 'legend') || '';

      const points = RUNTIME.ffi.toArray(get(rawData, 'ps'));

      const title = globalOptions['title'];
      const width = globalOptions['width'];
      const height = globalOptions['height'];
      const xAxisLabel = globalOptions['x-axis'];
      const yAxisLabel = globalOptions['y-axis'];
      const background = getColorOrDefault(globalOptions['backgroundColor'], 'transparent');



      const fixedPoints = points.map((p) => ({
        label: get(p, 'label'),
        count: toFixnum(get(p, 'count'))
      }));
      const data = [
        {
          name: 'bars',
          values: fixedPoints
        },
        {
          name: 'dotsData',
          values: fixedPoints.flatMap((p) =>
            Array.from({length: p.count}).map((_, i) => ({ label: p.label, value: i }))
          )
        },
      ];
      const signals = [
        { name: 'dotSize', update: "scale('secondary', 1) - scale('secondary', 0)" },
      ];
      const scales = [
        {
          name: 'primary',
          type: 'band',
          range: 'width',
          domain: { data: 'bars', field: 'label' }
        },
        {
          name: 'secondary',
          type: 'linear',
          range: 'height',
          nice: true, zero: true,
          domain: { data: 'bars', field: 'count' }
        }
      ];
      const axes = [
        { orient: 'bottom', scale: 'primary', zindex: 1, title: xAxisLabel },
        { orient: 'bottom', scale: 'primary', zindex: 0, grid: true, ticks: false, labels: false },
        { orient: 'left', scale: 'secondary', grid: true, ticks: true, labels: true, title: yAxisLabel, zindex: 1 }
      ];
      const marks = [
        {
          type: 'symbol',
          name: 'dots',
          from: { data: 'dotsData' },
          encode: {
            enter: {
              shape: { value: 'circle' },
              fillOpacity: { value: 0.5 },
              fill: { value: color },
              stroke: { value: 'white' },
              strokeWidth: { value: 0.25 },
              tooltip: { signal: '{ title: datum.label, Value: datum.value }' },
              xc: { scale: 'primary', field: 'label', offset: { scale: 'primary', band: 0.5 } },
              yc: { scale: 'secondary', field: 'value', offset: { signal: '0.5 * dotSize' } },
              size: { signal: '0.8 * 0.8 * dotSize * dotSize' },
            }
          }
        },
        {
          type: 'rect',
          name: 'blocks',
          from: { data: 'bars' },
          encode: {
            enter: {
              x: { scale: 'primary', field: 'label', offset: { scale: 'primary', band: 0.25 } },
              x2: { scale: 'primary', field: 'label', offset: { scale: 'primary', band: 0.75 } },
              y: { scale: 'secondary', value: 0 },
              y2: { scale: 'secondary', field: 'count' },
              tooltip: { signal: `{ title: datum.label, Count: datum.count }`},
              stroke: { value: 'gray' },
              strokeWidth: { value: 2 },
              fill: { value: 'transparent' },
            },
            update: {
              opacity: { value: 0 },
            },
            hover: {
              opacity: { value: 1 }
            }
          }
        },
      ];

      return {
        "$schema": "https://vega.github.io/schema/vega/v6.json",
        description: title,
        title: title ? { text: title } : '',
        width,
        height,
        padding: 0,
        autosize: 'fit',
        background,
        data,
        signals,
        scales,
        axes,
        marks,
        onExit: defaultImageReturn,
      };
    }

    function computeDomain(domainValues) {
      const domainMin = Math.min(...domainValues);
      const domainMax = Math.max(...domainValues);
      return [domainMin, domainMax];
    }

    function unionIntervals(domains) {
      if (domains.length === 0) { return []; }
      const ans = [...domains[0]];
      for (const d of domains) {
        ans[0] = Math.min(ans[0], d[0]);
        ans[1] = Math.max(ans[1], d[1]);
      }
      return ans;
    }

    function addCrosshairs(prefix, markNames, signals, marks, color) {
      signals.push(
        { name: `${prefix}crosshair`,
          // NOTE: start it in bounds, so as not to affect the drawing scale or area
          update: `{ x: xMinValue, y: yMinValue }`,
          on: [
            {
              events: markNames.map((name) => ({ markname: `${prefix}${name}`, type: 'click' })),
              force: true,
              update: `${prefix}crosshair === datum ? { x: 0, y: 0 } : datum`
            }
          ]
        },
        { name: `${prefix}crosshairOpacity`, update: `0`,
          on: [
            {
              events: markNames.map((name) => ({ markname: `${prefix}${name}`, type: 'click' })),
              force: true,
              update: `${prefix}crosshair === datum ? 1 : 0`
            },            
          ]
        }
      );
      marks.push(
        {
          type: 'rule',
          name: `${prefix}vertRule`,
          encode: {
            update: {
              x: { scale: `xscale`, signal: `${prefix}crosshair.x` },
              x2: { scale: `xscale`, signal: `${prefix}crosshair.x` },
              y: { signal: `range('yscale')[0]` },
              y2: { signal: `range('yscale')[1]` },
              stroke: { value: color },
              strokeWidth: { value: 1 },
              strokeOpacity: { signal: `${prefix}crosshairOpacity` }
            },
          }
        },
        {
          type: 'rule',
          name: `${prefix}horzRule`,
          encode: {
            update: {
              x: { signal: `range('xscale')[0]` },
              x2: { signal: `range('xscale')[1]` },
              y: { scale: `yscale`, signal: `${prefix}crosshair.y` },
              y2: { scale: `yscale`, signal: `${prefix}crosshair.y` },
              stroke: { value: color },
              strokeWidth: { value: 1 },
              strokeOpacity: { signal: `${prefix}crosshairOpacity` }
            },
          }
        }
      );
    }

    function scatterPlot(globalOptions, rawData, config) {
      const xAxisLabel = globalOptions['x-axis'];
      const yAxisLabel = globalOptions['y-axis'];
      const prefix = config.prefix || ''
      const defaultColor = config.defaultColor || default_colors[0];
      const color = getColorOrDefault(get(rawData, 'color'), defaultColor);
      const legend = get(rawData, 'legend') || config.legend;
      const pointSize = toFixnum(get(rawData, 'point-size'));
      const autosizeImage = isTrue(get(rawData, 'useImageSizes'));
      const pointshapeType = get(rawData, 'pointshapeType');
      const pointshapeSides = toFixnum(get(rawData, 'pointshapeSides'));
      const pointshapeDent = toFixnum(get(rawData, 'pointshapeDent'));
      const pointshapeRotation = toFixnum(get(rawData, 'pointshapeRotation'));
      const trendlineType = getOrDefault(get(rawData, 'trendlineType'), null);
      const trendlineColor = getColorOrDefault(get(rawData, 'trendlineColor'), 'green');
      const trendlineWidth = toFixnum(get(rawData, 'trendlineWidth'));
      const trendlineOpacity = toFixnum(get(rawData, 'trendlineOpacity'));
      const trendlineDegree = toFixnum(get(rawData, 'trendlineDegree'));
      const xMinValue = getNumOrDefault(globalOptions['x-min'], undefined);
      const xMaxValue = getNumOrDefault(globalOptions['x-max'], undefined);
      const yMinValue = getNumOrDefault(globalOptions['y-min'], undefined);
      const yMaxValue = getNumOrDefault(globalOptions['y-max'], undefined);

      const points = RUNTIME.ffi.toArray(get(rawData, 'ps'));

      const data = [
        {
          name: `${prefix}rawTable`,
          values: points.map((p) => ({
            label: get(p, 'label'),
            x: toFixnum(get(p, 'x')),
            y: toFixnum(get(p, 'y')),
            image: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => imageToCanvas(opaqueImg.val)
            }),
            imageWidth: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => opaqueImg.val.getWidth()
            }),
            imageHeight: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => opaqueImg.val.getHeight()
            }),
            imageOffsetX: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => opaqueImg.val.getPinholeX() / opaqueImg.val.getWidth()
            }),
            imageOffsetY: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => opaqueImg.val.getPinholeY() / opaqueImg.val.getHeight()
            }),
          })),
          transform: []
        },
        {
          name: `${prefix}table`,
          source: `${prefix}rawTable`,
          transform: [ { type: 'filter', expr: '!isValid(datum.image)' } ]
        },
        {
          name: `${prefix}images`,
          source: `${prefix}rawTable`,
          transform: [ { type: 'filter', expr: 'isValid(datum.image)' } ]
        },
      ];

      const domain = computeDomain(data[0].values.map((v) => v.x));

      const signals = [
        { name: `${prefix}extentX`, update: `extent(pluck(data("${prefix}rawTable"), "x"))` },
        { name: `${prefix}extentY`, update: `extent(pluck(data("${prefix}rawTable"), "y"))` },
      ];
      const scales = [
        { name: `${prefix}xscale`,
          type: 'linear',
          domain: { signal: `${prefix}extentX` },
          range: 'width',
          nice: true },
        { name: `${prefix}yscale`,
          type: 'linear',
          domain: { signal: `${prefix}extentY` },
          range: 'height',
          nice: true }
      ];
      const marks = [];

      addCrosshairs(prefix, ['ImageMarks', 'ShapeMarks'], signals, marks, color);

      // TODO: Factor this out so other charts can have trendlines too.
      if (trendlineType) {
        data.push(
          {
            name: `${prefix}regression`,
            source: `${prefix}rawTable`,
            transform: [{
              type: 'regression',
              method: trendlineType,
              order: trendlineDegree,
              x: { field: 'x' },
              y: { field: 'y' },
              as: ['u', 'v']
            }]
          },
          {
            name: `${prefix}regressionParams`,
            source: `${prefix}rawTable`,
            transform: [{
              type: 'regression',
              method: trendlineType,
              order: trendlineDegree,
              params: true,
              x: { field: 'x' },
              y: { field: 'y' },
            }]
          },
        )
        let tooltipTitle;
        const formatNum = (exp) => `trim(format(${exp}, '4~f'))`;
        const regressionParam = (idx) => `data('${prefix}regressionParams')[0].coef[${idx}]`;
        if (trendlineType === 'linear') {
          tooltipTitle = `${formatNum(regressionParam(0))} + ' * X + ' + ${formatNum(regressionParam(1))}`;
        } else if (trendlineType === 'exp') {
          tooltipTitle = `${formatNum(regressionParam(0))} + '* e ^ (' + ${formatNum(regressionParam(1))} + ' * X)'`;
        } else if (trendlineType === 'poly') {
          const params = Array.from(Array(trendlineDegree + 1)).map((_, i) => formatNum(regressionParam(i)));
          const xPows = params.map((_, i) => {
            if (i == 0) return '';
            if (i == 1) return 'X';
            return `X^${i}`
          });
          tooltipTitle = '';
          for (let i = trendlineDegree; i >= 0; i--) {
            if (i < trendlineDegree) { tooltipTitle += " + ' + ' + "; }
            if (i == 0) { tooltipTitle += params[i]; }
            else { tooltipTitle += `${params[i]} + ' * ${xPows[i]}'`; }
          }
        }
        tooltipTitle = `replace(${tooltipTitle}, /\\\\+ -/, '- ')`;
        marks.push(
          {
            type: 'line',
            from: { data: `${prefix}regression` },
            name: `${prefix}regressionLine`,
            encode: {
              enter: {
                stroke: { value: trendlineColor },
                strokeWidth: { value: trendlineWidth },
                opacity: { value: trendlineOpacity },
                tooltip: { signal: `{ title: "Trend for ${legend}", Trend: ${tooltipTitle}, x: ${formatNum('datum.u')}, y: ${formatNum('datum.v')} }` }
              },
              update: {
                x: { scale: `xscale`, field: 'u' },
                y: { scale: `yscale`, field: 'v' },
              }
            }
          },
          {
            type: 'symbol',
            from: { data: `${prefix}regression` },
            name: `${prefix}regressionSymbols`,
            encode: {
              enter: {
                tooltip: { signal: `{ title: "Trend for ${legend}", Trend: ${tooltipTitle}, x: ${formatNum('datum.u'
)}, y: ${formatNum('datum.v')} }` }
              },
              update: {
                x: { scale: `xscale`, field: 'u' },
                y: { scale: `yscale`, field: 'v' },
                stroke: { value: trendlineColor },
                strokeWidth: { value: trendlineWidth },
                size: { value: Math.max(40, trendlineWidth * trendlineWidth) },
                opacity: { value: 0 },
              },
              hover: {
                opacity: { value: trendlineOpacity },
              }
            }
          }
        );
      }
      const tooltips = [
        { test: 'datum.label != ""',
          signal: `{ title: "${legend}", Label: datum.label, x: datum.x, y: datum.y }` },
        { signal: `{ title: "${legend}", x: datum.x, y: datum.y }` },
      ];
      const imageScaleFactorX = autosizeImage ? '-datum.imageWidth' : -pointSize;
      const imageScaleFactorY = autosizeImage ? '-datum.imageHeight' : -pointSize;
      marks.push({
        type: 'image',
        from: { data: `${prefix}images` },
        name: `${prefix}ImageMarks`,
        encode: {
          enter: {
            width: autosizeImage ? undefined : { value: pointSize },
            height: autosizeImage ? undefined : { value: pointSize },
            image: { field: 'image' },
            tooltip: tooltips
          },
          update: {
            x: { scale: `xscale`, field: 'x', offset: { signal: `${imageScaleFactorX} * datum.imageOffsetX` } },
            y: { scale: `yscale`, field: 'y', offset: { signal: `${imageScaleFactorY} * datum.imageOffsetY` } },
            stroke: { signal: `hoveredLegend === '${prefix}' ? 'white' : '${color}'` },
            strokeWidth: { signal: `(hoveredLegend === '${prefix}' ? 1 : 0)` },
            zindex: { signal: `hoveredLegend === '${prefix}' ? 1 : null` }
          },
          hover: {
            zindex: { value: 1 }
          }
        }
      });
      if (pointshapeType === 'circle') {
        marks.push({
          type: 'symbol',
          from: { data: `${prefix}table` },
          name: `${prefix}ShapeMarks`,
          encode: {
            enter: {
              shape: { value: 'circle' },
              size: { value: pointSize * pointSize },
              fill: { value: color },
              tooltip: tooltips
            },
            update: {
              x: { scale: `xscale`, field: 'x' },
              y: { scale: `yscale`, field: 'y' },
              fill: { value: color },
              stroke: { signal: `hoveredLegend === '${prefix}' ? 'white' : '${color}'` },
              zindex: { signal: `hoveredLegend === '${prefix}' ? 1 : null` }
            },
            hover: {
              stroke: { value: color },
              zindex: { value: 1 }
            }
          }
        });
      } else if (pointshapeType === 'polygon') {
        const oneDegreeAsRadian = Math.PI / 180;
        const angleBetweenPoints = (2 * Math.PI) / (2 * pointshapeSides);  // each side is paired with a dent
        // According to https://vega.github.io/vega/docs/marks/symbol/,
        // we don't have a predefined star symbol, so we'll use an SVG path string
        // with intended bounding box of [-1,1]x[-1,1]
        // A dent of 1 will produce a regular polygon with straight lines, while
        // a dent of 0 will produce a spiky star with zero-width arms.
        // Starting with the first two points of a regular polygon of unit radius,
        // compute the midpoint between them, and use its radius to provide an absolute
        // scale for what dent==0 means.
        const firstMidpointX = (Math.cos(2 * angleBetweenPoints) + 1) / 2;
        const firstMidpointY = Math.sin(2 * angleBetweenPoints) / 2;
        const firstMidpointRad = Math.sqrt(firstMidpointX * firstMidpointX + firstMidpointY * firstMidpointY);
        const dentRad = pointshapeDent * firstMidpointRad;
        
        const svgPathComponents = [];
        svgPathComponents.push('M');
        // NOTE: we want 0degrees to point up, so sin and cos are swapped, and y is negated
        for (let i = 0; i < pointshapeSides; i++) {
          const rads = 2 * i * angleBetweenPoints;
          if (i !== 0) { svgPathComponents.push('L'); }
          svgPathComponents.push(Math.sin(rads));
          svgPathComponents.push(-Math.cos(rads));
          svgPathComponents.push('L')
          svgPathComponents.push(dentRad * Math.sin(rads + angleBetweenPoints));
          svgPathComponents.push(-dentRad * Math.cos(rads + angleBetweenPoints));
        }
        svgPathComponents.push('Z');
        const svgPath = svgPathComponents.join(' ');
        marks.push({
          type: 'symbol',
          from: { data: `${prefix}table` },
          name: `${prefix}ShapeMarks`,
          encode: {
            enter: {
              shape: { value: svgPath },
              size: { value: pointSize * pointSize },
              strokeWidth: { value: 1 },
              tooltip: tooltips
            },
            update: {
              x: { scale: `xscale`, field: 'x' },
              y: { scale: `yscale`, field: 'y' },
              fill: { value: color },
              stroke: { value: color },
            }
          }
        });
      }
        
      return {
        prefix,
        color,
        legend,
        data,
        signals,
        scales,
        marks,
        domain
      };
    }

    function linePlot(globalOptions, rawData, config) {
      const prefix = config.prefix || ''
      const defaultColor = config.defaultColor || default_colors[0];
      const lineWidth = toFixnum(get(rawData, 'lineWidth'));
      const color = getColorOrDefault(get(rawData, 'color'), defaultColor);
      const asScatterPlot = scatterPlot(globalOptions, rawData, config);
      const marks = asScatterPlot.marks;
      const markShapeMarks = marks.find((m) => m.name === `${prefix}ShapeMarks`);
      if (markShapeMarks) {
        markShapeMarks.encode.enter.size.value = Math.max(40, markShapeMarks.encode.enter.size.value);
        markShapeMarks.encode.hover = markShapeMarks.encode.update;
        markShapeMarks.encode.update = {
          fill: { signal: `${prefix}crosshair === datum ? '${color}' : 'transparent'` },
          stroke: { signal: `${prefix}crosshair === datum ? '${color}' : 'transparent'` },
        }
      }
      marks.push({
        type: 'line',
        from: { data: `${prefix}rawTable` },
        name: `${prefix}Line`,
        encode: {
          enter: {
            stroke: { value: color },
            tooltip: marks[marks.length - 1].encode.enter.tooltips
          },
          update: {
            x: { scale: `xscale`, field: 'x' },
            y: { scale: `yscale`, field: 'y' },
            strokeWidth: { signal: `(hoveredLegend === '${prefix}' ? 2 : 1) * ${lineWidth}` },
            zindex: { signal: `hoveredLegend === '${prefix}' ? 1 : null` }
          },
          hover: {
            strokeWidth: { value: 2 * lineWidth },
            zindex: { value: 1 }
          }
        }
      });
      asScatterPlot.prefix = prefix;
      return asScatterPlot;
    }

    function intervalPlot(globalOptions, rawData, config) {
      const xAxisLabel = globalOptions['x-axis'];
      const yAxisLabel = globalOptions['y-axis'];
      const legend = get(rawData, 'legend') || config.legend;
      const prefix = config.prefix || ''
      const defaultColor = config.defaultColor || default_colors[0];
      const dataColor = getColorOrDefault(get(rawData, 'color'), defaultColor);
      const intervalStyle = get(rawData, 'style');
      const intervalStickWidth = toFixnum(get(rawData, 'stick-width'));
      const intervalFillOpacity = ((intervalStyle == 'boxes') ? 0 : 1);
      const intervalColor = getColorOrDefault(get(rawData, 'pointer-color'), 'black')
      const pointSize = toFixnum(get(rawData, 'point-size'));

      const points = RUNTIME.ffi.toArray(get(rawData, 'ps'));
      const data = [
        {
          name: `${prefix}rawTable`,
          values: points.map((p) => ({
            label: get(p, 'label'),
            x: toFixnum(get(p, 'x')),
            y: toFixnum(get(p, 'y')),
            delta: toFixnum(get(p, 'delta')),
            image: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => imageToCanvas(opaqueImg.val)
            }),
            imageOffsetX: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => opaqueImg.val.getPinholeX() / opaqueImg.val.getWidth()
            }),
            imageOffsetY: cases(RUNTIME.ffi.isOption, 'Option', get(p, 'image'), {
              none: () => undefined,
              some: (opaqueImg) => opaqueImg.val.getPinholeY() / opaqueImg.val.getHeight()
            }),
          })),
          transform: [
            { type: 'formula', as: 'yprime', expr: 'datum.y + datum.delta' }
          ]
        },
        {
          name: `${prefix}table`,
          source: `${prefix}rawTable`,
          transform: [ { type: 'filter', expr: '!isValid(datum.image)' } ]
        },
        {
          name: `${prefix}images`,
          source: `${prefix}rawTable`,
          transform: [ { type: 'filter', expr: 'isValid(datum.image)' } ]
        },
        {
          name: `${prefix}yExtents`,
          source: `${prefix}rawTable`,
          transform: [
            { type: 'aggregate',
              fields: ['y', 'y', 'yprime', 'yprime'],
              ops: ['min', 'max', 'min', 'max'],
              as: ['ymin', 'ymax', 'yprimemin', 'yprimemax'] },
            { type: 'formula', as: 'min', expr: 'min(datum.ymin, datum.yprimemin)' },
            { type: 'formula', as: 'max', expr: 'max(datum.ymax, datum.yprimemax)' }
            ]
        }
      ];
      const domain = computeDomain(data[0].values.map((v) => v.x));
      const signals = [
        { name: `${prefix}extentX`, update: `extent(pluck(data("${prefix}rawTable"), "x"))` },
        { name: `${prefix}extentRawY`, update: `extent(pluck(data("${prefix}rawTable"), "y"))` },
        { name: `${prefix}extentPrime`, update: `extent(pluck(data("${prefix}rawTable"), "yprime"))` },
        { name: `${prefix}extentY`, update: `[data("${prefix}yExtents")[0].min, data("${prefix}yExtents")[0].max]` }
      ]

      const scales = [
        { name: `${prefix}xscale`,
          type: 'linear',
          domain: { signal: `${prefix}extentX` },
          range: 'width',
          nice: false },
        { name: `${prefix}yscale`,
          type: 'linear',
          domain: { signal: `${prefix}extentY` },
          range: 'height',
          nice: false }
      ];
      const tooltip = {
        signal: `{ title: datum.label, x: datum.x, y: datum.y, ŷ: datum.yprime, 'y - ŷ': datum.delta }`
      };
      const marks = [
        {
          type: 'image',
          from: { data: `${prefix}images` },
          name: `${prefix}ImageMarks`,
          encode: {
            enter: {
              width: { value: pointSize },
              height: { value: pointSize },
              image: { field: 'image' },
              tooltip
            },
            update: {
              x: { scale: `xscale`, field: 'x', offset: { signal: `${-pointSize} * datum.imageOffsetX` } },
              y: { scale: `yscale`, field: 'y', offset: { signal: `${-pointSize} * datum.imageOffsetY` } },
              stroke: { signal: `hoveredLegend === '${prefix}' ? 'white' : '${dataColor}'` },
              strokeWidth: { signal: `(hoveredLegend === '${prefix}' ? 1 : 0)` },
              zindex: { signal: `hoveredLegend === '${prefix}' ? 1 : null` }
            },
            hover: {
              zindex: { value: 1 }
            }
          }
        },
        {
          type: 'symbol',
          from: { data: `${prefix}table` },
          name: `${prefix}DataMarks`,
          encode: {
            enter: {
              shape: { value: 'circle' },
              size: { value: pointSize * pointSize },
              fill: { value: dataColor },
              tooltip
            },
            update: {
              xc: { scale: `xscale`, field: 'x' },
              yc: { scale: `yscale`, field: 'y' },
              fill: { value: dataColor },
              stroke: { signal: `hoveredLegend === '${prefix}' ? 'white' : '${dataColor}'` },
              zindex: { signal: `hoveredLegend === '${prefix}' ? 1 : null` }
            },
            hover: {
              zindex: { value: 1 }
            }
          }
        },
        {
          type: 'rule',
          name: `${prefix}IntervalBars`,
          from: { data: `${prefix}rawTable` },
          encode: {
            enter: {
              stroke: { value: dataColor }, // NOTE: this mimics the existing behavior, but it seems backwards
              strokeWidth: { value: intervalStickWidth },
            },
            update: {
              x: { scale: `xscale`, field: 'x' },
              y: { scale: `yscale`, field: 'y' },
              y2: { scale: `yscale`, field: 'yprime' },
            }
          }
        },
        {
          type: 'symbol',
          from: { data: `${prefix}table` },
          name: `${prefix}IntervalMarks`,
          encode: {
            enter: {
              shape: { value: 'circle' },
              size: { value: pointSize * pointSize },
              fill: { value: intervalColor },
              tooltip
            },
            update: {
              xc: { scale: `xscale`, field: 'x' },
              yc: { scale: `yscale`, field: 'yprime' },
              stroke: { signal: `hoveredLegend === '${prefix}' ? 'white' : '${intervalColor}'` },
              zindex: { signal: `hoveredLegend === '${prefix}' ? 1 : null` }
            },
            hover: {
              zindex: { value: 1 }
            }
          }
        },
      ];

      addCrosshairs(prefix, ['ImageMarks', 'DataMarks'], signals, marks, dataColor);

      return {
        prefix,
        color: dataColor,
        legend,
        data,
        signals,
        scales,
        marks,
        domain
      };
    }


    // NOTE: Must be run on Pyret stack
    function recomputePoints(func, samplePoints, then) {
      return RUNTIME.safeCall(() => {
        return RUNTIME.raw_array_map(RUNTIME.makeFunction((sample) => {
          return RUNTIME.execThunk(RUNTIME.makeFunction(() => func.app(sample)));
        }), samplePoints);
      }, (funcVals) => {
        const dataValues = [];
        funcVals.forEach((result, idx) => {
          cases(RUNTIME.ffi.isEither, 'Either', result, {
            left: (value) => dataValues.push({
              x: toFixnum(samplePoints[idx]),
              y: toFixnum(value)
            }),
            right: () => {}
          })
        });
        return then(dataValues);
      }, 'function-plot');
    }

    // NOTE: Must be run on Pyret stack
    function functionPlot(globalOptions, rawData, config) {
      const legend = get(rawData, 'legend') || config.legend;
      const prefix = config.prefix || '';
      const defaultColor = config.defaultColor || default_colors[0];
      const pointColor = getColorOrDefault(get(rawData, 'color'), defaultColor);
      const numSamples = toFixnum(globalOptions['num-samples']);
      const func = get(rawData, 'f');
      const domain = config.domain;
      const xMinValue = domain[0];
      const xMaxValue = domain[1];

      const fraction = (xMaxValue - xMinValue) / (numSamples - 1);

      const data = [ { name: `${prefix}table` } ];

      const signals = [
        { name: `${prefix}extentY`, update: `extent(pluck(data("${prefix}table"), "y"))` }
      ];
      const scales = [
        // NOTE: we don't bother defining an xscale, because functions obtain their domain
        // from the surrounding chart context
        {
          name: `${prefix}yscale`,
          type: 'linear',
          domain: { signal: `${prefix}extentY` },
          range: 'height',
          nice: true
        }
      ];
      const tooltip = {
        signal: `{ title: "${legend}", x: datum.x, y : datum.y }`
      }
      const pointSize = 6;
      const marks = [
        {
          type: 'symbol',
          from: { data: `${prefix}table` },
          name: `${prefix}Dots`,
          encode: {
            enter: {
              shape: { value: 'circle' },
              size: { value: pointSize * pointSize },
              fill: { value: pointColor },
              tooltip
            },
            update: {
              xc: { scale: `xscale`, field: 'x' },
              yc: { scale: `yscale`, field: 'y' },
              stroke: { signal: `hoveredLegend === '${prefix}' ? 'white' : '${pointColor}'` },
              strokeWidth: { signal: `(hoveredLegend === '${prefix}' ? 1 : 0)` },
              zindex: { signal: `hoveredLegend === '${prefix}' ? 1 : null` }
            },
            hover: {
              zindex: { value: 1 }
            }
          }
        }
      ];

      addCrosshairs(prefix, ['Dots'], signals, marks, pointColor);

      const samplePoints = [...Array(numSamples).keys().map((i) => (xMinValue + (fraction * i)))];

      return recomputePoints(func, samplePoints, (dataValues) => {
        data[0].values = dataValues;
        return {
          prefix,
          color: pointColor,
          legend,
          data,
          signals,
          scales,
          marks,
          recompute: (view) => {
            const { promise, resolve, reject } = Promise.withResolvers();
            const numSamples = globalOptions.numSamples;
            const xMinValue = globalOptions.xMinValue;
            const xMaxValue = globalOptions.xMaxValue;
            const fraction = (xMaxValue - xMinValue) / (numSamples - 1);
            const samplePoints = [...Array(numSamples).keys().map((i) => (xMinValue + (fraction * i)))];
            RUNTIME.runThunk(() => {
              // NOTE(Ben): We can use view.data(`${prefix}rawTable`, ...newData...)
              // to replace the existing data points in the _current_ view, so that
              // we do not have to reconstruct a new vega.View or restart the rendering process.
              // See https://vega.github.io/vega/docs/api/view/#view_data
              return recomputePoints(func, samplePoints, (dataValues) => {
                view.data(data[0].name, dataValues);
              });
            }, (res) => {
              if (RUNTIME.isSuccessResult(res)) {
                view.runAsync().then(resolve);
              } else {
                console.log(`Failed in ${prefix} to recompute points:`, res);
                reject(res);
              }
            });
            return promise;
          }
        };
      });
    }

    function unionScaleSignal(scales) {
      const domains0 = scales.map((s) => `domain('${s.name}')[0]`);
      const domains1 = scales.map((s) => `domain('${s.name}')[1]`);
      return `[min(${domains0.join(',')}), max(${domains1.join(',')})]`
    }

    function composeCharts(globalOptions, domain, allCharts) {
      const charts = [
        ...allCharts.scatterPlots,
        ...allCharts.linePlots,
        ...allCharts.intervalPlots,
        ...allCharts.functionPlots
      ];
      const xMinValue = getNumOrDefault(globalOptions['x-min'], undefined);
      const xMaxValue = getNumOrDefault(globalOptions['x-max'], undefined);
      const yMinValue = getNumOrDefault(globalOptions['y-min'], undefined);
      const yMaxValue = getNumOrDefault(globalOptions['y-max'], undefined);
      const numSamples = toFixnum(globalOptions['num-samples']);
      const xAxisLabel = globalOptions['x-axis'];
      const yAxisLabel = globalOptions['y-axis'];
      const width = toFixnum(globalOptions['width']);
      const height = toFixnum(globalOptions['height']);
      const background = getColorOrDefault(globalOptions['backgroundColor'], 'transparent');
      const title = globalOptions['title'];
      // TODO(Ben): support these options?
      const gridlines = getGridlines({}, globalOptions);
      const scales = charts.flatMap((c) => c.scales || []);
      const signals = charts.flatMap((c) => c.signals || []);
      // NOTE: must compute these before updating the scales array...or else the new scales will
      // become part of the signal definition, which is incorrect!
      signals.push(
        { name: 'xMinValue', value: xMinValue },
        { name: 'xMaxValue', value: xMaxValue },
        { name: 'yMinValue', value: yMinValue },
        { name: 'yMaxValue', value: yMaxValue },
        { name: 'numSamples', value: numSamples },
        { name: 'xscaleSignal', update: unionScaleSignal(scales.filter((s) => s.name.endsWith('xscale'))) },
        { name: 'yscaleSignal', update: unionScaleSignal(scales.filter((s) => s.name.endsWith('yscale'))) }
      );
      scales.push(
        { name: 'xscale', domain: { signal: 'xscaleSignal' }, range: 'width',
          domainMin: { signal: 'xMinValue' }, domainMax: { signal: 'xMaxValue' } },
        { name: 'yscale', domain: { signal: 'yscaleSignal' }, range: 'height',
          domainMin: { signal: 'yMinValue' }, domainMax: { signal: 'yMaxValue' } }
      );

      // NOTE: For the axes, we're going to want to put the bar lines at the zeros
      // of the domains, rather than the edges of the chart
      const axes = [
        { orient: 'bottom', scale: `xscale`, zindex: 0,
          grid: gridlines.hAxis.gridlines.show, gridColor: gridlines.hAxis.gridlines.color,
          domain: false, tickOpacity: 0, labelOpacity: 0 },
        { orient: 'bottom', scale: `xscale`, zindex: 1, title: xAxisLabel,
          domain: false, tickOpacity: 0, labelOpacity: 0 },
        { orient: 'bottom', scale: `xscale`, zindex: 1, 
          // For the horizontal axis, translate it *down from the top* to the 0 intercept, if it's in range
          offset: { scale: `yscale`,
                    signal: `clamp(0, domain('yscale')[0], domain('yscale')[1])`,
                    offset: { signal: 'height', mult: -1 } }
        },
        { orient: 'left', scale: `yscale`, zindex: 0,
          grid: gridlines.vAxis.gridlines.show, gridColor: gridlines.vAxis.gridlines.color,
          domain: false, tickOpacity: 0, labelOpacity: 0 },
        { orient: 'left', scale: `yscale`, zindex: 1, title: yAxisLabel,
          domain: false, tickOpacity: 0, labelOpacity: 0 },
        { orient: 'left', scale: `yscale`, zindex: 1, 
          // For the vertical axis, offset by minus the offset to the 0 intercept, if it's in range
          offset: { scale: `xscale`, mult: -1,
                    signal: `clamp(0, domain('xscale')[0], domain('xscale')[1])` }
        },
      ];

      const marks = [
        {
          type: 'group',
          name: `clip`,
          clip: true,
          encode: {
            enter: {
              x: { signal: `range("xscale")[0]` },
              x2: { signal: `range("xscale")[1]` },
              y: { signal: `range("yscale")[0]` },
              y2: { signal: `range("yscale")[1]` }
            }
          },
          marks: charts.flatMap((c) => c.marks || []),
        }
      ]

      const legends = [];
      if (charts.length > 1) {
        scales.push(
          {
            name: 'legendColor',
            type: 'ordinal',
            domain: charts.map((c) => c.prefix),
            range: charts.map((c) => c.color),
          },
          {
            name: 'legends',
            type: 'ordinal',
            domain: charts.map((c) => c.prefix),
            range: charts.map((c) => c.legend)
          }
        );
        legends.push({
          direction: 'horizontal',
          orient: 'bottom',
          columns: 4,
          type: 'symbol',
          fill: 'legendColor',
          symbolType: 'square',
          encode: {
            labels: {
              name: 'legend-labels',
              interactive: true,
              update: {
                text: { scale: 'legends', field: 'value' },
                cursor: { value: 'pointer' },
              },
              symbols: {
                name: 'legend-symbols',
                interactive: true,
                update: { cursor: { value: 'pointer' } }
              }
            }
          }
        });
        signals.push({
          name: 'hoveredLegend',
          value: 'null',
          on: [
            {
              events: [
                { markname: 'legend-labels', type: 'mouseover' },
                { markname: 'legend-symbols', type: 'mouseover' }
              ],
              force: true,
              update: 'datum.value'
            },
            {
              events: [
                { markname: 'legend-labels', type: 'mouseout' },
                { markname: 'legend-symbols', type: 'mouseout' }
              ],
              force: true,
              update: 'null'
            },
          ]
        });
      } else {
        signals.push({
          name: 'hoveredLegend',
          value: 'null'
        });
      }

      function addControls(view, overlay) {
        const overlayWidth = 200;
        $(view.container()).css('padding-right', `${overlayWidth + 10}px`);
        overlay.css({
          width: `${overlayWidth}px`,
          position: 'absolute',
          right: '0px',
          top: '50%',
          transform: 'translateY(-50%)',
          background: 'var(--ui-dialog-default)',
        });
        const inputSize = 16;
        function makeInputControl(label, value) {
          const control = $('<input/>', {
            'class': 'controller',
            type: 'text',
            placeholder: label,
            width: '100%',
          }).attr('size', inputSize)
                // Guaranteed that value.toFixed(5) contains a decimal point,
                // so the trailing zeroes are guaranteed to be decimals only.
                .val(value.toFixed(5).replace(/\.?0+$/, ''));
          return [
            $('<p/>')
              .append($('<label/>', { 'class': 'controller', text: `${label}: ` }))
              .append(control),
            control
          ];
        }

        const [xMinG, xMinC] = makeInputControl('x-min', view.signal('xMinValue') ?? view.signal('xscaleSignal')[0]);
        const [xMaxG, xMaxC] = makeInputControl('x-max', view.signal('xMaxValue') ?? view.signal('xscaleSignal')[1]);
        const [yMinG, yMinC] = makeInputControl('y-min', view.signal('yMinValue') ?? view.signal('yscaleSignal')[0]);
        const [yMaxG, yMaxC] = makeInputControl('y-max', view.signal('yMaxValue') ?? view.signal('yscaleSignal')[1]);
        const [numSamplesG, numSamplesC] = makeInputControl('#samples', numSamples);

        const redrawC = $('<button/>', {
          'class': 'controller',
          text: 'Redraw',
        }).click(async () => {
          const newWindow = getNewWindow({
            xMinValue: xMinC,
            xMaxValue: xMaxC,
            yMinValue: yMinC,
            yMaxValue: yMaxC,
            numSamples: numSamplesC
          });
          if (newWindow === null) return;
          for (const field in newWindow) {
            globalOptions[field] = toFixnum(newWindow[field]);
            view.signal(field, toFixnum(newWindow[field]));
          }
          for (const c of charts) {
            if (c.recompute) { await c.recompute(view); }
          }
          view.runAsync();
        });
        const controller = $('<div/>');
        overlay.empty();
        overlay.append(controller);
        controller.append(xMinG).append(xMaxG).append(yMinG).append(yMaxG);
        if (allCharts.functionPlots.length > 0) {
          controller.append(numSamplesG);
        }
        controller.append(redrawC);
      }
      
      return {
        "$schema": "https://vega.github.io/schema/vega/v6.json",
        description: title,
        title: title ? { text: title } : '',
        width,
        height,
        padding: 0,
        autosize: 'fit',
        background,
        data: charts.flatMap((c) => c.data || []),
        signals,
        scales,
        axes,
        marks,
        legends,
        config: {
          legend: {
            orient: 'bottom',
            layout: { bottom: { anchor: 'middle' } },
          }
        },
        onExit: defaultImageReturn,
        addControls,
      }
    }

    function plot(globalOptions, rawData) {
      const scatters = get(rawData, 'scatters');
      const lines = get(rawData, 'lines');
      const intervals = get(rawData, 'intervals');
      const functions = get(rawData, 'functions');
      const xMinValue = getNumOrDefault(globalOptions['x-min'], undefined);
      const xMaxValue = getNumOrDefault(globalOptions['x-max'], undefined);
      let i = 0;
      function nextColor() {
        return default_colors[i++ % default_colors.length];
      }
      function nextPlot() { return `Plot ${i}`; }
      function makeScatterPlots() {
        return RUNTIME.raw_array_mapi(
          RUNTIME.makeFunction((s, n) => scatterPlot(
            globalOptions, s,
            { prefix: `scatter${n}`, defaultColor: nextColor(), legend: nextPlot() }
          )), scatters);
      }
      function makeLinePlots() {
        return RUNTIME.raw_array_mapi(
          RUNTIME.makeFunction((l, n) => linePlot(
            globalOptions, l,
            { prefix: `line${n}`, defaultColor: nextColor(), legend: nextPlot() }
          )), lines);
      }
      function makeIntervalPlots() {
        return RUNTIME.raw_array_mapi(
          RUNTIME.makeFunction((i, n) => intervalPlot(
            globalOptions, i,
            { prefix: `interval${n}`, defaultColor: nextColor(), legend: nextPlot() }
          )), intervals);
      }
      function makeFunctionPlots(domain) {
        return RUNTIME.raw_array_mapi(
          RUNTIME.makeFunction((f, n) => functionPlot(
            globalOptions, f,
            { prefix: `function${n}`, defaultColor: nextColor(), legend: nextPlot(), domain }
          )), functions);
      }
      return RUNTIME.safeCall(
        makeScatterPlots,
        (scatterPlots) => RUNTIME.safeCall(
          makeLinePlots,
          (linePlots) => RUNTIME.safeCall(
            makeIntervalPlots,
            (intervalPlots) => {
              const domains = [...scatterPlots, ...linePlots, ...intervalPlots].map((p) => p.domain);
              const unionDomain = unionIntervals(domains);
              const clippedDomain = [xMinValue ?? unionDomain[0], xMaxValue ?? unionDomain[1]];
              if (clippedDomain[0] === undefined && clippedDomain[1] === undefined) {
                clippedDomain[0] = -10;
                clippedDomain[1] = 10;
              } else if (clippedDomain[0] === undefined) {
                clippedDomain[0] = clippedDomain[1] - 10;
              } else if (clippedDomain[1] === undefined) {
                clippedDomain[1] = clippedDomain[0] + 10;
              }
              // Reflect the updated values back into the globalOptions,
              // so that they'll be picked up by the initial computation of function plots
              globalOptions['x-min'] = RUNTIME.ffi.makeSome(clippedDomain[0]);
              globalOptions['x-max'] = RUNTIME.ffi.makeSome(clippedDomain[1]);
              return RUNTIME.safeCall(
                () => makeFunctionPlots(clippedDomain),
                (functionPlots) => composeCharts(globalOptions, clippedDomain, {
                  scatterPlots,
                  linePlots,
                  intervalPlots,
                  functionPlots
                })
              );
            }
          )
        )
      );
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


    function imageDataReturn(imageData, restarter) {
      restarter.resume(
        RUNTIME.makeOpaque(
          IMAGE.makeImageDataImage(imageData),
          IMAGE.imageEquals
        )
      );
    }

    function renderToCanvas(view, width, height) {
      const canvas = canvasLib.createCanvas(width, height);
      // NOTE(Ben): this externalContext *should* be unnecessary, but for some reason,
      // vega-as-bundled-in-a-jarr doesn't seem to notice NodeCanvas correctly
      const externalContext = canvas.getContext('2d');
      view.width(width).height(height).resize()
      return view.runAsync()
        .then(() => view.toCanvas(1, { externalContext }))
        .then(() => externalContext.getImageData(0, 0, width, height));
    }

    function defaultImageReturn(restarter, result) {
      const view = result.view;
      const container = view.container();
      // This indirection is again because of the 4.5px discrepancy noted below
      const svg = container.firstChild;
      const bbox = svg.getBoundingClientRect();
      const width = bbox.width
      const height = bbox.height;
      try {
        return renderToCanvas(view, width, height).then((data) => imageDataReturn(data, restarter))
      } catch (e) {
        return restarter.error(e);
      }
    }

    function renderStaticImage(processed, globalOptions, rawData) {
      return RUNTIME.pauseStack(restarter => {
        try {
          if (canvasLib && canvasLib.Canvas) {
            console.log(JSON.stringify(processed, (k, v) => (v && (IMAGE.isImage(v) || (v instanceof canvasLib.Canvas))) ? v.ariaText : v, 2));
          }
          const width = toFixnum(globalOptions['width']);
          const height = toFixnum(globalOptions['height']);
          const view = new vega.View(vega.parse(processed));
          return renderToCanvas(view, width, height).then((data) => imageDataReturn(data, restarter));
        } catch(e) {
          return restarter.error(e);
        }
      });
    }

    function renderInteractiveChart(processed, globalOptions, rawData) {
      return RUNTIME.pauseStack(restarter => {
        const root = $('<div/>');
        const chart = $('<div/>', { style: 'position: relative; display: inline-block;' });
        root.append(chart);
        const overlay = $('<div/>', {style: 'position: relative'});
        root.append(overlay);
        
        // Unfortunately these need to be mutable, to allow for resizing the chart as the dialog resizes
        let width = toFixnum(globalOptions['width']);
        let height = toFixnum(globalOptions['height']);
        const vegaTooltipHandler = new vegaTooltip.Handler({
          formatTooltip: (value, valueToHtml, maxDepth, baseURL) => {
            if (typeof value === 'object') {
              const { title, ...rest } = value;
              if (title instanceof Array) {
                const titleStr = `<h2>${title.map(valueToHtml).join('<br/>')}</h2>`;
                const restStr = vegaTooltip.formatValue(rest, valueToHtml, maxDepth, baseURL);
                return titleStr + restStr;
              }
            }
            return vegaTooltip.formatValue(value, valueToHtml, maxDepth, baseURL);
          }
        });
        const view = new vega.View(vega.parse(processed), {
          container: chart[0],
          renderer: 'svg',
          hover: true,
          tooltip: vegaTooltipHandler.call
        });
        view.width(width).height(height).resize();

        var tmp = processed;
        tmp.view = view;
        const options = {
          backgroundColor: {fill: 'transparent'},
          title: (globalOptions['title'] || {}).text,
        };

        tmp.options = $.extend({}, options, 'options' in tmp ? tmp.options : {});

        delete tmp.width;
        delete tmp.height;

        // only mutate result when everything is setup
        const result = tmp;
        try {
          view.runAsync()
            .then(() => {
              if (processed.addControls) {
                processed.addControls(view, overlay);
              }
              // return true;
              RUNTIME.getParam('chart-port')({
                root: root[0],
                onExit: () => {
                  // In case the tooltip was currently being shown while the window was being closed
                  $('#vg-tooltip-element').removeClass('visible');
                  onExitRetry(() => result, restarter);
                },
                draw: (ui) => {
                  if (ui && ui.size && ui.originalSize) {
                    // Unfortunately, there seems to be a 4.5px mismatch in heights, if we simply
                    // set the view's size to the container's size.  So instead,
                    // integrate the changes in dialog size over time, and only resize the chart
                    // when the dialog has finished resizing.
                    width += ui.size.width - ui.originalSize.width;
                    height += ui.size.height - ui.originalSize.height;
                    view.width(width).height(height).resize();
                  }
                  // This doubled-up approach of render/resize/re-render seems to produce
                  // better-sized results than a single render does
                  view.runAsync().then(() => view.resize().runAsync())
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
        

    function pyretObjToObj(globalOptions) {
      const ret = {};
      for (const field of RUNTIME.getFields(globalOptions)) {
        ret[field] = get(globalOptions, field);
      }
      return ret;
    }
    // NOTE: f is a function that will be run on the Pyret stack
    // It can choose to return a value directly, or it can use safeCall
    function makeFunction(f) {
      return RUNTIME.makeFunction((rawGlobalOptions, rawData) => {
        // Make a local, mutable copy of the Pyret ChartWindow value, so we can
        // edit the xmin/xmax/ymin/ymax/#samples fields as needed
        const globalOptions = pyretObjToObj(rawGlobalOptions);
        const isInteractive = isTrue(globalOptions['interact']);
        if (isInteractive) {
          if (RUNTIME.hasParam('chart-port')) {
            return RUNTIME.safeCall(() => f(globalOptions, rawData), (chart) => {
              return renderInteractiveChart(chart, globalOptions, rawData);
            }, 'render-interactive-chart');
            return renderInteractiveChart(f(globalOptions, rawData), globalOptions, rawData);
          } else {
            return RUNTIME.ffi.throwMessageException('Cannot display interactive charts headlessly');
          }
        } else {
          return RUNTIME.safeCall(() => f(globalOptions, rawData), (chart) => {
            return renderStaticImage(chart, globalOptions, rawData);
          }, 'render-static-image');
        }
      });
    }

    return RUNTIME.makeModuleReturn(
      {
        'pie-chart': makeFunction(pieChart),
        'bar-chart': makeFunction(barChart),
        'multi-bar-chart': makeFunction(multiBarChart),
        'histogram': makeFunction(histogram),
        'box-plot': makeFunction(boxPlot),
        'dot-chart': makeFunction(dotChart),
        'categorical-dot-chart': makeFunction(categoricalDotChart),
        'plot': makeFunction(plot),
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
