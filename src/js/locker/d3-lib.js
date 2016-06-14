var ALLHEIGHT = 476;
var ALLWIDTH = 601;
var MARGIN = {'top': 30, 'left': 100, 'bottom': 45, 'right': 100};
var NOMARGIN = {'top': 0, 'left': 0, 'bottom': 0, 'right': 0};

define(["js/js-numbers"],
    function (jsnums) {

    return function(d3) {

    function assert(val, msg) {
        if (!val) { throw new Error("Assertion failed: " + (msg || "")); }
    }

    function p(pred, name, runtime) {
        return function(val) {
            runtime.makeCheckType(pred, name)(val);
            return val;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // libNum
    ////////////////////////////////////////////////////////////////////////////

    function scaler(oldX, oldY, newX, newY, toFixnum) {
        /*
         * Produces a scaler function to convert a value in
         * an interval to another value in a new interval
         *
         * @param {jsnums} oldX
         * @param {jsnums} oldY
         * @param {jsnums} newX
         * @param {jsnums} newY
         * @param {boolean} toInt: if true, the result is converted to
         * integer fixnum
         * @return {Function}
         */
        return function (k) {
            var oldDiff = jsnums.subtract(k, oldX);
            var oldRange = jsnums.subtract(oldY, oldX);
            var portion = jsnums.divide(oldDiff, oldRange);
            var newRange = jsnums.subtract(newY, newX);
            var newPortion = jsnums.multiply(portion, newRange);
            var result = jsnums.add(newPortion, newX);
            return toFixnum ? jsnums.toFixnum(result) : result;
        };
    }

    function adjustInRange(k, vmin, vmax) {
        /*
         * Adjust k to be between vmin and vmax if it's not in the range
         *
         * @param {jsnums} k
         * @param {jsnums} vmin
         * @param {jsnums} vmax
         * @return {jsnums}
         */
        if (jsnums.lessThan(k, vmin)) return vmin;
        else if (jsnums.lessThan(vmax, k)) return vmax;
        else return k;
    }

    function max(a, b) {
        /*
         * Find the maximum value
         *
         * @param {jsnums} a
         * @param {jsnums} b
         * @return {jsnums}
         */
        return jsnums.lessThan(a, b) ? b : a;
    }

    function min(a, b) {
        /*
         * Find the minimum value
         *
         * @param {jsnums} a
         * @param {jsnums} b
         * @return {jsnums}
         */
        return jsnums.lessThan(a, b) ? a : b;
    }

    function random(a, b) {
        return Math.floor(Math.random() * (b - a + 1)) + a;
    }

    function format(num, digit) {
        if (num.toString().length > digit) {
            var fixnum = jsnums.toFixnum(num);
            if (fixnum.toString().length > digit) {
                var digitRounded = digit - 1;
                if (fixnum < 0) digitRounded--;
                if (fixnum.toString().indexOf(".") !== -1) digitRounded--;
                var fixnumRounded = d3
                        .format('.' + digitRounded + 'r')(fixnum);
                // d3 always cast the result of format to
                // string and .r formatter could give NaN
                if ((fixnumRounded === "NaN") ||
                    (fixnumRounded.length > digit)) {
                    // use only 3 position because this notation
                    // has xxx.xxxe+.. ~ 9 digits
                    return d3.format('.3e')(fixnum);
                } else {
                    return fixnumRounded;
                }
            } else {
                return fixnum;
            }
        } else {
            return num;
        }
    }

    function between(b, a, c) {
      return (jsnums.lessThanOrEqual(a, b) && jsnums.lessThanOrEqual(b, c)) ||
             (jsnums.lessThanOrEqual(c, b) && jsnums.lessThanOrEqual(b, a));
    }

    function calcPointOnEdge(pin, pout, xMin, xMax, yMin, yMax) {
      // This function is used to calculate the point on edge
      // of a window [xMin, xMax] x [yMin, yMax] between pin and pout
      // pin, pout: {x: ..., y: ...}

      // y = m * x + c // [3]
      // y2 = m * x2 + c
      // y - y2 = m * (x - x2)
      // m = (y - y2) / (x - x2) [1]
      // c = y - m * x [2]
      // x = (y - c) / m // [4]

      if(jsnums.eqv(pin.x, pout.x)) {
        // special mode
        if (jsnums.lessThan(pout.y, yMin)) {
          return {x: pin.x, y: yMin};
        } else {
          return {x: pin.x, y: yMax};
        }
      }

      var m = jsnums.divide(
                jsnums.subtract(pin.y, pout.y),
                jsnums.subtract(pin.x, pout.x)); // [1]
      var c = jsnums.subtract(pin.y, jsnums.multiply(m, pin.x)); // [2]

      function f(x) { return jsnums.add(jsnums.multiply(m, x), c); } // [3]
      function g(y) { return jsnums.divide(jsnums.subtract(y, c), m); } // [4]

      var yCurrent = f(xMin); // if (xMin, yCurrent) is the answer
      if (between(xMin, pin.x, pout.x) &&
          between(yCurrent, yMin, yMax) &&
          between(yCurrent, pin.y, pout.y)) {
        return {x: xMin, y: yCurrent};
      }

      yCurrent = f(xMax); // if (xMax, yCurrent) is the answer
      if (between(xMax, pin.x, pout.x) &&
          between(yCurrent, yMin, yMax) &&
          between(yCurrent, pin.y, pout.y)) {
        return {x: xMax, y: yCurrent};
      }

      var xCurrent = g(yMin); // if (xCurrent, yMin) is the answer
      if (between(yMin, pin.y, pout.y) &&
          between(xCurrent, xMin, xMax) &&
          between(xCurrent, pin.x, pout.x)) {
        return {x: xCurrent, y: yMin};
      }

      var xCurrent = g(yMax); // if (xCurrent, yMax) is the answer
      if (between(yMax, pin.y, pout.y) &&
          between(xCurrent, xMin, xMax) &&
          between(xCurrent, pin.x, pout.x)) {
        return {x: xCurrent, y: yMax};
      }
      throw "algorithm to calculate a point on the edge is buggy";
    }

    var libNum = {
        scaler: scaler,
        adjustInRange: adjustInRange,
        max: max,
        min: min,
        format: format,
        between: between,
        calcPointOnEdge: calcPointOnEdge
    }

    ////////////////////////////////////////////////////////////////////////////
    // libJS
    ////////////////////////////////////////////////////////////////////////////

    function drawImage(opts) {
        /*
         * Writes an image into a canvas taking into
         * account the backing store pixel ratio and
         * the device pixel ratio.
         *
         * http://www.html5rocks.com/en/tutorials/canvas/hidpi/
         *
         * @author Paul Lewis
         * @param {Object} opts The params for drawing an image to the canvas
         */
        if(!opts.canvas) {
            throw("A canvas is required");
        }
        if(!opts.image) {
            throw("Image is required");
        }

        // get the canvas and context
        var canvas = opts.canvas,
            context = canvas.getContext('2d'),
            image = opts.image,

            // now default all the dimension info
            srcx = opts.srcx || 0,
            srcy = opts.srcy || 0,
            srcw = opts.srcw || image.naturalWidth,
            srch = opts.srch || image.naturalHeight,
            desx = opts.desx || srcx,
            desy = opts.desy || srcy,
            desw = opts.desw || srcw,
            desh = opts.desh || srch,
            auto = opts.auto,

            // finally query the various pixel ratios
            devicePixelRatio = window.devicePixelRatio || 1,
            backingStoreRatio = context.webkitBackingStorePixelRatio ||
                context.mozBackingStorePixelRatio ||
                context.msBackingStorePixelRatio ||
                context.oBackingStorePixelRatio ||
                context.backingStorePixelRatio || 1,

            ratio = devicePixelRatio / backingStoreRatio;

        // ensure we have a value set for auto.
        // If auto is set to false then we
        // will simply not upscale the canvas
        // and the default behaviour will be maintained
        if (typeof auto === 'undefined') {
            auto = true;
        }

        // upscale the canvas if the two ratios don't match
        if (auto && devicePixelRatio !== backingStoreRatio) {

            var oldWidth = canvas.width;
            var oldHeight = canvas.height;

            canvas.width = oldWidth * ratio;
            canvas.height = oldHeight * ratio;

            canvas.style.width = oldWidth + 'px';
            canvas.style.height = oldHeight + 'px';

            // now scale the context to counter
            // the fact that we've manually scaled
            // our canvas element
            context.scale(ratio, ratio);

        }

        context.drawImage(image, srcx, srcy, srcw, srch, desx, desy, desw, desh);
        return canvas;
    }

    function getBoundingClientRect(elem) {
        /*
         * Find the bounding box of elem
         *
         * @param {element} elem
         * @return {object}
         */
        var div = d3.select('body').append('div');
        div.node().appendChild(elem.cloneNode(true));
        var bbox = div.node().firstChild.getBoundingClientRect();
        div.remove();
        return bbox;
    }

    function getBBox(svg) {
        /*
         * Find the bounding box of svg elem
         *
         * @param {element} svg
         * @return {object}
         */
        var div = d3.select('body').append('div');
        div.node().appendChild(svg.cloneNode(true));
        var bbox = div.node().firstChild.getBBox();
        div.remove();
        return bbox;
    }

    function htmlspecialchars(text) {
        var div = document.createElement('div');
        var textNode = document.createTextNode(text);
        div.appendChild(textNode);
        return div.innerHTML;
    }

    var libJS = {
        'getBBox': getBBox,
        'getBoundingClientRect': getBoundingClientRect,
        'htmlspecialchars': htmlspecialchars
    };

    ////////////////////////////////////////////////////////////////////////////
    // libData
    ////////////////////////////////////////////////////////////////////////////

    function lastElement(arr) {
        /*
         * Produces the last element of arr
         *
         * @param {array} arr
         * @return {Any}
         */
        return arr[arr.length - 1];
    }

    function flatten(lst) {
        /*
         * Flatten the list
         *
         * @param {array} lst
         * @return {array}
         */
        return [].concat.apply([], lst);
    }

    function fill(n, v) {
        var i, ret = [];
        for (i = 0; i < n; i++) {
            ret.push(v);
        }
        return ret;
    }

    function range(st, ed) {
        var i, ret = [];
        for (i = st; i < ed; i++) {
            ret.push(i);
        }
        return ret;
    }

    function shuffle(o){
        //+ Jonas Raoni Soares Silva
        //@ http://jsfromhell.com/array/shuffle [v1.0]
        for(var j, x, i = o.length; i;
            j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
        return o;
    };

    function FenwickTree(n) {
        /*
         * Fenwick Tree for computing prefix sum
         *
         * @param {fixnum} n: number of elements
         * @return {Object}
         */
        this.arr = fill(n + 1, 0); // use index 1 to n

        this.add = function (ind, val) {
            /*
             * Add `val` to position `ind`
             *
             * @param {fixnum} ind: index
             * @param {fixnum} val: value
             * @return {Object} this
             */
            assert(1 <= ind); // add from 1 to n
            assert(ind <= n);
            while (ind <= n) {
                this.arr[ind] += val;
                ind += (ind & (-ind));
            }
            return this;
        };

        this.sum = function (ind) {
            /*
             * Produces the sum of all values from position 1 to `ind`
             *
             * @param {fixnum} ind: index
             * @return {fixnum}
             */
            assert(0 <= ind); // query from 0 to n
            assert(ind <= n);
            var ret = 0;
            while (ind >= 1) {
                ret += this.arr[ind];
                ind -= (ind & (-ind));
            }
            return ret;
        };

        this.sumInterval = function (l, r) {
            /*
             * Produces the sum of all values between position `l` and `r`
             *
             * @param {fixnum} l
             * @param {fixnum} r
             * @return {fixnum}
             */
            return this.sum(r) - this.sum(l - 1);
        };
    }

    function LogTable(n) {
        /*
         * LogTable to check whether an interval is full
         *
         * @param {fixnum} n
         */
        this.fenwick = new FenwickTree(n);

        this.occupy = function (v) {
            /*
             * Occupied a space
             *
             * @param {fixnum} v
             * @return {Object} this
             */
            v += 1; // use based-1 index
            if (!Number.isNaN(v) && this.fenwick.sumInterval(v, v) === 0) {
                this.fenwick.add(v, 1);
            }
            return this;
        };

        this.isRangedOccupied = function (l, r) {
            /*
             * Answered whether the interval [l, r] is all occupied
             *
             * @param {fixnum} l
             * @param {fixnum} r
             * @return {Boolean}
             */
            l += 1;
            r += 1;
            if (Number.isNaN(l) || Number.isNaN(r)) {
                return false;
            } else {
                if (l > r) {
                    var tmp = l;
                    l = r;
                    r = tmp;
                }
                return this.fenwick.sumInterval(l, r) === (r - l + 1);
            }
        };
    }

    var libData = {
        'lastElement': lastElement,
        'flatten': flatten,
        'fill': fill,
        'range': range,
        'shuffle': shuffle,
        'LogTable': LogTable
    }

    ////////////////////////////////////////////////////////////////////////////
    // libColor
    ////////////////////////////////////////////////////////////////////////////

    function getContrast(r, g, b){
        /*
         * http://24ways.org/2010/calculating-color-contrast/
         */
    	return ((((r*299)+(g*587)+(b*114))/1000) >= 128) ? 'black' : 'white';
    }

    function convertColor(runtime, image) {
        var colorDb = image.colorDb,
            _checkColor = p(image.isColorOrColorString, "Color", runtime);

        function checkColor(val) {
            var aColor = _checkColor(val);
            if (colorDb.get(aColor)) {
                aColor = colorDb.get(aColor);
            }
            return aColor;
        }

        function colorString(aColor) {
            return "rgba(" + image.colorRed(aColor) + "," +
                image.colorGreen(aColor) + ", " +
                image.colorBlue(aColor) + ", " +
                image.colorAlpha(aColor) + ")";
        }

        return function (v) { return colorString(checkColor(v)); };
    }

    function changeColor(r, g, b, d) {
        r += d;
        b += d;
        g += d;

        if (r > 255) r = 255;
        else if (r < 0) r = 0;

        if (b > 255) b = 255;
        else if (b < 0) b = 0;

        if (g > 255) g = 255;
        else if (g < 0) g = 0;

        return 'rgba(' + r + ',' + g + ',' + b + ',255)';
    }

    var libColor = {
        'getContrast': getContrast,
        'convertColor': convertColor,
        'changeColor': changeColor
    }

    ////////////////////////////////////////////////////////////////////////////
    // libCheck
    ////////////////////////////////////////////////////////////////////////////

    function checkListGenerator(type, _checker, runtime) {
        // TODO: this will eventually not work
        var checker = function(x){ return p(_checker, type, runtime)(x); }
        return p(function(val) {
            return runtime.ffi.makeList(runtime.ffi.toArray(val).map(checker));
        }, "List<" + type + ">", runtime);
    }

    var libCheck = {
        'checkListGenerator': checkListGenerator
    }

    ////////////////////////////////////////////////////////////////////////////
    // d3common
    ////////////////////////////////////////////////////////////////////////////

    function getMargin(marginType) {
        if (marginType === "normal"){
            return MARGIN;
        }else if(marginType === "none"){
            return NOMARGIN;
        }else{
            throw "not supported margin"
        }
    }

    function getDimension(margin) {
        return {
            width: ALLWIDTH - margin.left - margin.right,
            height: ALLHEIGHT - margin.top - margin.bottom
        };
    }

    function svgTranslate(x, y) {
        if (y === undefined) {
            return "translate(" + x.toString() + ")";
        }
        return "translate(" + x.toString() + "," + y.toString() + ")";
    }

    function createDiv() {
        /*
         * Creates a blank div
         *
         * @return {d3 selection}
         */
        return d3.select(document.createElement("div"))
            .attr('class', 'maind3');
    }

    function createCanvas(detached, margin, orient) {
        /*
         * Creates a canva
         *
         * @param {fixnum} width: in pixel
         * @param {fixnum} height: in pixel
         * @param {d3 selection} detached
         * @param {string} orient
         * @return {d3 selection}
         */
        var divSvg = detached
                .append('div')
                .attr('class', 'divsvg'),
            canvas = divSvg
                .append("svg")
                .attr("width", ALLWIDTH)
                .attr("height", ALLHEIGHT)
                .append("g")
                .attr('class', 'maing')
                .append('g');

        if (orient === "top-left") {
            canvas.attr("transform", svgTranslate(margin.left, margin.top));
        } else if (orient == "center") {
            canvas.attr("transform",
                svgTranslate(ALLWIDTH / 2, ALLHEIGHT / 2));
        } else {
            throw "orient '" + orient  + "' not implemented";
        }
        return canvas;
    }

    function createSave(detached) {
        /*
         * Create a button to download the image of canvas as PNG file
         *
         * A part of these are adapted from
         * http://techslides.com/save-svg-as-an-image
         */
        detached.append('button')
            .attr('class', 'd3btn')
            .text('Save!')
            .on('click', function () {
                var svg = detached.select("svg")
                        .attr("version", 1.1)
                        .attr("xmlns", "http://www.w3.org/2000/svg"),

                    canvas = detached.append('canvas')
                        .style('display', 'none')
                        .attr('width', svg.attr("width"))
                        .attr('height', svg.attr("height")),

                    svgData = detached.append('div')
                        .style('display', 'none'),

                    html = detached.node().firstChild.innerHTML,

                    imgsrc = 'data:image/svg+xml;base64,' + btoa(html),

                    img = '<img src="' + imgsrc + '">';

                svgData.html(img);

                var image = new Image;
                image.src = imgsrc;
                image.onload = function () {
                    var opts = {
                        canvas: canvas.node(),
                        image: image
                    };
                    var a = document.createElement("a");
                    a.download = "sample.png";
                    a.href = drawImage(opts).toDataURL("image/png");
                    a.click();

                    // the image's size was doubled everytime we click
                    // the button, so remove all data to prevent this
                    // to happen
                    svgData.remove();
                    canvas.remove();
                };
            });
    }

    function callBigBang(runtime, detached) {
        createSave(detached);

        // insert space between buttons
        detached.selectAll('.d3btn').style({
            'margin-right': '20px'
        })

        runtime.getParam("current-animation-port")(detached.node());

        // TODO: below is a hack. Actually Pyret is supposed to take
        // care of it (https://github.com/brownplt/pyret-lang/issues/483)
        // Remove this when the bug is fixed
        var terminate = function () {  d3.selectAll(".maind3").remove(); };

        // simulate dialogclose
        d3.selectAll(".ui-dialog-titlebar-close").on("click", terminate);
        d3.select("body").on("keyup", function(e) {
            if (d3.event.keyCode == 27) { terminate(); } // esc key
        });
    }

    function stylizeTip(detached) {
        /*
         * Add styles for tooltip
         *
         * @param {d3 selection} detached
         */
        detached.selectAll('.d3-tip')
            .style({
                'background': 'rgba(0, 0, 0, 0.8)',
                'line-height': '1.5',
                'font-weight': 'bold',
                'font-size': '8pt',
                'color': '#fff',
                'padding': '10px',
                'border-radius': '2px'
            });
    }

    function d3tipBuilder(d3tipLib) {
      return function(detached) {
        return d3tipLib(d3, detached);
      };
    }

    var d3common = {
        'getMargin': getMargin,
        'getDimension': getDimension,
        'svgTranslate': svgTranslate,
        'createDiv': createDiv,
        'createCanvas': createCanvas,
        'callBigBang': callBigBang,
        'stylizeTip': stylizeTip,
        'd3tipBuilder': d3tipBuilder
    };

    return {
        'libData': libData,
        'libNum': libNum,
        'libJS': libJS,
        'libColor': libColor,
        'libCheck': libCheck,
        'd3common': d3common,
        'assert': assert
    };

    }
});

/*
var testFenwick = new FenwickTree(10);
testFenwick.add(1, 2);
assert(testFenwick.sumInterval(1, 10) === 2);
testFenwick.add(1, 3);
assert(testFenwick.sumInterval(1, 10) === 5);
testFenwick.add(3, 4);
assert(testFenwick.sumInterval(1, 10) === 9);
assert(testFenwick.sumInterval(1, 2) === 5);
assert(testFenwick.sumInterval(2, 3) === 4);

assert(calcPointOnEdge({x: 0, y: 0}, {x: -20, y: -30}, -10, 10, -10, 10), jsnums.divide(-20, 3));
assert(calcPointOnEdge({x: -20, y: -30}, {x: 0, y: 0}, -10, 10, -10, 10), jsnums.divide(-20, 3));
*/
