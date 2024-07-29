// Do not directly use this file. We stopify and webpack this file for each
// type of transformation.
function array_map(obj: any, callback: any, thisArg?: any) {
    var T, A, k;
    if (obj === null) {
      throw new TypeError('this is null or not defined');
    }
    // 1. Let O be the result of calling ToObject passing the |this|
    //    value as the argument.
    var O = Object(obj);
    // 2. Let lenValue be the result of calling the Get internal
    //    method of O with the argument "length".
    // 3. Let len be ToUint32(lenValue).
    var len = O.length >>> 0;
    // 4. If IsCallable(callback) is false, throw a TypeError exception.
    // See: http://es5.github.com/#x9.11
    if (typeof callback !== 'function') {
        debugger;
      throw new TypeError(callback + ' is not a function');
    }
    // 5. If thisArg was supplied, let T be thisArg; else let T be undefined.
    if (arguments.length > 2) {
      T = arguments[2];
    }
    // 6. Let A be a new array created as if by the expression new Array(len)
    //    where Array is the standard built-in constructor with that name and
    //    len is the value of len.
    A = new Array(len);
    // 7. Let k be 0
    k = 0;
    // 8. Repeat, while k < len
    while (k < len) {
      var kValue, mappedValue;
      // a. Let Pk be ToString(k).
      //   This is implicit for LHS operands of the in operator
      // b. Let kPresent be the result of calling the HasProperty internal
      //    method of O with argument Pk.
      //   This step can be combined with c
      // c. If kPresent is true, then
      if (k in O) {
        // i. Let kValue be the result of calling the Get internal
        //    method of O with argument Pk.
        kValue = O[k];
        // ii. Let mappedValue be the result of calling the Call internal
        //     method of callback with T as the this value and argument
        //     list containing kValue, k, and O.
        mappedValue = callback.call(T, kValue, k, O);
        // iii. Call the DefineOwnProperty internal method of A with arguments
        // Pk, Property Descriptor
        // { Value: mappedValue,
        //   Writable: true,
        //   Enumerable: true,
        //   Configurable: true },
        // and false.
        // In browsers that support Object.defineProperty, use the following:
        // Object.defineProperty(A, k, {
        //   value: mappedValue,
        //   writable: true,
        //   enumerable: true,
        //   configurable: true
        // });
        // For best browser support, use the following:
        A[k] = mappedValue;
      }
      // d. Increase k by 1.
      k++;
    }
    // 9. return A
    return A;
  }
  
  function array_filter(obj: any, fun: any/*, thisArg*/) {
    'use strict';
    if (obj === void 0 || obj === null) {
      throw new TypeError();
    }
    var t = Object(obj);
    var len = t.length >>> 0;
    if (typeof fun !== 'function') {
      throw new TypeError();
    }
    var res = [];
    var thisArg = arguments.length >= 2 ? arguments[1] : void 0;
    for (var i = 0; i < len; i++) {
      if (i in t) {
        var val = t[i];
        // NOTE: Technically this should Object.defineProperty at
        //       the next index, as push can be affected by
        //       properties on Object.prototype and Array.prototype.
        //       But that method's new, and collisions should be
        //       rare, so use the more-compatible alternative.
        if (fun.call(thisArg, val, i, t)) {
          res.push(val);
        }
      }
    }
    return res;
  }
  
  function array_sort(o: any, comparator?: any): any {
    "use strict";
  
    function min(a: any, b: any) {
      return a < b ? a : b;
    }
  
    function stringComparator(a: any, b: any) {
      var aString = a.string;
      var bString = b.string;
  
      var aLength = aString.length;
      var bLength = bString.length;
      var length = min(aLength, bLength);
  
      for (var i = 0; i < length; ++i) {
        var aCharCode = aString.charCodeAt(i);
        var bCharCode = bString.charCodeAt(i);
  
        if (aCharCode === bCharCode) {
          continue;
        }
  
        return aCharCode - bCharCode;
      }
  
      return aLength - bLength;
    }
  
    // Move undefineds and holes to the end of a sparse array. Result is [values..., undefineds..., holes...].
    function compactSparse(array: any, dst: any, src: any, length: any) {
      var seen: any = { };
      var valueCount = 0;
      var undefinedCount = 0;
  
      // Clean up after the in-progress non-sparse compaction that failed.
      for (let i = dst; i < src; ++i) {
        delete array[i];
      }
  
      for (var obj = array; obj; obj = Object.getPrototypeOf(obj)) {
        var propertyNames = Object.getOwnPropertyNames(obj);
        for (var i = 0; i < propertyNames.length; ++i) {
          var index = propertyNames[i];
          if (index < length) { // Exclude non-numeric properties and properties past length.
            if (seen[index]) { // Exclude duplicates.
              continue;
            }
            seen[index] = 1;
  
            var value = array[index];
            delete array[index];
  
            if (value === undefined) {
              ++undefinedCount;
              continue;
            }
  
            array[valueCount++] = value;
          }
        }
      }
  
      for (var i = valueCount; i < valueCount + undefinedCount; ++i) {
        array[i] = undefined;
      }
  
      return valueCount;
    }
  
    function compactSlow(array: any, length: any) {
      var holeCount = 0;
  
      for (var dst = 0, src = 0; src < length; ++src) {
        if (!(src in array)) {
          ++holeCount;
          if (holeCount < 256) {
            continue;
          }
          return compactSparse(array, dst, src, length);
        }
  
        var value = array[src];
        if (value === undefined) {
          continue;
        }
  
        array[dst++] = value;
      }
  
      var valueCount = dst;
      var undefinedCount = length - valueCount - holeCount;
  
      for (var i = valueCount; i < valueCount + undefinedCount; ++i) {
        array[i] = undefined;
      }
  
      for (var i = valueCount + undefinedCount; i < length; ++i) {
        delete array[i];
      }
  
      return valueCount;
    }
  
    // Move undefineds and holes to the end of an array. Result is [values..., undefineds..., holes...].
    function compact(array: any, length: any) {
      for (var i = 0; i < array.length; ++i) {
        if (array[i] === undefined) {
          return compactSlow(array, length);
        }
      }
  
      return length;
    }
  
    function merge(dst: any, src: any, srcIndex: any, srcEnd: any, width: any, comparator: any) {
      var left = srcIndex;
      var leftEnd = min(left + width, srcEnd);
      var right = leftEnd;
      var rightEnd = min(right + width, srcEnd);
  
      for (var dstIndex = left; dstIndex < rightEnd; ++dstIndex) {
        if (right < rightEnd) {
          if (left >= leftEnd) {
            dst[dstIndex] = src[right++];
            continue;
          }
  
          let comparisonResult = comparator(src[right], src[left]);
          if ((typeof comparisonResult === "boolean" && !comparisonResult) || comparisonResult < 0) {
            dst[dstIndex] = src[right++];
            continue;
          }
  
        }
  
        dst[dstIndex] = src[left++];
      }
    }
  
    function mergeSort(array: any, valueCount: any, comparator: any) {
      var buffer : any = [ ];
      buffer.length = valueCount;
  
      var dst = buffer;
      var src = array;
      for (var width = 1; width < valueCount; width *= 2) {
        for (var srcIndex = 0; srcIndex < valueCount; srcIndex += 2 * width) {
          merge(dst, src, srcIndex, valueCount, width, comparator);
        }
  
        var tmp = src;
        src = dst;
        dst = tmp;
      }
  
      if (src !== array) {
        for(var i = 0; i < valueCount; i++) {
          array[i] = src[i];
        }
      }
    }
  
    function bucketSort(array: any, dst: any, bucket: any, depth: any) {
      if (bucket.length < 32 || depth > 32) {
        mergeSort(bucket, bucket.length, stringComparator);
        for (var i = 0; i < bucket.length; ++i) {
          array[dst++] = bucket[i].value;
        }
        return dst;
      }
  
      var buckets: any = [ ];
      for (var i = 0; i < bucket.length; ++i) {
        var entry = bucket[i];
        var string = entry.string;
        if (string.length === depth) {
          array[dst++] = entry.value;
          continue;
        }
  
        var c = string.charCodeAt(depth);
        if (!buckets[c]) {
          buckets[c] = [ ];
        }
        buckets[c][buckets[c].length] = entry;
      }
  
      for (var i = 0; i < buckets.length; ++i) {
        if (!buckets[i]) {
          continue;
        }
        dst = bucketSort(array, dst, buckets[i], depth + 1);
      }
  
      return dst;
    }
  
    function comparatorSort(array: any, length: any, comparator: any) {
      var valueCount = compact(array, length);
      mergeSort(array, valueCount, comparator);
    }
  
    function stringSort(array: any, length: any) {
      var valueCount = compact(array, length);
  
      var strings = new Array(valueCount);
      for (var i = 0; i < valueCount; ++i) {
        strings[i] = { string: array[i], value: array[i] };
      }
  
      bucketSort(array, 0, strings, 0);
    }
  
    var array = o;
  
    var length = array.length >>> 0;
  
    // For compatibility with Firefox and Chrome, do nothing observable
    // to the target array if it has 0 or 1 sortable properties.
    if (length < 2) {
      return array;
    }
  
    if (typeof comparator === "function") {
      comparatorSort(array, length, comparator);
    }
    else if (comparator === null || comparator === undefined) {
      stringSort(array, length);
         }
    else {
      throw new TypeError("Array.prototype.sort requires the comparsion function be a function or undefined");
         }
  
    return array;
  }
  
  function array_each(o: any, f: any) {
    var i = 0;
    var l = o.length;
    while (i < l) {
      f(o[i], i, o);
      i = i + 1;
    }
  }
  
  /* @stopify flat */
  export function isStopifyRunning() {
    /**
     * The relevant enum is
     * 
     * enum EventMode { Running = 0, Paused = 1, Waiting = 2 }
     * 
     * The check below returns true when that mode is Running, indicating the Stopify stack is live.
     * Paused means “a stack is captured and waiting to be resumed”
     * Waiting means “we are between/after a completed run of a stopify program/event handler”
     */
    // @ts-ignore
    return typeof $STOPIFY !== "undefined" && $STOPIFY.eventMode === 0;
  }

  /* @stopify flat */
  function stopifyDispatch(stopped : any, native : any) {
    
    function callStopped(thisArg: any, argsObj : any) {
      return stopped(thisArg, ...Array.from(argsObj));
    }
    
    return /* @stopify flat */ function(this : any) {
        if(isStopifyRunning()) {
            return callStopped(this, arguments);
        }
        else {
            return native.apply(this, arguments);
        }
    }
  }
  
export const stopifyArrayPrototype = {
    //__proto__: Array.prototype,
    map: stopifyDispatch(array_map, Array.prototype.map),
    filter: stopifyDispatch(array_filter, Array.prototype.filter),
    forEach: stopifyDispatch(array_each, Array.prototype.forEach),
    /*
    filter: function(f: any) { return stopifyArray(filter(this, f)); },
    reduceRight: function(this: any[], f: any, init: any): any {
      // NOTE(arjun): The MDN polyfill did not pass a simple test. I am quite sure
      // we never tested it before. This version works just fine.
      var arrLen = this.length;
      var acc = arguments.length === 1 ? this[arrLen - 1] : init;
      var i = arguments.length === 1 ? arrLen - 2 : arrLen - 1;
      while (i >= 0) {
        acc = f(acc, this[i], i, this);
        i = i - 1;
      }
      return acc;
    },
    reduce: function(this: any[], f: any, init: any) {
      // NOTE(arjun): The MDN polyfill did not pass a simple test. I am quite sure
      // we never tested it before. This version works just fine.
      var arrLen = this.length;
      var acc = arguments.length === 1 ? this[arrLen - 1] : init;
      var bound = arguments.length === 1 ? arrLen - 1 : arrLen;
      var i = 0;
      while (i < bound) {
        acc = f(acc, this[i], i, this);
        i = i + 1;
      }
      return acc;
    },
    // NOTE(arjun): thisArg ignored
    some: function(this: any[], pred: any) {
      var i = 0;
      var l = this.length;
      while (i < l) {
        if (pred(this[i])) {
          return true;
        }
        i = i + 1;
      }
      return false;
    },
    every: function(this: any[],pred: any) {
      var i = 0;
      var l = this.length;
      while (i < l) {
        if (!pred(this[i])) {
          return false;
        }
        i = i + 1;
      }
      return true;
    },
    find: function(this: any[],pred: any) {
      var i = 0;
      var l = this.length;
      while (i < l) {
        if (pred(this[i])) {
          return this[i];
        }
        i = i + 1;
      }
    },
    findIndex: function(this: any[],pred: any) {
      var i = 0;
      var l = this.length;
      while (i < l) {
        if (pred(this[i])) {
          return i;
        }
        i = i + 1;
      }
      return -1;
    },
    sort: function(comparator: any) {
      return stopifyArray(array_sort(this, comparator));
    }
    */
  };

  Object.assign(Array.prototype, stopifyArrayPrototype);
