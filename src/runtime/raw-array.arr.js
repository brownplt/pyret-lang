const NUMBERS = require("./js-numbers.js");
const EQUALITY = require("./equality.js");
const OPTION = require("./option.arr.js");
const RUNTIME = require("./runtime.js");

function checkArrayIndex(name, array, index, length) {
  let reason = "";
  if(!(NUMBERS["isInteger"](index) && NUMBERS["isNonNegative"](index))) {
    reason = (`expected a non-negative integer, got ${index}`);
  }
  if(index >= length ) { reason = (`index too large: ${index} for array of length ${length}`); }
  else if (index < 0) { reason = (`negative index for array set: ${index}`); }
  if(reason != "") {
    RUNTIME.throwError("invalid-array-index", name, array, index, reason);
  }
}

module.exports = {
  'raw-array': {
    'make': function(arr) {
      return arr;
    }
  },
  'raw-array-length': function(arr) {
    return arr.length;
  },
  'raw-array-set': function( arr, index, value ) {
    checkArrayIndex("raw-array-set", arr, index, arr.length);
    arr[index] = value;
    return arr;
  },
  'raw-array-get': function( arr, index ) {
    checkArrayIndex("raw-array-set", arr, index, arr.length);
    return arr[index];
  },
  'raw-array-push': function( arr, elm ) {
    arr.push( elm );
    return arr;
  },
  'raw-array-map': function(fun, arr) {
    const ret = [];
    for(let i = 0; i < arr.length; i += 1) {
      ret.push(fun(arr[i]));
    }
    return ret;
  },
  'raw-array-for-each': function(fun, arr) {
    return arr.forEach(fun);
  },
  'raw-array-fold': function( fun, val, arr ) {
    return arr.reduce( fun, val );
  },
  'raw-array-foldr': function( fun, val, arr ) {
    return arr.reduceRight( fun, val );
  },
  'raw-array-sum': function( arr ) {
    return arr.reduce(function( x, y) {
      return NUMBERS["add"](x, y);
    }, 0);
  },
  'raw-array-min': function( arr ) {
    if (arr.length == 0) {
      throw new Error("Invalid array length: 0");
    }

    return arr.reduce(function(x, y) {
      if (EQUALITY["_lessthan"](x, y)) {
        return x;
      } else {
        return y;
      }
    }, arr[0]);
  },
  'raw-array-max': function( arr ) {
    if (arr.length == 0) {
      throw new Error("Invalid array length: 0");
    }

    return arr.reduce(function(x, y) {
      if (EQUALITY["_greaterthan"](x, y)) {
        return x;
      } else {
        return y;
      }
    }, arr[0]);
  },
  'raw-array-of': function(elem, n) {
    if (EQUALITY["_lessthan"](n, 0)) {
      throw "raw-array-of: <0 repititions";
    }

    const jsN = NUMBERS["toFixnum"](n);

    return new Array(jsN).fill(elem);
  },
  'raw-array-build': function(f, n) {
    // TODO(joe/ben May 2022): add/test for reasonable range
    if (EQUALITY["_lessthan"](n, 0)) {
      throw "raw-array-build: <0";
    }

    const jsN = NUMBERS["toFixnum"](n);
    let array = new Array(jsN);
    for (let i = 0; i < jsN; i++) {
      array[i] = f(i);
    }

    return array;
  },
  'raw-array-build-opt': function(f, n) {

    if (EQUALITY["_lessthan"](n, 0)) {
      throw "raw-array-build-opt: <0";
    }

    const jsN = NUMBERS["toFixnum"](n);
    let array = [];
    for (let i = 0; i < jsN; i++) {
      const v = f(i);
      if(OPTION["is-some"](v)) {
        array.push(v.value);
      }
    }

    return array;
  },
  'raw-array-filter': function(f, arr) {
    let array = [];
    for (let i = 0; i < arr.length; i++) {
      const v = f(arr[i]);
      if(v) { array.push(arr[i]); }
    }
    return array;
  },
  'raw-array-join': function(arr, sep) {
    return arr.join(sep);
  },
  'raw-array-join-last': function(arr, sep, sepLast) {
    if(arr.length === 0) { return ""; }
    if(arr.length === 1) { return arr[0]; }
    return arr.slice(0, arr.length - 1).join(sep) + sepLast + arr[arr.length - 1];
  },
  'is-raw-array': function(v) {
    // TODO(alex): may need to move this to primitives.ts
    return Array.isArray(v);
  },
  'raw-array-from-list': function(v) {
    const arr = [];
    let i = 0;
    while("rest" in v) {
      arr[i] = v.first;
      i += 1;
      v = v.rest;
    }
    return arr;
  },
  'raw-array-and-mapi': function(f, arr, start) {
    for(let i = start; i < arr.length; i += 1) {
      const result = f(arr[i], i);
      if(result === false) { return false; }
    }
    return true;
  },
  'raw-array-or-mapi': function(f, arr, start) {
    for(let i = start; i < arr.length; i += 1) {
      const result = f(arr[i], i);
      if(result === true) { return true; }
    }
    return false;
  },
  'raw-array-map-1': function(f1, f, arr) {
    if(arr.length === 0) { return []; }
    const result = new Array(arr.length);
    result[0] = f1(arr[0]);
    for(let i = 1; i < arr.length; i += 1) {
      result[i] = f(arr[i]);
    }
    return result;
  }
}
