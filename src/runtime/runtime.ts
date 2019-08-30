/*
 * 'export named-js-value' desugars into 'exports.name = js-value'
 *
 * https://stackoverflow.com/questions/16383795/difference-between-module-exports-and-exports-in-the-commonjs-module-system
 *
 */

const _NUMBER = require("./js-numbers.js");

const $EqualBrand = {"names":false};
const $NotEqualBrand = {"names":["reason","value1","value2"]};
const $UnknownBrand = {"names":["reason","value1","value2"]};
const $EqualTag = 0;
const $NotEqualTag = 1;
const $UnknownTag = 2;

const $PTupleBrand = "tuple";
const $PRefBrand = "ref";

type UndefBool = undefined | boolean

// ********* Runtime Type Representations (Non-Primitives) *********
export interface PTuple {
  $brand: string,
  [key: string]: any,
}

export function PTuple(values: any[]): PTuple {
  values["$brand"] = $PTupleBrand;

  return <PTuple><any>values;
}

export interface DataValue {
  $brand: any,
  [key: string]: any
}

export interface PRef {
  $brand: string,
  ref: Object,
}

// ********* EqualityResult Representations *********
export interface Equal { 
  $brand: any,
  $tag: number,
}

export interface NotEqual {
  $brand: any,
  $tag: number,
  reason: string,
  value1: any,
  value2: any,
}

export interface Unknown {
  $brand: any,
  $tag: number,
  reason: string,
  value1: any,
  value2: any,
}

export type EqualityResult = Equal | NotEqual | Unknown;

function Equal(): Equal {
  return {
    "$brand": $EqualBrand,
    "$tag": $EqualTag,
  };
}

export function NotEqual(reason: string, value1: any, value2: any): NotEqual {
  return {
    "$brand": $NotEqualBrand,
    "$tag": $NotEqualTag,
    "reason": reason,
    "value1": value1,
    "value2": value2,
  };
}

export function Unknown(reason: string, value1: any, value2: any): Unknown {
  return {
    "$brand": $UnknownBrand,
    "$tag": $UnknownTag,
    "reason": reason,
    "value1": value1,
    "value2": value2,
  };
}

export function isEqual(val: any): boolean{
  return val.$brand === $EqualBrand;
}

export function isNotEqual(val: any): boolean {
  return val.$brand === $NotEqualBrand;
}

export function isUnknown(val: any): boolean {
  return val.$brand === $UnknownBrand;
}


// ********* Helpers *********
function equalityResultToBool(ans: EqualityResult): boolean {
  if (isEqual(ans)) { 
    return true; 
  } else if (isNotEqual(ans)) { 
    return false; 
  } else if (isUnknown(ans)) {
    let unknownVariant = ans as Unknown;
    throw {
      reason: unknownVariant.reason,
      value1: unknownVariant.value1,
      value2: unknownVariant.value2,
    };
  }
}

function isFunction(obj: any): boolean { return typeof obj === "function"; }

// TODO(alex): Identify methods
function isMethod(obj: any): boolean { return typeof obj === "function"; }

// TODO(alex): Will nothing always be value 'undefined'?
function isNothing(obj: any): boolean { return obj === undefined };

// TODO(alex): Identify opaque types
function isOpaque(val: any): boolean { return false; }

const isNumber: (val: any) => boolean = _NUMBER["isPyretNumber"];
const isRoughNumber: (val: any) => boolean = _NUMBER["isRoughnum"];
const numericEquals: (v1: any, v2: any, callbacks: NumericErrorCallbacks) => boolean = _NUMBER["equals"];

function isBoolean(val: any): boolean {
  return typeof val === "boolean";
}

function isString(val: any): boolean {
  return typeof val === "string";
}

function isDataVariant(val: any): boolean {
  return (typeof val === "object") && ("$brand" in val) && !(isPTuple(val));
}

function isRawObject(val: any): boolean {
  return (typeof val === "object") && !("$brand" in val);
}

function isPTuple(val: any): boolean {
  return (Array.isArray(val)) && ("$brand" in val) && (val["$brand"] === $PTupleBrand);
}

function isArray(val: any): boolean {
  return (Array.isArray(val)) && !("$brand" in val);
}

function isPRef(val: any): boolean {
  return (typeof val === "object") && ("$brand" in val) && (val["$brand"] === $PRefBrand);
}

export interface NumericErrorCallbacks {
  throwDivByZero: (msg: any) => void,
  throwToleranceError: (msg: any) => void,
  throwRelToleranceError: (msg: any) => void,
  throwGeneralError: (msg: any) => void,
  throwDomainError: (msg: any) => void,
  throwSqrtNegative: (msg: any) => void,
  throwLogNonPositive: (msg: any) => void,
  throwIncomparableValues: (msg: any) => void,
  throwInternalError: (msg: any) => void,
}

var NumberErrbacks: NumericErrorCallbacks = {
  throwDivByZero: function(msg) { throw msg; },
  throwToleranceError: function(msg) { throw msg; },
  throwRelToleranceError: function(msg) { throw msg; },
  throwGeneralError: function(msg) { throw msg; },
  throwDomainError: function(msg) { throw msg; },
  throwSqrtNegative: function(msg) { throw msg; },
  throwLogNonPositive: function(msg) { throw msg; },
  throwIncomparableValues: function(msg) { throw msg; },
  throwInternalError: function(msg) { throw msg; },
};

// ********* Equality Functions *********
export function identical3(v1: any, v2: any): EqualityResult {
  if (isFunction(v1) && isFunction(v2)) {
    return Unknown("Function", v1, v2);
  // TODO(alex): Handle/detect methods
  // } else if (isMethod(v1) && isMethod(v2)) {
  //  return thisRuntime.ffi.unknown.app('Methods', v1,  v2);
  //  TODO(alex): Handle/detect rough numbers
  } else if (isRoughNumber(v1) && isRoughNumber(v2)) {
    return Unknown('Roughnums', v1,  v2);
  } else if (v1 === v2) {
    return Equal();
  } else {
    return NotEqual("", v1, v2);
  }
}

export function identical(v1: any, v2: any): boolean {
  let ans: EqualityResult = identical3(v1, v2);
  return equalityResultToBool(ans);
}

/*
 * Structural equality. Stops at mutable data (refs) and only checks that 
 * mutable data are identical.
 *
 * Data variants and raw (unbranded) objects are NEVER equal.
 *
 */
export function equalAlways3(e1: any, e2: any): EqualityResult {
  if (isEqual(identical3(e1, e2))) {
    // Identical so must always be equal
    return Equal();
  }

  var worklist = [[e1, e2]];
  while (worklist.length > 0) {
    var curr = worklist.pop();
    var v1: any = curr[0];
    var v2: any = curr[1];

    if (isEqual(identical3(v1, v2))) {
      // Identical so must always be equal
      continue; 
    }

    if (isNumber(v1) && isNumber(v2)) {
      if (isRoughNumber(v1) || isRoughNumber(v2)) {
        return Unknown("Rough Number equal-always", v1, v2);
      } else if (numericEquals(v1, v2, NumberErrbacks)) {
        continue;
      } else {
        return NotEqual("Numers", v1, v2);
      }

    } else if (isBoolean(v1) && isBoolean(v2)) {
      if (v1 !== v2) { return NotEqual("Booleans", v1, v2); }
      continue;

    } else if (isString(v1) && isString(v2)) {
      if (v1 !== v2) { return NotEqual("Strings", v1, v2); }
      continue

    } else if (isFunction(v1) && isFunction(v2)) {
      // Cannot compare functions for equality
      return Unknown("Functions", v1, v2);
      
      // TODO(alex): Handle methods
    } else if (isPTuple(v1) && isPTuple(v2)) {
      if (v1.length !== v2.length) {
        return NotEqual("PTuple Length", v1, v2);
      }

      for (var i = 0; i < v1.length; i++) {
        worklist.push([v1[i], v2[i]]);
      }
      continue;

    } else if (isArray(v1) && isArray(v2)) {
      if (v1.length !== v2.length) {
        return NotEqual("Array Length", v1, v2);
      }

      for (var i = 0; i < v1.length; i++) {
        worklist.push([v1[i], v2[i]]);
      }
      continue;

    } else if (isNothing(v1) && isNothing(v2)) {
      // TODO(alex): Is equality defined for Pyret Nothing?
      continue; 

    } else if (isPRef(v1) && isPRef(v2)) {
      // In equal-always, non-identical refs are not equal
      if (v1.ref !== v2.ref) {
        return NotEqual("PRef'd Objects", v1, v2);
      }
      continue;

    } else if (isDataVariant(v1) && isDataVariant(v2)) {
      // TODO(alex): Check for _equal method
      if(v1.$brand && v1.$brand === v2.$brand) {
        var fields1 = v1.$brand.names;
        var fields2 = v2.$brand.names;

        if(fields1.length !== fields2.length) { 
          // Not the same brand
          return NotEqual("Object Brands", v1, v2);
        }
        for(var i = 0; i < fields1.length; i += 1) {
          if(fields1[i] != fields2[i]) { 
            // Not the same brand
            return NotEqual("Field Brands", fields1[i], fields2[i]);
          }
          worklist.push([v1[fields1[i]], v2[fields2[i]]]);
        }
        continue;
      }
    } else if (isRawObject(v1) && isRawObject(v2)) {
      let keys1 = Object.keys(v1);
      let keys2 = Object.keys(v2);

      if (keys1.length !== keys2.length) {
        return NotEqual("Raw Object Field Count", v1, v2);
      }

      // Check for matching keys and push field to worklist
      for (var i = 0; i < keys1.length; i++) {
        if (!keys2.includes(keys1[i])) {
          // Key in v1 not found in v2
          return NotEqual(`Raw Object Missing Field '${keys1[i]}'`, v1, v2);
        } else {
          // Push common field to worklist
          worklist.push([v1[keys1[i]], v2[keys2[i]]]);
        }
      }

      continue;
    } else {
      return NotEqual("", e1, e2);
    }
  }

  return Equal();
}

export function equalAlways(v1: any, v2: any): boolean {
  let ans = equalAlways3(v1, v2);
  return equalityResultToBool(ans);
}

// *********Spy Stuff*********
export interface SpyExpr {
  key: string,
  expr: () => any,
  loc: string
}

export interface SpyObject {
  message: () => string,
  loc: string,
  exprs: SpyExpr[],
}

function _spy(spyObject: SpyObject): void {
  const message = spyObject.message();
  const spyLoc = spyObject.loc;
  if (message != null) {
    console.log("Spying \"" + message + "\" (at " + spyLoc + ")");
  } else {
    console.log("Spying (at " + spyLoc + ")");
  }

  const exprs = spyObject.exprs;
  for (let i = 0; i < exprs.length; i++) {
    const key = exprs[i].key;
    const loc = exprs[i].loc;
    const value = exprs[i].expr();
    console.log("  " + key + ": " + value);
  }
}

// *********Check Stuff*********
interface CheckResult {
  success: boolean,
  path: string,
  loc: string,
  lhs: any,
  rhs: any,
}

interface CheckTestResult {
  success: boolean,
  lhs: any,
  rhs: any,
}

var _globalCheckContext: string[] = [];
var _globalCheckResults: CheckResult[] = [];

function checkResults(): CheckResult[] {
  return _globalCheckResults.slice();
}

function eagerCheckTest(test: () => CheckTestResult, loc: string): void {
  try {
    let result = test();
    _globalCheckResults.push({
        success: result.success,
        path: _globalCheckContext.join(),
        loc: loc,
        lhs: result.lhs,
        rhs: result.rhs,
    });
  } catch(e) {
    _globalCheckResults.push({
        success: false,
        path: _globalCheckContext.join(),
        loc: loc,
        lhs: undefined,
        rhs: undefined,
    });
  } 
}

function eagerCheckBlockRunner(name: string, checkBlock: () => void): void {
  _globalCheckContext.push(name);

  try {
    checkBlock();

  } catch(e) {
    throw e;

  } finally {

    _globalCheckContext.pop();
  }
}

// ********* Other Functions *********
export function traceValue(loc, value) {
  // NOTE(alex): stubbed out until we decide what to actually do with it
  return value;
}

// Allow '+' for string concat. 
// Otherwise, defer to the number library.
function customAdd(lhs: any, rhs: any, errbacks: NumericErrorCallbacks): any {
  if (typeof(lhs) === "string" && typeof(rhs) === "string") {
    return lhs + rhs;
  } else {
    return _NUMBER["add"](lhs, rhs, errbacks);
  }
}


// Hack needed b/c of interactions with the 'export' keyword
// Pyret instantiates singleton data varaints by taking a reference to the value
// TODO(alex): Should Pyret perform a function call to create a singleton data variant
module.exports["Equal"] = Equal();

// Hack needed to match generate Pyret-code
module.exports["is-Equal"] = isEqual;
module.exports["is-NotEqual"] = isNotEqual;
module.exports["is-Unknown"] = isUnknown;

// Expected runtime functions
module.exports["_checkTest"] = eagerCheckTest;
module.exports["_checkBlock"] = eagerCheckBlockRunner;
module.exports["_checkResults"] = checkResults;

module.exports["_spy"] = _spy;
module.exports["_makeRational"] = _NUMBER["makeRational"];
module.exports["_makeRoughnum"] = _NUMBER["makeRoughnum"];
module.exports["_errCallbacks"] = NumberErrbacks;

module.exports["_add"] = customAdd;
module.exports["_subtract"] = _NUMBER["subtract"];
module.exports["_multiply"] = _NUMBER["multiply"];
module.exports["_divide"] = _NUMBER["divide"];

module.exports["_lessThan"] = _NUMBER["lessThan"];
module.exports["_greaterThan"] = _NUMBER["greaterThan"];
module.exports["_lessThanOrEqual"] = _NUMBER["lessThanOrEqual"];
module.exports["_greaterThanOrEqual"] = _NUMBER["greaterThanOrEqual"];
module.exports["_makeNumberFromString"] = _NUMBER['fromString'];
