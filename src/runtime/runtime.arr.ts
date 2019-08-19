/*
 * 'export named-js-value' desugars into 'exports.name = js-value'
 * 'exports' is set to 'module.exports'
 *
 * https://stackoverflow.com/questions/16383795/difference-between-module-exports-and-exports-in-the-commonjs-module-system
 *
 */

const $EqualBrand = {"names":false};
const $NotEqualBrand = {"names":["reason","value1","value2"]};
const $UnknownBrand = {"names":["reason","value1","value2"]};
const $EqualTag = 0;
const $NotEqualTag = 1;
const $UnknownTag = 2;

type UndefBool = undefined | boolean

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

export type TypeEqualityResult = Equal | NotEqual | Unknown;

export function Equal(): Equal {
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

function equalityResultToBool(ans: TypeEqualityResult): boolean {
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

// TODO(alex): Handle Pyret numbers
function isNumber(val: any): boolean {
  return typeof val === "number";
}

function isBoolean(val: any): boolean {
  return typeof val === "boolean";
}

function isBrandedObject(val: any): boolean {
  return (typeof val === "object") && ("$brand" in val);
}

export function identical3(v1: any, v2: any): TypeEqualityResult {
  if (isFunction(v1) && isFunction(v2)) {
    return Unknown("Function", v1, v2);
  // TODO(alex): Handle/detect methods
  // } else if (isMethod(v1) && isMethod(v2)) {
  //  return thisRuntime.ffi.unknown.app('Methods', v1,  v2);
  //  TODO(alex): Handle/detect rough numbers
  // } else if (jsnums.isRoughnum(v1) && jsnums.isRoughnum(v2)) {
  //  return thisRuntime.ffi.unknown.app('Roughnums', v1,  v2);
  } else if (v1 === v2) {
    return Equal();
  } else {
    return NotEqual("", v1, v2);
  }
}

export function identical(v1: any, v2: any): boolean {
  let ans: TypeEqualityResult = identical3(v1, v2);
  return equalityResultToBool(ans);
}

export function equalAlways3(e1: any, e2: any) {
  if (isEqual(identical3(e1, e2))) {
    // Identical so must always be equal
    return Equal;
  }

  var worklist = [[e1, e2]];
  while (worklist.length > 0) {
    var curr = worklist.pop();
    var v1: any = curr[0];
    var v2: any = curr[1];

    if (isEqual(identical3(e1, e2))) {
      // Identical so must always be equal
      continue; 
    }

    if (isNumber(v1) && isNumber(v2)) {
      // TODO(alex): Assuming JS numbers. Create helper that abstracts over either
      if (v1 !== v2) { return NotEqual("Numbers", v1, v2); }
      continue;

    } else if (isBoolean(v1) && isBoolean(v2)) {
      if (v1 !== v2) { return NotEqual("Booleans", v1, v2); }

    } else if (isFunction(v1) && isFunction(v2)) {
      // Cannot compare functions for equality
      return Unknown("Functions", v1, v2);

    } else if (isBrandedObject(v1) && isBrandedObject(v2)) {
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
    } else {
      return NotEqual("", e1, e2);
    }
  }

  return Equal();
}

export function traceValue(loc, value) {
  // NOTE(alex): stubbed out until we decide what to actually do with it
  return value;
}
