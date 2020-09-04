// TODO(alex): roughly-equals and co (not documented but found in js/base/runtime.js)

const _NUMBER = require("./js-numbers.js");
const PRIMTIVES = require("./primitives.js");

const $EqualBrand = {"names":false};
const $NotEqualBrand = {"names":["reason","value1","value2"]};
const $UnknownBrand = {"names":["reason","value1","value2"]};
const $EqualTag = 0;
const $NotEqualTag = 1;
const $UnknownTag = 2;

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

// This function is used by flat functions in image.arr.js, so it must also be flat.
/* @stopify flat */
export function Equal(): Equal {
  return {
    "$brand": $EqualBrand,
    "$tag": $EqualTag,
  };
}

// This function is used by flat functions in image.arr.js, so it must also be flat.
/* @stopify flat */
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

const TOL_IS_REL = true;
const TOL_IS_ABS = false;
const EQUAL_ALWAYS = true;
const EQUAL_NOW = false;
const FROM_WITHIN = true;

const numericEquals: (v1: any, v2: any, callbacks: NumericErrorCallbacks) => boolean = _NUMBER["equals"];

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

const NumberErrbacks: NumericErrorCallbacks = {
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

// Hack needed to not mess with Stopify
// See primitives.ts for more info
export {
    NumberErrbacks
}

// ********* Equality Functions *********
export function identical3(v1: any, v2: any): EqualityResult {
  if (PRIMTIVES.isFunction(v1) && PRIMTIVES.isFunction(v2)) {
    return Unknown("Function", v1, v2);
  } else if (PRIMTIVES.isMethod(v1) && PRIMTIVES.isMethod(v2)) {
    return Unknown("Method", v1, v2);
  } else if (PRIMTIVES.isRoughNumber(v1) && PRIMTIVES.isRoughNumber(v2)) {
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

export function equalNow(v1: any, v2: any): boolean {
  let ans: EqualityResult = equalNow3(v1, v2);
  return equalityResultToBool(ans);
}

export function equalNow3(v1: any, v2: any): EqualityResult {
  return equalCore3(v1, v2, EQUAL_NOW, 0, TOL_IS_ABS, false);
}

/*
 * Structural equality. Stops at mutable data (refs) and only checks that
 * mutable data are identical.
 *
 * Data variants and raw (unbranded) objects are NEVER equal.
 *
 */
export function equalAlways3(v1: any, v2: any): EqualityResult {
  return equalCore3(v1, v2, EQUAL_ALWAYS, 0, TOL_IS_ABS, false);
}

export function equalAlways(v1: any, v2: any): boolean {
  let ans = equalAlways3(v1, v2);
  return equalityResultToBool(ans);
}

//fun equal-and(er1 :: EqualityResult, er2 :: EqualityResult):
//  ask:
//    | is-NotEqual(er1) then: er1
//    | is-NotEqual(er2) then: er2
//    | is-Unknown(er1) then: er1 #: i.e., the first Unknown
//    | otherwise: er2 # Equal or Equal/Equal or Unknown
//  end
//end
export function equal_and(er1: EqualityResult, er2: EqualityResult): EqualityResult {
    if (isNotEqual(er1)) {
        return er1;
    } else if (isNotEqual(er2)) {
        return er2;
    } else if (isUnknown(er1)) {
        return er1;
    } else {
        return er2;
    }
}


//fun equal-or(er1 :: EqualityResult, er2 :: EqualityResult):
//  ask:
//    | is-Equal(er1) then: er1
//    | is-Equal(er2) then: er2
//    | is-Unknown(er1) then: er1 # i.e., the first Unknown
//    | otherwise: er2 # NotEqual or NotEqual/NotEqual or Unknown
//  end
//end
export function equal_or(er1: EqualityResult, er2: EqualityResult): EqualityResult {
    if (isEqual(er1)) {
        return er1;
    } else if (isEqual(er2)) {
        return er2;
    } else if (isUnknown(er1)) {
        return er1;
    } else {
        return er2;
    }
}


//fun to-boolean(er :: EqualityResult):
//  cases(EqualityResult) er:
//    | Unknown(r, v1, v2) => raise(error.equality-failure(r, v1, v2))
//    | Equal => true
//    | NotEqual(_,_,_) => false
//  end
//end
export function to_boolean(er: EqualityResult): boolean {
    if (isUnknown(er)) {
        // TODO(alex): Fill this in with the generic `raise` function
        //   CANNOT IMPORT "global.arr.js" OR "runtime.ts" directly b/c the circular depenency
        //   will mess with loading...
        throw "Unable to convert Unknown (EqualityResult) to boolean";
    } else {
        return isEqual(er);
    }

}

/// within-*

const within = withinRel;
const withinNow = withinRelNow;
const within3 = withinRel3;
const withinNow3 = withinRelNow3;

// NOTE(alex): DO NOT EXPORT CONSTANTS DIRECTLY
//   Hack required by Stopify (see primitives.ts for more details)
export {
  within,
  withinNow,
  within3,
  withinNow3,
}

export function withinRel(tolerance) {
  const inner = withinRel3(tolerance);
  return function(l, r): boolean {
    return to_boolean(inner(l, r));
  };
}

export function withinAbs(tolerance) {
  const inner = withinAbs3(tolerance);
  return function(l, r): boolean {
    return to_boolean(inner(l, r));
  };
}

export function withinRelNow(tolerance) {
  const inner = withinRelNow3(tolerance);
  return function(l, r): boolean {
    return to_boolean(inner(l, r));
  };
}

export function withinAbsNow(tolerance) {
  const inner = withinAbsNow3(tolerance);
  return function(l, r): boolean {
    return to_boolean(inner(l, r));
  };
}

/// within-*3
export function withinRel3(tolerance) {
  return function(l, r) {
    return equalCore3(l, r, EQUAL_ALWAYS, tolerance, TOL_IS_REL, FROM_WITHIN);
  };
}

export function withinAbs3(tolerance) {
  return function(l, r) {
    return equalCore3(l, r, EQUAL_ALWAYS, tolerance, TOL_IS_ABS, FROM_WITHIN);
  };
}

export function withinRelNow3(tolerance) {
  return function(l, r) {
    return equalCore3(l, r, EQUAL_NOW, tolerance, TOL_IS_REL, FROM_WITHIN);
  };
}

export function withinAbsNow3(tolerance) {
  return function(l, r) {
    return equalCore3(l, r, EQUAL_NOW, tolerance, TOL_IS_ABS, FROM_WITHIN);
  };
}

export function _lessthan(lhs: any, rhs: any): boolean {
    // Check if object has a '<' custom implementation
    if ((typeof lhs === "object") && ("_lessthan" in lhs)) {
        return lhs._lessthan(rhs);
    } else if (PRIMTIVES.isString(lhs) && PRIMTIVES.isString(rhs)) {
        // NOTE: JS uses lexicographical order
        return lhs < rhs;
    } else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.lessThan(lhs, rhs);
    } else {
        // NOTE: may be a dynamic error
        try {
            return lhs < rhs;
        } catch (error) {
            throw `Unable to perform '<' on (${lhs}) and (${rhs})`;
        }
    }
}

export function _greaterthan(lhs: any, rhs: any): boolean {
    // Check if object has a '>' custom implementation
    if ((typeof lhs === "object") && ("_greaterthan" in lhs)) {
        return lhs._greaterthan(rhs);
    } else if (PRIMTIVES.isString(lhs) && PRIMTIVES.isString(rhs)) {
        // NOTE: JS uses lexicographical order
        return lhs > rhs;
    } else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.greaterThan(lhs, rhs);
    } else {
        // NOTE: may be a dynamic error
        try {
            return lhs > rhs;
        } catch (error) {
            throw `Unable to perform '>' on (${lhs}) and (${rhs})`;
        }
    }
}

export function _lessequal(lhs: any, rhs: any): boolean {
    return equalAlways(lhs, rhs) || _lessthan(lhs, rhs);
}

export function _greaterequal(lhs: any, rhs: any): boolean {
    return equalAlways(lhs, rhs) || _greaterthan(lhs, rhs);
}

// tol and rel are PyretNumber's
function equalCore3(left: any, right: any, alwaysFlag: boolean, tol: number, rel: boolean, fromWithin: boolean): EqualityResult {

  if(tol === undefined) { // means that we aren't doing any kind of within
    const isIdentical = identical3(left, right);
    if (!isNotEqual(isIdentical)) {
      return isIdentical;   // Equal or Unknown
    }
  }

  let worklist = [[left, right]];

  // Actual equality implementation on individual items
  function equalHelp(): EqualityResult {
    let curr = worklist.pop();
    let v1: any = curr[0];
    let v2: any = curr[1];

    if (isEqual(identical3(v1, v2))) {
      // Identical so must always be equal
      return Equal();
    }

    if (PRIMTIVES.isNumber(v1) && PRIMTIVES.isNumber(v2)) {

      if (tol) {
        if (rel) {
          if (_NUMBER["roughlyEqualsRel"](v1, v2, tol, NumberErrbacks)) {
            return Equal();
          } else {
            return NotEqual("", v1, v2);
          }
        } else if (_NUMBER["roughlyEquals"](v1, v2, tol, NumberErrbacks)) {
          return Equal();
        } else {
          return NotEqual("", v1, v2);
        }
      } else if (PRIMTIVES.isRoughNumber(v1) || PRIMTIVES.isRoughNumber(v2)) {
        return Unknown("Rough Number equal-always", v1, v2);
      } else if (numericEquals(v1, v2, NumberErrbacks)) {
        return Equal();
      } else {
        return NotEqual("Numbers", v1, v2);
      }

    } else if (PRIMTIVES.isBoolean(v1) && PRIMTIVES.isBoolean(v2)) {
      if (v1 !== v2) { return NotEqual("Booleans", v1, v2); }
      return Equal();

    } else if (PRIMTIVES.isString(v1) && PRIMTIVES.isString(v2)) {
      if (v1 !== v2) { return NotEqual("Strings", v1, v2); }
      return Equal();

    } else if (PRIMTIVES.isFunction(v1) && PRIMTIVES.isFunction(v2)) {
      // Cannot compare functions for equality
      return Unknown("Functions", v1, v2);
    } else if (PRIMTIVES.isMethod(v1) && PRIMTIVES.isMethod(v2)) {
      return Unknown("Methods", v1, v2);
    } else if (PRIMTIVES.isPTuple(v1) && PRIMTIVES.isPTuple(v2)) {
      if (v1.length !== v2.length) {
        return NotEqual("PTuple Length", v1, v2);
      }

      for (var i = 0; i < v1.length; i++) {
        worklist.push([v1[i], v2[i]]);
      }
      return Equal();

    } else if (PRIMTIVES.isArray(v1) && PRIMTIVES.isArray(v2)) {
      if (alwaysFlag || v1.length !== v2.length) {
        return NotEqual("Array Length", v1, v2);
      }

      for (var i = 0; i < v1.length; i++) {
        worklist.push([v1[i], v2[i]]);
      }
      return Equal();

    } else if (PRIMTIVES.isNothing(v1) && PRIMTIVES.isNothing(v2)) {
      // Equality is defined for 'nothing'
      // 'nothing' is always equal to 'nothing'
      return Equal();

    } else if (PRIMTIVES.isPRef(v1) && PRIMTIVES.isPRef(v2)) {
      // In equal-always, non-identical refs are not equal
      // TODO: handle alwaysFlag
      if (v1.ref !== v2.ref) {
        return NotEqual("PRef'd Objects", v1, v2);
      }

      return Equal();

    } else if (PRIMTIVES.isTable(v1) && PRIMTIVES.isTable(v2)) {
        let v1_headers = v1._headers;
        let v2_headers = v2._headers;

        if (v1_headers.length !== v2_headers.length) {
            return NotEqual(`Row Header Length (${v1_headers.length}, ${v2_headers.length})`, v1, v2);
        }

        if (v1._rows.length !== v2._rows.length) {
            return NotEqual(`Row Length (${v1._rows.length}, ${v2._rows.length})`, v1, v2);
        }

        // Check table headers
        for (let h = 0; h < v1_headers.length; h++) {
            if (v1_headers[h] !== v2_headers[h]) {
                return NotEqual(`Row headers (index ${h})`, v1, v2);
            }
        }

        // Check row elements
        for (let row = 0; row < v1._rows.length; row++) {
            let v1_row = v1._rows[row];
            let v2_row = v2._rows[row];

            for (let i = 0; i < v1_row.length; i++) {
                worklist.push([v1_row[i], v2_row[i]]);
            }
        }

        return Equal();
    } else if (PRIMTIVES.isRow(v1) && PRIMTIVES.isRow(v2)) {

        let v1_headers = v1._headers;
        let v2_headers = v2._headers;
        if (v1_headers.length !== v2_headers.length) {
            return NotEqual(`Row Header Length (${v1_headers.length}, ${v2_headers.length})`, v1, v2);
        }

        // TODO(alex): is this check necessary?
        if (v1._elements.length !== v2._elements.length) {
            return NotEqual(`Row Elements Length (${v1._elements.length}, ${v1._elements.length})`, v1, v2);
        }

        for (let i = 0; i < v1_headers.length; i++) {
            if (v1_headers[i] !== v2_headers[i]) {
                return NotEqual(`Row headers (index ${i})`, v1, v2);
            }
        }

        for (let i = 0; i < v1._elements.length; i++) {
            worklist.push([v1[i], v2[i]]);
        }

        return Equal();

    } else if (PRIMTIVES.isDataVariant(v1) && PRIMTIVES.isDataVariant(v2)) {
      if(v1.$brand && v1.$brand === v2.$brand) {
        if ("_equals" in v1) {
          //   NOTE(alex): CURRENTLY FAILS WITH CYCLIC STRUCTURES
          var ans = v1["_equals"](v2, equalHelp);

          return ans;
        }

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
        return Equal();

      } else {
        return NotEqual("Variant Brands", v1, v2);
      }
    } else if (PRIMTIVES.isRawObject(v1) && PRIMTIVES.isRawObject(v2)) {
      let keys1 = Object.keys(v1);
      let keys2 = Object.keys(v2);

      if (keys1.length !== keys2.length) {
        return NotEqual("Raw Object Field Count", v1, v2);
      }

      // Check for matching keys and push field to worklist
      for (var i = 0; i < keys1.length; i++) {
        let key2Index = keys2.indexOf(keys1[i]);
        if (key2Index === -1) {
          // Key in v1 not found in v2
          return NotEqual(`Raw Object Missing Field '${keys1[i]}'`, v1, v2);
        } else {
          // Push common field to worklist
          worklist.push([v1[keys1[i]], v2[keys2[key2Index]]]);
        }
      }

      return Equal();
    } else {
      return NotEqual("", left, right);
    }
  }

  while (worklist.length > 0) {
    // Scaffolding to set use equalHelp()
    const result = equalHelp();
    if (!isEqual(result)) {
      return result;
    }
  }

  return Equal();
}
