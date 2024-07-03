// TODO(alex): roughly-equals and co (not documented but found in js/base/runtime.js)

import type * as PRIMITIVES_TYPES from './primitives';
import type * as EQ from './types/equality-types';

const _NUMBER = require("./js-numbers.js");
const PRIMITIVES = require('./primitives') as typeof PRIMITIVES_TYPES;

// ********* EqualityResult Representations *********

const sharedBase_EqualityResult = { ['$methods']: {} };
const variantBase_Equal = PRIMITIVES.createVariant(sharedBase_EqualityResult, { ['$methods']: {} }, {
    ['$data']: sharedBase_EqualityResult,
    ['$name']: 'Equal',
    ['$fieldNames']: null as any
});
const variantBase_NotEqual = PRIMITIVES.createVariant(sharedBase_EqualityResult, { ['$methods']: {} }, {
    ['$data']: sharedBase_EqualityResult,
    ['$name']: 'NotEqual',
    ['$fieldNames']: [
        'reason',
        'value1',
        'value2'
    ]
});
const variantBase_Unknown = PRIMITIVES.createVariant(sharedBase_EqualityResult, { ['$methods']: {} }, {
    ['$data']: sharedBase_EqualityResult,
    ['$name']: 'Unknown',
    ['$fieldNames']: [
        'reason',
        'value1',
        'value2'
    ]
});

/* @stopify flat */
export function isEqualityResult(val : any) {
  return typeof val === 'object' && val !== null && val['$data'] === sharedBase_EqualityResult;
}
/* @stopify flat */
export function isEqual(val : any) {
  return typeof val === 'object' && val !== null && val['$variant'] === Equal.$variant;
}
const Equal : EQ.Equal = variantBase_Equal;
export { Equal };
/* @stopify flat */
export function isNotEqual(val : any) {
  return typeof val === 'object' && val !== null && val['$variant'] === variantBase_NotEqual.$variant;
}
/* @stopify flat */
export function NotEqual(reason: string, value1: any, value2: any): EQ.NotEqual {
  return PRIMITIVES.makeDataValue(variantBase_NotEqual, {
      ['reason']: reason,
      ['value1']: value1,
      ['value2']: value2
  });
}
/* @stopify flat */
export function isUnknown(val : any) {
  return typeof val === 'object' && val !== null && val['$variant'] === variantBase_Unknown.$variant;
}
/* @stopify flat */
export function Unknown(reason: string, value1: any, value2: any): EQ.Unknown {
  return PRIMITIVES.makeDataValue(variantBase_Unknown, {
      ['reason']: reason,
      ['value1']: value1,
      ['value2']: value2
  });
}

const TOL_IS_REL = true;
const TOL_IS_ABS = false;
const EQUAL_ALWAYS = true;
const EQUAL_NOW = false;
const FROM_WITHIN = true;

const numericEquals: (v1: any, v2: any, callbacks: NumericErrorCallbacks) => boolean = _NUMBER["equals"];


// ********* Helpers *********
function equalityResultToBool(ans: EQ.EqualityResult): boolean {
  if (isEqual(ans)) {
    return true;
  } else if (isNotEqual(ans)) {
    return false;
  } else if (isUnknown(ans)) {
    let unknownVariant = ans as EQ.Unknown;
    throw {
      reason: unknownVariant.reason,
      value1: unknownVariant.value1,
      value2: unknownVariant.value2,
    };
  }
  else {
    throw "Matched none of Equal, NotEqual, or Unknown in equalityResultToBool";
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
export function identical3(v1: any, v2: any): EQ.EqualityResult {
  if (PRIMITIVES.isFunction(v1) && PRIMITIVES.isFunction(v2)) {
    return Unknown("Function", v1, v2);
  } else if (PRIMITIVES.isMethod(v1) && PRIMITIVES.isMethod(v2)) {
    return Unknown("Method", v1, v2);
  } else if (PRIMITIVES.isRoughNumber(v1) && PRIMITIVES.isRoughNumber(v2)) {
    return Unknown('Roughnums', v1,  v2);
  } else if (v1 === v2) {
    return Equal;
  } else {
    return NotEqual("", v1, v2);
  }
}

export function identical(v1: any, v2: any): boolean {
  let ans: EQ.EqualityResult = identical3(v1, v2);
  return equalityResultToBool(ans);
}

export function equalNow(v1: any, v2: any): boolean {
  let ans: EQ.EqualityResult = equalNow3(v1, v2);
  return equalityResultToBool(ans);
}

export function equalNow3(v1: any, v2: any): EQ.EqualityResult {
  return equalCore3(v1, v2, EQUAL_NOW, 0, TOL_IS_ABS, false);
}

/*
 * Structural equality. Stops at mutable data (refs) and only checks that
 * mutable data are identical.
 *
 * Data variants and raw (unbranded) objects are NEVER equal.
 *
 */
export function equalAlways3(v1: any, v2: any): EQ.EqualityResult {
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
export function equal_and(er1: EQ.EqualityResult, er2: EQ.EqualityResult): EQ.EqualityResult {
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
export function equal_or(er1: EQ.EqualityResult, er2: EQ.EqualityResult): EQ.EqualityResult {
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
export function to_boolean(er: EQ.EqualityResult): boolean {
    if (isUnknown(er)) {
        // TODO(alex): Fill this in with the generic `raise` function
        //   CANNOT IMPORT "global.arr.js" OR "runtime.ts" directly b/c the circular depenency
        //   will mess with loading...
        throw new Error("Unable to convert Unknown (EqualityResult) to boolean");
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

export function withinRel(tolerance : any) {
  const inner = withinRel3(tolerance);
  return function(l : any, r : any): boolean {
    return to_boolean(inner(l, r));
  };
}

export function withinAbs(tolerance : any) {
  const inner = withinAbs3(tolerance);
  return function(l : any, r : any): boolean {
    return to_boolean(inner(l, r));
  };
}

export function withinRelNow(tolerance : any) {
  const inner = withinRelNow3(tolerance);
  return function(l : any, r : any): boolean {
    return to_boolean(inner(l, r));
  };
}

export function withinAbsNow(tolerance : any) {
  const inner = withinAbsNow3(tolerance);
  return function(l : any, r : any): boolean {
    return to_boolean(inner(l, r));
  };
}

/// within-*3
export function withinRel3(tolerance : any) {
  return function(l : any, r : any) {
    return equalCore3(l, r, EQUAL_ALWAYS, tolerance, TOL_IS_REL, FROM_WITHIN);
  };
}

export function withinAbs3(tolerance : any) {
  return function(l : any, r : any) {
    return equalCore3(l, r, EQUAL_ALWAYS, tolerance, TOL_IS_ABS, FROM_WITHIN);
  };
}

export function withinRelNow3(tolerance : any) {
  return function(l : any, r : any) {
    return equalCore3(l, r, EQUAL_NOW, tolerance, TOL_IS_REL, FROM_WITHIN);
  };
}

export function withinAbsNow3(tolerance : any) {
  return function(l : any, r : any) {
    return equalCore3(l, r, EQUAL_NOW, tolerance, TOL_IS_ABS, FROM_WITHIN);
  };
}

const ROUGH_TOL = _NUMBER.fromFixnum(0.000001, NumberErrbacks);
export function roughlyEqualAlways3(left : any, right : any) {
  return equalCore3(left, right, EQUAL_ALWAYS, ROUGH_TOL, TOL_IS_REL, /*fromWithin?*/false);
};
export function roughlyEqualAlways(v1 : any, v2 : any) {
  if (((typeof v1 === 'number')  && (typeof v2 === 'number')) ||
      ((typeof v1 === 'string')  && (typeof v2 === 'string')) ||
      ((typeof v1 === 'boolean') && (typeof v2 === 'boolean'))) {
    return v1 === v2;
  }
  return equalCore3(v1, v2, EQUAL_ALWAYS, ROUGH_TOL, TOL_IS_REL, /*fromWithin?*/false);
};
export function roughlyEqualNow3(left : any, right : any) {
  return equalCore3(left, right, EQUAL_NOW, ROUGH_TOL, TOL_IS_REL, /*fromWithin?*/false);
};
export function roughlyEqualNow(v1 : any, v2 : any) {
  return equalCore3(v1, v2, EQUAL_NOW, ROUGH_TOL, TOL_IS_REL, /*fromWithin?*/false);
};

export function _lessthan(lhs: any, rhs: any): boolean {
    // Check if object has a '<' custom implementation
    if ((typeof lhs === "object") && ("_lessthan" in lhs)) {
        return lhs._lessthan(rhs);
    } else if (PRIMITIVES.isString(lhs) && PRIMITIVES.isString(rhs)) {
        // NOTE: JS uses lexicographical order
        return lhs < rhs;
    } else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.lessThan(lhs, rhs);
    } else {
        // NOTE: may be a dynamic error
        try {
            return lhs < rhs;
        } catch (error) {
            throw new Error(`Unable to perform '<' on (${lhs}) and (${rhs})`);
        }
    }
}

export function _greaterthan(lhs: any, rhs: any): boolean {
    // Check if object has a '>' custom implementation
    if ((typeof lhs === "object") && ("_greaterthan" in lhs)) {
        return lhs._greaterthan(rhs);
    } else if (PRIMITIVES.isString(lhs) && PRIMITIVES.isString(rhs)) {
        // NOTE: JS uses lexicographical order
        return lhs > rhs;
    } else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.greaterThan(lhs, rhs);
    } else {
        // NOTE: may be a dynamic error
        try {
            return lhs > rhs;
        } catch (error) {
            throw new Error(`Unable to perform '>' on (${lhs}) and (${rhs})`);
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
function equalCore3(left: any, right: any, alwaysFlag: boolean, tol: number, rel: boolean, fromWithin: boolean): EQ.EqualityResult {

  if(tol === undefined) { // means that we aren't doing any kind of within
    const isIdentical = identical3(left, right);
    if (!isNotEqual(isIdentical)) {
      return isIdentical;   // Equal or Unknown
    }
  }

  // TODO(alex): handle cycles
  // TODO(alex): Result caching
  let worklist: [any, any][] = [[left, right]];

  function worklistPush(v1: any, v2: any) {
    worklist.push([v1, v2]);
  }

  function worklistPop(): [any, any] {
    return worklist.pop()!;
  }

  function equalRec(l: any, r: any): EQ.EqualityResult {
    const result = equalCore3(l, r, alwaysFlag, tol, rel, fromWithin);
    return result;
  }

  // Actual equality implementation on individual items
  function equalHelp(v1: any, v2: any): EQ.EqualityResult {

    if (isEqual(identical3(v1, v2))) {
      // Identical so must always be equal
      return Equal;
    }

    if (PRIMITIVES.isNumber(v1) && PRIMITIVES.isNumber(v2)) {

      if (tol) {
        if (rel) {
          if (_NUMBER["roughlyEqualsRel"](v1, v2, tol, NumberErrbacks)) {
            return Equal;
          } else {
            return NotEqual("", v1, v2);
          }
        } else if (_NUMBER["roughlyEquals"](v1, v2, tol, NumberErrbacks)) {
          return Equal;
        } else {
          return NotEqual("", v1, v2);
        }
      } else if (PRIMITIVES.isRoughNumber(v1) || PRIMITIVES.isRoughNumber(v2)) {
        return Unknown("Rough Number equal-always", v1, v2);
      } else if (numericEquals(v1, v2, NumberErrbacks)) {
        return Equal;
      } else {
        return NotEqual("Numbers", v1, v2);
      }

    } else if (PRIMITIVES.isBoolean(v1) && PRIMITIVES.isBoolean(v2)) {
      if (v1 !== v2) { return NotEqual("Booleans", v1, v2); }
      return Equal;

    } else if (PRIMITIVES.isString(v1) && PRIMITIVES.isString(v2)) {
      if (v1 !== v2) { return NotEqual("Strings", v1, v2); }
      return Equal;

    } else if (PRIMITIVES.isFunction(v1) && PRIMITIVES.isFunction(v2)) {
      // Cannot compare functions for equality
      return Unknown("Functions", v1, v2);
    } else if (PRIMITIVES.isMethod(v1) && PRIMITIVES.isMethod(v2)) {
      return Unknown("Methods", v1, v2);
    } else if (PRIMITIVES.isPTuple(v1) && PRIMITIVES.isPTuple(v2)) {
      if (v1.length !== v2.length) {
        return NotEqual("PTuple Length", v1, v2);
      }

      for (var i = 0; i < v1.length; i++) {
        worklistPush(v1[i], v2[i]);
      }
      return Equal;

    } else if (PRIMITIVES.isArray(v1) && PRIMITIVES.isArray(v2)) {
      if (alwaysFlag || v1.length !== v2.length) {
        return NotEqual("Array Length", v1, v2);
      }

      for (var i = 0; i < v1.length; i++) {
        worklistPush(v1[i], v2[i]);
      }
      return Equal;

    } else if (PRIMITIVES.isNothing(v1) && PRIMITIVES.isNothing(v2)) {
      // Equality is defined for 'nothing'
      // 'nothing' is always equal to 'nothing'
      return Equal;

    } else if (PRIMITIVES.isPRef(v1) && PRIMITIVES.isPRef(v2)) {
      // In equal-always, non-identical refs are not equal
      // TODO: handle alwaysFlag
      if (v1.ref !== v2.ref) {
        return NotEqual("PRef'd Objects", v1, v2);
      }

      return Equal;

    } else if (PRIMITIVES.isTable(v1) && PRIMITIVES.isTable(v2)) {
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
                worklistPush(v1_row[i], v2_row[i]);
            }
        }

        return Equal;
    } else if (PRIMITIVES.isRow(v1) && PRIMITIVES.isRow(v2)) {

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
            worklistPush(v1[i], v2[i]);
        }

        return Equal;

    } else if (PRIMITIVES.isDataVariant(v1) && PRIMITIVES.isDataVariant(v2)) {
      if(v1.$variant && v1.$variant === v2.$variant) {
        if ("_equals" in v1) {
          //   NOTE(alex): CURRENTLY FAILS WITH CYCLIC STRUCTURES
          var ans = v1["_equals"](v2, equalRec);

          return ans;
        }

        var fields1 = v1.$fieldNames;
        var fields2 = v2.$fieldNames;

        for(var i = 0; i < fields1.length; i += 1) {
          worklistPush(v1[fields1[i]], v2[fields2[i]]);
        }
        return Equal;

      } else {
        return NotEqual("Variant Brands", v1, v2);
      }
    } else if (PRIMITIVES.isRawObject(v1) && PRIMITIVES.isRawObject(v2)) {
      if ("_equals" in v1) {
        //   NOTE(alex): CURRENTLY FAILS WITH CYCLIC STRUCTURES
        var ans = v1["_equals"](v2, equalRec);

        return ans;
      }
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
          worklistPush(v1[keys1[i]], v2[keys2[key2Index]]);
        }
      }

      return Equal;
    } else {
      return NotEqual("", left, right);
    }
  }

  while (worklist.length > 0) {
    // Scaffolding to set use equalHelp()
    const curr = worklistPop();
    const result = equalHelp(curr[0], curr[1]);
    if (!isEqual(result)) {
      return result;
    }
  }

  return Equal;
}
