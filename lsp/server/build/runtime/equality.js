"use strict";
// TODO(alex): roughly-equals and co (not documented but found in js/base/runtime.js)
var _a, _b, _c, _d, _e, _f, _g;
exports.__esModule = true;
exports._greaterequal = exports._lessequal = exports._greaterthan = exports._lessthan = exports.withinAbsNow3 = exports.withinRelNow3 = exports.withinAbs3 = exports.withinRel3 = exports.withinAbsNow = exports.withinRelNow = exports.withinAbs = exports.withinRel = exports.withinNow3 = exports.within3 = exports.withinNow = exports.within = exports.to_boolean = exports.equal_or = exports.equal_and = exports.equalAlways = exports.equalAlways3 = exports.equalNow3 = exports.equalNow = exports.identical = exports.identical3 = exports.NumberErrbacks = exports.Unknown = exports.isUnknown = exports.NotEqual = exports.isNotEqual = exports.Equal = exports.isEqual = exports.isEqualityResult = void 0;
var _NUMBER = require("./js-numbers.js");
var PRIMITIVES = require('./primitives');
// ********* EqualityResult Representations *********
var sharedBase_EqualityResult = (_a = {}, _a['$methods'] = {}, _a);
var variantBase_Equal = PRIMITIVES.createVariant(sharedBase_EqualityResult, (_b = {}, _b['$methods'] = {}, _b), (_c = {},
    _c['$data'] = sharedBase_EqualityResult,
    _c['$name'] = 'Equal',
    _c['$fieldNames'] = null,
    _c));
var variantBase_NotEqual = PRIMITIVES.createVariant(sharedBase_EqualityResult, (_d = {}, _d['$methods'] = {}, _d), (_e = {},
    _e['$data'] = sharedBase_EqualityResult,
    _e['$name'] = 'NotEqual',
    _e['$fieldNames'] = [
        'reason',
        'value1',
        'value2'
    ],
    _e));
var variantBase_Unknown = PRIMITIVES.createVariant(sharedBase_EqualityResult, (_f = {}, _f['$methods'] = {}, _f), (_g = {},
    _g['$data'] = sharedBase_EqualityResult,
    _g['$name'] = 'Unknown',
    _g['$fieldNames'] = [
        'reason',
        'value1',
        'value2'
    ],
    _g));
/* @stopify flat */
function isEqualityResult(val) {
    return typeof val === 'object' && val !== null && val['$data'] === sharedBase_EqualityResult;
}
exports.isEqualityResult = isEqualityResult;
/* @stopify flat */
function isEqual(val) {
    return typeof val === 'object' && val !== null && val['$variant'] === Equal.$variant;
}
exports.isEqual = isEqual;
var Equal = variantBase_Equal;
exports.Equal = Equal;
/* @stopify flat */
function isNotEqual(val) {
    return typeof val === 'object' && val !== null && val['$variant'] === variantBase_NotEqual.$variant;
}
exports.isNotEqual = isNotEqual;
/* @stopify flat */
function NotEqual(reason, value1, value2) {
    var _a;
    return PRIMITIVES.makeDataValue(variantBase_NotEqual, (_a = {},
        _a['reason'] = reason,
        _a['value1'] = value1,
        _a['value2'] = value2,
        _a));
}
exports.NotEqual = NotEqual;
/* @stopify flat */
function isUnknown(val) {
    return typeof val === 'object' && val !== null && val['$variant'] === variantBase_Unknown.$variant;
}
exports.isUnknown = isUnknown;
/* @stopify flat */
function Unknown(reason, value1, value2) {
    var _a;
    return PRIMITIVES.makeDataValue(variantBase_Unknown, (_a = {},
        _a['reason'] = reason,
        _a['value1'] = value1,
        _a['value2'] = value2,
        _a));
}
exports.Unknown = Unknown;
var TOL_IS_REL = true;
var TOL_IS_ABS = false;
var EQUAL_ALWAYS = true;
var EQUAL_NOW = false;
var FROM_WITHIN = true;
var numericEquals = _NUMBER["equals"];
// ********* Helpers *********
function equalityResultToBool(ans) {
    if (isEqual(ans)) {
        return true;
    }
    else if (isNotEqual(ans)) {
        return false;
    }
    else if (isUnknown(ans)) {
        var unknownVariant = ans;
        throw {
            reason: unknownVariant.reason,
            value1: unknownVariant.value1,
            value2: unknownVariant.value2
        };
    }
}
var NumberErrbacks = {
    throwDivByZero: function (msg) { throw msg; },
    throwToleranceError: function (msg) { throw msg; },
    throwRelToleranceError: function (msg) { throw msg; },
    throwGeneralError: function (msg) { throw msg; },
    throwDomainError: function (msg) { throw msg; },
    throwSqrtNegative: function (msg) { throw msg; },
    throwLogNonPositive: function (msg) { throw msg; },
    throwIncomparableValues: function (msg) { throw msg; },
    throwInternalError: function (msg) { throw msg; }
};
exports.NumberErrbacks = NumberErrbacks;
// ********* Equality Functions *********
function identical3(v1, v2) {
    if (PRIMITIVES.isFunction(v1) && PRIMITIVES.isFunction(v2)) {
        return Unknown("Function", v1, v2);
    }
    else if (PRIMITIVES.isMethod(v1) && PRIMITIVES.isMethod(v2)) {
        return Unknown("Method", v1, v2);
    }
    else if (PRIMITIVES.isRoughNumber(v1) && PRIMITIVES.isRoughNumber(v2)) {
        return Unknown('Roughnums', v1, v2);
    }
    else if (v1 === v2) {
        return Equal;
    }
    else {
        return NotEqual("", v1, v2);
    }
}
exports.identical3 = identical3;
function identical(v1, v2) {
    var ans = identical3(v1, v2);
    return equalityResultToBool(ans);
}
exports.identical = identical;
function equalNow(v1, v2) {
    var ans = equalNow3(v1, v2);
    return equalityResultToBool(ans);
}
exports.equalNow = equalNow;
function equalNow3(v1, v2) {
    return equalCore3(v1, v2, EQUAL_NOW, 0, TOL_IS_ABS, false);
}
exports.equalNow3 = equalNow3;
/*
 * Structural equality. Stops at mutable data (refs) and only checks that
 * mutable data are identical.
 *
 * Data variants and raw (unbranded) objects are NEVER equal.
 *
 */
function equalAlways3(v1, v2) {
    return equalCore3(v1, v2, EQUAL_ALWAYS, 0, TOL_IS_ABS, false);
}
exports.equalAlways3 = equalAlways3;
function equalAlways(v1, v2) {
    var ans = equalAlways3(v1, v2);
    return equalityResultToBool(ans);
}
exports.equalAlways = equalAlways;
//fun equal-and(er1 :: EqualityResult, er2 :: EqualityResult):
//  ask:
//    | is-NotEqual(er1) then: er1
//    | is-NotEqual(er2) then: er2
//    | is-Unknown(er1) then: er1 #: i.e., the first Unknown
//    | otherwise: er2 # Equal or Equal/Equal or Unknown
//  end
//end
function equal_and(er1, er2) {
    if (isNotEqual(er1)) {
        return er1;
    }
    else if (isNotEqual(er2)) {
        return er2;
    }
    else if (isUnknown(er1)) {
        return er1;
    }
    else {
        return er2;
    }
}
exports.equal_and = equal_and;
//fun equal-or(er1 :: EqualityResult, er2 :: EqualityResult):
//  ask:
//    | is-Equal(er1) then: er1
//    | is-Equal(er2) then: er2
//    | is-Unknown(er1) then: er1 # i.e., the first Unknown
//    | otherwise: er2 # NotEqual or NotEqual/NotEqual or Unknown
//  end
//end
function equal_or(er1, er2) {
    if (isEqual(er1)) {
        return er1;
    }
    else if (isEqual(er2)) {
        return er2;
    }
    else if (isUnknown(er1)) {
        return er1;
    }
    else {
        return er2;
    }
}
exports.equal_or = equal_or;
//fun to-boolean(er :: EqualityResult):
//  cases(EqualityResult) er:
//    | Unknown(r, v1, v2) => raise(error.equality-failure(r, v1, v2))
//    | Equal => true
//    | NotEqual(_,_,_) => false
//  end
//end
function to_boolean(er) {
    if (isUnknown(er)) {
        // TODO(alex): Fill this in with the generic `raise` function
        //   CANNOT IMPORT "global.arr.js" OR "runtime.ts" directly b/c the circular depenency
        //   will mess with loading...
        throw new Error("Unable to convert Unknown (EqualityResult) to boolean");
    }
    else {
        return isEqual(er);
    }
}
exports.to_boolean = to_boolean;
/// within-*
var within = withinRel;
exports.within = within;
var withinNow = withinRelNow;
exports.withinNow = withinNow;
var within3 = withinRel3;
exports.within3 = within3;
var withinNow3 = withinRelNow3;
exports.withinNow3 = withinNow3;
function withinRel(tolerance) {
    var inner = withinRel3(tolerance);
    return function (l, r) {
        return to_boolean(inner(l, r));
    };
}
exports.withinRel = withinRel;
function withinAbs(tolerance) {
    var inner = withinAbs3(tolerance);
    return function (l, r) {
        return to_boolean(inner(l, r));
    };
}
exports.withinAbs = withinAbs;
function withinRelNow(tolerance) {
    var inner = withinRelNow3(tolerance);
    return function (l, r) {
        return to_boolean(inner(l, r));
    };
}
exports.withinRelNow = withinRelNow;
function withinAbsNow(tolerance) {
    var inner = withinAbsNow3(tolerance);
    return function (l, r) {
        return to_boolean(inner(l, r));
    };
}
exports.withinAbsNow = withinAbsNow;
/// within-*3
function withinRel3(tolerance) {
    return function (l, r) {
        return equalCore3(l, r, EQUAL_ALWAYS, tolerance, TOL_IS_REL, FROM_WITHIN);
    };
}
exports.withinRel3 = withinRel3;
function withinAbs3(tolerance) {
    return function (l, r) {
        return equalCore3(l, r, EQUAL_ALWAYS, tolerance, TOL_IS_ABS, FROM_WITHIN);
    };
}
exports.withinAbs3 = withinAbs3;
function withinRelNow3(tolerance) {
    return function (l, r) {
        return equalCore3(l, r, EQUAL_NOW, tolerance, TOL_IS_REL, FROM_WITHIN);
    };
}
exports.withinRelNow3 = withinRelNow3;
function withinAbsNow3(tolerance) {
    return function (l, r) {
        return equalCore3(l, r, EQUAL_NOW, tolerance, TOL_IS_ABS, FROM_WITHIN);
    };
}
exports.withinAbsNow3 = withinAbsNow3;
function _lessthan(lhs, rhs) {
    // Check if object has a '<' custom implementation
    if ((typeof lhs === "object") && ("_lessthan" in lhs)) {
        return lhs._lessthan(rhs);
    }
    else if (PRIMITIVES.isString(lhs) && PRIMITIVES.isString(rhs)) {
        // NOTE: JS uses lexicographical order
        return lhs < rhs;
    }
    else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.lessThan(lhs, rhs);
    }
    else {
        // NOTE: may be a dynamic error
        try {
            return lhs < rhs;
        }
        catch (error) {
            throw new Error("Unable to perform '<' on (" + lhs + ") and (" + rhs + ")");
        }
    }
}
exports._lessthan = _lessthan;
function _greaterthan(lhs, rhs) {
    // Check if object has a '>' custom implementation
    if ((typeof lhs === "object") && ("_greaterthan" in lhs)) {
        return lhs._greaterthan(rhs);
    }
    else if (PRIMITIVES.isString(lhs) && PRIMITIVES.isString(rhs)) {
        // NOTE: JS uses lexicographical order
        return lhs > rhs;
    }
    else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.greaterThan(lhs, rhs);
    }
    else {
        // NOTE: may be a dynamic error
        try {
            return lhs > rhs;
        }
        catch (error) {
            throw new Error("Unable to perform '>' on (" + lhs + ") and (" + rhs + ")");
        }
    }
}
exports._greaterthan = _greaterthan;
function _lessequal(lhs, rhs) {
    return equalAlways(lhs, rhs) || _lessthan(lhs, rhs);
}
exports._lessequal = _lessequal;
function _greaterequal(lhs, rhs) {
    return equalAlways(lhs, rhs) || _greaterthan(lhs, rhs);
}
exports._greaterequal = _greaterequal;
// tol and rel are PyretNumber's
function equalCore3(left, right, alwaysFlag, tol, rel, fromWithin) {
    if (tol === undefined) { // means that we aren't doing any kind of within
        var isIdentical = identical3(left, right);
        if (!isNotEqual(isIdentical)) {
            return isIdentical; // Equal or Unknown
        }
    }
    // TODO(alex): handle cycles
    // TODO(alex): Result caching
    var worklist = [[left, right]];
    function worklistPush(v1, v2) {
        worklist.push([v1, v2]);
    }
    function worklistPop() {
        return worklist.pop();
    }
    function equalRec(l, r) {
        return equalHelp(l, r);
    }
    // Actual equality implementation on individual items
    function equalHelp(v1, v2) {
        if (isEqual(identical3(v1, v2))) {
            // Identical so must always be equal
            return Equal;
        }
        if (PRIMITIVES.isNumber(v1) && PRIMITIVES.isNumber(v2)) {
            if (tol) {
                if (rel) {
                    if (_NUMBER["roughlyEqualsRel"](v1, v2, tol, NumberErrbacks)) {
                        return Equal;
                    }
                    else {
                        return NotEqual("", v1, v2);
                    }
                }
                else if (_NUMBER["roughlyEquals"](v1, v2, tol, NumberErrbacks)) {
                    return Equal;
                }
                else {
                    return NotEqual("", v1, v2);
                }
            }
            else if (PRIMITIVES.isRoughNumber(v1) || PRIMITIVES.isRoughNumber(v2)) {
                return Unknown("Rough Number equal-always", v1, v2);
            }
            else if (numericEquals(v1, v2, NumberErrbacks)) {
                return Equal;
            }
            else {
                return NotEqual("Numbers", v1, v2);
            }
        }
        else if (PRIMITIVES.isBoolean(v1) && PRIMITIVES.isBoolean(v2)) {
            if (v1 !== v2) {
                return NotEqual("Booleans", v1, v2);
            }
            return Equal;
        }
        else if (PRIMITIVES.isString(v1) && PRIMITIVES.isString(v2)) {
            if (v1 !== v2) {
                return NotEqual("Strings", v1, v2);
            }
            return Equal;
        }
        else if (PRIMITIVES.isFunction(v1) && PRIMITIVES.isFunction(v2)) {
            // Cannot compare functions for equality
            return Unknown("Functions", v1, v2);
        }
        else if (PRIMITIVES.isMethod(v1) && PRIMITIVES.isMethod(v2)) {
            return Unknown("Methods", v1, v2);
        }
        else if (PRIMITIVES.isPTuple(v1) && PRIMITIVES.isPTuple(v2)) {
            if (v1.length !== v2.length) {
                return NotEqual("PTuple Length", v1, v2);
            }
            for (var i = 0; i < v1.length; i++) {
                worklistPush(v1[i], v2[i]);
            }
            return Equal;
        }
        else if (PRIMITIVES.isArray(v1) && PRIMITIVES.isArray(v2)) {
            if (alwaysFlag || v1.length !== v2.length) {
                return NotEqual("Array Length", v1, v2);
            }
            for (var i = 0; i < v1.length; i++) {
                worklistPush(v1[i], v2[i]);
            }
            return Equal;
        }
        else if (PRIMITIVES.isNothing(v1) && PRIMITIVES.isNothing(v2)) {
            // Equality is defined for 'nothing'
            // 'nothing' is always equal to 'nothing'
            return Equal;
        }
        else if (PRIMITIVES.isPRef(v1) && PRIMITIVES.isPRef(v2)) {
            // In equal-always, non-identical refs are not equal
            // TODO: handle alwaysFlag
            if (v1.ref !== v2.ref) {
                return NotEqual("PRef'd Objects", v1, v2);
            }
            return Equal;
        }
        else if (PRIMITIVES.isTable(v1) && PRIMITIVES.isTable(v2)) {
            var v1_headers = v1._headers;
            var v2_headers = v2._headers;
            if (v1_headers.length !== v2_headers.length) {
                return NotEqual("Row Header Length (" + v1_headers.length + ", " + v2_headers.length + ")", v1, v2);
            }
            if (v1._rows.length !== v2._rows.length) {
                return NotEqual("Row Length (" + v1._rows.length + ", " + v2._rows.length + ")", v1, v2);
            }
            // Check table headers
            for (var h = 0; h < v1_headers.length; h++) {
                if (v1_headers[h] !== v2_headers[h]) {
                    return NotEqual("Row headers (index " + h + ")", v1, v2);
                }
            }
            // Check row elements
            for (var row = 0; row < v1._rows.length; row++) {
                var v1_row = v1._rows[row];
                var v2_row = v2._rows[row];
                for (var i_1 = 0; i_1 < v1_row.length; i_1++) {
                    worklistPush(v1_row[i_1], v2_row[i_1]);
                }
            }
            return Equal;
        }
        else if (PRIMITIVES.isRow(v1) && PRIMITIVES.isRow(v2)) {
            var v1_headers = v1._headers;
            var v2_headers = v2._headers;
            if (v1_headers.length !== v2_headers.length) {
                return NotEqual("Row Header Length (" + v1_headers.length + ", " + v2_headers.length + ")", v1, v2);
            }
            // TODO(alex): is this check necessary?
            if (v1._elements.length !== v2._elements.length) {
                return NotEqual("Row Elements Length (" + v1._elements.length + ", " + v1._elements.length + ")", v1, v2);
            }
            for (var i_2 = 0; i_2 < v1_headers.length; i_2++) {
                if (v1_headers[i_2] !== v2_headers[i_2]) {
                    return NotEqual("Row headers (index " + i_2 + ")", v1, v2);
                }
            }
            for (var i_3 = 0; i_3 < v1._elements.length; i_3++) {
                worklistPush(v1[i_3], v2[i_3]);
            }
            return Equal;
        }
        else if (PRIMITIVES.isDataVariant(v1) && PRIMITIVES.isDataVariant(v2)) {
            if (v1.$variant && v1.$variant === v2.$variant) {
                if ("_equals" in v1) {
                    //   NOTE(alex): CURRENTLY FAILS WITH CYCLIC STRUCTURES
                    var ans = v1["_equals"](v2, equalRec);
                    return ans;
                }
                var fields1 = v1.$fieldNames;
                var fields2 = v2.$fieldNames;
                for (var i = 0; i < fields1.length; i += 1) {
                    worklistPush(v1[fields1[i]], v2[fields2[i]]);
                }
                return Equal;
            }
            else {
                return NotEqual("Variant Brands", v1, v2);
            }
        }
        else if (PRIMITIVES.isRawObject(v1) && PRIMITIVES.isRawObject(v2)) {
            if ("_equals" in v1) {
                //   NOTE(alex): CURRENTLY FAILS WITH CYCLIC STRUCTURES
                var ans = v1["_equals"](v2, equalRec);
                return ans;
            }
            var keys1 = Object.keys(v1);
            var keys2 = Object.keys(v2);
            if (keys1.length !== keys2.length) {
                return NotEqual("Raw Object Field Count", v1, v2);
            }
            // Check for matching keys and push field to worklist
            for (var i = 0; i < keys1.length; i++) {
                var key2Index = keys2.indexOf(keys1[i]);
                if (key2Index === -1) {
                    // Key in v1 not found in v2
                    return NotEqual("Raw Object Missing Field '" + keys1[i] + "'", v1, v2);
                }
                else {
                    // Push common field to worklist
                    worklistPush(v1[keys1[i]], v2[keys2[key2Index]]);
                }
            }
            return Equal;
        }
        else {
            return NotEqual("", left, right);
        }
    }
    while (worklist.length > 0) {
        // Scaffolding to set use equalHelp()
        var curr = worklistPop();
        var result = equalHelp(curr[0], curr[1]);
        if (!isEqual(result)) {
            return result;
        }
    }
    return Equal;
}
