import { callbackify } from "util";
import { NumericErrorCallbacks } from "./equality";

// TODO(alex): `import type` syntax is causing a parsing error
// import type { NumericErrorCallbacks } from "equality";

/*
 * 'export named-js-value' desugars into 'exports.name = js-value'
 *
 * https://stackoverflow.com/questions/16383795/difference-between-module-exports-and-exports-in-the-commonjs-module-system
 *
 */

const _NUMBER = require("./js-numbers.js");
const _EQUALITY = require('./equality.js');
const _PRIMITIVES = require("./primitives.js");



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
    console.log(`Spying "${message}" (at ${spyLoc})`);
  } else {
    console.log(`Spying (at ${spyLoc})`);
  }

  const exprs = spyObject.exprs;
  for (let i = 0; i < exprs.length; i++) {
    const key = exprs[i].key;
    const loc = exprs[i].loc;
    const value = exprs[i].expr();
    console.log(`    ${key}: ${value} (at ${loc})`);
  }
}

// *********Check Stuff*********
interface CheckResult {
  success: boolean,
  path: string,
  loc: string,
  lhs: CheckExprEvalResult,
  rhs: CheckExprEvalResult,
  exception?: any,
}

interface CheckExprEvalResult {
  value: any,
  exception: boolean,
  exception_val: any,
}

interface CheckTestResult {
  success: boolean,
  lhs: CheckExprEvalResult,
  rhs: CheckExprEvalResult,
}

var _globalCheckContext: string[] = [];
var _globalCheckResults: CheckResult[] = [];

function getCheckResults(): CheckResult[] {
  return _globalCheckResults.slice();
}

function checkResults(): CheckResult[] {
  let errorCount = 0;
  _globalCheckResults.forEach((result) => {
    if (!result.success) {
      errorCount += 1;
    }
  });

  if (errorCount === 0) {
    console.log("Looks shipshape, all tests passed, mate!");
  } else {
    console.log("Some tests failed.");
  }
  _globalCheckResults.forEach((result) => {
    let result_lhs = JSON.stringify(result.lhs, null, "\t");
    let result_rhs = JSON.stringify(result.rhs, null, "\t");
    if (result.success) {
      console.log(`[PASS] ([${result.path}], at ${result.loc})`);
    } else {
      if (result.exception !== undefined) {
        console.log(`[FAIL] Caught exception <${result.exception}>. Found <${result_lhs}>. Expected <${result_rhs}> ([${result.path}], at ${result.loc})`);

      } else {
        console.log(`[FAIL] Found <${result_lhs}>. Expected <${result_rhs}> ([${result.path}], at ${result.loc})`);
      }
    }
  });

  return getCheckResults();
}

function eagerCheckTest(lhs: () => any,  rhs: () => any,
  test: (lhs: CheckExprEvalResult, rhs: CheckExprEvalResult) => CheckTestResult,
  loc: string): void {

  let lhs_expr_eval: CheckExprEvalResult = {
    value: undefined,
    exception: false,
    exception_val: undefined,
  };

  let rhs_expr_eval: CheckExprEvalResult = {
    value: undefined,
    exception: false,
    exception_val: undefined,
  };

  try {
    lhs_expr_eval.value = lhs();
  } catch(e) {
    lhs_expr_eval.exception = true;
    lhs_expr_eval.exception_val = e;
  }

  try {
    rhs_expr_eval.value = rhs();
  } catch(e) {
    rhs_expr_eval.exception = true;
    rhs_expr_eval.exception_val = e;
  }

  try {
    let result = test(lhs_expr_eval, rhs_expr_eval);
    _globalCheckResults.push({
        success: result.success,
        path: _globalCheckContext.join(),
        loc: loc,
        lhs: result.lhs,
        rhs: result.rhs,
        exception: undefined,
    });
  } catch(e) {
    _globalCheckResults.push({
        success: false,
        path: _globalCheckContext.join(),
        loc: loc,
        lhs: lhs_expr_eval,
        rhs: rhs_expr_eval,
        exception: e,
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

var _globalTraceValues = [];

// ********* Other Functions *********
export function traceValue(loc, value) {
  // NOTE(alex): stubbed out until we decide what to actually do with it
  _globalTraceValues.push({srcloc: loc, value});
  return value;
}

function getTraces() { return _globalTraceValues; }

// Allow '+' for string concat.
// Otherwise, defer to the number library.
function customAdd(lhs: any, rhs: any, errbacks: NumericErrorCallbacks): any {
  if (typeof(lhs) === "string" && typeof(rhs) === "string") {
    return lhs + rhs;
  } else {
    return _NUMBER["add"](lhs, rhs, errbacks);
  }
}

// MUTATES an object to rebind any methods to it
function _rebind(toRebind: any): any {
  if (typeof toRebind === "object") {
    Object.keys(toRebind).forEach((key) => {
      if (key === "$brand" || key === "$tag") {
        return;
      }

      let value = toRebind[key];
      if (_PRIMITIVES.isMethod(value)) {
        toRebind[key] = value["$binder"](toRebind);
      }
    });
  }

  return toRebind;
}

export function pauseStack(callback) {
  // @ts-ignore
  return $STOPIFY.pauseK(kontinue => {
    return callback({
      resume: (val) => kontinue({ type: "normal", value: val }),
      error: (err) => kontinue({ type: "error", error: err })
    })
  });
}

const allModules = { };

function addModule(uri : string, vals : any) {
  allModules[uri] = {values: vals};
}
function getModuleValue(uri : string, k : string) {
  return allModules[uri].values[k];
}

function raise(msg: object) {
  // NOTE(alex): Changing the representation needs to be reflected in raiseExtract()
  throw msg;
}

function raiseExtract(exception: object): object {
  // NOTE(alex): Used by `raises` check operator
  //   Any changes to the `raise` exception format needs to be reflected
  //   here as well.
  return exception;
}

// NOTE(alex): stub implementation used by testing infrastructure
function torepr(v) {
  return JSON.stringify(v);
}

module.exports["addModule"] = addModule;
module.exports["getModuleValue"] = getModuleValue;


// Hack needed b/c of interactions with the 'export' keyword
// Pyret instantiates singleton data varaints by taking a reference to the value
// TODO(alex): Should Pyret perform a function call to create a singleton data variant
module.exports["Equal"] = _EQUALITY.Equal;

module.exports["NotEqual"] = _EQUALITY.NotEqual;
module.exports["Uknown"] = _EQUALITY.Unknown;

// Hack needed to match generate Pyret-code
module.exports["is-Equal"] = _EQUALITY.isEqual;
module.exports["is-NotEqual"] = _EQUALITY.isNotEqual;
module.exports["is-Unknown"] = _EQUALITY.isUnknown;

module.exports["equal-now"] = _EQUALITY.equalNow;
module.exports["equal-now3"] = _EQUALITY.equalNow3;

module.exports["equal-always"] = _EQUALITY.equalAlways;
module.exports["equal-always3"] = _EQUALITY.equalAlways3;

module.exports["identical"] = _EQUALITY.identical;
module.exports["identical3"] = _EQUALITY.identical3;

// Expected runtime functions
module.exports["raise"] = raise;
module.exports["$raiseExtract"] = raiseExtract;
module.exports["trace-value"] = traceValue;
module.exports["$getTraces"] = getTraces;

module.exports["$spy"] = _spy;
module.exports["$rebind"] = _rebind;

module.exports["$checkTest"] = eagerCheckTest;
module.exports["$checkBlock"] = eagerCheckBlockRunner;
module.exports["$checkResults"] = checkResults;
module.exports["$getCheckResults"] = getCheckResults;

module.exports["$makeRational"] = _NUMBER["makeRational"];
module.exports["$makeRoughnum"] = _NUMBER["makeRoughnum"];
module.exports["$errCallbacks"] = _EQUALITY.NumberErrbacks;

module.exports["_add"] = customAdd;
module.exports["_subtract"] = _NUMBER["subtract"];
module.exports["_multiply"] = _NUMBER["multiply"];
module.exports["_divide"] = _NUMBER["divide"];

module.exports["_lessthan"] = _NUMBER["lessThan"];
module.exports["_greaterthan"] = _NUMBER["greaterThan"];
module.exports["_lessequal"] = _NUMBER["lessThanOrEqual"];
module.exports["_greaterequal"] = _NUMBER["greaterThanOrEqual"];
module.exports["_makeNumberFromString"] = _NUMBER['fromString'];

module.exports["PTuple"] = _PRIMITIVES["PTuple"];


module.exports["$torepr"] = torepr;
