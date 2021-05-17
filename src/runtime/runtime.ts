import { callbackify } from "util";
import { NumericErrorCallbacks } from "./equality";
import { CheckResult, CheckExprEvalResult, CheckTestResult } from "./common-runtime-types";

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

var $spyMessageHandler = function(data) {
  if (data.message) {
    console.log(`Spying "${data.message}" (at ${data.loc})`);
  } else {
    console.log(`Spying (at ${data.loc})`);
  }
};

var $spyValueHandler = function(data) {
  console.log(`    ${data.key}: ${data.value} (at ${data.loc})`);
};

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

export function $setSpyMessageHandler(handler) {
  $spyMessageHandler = handler;
}

export function $setSpyValueHandler(handler) {
  $spyValueHandler = handler;
}

function _not(x: boolean): boolean { return !x; }

function _spy(spyObject: SpyObject): void {

  const message = spyObject.message();
  const spyLoc = spyObject.loc;
  if ($spyMessageHandler) {
    $spyMessageHandler({ message: message, loc: spyLoc });
  }

  const exprs = spyObject.exprs;
  for (let i = 0; i < exprs.length; i++) {
    const key = exprs[i].key;
    const loc = exprs[i].loc;
    const value = exprs[i].expr();
    if ($spyValueHandler) {
      $spyValueHandler({ key: key, value: value, loc: loc });
    }
  }
}

// *********Check Stuff*********
var _globalCheckContext: string[] = [];
var _globalCheckResults: CheckResult[] = [];
// TODO: Pass in the URI to the check test executors
//   so we can attempt to filter check blocks by module
// TODO: Add check test override
// TODO: Need to expose an check runner test API to the IDE
var $checkBlockExecutor = eagerCheckBlockRunner;
var $checkBlockFilter: (srcloc: string, name: string) => boolean | null = null;

export function $setCheckBlockFilter(filter: (srcloc: string, name: string) => boolean): void {
  $checkBlockFilter = filter;
}

export function $setCheckBlockExecutor(executor): void {
  $checkBlockExecutor = executor;
}

function checkBlockHandler(srcloc: string, name: string, checkBlock: () => void): void {
  $checkBlockExecutor(srcloc, name, checkBlock);
}

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
      if (result.exception) {
        console.log(`[FAIL] Caught exception <${result.exception.toString()}>. Found <${result_lhs}>. Expected <${result_rhs}> ([${result.path}], at ${result.loc})`);

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

// TODO(alex): Common URI object that's not a string
function eagerCheckBlockRunner(uri: string, name: string, checkBlock: () => void): void {
  if ($checkBlockFilter && !$checkBlockFilter(uri, name)) {
    return;
  }

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
function customPlus(lhs: any, rhs: any, errbacks: NumericErrorCallbacks): any {
    if ((typeof lhs === "object") && ("_plus" in lhs)) {
        return lhs._plus(rhs);
    } else if (_PRIMITIVES.isString(lhs) && _PRIMITIVES.isString(rhs)) {
        return lhs + rhs;
    } else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.add(lhs, rhs, errbacks);
    } else {
        // NOTE: may be a dynamic error
        try {
            return lhs + rhs;
        } catch (error) {
            throw new Error(`Unable to perform '+' on (${lhs}) and (${rhs})`);
        }
    }
}

function customMinus(lhs: any, rhs: any, errbacks: NumericErrorCallbacks): any {
    if ((typeof lhs === "object") && ("_minus" in lhs)) {
        return lhs._minus(rhs);
    } else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.subtract(lhs, rhs, errbacks);
    } else {
        // NOTE: may be a dynamic error
        try {
            return lhs - rhs;
        } catch (error) {
            throw new Error(`Unable to perform '-' on (${lhs}) and (${rhs})`);
        }
    }
}

function customTimes(lhs: any, rhs: any, errbacks: NumericErrorCallbacks): any {
    if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.multiply(lhs, rhs, errbacks);
    } else if ((typeof lhs === "object") && ("_times" in lhs)) {
        return lhs._times(rhs);
    } else {
        // NOTE: may be a dynamic error
        try {
            return lhs * rhs;
        } catch (error) {
            throw new Error(`Unable to perform '*' on (${lhs}) and (${rhs})`);
        }
    }
}

function customDivide(lhs: any, rhs: any, errbacks: NumericErrorCallbacks): any {
    if ((typeof lhs === "object") && ("_divide" in lhs)) {
        return lhs._divide(rhs);
    } else if (_NUMBER.isPyretNumber(lhs) && _NUMBER.isPyretNumber(rhs)) {
        return _NUMBER.divide(lhs, rhs, errbacks);
    } else {
        // NOTE: may be a dynamic error
        try {
            return lhs / rhs;
        } catch (error) {
            throw new Error(`Unable to perform '/' on (${lhs}) and (${rhs})`);
        }
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

// NOTE(alex): Handles method rebinding
function shallowCopyObject(myObject: any): any {
  let shallowCopy = Object.assign({}, myObject);
  return _rebind(shallowCopy);
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

// TODO(alex): common Pyret error objects
function raise(msg: object) {
  // NOTE(alex): Changing the representation needs to be reflected in raiseExtract()
  throw msg;
}

function raiseExtract(exception: any): string {
  // NOTE(alex): Used by `raises` check operator
  //   Any changes to the `raise` exception format needs to be reflected
  //   here as well.
  return exception.toString();
}

// NOTE(alex): stub implementation used by testing infrastructure
function torepr(v) {
  return JSON.stringify(v);
}

function customThrow(exn) {
  exn.toString = function() { return JSON.stringify(this); }
  throw new Error(exn);
}

let imageUrlProxyWrapper = function(url: string): string {
  return url;
}

export function $setImgUrlProxyWrapper(wrapperFn: (url: string) => string) {
  imageUrlProxyWrapper = wrapperFn;
}

export function $imgUrlProxy(url: string): string {
  return imageUrlProxyWrapper(url);
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
module.exports["$shallowCopyObject"] = shallowCopyObject;

module.exports["$checkTest"] = eagerCheckTest;
module.exports["$checkBlock"] = checkBlockHandler;
module.exports["$checkResults"] = checkResults;
module.exports["$getCheckResults"] = getCheckResults;

module.exports["$makeRational"] = _NUMBER["makeRational"];
module.exports["$makeRoughnum"] = _NUMBER["makeRoughnum"];
module.exports["$errCallbacks"] = _EQUALITY.NumberErrbacks;

module.exports["_not"] = _not;

module.exports["_plus"] = customPlus;
module.exports["_minus"] = customMinus;
module.exports["_times"] = customTimes;
module.exports["_divide"] = customDivide;

module.exports["_lessthan"] = _EQUALITY._lessthan;
module.exports["_greaterthan"] = _EQUALITY._greaterthan;
module.exports["_lessequal"] = _EQUALITY._lessequal;
module.exports["_greaterequal"] = _EQUALITY._greaterequal;
module.exports["_makeNumberFromString"] = _NUMBER['fromString'];

module.exports["PTuple"] = _PRIMITIVES["PTuple"];
module.exports["$makeMethodBinder"] = _PRIMITIVES["makeMethodBinder"];

module.exports["$torepr"] = torepr;
module.exports["$nothing"] = _PRIMITIVES["$nothing"];

module.exports["$customThrow"] = customThrow;

module.exports["$messageThrow"] = function(srcloc, message) {
  customThrow({
    "message": message,
    "$srcloc": srcloc
  });
}

module.exports["throwUnfinishedTemplate"] = function(srcloc) {
  customThrow({
    "$template-not-finished": srcloc
  });
};

// TODO(alex): Fill out exceptions with useful info
module.exports["throwNoCasesMatched"] = function(srcloc) {
  customThrow({
    "kind": "throwNoCasesMatched",
    "$srcloc": srcloc
  });
};

module.exports["throwNoBranchesMatched"] = function(srcloc) {
  customThrow({
    "kind": "throwNoBranchesMatched",
    "$srcloc": srcloc
  });
};

// TODO(alex): is exn necessary?
module.exports["throwNonBooleanOp"] = function(srcloc) {
  customThrow({
    "kind": "throwNonBooleanOp",
    "$srcloc": srcloc
  });
};

// TODO(alex): is exn necessary?
module.exports["throwNonBooleanCondition"] = function(srcloc) {
  customThrow({
    "kind": "throwNonBooleanCondition",
    "$srcloc": srcloc
  });
};

// NOTE: "is-roughly" => "is%(within(0.000001))"
module.exports["within"] = _EQUALITY["within"];
