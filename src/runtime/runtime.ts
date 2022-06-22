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
var _globalCheckResults: { [uri : string]: CheckResult[] } = {};
// TODO: Pass in the URI to the check test executors
//   so we can attempt to filter check blocks by module
// TODO: Add check test override
// TODO: Need to expose an check runner test API to the IDE
var $checkBlockExecutor = eagerCheckBlockRunner;
var $checkBlockFilter: (uri: string, name?: string) => boolean | null = null;

export function $setCheckBlockFilter(filter: (uri: string, name?: string) => boolean): void {
  $checkBlockFilter = filter;
}

export function $setCheckBlockExecutor(executor): void {
  $checkBlockExecutor = executor;
}

function checkBlockHandler(srcloc: string, name: string, checkBlock: () => void): void {
  $checkBlockExecutor(srcloc, name, checkBlock);
}

function getCheckResults(uri : string): CheckResult[] {
  return _globalCheckResults[uri].slice();
}

let currentMainURI = false;
function claimMainIfLoadedFirst(uri) {
  if(currentMainURI === false) {
    currentMainURI = uri;
    // The runtime can initialize filter if it wants to, to something other than null
    // Then, wrapping contexts, like an IDE, could call setCheckBlockFilter
    //
    // Then, a main program (usually run with node), can set it via claimMain, BUT
    // this is a no-op if the filter has already been managed/set by, say, the IDE
    // or an overriding command-line option.
    if($checkBlockFilter === null) {
      $setCheckBlockFilter((uriOfTester, _) => {
        return uri === uriOfTester;
      });
    }
  }
}
function clearChecks(uri) { _globalCheckResults[uri] = []; }
function checkResults(uri : string): CheckResult[] {
  let errorCount = 0;
  if($checkBlockFilter && !$checkBlockFilter(uri)) {
    return getCheckResults(uri);
  }
  _globalCheckResults[uri].forEach((result) => {
    if (!result.success) {
      errorCount += 1;
    }
  });

  if (errorCount === 0) {
    console.log("Looks shipshape, all tests passed, mate!");
  } else {
    console.log("Some tests failed.");
  }
  _globalCheckResults[uri].forEach((result) => {
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

  return getCheckResults(uri);
}

function eagerCheckTest(lhs: () => any,  rhs: () => any,
  test: (lhs: CheckExprEvalResult, rhs: CheckExprEvalResult) => CheckTestResult,
  loc: string): void {
  
  const uri = getUriForCheckLoc(loc);
  if(!(uri in _globalCheckResults)) {
    _globalCheckResults[uri] = [];
  }

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
    _globalCheckResults[uri].push({
        success: result.success,
        path: _globalCheckContext.join(),
        loc: loc,
        lhs: result.lhs,
        rhs: result.rhs,
        exception: undefined,
    });
  } catch(e) {
    _globalCheckResults[uri].push({
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
function eagerCheckBlockRunner(srcloc: string, name: string, checkBlock: () => void): void {
  if ($checkBlockFilter && !$checkBlockFilter(getUriForCheckLoc(srcloc), name)) {
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

var _globalTraceValues = {};

function getUri(loc : [string, number, number, number, number, number, number]) {
  return loc[0];
}
function getUriForCheckLoc(loc : string) {
  // NOTE(joe/luna): The locations look like file:///path:start-end. This gets
  // the bit between the two colons (one after file:, one after path:)
  return loc.substring(0, loc.indexOf(":", loc.indexOf(":") + 1));
}

// ********* Other Functions *********
export function traceValue(loc, value) {
  // NOTE(alex): stubbed out until we decide what to actually do with it
  const uri = getUri(loc);
  if(!(uri in _globalTraceValues)) {
    _globalTraceValues[uri] = [];
  }
  _globalTraceValues[uri].push({srcloc: loc, value});
  return value;
}

function getTraces(uri) { return _globalTraceValues[uri]; }
function clearTraces(uri) { _globalTraceValues[uri] = []; }

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

/* @stopify flat */
export function pauseStack(callback) {
  // @ts-ignore
  return $STOPIFY.pauseK(/* @stopify flat */ (kontinue) => {
    return callback({
      resume: /* @stopify flat */ (val) => kontinue({ type: "normal", value: val }),
      error: /* @stopify flat */ (err) => kontinue({ type: "error", error: err })
    })
  });
}

export function safeVoidCallback<A>(f : ((...args : any[]) => A)) : () => void {
  return function(...args : any[]) {
    return run(() => f(...args), (a) => {
      if(a.type === 'error') {
        console.error("A safeVoidCallback failed with an error: ", a);
      }
      else {
        console.log("A safeVoidCallback succeeded");
      }
    });
  }
}

export function run<A>(f : (() => A), onDone : ((a : {type: 'normal', value: A} | {type: 'error', result: any}) => void)) : void {
  // @ts-ignore
  return $STOPIFY.runStopifiedCode(f, onDone);
}

const allModules = { };

function addModule(uri : string, vals : any) {
  allModules[uri] = {values: vals};
}
function getModuleValue(uri : string, k : string) {
  return allModules[uri].values[k];
}

function installMethod(obj, name, method) {
  Object.defineProperty(obj, name, {enumerable: true, value: method, writable: false});
  return method;
}
function setupMethodGetters(obj) {
  const extension = {};
  for (let k in obj.$methods) {
    extension[k] = { enumerable: true, get: obj.$methods[k], configurable: true };
  }
  Object.defineProperties(obj, extension);
  return obj;
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
type ValueSkeleton = any;
type PyretValue = any;

type CacheRecord<A> = { elt: A, name: string | null, next: CacheRecord<A>} | undefined
type Cache<A> = {
  add : (elts: CacheRecord<A>, elt: A) => CacheRecord<A>,
  check : (elts : CacheRecord<A>, elt : A) => string | null
};
type StackRecord = {
  todo: PyretValue[],
  done: ValueSkeleton[],
  arrays?: CacheRecord<any[]>,
  objects?: CacheRecord<any>,
} & (
{
  type: "root",
  extra: null
} |
{
  type: "object",
  extra: { fieldNames: string[] }
} |
{
  type: "data",
  // NOTE: this is just (a fragment of) the representation of a data value
  extra: { $fieldNames: string[], $name: string }
} |
{
  type: "array",
  extra: null
} |
{
  type: "tuple",
  extra: null
})

function toOutput(val : any) {
  const VS = require("./valueskeleton.arr.js");
  function makeCache<A>(type : string) : Cache<A> {
    var cyclicCounter = 1;
    // Note (Ben): using concat was leading to quadratic copying times and memory usage...
    return {
      add: function(elts, elt) {
        return {elt: elt, name: null, next: elts};
      },
      check: function(elts, elt) {
        var cur = elts;
        while (cur !== undefined) {
          if (cur.elt === elt) {
            if (cur.name === null) {
              cur.name = "<cyclic-" + type + "-" + cyclicCounter++ + ">";
            }
            return cur.name;
          } else {
            cur = cur.next;
          }
        }
        return null;
      }
    };
  }
  var arrayCache = makeCache<any[]>("array");
  var addNewArray = arrayCache.add;
  var findSeenArray = arrayCache.check;
  var objCache = makeCache<{}>("object");
  var addNewObject = objCache.add;
  var findSeenObject = objCache.check;
  /*

  We could implement this recursively, and it would be simpler. However, we roll
  our own stack because values can be deeply nested in natural Pyret programs,
  like a student-implemented linked-list. We want to be able to convert,
  represent, and render these structures without doing a stack-manipulating
  conversion on the code (e.g. no Stopify) to keep things running nicely in an
  synchronous mode.

  The main data structure is a stack of in-progress lists of values to render
  that will be filled in to some structure (like an objcet or array rendering)
  when complete. Each element of the stack has two key fields, `todo` and
  `done`. When we visit e.g. an array like [ v1, v2, v3, v4 ], we push a new
  entry to the stack with `todo` containing all the v_i. These
  get popped and converted to rendered representations by the worklist loop,
  then added to `done`.

  After processing v4 and v3 to rendered versions rv1 and rv2, the entry will
  look like this (yes, v4 then v3):

  todo: [ v1, v2 ]      done: [ _, _, rv3, rv4 ]

  The entry also stores, in the `type`, field, that we are processing an array,
  for later dispatch to wrap the values in vs-constr.

  We pre-allocate the done array and fill it in back-to-front. This makes it so
  `done` ends in the expected order for clients without needing an extra
  reverse.

  */
  function toOutputHelp(val : PyretValue) {
    const stack : StackRecord[] = [{
      type: "root",
      arrays: undefined,
      objects: undefined,
      todo: [val],
      done: [undefined],
      extra: null
    }];
    function pushTodo<T extends StackRecord["type"]>(newArray : any[] | undefined, newObject : any, todo : any[], type : T, extra : (StackRecord & { type: T })["extra"]) {
      var top = stack[stack.length - 1];
      stack.push({
        arrays: (newArray !== undefined) ? addNewArray(top.arrays, newArray) : top.arrays,
        objects: (newObject !== undefined) ? addNewObject(top.objects, newObject) : top.objects,
        todo: todo,
        done: new Array(todo.length),
        type: type,
        extra: extra as any
      });
    }
    var top : StackRecord;
    function finishVal(vs : ValueSkeleton) {
      // NOTE(joe): attempt to be clever -- fill in top.done from the high indices
      // to avoid a reverse() later! So top.done starts at top.todo.length - 1,
      // this is also the reason for initializing done with a particular length above
      const curr = stack[stack.length - 1];
      curr.todo.pop();
      curr.done[curr.todo.length] = vs;
    }
    while (stack.length > 0 && stack[0].todo.length > 0) {
      top = stack[stack.length - 1];
      if (top.todo.length > 0) {
        var next = top.todo[top.todo.length - 1];
        if(_PRIMITIVES.isNumber(next)) { finishVal(VS["vs-num"](next)); }
        else if (_PRIMITIVES.isBoolean(next)) { finishVal(VS["vs-bool"](next)); }
        else if (_PRIMITIVES.isNothing(next)) { finishVal(VS["vs-nothing"]); }
        else if (_PRIMITIVES.isFunction(next)) { finishVal(VS["vs-function"](next)); }
        else if (_PRIMITIVES.isMethod(next)) { finishVal(VS["vs-method"](next)); }
        else if (_PRIMITIVES.isString(next)) { finishVal(VS["vs-str"](next)); }
        else if (_PRIMITIVES.isArray(next)) {
          const arrayHasBeenSeen = findSeenArray(top.arrays, next);
          if(typeof arrayHasBeenSeen === "string") {
            finishVal(VS["vs-cyclic"](arrayHasBeenSeen));
          }
          else {
            // NOTE(joe): the spread to copy the array below is important
            // because we will pop from it when processing the stack
            // Baffling bugs will result if next is passed directly; user arrays
            // will empty on rendering
            pushTodo(next, undefined, [...next], "array", null);
          }
        }
        else if (_PRIMITIVES.isPTuple(next)) {
          pushTodo(undefined, undefined, [...next], "tuple", null);
        }
        else if (_PRIMITIVES.isRawObject(next) || _PRIMITIVES.isDataVariant(next)) {
          const objHasBeenSeen = findSeenObject(top.objects, next);
          if(typeof objHasBeenSeen === "string") {
            finishVal(VS["vs-cyclic"](objHasBeenSeen));
          }
          else if('_output' in next && (_PRIMITIVES.isMethod(next['_output']))) {
            const m = next._output(toOutputHelp);
            finishVal(m);
          }
          else if(_PRIMITIVES.isDataVariant(next)) {
            const names = next.$fieldNames;
            if(names === null) {
              finishVal(VS['vs-literal-str'](next.$name));
            }
            else {
              const vals = names.map(n => next[n]);
              pushTodo(undefined, next, vals, "data", next);
            }
          }
          else if(_PRIMITIVES.isRawObject(next)) {
            const names = _PRIMITIVES.getRawObjectFields(next);
            const vals = names.map(n => next[n]);
            pushTodo(undefined, next, vals, "object", { fieldNames: names });
          }
          else {
            finishVal(VS['vs-literal-str'](JSON.stringify(next) + "\n" + new Error().stack))
          }
        }
        else {
          finishVal(VS['vs-literal-str'](JSON.stringify(next) + "\n" + new Error().stack))
        }
      }
      else {
        // We just finished a compuond value's components, and have a reference
        // to the results in top. The goal is to put the finished composite
        // result into the done list of the preceding stack entry, and remove
        // the stack entry we just finished. So pop here, and top will be a
        // reference to the element we just removed.
        stack.pop();
        switch(top.type) {
          case "array":
            finishVal(VS["vs-collection"]("raw-array", top.done));
            break;
          case "tuple":
            finishVal(VS["vs-tuple"](top.done));
            break;
          case "data":
            finishVal(VS["vs-constr"](top.extra.$name, top.extra.$fieldNames, top.done));
            break;
          case "object":
            finishVal(VS["vs-record"](top.extra.fieldNames, top.done));
            break;
        }
      }
    }
    var finalAns = stack[0].done[0];
    return finalAns;
  }
  return toOutputHelp(val);
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
module.exports["$clearTraces"] = clearTraces;

module.exports["$spy"] = _spy;

module.exports["$extend"] = _PRIMITIVES.extend;
module.exports["$installMethod"] = installMethod;
module.exports["$setupMethodGetters"] = setupMethodGetters;
module.exports["$makeDataValue"] = _PRIMITIVES.makeDataValue;
module.exports["$createVariant"] = _PRIMITIVES.createVariant;

module.exports["$claimMainIfLoadedFirst"] = claimMainIfLoadedFirst;

module.exports["$checkTest"] = eagerCheckTest;
module.exports["$checkBlock"] = checkBlockHandler;
module.exports["$checkResults"] = checkResults;
module.exports["$getCheckResults"] = getCheckResults;
module.exports["$clearChecks"] = clearChecks;

module.exports["$makeRational"] = _NUMBER["makeRational"];
module.exports["$makeRoughnum"] = _NUMBER['makeRoughnum'];
module.exports["$numToRoughnum"] = (n, errbacks) => _NUMBER['makeRoughnum'](_NUMBER['toFixnum'](n), errbacks);
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
module.exports["$tooutput"] = toOutput;
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
