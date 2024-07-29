
import { stopifyArrayPrototype } from "./hof-array-polyfill";
import { NumericErrorCallbacks } from "./equality";
import {
  Srcloc,
  CheckResult,
} from "./common-runtime-types";
import { $PMethodBrand, applyBrand } from "./primitives";
import type * as E_TYPES from '../runtime-arr/either.arr';
import type { Variant } from './types/primitive-types';

console.log(stopifyArrayPrototype);

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

function formatSrcloc(loc: Srcloc, showFile: boolean): string {
  switch(loc.length) {
    case 1: return `<builtin ${loc[0]}>`;
    case 7:
      const [uri, startLine, startCol, _startChar, endLine, endCol, _endChar] = loc;
      if (showFile) {
        const start = `${uri}:${startLine}:${startCol}`;
        const end = `${endLine}:${endCol}`;
        return `${start}-${end}`;
      } else {
        return `line ${startLine}, column ${startCol}`;
      }
  }
}


var $spyMessageHandler = function(data : any) {
  if (data.message) {
    console.log(`Spying "${data.message}" (at ${formatSrcloc(data.loc, true)})`);
  } else {
    console.log(`Spying (at ${formatSrcloc(data.loc, true)})`);
  }
};

var $spyValueHandler = function(data : any) {
  console.log(`    ${data.key}: ${data.value} (at ${formatSrcloc(data.loc, true)})`);
};

export interface SpyExpr {
  key: string,
  expr: () => any,
  loc: Srcloc
}

export interface SpyObject {
  message: () => string,
  loc: Srcloc,
  exprs: SpyExpr[],
}

export function $setSpyMessageHandler(handler : any) {
  $spyMessageHandler = handler;
}

export function $setSpyValueHandler(handler : any) {
  $spyValueHandler = handler;
}

export function _not(x: boolean): boolean { return !x; }

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
var $checkBlockFilter: ((uri: string, name?: string) => boolean) | null = null;

export function $setCheckBlockFilter(filter: (uri: string, name?: string) => boolean): void {
  $checkBlockFilter = filter;
}

function getCheckResults(uri : string): CheckResult[] {
  return _globalCheckResults[uri].slice();
}

function stubCheck(args : any[]) {
  console.log("Stubbed out check function", args);
}

const stubCheckContext = {
    'runChecks': function(...args: any[]) { stubCheck(args); },
    'checkIs': function(...args: any[]) { stubCheck(args); },
    'checkIsCause': function(...args: any[]) { stubCheck(args); },
    'checkIsRoughly': function(...args: any[]) { stubCheck(args); },
    'checkIsRoughlyCause': function(...args: any[]) { stubCheck(args); },
    'checkIsNot': function(...args: any[]) { stubCheck(args); },
    'checkIsNotCause': function(...args: any[]) { stubCheck(args); },
    'checkIsNotRoughly': function(...args: any[]) { stubCheck(args); },
    'checkIsNotRoughlyCause': function(...args: any[]) { stubCheck(args); },
    'checkIsRefinement': function(...args: any[]) { stubCheck(args); },
    'checkIsRefinementCause': function(...args: any[]) { stubCheck(args); },
    'checkIsNotRefinement': function(...args: any[]) { stubCheck(args); },
    'checkIsNotRefinementCause': function(...args: any[]) { stubCheck(args); },
    'checkSatisfiesDelayed': function(...args: any[]) { stubCheck(args); },
    'checkSatisfiesDelayedCause': function(...args: any[]) { stubCheck(args); },
    'checkSatisfiesNotDelayed': function(...args: any[]) { stubCheck(args); },
    'checkSatisfiesNotDelayedCause': function(...args: any[]) { stubCheck(args); },
    'checkRaisesStr': function(...args: any[]) { stubCheck(args); },
    'checkRaisesStrCause': function(...args: any[]) { stubCheck(args); },
    'checkRaisesOtherStr': function(...args: any[]) { stubCheck(args); },
    'checkRaisesOtherStrCause': function(...args: any[]) { stubCheck(args); },
    'checkRaisesNot': function(...args: any[]) { stubCheck(args); },
    'checkRaisesNotCause': function(...args: any[]) { stubCheck(args); },
    'checkRaisesSatisfies': function(...args: any[]) { stubCheck(args); },
    'checkRaisesSatisfiesCause': function(...args: any[]) { stubCheck(args); },
    'checkRaisesViolates': function(...args: any[]) { stubCheck(args); },
    'checkRaisesViolatesCause': function(...args: any[]) { stubCheck(args); },
    'results': function() { return []; },
};
let checkContext = stubCheckContext;
function initializeCheckContext(uri : string, all : boolean) {
  if(stubCheckContext === checkContext && currentMainURI) {
    const checker = require("./checker" + ".js");
    checkContext = checker['makeCheckContext'](currentMainURI, all);
  }
  return checkContext;
}

function currentCheckContext() {
  return checkContext;
}

let currentMainURI : boolean | string = false;
function getCurrentMainURI() {
  return currentMainURI;
}
function claimMainIfLoadedFirst(uri : string) {
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
function clearChecks(uri : string) { _globalCheckResults[uri] = []; }
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

type CompiledSrcloc = [string, number, number, number, number, number, number] | string;

function srclocToPyretLoc(loc: CompiledSrcloc) {
  const srcloc = require("./srcloc" + ".arr.js");
  if(typeof loc === 'string') {
    return srcloc.builtin(loc);
  }
  else {
    return srcloc.srcloc(loc[0], loc[1], loc[2], loc[3], loc[4], loc[5], loc[6]);
  }
}

var _globalTraceValues : any = {};

function getUri(loc : Srcloc) {
  return loc[0];
}
function getUriForCheckLoc(loc : Srcloc) {
  // NOTE(joe/luna): The locations look like file:///path:start-end. This gets
  // the bit between the two colons (one after file:, one after path:)
  return loc[0].substring(0, loc.indexOf(":", loc.indexOf(":") + 1));
}

// ********* Other Functions *********
export function $traceValue(loc : any, value : any) {
  // NOTE(alex): stubbed out until we decide what to actually do with it
  const uri = getUri(loc);
  if(!(uri in _globalTraceValues)) {
    _globalTraceValues[uri] = [];
  }
  _globalTraceValues[uri].push({srcloc: loc, value});
  return value;
}

function getTraces(uri : any) { return _globalTraceValues[uri]; }
function clearTraces(uri : any) { _globalTraceValues[uri] = []; }

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
export function pauseStack(callback : any) {
  // @ts-ignore
  return $STOPIFY.pauseK(/* @stopify flat */ (kontinue) => {
    return callback({
      resume: /* @stopify flat */ (val : any) => kontinue({ type: "normal", value: val }),
      error: /* @stopify flat */ (err : any) => kontinue({ type: "error", error: err })
    })
  });
}

/* @stopify flat */
export function safeVoidCallback<A>(f : ((...args : any[]) => A)) : () => void {
  return /* @stopify flat */ function(...args : any[]) {
    return run(() => f(...args), /* @stopify flat */ (a) => {
      if(a.type === 'error') {
        console.error("A safeVoidCallback failed with an error: ", a);
      }
      else {
        console.log("A safeVoidCallback succeeded", a);
      }
    });
  }
}

/* @stopify flat */
export function run<A>(f : (() => A), onDone : ((a : {type: 'normal', value: A} | {type: 'error', result: any}) => void)) : void {
  // @ts-ignore
  return $STOPIFY.runStopifiedCode(f, onDone);
}

const allModules : any = { };

export function addModule(uri : string, vals : any) {
  allModules[uri] = {values: vals};
}
export function getModuleValue(uri : string, k : string) {
  return allModules[uri].values[k];
}

/* @stopify flat */
function installMethod(obj : any, name : any, method : any) {
  applyBrand($PMethodBrand, method);
  Object.defineProperty(obj, name, {enumerable: true, value: method, writable: false});
  return method;
}
/* @stopify flat */
function setupMethodGetters(obj : any) {
  const extension : any = {};
  for (let k in obj.$methods) {
    extension[k] = { enumerable: true, get: obj.$methods[k], configurable: true };
  }
  Object.defineProperties(obj, extension);
  return obj;
}


// TODO(alex): common Pyret error objects
export function raise(msg: object) {
  // NOTE(alex): Changing the representation needs to be reflected in raiseExtract()
  throw msg;
}

function raiseExtract(exception: any): string {
  // NOTE(alex): Used by `raises` check operator
  //   Any changes to the `raise` exception format needs to be reflected
  //   here as well.
  return exception.toString();
}

function toRepr(v : any) {
  return renderValueSkeleton(toOutput(v), RenderToRepr);
}

function toString(v : any) {
  return renderValueSkeleton(toOutput(v), RenderToString);
}

export type ValueSkeleton = 
  { $name: "vs-literal-str", s: string }
| { $name: "vs-str", s: string }
| { $name: "vs-num", v: PyretValue }
| { $name: "vs-bool", v: boolean }
| { $name: "vs-nothing" }
| { $name: "vs-tuple", vals: ValueSkeleton[] }
| { $name: "vs-function", v: PyretValue }
| { $name: "vs-method", v: PyretValue }
| { $name: "vs-record", "field-names": string[], vals: ValueSkeleton[] }
| { $name: "vs-collection", name: string, items: ValueSkeleton[] }
| { $name: "vs-constr", name: string, "field-names": string[], args: ValueSkeleton[] }
| { $name: "vs-table", headers: string[], rows: ValueSkeleton[][] }
| { $name: "vs-row", headers: string[], values: ValueSkeleton[] }
| { $name: "vs-seq", items: ValueSkeleton[] }
| { $name: "vs-cyclic", label: string, v: any }
| { $name: "vs-reactor", v: any }
| { $name: "vs-other", v: any };
export type PyretValue = any;

type ReprVisitor<A> = {
  [method in ValueSkeleton["$name"]]: 
    (this: ReprVisitor<A>, vs: Variant<ValueSkeleton, method>) => A
}


function renderValueSkeleton<A>(vs: ValueSkeleton, visitor: ReprVisitor<A>): A {
  return (visitor[vs.$name] as any).apply(visitor, [vs]);
}

// NOTE(Ben): this really should go in string.arr.js, but that causes an import-loop
// error and the compiler won't boot itself up
function replaceUnprintableStringChars(s: string): string {
  const ret: string[] = [];
  for (let i = 0; i < s.length; i++) {
    var val = s.charCodeAt(i);
    switch(val) {
    case 7: ret.push('\\a'); break;
    case 8: ret.push('\\b'); break;
    case 9: ret.push('\\t'); break;
    case 10: ret.push('\\n'); break;
    case 11: ret.push('\\v'); break;
    case 12: ret.push('\\f'); break;
    case 13: ret.push('\\r'); break;
    case 34: ret.push('\\"'); break;
    case 92: ret.push('\\\\'); break;
    default:
      if (val >= 32 && val <= 126) {
        ret.push( s.charAt(i) );
      }
      else {
        var numStr = val.toString(16).toUpperCase();
        while (numStr.length < 4) {
          numStr = '0' + numStr;
        }
        ret.push('\\u' + numStr);
      }
      break;
    }
  }
  return ret.join('');
}

const RenderToRepr: ReprVisitor<string> = {
  "vs-literal-str": (vs) => vs.s,
  "vs-bool": (vs) => String(vs.v),
  "vs-str": (vs) => '"' + replaceUnprintableStringChars(vs.s) + '"',
  "vs-num": (vs) => String(vs.v),
  "vs-nothing": (_vs) => "nothing",
  "vs-function": (_vs) => `<function>`,
  "vs-method": (_vs) => `<method>`,
  "vs-tuple": function (vs) {
    return `{${vs.vals.map((v) => renderValueSkeleton(v, this)).join("; ")}}`;
  },
  "vs-record": function (vs) {
    const pairs = vs.vals.map((v, idx) => `${vs["field-names"][idx]}: ${renderValueSkeleton(v, this)}`);
    return `{ ${pairs.join(", ")} }`;
  },
  "vs-collection": function (vs) {
    const items = vs.items.map((v) => renderValueSkeleton(v, this)).join(", ");
    return `[${vs.name}: ${items}]`;
  },
  "vs-constr": function (vs) {
    const items = vs.args.map((v) => renderValueSkeleton(v, this)).join(", ");
    return `${vs.name}(${items})`;
  },
  "vs-table": (_vs) => "<table>",
  "vs-row": (_vs) => "<row>",
  "vs-seq": function (vs) {
    return vs.items.map((v) => renderValueSkeleton, this).join("\n");
  },
  "vs-reactor": (vs) => "<reactor>",
  "vs-cyclic": (vs) => `<${vs.label}>`,
  "vs-other": (vs) => JSON.stringify(vs.v)
};

const RenderToString: ReprVisitor<string> = Object.assign({}, RenderToRepr, {
  "vs-str": (vs : Variant<ValueSkeleton, "vs-str">) => String(vs.s)
});

const RenderToCli: ReprVisitor<string> = Object.assign({}, RenderToRepr, {
  "vs-function": (vs: Variant<ValueSkeleton, "vs-function">) => `<function:${vs.v.name}>`,
  "vs-method": (vs: Variant<ValueSkeleton, "vs-method">) => `<function:${vs.v.name}>`
});



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
  const VS = require("./value" + "skeleton.arr.js");
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
      done: new Array(1),
      extra: null
    }];
    function pushArrayTodo(newArray: PyretValue[], todo: PyretValue[]) {
      const top = stack[stack.length - 1];
      stack.push({
        type: "array",
        arrays: addNewArray(top.arrays, newArray),
        objects: top.objects,
        todo,
        done: new Array(todo.length),
        extra: null
      });
    }
    function pushTupleTodo(todo: PyretValue[]) {
      const top = stack[stack.length - 1];
      stack.push({
        type: "tuple",
        arrays: top.arrays,
        objects: top.objects,
        todo,
        done: new Array(todo.length),
        extra: null
      });
    }
    function pushDataTodo(newObject: PyretValue, todo: PyretValue[], extra: (StackRecord & {type: "data"})["extra"]) {
      const top = stack[stack.length - 1];
      stack.push({
        type: "data",
        arrays: top.arrays,
        objects: addNewObject(top.objects, newObject),
        todo,
        done: new Array(todo.length),
        extra
      });
    }
    function pushObjectTodo(newObject: PyretValue, todo: PyretValue[], extra: (StackRecord & {type: "object"})["extra"]) {
      const top = stack[stack.length - 1];
      stack.push({
        type: "object",
        arrays: top.arrays,
        objects: addNewObject(top.objects, newObject),
        todo,
        done: new Array(todo.length),
        extra
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
            finishVal(VS["vs-cyclic"](arrayHasBeenSeen, next));
          }
          else {
            // NOTE(joe): the spread to copy the array below is important
            // because we will pop from it when processing the stack
            // Baffling bugs will result if next is passed directly; user arrays
            // will empty on rendering
            pushArrayTodo(next, [...next]);
          }
        }
        else if (_PRIMITIVES.isPTuple(next)) {
          pushTupleTodo([...next]);
        }
        else if (_PRIMITIVES.isTable(next)) {
          const m = next._output(toOutputHelp);
          finishVal(m);
        }
        else if (_PRIMITIVES.isRawObject(next) || _PRIMITIVES.isDataVariant(next)) {
          const objHasBeenSeen = findSeenObject(top.objects, next);
          if(typeof objHasBeenSeen === "string") {
            finishVal(VS["vs-cyclic"](objHasBeenSeen, next));
          }
          else if('_output' in next && (_PRIMITIVES.isCallable(next['_output']))) {
            const m = next._output(toOutputHelp);
            finishVal(m);
          }
          /* This seems like it would always be false (['$methods']['_output'] wouldn't have the right brands)
          else if('$methods' in next && '_output' in next['$methods'] && (_PRIMITIVES.isMethod(next['$methods']['_output']))) {
            const m = next['$methods']['_output'](toOutputHelp);
            finishVal(m);
          }
          */
          else if(_PRIMITIVES.isDataVariant(next)) {
            const names = next.$fieldNames;
            if(names === null) {
              finishVal(VS['vs-literal-str'](next.$name));
            }
            else {
              const vals = names.map((n : any) => next[n]);
              pushDataTodo(next, vals, next);
            }
          }
          else if(_PRIMITIVES.isRawObject(next)) {
            const names = _PRIMITIVES.getRawObjectFields(next);
            const vals = names.map((n : any) => next[n]);
            pushObjectTodo(next, vals, { fieldNames: names });
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

export function errors() {
  return require("./" + "error.arr.js");
}

class PyretError extends Error {
  errVal : any;
  errorDisplay : any;
  constructor(errVal : any) {
    const ed = errVal['render-reason']();
    const red = require("./" + "error-display.arr.js");
    super(red["display-to-string"](ed, toRepr, []));
    this.errVal = errVal;
    this.errorDisplay = ed;
  }
  toString() {
    const ed = this.errVal['render-reason']();
    const red = require("./" + "error-display.arr.js");
    console.log(red["display-to-string"](ed, toRepr, []));
  }
}

export function throwError(name : string, ...args : any[]) {
  throw new PyretError(errors()[name](...args));
}

export function checkType(val : any, test : (v : any) => boolean, typeName : string) {
  if(!test(val)) {
    throwError("generic-type-mismatch", val, typeName);
  }
  return true;
}

export function makeCheckType(test : (v : any) => boolean, typeName : string) {
  return function(val : any) {
    debugger;
    return checkType(val, test, typeName);
  };
}

function isNatural(n : any) {
  return _NUMBER.isInteger(n) && _NUMBER.isNonNegative(n);
}

export const checkString = makeCheckType(_PRIMITIVES.isString, "String");
export const checkNumber = makeCheckType(_PRIMITIVES.isNumber, "Number");
export const checkExactnum = makeCheckType(_NUMBER.isRational, "Exactnum");
export const checkRoughnum = makeCheckType(_NUMBER.isRoughnum, "Roughnum");
export const checkNumInteger = makeCheckType(_NUMBER.isInteger, "NumInteger");
export const checkNumRational = makeCheckType(_NUMBER.isRational, "NumRational");
export const checkNumPositive = makeCheckType(_NUMBER.isPositive, "NumPositive");
export const checkNumNegative = makeCheckType(_NUMBER.isNegative, "NumNegative");
export const checkNumNonPositive = makeCheckType(_NUMBER.isNonPositive, "NumNonPositive");
export const checkNumNonNegative = makeCheckType(_NUMBER.isNonNegative, "NumNonNegative");
export const checkNumNatural = makeCheckType(isNatural, "Natural Number");

function customThrow(exn : any) {
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

function runTask<A>(f : (() => A)): E_TYPES.Either<A, any> {
  const Either = require("./either" + ".arr.js") as typeof E_TYPES;
  try {
    return Either.left(f());
  }
  catch(e) {
    return Either.right(e);
  }
}

const omittedCheckResults: {[uri: string]: boolean} = {};
const postLoadHooks : {[uri: string]: (result: any) => void} = {
};

function omitCheckResults(uri: string) {
  omittedCheckResults[uri] = true;
}

const emptySummary = {
  message: "The program didn't define any tests.",
  passed: 0,
  failed: 0,
  errored: 0,
  total: 0
}

function postLoadHook(uri : string, result : any) {
  if(uri === currentMainURI && omittedCheckResults[uri] !== true) {
    let renderedChecks = [];
    let summary = emptySummary;
    if (result.$checks.length > 0) {
      const checker = require('./checker' + ".js");
      renderedChecks = result.$checks.map((br : any) => {
        return { ...br,
          testResults: br.testResults.map((tr : any) => {
            return { ...tr, rendered: checker.renderFancyReason(tr) };
          })
        }
      });
      summary = checker.resultsSummary(result.$checks);
    }
    console.log("Check summary: ", summary, result.$checks);
    currentMainURI = false;
    checkContext = stubCheckContext;
    return { ...result, $renderedChecks: { checkSummary: summary, renderedChecks: renderedChecks } };
  }
  if(uri in postLoadHooks) {
    return postLoadHooks[uri](result);
  }
  else {
    return result;
  }
}


export const Equal = _EQUALITY.Equal;

export const NotEqual = _EQUALITY.NotEqual;
export const Unknown = _EQUALITY.Unknown;

// Hack needed to match generate Pyret-code
export const $isEqual = _EQUALITY.isEqual;
export const $isNotEqual = _EQUALITY.isNotEqual;
export const $isUnknown = _EQUALITY.isUnknown;

export const $equalNow = _EQUALITY.equalNow;
export const $equalNow3 = _EQUALITY.equalNow3;
export const $equalAlways = _EQUALITY.equalAlways;
export const $equalAlways3 = _EQUALITY.equalAlways3;

export const $identical = _EQUALITY.identical;
export const $identical3 = _EQUALITY.identical3;

// Expected runtime functions
export const $raiseExtract = raiseExtract;
export const $getTraces = getTraces;
export const $clearTraces = clearTraces;

export const $spy = _spy;

export const $extend = _PRIMITIVES.extend;
export const $installMethod = installMethod;
export const $setupMethodGetters = setupMethodGetters;
export const $makeDataValue = _PRIMITIVES.makeDataValue;
export const $createVariant = _PRIMITIVES.createVariant;

export const $claimMainIfLoadedFirst = claimMainIfLoadedFirst;
export const $initializeCheckContext = initializeCheckContext;
export const $omitCheckResults = omitCheckResults;
export const $postLoadHook = postLoadHook;

export const $checkResults = checkResults;
export const $getCheckResults = getCheckResults;
export const $clearChecks = clearChecks;

export const $makeRational = _NUMBER["makeRational"];
export const $makeRoughnum = _NUMBER['makeRoughnum'];
export const $numToRoughnum = (n : any, errbacks : any) => _NUMBER['makeRoughnum'](_NUMBER['toFixnum'](n), errbacks);
export const $errCallbacks = _EQUALITY.NumberErrbacks;

export const _plus = customPlus;
export const _minus = customMinus;
export const _times = customTimes;
export const _divide = customDivide;

export const _lessthan = _EQUALITY._lessthan;
export const _greaterthan = _EQUALITY._greaterthan;
export const _lessequal = _EQUALITY._lessequal;
export const _greaterequal = _EQUALITY._greaterequal;
export const _makeNumberFromString = _NUMBER['fromString'];

export const PTuple = _PRIMITIVES["PTuple"];
export const $makeMethodBinder = _PRIMITIVES["makeMethodBinder"];

export const $torepr = toRepr;
export const $tostring = toString;
export const $tooutput = toOutput;
export const $nothing = _PRIMITIVES["$nothing"];

export const $customThrow = customThrow;

export const $messageThrow = function(srcloc : any, message : any) {
  customThrow({
    "message": message,
    "$srcloc": srcloc
  });
}

export const throwUnfinishedTemplate = function(srcloc : any) {
  customThrow({
    "$template-not-finished": srcloc
  });
};

// TODO(alex): Fill out exceptions with useful info
export const throwNoCasesMatched = function(srcloc : any) {
  customThrow({
    "kind": "throwNoCasesMatched",
    "$srcloc": srcloc
  });
};

export const throwNoBranchesMatched = function(srcloc : any) {
  customThrow({
    "kind": "throwNoBranchesMatched",
    "$srcloc": srcloc
  });
};

// TODO(alex): is exn necessary?
export const throwNonBooleanOp = function(srcloc : any) {
  customThrow({
    "kind": "throwNonBooleanOp",
    "$srcloc": srcloc
  });
};

// TODO(alex): is exn necessary?
export const throwNonBooleanCondition = function(srcloc : any) {
  customThrow({
    "kind": "throwNonBooleanCondition",
    "$srcloc": srcloc
  });
};

// NOTE: "is-roughly" => "is%(within(0.000001))"
export const within = _EQUALITY["within"];
export const jsnums = _NUMBER;

export const debug = /* @stopify flat */ function() { debugger; };

export const $runTask = runTask;

export const $isBoolean = _PRIMITIVES.isBoolean;
export const $isFunction = _PRIMITIVES.isFunction;
