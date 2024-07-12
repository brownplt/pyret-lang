// TODO: These can't be imported, correct?
import type * as EQ from './types/equality-types';
import type * as EQUALITY_TYPES from './equality';
import type * as RUNTIME_TYPES from './runtime';
const EQUALITY = require("./equality.js") as typeof EQUALITY_TYPES;
const RUNTIME = require('./runtime') as typeof RUNTIME_TYPES;

// TODO: import this from somewhere in the runtime
type Variant<T, V> = T & { $name: V };

// TODO: import this from somewhere in the runtime
export type Srcloc = 
  | { $name: "builtin", dict: { 'module-name': string } }
  | {
    $name: "srcloc",
    dict: 
      {
        'source': string,
        'start-line': number,
        'start-column': number,
        'start-char': number,
        'end-line': number,
        'end-column': number,
        'end-char': number
      }
  }

// TODO: import Options from somewhere in the runtime
type Option<A> =
| { $name: 'none' }
| { $name: 'some', value: A }

function some<A>(value: A): Variant<Option<A>, 'some'> {
  return { $name: 'some', value };
}
const none = { $name: 'none' as const }

type Thunk<A> = () => A;
type Opaque<A> = { v: A };
type TestThunk<A> = Thunk<A> | Opaque<Thunk<A>>
type Either<A, B> = 
| { $name: 'left', val: A }
| { $name: 'right', val: B }
// TODO(Ben/Joe): when we have real types for Either,
// replace this with the function in runtime.
function runTask<A>(f : (() => A)) : Either<A, any> {
  try {
    return { $name: 'left', val: f() };
  }
  catch(e) {
    return { $name: 'right', val: e };
  }
}

class ExhaustiveSwitchError extends Error {
  constructor(v: never, message?: string) {
    super(`Switch is not exhaustive on \`${JSON.stringify(v)}\`: ${message}`);
  }
}




export type IsOp = 'op==' | 'op=~' | 'op<=>'

export function getOpFunName(opname : IsOp) : string {
  switch(opname) {
    case 'op==': return 'equal-always';
    case 'op=~': return 'equal-now';
    case 'op<=>': return 'identical';
    default: throw new ExhaustiveSwitchError(opname);
  }
}

export type CheckOperand = 'on-left' | 'on-right' | 'on-refinement' | 'on-cause'

export function sideOfCheckOp(checkop: CheckOperand): string {
  switch(checkop) {
    case 'on-left': return 'left side';
    case 'on-right': return 'right side';
    case 'on-refinement': return 'refinement';
    case 'on-cause': return 'explanation';
    default: throw new ExhaustiveSwitchError(checkop);
  }
}

export type TestCase = {
  name: string,
  run: Thunk<any>,
  keywordCheck: boolean,
  location: Srcloc,
}

function checkBlockResult(
  name: string,
  loc: Srcloc,
  isKeywordCheck: boolean,
  testResults: TestResult[],
  maybeErr: Option<any>, 
) {
  return { name, loc, isKeywordCheck, testResults, maybeErr };
}

export type CheckBlockResult = ReturnType<typeof checkBlockResult>

function success(
  loc: Srcloc
) {
  return { $name: 'success' as const, loc };
}
function failureNotEqual(
  loc: Srcloc, 
  refinement: any,
  left: any,
  leftSrc: CheckOperand,
  right: any,
  rightSrc: CheckOperand
) {
  return { $name: 'failure-not-equal' as const, refinement, loc, left, leftSrc, right, rightSrc };
}
function failureIsIncomparable(
  loc: Srcloc,
  eqResult: Variant<EQ.EqualityResult, 'Unknown'>,
  left: any,
  leftSrc: CheckOperand,
  right: any,
  rightSrc: CheckOperand
) {
  return { $name: 'failure-is-incomparable' as const, loc, eqResult, left, leftSrc, right, rightSrc };
}
function failureNotDifferent(
  loc: Srcloc,
  refinement: Option<any>,
  left: any,
  leftSrc: CheckOperand,
  right: any,
  rightSrc: CheckOperand
) {
  return { $name: 'failure-not-different' as const, loc, refinement, left, leftSrc, right, rightSrc };
}
function failureNotSatisfied(
  loc: Srcloc,
  val: any,
  valSrc: CheckOperand,
  pred: any
) {
  return { $name: 'failure-not-satisfied' as const, loc, val, valSrc, pred };
}
function failureNotDissatisfied(
  loc: Srcloc,
  val: any,
  valSrc: CheckOperand,
  pred: any
) {
  return { $name: 'failure-not-dissatisfied' as const, loc, val, valSrc, pred };
}
function failureWrongExn(
  loc: Srcloc,
  exnExpected: any,
  exnActual: any,
  actualSrc: CheckOperand
) {
  return { $name: 'failure-wrong-exn' as const, loc, exnExpected, exnActual, actualSrc };
}
function failureRightExn(
  loc: Srcloc,
  exnNotExpected: any,
  exnActual: any,
  actualSrc: CheckOperand
) {
  return { $name: 'failure-right-exn' as const, loc, exnNotExpected, exnActual, actualSrc };
}
function failureExn(
  loc: Srcloc,
  actualExn: any,
  exnPlace: CheckOperand
) {
  return { $name: 'failure-exn' as const, loc, actualExn, exnPlace };
}
function failureNoExn(
  loc: Srcloc,
  exnExpected: Option<string>,
  exnSrc: CheckOperand,
  wanted: boolean,
) {
  return { $name: 'failure-no-exn' as const, loc, exnExpected, exnSrc, wanted };
}
function failureRaiseNotSatisfied(
  loc: Srcloc,
  exn: any,
  exnSrc: CheckOperand,
  pred: any
) {
  return { $name: 'failure-raise-not-satisfied' as const, loc, exn, exnSrc, pred };
}
function failureRaiseNotDissatisfied(
  loc: Srcloc,
  exn: any,
  exnSrc: CheckOperand,
  pred: any
) {
  return { $name: 'failure-raise-not-dissatisfied' as const, loc, exn, exnSrc, pred };
}
function errorNotBoolean(
  loc: Srcloc,
  refinement: any,
  left: any,
  leftSrc: CheckOperand,
  right: any,
  rightSrc: CheckOperand,
  testResult: any
) {
  return { $name: 'error-not-boolean' as const, loc, refinement, left, leftSrc, right, rightSrc, testResult };
}
function errorNotPred(
  loc: Srcloc,
  predicate: any,
  arity: number,
) {
  return { $name: 'error-not-pred' as const, loc, predicate, arity };
}
function errorNotBooleanPred(
  loc: Srcloc,
  predicate: any,
  left: any,
  leftSrc: CheckOperand,
  testResult: any,
) {
  return { $name: 'error-not-boolean-pred' as const, loc, predicate, left, leftSrc, testResult };
}
export type TestResult = 
| ReturnType<typeof success>
| ReturnType<typeof failureNotEqual>
| ReturnType<typeof failureIsIncomparable>
| ReturnType<typeof failureNotDifferent>
| ReturnType<typeof failureNotSatisfied>
| ReturnType<typeof failureNotDissatisfied>
| ReturnType<typeof failureWrongExn>
| ReturnType<typeof failureRightExn>
| ReturnType<typeof failureExn>
| ReturnType<typeof failureNoExn>
| ReturnType<typeof failureRaiseNotSatisfied>
| ReturnType<typeof failureRaiseNotDissatisfied>
| ReturnType<typeof errorNotBoolean>
| ReturnType<typeof errorNotPred>
| ReturnType<typeof errorNotBooleanPred>




export function makeCheckContext(mainModuleName: string, checkAll: boolean) {
  const blockResults: CheckBlockResult[] = [];
  function addBlockResult(cbr : CheckBlockResult) {
    blockResults.push(cbr);
  }
  let currentResults: TestResult[] = [];
  function addResult(tr : TestResult) {
    currentResults.push(tr);
  }
  function leftRightCheck(
    loc: Srcloc,
    left: TestThunk<any>,
    right: TestThunk<any>,
    withVals: ((left: any, right: any) => any),
  ) {
    const lv = runTask(typeof left === 'function' ? left : left.v );
    if (lv.$name === 'right') {
      addResult(failureExn(loc, lv.val, 'on-left'))
    } else {
      const rv = runTask(typeof right === 'function' ? right : right.v);
      if (rv.$name === 'right') {
        addResult(failureExn(loc, rv.val, 'on-right'));
      } else {
        const res = runTask(() => withVals(lv.val, rv.val));
        if (res.$name === 'right') {
          addResult(failureExn(loc, res.val, 'on-refinement'));
        } else {
          return res.val;
        }
      }
    }
  }
  function leftRightCauseCheck(
    loc: Srcloc,
    left: TestThunk<any>, 
    right: TestThunk<any>,
    cause: TestThunk<any>,
    withVals: ((left: any, right: any, cause: any) => any), 
  ) {
    const lv = runTask(typeof left === 'function' ? left : left.v );
    if (lv.$name === 'right') {
      addResult(failureExn(loc, lv.val, 'on-left'))
    } else {
      const rv = runTask(typeof right === 'function' ? right : right.v);
      if (rv.$name === 'right') {
        addResult(failureExn(loc, rv.val, 'on-right'));
      } else {
        const cv = runTask(typeof cause === 'function' ? cause : cause.v);
        if (cv.$name === 'right') {
          addResult(failureExn(loc, cv.val, 'on-cause'));
        } else {
          const res = runTask(() => withVals(lv.val, rv.val, cv.val));
          if (res.$name === 'right') {
            addResult(failureExn(loc, res.val, 'on-refinement'));
          } else {
            return res.val;
          }
        }
      }
    }
  }
  function checkBool(loc: Srcloc, testResult: boolean, onFail: (loc: Srcloc) => TestResult) {
    if (testResult) {
      addResult(success(loc));
    } else {
      addResult(onFail(loc));
    }
  }
  function causesErrorNotPred(exn: any): boolean {
    return typeof exn === 'object' && (exn.$name === 'arity-mismatch' || exn.$name === 'non-function-app');
  }
  function isUnknown(result : boolean | EQ.EqualityResult): result is Variant<EQ.EqualityResult, 'Unknown'> {
    return typeof result === 'object' && result.$name === 'Unknown';
  }
  function isTruthOrEqual(result : boolean | EQ.EqualityResult): result is true | Variant<EQ.EqualityResult, 'Equal'> {
    return result === true || (typeof result === 'object' && result.$name === 'Equal');
  }
  function isFalseOrNotEqual(result : boolean | EQ.EqualityResult): result is false | Variant<EQ.EqualityResult, 'NotEqual'> {
    return result === false || (typeof result === 'object' && result.$name === 'NotEqual');
  }
  function isNeitherBooleanNorEqual(result : boolean | EQ.EqualityResult): boolean {
    return !(typeof result === 'boolean' || (typeof result === 'object' && result.$name === 'Equal'));
  }
  function isNeitherBooleanNorNotEqual(result : boolean | EQ.EqualityResult): boolean {
    return !(typeof result === 'boolean' || (typeof result === 'object' && result.$name === 'NotEqual'));
  }
  function isUserException(value : any) : value is { $name: 'user-exception', value: any } {
    return typeof value === 'object' && value.$name === 'user-exception';
  }
  return {
    runChecks: (moduleName: string, checks: TestCase[]) => {
      if (checkAll || (moduleName === mainModuleName)) {
        checks.forEach(c => {
          const { name, location, keywordCheck, run } = c;
          const resultsBefore = currentResults;
          currentResults = [];
          const result = runTask(run);
          if (result.$name === 'left') {
            addBlockResult(checkBlockResult(name, location, keywordCheck, currentResults, none));
          } else {
            addBlockResult(checkBlockResult(name, location, keywordCheck, currentResults, some(result.val)));
          }
          currentResults = resultsBefore;
        });
      }
    },
    checkIs: (left: TestThunk<any>, right: TestThunk<any>, loc: Srcloc) => {
      leftRightCheck(loc, left, right, (lv, rv) => {
        const eqLvRv = EQUALITY.equalAlways3(lv, rv);
        switch (eqLvRv.$name) {
          case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, 'on-left', rv, 'on-right')); break;
          case 'NotEqual': addResult(failureNotEqual(loc, none, lv, 'on-left', rv, 'on-right')); break
          case 'Equal': addResult(success(loc)); break;
          default: throw new ExhaustiveSwitchError(eqLvRv);
        }
      });
    },
    checkIsCause: (left: TestThunk<any>, right: TestThunk<any>, cause: TestThunk<any>, loc: Srcloc) => {
      leftRightCauseCheck(loc, left, right, cause, (lv, rv, cv) => {
        const eqCvRv = EQUALITY.equalAlways3(cv, rv)
        switch (eqCvRv.$name) {
          case 'Unknown': addResult(failureIsIncomparable(loc, eqCvRv, cv, 'on-cause', rv, 'on-right')); break;
          case 'NotEqual': addResult(failureNotEqual(loc, none, cv, 'on-cause', rv, 'on-right')); break;
          case 'Equal':
            const eqLvRv = EQUALITY.equalAlways3(lv, rv);
            switch (eqLvRv.$name) {
              case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, 'on-left', rv, 'on-right')); break;
              case 'NotEqual': addResult(failureNotEqual(loc, none, lv, 'on-left', rv, 'on-right')); break;
              case 'Equal': addResult(success(loc)); break;
              default: throw new ExhaustiveSwitchError(eqLvRv);
            }
            break;
          default: throw new ExhaustiveSwitchError(eqCvRv);
        }
      });
    },
    checkIsRoughly: (left: TestThunk<any>, right: TestThunk<any>, loc: Srcloc) => {
      // TODO(Ben/Joe): Update equality.ts to include the new roughlyEqual
      // and redefinition of within* as separated from withinRel*
      leftRightCheck(loc, left, right, (lv, rv) => {
        const eqLvRv = EQUALITY.within3(1e-6)(lv, rv);
        switch (eqLvRv.$name) {
          case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, 'on-left', rv, 'on-right')); break;
          case 'NotEqual': addResult(failureNotEqual(loc, none, lv, 'on-left', rv, 'on-right')); break
          case 'Equal': addResult(success(loc)); break;
          default: throw new ExhaustiveSwitchError(eqLvRv);
        }
      });
    },
    checkIsRoughlyCause: (left: TestThunk<any>, right: TestThunk<any>, cause: TestThunk<any>, loc: Srcloc) => {
      leftRightCauseCheck(loc, left, right, cause, (lv, rv, cv) => {
        const eqCvRv = EQUALITY.within3(1e-6)(cv, rv)
        switch (eqCvRv.$name) {
          case 'Unknown': addResult(failureIsIncomparable(loc, eqCvRv, cv, 'on-cause', rv, 'on-right')); break;
          case 'NotEqual': addResult(failureNotEqual(loc, none, cv, 'on-cause', rv, 'on-right')); break;
          case 'Equal':
            const eqLvRv = EQUALITY.within3(1e-6)(lv, rv);
            switch (eqLvRv.$name) {
              case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, 'on-left', rv, 'on-right')); break;
              case 'NotEqual': addResult(failureNotEqual(loc, none, lv, 'on-left', rv, 'on-right')); break;
              case 'Equal': addResult(success(loc)); break;
              default: throw new ExhaustiveSwitchError(eqLvRv);
            }
            break;
          default: throw new ExhaustiveSwitchError(eqCvRv);
        }
      });
    },
    checkIsNot: (left: TestThunk<any>, right: TestThunk<any>, loc: Srcloc) => {
      leftRightCheck(loc, left, right, (lv, rv) => {
        const eqLvRv = EQUALITY.equalAlways3(lv, rv);
        switch (eqLvRv.$name) {
          case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, 'on-left', rv, 'on-right')); break;
          case 'Equal': addResult(failureNotDifferent(loc, none, lv, 'on-left', rv, 'on-right')); break
          case 'NotEqual': addResult(success(loc)); break;
          default: throw new ExhaustiveSwitchError(eqLvRv);
        }
      });
    },
    checkIsNotCause: (left: TestThunk<any>, right: TestThunk<any>, cause: TestThunk<any>, loc: Srcloc) => {
      leftRightCauseCheck(loc, left, right, cause, (lv, rv, cv) => {
        const eqCvRv = EQUALITY.equalAlways3(cv, rv)
        switch (eqCvRv.$name) {
          case 'Unknown': addResult(failureIsIncomparable(loc, eqCvRv, cv, 'on-cause', rv, 'on-right')); break;
          case 'Equal': addResult(failureNotDifferent(loc, none, cv, 'on-cause', rv, 'on-right')); break;
          case 'NotEqual':
            const eqLvRv = EQUALITY.equalAlways3(lv, rv);
            switch (eqLvRv.$name) {
              case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, 'on-left', rv, 'on-right')); break;
              case 'Equal': addResult(failureNotDifferent(loc, none, lv, 'on-left', rv, 'on-right')); break;
              case 'NotEqual': addResult(success(loc)); break;
              default: throw new ExhaustiveSwitchError(eqLvRv);
            }
            break;
          default: throw new ExhaustiveSwitchError(eqCvRv);
        }
      });
    },
    checkIsNotRoughly: (left: TestThunk<any>, right: TestThunk<any>, loc: Srcloc) => {
      leftRightCheck(loc, left, right, (lv, rv) => {
        const eqLvRv = EQUALITY.within3(1e-6)(lv, rv);
        switch (eqLvRv.$name) {
          case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, 'on-left', rv, 'on-right')); break;
          case 'Equal': addResult(failureNotDifferent(loc, none, lv, 'on-left', rv, 'on-right')); break
          case 'NotEqual': addResult(success(loc)); break;
          default: throw new ExhaustiveSwitchError(eqLvRv);
        }
      });
    },
    checkIsNotRoughlyCause: (left: TestThunk<any>, right: TestThunk<any>, cause: TestThunk<any>, loc: Srcloc) => {
      leftRightCauseCheck(loc, left, right, cause, (lv, rv, cv) => {
        const eqCvRv = EQUALITY.within3(1e-6)(cv, rv)
        switch (eqCvRv.$name) {
          case 'Unknown': addResult(failureIsIncomparable(loc, eqCvRv, cv, 'on-cause', rv, 'on-right')); break;
          case 'Equal': addResult(failureNotDifferent(loc, none, cv, 'on-cause', rv, 'on-right')); break;
          case 'NotEqual':
            const eqLvRv = EQUALITY.equalAlways3(lv, rv);
            switch (eqLvRv.$name) {
              case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, 'on-left', rv, 'on-right')); break;
              case 'Equal': addResult(failureNotDifferent(loc, none, lv, 'on-left', rv, 'on-right')); break;
              case 'NotEqual': addResult(success(loc)); break;
              default: throw new ExhaustiveSwitchError(eqLvRv);
            }
            break;
          default: throw new ExhaustiveSwitchError(eqCvRv);
        }
      });
    },
    checkIsRefinement: (refinement: (left: any, right: any) => boolean | EQ.EqualityResult, left: TestThunk<any>, right: TestThunk<any>, loc: Srcloc) => {
      leftRightCheck(loc, left, right, (lv, rv) => {
        const refine = runTask(() => refinement(lv, rv));
        if (refine.$name === 'right') {
          const exn = refine.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, refinement, 2));
          } else {
            addResult(failureExn(loc, exn, 'on-refinement'));
          }
        } else {
          const testResult = refine.val;
          if (isUnknown(testResult)) {
            addResult(failureIsIncomparable(loc, testResult, lv, 'on-left', rv, 'on-right'));
          } else if (isFalseOrNotEqual(testResult)) {
            addResult(failureNotEqual(loc, some(refinement), lv, 'on-left', rv, 'on-right'));
          } else if (isNeitherBooleanNorEqual(testResult)) {
            addResult(errorNotBoolean(loc, refinement, lv, 'on-left', rv, 'on-right', testResult));
          } else {
            addResult(success(loc));
          }
        }
      });
    },
    checkIsRefinementCause: (refinement: (left: any, right: any) => boolean | EQ.EqualityResult, left: TestThunk<any>, right: TestThunk<any>, cause: TestThunk<any>, loc: Srcloc) => {
      leftRightCauseCheck(loc, left, right, cause, (lv, rv, cv) => {
        const refineCvRv = runTask(() => refinement(cv, rv)); // same orderas refinement(lv, rv)
        if (refineCvRv.$name === 'right') {
          const exn = refineCvRv.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, refinement, 2));
          } else {
            addResult(failureExn(loc, exn, 'on-refinement'));
          }
        } else {
          const causeResult = refineCvRv.val;
          if (isUnknown(causeResult)) {
            addResult(failureIsIncomparable(loc, causeResult, cv, 'on-cause', rv, 'on-right'));
          } else if (isFalseOrNotEqual(causeResult)) {
            addResult(failureNotEqual(loc, some(refinement), cv, 'on-cause', rv, 'on-right'));
          } else if (isNeitherBooleanNorEqual(causeResult)) {
            addResult(errorNotBoolean(loc, refinement, cv, 'on-cause', rv, 'on-right', causeResult));
          } else {
            const refine = runTask(() => refinement(lv, rv));
            if (refine.$name === 'right') {
              const exn = refine.val;
              if (causesErrorNotPred(exn)) {
                addResult(errorNotPred(loc, refinement, 2));
              } else {
                addResult(failureExn(loc, exn, 'on-refinement'));
              }
            } else {
              const testResult = refine.val;
              if (isUnknown(testResult)) {
                addResult(failureIsIncomparable(loc, testResult, lv, 'on-left', rv, 'on-right'));
              } else if (isFalseOrNotEqual(testResult)) {
                addResult(failureNotEqual(loc, some(refinement), lv, 'on-left', rv, 'on-right'));
              } else if (isNeitherBooleanNorEqual(testResult)) {
                addResult(errorNotBoolean(loc, refinement, lv, 'on-left', rv, 'on-right', testResult));
              } else {
                addResult(success(loc));
              }
            }
          }
        }
      });
    },
    checkIsNotRefinement: (refinement: (left: any, right: any) => boolean | EQ.EqualityResult, left: TestThunk<any>, right: TestThunk<any>, loc: Srcloc) => {
      leftRightCheck(loc, left, right, (lv, rv) => {
        const refine = runTask(() => refinement(lv, rv));
        if (refine.$name === 'right') {
          const exn = refine.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, refinement, 2));
          } else {
            addResult(failureExn(loc, exn, 'on-refinement'));
          }
        } else {
          const testResult = refine.val;
          if (isUnknown(testResult)) {
            addResult(failureIsIncomparable(loc, testResult, lv, 'on-left', rv, 'on-right'));
          } else if (isTruthOrEqual(testResult)) {
            addResult(failureNotDifferent(loc, some(refinement), lv, 'on-left', rv, 'on-right'));
          } else if (isNeitherBooleanNorNotEqual(testResult)) {
            addResult(errorNotBoolean(loc, refinement, lv, 'on-left', rv, 'on-right', testResult));
          } else {
            addResult(success(loc));
          }
        }
      });
    },
    checkIsNotRefinementCause: (refinement: (left: any, right: any) => boolean | EQ.EqualityResult, left: TestThunk<any>, right: TestThunk<any>, cause: TestThunk<any>, loc: Srcloc) => {
      leftRightCauseCheck(loc, left, right, cause, (lv, rv, cv) => {
        // NOTE(Ben): I think the commented out code below is equivalent to the 2x-duplicated-and-nested code below that.
        // I think this duplicated pattern might be the right approach to handling all the -cause variants.
        /*
        function check(toCheck: Thunk<boolean | EQ.EqualityResult>, blame: CheckOperand): boolean {
          const refineCvRv = runTask(toCheck);
          if (refineCvRv.$name === 'right') {
            const exn = refineCvRv.val;
            if (causesErrorNotPred(exn)) {
              addResult(errorNotPred(loc, refinement, 2));
            } else {
              addResult(failureExn(loc, exn, 'on-refinement'));
            }
          } else {
            const causeResult = refineCvRv.val;
            if (isUnknown(causeResult)) {
              addResult(failureIsIncomparable(loc, causeResult, cv, blame, rv, 'on-right'));
            } else if (isTruthOrEqual(causeResult)) {
              addResult(failureNotDifferent(loc, some(refinement), cv, blame, rv, 'on-right'));
            } else if (isNeitherBooleanNorNotEqual(causeResult)) {
              addResult(errorNotBoolean(loc, refinement, cv, blame, rv, 'on-right', causeResult));
            } else {
              return false;
            }
          }
          return true;
        }
        type OptBlame = { thunk: Thunk<boolean | EQ.EqualityResult>, blame: CheckOperand }
        function tryOptions(opts: OptBlame[]) {
          for (const {thunk, blame} of opts) {
            if (check(thunk, blame)) return;
          }
          addResult(success(loc));
        }
        tryOptions([
          {thunk: () => refinement(cv, rv), blame: 'on-cause'},
          {thunk: () => refinement(lv, rv), blame: 'on-left'}
        ]);
        */

        const refineCvRv = runTask(() => refinement(cv, rv)); // same order as refinement(lv, rv)
        if (refineCvRv.$name === 'right') {
          const exn = refineCvRv.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, refinement, 2));
          } else {
            addResult(failureExn(loc, exn, 'on-refinement'));
          }
        } else {
          const causeResult = refineCvRv.val;
          if (isUnknown(causeResult)) {
            addResult(failureIsIncomparable(loc, causeResult, cv, 'on-cause', rv, 'on-right'));
          } else if (isTruthOrEqual(causeResult)) {
            addResult(failureNotDifferent(loc, some(refinement), cv, 'on-cause', rv, 'on-right'));
          } else if (isNeitherBooleanNorNotEqual(causeResult)) {
            addResult(errorNotBoolean(loc, refinement, cv, 'on-cause', rv, 'on-right', causeResult));
          } else {
            const refine = runTask(() => refinement(lv, rv));
            if (refine.$name === 'right') {
              const exn = refine.val;
              if (causesErrorNotPred(exn)) {
                addResult(errorNotPred(loc, refinement, 2));
              } else {
                addResult(failureExn(loc, exn, 'on-refinement'));
              }
            } else {
              const testResult = refine.val;
              if (isUnknown(testResult)) {
                addResult(failureIsIncomparable(loc, testResult, lv, 'on-left', rv, 'on-right'));
              } else if (isTruthOrEqual(testResult)) {
                addResult(failureNotDifferent(loc, some(refinement), lv, 'on-left', rv, 'on-right'));
              } else if (isNeitherBooleanNorNotEqual(testResult)) {
                addResult(errorNotBoolean(loc, refinement, lv, 'on-left', rv, 'on-right', testResult));
              } else {
                addResult(success(loc));
              }
            }
          }
        }
      });
    },
    checkSatisfiesDelayed: (left: TestThunk<any>, pred: TestThunk<(v : any) => any>, loc: Srcloc) => {
      leftRightCheck(loc, left, pred, (lv, pv) => {
        const result = runTask(() => pv(lv));
        if (result.$name === 'right') {
          const exn = result.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, pv, 1));
          } else {
            addResult(failureExn(loc, exn, 'on-right'));
          }
        } else {
          const testResult = result.val;
          if (!(typeof testResult === 'boolean')) {
            addResult(errorNotBooleanPred(loc, pv, lv, 'on-left', testResult));
          } else if (!testResult) {
            addResult(failureNotSatisfied(loc, lv, 'on-left', pv));
          } else {
            addResult(success(loc));
          }
        }
      })
    },
    checkSatisfiesDelayedCause: (left: TestThunk<any>, pred: TestThunk<(v : any) => any>, cause: TestThunk<any>, loc: Srcloc) => {
      leftRightCauseCheck(loc, left, pred, cause, (lv, pv, cv) => {
        const resultCv = runTask(() => pv(cv));
        if (resultCv.$name === 'right') {
          const exn = resultCv.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, pv, 1));
          } else {
            addResult(failureExn(loc, exn, 'on-cause'));
          }
        } else {
          const causeResult = resultCv.val;
          if (!(typeof causeResult === 'boolean')) {
            addResult(errorNotBooleanPred(loc, pv, cv, 'on-cause', causeResult));
          } else if (!causeResult) {
            addResult(failureNotSatisfied(loc, cv, 'on-cause', pv));
          } else {
            const result = runTask(() => pv(lv));
            if (result.$name === 'right') {
              const exn = result.val;
              if (causesErrorNotPred(exn)) {
                addResult(errorNotPred(loc, pv, 1));
              } else {
                addResult(failureExn(loc, exn, 'on-right'));
              }
            } else {
              const testResult = result.val;
              if (!(typeof testResult === 'boolean')) {
                addResult(errorNotBooleanPred(loc, pv, lv, 'on-left', testResult));
              } else if (!testResult) {
                addResult(failureNotSatisfied(loc, lv, 'on-left', pv));
              } else {
                addResult(success(loc));
              }
            }
          }
        }
      })
    },
    checkSatisfiesNotDelayed: (left: TestThunk<any>, pred: TestThunk<(v : any) => any>, loc: Srcloc) => {
      leftRightCheck(loc, left, pred, (lv, pv) => {
        const result = runTask(() => pv(lv));
        if (result.$name === 'right') {
          const exn = result.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, pv, 1));
          } else {
            addResult(failureExn(loc, exn, 'on-right'));
          }
        } else {
          const testResult = result.val;
          if (!(typeof testResult === 'boolean')) {
            addResult(errorNotBooleanPred(loc, pv, lv, 'on-left', testResult));
          } else if (testResult) {
            addResult(failureNotDissatisfied(loc, lv, 'on-left', pv));
          } else {
            addResult(success(loc));
          }
        }
      })
    },
    checkSatisfiesNotDelayedCause: (left: TestThunk<any>, pred: TestThunk<(v : any) => any>, cause: TestThunk<any>, loc: Srcloc) => {
      leftRightCauseCheck(loc, left, pred, cause, (lv, pv, cv) => {
        const resultCv = runTask(() => pv(cv));
        if (resultCv.$name === 'right') {
          const exn = resultCv.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, pv, 1));
          } else {
            addResult(failureExn(loc, exn, 'on-cause'));
          }
        } else {
          const causeResult = resultCv.val;
          if (!(typeof causeResult === 'boolean')) {
            addResult(errorNotBooleanPred(loc, pv, cv, 'on-cause', causeResult));
          } else if (causeResult) {
            addResult(failureNotDissatisfied(loc, cv, 'on-cause', pv));
          } else {
            const result = runTask(() => pv(lv));
            if (result.$name === 'right') {
              const exn = result.val;
              if (causesErrorNotPred(exn)) {
                addResult(errorNotPred(loc, pv, 1));
              } else {
                addResult(failureExn(loc, exn, 'on-right'));
              }
            } else {
              const testResult = result.val;
              if (!(typeof testResult === 'boolean')) {
                addResult(errorNotBooleanPred(loc, pv, lv, 'on-left', testResult));
              } else if (testResult) {
                addResult(failureNotDissatisfied(loc, lv, 'on-left', pv));
              } else {
                addResult(success(loc));
              }
            }
          }
        }
      })
    },
    checkSatisfies: (left: any, pred: (v: any) => boolean, loc: Srcloc) => {
      // XXX Where is this ever called?
      // NOTE: Existing checker.arr omits the 'on-left' argument
      checkBool(loc, pred(left), () => failureNotSatisfied(loc, left, 'on-left', pred))
    },
    checkSatisfiesNot: (left: any, pred: (v: any) => boolean, loc: Srcloc) => {
      // XXX Where is this ever called?
      // NOTE: Existing checker.arr omits the 'on-left' argument
      checkBool(loc, !pred(left), () => failureNotDissatisfied(loc, left, 'on-left', pred))
    },
    checkRaisesStr: (thunk: Thunk<any>, str: string, loc: Srcloc) => {
      const result = runTask(thunk);
      if (result.$name === 'left') {
        addResult(failureNoExn(loc, some(str), 'on-left', true));
      } else {
        // TODO: is this the right way to call to-repr?  The types don't think it exists...
        if (!((RUNTIME['$torepr'](result.val) as string).includes(str))) {
          addResult(failureWrongExn(loc, str, result.val, 'on-left'));
        } else {
          addResult(success(loc));
        }
      }
    },
    checkRaisesStrCause: (thunk: Thunk<any>, str: string, cause: Thunk<any>, loc: Srcloc) => {
      const causeResult = runTask(cause);
      if (causeResult.$name === 'left') {
        addResult(failureNoExn(loc, some(str), 'on-cause', true));
      } else {
        // TODO: is this the right way to call to-repr?  The types don't think it exists...
        if (!((RUNTIME['$torepr'](causeResult.val) as string).includes(str))) {
          addResult(failureWrongExn(loc, str, causeResult.val, 'on-cause'));
        } else {
          const result = runTask(thunk);
          if (result.$name === 'left') {
            addResult(failureNoExn(loc, some(str), 'on-left', true));
          } else {
            // TODO: is this the right way to call to-repr?  The types don't think it exists...
            if (!((RUNTIME['$torepr'](result.val) as string).includes(str))) {
              addResult(failureWrongExn(loc, str, result.val, 'on-left'));
            } else {
              addResult(success(loc));
            }
          }
        }
      }
    },
    checkRaisesOtherStr: (thunk: Thunk<any>, str: string, loc: Srcloc) => {
      const result = runTask(thunk);
      if (result.$name === 'left') {
        addResult(failureNoExn(loc, some(str), 'on-left', true));
      } else {
        // TODO: is this the right way to call to-repr?  The types don't think it exists...
        if ((RUNTIME['$torepr'](result.val) as string).includes(str)) {
          addResult(failureRightExn(loc, str, result.val, 'on-left'));
        } else {
          addResult(success(loc));
        }
      }
    },
    checkRaisesOtherStrCause: (thunk: Thunk<any>, str: string, cause: Thunk<any>, loc: Srcloc) => {
      const causeResult = runTask(cause);
      if (causeResult.$name === 'left') {
        addResult(failureNoExn(loc, some(str), 'on-cause', true));
      } else {
        // TODO: is this the right way to call to-repr?  The types don't think it exists...
        if ((RUNTIME['$torepr'](causeResult.val) as string).includes(str)) {
          addResult(failureRightExn(loc, str, causeResult.val, 'on-cause'));
        } else {
          const result = runTask(thunk);
          if (result.$name === 'left') {
            addResult(failureNoExn(loc, some(str), 'on-left', true));
          } else {
            // TODO: is this the right way to call to-repr?  The types don't think it exists...
            if ((RUNTIME['$torepr'](result.val) as string).includes(str)) {
              addResult(failureRightExn(loc, str, result.val, 'on-left'));
            } else {
              addResult(success(loc));
            }
          }
        }
      }
    },
    checkRaisesNot: (thunk: Thunk<any>, loc: Srcloc) => {
      const result = runTask(thunk);
      if (result.$name === 'right') {
        addResult(failureExn(loc, result.val, 'on-left'));
      } else {
        addResult(success(loc));
      }
    },
    checkRaisesNotCause: (thunk: Thunk<any>, cause: Thunk<any>, loc: Srcloc) => {
      const causeResult = runTask(thunk);
      if (causeResult.$name === 'right') {
        addResult(failureExn(loc, causeResult.val, 'on-cause'));
      } else {
        const result = runTask(thunk);
        if (result.$name === 'right') {
          addResult(failureExn(loc, result.val, 'on-left'));
        } else {
          addResult(success(loc));
        }
        }
    },
    checkRaisesSatisfies: (thunk: Thunk<any>, pred: (v: any) => boolean, loc: Srcloc) => {
      const result = runTask(thunk);
      if (result.$name === 'left') {
        addResult(failureNoExn(loc, none, 'on-left', true));
      } else {
        const exn = result.val;
        const exnVal = (isUserException(exn) ? exn.value : exn);
        const exnResult = runTask(() => pred(exnVal));
        if (exnResult.$name === 'right') {
          const exn = exnResult.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, pred, 1));
          } else {
            addResult(failureExn(loc, exn, 'on-right'));
          }
        } else {
          const predResult = exnResult.val;
          if (!(typeof predResult === 'boolean')) {
            addResult(errorNotBooleanPred(loc, pred, exnVal, 'on-left', predResult));
          } else if (!predResult) {
            addResult(failureRaiseNotSatisfied(loc, exn, 'on-left', pred));
          } else {
            addResult(success(loc));
          }
        }
      }
    },
    checkRaisesSatisfiesCause: (thunk: Thunk<any>, pred: (v: any) => boolean, cause: Thunk<any>, loc: Srcloc) => {
      const causeResult = runTask(thunk);
      if (causeResult.$name === 'left') {
        addResult(failureNoExn(loc, none, 'on-cause', true));
      } else {
        const exn = causeResult.val;
        const exnVal = (isUserException(exn) ? exn.value : exn);
        const exnResult = runTask(() => pred(exnVal));
        if (exnResult.$name === 'right') {
          const exn = exnResult.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, pred, 1));
          } else {
            addResult(failureExn(loc, exn, 'on-right'));
          }
        } else {
          const predResult = exnResult.val;
          if (!(typeof predResult === 'boolean')) {
            addResult(errorNotBooleanPred(loc, pred, exnVal, 'on-cause', predResult));
          } else if (!predResult) {
            addResult(failureRaiseNotSatisfied(loc, exn, 'on-cause', pred));
          } else {
            const result = runTask(thunk);
            if (result.$name === 'left') {
              addResult(failureNoExn(loc, none, 'on-left', true));
            } else {
              const exn = result.val;
              const exnVal = (isUserException(exn) ? exn.value : exn);
              const exnResult = runTask(() => pred(exnVal));
              if (exnResult.$name === 'right') {
                const exn = exnResult.val;
                if (causesErrorNotPred(exn)) {
                  addResult(errorNotPred(loc, pred, 1));
                } else {
                  addResult(failureExn(loc, exn, 'on-right'));
                }
              } else {
                const predResult = exnResult.val;
                if (!(typeof predResult === 'boolean')) {
                  addResult(errorNotBooleanPred(loc, pred, exnVal, 'on-left', predResult));
                } else if (!predResult) {
                  addResult(failureRaiseNotSatisfied(loc, exn, 'on-left', pred));
                } else {
                  addResult(success(loc));
                }
              }
            }
          }
        }
      }
    },
    checkRaisesViolates: (thunk: Thunk<any>, pred: (v: any) => boolean, loc: Srcloc) => {
      const result = runTask(thunk);
      if (result.$name === 'left') {
        addResult(failureNoExn(loc, none, 'on-left', true));
      } else {
        const exn = result.val;
        const exnVal = (isUserException(exn) ? exn.value : exn);
        const exnResult = runTask(() => pred(exnVal));
        if (exnResult.$name === 'right') {
          const exn = exnResult.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, pred, 1));
          } else {
            addResult(failureExn(loc, exn, 'on-right'));
          }
        } else {
          const predResult = exnResult.val;
          if (!(typeof predResult === 'boolean')) {
            addResult(errorNotBooleanPred(loc, pred, exnVal, 'on-left', predResult));
          } else if (predResult) {
            addResult(failureRaiseNotDissatisfied(loc, exn, 'on-left', pred));
          } else {
            addResult(success(loc));
          }
        }
      }
    },
    checkRaisesViolatesCause: (thunk: Thunk<any>, pred: (v: any) => boolean, cause: Thunk<any>, loc: Srcloc) => {
      const causeResult = runTask(thunk);
      if (causeResult.$name === 'left') {
        addResult(failureNoExn(loc, none, 'on-cause', true));
      } else {
        const exn = causeResult.val;
        const exnVal = (isUserException(exn) ? exn.value : exn);
        const exnResult = runTask(() => pred(exnVal));
        if (exnResult.$name === 'right') {
          const exn = exnResult.val;
          if (causesErrorNotPred(exn)) {
            addResult(errorNotPred(loc, pred, 1));
          } else {
            addResult(failureExn(loc, exn, 'on-right'));
          }
        } else {
          const predResult = exnResult.val;
          if (!(typeof predResult === 'boolean')) {
            addResult(errorNotBooleanPred(loc, pred, exnVal, 'on-cause', predResult));
          } else if (predResult) {
            addResult(failureRaiseNotDissatisfied(loc, exn, 'on-cause', pred));
          } else {
            const result = runTask(thunk);
            if (result.$name === 'left') {
              addResult(failureNoExn(loc, none, 'on-left', true));
            } else {
              const exn = result.val;
              const exnVal = (isUserException(exn) ? exn.value : exn);
              const exnResult = runTask(() => pred(exnVal));
              if (exnResult.$name === 'right') {
                const exn = exnResult.val;
                if (causesErrorNotPred(exn)) {
                  addResult(errorNotPred(loc, pred, 1));
                } else {
                  addResult(failureExn(loc, exn, 'on-right'));
                }
              } else {
                const predResult = exnResult.val;
                if (!(typeof predResult === 'boolean')) {
                  addResult(errorNotBooleanPred(loc, pred, exnVal, 'on-left', predResult));
                } else if (predResult) {
                  addResult(failureRaiseNotDissatisfied(loc, exn, 'on-left', pred));
                } else {
                  addResult(success(loc));
                }
              }
            }
          }
        }
      }
    },
    results: function() { return blockResults; }
  }
}