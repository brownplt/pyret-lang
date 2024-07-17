import type * as EQ from './types/equality-types';
import type * as EQUALITY_TYPES from './equality';
import type * as RUNTIME_TYPES from './runtime';
import { ExhaustiveSwitchError, Srcloc } from './common-runtime-types';
import { displayToString } from './render-error-display';
import type * as ED_TYPES from '../runtime-arr/error-display.arr';
import type * as SL_TYPES from '../runtime-arr/srcloc.arr';
import type * as OPT_TYPES from '../runtime-arr/option.arr';
const ED = require('./error-display' + '.arr.js') as typeof ED_TYPES;
const EQUALITY = require("./equality.js") as typeof EQUALITY_TYPES;
const RUNTIME = require('./runtime') as typeof RUNTIME_TYPES;
const srcloc = require('./srcloc' + '.arr.js') as typeof SL_TYPES;
const option = require('./option' + '.arr.js') as typeof OPT_TYPES;

// TODO: import this from somewhere in the runtime
type Variant<T, V> = T & { $name: V };

function some<A>(value: A): Variant<OPT_TYPES.Option<A>, 'some'> {
  return option.some(value);
}
const none = option.none;

type Thunk<A> = () => A;
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
  maybeErr: OPT_TYPES.Option<any>, 
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
  refinement: OPT_TYPES.Option<any>,
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
  exnExpected: OPT_TYPES.Option<string>,
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

/**
 * At compile time, we embed the source locations of the various components
 * of the check-test into the call to the checker library, so that at runtime,
 * we do not need to re-parse the check-test in order to find where the components
 * are.
 * 
 * TODO: Actually do this!
 */
type LocsRecord = Record<CheckOperand, Srcloc | undefined>

export function makeCheckContext(mainModuleName: string, checkAll: boolean) {
  const blockResults: CheckBlockResult[] = [];
  function addBlockResult(cbr : CheckBlockResult) {
    blockResults.push(cbr);
  }
  let currentResults: TestResult[] = [];
  function addResult(tr : TestResult) {
    currentResults.push(tr);
  }
  function unthunk<T>(loc: Srcloc, locs: LocsRecord, where: CheckOperand, thunk: Thunk<T>, cont: (val: T) => any) {
    const result = runTask(thunk);
    if (result.$name === 'right') {
      addResult(failureExn(loc, result.val, where));
    } else {
      cont(result.val);
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

  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsCont(loc: Srcloc, locs: LocsRecord, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
    const eqLvRv = EQUALITY.equalAlways3(lv, rv);
    switch (eqLvRv.$name) {
      case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, lvSrc, rv, rvSrc)); break;
      case 'NotEqual': addResult(failureNotEqual(loc, none, lv, lvSrc, rv, rvSrc)); break
      case 'Equal': cont(); break;
      default: throw new ExhaustiveSwitchError(eqLvRv);
    }
  }
  const CHECK_IS = {
    checkIs: (left: Thunk<any>, right: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          checkIsCont(loc, locs, lv, 'on-left', rv, 'on-right', () => addResult(success(loc)))));
    },
    checkIsCause: (left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          unthunk(loc, locs, 'on-cause', cause, (cv) =>
            checkIsCont(loc, locs, cv, 'on-cause', rv, 'on-right', () =>
              checkIsCont(loc, locs, lv, 'on-left', rv, 'on-right', () => 
                addResult(success(loc)))))));
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsRoughlyCont(loc: Srcloc, locs: LocsRecord, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
    // TODO(Ben/Joe): Update equality.ts to include the new roughlyEqual
    // and redefinition of within* as separated from withinRel*
    const eqLvRv = EQUALITY.within3(1e-6)(lv, rv);
    switch (eqLvRv.$name) {
      case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, lvSrc, rv, rvSrc)); break;
      case 'NotEqual': addResult(failureNotEqual(loc, none, lv, lvSrc, rv, rvSrc)); break
      case 'Equal': cont(); break;
      default: throw new ExhaustiveSwitchError(eqLvRv);
    }
  };
  const CHECK_IS_ROUGHLY = {
    checkIsRoughly: (left: Thunk<any>, right: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          checkIsRoughlyCont(loc, locs, lv, 'on-left', rv, 'on-right', () => addResult(success(loc)))));
    },
    checkIsRoughlyCause: (left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          unthunk(loc, locs, 'on-cause', cause, (cv) =>
            checkIsRoughlyCont(loc, locs, cv, 'on-cause', rv, 'on-right', () =>
              checkIsRoughlyCont(loc, locs, lv, 'on-left', rv, 'on-right', () => 
                addResult(success(loc)))))));
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsNotCont(loc: Srcloc, locs: LocsRecord, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
    const eqLvRv = EQUALITY.equalAlways3(lv, rv);
    switch (eqLvRv.$name) {
      case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, lvSrc, rv, rvSrc)); break;
      case 'Equal': addResult(failureNotDifferent(loc, none, lv, lvSrc, rv, rvSrc)); break
      case 'NotEqual': cont(); break;
      default: throw new ExhaustiveSwitchError(eqLvRv);
    }
  }
  const CHECK_IS_NOT = {
    checkIsNot: (left: Thunk<any>, right: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          checkIsNotCont(loc, locs, lv, 'on-left', rv, 'on-right', () => addResult(success(loc)))));
    },
    checkIsNotCause: (left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          unthunk(loc, locs, 'on-cause', cause, (cv) =>
            checkIsNotCont(loc, locs, cv, 'on-cause', rv, 'on-right', () =>
              checkIsNotCont(loc, locs, lv, 'on-left', rv, 'on-right', () => 
                addResult(success(loc)))))));
    },
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsNotRoughlyCont(loc: Srcloc, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
    const eqLvRv = EQUALITY.within3(1e-6)(lv, rv);
    switch (eqLvRv.$name) {
      case 'Unknown': addResult(failureIsIncomparable(loc, eqLvRv, lv, lvSrc, rv, rvSrc)); break;
      case 'Equal': addResult(failureNotDifferent(loc, none, lv, lvSrc, rv, rvSrc)); break
      case 'NotEqual': cont(); break;
      default: throw new ExhaustiveSwitchError(eqLvRv);
    }
  };
  const CHECK_IS_NOT_ROUGHLY = {
    checkIsNotRoughly: (left: Thunk<any>, right: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          checkIsNotRoughlyCont(loc, lv, 'on-left', rv, 'on-right', () => addResult(success(loc)))));
    },
    checkIsNotRoughlyCause: (left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          unthunk(loc, locs, 'on-cause', cause, (cv) =>
            checkIsNotRoughlyCont(loc, cv, 'on-cause', rv, 'on-right', () =>
              checkIsNotRoughlyCont(loc, lv, 'on-left', rv, 'on-right', () => 
                addResult(success(loc)))))));
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsRefinementCont(loc: Srcloc, locs: LocsRecord, refinement: (left: any, right: any) => boolean | EQ.EqualityResult, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
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
        addResult(failureIsIncomparable(loc, testResult, lv, lvSrc, rv, rvSrc));
      } else if (isFalseOrNotEqual(testResult)) {
        addResult(failureNotEqual(loc, some(refinement), lv, lvSrc, rv, rvSrc));
      } else if (isNeitherBooleanNorEqual(testResult)) {
        addResult(errorNotBoolean(loc, refinement, lv, lvSrc, rv, rvSrc, testResult));
      } else {
        cont();
      }
    }
  }
  const CHECK_IS_REFINEMENT = {
    checkIsRefinement: (refinement: Thunk<(left: any, right: any) => boolean | EQ.EqualityResult>, left: Thunk<any>, right: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          unthunk(loc, locs, 'on-refinement', refinement, (refv) => 
            checkIsRefinementCont(loc, locs, refv, lv, 'on-left', rv, 'on-right', () =>
              addResult(success(loc))))))
    },
    checkIsRefinementCause: (refinement: Thunk<(left: any, right: any) => boolean | EQ.EqualityResult>, left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          unthunk(loc, locs, 'on-cause', cause, (cv) => 
            unthunk(loc, locs, 'on-refinement', refinement, (refv) => 
              checkIsRefinementCont(loc, locs, refv, cv, 'on-cause', rv, 'on-right', () => 
                checkIsRefinementCont(loc, locs, refv, lv, 'on-left', rv, 'on-right', () =>
                  addResult(success(loc))))))))
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsNotRefinementCont(loc: Srcloc, locs: LocsRecord, refinement: (left: any, right: any) => boolean | EQ.EqualityResult, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
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
        addResult(failureIsIncomparable(loc, testResult, lv, lvSrc, rv, rvSrc));
      } else if (isTruthOrEqual(testResult)) {
        addResult(failureNotDifferent(loc, some(refinement), lv, lvSrc, rv, rvSrc));
      } else if (isNeitherBooleanNorNotEqual(testResult)) {
        addResult(errorNotBoolean(loc, refinement, lv, lvSrc, rv, rvSrc, testResult));
      } else {
        cont();
      }
    }
  }
  const CHECK_IS_NOT_REFINEMENT = {
    checkIsNotRefinement: (refinement: Thunk<(left: any, right: any) => boolean | EQ.EqualityResult>, left: Thunk<any>, right: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          unthunk(loc, locs, 'on-refinement', refinement, (refv) => 
            checkIsNotRefinementCont(loc, locs, refv, lv, 'on-left', rv, 'on-right', () =>
              addResult(success(loc))))))
    },
    checkIsNotRefinementCause: (refinement: Thunk<(left: any, right: any) => boolean | EQ.EqualityResult>, left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', right, (rv) =>
          unthunk(loc, locs, 'on-cause', cause, (cv) => 
            unthunk(loc, locs, 'on-refinement', refinement, (refv) => 
              checkIsNotRefinementCont(loc, locs, refv, cv, 'on-cause', rv, 'on-right', () => 
                checkIsNotRefinementCont(loc, locs, refv, lv, 'on-left', rv, 'on-right', () =>
                  addResult(success(loc))))))))
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkSatisfiesDelayedCont(loc: Srcloc, locs: LocsRecord, lv: any, lvSrc: CheckOperand, pv: (v: any) => any, pvSrc: CheckOperand, cont: Thunk<any>) {
    const result = runTask(() => pv(lv));
    if (result.$name === 'right') {
      const exn = result.val;
      if (causesErrorNotPred(exn)) {
        addResult(errorNotPred(loc, pv, 1));
      } else {
        addResult(failureExn(loc, exn, pvSrc));
      }
    } else {
      const testResult = result.val;
      if (!(typeof testResult === 'boolean')) {
        addResult(errorNotBooleanPred(loc, pv, lv, lvSrc, testResult));
      } else if (!testResult) {
        addResult(failureNotSatisfied(loc, lv, lvSrc, pv));
      } else {
        cont();
      }
    }
  }
  const CHECK_SATISFIES_DELAYED = {
    checkSatisfiesDelayed: (left: Thunk<any>, pred: Thunk<(v : any) => any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', pred, (pv) =>
          checkSatisfiesDelayedCont(loc, locs, lv, 'on-left', pv, 'on-right', () =>
            addResult(success(loc)))))
    },
    checkSatisfiesDelayedCause: (left: Thunk<any>, pred: Thunk<(v : any) => any>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', pred, (pv) =>
          unthunk(loc, locs, 'on-cause', cause, (cv) => 
            checkSatisfiesDelayedCont(loc, locs, cv, 'on-cause', pv, 'on-right', () =>
              checkSatisfiesDelayedCont(loc, locs, lv, 'on-left', pv, 'on-right', () =>
                addResult(success(loc)))))))
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkSatisfiesNotDelayedCont(loc: Srcloc, locs: LocsRecord, lv: any, lvSrc: CheckOperand, pv: (v: any) => any, pvSrc: CheckOperand, cont: Thunk<any>) {
    const result = runTask(() => pv(lv));
    if (result.$name === 'right') {
      const exn = result.val;
      if (causesErrorNotPred(exn)) {
        addResult(errorNotPred(loc, pv, 1));
      } else {
        addResult(failureExn(loc, exn, pvSrc));
      }
    } else {
      const testResult = result.val;
      if (!(typeof testResult === 'boolean')) {
        addResult(errorNotBooleanPred(loc, pv, lv, lvSrc, testResult));
      } else if (testResult) {
        addResult(failureNotDissatisfied(loc, lv, lvSrc, pv));
      } else {
        cont();
      }
    }
}
  const CHECK_SATISFIES_NOT_DELAYED = {
    checkSatisfiesNotDelayed: (left: Thunk<any>, pred: Thunk<(v : any) => any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', pred, (pv) =>
          checkSatisfiesNotDelayedCont(loc, locs, lv, 'on-left', pv, 'on-right', () =>
            addResult(success(loc)))))
    },
    checkSatisfiesNotDelayedCause: (left: Thunk<any>, pred: Thunk<(v : any) => any>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-left', left, (lv) =>
        unthunk(loc, locs, 'on-right', pred, (pv) =>
          unthunk(loc, locs, 'on-cause', cause, (cv) => 
            checkSatisfiesNotDelayedCont(loc, locs, cv, 'on-cause', pv, 'on-right', () =>
              checkSatisfiesNotDelayedCont(loc, locs, lv, 'on-left', pv, 'on-right', () =>
                addResult(success(loc)))))))
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkRaisesStrCont(loc: Srcloc, locs: LocsRecord, thunk: any, thunkSrc: CheckOperand, str: string, cont: Thunk<any>) {
    const result = runTask(thunk);
    if (result.$name === 'left') {
      addResult(failureNoExn(loc, some(str), thunkSrc, true));
    } else {
      // TODO: is this the right way to call to-repr?  The types don't think it exists...
      console.log("Result form an exn test:" , result.val, (RUNTIME['$torepr'](result.val)));
      if (!((RUNTIME['$torepr'](result.val) as string).includes(str)) &&
          !(String(result.val).includes(str))) {
        addResult(failureWrongExn(loc, str, result.val, thunkSrc));
      } else {
        cont();
      }
    }
  }
  const CHECK_RAISES_STR = {
    checkRaisesStr: (thunk: Thunk<any>, str: Thunk<string>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-right', str, (sv) =>
        checkRaisesStrCont(loc, locs, thunk, 'on-left', sv, () =>
          addResult(success(loc))))
    },
    checkRaisesStrCause: (thunk: Thunk<any>, str: Thunk<string>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-right', str, (sv) =>
        checkRaisesStrCont(loc, locs, cause, 'on-cause', sv, () =>
          checkRaisesStrCont(loc, locs, thunk, 'on-left', sv, () =>
            addResult(success(loc)))))
    },
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkRaisesOtherStrCont(loc: Srcloc, locs: LocsRecord, thunk: any, thunkSrc: CheckOperand, str: string, cont: Thunk<any>) {
    const result = runTask(thunk);
    if (result.$name === 'left') {
      addResult(failureNoExn(loc, some(str), thunkSrc, true));
    } else {
      // TODO: is this the right way to call to-repr?  The types don't think it exists...
      if ((RUNTIME['$torepr'](result.val) as string).includes(str)) {
        addResult(failureRightExn(loc, str, result.val, thunkSrc));
      } else {
        cont();
      }
    }
  }
  const CHECK_RAISES_OTHER_STR = {
    checkRaisesOtherStr: (thunk: Thunk<any>, str: Thunk<string>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-right', str, (sv) =>
        checkRaisesOtherStrCont(loc, locs, thunk, 'on-left', sv, () =>
          addResult(success(loc))))
    },
    checkRaisesOtherStrCause: (thunk: Thunk<any>, str: Thunk<string>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-right', str, (sv) =>
        checkRaisesOtherStrCont(loc, locs, cause, 'on-cause', sv, () =>
          checkRaisesOtherStrCont(loc, locs, thunk, 'on-left', sv, () =>
            addResult(success(loc)))))
    },
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkRaisesNotCont(loc: Srcloc, locs: LocsRecord, thunk: any, thunkSrc: CheckOperand, cont: Thunk<any>) {
    const result = runTask(thunk);
    if (result.$name === 'right') {
      addResult(failureExn(loc, result.val, thunkSrc));
    } else {
      cont();
    }
}
  const CHECK_RAISES_NOT = {
    checkRaisesNot: (thunk: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      checkRaisesNotCont(loc, locs, thunk, 'on-left', () =>
        addResult(success(loc)))
    },
    checkRaisesNotCause: (thunk: Thunk<any>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      checkRaisesNotCont(loc, locs, cause, 'on-cause', () =>
        checkRaisesNotCont(loc, locs, thunk, 'on-left', () =>
          addResult(success(loc))))
    },
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkRaisesSatisfiesCont(loc: Srcloc, locs: LocsRecord, thunk: Thunk<any>, thunkSrc: CheckOperand, pred: (v: any) => boolean, predSrc: CheckOperand, cont: Thunk<any>) {
    const result = runTask(thunk);
    if (result.$name === 'left') {
      addResult(failureNoExn(loc, none, thunkSrc, true));
    } else {
      const exn = result.val;
      const exnVal = (isUserException(exn) ? exn.value : exn);
      const exnResult = runTask(() => pred(exnVal));
      if (exnResult.$name === 'right') {
        const exn = exnResult.val;
        if (causesErrorNotPred(exn)) {
          addResult(errorNotPred(loc, pred, 1));
        } else {
          addResult(failureExn(loc, exn, predSrc));
        }
      } else {
        const predResult = exnResult.val;
        if (!(typeof predResult === 'boolean')) {
          addResult(errorNotBooleanPred(loc, pred, exnVal, thunkSrc, predResult));
        } else if (!predResult) {
          addResult(failureRaiseNotSatisfied(loc, exn, thunkSrc, pred));
        } else {
          cont();
        }
      }
    }
  }
  const CHECK_RAISES_SATISFIES = {
    checkRaisesSatisfies: (thunk: Thunk<any>, pred: Thunk<(v: any) => boolean>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-right', pred, (pv) => 
        checkRaisesSatisfiesCont(loc, locs, thunk, 'on-left', pv, 'on-right', () =>
          addResult(success(loc))))
    },
    checkRaisesSatisfiesCause: (thunk: Thunk<any>, pred: Thunk<(v: any) => boolean>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-right', pred, (pv) => 
        checkRaisesSatisfiesCont(loc, locs, cause, 'on-cause', pv, 'on-right', () => 
          checkRaisesSatisfiesCont(loc, locs, thunk, 'on-left', pv, 'on-right', () =>
            addResult(success(loc)))))
    },
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkRaisesViolatesCont(loc: Srcloc, locs: LocsRecord, thunk: Thunk<any>, thunkSrc: CheckOperand, pred: (v: any) => boolean, predSrc: CheckOperand, cont: Thunk<any>) {
    const result = runTask(thunk);
    if (result.$name === 'left') {
      addResult(failureNoExn(loc, none, thunkSrc, true));
    } else {
      const exn = result.val;
      const exnVal = (isUserException(exn) ? exn.value : exn);
      const exnResult = runTask(() => pred(exnVal));
      if (exnResult.$name === 'right') {
        const exn = exnResult.val;
        if (causesErrorNotPred(exn)) {
          addResult(errorNotPred(loc, pred, 1));
        } else {
          addResult(failureExn(loc, exn, predSrc));
        }
      } else {
        const predResult = exnResult.val;
        if (!(typeof predResult === 'boolean')) {
          addResult(errorNotBooleanPred(loc, pred, exnVal, thunkSrc, predResult));
        } else if (predResult) {
          addResult(failureRaiseNotDissatisfied(loc, exn, thunkSrc, pred));
        } else {
          cont();
        }
      }
    }
  }
  const CHECK_RAISES_VIOLATES = {
    checkRaisesViolates: (thunk: Thunk<any>, pred: Thunk<(v: any) => boolean>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-right', pred, (pv) => 
        checkRaisesViolatesCont(loc, locs, thunk, 'on-left', pv, 'on-right', () =>
          addResult(success(loc))))
    },
    checkRaisesViolatesCause: (thunk: Thunk<any>, pred: Thunk<(v: any) => boolean>, cause: Thunk<any>, loc: Srcloc, locs: LocsRecord) => {
      unthunk(loc, locs, 'on-right', pred, (pv) => 
        checkRaisesViolatesCont(loc, locs, cause, 'on-cause', pv, 'on-right', () => 
          checkRaisesViolatesCont(loc, locs, thunk, 'on-left', pv, 'on-right', () =>
            addResult(success(loc)))))
    },
  };

  return {
    runChecks: (moduleName: string, checks: TestCase[]) => {
      console.log(`Running a check-block for ${moduleName} while in mainModule ${mainModuleName}`);
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
          console.log("At end of runChecks, currentResults =", currentResults);
          currentResults = resultsBefore;
        });
      }
    },
    ...CHECK_IS,
    ...CHECK_IS_ROUGHLY,
    ...CHECK_IS_NOT,
    ...CHECK_IS_NOT_ROUGHLY,
    ...CHECK_IS_REFINEMENT,
    ...CHECK_IS_NOT_REFINEMENT,
    ...CHECK_SATISFIES_DELAYED,
    ...CHECK_SATISFIES_NOT_DELAYED,
    ...CHECK_RAISES_STR,
    ...CHECK_RAISES_OTHER_STR,
    ...CHECK_RAISES_NOT,
    ...CHECK_RAISES_SATISFIES,
    ...CHECK_RAISES_VIOLATES,
    results: function() { 
      console.log(`In results() for ${mainModuleName}`, blockResults);
      return blockResults; 
    },
    resultsSummary: function() {
      return resultsSummary(blockResults);
    }

  }
}

/*
# NOTE(joe): get-stack lets us hide the stack from Pyret's semantics, and
# require that magical callers provide a get-stack function that produces
# the list of locations to render
fun results-summary(block-results :: List<CheckBlockResult>, get-stack):
  init = {
      message: "",
      errored: 0,
      passed: 0,
      failed: 0,
      total: 0
    }
  complete-summary = for fold(summary from init, br from block-results.reverse()):
    block-summary = for fold(s from init, tr from br.test-results.reverse()):
      cases(TestResult) tr:
        | success(loc) => s.{
            message: s.message + "\n  " + loc.format(false) + ": ok",
            passed: s.passed + 1,
            total: s.total + 1
          }
        | else =>
          stack = tr.access-stack(get-stack)
          m = s.message + "\n  " + tr.loc.format(false) + ": failed because: \n    "
            + RED.display-to-string(tr.render-reason(), torepr, stack)
          s.{
            message: m,
            failed: s.failed + 1,
            total: s.total + 1
          }
      end
    end
    block-type = if br.keyword-check: "Check" else: "Examples" end
    ended-in-error = cases(Option) br.maybe-err:
      | none => ""
      | some(err) =>
        stack = get-stack(err)
        "\n  " + block-type + " block ended in the following error (not all tests may have run): \n\n  "
          + RED.display-to-string(exn-unwrap(err).render-reason(), torepr, stack)
          + RED.display-to-string(ED.v-sequence(map(ED.loc, stack)), torepr, empty)
          + "\n\n"
    end
    message = summary.message + "\n\n" + br.loc.format(true) + ": " + br.name + " (" + tostring(block-summary.passed) + "/" + tostring(block-summary.total) + ") \n"
    with-error-notification = message + ended-in-error
    rest-of-message =
      if block-summary.failed == 0: ""
      else: block-summary.message
      end
    {
      message: with-error-notification + rest-of-message,
      errored: summary.errored + if is-some(br.maybe-err): 1 else: 0 end,
      passed: summary.passed + block-summary.passed,
      failed: summary.failed + block-summary.failed,
      total: summary.total + block-summary.total
    }
  end
  if (complete-summary.total == 0) and (complete-summary.errored == 0):
    complete-summary.{message: "The program didn't define any tests."}
  else if (complete-summary.failed == 0) and (complete-summary.errored == 0):
    happy-msg = if complete-summary.passed == 1:
        "Looks shipshape, your test passed, mate!"
      else:
        "Looks shipshape, all " + tostring(complete-summary.passed) + " tests passed, mate!"
      end
    complete-summary.{message: happy-msg}
  else:
    c = complete-summary
    c.{
      message: c.message + "\n\nPassed: " + tostring(c.passed) + "; Failed: " + tostring(c.failed) + "; Ended in Error: " + tostring(c.errored) + "; Total: " + tostring(c.total) + "\n"
    }
  end
end


*/

function makeSrcloc(loc: Srcloc): any {
  if (loc.length === 1) {
    return srcloc.builtin(loc[0]);
  } else {
    return srcloc.srcloc(loc[0], loc[1], loc[2], loc[3], loc[4], loc[5], loc[6]);
  }
}

export function resultsSummary(blockResults : CheckBlockResult[]) {
  let message = "", errored = 0, passed = 0, failed = 0, total = 0;
  blockResults.forEach(br => {
    const { name, loc, isKeywordCheck, testResults, maybeErr } = br;
    let blockMessage = "", blockPassed = 0, blockFailed = 0, blockTotal = 0;
    testResults.forEach(tr => {
      switch(tr.$name) {
        case 'success':
          blockMessage += "\n  " + makeSrcloc(tr.loc).format(false) + ": ok";
          blockPassed++;
          blockTotal++;
          break;
        default:
          const m = makeSrcloc(tr.loc).format(false) + ": failed because: \n    " + displayToString(renderReason(tr), RUNTIME['$torepr']);
          blockMessage += "\n  " + m;
          blockFailed++;
          blockTotal++;
      }
    });
    let blockType = isKeywordCheck ? "Check" : "Examples";
    let endedInError = maybeErr.$name === 'some' ? "\n  " + blockType + " block ended in the following error (not all tests may have run): \n\n  " +
      displayToString(renderReason(maybeErr.value), RUNTIME['$torepr']) + displayToString(ED['v-sequence'](maybeErr.value.stack.map(makeSrcloc)), RUNTIME['$torepr']) + "\n\n" : "";
    message += "\n\n" + makeSrcloc(loc).format(true) + ": " + name + " (" + passed + "/" + total + ") \n";
    message += endedInError;
    if (failed > 0) {
      message += blockMessage;
    }
    errored += maybeErr.$name === 'some' ? 1 : 0;
    passed += blockPassed;
    failed += blockFailed;
    total += blockTotal;
  });
  if(total === 0 && errored === 0) {
    return {
      message: "The program didn't define any tests.",
      errored,
      passed,
      failed,
      total
    };
  }
  if(failed === 0 && errored === 0) {
    const happyMsg = passed === 1 ? "Looks shipshape, your test passed, mate!" : "Looks shipshape, all " + passed + " tests passed, mate!";
    return {
      message: happyMsg,
      errored,
      passed,
      failed,
      total
    };
  }
  message += "\n\nPassed: " + passed + "; Failed: " + failed + "; Ended in Error: " + errored + "; Total: " + total + "\n";
  return {
    message,
    errored,
    passed,
    failed,
    total
  };
}

export type Checker = ReturnType<typeof makeCheckContext>;
// export type Failure =
//   | { $name: 'paragraph', 'contents': Array<Failure> }
//   | { $name: 'bulleted-sequence', 'contents': Array<Failure> }
//   | { $name: 'v-sequence', 'contents': Array<Failure> }
//   | { $name: 'h-sequence', 'contents': Array<Failure>, 'sep': string }
//   | {
//     $name: 'h-sequence-sep',
//     'contents': Array<Failure>, 'sep': string, 'last': string }
//   | { $name: 'embed', 'val': any }
//   | { $name: 'text', 'str': string }
//   | { $name: 'loc', 'loc': Srcloc }
//   | { $name: 'code', 'contents': Failure }
//   | { $name: 'cmcode', 'loc': Srcloc }
//   | {
//     $name: 'loc-display',
//     'loc': Srcloc, 'style': string, 'contents': Failure }
//   | { $name: 'optional', 'contents': Failure }
//   | {
//     $name: 'highlight',
//     'contents': Failure, 'locs': Array<Srcloc>, 'color': Number }
//   | {
//     $name: 'maybe-stack-loc',
//     n: number, 'user-frames-only': boolean, 'contents-with-loc': (l : Srcloc) => Failure,
//     'contents-without-loc': Failure
//   };

export function renderReason(testResult: TestResult): ED_TYPES.ErrorDisplay {
    switch(testResult.$name) {
      case 'failure-not-equal':
        return ED.error.make([
          // ED.para.make([,
          //   testResult.refinement.$name === 'none' ? ED.text("Values not equal") : ED.text("Values not equal, using custom equality"),
          //   ED.embed(testResult.left),
          //   ED.embed(testResult.right)
          // ])
        ]);
      case 'success':
      case 'failure-is-incomparable':
      case 'failure-not-different':
      case 'failure-not-satisfied':
      case 'failure-not-dissatisfied':
      case 'failure-wrong-exn':
      case 'failure-right-exn':
      case 'failure-exn':
      case 'failure-no-exn':
      case 'failure-raise-not-satisfied':
      case 'failure-raise-not-dissatisfied':
      case 'error-not-boolean':
      case 'error-not-pred':
      case 'error-not-boolean-pred':
        throw new Error('renderReason not implemented for ' + testResult.$name);
    }
}

export function renderFancyReason(testResult: TestResult) : ED_TYPES.ErrorDisplay {
  throw new Error('renderFancyReason Not implemented');
}
