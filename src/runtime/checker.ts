import type * as EQ from './types/equality-types';
import * as EQUALITY from './equality';
import type { Variant } from './types/primitive-types';
import * as RUNTIME from './runtime';
import * as NUMBER  from "./js-numbers";
import { ExhaustiveSwitchError, Srcloc } from './common-runtime-types';
import { displayToString } from './render-error-display';
import type * as ED_TYPES from '../runtime-arr/error-display.arr';
import type * as SL_TYPES from '../runtime-arr/srcloc.arr';
import type * as OPT_TYPES from '../runtime-arr/option.arr';
const ED = require('./error-display' + '.arr.js') as typeof ED_TYPES;
const srcloc = require('./srcloc' + '.arr.js') as typeof SL_TYPES;
const option = require('./option' + '.arr.js') as typeof OPT_TYPES;

type Option<A> = OPT_TYPES.Option<A>;
type ErrorDisplay = ED_TYPES.ErrorDisplay;


function some<A>(value: A): Variant<OPT_TYPES.Option<A>, 'some'> {
  return option.some(value);
}
const none = option.none;

type Thunk<A> = () => A;

export type IsOp = 'op==' | 'op=~' | 'op<=>'

export function getOpFunName(opname : IsOp) : string {
  switch(opname) {
    case 'op==': return 'equal-always';
    case 'op=~': return 'equal-now';
    case 'op<=>': return 'identical';
    default: throw new ExhaustiveSwitchError(opname);
  }
}

type CheckOp = "s-op-is" | "s-op-is-roughly" | "s-op-is-not-roughly" | "s-op-is-op" | "s-op-is-not"
| "s-op-is-not-op" | "s-op-satisfies" | "s-op-satisfies-not"
| "s-op-raises" | "s-op-raises-other" | "s-op-raises-not" | "s-op-raises-satisfies"
| "s-op-raises-violates"

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

export type RenderedCheckResultsAndSummary = { checkSummary: ReturnType<typeof resultsSummary>, renderedChecks: RenderedCheckBlockResult[] }
export type RenderedCheckBlockResult = Omit<CheckBlockResult, 'testResults'> & { testResults: RenderedTestResult[] };
export type RenderedTestResult = TestResult & { rendered: ErrorDisplay };

function success(
  metadata: LocsRecord,
) {
  return { $name: 'success' as const, metadata };
}
function failureNotEqual(
  metadata: LocsRecord,
  refinement: any,
  left: any,
  leftSrc: CheckOperand,
  right: any,
  rightSrc: CheckOperand
) {
  return { $name: 'failure-not-equal' as const, refinement, metadata, left, leftSrc, right, rightSrc };
}
function failureIsIncomparable(
  metadata: LocsRecord,
  eqResult: Variant<EQ.EqualityResult, 'Unknown'>,
  left: any,
  leftSrc: CheckOperand,
  right: any,
  rightSrc: CheckOperand
) {
  return { $name: 'failure-is-incomparable' as const, metadata, eqResult, left, leftSrc, right, rightSrc };
}
function failureNotDifferent(
  metadata: LocsRecord,
  refinement: Option<any>,
  left: any,
  leftSrc: CheckOperand,
  right: any,
  rightSrc: CheckOperand
) {
  return { $name: 'failure-not-different' as const, metadata, refinement, left, leftSrc, right, rightSrc };
}
function failureNotSatisfied(
  metadata: LocsRecord,
  val: any,
  valSrc: CheckOperand,
  pred: any
) {
  return { $name: 'failure-not-satisfied' as const, metadata, val, valSrc, pred };
}
function failureNotDissatisfied(
  metadata: LocsRecord,
  val: any,
  valSrc: CheckOperand,
  pred: any
) {
  return { $name: 'failure-not-dissatisfied' as const, metadata, val, valSrc, pred };
}
function failureWrongExn(
  metadata: LocsRecord,
  exnExpected: any,
  exnActual: any,
  actualSrc: CheckOperand
) {
  return { $name: 'failure-wrong-exn' as const, metadata, exnExpected, exnActual, actualSrc };
}
function failureRightExn(
  metadata: LocsRecord,
  exnNotExpected: any,
  exnActual: any,
  actualSrc: CheckOperand
) {
  return { $name: 'failure-right-exn' as const, metadata, exnNotExpected, exnActual, actualSrc };
}
function failureExn(
  metadata: LocsRecord,
  actualExn: any,
  exnPlace: CheckOperand
) {
  return { $name: 'failure-exn' as const, metadata, actualExn, exnPlace };
}
function failureNoExn(
  metadata: LocsRecord,
  exnExpected: Option<string>,
  exnSrc: CheckOperand,
  wanted: boolean,
) {
  return { $name: 'failure-no-exn' as const, metadata, exnExpected, exnSrc, wanted };
}
function failureRaiseNotSatisfied(
  metadata: LocsRecord,
  exn: any,
  exnSrc: CheckOperand,
  pred: any
) {
  return { $name: 'failure-raise-not-satisfied' as const, metadata, exn, exnSrc, pred };
}
function failureRaiseNotDissatisfied(
  metadata: LocsRecord,
  exn: any,
  exnSrc: CheckOperand,
  pred: any
) {
  return { $name: 'failure-raise-not-dissatisfied' as const, metadata, exn, exnSrc, pred };
}
function errorNotBoolean(
  metadata: LocsRecord,
  refinement: any,
  left: any,
  leftSrc: CheckOperand,
  right: any,
  rightSrc: CheckOperand,
  testResult: any
) {
  return { $name: 'error-not-boolean' as const, metadata, refinement, left, leftSrc, right, rightSrc, testResult };
}
function errorNotPred(
  metadata: LocsRecord,
  predicate: any,
  arity: number,
) {
  return { $name: 'error-not-pred' as const, metadata, predicate, arity };
}
function errorNotBooleanPred(
  metadata: LocsRecord,
  predicate: any,
  left: any,
  leftSrc: CheckOperand,
  testResult: any,
) {
  return { $name: 'error-not-boolean-pred' as const, metadata, predicate, left, leftSrc, testResult };
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
type LocsRecord = {
  [checkOp in CheckOperand]: Srcloc | undefined;
} & {
  opName: CheckOp,
  opIsName?: IsOp,
  loc: Srcloc,
};

// Record<CheckOperand, Srcloc | undefined>

export function makeCheckContext(mainModuleName: string, checkAll: boolean) {
  // fixme: blockResults no longer constant
  let blockResults: CheckBlockResult[] = [];
  function addBlockResult(cbr : CheckBlockResult) {
    blockResults.push(cbr);
  }
  let currentResults: TestResult[] = [];
  function addResult(tr : TestResult) {
    currentResults.push(tr);
  }
  function unthunk<T>(metadata: LocsRecord, where: CheckOperand, thunk: Thunk<T>, cont: (val: T) => any) {
    const result = RUNTIME.$runTask(thunk);
    if (result.$name === 'right') {
      addResult(failureExn(metadata, result.v, where));
    } else {
      cont(result.v);
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
  function checkIsCont(metadata: LocsRecord, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
    const eqLvRv = EQUALITY.equalAlways3(lv, rv);
    switch (eqLvRv.$name) {
      case 'Unknown': addResult(failureIsIncomparable(metadata, eqLvRv, lv, lvSrc, rv, rvSrc)); break;
      case 'NotEqual': addResult(failureNotEqual(metadata, none, lv, lvSrc, rv, rvSrc)); break
      case 'Equal': cont(); break;
      default: throw new ExhaustiveSwitchError(eqLvRv);
    }
  }
  const CHECK_IS = {
    checkIs: (left: Thunk<any>, right: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          checkIsCont(metadata, lv, 'on-left', rv, 'on-right', () => addResult(success(metadata)))));
    },
    checkIsCause: (left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          unthunk(metadata, 'on-cause', cause, (cv) =>
            checkIsCont(metadata, cv, 'on-cause', rv, 'on-right', () =>
              checkIsCont(metadata, lv, 'on-left', rv, 'on-right', () => 
                addResult(success(metadata)))))));
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsRoughlyCont(metadata: LocsRecord, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
    // TODO(Ben/Joe): Update equality.ts to include the new roughlyEqual
    // and redefinition of within* as separated from withinRel*
    const eqLvRv = EQUALITY.within3(1e-6)(lv, rv);
    switch (eqLvRv.$name) {
      case 'Unknown': addResult(failureIsIncomparable(metadata, eqLvRv, lv, lvSrc, rv, rvSrc)); break;
      case 'NotEqual': addResult(failureNotEqual(metadata, none, lv, lvSrc, rv, rvSrc)); break
      case 'Equal': cont(); break;
      default: throw new ExhaustiveSwitchError(eqLvRv);
    }
  };
  const CHECK_IS_ROUGHLY = {
    checkIsRoughly: (left: Thunk<any>, right: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          checkIsRoughlyCont(metadata, lv, 'on-left', rv, 'on-right', () => addResult(success(metadata)))));
    },
    checkIsRoughlyCause: (left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          unthunk(metadata, 'on-cause', cause, (cv) =>
            checkIsRoughlyCont(metadata, cv, 'on-cause', rv, 'on-right', () =>
              checkIsRoughlyCont(metadata, lv, 'on-left', rv, 'on-right', () => 
                addResult(success(metadata)))))));
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsNotCont(metadata: LocsRecord, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
    const eqLvRv = EQUALITY.equalAlways3(lv, rv);
    switch (eqLvRv.$name) {
      case 'Unknown': addResult(failureIsIncomparable(metadata, eqLvRv, lv, lvSrc, rv, rvSrc)); break;
      case 'Equal': addResult(failureNotDifferent(metadata, none, lv, lvSrc, rv, rvSrc)); break
      case 'NotEqual': cont(); break;
      default: throw new ExhaustiveSwitchError(eqLvRv);
    }
  }
  const CHECK_IS_NOT = {
    checkIsNot: (left: Thunk<any>, right: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          checkIsNotCont(metadata, lv, 'on-left', rv, 'on-right', () => addResult(success(metadata)))));
    },
    checkIsNotCause: (left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          unthunk(metadata, 'on-cause', cause, (cv) =>
            checkIsNotCont(metadata, cv, 'on-cause', rv, 'on-right', () =>
              checkIsNotCont(metadata, lv, 'on-left', rv, 'on-right', () => 
                addResult(success(metadata)))))));
    },
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsNotRoughlyCont(metadata: LocsRecord, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
    const eqLvRv = EQUALITY.within3(1e-6)(lv, rv);
    switch (eqLvRv.$name) {
      case 'Unknown': addResult(failureIsIncomparable(metadata, eqLvRv, lv, lvSrc, rv, rvSrc)); break;
      case 'Equal': addResult(failureNotDifferent(metadata, none, lv, lvSrc, rv, rvSrc)); break
      case 'NotEqual': cont(); break;
      default: throw new ExhaustiveSwitchError(eqLvRv);
    }
  };
  const CHECK_IS_NOT_ROUGHLY = {
    checkIsNotRoughly: (left: Thunk<any>, right: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          checkIsNotRoughlyCont(metadata, lv, 'on-left', rv, 'on-right', () => addResult(success(metadata)))));
    },
    checkIsNotRoughlyCause: (left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          unthunk(metadata, 'on-cause', cause, (cv) =>
            checkIsNotRoughlyCont(metadata, cv, 'on-cause', rv, 'on-right', () =>
              checkIsNotRoughlyCont(metadata, lv, 'on-left', rv, 'on-right', () => 
                addResult(success(metadata)))))));
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsRefinementCont(metadata: LocsRecord, refinement: (left: any, right: any) => boolean | EQ.EqualityResult, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
    const refine = RUNTIME.$runTask(() => refinement(lv, rv));
    if (refine.$name === 'right') {
      const exn = refine.v;
      if (causesErrorNotPred(exn)) {
        addResult(errorNotPred(metadata, refinement, 2));
      } else {
        addResult(failureExn(metadata, exn, 'on-refinement'));
      }
    } else {
      const testResult = refine.v;
      if (isUnknown(testResult)) {
        addResult(failureIsIncomparable(metadata, testResult, lv, lvSrc, rv, rvSrc));
      } else if (isFalseOrNotEqual(testResult)) {
        addResult(failureNotEqual(metadata, some(refinement), lv, lvSrc, rv, rvSrc));
      } else if (isNeitherBooleanNorEqual(testResult)) {
        addResult(errorNotBoolean(metadata, refinement, lv, lvSrc, rv, rvSrc, testResult));
      } else {
        cont();
      }
    }
  }
  const CHECK_IS_REFINEMENT = {
    checkIsRefinement: (refinement: Thunk<(left: any, right: any) => boolean | EQ.EqualityResult>, left: Thunk<any>, right: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          unthunk(metadata, 'on-refinement', refinement, (refv) => 
            checkIsRefinementCont(metadata, refv, lv, 'on-left', rv, 'on-right', () =>
              addResult(success(metadata))))))
    },
    checkIsRefinementCause: (refinement: Thunk<(left: any, right: any) => boolean | EQ.EqualityResult>, left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          unthunk(metadata, 'on-cause', cause, (cv) => 
            unthunk(metadata, 'on-refinement', refinement, (refv) => 
              checkIsRefinementCont(metadata, refv, cv, 'on-cause', rv, 'on-right', () => 
                checkIsRefinementCont(metadata, refv, lv, 'on-left', rv, 'on-right', () =>
                  addResult(success(metadata))))))))
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkIsNotRefinementCont(metadata: LocsRecord, refinement: (left: any, right: any) => boolean | EQ.EqualityResult, lv: any, lvSrc: CheckOperand, rv: any, rvSrc: CheckOperand, cont: Thunk<any>) {
    const refine = RUNTIME.$runTask(() => refinement(lv, rv));
    if (refine.$name === 'right') {
      const exn = refine.v;
      if (causesErrorNotPred(exn)) {
        addResult(errorNotPred(metadata, refinement, 2));
      } else {
        addResult(failureExn(metadata, exn, 'on-refinement'));
      }
    } else {
      const testResult = refine.v;
      if (isUnknown(testResult)) {
        addResult(failureIsIncomparable(metadata, testResult, lv, lvSrc, rv, rvSrc));
      } else if (isTruthOrEqual(testResult)) {
        addResult(failureNotDifferent(metadata, some(refinement), lv, lvSrc, rv, rvSrc));
      } else if (isNeitherBooleanNorNotEqual(testResult)) {
        addResult(errorNotBoolean(metadata, refinement, lv, lvSrc, rv, rvSrc, testResult));
      } else {
        cont();
      }
    }
  }
  const CHECK_IS_NOT_REFINEMENT = {
    checkIsNotRefinement: (refinement: Thunk<(left: any, right: any) => boolean | EQ.EqualityResult>, left: Thunk<any>, right: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          unthunk(metadata, 'on-refinement', refinement, (refv) => 
            checkIsNotRefinementCont(metadata, refv, lv, 'on-left', rv, 'on-right', () =>
              addResult(success(metadata))))))
    },
    checkIsNotRefinementCause: (refinement: Thunk<(left: any, right: any) => boolean | EQ.EqualityResult>, left: Thunk<any>, right: Thunk<any>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', right, (rv) =>
          unthunk(metadata, 'on-cause', cause, (cv) => 
            unthunk(metadata, 'on-refinement', refinement, (refv) => 
              checkIsNotRefinementCont(metadata, refv, cv, 'on-cause', rv, 'on-right', () => 
                checkIsNotRefinementCont(metadata, refv, lv, 'on-left', rv, 'on-right', () =>
                  addResult(success(metadata))))))))
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkSatisfiesDelayedCont(metadata: LocsRecord, lv: any, lvSrc: CheckOperand, pv: (v: any) => any, pvSrc: CheckOperand, cont: Thunk<any>) {
    const result = RUNTIME.$runTask(() => pv(lv));
    if (result.$name === 'right') {
      const exn = result.v;
      if (causesErrorNotPred(exn)) {
        addResult(errorNotPred(metadata, pv, 1));
      } else {
        addResult(failureExn(metadata, exn, pvSrc));
      }
    } else {
      const testResult = result.v;
      if (!(typeof testResult === 'boolean')) {
        addResult(errorNotBooleanPred(metadata, pv, lv, lvSrc, testResult));
      } else if (!testResult) {
        addResult(failureNotSatisfied(metadata, lv, lvSrc, pv));
      } else {
        cont();
      }
    }
  }
  const CHECK_SATISFIES_DELAYED = {
    checkSatisfiesDelayed: (left: Thunk<any>, pred: Thunk<(v : any) => any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', pred, (pv) =>
          checkSatisfiesDelayedCont(metadata, lv, 'on-left', pv, 'on-right', () =>
            addResult(success(metadata)))))
    },
    checkSatisfiesDelayedCause: (left: Thunk<any>, pred: Thunk<(v : any) => any>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', pred, (pv) =>
          unthunk(metadata, 'on-cause', cause, (cv) => 
            checkSatisfiesDelayedCont(metadata, cv, 'on-cause', pv, 'on-right', () =>
              checkSatisfiesDelayedCont(metadata, lv, 'on-left', pv, 'on-right', () =>
                addResult(success(metadata)))))))
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkSatisfiesNotDelayedCont(metadata: LocsRecord, lv: any, lvSrc: CheckOperand, pv: (v: any) => any, pvSrc: CheckOperand, cont: Thunk<any>) {
    const result = RUNTIME.$runTask(() => pv(lv));
    if (result.$name === 'right') {
      const exn = result.v;
      if (causesErrorNotPred(exn)) {
        addResult(errorNotPred(metadata, pv, 1));
      } else {
        addResult(failureExn(metadata, exn, pvSrc));
      }
    } else {
      const testResult = result.v;
      if (!(typeof testResult === 'boolean')) {
        addResult(errorNotBooleanPred(metadata, pv, lv, lvSrc, testResult));
      } else if (testResult) {
        addResult(failureNotDissatisfied(metadata, lv, lvSrc, pv));
      } else {
        cont();
      }
    }
}
  const CHECK_SATISFIES_NOT_DELAYED = {
    checkSatisfiesNotDelayed: (left: Thunk<any>, pred: Thunk<(v : any) => any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', pred, (pv) =>
          checkSatisfiesNotDelayedCont(metadata, lv, 'on-left', pv, 'on-right', () =>
            addResult(success(metadata)))))
    },
    checkSatisfiesNotDelayedCause: (left: Thunk<any>, pred: Thunk<(v : any) => any>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-left', left, (lv) =>
        unthunk(metadata, 'on-right', pred, (pv) =>
          unthunk(metadata, 'on-cause', cause, (cv) => 
            checkSatisfiesNotDelayedCont(metadata, cv, 'on-cause', pv, 'on-right', () =>
              checkSatisfiesNotDelayedCont(metadata, lv, 'on-left', pv, 'on-right', () =>
                addResult(success(metadata)))))))
    }
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkRaisesStrCont(metadata: LocsRecord, thunk: any, thunkSrc: CheckOperand, str: string, cont: Thunk<any>) {
    const result = RUNTIME.$runTask(thunk);
    if (result.$name === 'left') {
      addResult(failureNoExn(metadata, some(str), thunkSrc, true));
    } else {
      // TODO: is this the right way to call to-repr?  The types don't think it exists...
      console.log("Result form an exn test:" , result.v, (RUNTIME.$torepr(result.v)));
      if (!((RUNTIME.$torepr(result.v) as string).includes(str)) &&
          !(String(result.v).includes(str))) {
        addResult(failureWrongExn(metadata, str, result.v, thunkSrc));
      } else {
        cont();
      }
    }
  }
  const CHECK_RAISES_STR = {
    checkRaisesStr: (thunk: Thunk<any>, str: Thunk<string>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-right', str, (sv) =>
        checkRaisesStrCont(metadata, thunk, 'on-left', sv, () =>
          addResult(success(metadata))))
    },
    checkRaisesStrCause: (thunk: Thunk<any>, str: Thunk<string>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-right', str, (sv) =>
        checkRaisesStrCont(metadata, cause, 'on-cause', sv, () =>
          checkRaisesStrCont(metadata, thunk, 'on-left', sv, () =>
            addResult(success(metadata)))))
    },
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkRaisesOtherStrCont(metadata: LocsRecord, thunk: any, thunkSrc: CheckOperand, str: string, cont: Thunk<any>) {
    const result = RUNTIME.$runTask(thunk);
    if (result.$name === 'left') {
      addResult(failureNoExn(metadata, some(str), thunkSrc, true));
    } else {
      // TODO: is this the right way to call to-repr?  The types don't think it exists...
      if ((RUNTIME.$torepr(result.v) as string).includes(str)) {
        addResult(failureRightExn(metadata, str, result.v, thunkSrc));
      } else {
        cont();
      }
    }
  }
  const CHECK_RAISES_OTHER_STR = {
    checkRaisesOtherStr: (thunk: Thunk<any>, str: Thunk<string>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-right', str, (sv) =>
        checkRaisesOtherStrCont(metadata, thunk, 'on-left', sv, () =>
          addResult(success(metadata))))
    },
    checkRaisesOtherStrCause: (thunk: Thunk<any>, str: Thunk<string>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-right', str, (sv) =>
        checkRaisesOtherStrCont(metadata, cause, 'on-cause', sv, () =>
          checkRaisesOtherStrCont(metadata, thunk, 'on-left', sv, () =>
            addResult(success(metadata)))))
    },
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkRaisesNotCont(metadata: LocsRecord, thunk: any, thunkSrc: CheckOperand, cont: Thunk<any>) {
    const result = RUNTIME.$runTask(thunk);
    if (result.$name === 'right') {
      addResult(failureExn(metadata, result.v, thunkSrc));
    } else {
      cont();
    }
}
  const CHECK_RAISES_NOT = {
    checkRaisesNot: (thunk: Thunk<any>, metadata: LocsRecord) => {
      checkRaisesNotCont(metadata, thunk, 'on-left', () =>
        addResult(success(metadata)))
    },
    checkRaisesNotCause: (thunk: Thunk<any>, cause: Thunk<any>, metadata: LocsRecord) => {
      checkRaisesNotCont(metadata, cause, 'on-cause', () =>
        checkRaisesNotCont(metadata, thunk, 'on-left', () =>
          addResult(success(metadata))))
    },
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkRaisesSatisfiesCont(metadata: LocsRecord, thunk: Thunk<any>, thunkSrc: CheckOperand, pred: (v: any) => boolean, predSrc: CheckOperand, cont: Thunk<any>) {
    const result = RUNTIME.$runTask(thunk);
    if (result.$name === 'left') {
      addResult(failureNoExn(metadata, none, thunkSrc, true));
    } else {
      const exn = result.v;
      const exnVal = (isUserException(exn) ? exn.value : exn);
      const exnResult = RUNTIME.$runTask(() => pred(exnVal));
      if (exnResult.$name === 'right') {
        const exn = exnResult.v;
        if (causesErrorNotPred(exn)) {
          addResult(errorNotPred(metadata, pred, 1));
        } else {
          addResult(failureExn(metadata, exn, predSrc));
        }
      } else {
        const predResult = exnResult.v;
        if (!(typeof predResult === 'boolean')) {
          addResult(errorNotBooleanPred(metadata, pred, exnVal, thunkSrc, predResult));
        } else if (!predResult) {
          addResult(failureRaiseNotSatisfied(metadata, exn, thunkSrc, pred));
        } else {
          cont();
        }
      }
    }
  }
  const CHECK_RAISES_SATISFIES = {
    checkRaisesSatisfies: (thunk: Thunk<any>, pred: Thunk<(v: any) => boolean>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-right', pred, (pv) => 
        checkRaisesSatisfiesCont(metadata, thunk, 'on-left', pv, 'on-right', () =>
          addResult(success(metadata))))
    },
    checkRaisesSatisfiesCause: (thunk: Thunk<any>, pred: Thunk<(v: any) => boolean>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-right', pred, (pv) => 
        checkRaisesSatisfiesCont(metadata, cause, 'on-cause', pv, 'on-right', () => 
          checkRaisesSatisfiesCont(metadata, thunk, 'on-left', pv, 'on-right', () =>
            addResult(success(metadata)))))
    },
  };
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  function checkRaisesViolatesCont(metadata: LocsRecord, thunk: Thunk<any>, thunkSrc: CheckOperand, pred: (v: any) => boolean, predSrc: CheckOperand, cont: Thunk<any>) {
    const result = RUNTIME.$runTask(thunk);
    if (result.$name === 'left') {
      addResult(failureNoExn(metadata, none, thunkSrc, true));
    } else {
      const exn = result.v;
      const exnVal = (isUserException(exn) ? exn.value : exn);
      const exnResult = RUNTIME.$runTask(() => pred(exnVal));
      if (exnResult.$name === 'right') {
        const exn = exnResult.v;
        if (causesErrorNotPred(exn)) {
          addResult(errorNotPred(metadata, pred, 1));
        } else {
          addResult(failureExn(metadata, exn, predSrc));
        }
      } else {
        const predResult = exnResult.v;
        if (!(typeof predResult === 'boolean')) {
          addResult(errorNotBooleanPred(metadata, pred, exnVal, thunkSrc, predResult));
        } else if (predResult) {
          addResult(failureRaiseNotDissatisfied(metadata, exn, thunkSrc, pred));
        } else {
          cont();
        }
      }
    }
  }
  const CHECK_RAISES_VIOLATES = {
    checkRaisesViolates: (thunk: Thunk<any>, pred: Thunk<(v: any) => boolean>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-right', pred, (pv) => 
        checkRaisesViolatesCont(metadata, thunk, 'on-left', pv, 'on-right', () =>
          addResult(success(metadata))))
    },
    checkRaisesViolatesCause: (thunk: Thunk<any>, pred: Thunk<(v: any) => boolean>, cause: Thunk<any>, metadata: LocsRecord) => {
      unthunk(metadata, 'on-right', pred, (pv) => 
        checkRaisesViolatesCont(metadata, cause, 'on-cause', pv, 'on-right', () => 
          checkRaisesViolatesCont(metadata, thunk, 'on-left', pv, 'on-right', () =>
            addResult(success(metadata)))))
    },
  };

  function moduleDirPart(moduleName: string) {
    return moduleName.replace(/\/([^\/]*)$/, '');
  }

  return {
    runChecks: (moduleName: string, checks: TestCase[]) => {
      console.log(`Running a check-block for ${moduleName} while in mainModule ${mainModuleName}`);
      if (moduleDirPart(moduleName) !== moduleDirPart(mainModuleName)) {
        // fixme: blockResults emptied when changing Anchor project dir
        blockResults = [];
      }
      if (checkAll || (moduleName === mainModuleName)) {
        checks.forEach(c => {
          const { name, location, keywordCheck, run } = c;
          const resultsBefore = currentResults;
          currentResults = [];
          const result = RUNTIME.$runTask(run);
          if (result.$name === 'left') {
            addBlockResult(checkBlockResult(name, location, keywordCheck, currentResults, none));
          } else {
            addBlockResult(checkBlockResult(name, location, keywordCheck, currentResults, some(result.v)));
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

export function resultsSummary(blockResults : CheckBlockResult[]) {
  let message = "", errored = 0, passed = 0, failed = 0, total = 0;
  blockResults.forEach(br => {
    const { name, loc, isKeywordCheck, testResults, maybeErr } = br;
    let blockMessage = "", blockPassed = 0, blockFailed = 0, blockTotal = 0;
    testResults.forEach(tr => {
      switch(tr.$name) {
        case 'success':
          blockMessage += "\n  " + makeSrcloc(tr.metadata.loc).format(false) + ": ok";
          blockPassed++;
          blockTotal++;
          break;
        default:
          const m = makeSrcloc(tr.metadata.loc).format(false) + ": failed because: \n    " + displayToString(renderFancyReason(tr), RUNTIME.$torepr);
          blockMessage += "\n  " + m;
          blockFailed++;
          blockTotal++;
      }
    });
    let blockType = isKeywordCheck ? "Check" : "Examples";
    let endedInError = maybeErr.$name === 'some' ? "\n  " + blockType + " block ended in the following error (not all tests may have run): \n\n  " +
      displayToString(renderFancyReason(maybeErr.value), RUNTIME.$torepr) + displayToString(ED['v-sequence'](maybeErr.value.stack.map(makeSrcloc)), RUNTIME.$torepr) + "\n\n" : "";
    message += "\n\n" + makeSrcloc(loc).format(true) + ": " + name + " (" + blockPassed + "/" + blockTotal + ") \n";
    message += endedInError;
    if (blockFailed > 0) {
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

function makeSrcloc(loc: Srcloc): SL_TYPES.Srcloc {
  if (loc.length === 1) {
    return srcloc.builtin(loc[0]);
  } else {
    return srcloc.srcloc(loc[0], loc[1], loc[2], loc[3], loc[4], loc[5], loc[6]);
  }
}

function humanOpName(op: CheckOp): string {
  switch(op) {
    case 's-op-is': return 'is';
    case 's-op-is-roughly': return 'is-roughly';
    case 's-op-is-not-roughly': return 'is-not-roughly';
    case 's-op-is-op': return 'is%';
    case 's-op-is-not': return 'is-not';
    case 's-op-is-not-op': return 'is-not%';
    case 's-op-satisfies': return 'satisfies';
    case 's-op-satisfies-not': return 'satisfies-not';
    case 's-op-raises-not': return 'raises-not';
    case 's-op-raises': return 'raises';
    case 's-op-raises-other': return 'raises-other';
    case 's-op-raises-satisfies': return 'raises-satisfies';
    case 's-op-raises-violates': return 'raises-violates';
    default: throw new ExhaustiveSwitchError(op);
  }
}
  
function testPreamble(testOp: CheckOperand, testResult: TestResult): ErrorDisplay {
  if (testOp === 'on-cause') {
    return ED.para.make([ED.text("The test was inconsistent: the expected answer and the explanation do not match for the test:")]);
  } else {
    return ED.para.make([
      ED.text("The test operator "),
      // TODO: this used to call testAst.op.tosource()
      ED.code(ED.text(humanOpName(testResult.metadata.opName))),
      ED.text(" failed for the test:")]);
  }
}

function reportValue(operand: ErrorDisplay, value: any): ErrorDisplay {
  return ED.sequence.make([
    ED.para.make([
      ED.text("The "), operand, ED.text(" was:")
    ]),
    ED.para.make1(ED.embed(value))
  ]);
}

export function renderFancyReason(testResult: TestResult): ErrorDisplay {
  switch(testResult.$name) {
    case 'success': return ED.text("Success");
    case 'failure-not-equal': {
      const lhsLoc = testResult.metadata[testResult.leftSrc]!;
      const rhsLoc = testResult.metadata[testResult.rightSrc]!;
      const highlightLhs = ED.highlight(ED.text(sideOfCheckOp(testResult.leftSrc)), [makeSrcloc(lhsLoc)], 3);
      const highlightRhs = ED.highlight(ED.text(sideOfCheckOp(testResult.rightSrc)), [makeSrcloc(rhsLoc)], 4);
      let para: ErrorDisplay = ED.text("");
      switch(testResult.metadata.opName) {
        case 's-op-is-roughly':
          para = ED.sequence.make([
            ED.text("It succeeds only if the "),
            highlightLhs, ED.text(" and "), highlightRhs,
            ED.text(" are equal (allowing for rough equality).")
          ]);
          break;
        case 's-op-is':
          if (testResult.metadata['on-refinement'] === undefined) {
            para = ED.sequence.make([
              ED.text("It succeeds only if the "),
              highlightLhs, ED.text(" and "), highlightRhs,
              ED.text(" are equal.")
            ]);
          } else {
            para = ED.sequence.make([
              ED.text("It succeeds only if the predicate "),
              ED.highlight(ED.text("predicate"), [makeSrcloc(testResult.metadata['on-refinement'])], 1),
              ED.text(" is satisfied when the "),
              highlightLhs, ED.text(" and "), highlightRhs,
              ED.text(" are applied to it.")
            ]);
          }
          break;
        case 's-op-is-op':
          para = ED.sequence.make([
            ED.text("It succeeds only if the predicate "),
            ED.code(ED.text(getOpFunName(testResult.metadata['opIsName']!))),
            ED.text(" is satisfied when the "),
            highlightLhs, ED.text(" and "), highlightRhs,
            ED.text(" are applied to it.")
        ]);
      }
      return ED.error.make([
        testPreamble(testResult.leftSrc, testResult),
        ED.cmcode(makeSrcloc(testResult.metadata.loc)),
        ED.para.make1(para),
        reportValue(highlightLhs, testResult.left),
        reportValue(highlightRhs, testResult.right)
      ]);
    }
    case 'failure-is-incomparable': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        const highlightLhs = ED.highlight(ED.text(sideOfCheckOp(testResult.leftSrc)), [makeSrcloc(testResult.metadata[testResult.leftSrc]!)], 3);
        const highlightRhs = ED.highlight(ED.text(sideOfCheckOp(testResult.rightSrc)), [makeSrcloc(testResult.metadata[testResult.rightSrc]!)], 3);
        const withinName = (testResult.metadata.opIsName === 'op=~') ? 'within-now' : 'within';
        const useWithin = ED.para.make([
          ED.text("Use "), ED.code(ED.text("is-roughly")),
          ED.text(" as the testing operator, or consider using the "),
          ED.code(ED.text(withinName)), ED.text(" function to compare them instead.")
        ]);
        let msg1: string;
        let msg2: ErrorDisplay;
        const val1IsFunction = typeof testResult.eqResult.value1 === 'function';
        const val2IsFunction = typeof testResult.eqResult.value2 === 'function';
        if (val1IsFunction && val2IsFunction) {
          msg1 = "Attempted to compare two functions for equality, which is not allowed:";
          msg2 = ED.para.make1(ED.text("Did you mean to call them first?"));
        } else if (val1IsFunction || val2IsFunction) {
          msg1 = "Attempted to compare a function to another value for equality:"
          msg2 = ED.para.make1(ED.text("Did you mean to call the function first?"));
        } else if (NUMBER.isRoughnum(testResult.eqResult.value1) && NUMBER.isRoughnum(testResult.eqResult.value2)) {
          msg1 = "Attempted to compare two roughnums for equality, which is not allowed:";
          msg2 = useWithin;
        } else if (NUMBER.isRoughnum(testResult.eqResult.value1)) {
          msg1 = "Attempted to compare a roughnum to an exactnum for equality, which is not allowed:";
          msg2 = useWithin;
        } else {
          msg1 = "Attempted to compare an exactnum to a roughnum for equality, which is not allowed:";
          msg2 = useWithin;          
        }
        return ED.error.make([
          testPreamble(testResult.leftSrc, testResult),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          ED.para.make1(ED.text(msg1)),
          reportValue(highlightLhs, testResult.left),
          reportValue(highlightRhs, testResult.right)
        ]);
      }
    }
    case 'failure-not-different': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        const highlightLhs = ED.highlight(
          ED.text(sideOfCheckOp(testResult.leftSrc)),
          [makeSrcloc(testResult.metadata[testResult.leftSrc]!)],
          3);
        const highlightRhs = ED.highlight(
          ED.text(sideOfCheckOp(testResult.rightSrc)),
          [makeSrcloc(testResult.metadata[testResult.rightSrc]!)],
          4);
        let msg: ErrorDisplay;
        if (testResult.metadata.opName === 's-op-is-not') {
          if (testResult.metadata['on-refinement'] === undefined) {
            msg = ED.sequence.make([
              ED.text("It succeeds only if the "), highlightLhs, ED.text(" and "), highlightRhs, ED.text(" sides are not equal.")
            ]);
          } else {
            msg = ED.sequence.make([
              ED.text("It succeeds only if the "),
              ED.highlight(ED.text("predicate"), [makeSrcloc(testResult.metadata['on-refinement'])], 1),
              ED.text(" is not satisfied when the "),
              highlightLhs, ED.text(" and "), highlightRhs, ED.text(" sides are applied to it.")
            ]);
          }
        } else if (testResult.metadata.opName === 's-op-is-not-op') {
          msg = ED.sequence.make([
            ED.text("It succeeds only if the predicate "),
            ED.code(ED.text(getOpFunName(testResult.metadata.opIsName!))),
            ED.text(" is not satisfied when the "),
            highlightLhs, ED.text(" and "), highlightRhs,
            ED.text(" sides are applied to it.")
          ])
        } else {
          // How could we possibly get here?
          return renderReason(testResult);
        }
        return ED.error.make([
          testPreamble(testResult.leftSrc, testResult),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          msg,
          reportValue(highlightLhs, testResult.left),
          reportValue(highlightRhs, testResult.right)
        ]);
      }
    }
    case 'failure-not-satisfied': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        const highlightLhs = ED.highlight(
          ED.text(sideOfCheckOp(testResult.valSrc)),
          [makeSrcloc(testResult.metadata[testResult.valSrc]!)],
          3);
        const highlightRhs = ED.highlight(
          ED.text("predicate"),
          [makeSrcloc(testResult.metadata['on-right']!)],
          4);
        return ED.error.make([
          testPreamble(testResult.valSrc, testResult),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          ED.para.make([
            ED.text("It succeeds only if the "), highlightRhs,
            ED.text(" is satisfied when applied to the "), highlightLhs,
            ED.text(". The "), highlightLhs, ED.text(" is:"),
          ]),
          ED.embed(testResult.val)
        ])
      }
    }
    case 'failure-not-dissatisfied': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        const highlightLhs = ED.highlight(
          ED.text(sideOfCheckOp(testResult.valSrc)),
          [makeSrcloc(testResult.metadata[testResult.valSrc]!)],
          3);
        const highlightRhs = ED.highlight(
          ED.text("predicate"),
          [makeSrcloc(testResult.metadata['on-right']!)],
          4);
        return ED.error.make([
          testPreamble(testResult.valSrc, testResult),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          ED.para.make([
            ED.text("It succeeds only if the "), highlightRhs,
            ED.text(" is not satisfied when applied to the "), highlightLhs,
            ED.text(". The "), highlightLhs, ED.text(" is:"),
          ]),
          ED.embed(testResult.val)
        ])

      }
    }
    case 'failure-wrong-exn': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        return ED.error.make([
          testPreamble(testResult.actualSrc, testResult),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          ED.para.make1(ED.text("Got unexpected exception ")),
          ED.embed(testResult.exnActual),
          ED.para.make1(ED.text("when expecting ")),
          ED.embed(testResult.exnExpected)
        ])
      }
    }
    case 'failure-right-exn': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        return ED.error.make([
          testPreamble(testResult.actualSrc, testResult),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          ED.para.make1(ED.text("Got exception ")),
          ED.embed(testResult.exnActual),
          ED.para.make1(ED.text("and expected it not to contain ")),
          ED.embed(testResult.exnNotExpected)
        ])
      }
    }
    case 'failure-exn': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        let highlight: ED_TYPES.ErrorDisplay;
        if (testResult.exnPlace === 'on-refinement' && testResult.metadata['on-refinement'] !== undefined) {
          highlight = ED.highlight(ED.text("refinement"), [makeSrcloc(testResult.metadata['on-refinement'])], -3)
        } else {
          highlight = ED.highlight(ED.text(sideOfCheckOp(testResult.exnPlace)), [makeSrcloc(testResult.metadata[testResult.exnPlace]!)], -3);
        }
        return ED.error.make([
          testPreamble(testResult.exnPlace, testResult),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          ED.para.make([
            ED.text("It did not expect the evaluation of the "),
            highlight,
            ED.text(" to raise an exception:")
          ]),
          ED.embed(testResult.actualExn)
        ]);
      }
    }
    case 'failure-no-exn': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        const highlightExn = ED.highlight(ED.text("predicate"), [makeSrcloc(testResult.metadata['on-right']!)], 4);
        if (testResult.exnExpected.$name === 'some') {
          return ED.error.make([
            ED.para.make1(testPreamble(testResult.exnSrc, testResult)),
            ED.cmcode(makeSrcloc(testResult.metadata.loc)),
            ED.para.make([
              ED.text("No exception raised while evaluating the"),
              highlightExn,
              (testResult.wanted 
                ? ED.text(", expected")
                : ED.text(", expected any exception other than")
              ),
            ]),
            ED.embed(testResult.exnExpected)
          ])
        } else {
          return ED.error.make3(
            testPreamble(testResult.exnSrc, testResult),
            ED.cmcode(makeSrcloc(testResult.metadata.loc)),
            ED.para.make2(ED.text("No exception raised while evaluating the "), highlightExn)
          )
        }
      }
    }
    case 'failure-raise-not-satisfied': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        const highlightLhs = ED.highlight(ED.text(sideOfCheckOp(testResult.exnSrc)), [makeSrcloc(testResult.metadata[testResult.exnSrc]!)], 3);
        const highlightPred = ED.highlight(ED.text("predicate"), [makeSrcloc(testResult.metadata['on-right']!)], 4);
        return ED.error.make([
          ED.para.make1(testPreamble(testResult.exnSrc, testResult)),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          ED.para.make([
            ED.text("It succeeds only if the "),
            highlightPred,
            ED.text(" is satisfied when the value of the exception raised by the "),
            highlightLhs,
            ED.text(" is applied to it.  The value of the "),
            highlightLhs,
            ED.text(" is:")
          ]),
          ED.embed(testResult.exn)
        ])
      }
    }
    case 'failure-raise-not-dissatisfied': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        const highlightLhs = ED.highlight(ED.text(sideOfCheckOp(testResult.exnSrc)), [makeSrcloc(testResult.metadata[testResult.exnSrc]!)], 3);
        const highlightPred = ED.highlight(ED.text("predicate"), [makeSrcloc(testResult.metadata['on-right']!)], 4);
        return ED.error.make([
          ED.para.make1(testPreamble(testResult.exnSrc, testResult)),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          ED.para.make([
            ED.text("It succeeds only if the "),
            highlightPred,
            ED.text(" is not satisfied when the value of the exception raised by the "),
            highlightLhs,
            ED.text(" is applied to it.  The value of the "),
            highlightLhs,
            ED.text(" is:")
          ]),
          ED.embed(testResult.exn)
        ])
      }
    }
    case 'error-not-boolean': return renderReason(testResult);
    case 'error-not-pred': {
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        const highlightPred = ED.highlight(ED.text("test predicate"), [makeSrcloc(testResult.metadata['on-right']!)], 2);
        return ED.error.make3(
          ED.para.make([ED.text("The "), highlightPred, ED.text(" must be a " + testResult.arity + "-argument function that returns a boolean, but instead it was:")]),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          ED.para.make2(ED.text("Instead it was:"), ED.embed(testResult.predicate))
        );
      }
    }
    case 'error-not-boolean-pred':{
      if (srcloc['is-builtin'](testResult.metadata.loc)) {
        return renderReason(testResult);
      } else {
        const highlightLhs = ED.highlight(ED.text(sideOfCheckOp(testResult.leftSrc)), [makeSrcloc(testResult.metadata[testResult.leftSrc]!)], 3);
        const highlightPred = ED.highlight(ED.text("test predicate"), [makeSrcloc(testResult.metadata['on-right']!)], 4);
        return ED.error.make3(
          ED.para.make([ED.text("The "), highlightPred, ED.text(" must return a boolean:")]),
          ED.cmcode(makeSrcloc(testResult.metadata.loc)),
          ED.para.make([
            ED.text("Instead it returned:"), ED.embed(testResult.testResult),
            ED.text(" when applied to the "), highlightLhs
          ])
        );
      }
    }
    default:
      throw new ExhaustiveSwitchError(testResult, 'renderReason not implemented');
  }
}

export function renderReason(testResult: TestResult) : ErrorDisplay {
  switch (testResult.$name) {
    case 'success': return ED.text("Success");
    case 'failure-not-equal': {
      return ED.error.make3(
        ED.para.make1(option['is-none'](testResult)
          ? ED.text("Values not equal")
          : ED.text("Values not equal (using custom equality):")),
        ED.embed(testResult.left),
        ED.embed(testResult.right)
      )
    }
    case 'failure-is-incomparable': {
      const val1IsFunction = typeof testResult.eqResult.value1 === 'function';
      const val2IsFunction = typeof testResult.eqResult.value2 === 'function';
      let msg: ED_TYPES.ErrorDisplay;
      if (val1IsFunction && val2IsFunction) {
        msg = ED.para.make1(ED.text("Attempted to compare two functions using strict equality: did you mean to call them first?"))
      } else if (val1IsFunction || val2IsFunction) {
        msg = ED.para.make1(
          ED.text("Attempted to compare a function to another value using strict equality: did you mean to call the function first?"));
      } else {
        msg = ED.para.make([
          ED.text("Attempted to compare roughnums using strict equality: use "), 
          ED.code(ED.text("is-roughly")),
          ED.text(", or consider using the"),
          ED.code(ED.text("within")), 
          ED.text(" function to compare them instead.")
        ]);
      }
      return ED.error.make3(msg, ED.embed(testResult.left), ED.embed(testResult.right));
    }
    case 'failure-not-different': {
      return ED.error.make3(
        ED.para.make1(testResult.refinement === undefined 
          ? ED.text("Values not different")
          : ED.text("Values not different (using custom equality):")),
        ED.embed(testResult.left),
        ED.embed(testResult.right)
      )
    }
    case 'failure-not-satisfied': {
      return ED.error.make2(
        ED.para.make1(ED.text("Predicate failed for value:")), 
        ED.embed(testResult.val));
    }
    case 'failure-not-dissatisfied': {
      return ED.error.make2(
        ED['para-nospace'].make1(ED.text("Predicate succeeded for value (it should have failed):")),
        ED.embed(testResult.val)
      )
    }
    case 'failure-wrong-exn': {
      return ED.error.make([
        ED.para.make1(ED.text("Got unexpected exception ")),
        ED.embed(testResult.exnActual),
        ED.para.make1(ED.text("when expecting ")),
        ED.embed(testResult.exnExpected)
      ]);
    }
    case 'failure-right-exn': {
      return ED.error.make([
        ED.para.make1(ED.text("Got exception ")),
        ED.embed(testResult.exnActual),
        ED.para.make1(ED.text("and expected it not to contain ")),
        ED.embed(testResult.exnNotExpected)
      ]);
    }
    case 'failure-exn': {
      return ED.error.make([
        ED.para.make1(ED.text("Got unexpected exception ")),
        ED.embed(testResult.actualExn),
      ]);
    }
    case 'failure-no-exn': {
      if (testResult.exnExpected.$name === 'some') {
        return ED.error.make2(
          ED.para.make1(ED.text(
            "No exception raised while evaluting the " 
            + sideOfCheckOp(testResult.exnSrc)
            + (testResult.wanted ? ', expected' : ', expected any exception other than')
            )),
          ED.embed(testResult.exnExpected.value)
        );
      } else {
        return ED.error.make1(
          ED.para.make1(
            ED.text("No exception raised while evaluating the " + sideOfCheckOp(testResult.exnSrc))));
      }
    }
    case 'failure-raise-not-satisfied': {
      return ED.error.make2(
        ED.para.make1(ED.text("Predicate failed for exception:")),
        ED['para-nospace'].make1(ED.embed(testResult.exn))
      );
    }
    case 'failure-raise-not-dissatisfied': {
      return ED.error.make2(
        ED.para.make1(ED.text("Predicate succeeded for exception (it should have failed):")),
        ED['para-nospace'].make1(ED.embed(testResult.exn))
      );
    }
    case 'error-not-boolean': {
      return ED.error.make2(
        ED.para.make1(ED.text("The custom equality function must return a boolean, but instead it returned: ")),
        ED.para.make1(ED.embed(testResult.testResult)));
    }
    case 'error-not-pred': {
      return ED.error.make2(
        ED.para.make1(ED.text("The test predicate must be a " + testResult.arity + "-argument function that returns a boolean, but instead it was: ")),
        ED.para.make1(ED.embed(testResult.predicate)));
    }
    case 'error-not-boolean-pred': {
      return ED.error.make2(
        ED.para.make1(ED.text("The test predicate must return a boolean, but instead it returned: ")),
        ED.para.make1(ED.embed(testResult.testResult)));
      }
    default:
      throw new ExhaustiveSwitchError(testResult, 'renderReason not implemented');
  }
}
