/* Exports types for dealing with the module results of a pyret program as well
   as spy statements.

   The lifecycle of these values goes like this:
   - Pyret program runs, producing a module result
   - Module result is parsed into multiple `RHSObject`s
   - RenderedValue.tsx is used to render RHSObjects into HTML
   - HTML is dipslayed in the RHS.tsx component or inline in DefChunk.tsx */

import { CompileAndRunResult } from "./control";
import { Srcloc } from "../../src/runtime/common-runtime-types";
import type { CheckBlockResult, RenderedCheckBlockResult, RenderedCheckResultsAndSummary } from "../../src/runtime/checker";

export type RHSCheckValue = {
  exception: boolean,
  value: any,
  exception_val: any,
};

export type CheckResults = {
  tag: 'check-results',
  key: 'check-results',
  checkBlockResults: CheckBlockResult[],
  renderedCheckBlockResults: RenderedCheckResultsAndSummary,
}


export type Location = {
  tag: 'location',
  key?: string,
  name: string,
  value: any,
  srcloc: Srcloc,
};

export type Trace = {
  tag: 'trace',
  key?: string,
  value: any,
  srcloc: Srcloc,
};

export type ExamplarResult = { success: boolean, result: CompileAndRunResult };

export type ExamplarReport = {
  tag: 'examplar',
  key?: string,
  wheatResults: ExamplarResult[],
  chaffResults: ExamplarResult[],
  hintMessage: string,
  qtmVariations: number,
  srcloc: Srcloc
  // TODO(joe): add much more here to report on wheat/chaff specifics
}

type RawRHSObject<T> = Omit<T, 'tag'>;

export type RHSObject = Trace | Location | ExamplarReport | CheckResults;

export function isTrace(a: RHSObject): a is Trace {
  return a.tag === 'trace';
}

export function isLocation(a: RHSObject): a is Location {
  return a.tag === 'location';
}

export function isCheckResults(a: RHSObject): a is CheckResults {
  return a.tag === 'check-results';
}

export function isExamplarReport(a: RHSObject): a is ExamplarReport {
  return a.tag === 'examplar';
}

export type HasSrcLoc =
  { srcloc: Srcloc } // Trace or Location
  | { loc: Srcloc }; // RHSCheck

// Typescript can't yet narrow union types using the default
// hasOwnProperty predicate. This one works, though.
// See https://fettblog.eu/typescript-hasownproperty/
function hasOwnProperty<X extends {}, Y extends PropertyKey>(
  obj: X,
  prop: Y,
): obj is X & Record<Y, any> {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

export function getRow(hasSrcLoc: HasSrcLoc): number {
  if (hasOwnProperty(hasSrcLoc, 'srcloc')) {
    return hasSrcLoc.srcloc[1];
  }

  const { loc } = hasSrcLoc;

  if (loc.length !== 7) {
    throw new Error(`getRow: received malformed srcloc ${String(loc)}`);
  }

  return Number(loc[1]);
}

export type RHSObjects = {
  objects: RHSObject[],
  outdated: boolean,
};

export type RunResult = {
  time: number,
  result: {
    $answer: any,
    $locations: RawRHSObject<Location>[],
    $traces: RawRHSObject<Trace>[],
    $checks: CheckBlockResult[],
    $renderedChecks: RenderedCheckResultsAndSummary,
  },
};

export function makeRHSObjects(result: RunResult, moduleUri: string): RHSObject[] {
  const {
    $locations,
    $traces,
    $checks,
    $renderedChecks
  } = result.result;

  // only keep toplevel expressions from this module.
  const justTraces: RHSObject[] = $traces
    .filter((t) => t.srcloc[0] === moduleUri)
    .map((t) => ({
      tag: 'trace',
      ...t,
    }));

  const withLocations = justTraces.concat($locations.map((location) => ({
    tag: 'location',
    ...location,
    value: (result as any).result[location.name],
  })));

  // Add unique keys to each object so that React can re-render them properly.
  // We assume that each trace / check / location came from a different row.
  // NOTE(alex): this is not true for data definitions and 'is-data-variant' functions
  //
  //  data Foo:
  //    | bar
  //  end
  //
  //
  //  foo: () -> ()
  // and
  //  is-foo: (Foo) -> bool
  //
  // will share the same location
  //
  let withKeys : HasSrcLoc[] = withLocations.filter(rhs => 'srcloc' in rhs) as HasSrcLoc[];
  withKeys = withKeys.map((rhsObject) => {
    let key = getRow(rhsObject).toString();
    if (isLocation(rhsObject as RHSObject)) {
      key += key + (<Location>rhsObject).name;
    }

    return {
      key,
      ...rhsObject,
    };
  });

  withKeys.sort((a, b) => getRow(a) - getRow(b));

  const checkResults : RHSObject = {
    tag: 'check-results',
    key: 'check-results',
    checkBlockResults: $checks,
    renderedCheckBlockResults: $renderedChecks
  };

  return [... withKeys as RHSObject[], checkResults ];
}
