export type RHSCheckValue = {
  exception: boolean,
  value: any,
  exception_val: any,
};

// The result of evaluating a `check:` or `examples:` block.
//
// check:
//   2 + 2 is 4
// end
export type RHSCheck = {
  key?: string,
  lhs: RHSCheckValue, // 2 + 2
  rhs: RHSCheckValue, // 4
  path: string, // something like "$check$block8" (not used here)
  loc: string, // something like "file:///projects/program.arr:4:2-4:14"
  success: boolean, // `true`, since 2 + 2 = 4
};

type N = number;
export type SrcLoc = [string, N, N, N, N, N, N];

export type SpyMessage = {
  key?: string,
  value: any,
  loc: string,
};

export type SpyValue = {
  key: string,
  value: any,
  loc: string,
};

export type Location = {
  key?: string,
  name: string,
  value: any,
  srcloc: SrcLoc,
};

export type Trace = {
  key?: string,
  value: any,
  srcloc: SrcLoc,
};

export type RHSObject = Trace | Location | RHSCheck | SpyMessage | SpyValue;

export function isSpyValue(a: RHSObject): a is SpyValue {
  const hasProp = Object.prototype.hasOwnProperty;
  return hasProp.call(a, 'key') && hasProp.call(a, 'value') && hasProp.call(a, 'loc');
}

export function isSpyMessage(a: RHSObject): a is SpyMessage {
  const hasProp = Object.prototype.hasOwnProperty;
  return hasProp.call(a, 'message') && hasProp.call(a, 'loc');
}

export function isTrace(a: RHSObject): a is Trace {
  const hasProp = Object.prototype.hasOwnProperty;
  return hasProp.call(a, 'value') && !hasProp.call(a, 'name');
}

export function isLocation(a: RHSObject): a is Location {
  const hasProp = Object.prototype.hasOwnProperty;
  return hasProp.call(a, 'name');
}

export function isRHSCheck(a: RHSObject): a is RHSCheck {
  const hasProp = Object.prototype.hasOwnProperty;
  return hasProp.call(a, 'lhs');
}

export type HasSrcLoc =
  { srcloc: SrcLoc } // Trace or Location
  | { loc: string }; // RHSCheck

// Typescript can't yet narrow union types using the default
// hasOwnProperty predicate. This one works, though.
// See https://fettblog.eu/typescript-hasownproperty/
function hasOwnProperty<X extends {}, Y extends PropertyKey>(
  obj: X,
  prop: Y,
): obj is X & Record<Y, unknown> {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

export function getRow(hasSrcLoc: HasSrcLoc): number {
  if (hasOwnProperty(hasSrcLoc, 'srcloc')) {
    return hasSrcLoc.srcloc[1];
  }

  const { loc } = hasSrcLoc;

  const matches = loc.match(/:(\d+):\d+-\d+:\d+$/);

  if (matches === null) {
    throw new Error('getRow: received malformed srcloc');
  }

  return Number(matches[1]);
}

export type RHSObjects = {
  objects: RHSObject[],
  spyData: (SpyMessage | SpyValue)[]
  outdated: boolean,
};

export type RunResult = {
  time: number,
  result: {
    $answer: any,
    $checks: RHSCheck[],
    $locations: Location[],
    $traces: Trace[],
  },
};

export function makeRHSObjects(result: RunResult, moduleUri: string): RHSObjects {
  const {
    $checks,
    $locations,
    $traces,
  } = result.result;

  function compareRHSObjects(a: RHSObject, b: RHSObject): number {
    return getRow(a) - getRow(b);
  }

  // only keep toplevel expressions from this module.
  const justTraces: RHSObject[] = $traces.filter((t) => t.srcloc[0] === moduleUri);

  const withLocations = justTraces.concat($locations.map((location) => ({
    ...location,
    value: (result as any).result[location.name],
  })));

  const nonBuiltinChecks = $checks.filter((c) => !/builtin/.test(c.loc));
  const withChecks = withLocations.concat(nonBuiltinChecks);

  const sorted = withChecks.sort(compareRHSObjects);

  // Add unique keys to each object so that React can re-render them properly.
  // We assume that each trace / check / location came from a different row.
  const withKeys = sorted.map((rhsObject) => ({
    key: getRow(rhsObject).toString(),
    ...rhsObject,
  }));

  return {
    objects: withKeys,
    outdated: false,

    // The spy messages (if any) are already present in the Redux store. A
    // reducer will interleave them with this object so we don't have to!
    // See also: 'handleRunSuccess' (reducer).
    spyData: [],
  };
}
