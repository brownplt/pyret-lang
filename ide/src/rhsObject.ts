/* Exports types for dealing with the module results of a pyret program as well
   as spy statements.

   The lifecycle of these values goes like this:
   - Pyret program runs, producing a module result
   - Module result is parsed into multiple `RHSObject`s
   - RenderedValue.tsx is used to render RHSObjects into HTML
   - HTML is dipslayed in the RHS.tsx component or inline in DefChunk.tsx */

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
  tag: 'rhs-check',
  key?: string,
  lhs: RHSCheckValue, // 2 + 2
  rhs: RHSCheckValue, // 4
  path: string, // something like '$check$block8' (not used here)
  loc: string, // something like 'file:///projects/program.arr:4:2-4:14'
  success: boolean, // `true`, since 2 + 2 = 4
};

type N = number;
export type SrcLoc = [string, N, N, N, N, N, N];

export type Location = {
  tag: 'location',
  key?: string,
  name: string,
  value: any,
  srcloc: SrcLoc,
};

export type Trace = {
  tag: 'trace',
  key?: string,
  value: any,
  srcloc: SrcLoc,
};

type RawRHSObject<T> = Omit<T, 'tag'>;

export type RHSObject = Trace | Location | RHSCheck;

export function isTrace(a: RHSObject): a is Trace {
  return a.tag === 'trace';
}

export function isLocation(a: RHSObject): a is Location {
  return a.tag === 'location';
}

export function isRHSCheck(a: RHSObject): a is RHSCheck {
  return a.tag === 'rhs-check';
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
  outdated: boolean,
};

export type RunResult = {
  time: number,
  result: {
    $answer: any,
    $checks: RawRHSObject<RHSCheck>[],
    $locations: RawRHSObject<Location>[],
    $traces: RawRHSObject<Trace>[],
  },
};

export function makeRHSObjects(result: RunResult, moduleUri: string): RHSObjects {
  const {
    $checks,
    $locations,
    $traces,
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

  const nonBuiltinChecks: RHSCheck[] = $checks
    .filter((c) => !/builtin/.test(c.loc))
    .map((c) => ({
      tag: 'rhs-check',
      ...c,
    }));
  const withChecks = withLocations.concat(nonBuiltinChecks);

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
  const withKeys = withChecks.map((rhsObject) => {
    let key = getRow(rhsObject).toString();
    if (isLocation(rhsObject)) {
      key += key + (<Location>rhsObject).name;
    }

    return {
      key,
      ...rhsObject,
    };
  });

  return {
    objects: withKeys,
    outdated: false,
  };
}
