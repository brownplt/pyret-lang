/*
  Ported from: src/arr/compiler/repl.arr

  The REPL library: manages the chain of interaction modules, threading
  each interaction's provides into the next one's globals, exactly as the
  Pyret original does. The original talks to runtime-lib/load-lib realms
  directly; those are Pyret-runtime constructs, so this port factors them
  behind a host-provided ReplExecutor. The compile side (provide
  rewriting, globals chaining, locator caching, worklist compilation) is
  fully implemented here; the host (e.g. code.pyret.org wrapping
  load-lib, or a test harness) supplies execution.

  Mirrors of the runtime touchpoints in repl.arr:
    CL.run-program(ws, prog, realm, runtime, options)
      -> compile errors are filtered HERE (left(problems)), then
         executor.run(realm, jsSource, options) receives the standalone
         program source (program.js-ast.to-ugly-source()), like
         L.run-program in the original.
    L.is-success-result(answer) -> executor.isSuccessResult(answer)
    L.get-result-realm(answer)  -> executor.getResultRealm(answer)
*/

import * as A from './ast';
import * as CS from './compile-structs';
import * as CL from './compile-lib';
import * as P from './parse-pyret';
import { Either, left, right, mapSet } from './shared';

// Result is whatever the host's run produces (a load-lib result in CPO).
// run may be synchronous or return a Promise (a browser host has to thread
// execution through the Pyret runtime's trampoline, which is async); the
// repl awaits it either way, so its own entry points return Promises.
export interface ReplExecutor<Realm = any, Result = any> {
  run(realm: Realm, programJsSource: string, options: CS.CompileOptions): Result | Promise<Result>;
  isSuccessResult(result: Result): boolean;
  getResultRealm(result: Result): Realm;
}

// May be async: browser hosts locate modules via fetch or host RPCs, and the
// dependency chase (CL.compileWorklist*) awaits each step.
export type Finder<A2> = (context: A2, dep: CS.AnyDependency) => CL.Located<A2> | Promise<CL.Located<A2>>;

export function makeProvideForRepl(p: A.Program): A.Program {
  return new A.SProgram(
    p.l,
    p._use,
    new A.SProvideNone(p.l),
    new A.SProvideTypesNone(p.l),
    [...p.provides, new A.SProvideBlock(p.l, [], [
      new A.SProvideName(p.l, new A.SStar(p.l, [])),
      new A.SProvideType(p.l, new A.SStar(p.l, [])),
      new A.SProvideModule(p.l, new A.SStar(p.l, []))
      // Adding s-provide-data for imports would be redundant because the
      // name/type exports will refer to the data anyway
    ])],
    p.imports,
    p.block
  );
}

export function addGlobalsFromEnv(postEnv: CS.ComputedEnv, g: CS.Globals): CS.Globals {
  let moduleGlobals = g.modules;
  for (const [k, mb] of postEnv.moduleEnv) {
    moduleGlobals = mapSet(moduleGlobals, k, mb.origin);
  }
  let valGlobals = g.values;
  for (const [k, vb] of postEnv.env) {
    valGlobals = mapSet(valGlobals, k, vb.origin);
  }
  let typeGlobals = g.types;
  for (const [k, tb] of postEnv.typeEnv) {
    typeGlobals = mapSet(typeGlobals, k, tb.origin);
  }
  return new CS.Globals(moduleGlobals, valGlobals, typeGlobals);
}

// Mirrors CL.run-program from compile-lib.arr for the repl's use: filter
// compile errors, build the standalone program, hand its JS source to the
// executor.
async function runProgramWith<Realm, Result>(
  executor: ReplExecutor<Realm, Result>,
  ws: CL.ToCompile[],
  prog: CL.CompiledProgram,
  realm: Realm,
  options: CS.CompileOptions
): Promise<Either<any[], Result>> {
  const errors = prog.loadables.filter(CL.isErrorCompilation);
  if (errors.length === 0) {
    const program = CL.makeStandalone(ws, prog, options);
    if (program.$name === 'left') {
      return left(program.v as any[]);
    }
    return right(await executor.run(realm, program.v.jsAst.toUglySource(), options));
  } else {
    return left(errors.map((e) => e.resultPrinter));
  }
}

export class Cancelled extends Error {
  constructor() {
    super('The run was cancelled');
    this.name = 'Cancelled';
  }
}

// cancel() rejects the promise *and* sets isCancelled()
// The promise half is used to race([]) against other async work,
// while the isCancelled() half is used for synchronous checks for early
// returns.
export interface Cancellation {
  cancel(): void;
  promise: Promise<never>;
  isCancelled(): boolean;
}

export function makeCancellation(): Cancellation {
  let cancelled = false;
  const { promise, reject } = Promise.withResolvers<never>();
  // *Always* catch these. They can be rejected from many places, sometimes
  // before relevant places can install handlers, or after relevant places have
  // cleared them. These should never bubble up as "UnhandledRejections"
  promise.catch(() => undefined);
  return {
    promise,
    isCancelled: () => cancelled,
    cancel: () => {
      if (cancelled) { return; }
      cancelled = true;
      reject(new Cancelled());
    },
  };
}

// A cancellation that is never triggered, for callers that do not have one.
const NEVER_CANCELLED: Cancellation = {
  promise: new Promise<never>(() => undefined),
  isCancelled: () => false,
  cancel: () => undefined,
};

export interface Repl<A2, Realm = any, Result = any> {
  restartInteractions(defsLocator: CL.Locator, options: CS.CompileOptions, cancel?: Cancellation): Promise<Either<any[], Result>>;
  makeInteractionLocator(getInteractions: () => string): CL.Locator;
  makeDefinitionsLocator(getDefs: () => string, globals: CS.Globals): CL.Locator;
  runInteraction(locator: CL.Locator, cancel?: Cancellation): Promise<Either<any[], Result>>;
}

export function makeRepl<A2, Realm = any, Result = any>(
  executor: ReplExecutor<Realm, Result>,
  modules: Map<string, CS.Loadable>,
  realm: Realm,
  compileContext: A2,
  makeFinder: () => Finder<A2>
): Repl<A2, Realm, Result> {

  let globals: CS.Globals = CS.standardGlobals;
  let currentCompileOptions: CS.CompileOptions = { ...CS.defaultCompileOptions, checks: 'main' };
  let currentModules: Map<string, CS.Loadable> = modules;
  let currentRealm: Realm = realm;
  let locatorCache = new Map<string, CL.Locator>();
  let currentInteraction = 0;
  let currentFinder: Finder<A2> = makeFinder();

  const finder: Finder<A2> = (context, dep) => {
    if (dep instanceof CS.Dependency) {
      const cached = locatorCache.get(dep.arguments[0]);
      if (cached !== undefined) {
        return new CL.Located(cached, context);
      }
      return currentFinder(context, dep);
    }
    return currentFinder(context, dep);
  };

  function updateEnv(result: Result, loc: CL.Locator, cr: CS.Loadable): void {
    globals = addGlobalsFromEnv(cr.postCompileEnv as CS.ComputedEnv, globals);
    locatorCache.set(loc.uri(), loc);
    currentRealm = executor.getResultRealm(result);
  }

  async function runInteraction(
    locator: CL.Locator,
    cancel: Cancellation = NEVER_CANCELLED
  ): Promise<Either<any[], Result>> {
    // Cancellation here kills this function's evaluation, but the
    // compileWorklist continues to completion regardless.
    const worklist = await Promise.race([
      cancel.promise,
      CL.compileWorklistKnownModules(finder, locator, compileContext, currentModules as any),
    ]);

    const compiled = CL.compileProgramWith(worklist, currentModules, currentCompileOptions);
    // It's important to guard synchronously here so we don't even *launch* a
    // run below if we cancelled during compile
    if (cancel.isCancelled()) { throw new Cancelled(); }
    for (const [k, v] of compiled.modules) {
      currentModules.set(k, v);
    }
    // At this point, we trust runProgramWith to run synchronously right up
    // until a Pyret stack is installed. breakAll() (e.g. stop button) is the
    // stop mechanism from here on, not the `cancel`
    const result = await runProgramWith(executor, worklist, compiled, currentRealm, currentCompileOptions);
    if (result.$name === 'right') {
      if (executor.isSuccessResult(result.v)) {
        updateEnv(result.v, locator, compiled.loadables[compiled.loadables.length - 1]);
      }
    }
    return result;
  }

  function restartInteractions(
    defsLocator: CL.Locator,
    options: CS.CompileOptions,
    cancel: Cancellation = NEVER_CANCELLED
  ): Promise<Either<any[], Result>> {
    currentInteraction = 0;
    currentCompileOptions = options;
    currentRealm = realm;
    locatorCache = new Map();
    currentModules = new Map(modules); // Make a copy
    currentFinder = makeFinder();
    globals = defsLocator.getGlobals();
    return runInteraction(defsLocator, cancel);
  }

  function makeInteractionLocator(getInteractions: () => string): CL.Locator {
    currentInteraction = currentInteraction + 1;
    const thisInteraction = currentInteraction;
    const uri = 'interactions://' + String(thisInteraction);
    let ast: A.Program | undefined = undefined;
    function getAst(): A.Program {
      if (ast === undefined) {
        const interactions = getInteractions();
        const parsed = P.surfaceParse(interactions, uri);
        ast = makeProvideForRepl(parsed);
      }
      return ast;
    }
    const globalsNow = globals;
    const self: CL.Locator = {
      getUncached: () => undefined,
      needsCompile: (_provs) => true,
      getModifiedTime: () => 0,
      getOptions: (options) => options,
      getNativeModules: () => [],
      getModule: () => new CL.PyretAst(getAst()),
      getExtraImports: () => new CS.ExtraImports([]),
      getDependencies: () => {
        const modDeps = CL.getDependencies(self.getModule(), self.uri());
        return [...modDeps, ...self.getExtraImports().imports.map((i) => i.dependency)];
      },
      getGlobals: () => globalsNow,
      uri: () => uri,
      name: () => 'interactions' + String(thisInteraction),
      setCompiled: (_env, _result) => undefined,
      getCompiled: () => undefined,
    };
    return self;
  }

  function makeDefinitionsLocator(getDefs: () => string, defsGlobals: CS.Globals): CL.Locator {
    let ast: A.Program | undefined = undefined;
    function getAst(): A.Program {
      if (ast === undefined) {
        const initialDefinitions = getDefs();
        const parsed = P.surfaceParse(initialDefinitions, 'definitions://');
        ast = makeProvideForRepl(parsed);
      }
      return ast;
    }
    const self: CL.Locator = {
      getUncached: () => undefined,
      needsCompile: (_provs) => true,
      getModifiedTime: () => 0,
      getOptions: (options) => options,
      getNativeModules: () => [],
      getModule: () => new CL.PyretAst(getAst()),
      getExtraImports: () => CS.standardImports,
      getDependencies: () => CL.getStandardDependencies(self.getModule(), self.uri()),
      getGlobals: () => defsGlobals,
      uri: () => 'definitions://',
      name: () => 'definitions',
      setCompiled: (_env, _result) => undefined,
      getCompiled: () => undefined,
    };
    return self;
  }

  return {
    restartInteractions,
    makeInteractionLocator,
    makeDefinitionsLocator,
    runInteraction,
  };
}
