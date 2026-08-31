/*
  Ported from: src/arr/compiler/compile-lib.arr

  run-program and compile-and-run-locator require an in-process Pyret
  runtime (runtime-lib/load-lib realms) and are ported as stubs that
  throw TODOError; everything else is ported fully.
*/

import { sha256 } from './sha256';
import * as A from './ast';
import * as AU from './ast-util';
import * as CL from './concat-lists';
import * as CS from './compile-structs';
import * as D from './desugar';
import * as DP from './desugar-post-tc';
import * as G from './gensym';
import * as J from './js-ast';
import * as JSP from './js-of-pyret';
import * as P from './parse-pyret';
import * as RS from './resolve-scope';
import * as T from './type-check';
import * as W from './well-formed';
import * as CH from './desugar-check';
import { PostScopeMapVisitor } from './ast-visitors';
import {
  Either, left, right,
  InternalCompilerError, TODOError,
  filterMap, each2, mapGetValue, mapMergeNow
} from './shared';

// Pyret's time-now() is milliseconds-since-epoch.
function timeNow(): number {
  return Date.now();
}

// ---------- data CompilationPhase ----------

export interface PhaseDatum {
  name: string;
  time: number;
  result: any;
}

export abstract class CompilationPhaseBase {
  abstract get $name(): string;
  tolist(): PhaseDatum[] {
    const acc: PhaseDatum[] = [];
    let thePhase: CompilationPhase = this as any;
    while (!isStart(thePhase)) {
      acc.unshift({
        name: thePhase.name,
        time: thePhase.time - thePhase.prev.time,
        result: thePhase.result
      });
      thePhase = thePhase.prev;
    }
    return acc;
  }
}

export class Start extends CompilationPhaseBase {
  get $name(): 'start' { return 'start'; }
  constructor(public time: number) { super(); }
}

export class Phase extends CompilationPhaseBase {
  get $name(): 'phase' { return 'phase'; }
  constructor(
    public name: string,
    public result: any,
    public time: number,
    public prev: CompilationPhase
  ) { super(); }
}

export type CompilationPhase = Start | Phase;
export function isStart(x: any): x is Start { return x instanceof Start; }
export function isPhase(x: any): x is Phase { return x instanceof Phase; }
export function start(time: number): Start { return new Start(time); }
export function phase(name: string, result: any, time: number, prev: CompilationPhase): Phase {
  return new Phase(name, result, time, prev);
}

export type URI = string;

// ---------- data PyretCode ----------

export class PyretString {
  get $name(): 'pyret-string' { return 'pyret-string'; }
  constructor(public s: string) {}
}

export class PyretAst {
  get $name(): 'pyret-ast' { return 'pyret-ast'; }
  constructor(public ast: A.Program) {}
}

export type PyretCode = PyretString | PyretAst;
export function pyretString(s: string): PyretString { return new PyretString(s); }
export function pyretAst(ast: A.Program): PyretAst { return new PyretAst(ast); }
export function isPyretString(x: any): x is PyretString { return x instanceof PyretString; }
export function isPyretAst(x: any): x is PyretAst { return x instanceof PyretAst; }

export type ModuleResult = any;

export type Loadable = CS.Loadable;
export type Provides = CS.Provides;

// ---------- Locator ----------

// IMPORTANT: implement locators as plain object literals, never class
// instances. cli-module-loader extends locators by spread (`{ ...loc, uri() {
// ... } }`, see getModuleForDep) and the repl aliases them the same way; spread
// copies only own-enumerable properties, so a class instance's prototype
// methods would be silently dropped. Pyret's Locator `_equals` is likewise not
// ported — locator identity is the `uri()` string (see NOTE at the equality
// discipline in CONVENTIONS.md).
export interface Locator {
  needsCompile(provides: Map<string, CS.Provides>): boolean;
  getModifiedTime(): number;
  getOptions(options: CS.CompileOptions): CS.CompileOptions;
  getModule(): PyretCode;
  getDependencies(): CS.AnyDependency[];
  getNativeModules(): CS.NativeModule[];
  getExtraImports(): CS.ExtraImports;
  getGlobals(): CS.Globals;
  uri(): string;
  name(): string;
  setCompiled(loadable: CS.Loadable, provides: Map<string, CS.Provides>): void;
  getCompiled(): CS.Loadable | undefined;
  getUncached?(): Locator | undefined;   // used by cli-module-loader cached wrappers
  realPath?: string; // optional extras allowed
}

export function stringLocator(uri: URI, s: string): Locator {
  return {
    needsCompile(_provides: Map<string, CS.Provides>): boolean { return true; },
    getModifiedTime(): number { return 0; },
    getOptions(options: CS.CompileOptions): CS.CompileOptions { return options; },
    getModule(): PyretCode { return pyretString(s); },
    getNativeModules(): CS.NativeModule[] { return []; },
    getDependencies(): CS.AnyDependency[] { return getStandardDependencies(pyretString(s), uri); },
    getExtraImports(): CS.ExtraImports { return CS.standardImports; },
    getGlobals(): CS.Globals { return CS.standardGlobals; },
    uri(): URI { return uri; },
    name(): string { return uri; },
    setCompiled(_loadable: CS.Loadable, _provides: Map<string, CS.Provides>): void { return; },
    getCompiled(): CS.Loadable | undefined { return undefined; }
  };
}

// ---------- data Located ----------

export class Located<A2> {
  get $name(): 'located' { return 'located'; }
  constructor(public locator: Locator, public context: A2) {}
}
export function located<A2>(locator: Locator, context: A2): Located<A2> {
  return new Located(locator, context);
}
export function isLocated(x: any): x is Located<any> { return x instanceof Located; }

export function getAst(p: PyretCode, uri: URI): A.Program {
  switch (p.$name) {
    case 'pyret-string': return P.surfaceParse(p.s, uri);
    case 'pyret-ast': return p.ast;
    default: throw new InternalCompilerError("getAst: unknown PyretCode " + (p as any).$name);
  }
}

export function getImportType(i: A.Import): A.ImportType | undefined {
  switch (i.$name) {
    case 's-import': return i.file;
    case 's-import-types': return i.file;
    case 's-include': return i.mod;
    case 's-import-fields': return i.file;
    case 's-include-from': return undefined;
    default: throw new InternalCompilerError("getImportType: unknown Import " + (i as any).$name);
  }
}

export function getDependencies(p: PyretCode, uri: URI): CS.AnyDependency[] {
  const parsed = getAst(p, uri);
  const fromImports = filterMap(getImportType, parsed.imports).map((s) => AU.importToDep(s));
  if (parsed._use === undefined) {
    return fromImports;
  } else {
    return [AU.importToDep(parsed._use.mod), ...fromImports];
  }
}

export function getStandardDependencies(p: PyretCode, uri: URI): CS.AnyDependency[] {
  const modDeps = getDependencies(p, uri);
  return [...modDeps, ...CS.standardImports.imports.map((i) => i.dependency)];
}

export function constDict<A2>(strs: string[], val: A2): Map<string, A2> {
  const d = new Map<string, A2>();
  for (const s of strs) {
    d.set(s, val);
  }
  return d;
}

export interface ToCompile {
  locator: Locator;
  dependencyMap: Map<string, Locator>;
}

export function dictMap<A2, B>(sd: Map<string, A2>, f: (k: string, v: A2) => B): Map<string, B> {
  const sd2 = new Map<string, B>();
  for (const k of sd.keys()) {
    sd2.set(k, f(k, sd.get(k)!));
  }
  return sd2;
}

export const dummyProvides = (uri: URI): CS.Provides =>
  new CS.Provides(uri, new Map(), new Map(), new Map(), new Map());

// A module finder. The dependency chase awaits each result, so a finder may
// be synchronous (CLI: node fs) or asynchronous (browser hosts: fetch, or
// filesystem RPCs to an embedding host like the vscode extension) -- the
// locator steps a host configures are respected either way.
export type DFind<A2> = (context: A2, dep: CS.AnyDependency) => Located<A2> | Promise<Located<A2>>;

export function compileWorklist<A2>(
  dfind: DFind<A2>,
  locator: Locator,
  context: A2
): Promise<ToCompile[]> {
  return compileWorklistKnownModules(dfind, locator, context, new Map());
}

export async function compileWorklistKnownModules<A2>(
  dfind: DFind<A2>,
  locator: Locator,
  context: A2,
  currentModules: Map<string, CS.Provides>
): Promise<ToCompile[]> {
  const tempMarked = new Map<string, boolean>();
  let topo: ToCompile[] = [];
  // Dependencies are located and visited strictly in order (awaited one at a
  // time), so the topological order -- and therefore all downstream output --
  // is byte-identical to the synchronous chase.
  async function visit(locator: Locator, context: A2, currPath: Locator[]): Promise<ToCompile[]> {
    const mark = tempMarked.get(locator.uri());
    if (mark !== undefined) {
      if (mark) {
        throw new InternalCompilerError(
          "Detected module cycle: " + [...currPath].reverse().map((l) => l.uri()).join(" => "));
      }
    } else {
      // mark current locator temporarily
      tempMarked.set(locator.uri(), true);
      const pmap = new Map<string, Locator>();
      const deps = locator.getDependencies();
      const foundMods: Located<A2>[] = [];
      for (const d of deps) {
        const found = await dfind(context, d);
        pmap.set(d.key(), found.locator);
        foundMods.push(found);
      }
      // visit all dependents
      for (const f of foundMods) {
        if (!currentModules.has(f.locator.uri())) {
          await visit(f.locator, f.context, [f.locator, ...currPath]);
        }
      }
      // add current locator to head of topo sort
      topo = [{ locator: locator, dependencyMap: pmap }, ...topo];
      // mark current locator permanently
      tempMarked.set(locator.uri(), false);
    }
    return topo;
  }
  // our include edges are backwards to how the topological sort algorithm expects dependencies,
  // so reverse the result
  const ans = [...await visit(locator, context, [locator])].reverse();
  return ans;
}

export function modulesFromWorklist(
  wl: ToCompile[],
  getLoadable: (t: ToCompile, maxDepTimes: Map<string, number>) => Loadable | undefined
): Map<string, Loadable> {
  const maxDepTimes = depTimesFromWorklist(wl);
  const maybeModules = wl.map((t) => getLoadable(t, maxDepTimes));
  const modules = new Map<string, Loadable>();
  each2((m, t: ToCompile) => {
    if (m !== undefined) {
      modules.set(t.locator.uri(), m);
    }
  }, maybeModules, wl);
  return modules;
}

export function depTimesFromWorklist(wl: ToCompile[]): Map<string, number> {
  const sd = new Map<string, number>();
  for (const located of wl) {
    const curModTime = located.locator.getModifiedTime();
    const dm = located.dependencyMap;
    let maxDepTime = curModTime;
    for (const depKey of dm.keys()) {
      const depLoc = dm.get(depKey)!;
      maxDepTime = Math.max(mapGetValue(sd, depLoc.uri()), maxDepTime);
    }
    sd.set(located.locator.uri(), maxDepTime);
  }
  return sd;
}

export interface CompiledProgram {
  loadables: Loadable[];
  modules: Map<string, Loadable>;
}

export function compileProgramWith(
  worklist: ToCompile[],
  modules: Map<string, Loadable>,
  options: CS.CompileOptions
): CompiledProgram {
  const cache = modules;
  const loadables = worklist.map((w) => {
    const uri = w.locator.uri();
    if (!cache.has(uri)) {
      const provideMap = dictMap(w.dependencyMap, (_k, v) => v.uri());
      options.beforeCompile(w.locator);
      const [loadable, trace] = compileModule(w.locator, provideMap, cache, options);
      // I feel like here we want to generate two copies of the loadable:
      // - One local for calling on-compile with and serializing
      // - One canonicalized for the local cache
      cache.set(uri, loadable);
      const localLoadable = new CS.ModuleAsString(
        AU.localizeProvides(loadable.provides, loadable.compileEnv),
        loadable.compileEnv,
        loadable.postCompileEnv,
        loadable.resultPrinter);
      // allow on-compile to return a new loadable
      return options.onCompile(w.locator, localLoadable, trace);
    } else {
      return mapGetValue(cache, uri);
    }
  });
  return { loadables: loadables, modules: cache };
}

export function compileProgram(worklist: ToCompile[], options: CS.CompileOptions): CompiledProgram {
  return compileProgramWith(worklist, new Map(), options);
}

export function isBuiltinModule(uri: string): boolean {
  return uri.indexOf("builtin://") === 0;
}

// Pyret `==` is structural; `unique` deduplicates compile errors by
// structural equality (sets.list-to-list-set). Names/srclocs/dependencies
// compare via their key() methods.
function structurallyEqual(a: any, b: any): boolean {
  if (a === b) { return true; }
  if (a === null || b === null || typeof a !== 'object' || typeof b !== 'object') { return false; }
  if (Array.isArray(a) || Array.isArray(b)) {
    if (!Array.isArray(a) || !Array.isArray(b) || a.length !== b.length) { return false; }
    return a.every((x, i) => structurallyEqual(x, b[i]));
  }
  if (a.constructor !== b.constructor) { return false; }
  if (typeof a.key === 'function' && typeof b.key === 'function') {
    return a.key() === b.key();
  }
  if (a instanceof Map) {
    if (!(b instanceof Map) || a.size !== b.size) { return false; }
    for (const [k, v] of a) {
      if (!b.has(k) || !structurallyEqual(v, b.get(k))) { return false; }
    }
    return true;
  }
  const ka = Object.keys(a);
  const kb = Object.keys(b);
  if (ka.length !== kb.length) { return false; }
  return ka.every((k) => structurallyEqual(a[k], (b as any)[k]));
}

export function unique<A2>(lst: A2[]): A2[] {
  const out: A2[] = [];
  for (const x of lst) {
    if (!out.some((y) => structurallyEqual(x, y))) {
      out.push(x);
    }
  }
  // Pyret's `sets.list-to-list-set(lst).to-list()` keeps the first occurrence of
  // each element but returns them in REVERSED first-occurrence order; match that
  // so multi-error output (e.g. several unbound names) is byte-identical to the
  // .arr compiler rather than the exact reverse.
  return out.reverse();
}

class SpyStripVisitor extends PostScopeMapVisitor {
  sScopeBlock(node: A.SScopeBlock): A.Expr {
    // foldr: rightmost entries are visited first
    const entries: A.ScopeEntry[] = [];
    for (let i = node.entries.length - 1; i >= 0; i--) {
      const entry = node.entries[i];
      if (!(A.isSScopeStmt(entry) && A.isSSpyBlock(entry.stmt))) {
        entries.unshift(entry.visit(this));
      }
    }
    return new A.SScopeBlock(node.l, entries, node.tail.visit(this));
  }
}
const spyStripVisitor = new SpyStripVisitor();

export function compileModule(
  locator: Locator,
  provideMap: Map<string, URI>,
  modules: Map<string, Loadable>,
  options: CS.CompileOptions
): [Loadable, PhaseDatum[]] {
  /*
    Invariant: provide-map maps dependency keys to URIs
    which ALL must be keys in modules.
  */
  G.reset();
  A.globalNames.reset();
  const env = new CS.CompileEnv(locator.getGlobals(), modules, provideMap);
  const alreadyCompiled = locator.getCompiled();
  if (alreadyCompiled !== undefined) {
    return [
      new CS.ModuleAsString(
        AU.canonicalizeProvides(alreadyCompiled.provides, env),
        alreadyCompiled.compileEnv,
        alreadyCompiled.postCompileEnv,
        alreadyCompiled.resultPrinter),
      []
    ];
  } else {
    options = locator.getOptions(options);
    const libs = locator.getExtraImports();
    const mod = locator.getModule();
    let ast: A.Program | undefined;
    switch (mod.$name) {
      case 'pyret-string':
        ast = P.surfaceParse(mod.s, locator.uri());
        break;
      case 'pyret-ast':
        ast = mod.ast;
        break;
      default:
        throw new InternalCompilerError("compileModule: unknown PyretCode " + (mod as any).$name);
    }
    let ret: CompilationPhase = start(timeNow());
    function addPhase<V>(name: string, value: V): V {
      if (options.collectAll) {
        ret = phase(name, value, timeNow(), ret);
      } else if (options.collectTimes) {
        ret = phase(name, undefined, timeNow(), ret);
      }
      return value;
    }
    let astEnded: A.Program | undefined = AU.appendNothingIfNecessary(ast);
    ast = undefined;
    addPhase("Added nothing", astEnded);
    let wf: CS.CompileResult<A.Program> | undefined = W.checkWellFormed(astEnded);
    astEnded = undefined;
    addPhase("Checked well-formedness", wf);
    const checker = (!(options.checks === "none") && !isBuiltinModule(locator.uri()))
      ? CH.desugarCheck
      : CH.desugarNoChecks;
    if (CS.isOk(wf)) {
      let wfAst: A.Program | undefined = AU.wrapToplevels(wf.code);
      wf = undefined;
      let checked: A.Program | undefined = checker(wfAst);
      wfAst = undefined;
      addPhase(!(options.checks === "none") ? "Desugared (with checks)" : "Desugared (skipping checks)", checked);
      let imported: A.Program | undefined = AU.wrapExtraImports(checked, libs);
      checked = undefined;
      addPhase("Added imports", imported);
      let scoped: CS.ScopeResolution | undefined = RS.desugarScope(imported, env);
      imported = undefined;
      addPhase("Desugared scope", scoped);
      const namedResult = RS.resolveNames(scoped.ast, locator.uri(), env);
      let anyErrors: CS.CompileError[] = [...scoped.errors, ...namedResult.errors];
      scoped = undefined;
      if (anyErrors.length > 0) {
        return [
          new CS.ModuleAsString(dummyProvides(locator.uri()), env, CS.computedNone, CS.err(unique(anyErrors))),
          (options.collectAll || options.collectTimes)
            ? phase("Result", namedResult.ast, timeNow(), ret).tolist()
            : []
        ];
      } else {
        addPhase("Resolved names", namedResult);
        let spied: A.Program | undefined =
          options.enableSpies
            ? namedResult.ast
            : namedResult.ast.visit(spyStripVisitor);
        let provides = dummyProvides(locator.uri());
        // Once name resolution has happened, any newly-created s-binds must be added to bindings...
        let desugared: { ast: A.Program; newBinds: Map<string, CS.ValueBind> } | undefined = D.desugar(spied!);
        spied = undefined;
        mapMergeNow((namedResult.env as CS.ComputedEnv).bindings, desugared.newBinds);
        // ...in order to be checked for bad assignments here
        anyErrors = RS.checkUnboundIdsBadAssignments(desugared.ast, namedResult, env);
        addPhase("Fully desugared", desugared.ast);
        let typeChecked: CS.CompileResult<A.Program>;
        if (anyErrors.length > 0) {
          typeChecked = CS.err(unique(anyErrors));
        } else if (options.typeCheck) {
          const tc = T.typeCheck(desugared.ast, env, namedResult.env, modules);
          if (CS.isOk(tc)) {
            provides = AU.getTypedProvides(namedResult, tc.code, locator.uri(), env);
            typeChecked = CS.ok(tc.code.ast);
          } else {
            typeChecked = tc;
          }
        } else {
          typeChecked = CS.ok(desugared.ast);
        }
        desugared = undefined;
        addPhase("Type Checked", typeChecked);
        // Pyret: options.{should-profile: options.should-profile(locator)}
        // — the function-valued field is replaced by the boolean result for
        // this locator before being handed to code generation.
        options = { ...options, shouldProfile: options.shouldProfile(locator) as any };
        if (CS.isOk(typeChecked)) {
          let tcAst: A.Program | undefined = typeChecked.code;
          let dpAst: A.Program | undefined = DP.desugarPostTc(tcAst, env);
          tcAst = undefined;
          let cleaned: A.Program | undefined = dpAst;
          dpAst = undefined;
          cleaned = cleaned.visit(AU.letrecVisitor)
            .visit(AU.inlineLams)
            .visit(AU.setRecursiveVisitor)
            .visit(AU.setTailVisitor);
          if (!options.userAnnotations) {
            cleaned = cleaned!.visit(AU.stripAnnotationsVisitor);
          }
          addPhase("Cleaned AST", cleaned);
          if (!options.typeCheck) {
            provides = AU.getNamedProvides(namedResult, locator.uri(), env);
          }
          const [finalProvides, cr] = JSP.traceMakeCompiledPyret(addPhase, cleaned!, env, namedResult.env, provides, options);
          cleaned = undefined;
          const canonicalProvides = AU.canonicalizeProvides(finalProvides, env);
          const modResult = new CS.ModuleAsString(canonicalProvides, env, namedResult.env, cr);
          return [
            modResult,
            (options.collectAll || options.collectTimes) ? ret.tolist() : []
          ];
        } else {
          return [
            new CS.ModuleAsString(provides, env, CS.computedNone, typeChecked),
            (options.collectAll || options.collectTimes)
              ? phase("Result", typeChecked, timeNow(), ret).tolist()
              : []
          ];
        }
      }
    } else {
      return [
        new CS.ModuleAsString(dummyProvides(locator.uri()), env, CS.computedNone, wf!),
        (options.collectAll || options.collectTimes)
          ? phase("Result", wf, timeNow(), ret).tolist()
          : []
      ];
    }
  }
}

export type PyretAnswer = any;
export type PyretMod = any;

export function isErrorCompilation(cr: any): boolean {
  return CS.isModuleAsString(cr) && CS.isErr(cr.resultPrinter);
}

export function runProgram(
  _ws: ToCompile[],
  _prog: CompiledProgram,
  _realm: any,
  _runtime: any,
  _options: CS.CompileOptions
): Either<any[], any> {
  throw new TODOError('in-process run not supported; use the standalone path');
}

export function compileAndRunLocator(
  _locator: Locator,
  _finder: any,
  _context: any,
  _realm: any,
  _runtime: any,
  _starterModules: Map<string, Loadable>,
  _options: CS.CompileOptions
): Either<any[], any> {
  throw new TODOError('in-process run not supported; use the standalone path');
}

export function compileStandalone(
  wl: ToCompile[],
  starterModules: Map<string, Loadable>,
  options: CS.CompileOptions
): Either<CS.CompileError[], { jsAst: J.JExprT; natives: string[] }> {
  const compiled = compileProgramWith(wl, starterModules, options);
  return makeStandalone(wl, compiled, options);
}

// NOTE(joe): I strongly suspect options will be used in the future
export function makeStandalone(
  wl: ToCompile[],
  compiled: CompiledProgram,
  options: CS.CompileOptions
): Either<CS.CompileError[], { jsAst: J.JExprT; natives: string[] }> {
  let natives: string[] = [];
  for (const w of wl) {
    natives = [...w.locator.getNativeModules().map((n) => n.path), ...natives];
  }
  natives.sort();

  let allCompileProblems: CS.CompileError[] = [];
  const staticModules = new J.JObj(CL.map_list<ToCompile, J.JFieldT>((w) => {
    const loadable = mapGetValue(compiled.modules, w.locator.uri());
    const rp = loadable.resultPrinter;
    if (CS.isOk(rp)) {
      return new J.JField(w.locator.uri(), new J.JRawCode(rp.code.pyretToJsRunnable()));
    } else {
      allCompileProblems = [...rp.problems, ...allCompileProblems];
      return new J.JField(w.locator.uri(), new J.JRawCode("\"error\""));
    }
  }, wl));

  const checkStr = !options.checkMode ? "none" : options.checks;

  const runtimeOptions = new J.JObj(CL.clist<J.JFieldT>(
    new J.JField("checksFormat", new J.JStr(options.checksFormat)),
    new J.JField("checks", new J.JStr(checkStr)),
    new J.JField("disableAnnotationChecks",
      options.runtimeAnnotations
        ? new J.JFalse()
        : new J.JTrue()),
    new J.JField("pauseSchedule",
      options.pauseSchedule === undefined
        ? new J.JFalse()
        : new J.JStr(options.pauseSchedule))
  ));

  if (allCompileProblems.length > 0) {
    return left(allCompileProblems);
  } else {
    const depmap = new J.JObj(CL.map_list<ToCompile, J.JFieldT>((w) => {
      const deps = w.dependencyMap;
      return new J.JField(w.locator.uri(),
        new J.JObj(CL.map_list<string, J.JFieldT>((k) =>
          new J.JField(k, new J.JStr(deps.get(k)!.uri())),
          [...deps.keys()].sort())));
    }, wl));

    const toLoad = new J.JList(false, CL.map_list<ToCompile, J.JExprT>((w) =>
      new J.JStr(w.locator.uri()), wl));

    const uris = new J.JObj(CL.map_list<ToCompile, J.JFieldT>((w) => {
      const uri = w.locator.uri();
      const hashed = sha256(uri);
      return new J.JField(hashed, new J.JStr(uri));
    }, wl));

    const programAsJs = new J.JObj(CL.clist<J.JFieldT>(
      new J.JField("staticModules", staticModules),
      new J.JField("depMap", depmap),
      new J.JField("toLoad", toLoad),
      new J.JField("uris", uris),
      new J.JField("runtimeOptions", runtimeOptions)
    ));

    return right({
      jsAst: programAsJs,
      natives: natives
    });
  }
}
