import * as J from 'estree';
import type * as A from './ts-ast'
import { NativeModule, Loadable, URI, Provides, CompileResult, Dependency, ExtraImports, Globals } from './ts-compile-structs';
import { List, PFunction, MutableStringDict, StringDict, Either, PMethod, Runtime, Option, PausePackage, PTuple, SuccessResult } from './ts-impl-types';
import type * as TAU from './ts-ast-util';
import type * as TJ from './ts-codegen-helpers';
import type * as TCSH from './ts-compile-structs-helpers';
import type * as P from './parse-pyret';
import type * as TCS from './ts-compile-structs';
import type * as TG from './ts-gensym';
import type * as TW from './ts-well-formed-impl';
import type * as D from './ts-desugar-impl';
import type * as DP from './ts-desugar-post-tc';
import type * as TC from './ts-type-check-impl';
import type * as JSP from './ts-js-of-pyret';
import type * as TTS from './ts-type-check-structs';

export type CompilationPhase = 
  | { $name: "start", dict: { 'time': Number } }
  | {
    $name: "phase",
    dict: 
      {
        'name': string,
        'result': any,
        'time': Number,
        'prev': CompilationPhase
      }
  }

export type PyretCode = 
  | { $name: "pyret-string", dict: { 's': string } }
  | { $name: "pyret-ast", dict: { 'ast': A.Program } }

export type CompileTODO = 
  | { $name: "already-done", dict: { 'result': Loadable } }
  | {
    $name: "arr-js-file",
    dict: { 'provides': Provides, 'header-file': string, 'code-file': string }
  }
  | {
    $name: "arr-file",
    dict: { 'mod': PyretCode, 'libs': TCS.ExtraImports, 'options': CompileOptions }
  }

export type Located<a> = 
  | { $name: "located", dict: { 'locator': Locator, 'context': a } }


export type Locator = {
  dict: {
    'get-modified-time': PMethod<Locator, () => number>,
    'get-options': PMethod<Locator, (opts: CompileOptions) => CompileOptions>,
    'needs-compile': PMethod<Locator, (provides: StringDict<Provides>) => boolean>,
    'get-module': PMethod<Locator, () => PyretCode>,
    'get-dependencies': PMethod<Locator, () => List<Dependency>>,
    'get-native-modules': PMethod<Locator, () => List<NativeModule>>,
    'get-extra-imports': PMethod<Locator, () => ExtraImports>,
    'get-globals': PMethod<Locator, () => Globals>,
    uri: PMethod<Locator, () => URI>,
    name: PMethod<Locator, () => string>,
    'set-compiled': PMethod<Locator, (loadable: Loadable, provides: StringDict<Provides>) => void>,
    'get-compiled': PMethod<Locator, (options: CompileOptions) => CompileTODO>,
  }
}

export type CompileOptions = {
  dict: {
    'add-profiling': boolean,
    'allow-shadowed' : boolean,
    'base-dir': string,
    'before-compile' : PMethod<CompileOptions, (locator: Locator) => void>,
    'build-runnable' : string,
    'builtin-js-dirs' : List<string>,
    'check-all' : boolean,
    'check-mode' : boolean,
    'checks' : string,
    'collect-all' : boolean,
    'collect-times' : boolean,
    'compile-mode' : TCS.CompileMode,
    'compiled-cache' : string,
    'compiled-dir' : string,
    'compiled-read-only': List<string>,
    'deps-file' : string,
    'display-progress' : boolean,
    'enable-spies' : boolean,
    'html-file' : Option<string>,
    'ignore-unbound' : boolean,
    'inline-case-body-limit': number,
    'lint' : boolean,
    'log-error' : PFunction<(s: string) => string>,
    'log': PFunction<(s: string, toClear: Option<number>) => void>,
    'module-eval' : boolean,
    'on-compile' : PMethod<CompileOptions, (locator: Locator, loadable: Loadable, trace: any) => Loadable>,
    'pipeline' : TCS.Pipeline
    'proper-tail-calls' : boolean,
    'recompile-builtins': boolean,
    'require-config' : string,
    'runtime-annotations' : boolean,
    'runtime-builtin-relative-path' : Option<string>,
    'runtime-path': string,
    'session-delete' : boolean,
    'session-filter' : Option<string>,
    'session' : string,
    'should-profile': PMethod<CompileOptions, (uri: string) => boolean>,
    'standalone-file' : string,
    'this-pyret-dir': string,
    'type-check' : boolean,
    'user-annotations' : boolean,
  }
}


type ToCompile = {
  dict: {
    locator: Locator,
    'dependency-map': MutableStringDict<string>,
  }
}

type CompiledProgram = {
  dict: {
    loadables : List<Loadable>, 
    modules : MutableStringDict<Loadable>
  }
}


export interface Exports {
  dict: { values: { dict: {
    'compile-worklist': PFunction<<A>(dfind: PFunction<(context: A, dep: Dependency) => Located<A>>, locator: Locator, context: A) => List<ToCompile>>,
    'compile-worklist-known-modules': PFunction<<A>(dfind: PFunction<(context: A, dep: Dependency) => Located<A>>, locator: Locator, context: A, currentModules: MutableStringDict<Loadable>) => List<ToCompile>>,
    'get-dependencies': PFunction<(p: PyretCode, uri: string) => List<Dependency>>,
    'get-standard-dependencies': PFunction<(p: PyretCode, uri: string) => List<Dependency>>,
    'make-standalone': PFunction<(worklist: List<ToCompile>, compiled: CompiledProgram, options: CompileOptions) => 
          Either<List<any>, {dict: {'js-ast': J.ObjectExpression, natives: string[]}}>>

    'modules-from-worklist-known-modules': PFunction<(worklist: List<ToCompile>, modules: MutableStringDict<Loadable>, maxDepTimes: StringDict<number>, getLoadable: PFunction<(toCompile: ToCompile, maxDepTimes: StringDict<number>) => Option<Loadable>>) => MutableStringDict<Loadable>>,
    'dep-times-from-worklist': PFunction<(worklist: List<ToCompile>, baseTime: number) => StringDict<number>>,
    'compile-program-with': PFunction<(worklist: List<ToCompile>, modules: MutableStringDict<Loadable>, options: CompileOptions) => CompiledProgram>
    'compile-program': PFunction<(worklist: List<ToCompile>, options: CompileOptions) => CompiledProgram>
  }}}
}

type EitherExports = {
  dict: {
    values: {
      dict: {
        'left': PFunction<<L, R>(l : L) => TJ.Variant<Either<L, R>, "left">>,
        'right': PFunction<<L, R>(r : R) => TJ.Variant<Either<L, R>, "right">>,
      }
    }
  }
}

type ShaExports = {
  dict: {
    values: {
      dict: {
        sha256: PFunction<(str: string) => string>,
      }
    }
  }
}

type ResolveScopeExports = {
  dict: {
    values: {
      dict: {
        'desugar-scope': PFunction<(program: A.Program, env: TCS.CompileEnvironment, options : CompileOptions) => TCS.ScopeResolution>,
        'resolve-names': PFunction<(program: A.Program, uri: string, env: TCS.CompileEnvironment) => TCS.NameResolution>,
        'check-unbound-ids-bad-assignments': PFunction<(program: A.Program, namedResult: TCS.NameResolution, env: TCS.CompileEnvironment) => List<TCS.CompileError>>,
      }
    }
  }
}

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['parse-pyret'] },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers'] },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-compile-structs-helpers'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['ast-util.arr'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr'] },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-gensym'] },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-well-formed-impl'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['resolve-scope.arr'] },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-desugar-impl'] },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-desugar-post-tc'] },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-type-check-impl'] },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-js-of-pyret'] },
    { 'import-type': 'builtin', name: 'either' },
    { 'import-type': 'builtin', name: 'sha' },
  ],
  nativeRequires: [],
  provides: {
    values: {
      'compile-worklist': "tany",
      'compile-worklist-known-modules': "tany",
      'get-dependencies': "tany",
      'get-standard-dependencies': "tany",
      'make-standalone': "tany",
      'modules-from-worklist-known-modules': "tany",
      'dep-times-from-worklist': "tany",
      "compile-program-with": "tany",
      "compile-program": "tany",
    },
  },
  theModule: function(runtime: Runtime, _, __, 
    Pin: P.Exports,
    tj : TJ.Exports,
    TCSH: TCSH.Exports,
    Ain: A.Exports,
    AUin: TAU.Exports,
    CSin: TCS.Exports,
    Gin: TG.Exports,
    Win: TW.Exports,
    RSin: ResolveScopeExports,
    Din: D.Exports,
    DPin: DP.Exports,
    TCin: TC.Exports,
    JSPin: JSP.Exports,
    Ein: EitherExports,
    SHAin: ShaExports,
  ) {
    const { 
      ExhaustiveSwitchError,
      InternalCompilerError,
      ArrayExpression,
      Literal,
      listToArray,
      ObjectExpression,
      Property,
      mutableStringDictFromMap,
      stringDictFromMap,
      mapFromMutableStringDict,
      mapFromStringDict,
      map,
    } = tj;
    const { callMethod } = TCSH;
    const A = Ain.dict.values.dict;
    const AU = AUin.dict.values.dict;
    const CS = CSin.dict.values.dict;
    const P = Pin.dict.values.dict;
    const E = Ein.dict.values.dict;
    const G = Gin.dict.values.dict;
    const SHA = SHAin.dict.values.dict;
    const W = Win.dict.values.dict;
    const RS = RSin.dict.values.dict;
    const D = Din.dict.values.dict;
    const DP = DPin.dict.values.dict;
    const TC = TCin.dict.values.dict;
    const JSP = JSPin.dict.values.dict;

    function prettyIsh(v : any) : string {
      return require('util').inspect(v, {depth:null});
    }

    function getAst(p : PyretCode, uri: string): A.Program {
      switch(p.$name) {
        case 'pyret-string': return P['surface-parse'].app(p.dict.s, uri);
        case 'pyret-ast': return p.dict.ast;
        default: throw new ExhaustiveSwitchError(p);
      }
    }

    function getImportType(i: A.Import): A.ImportType | false {
      switch(i.$name) {
        case 's-import':
        case 's-import-types': 
        case 's-import-fields': return i.dict.file;
        case 's-include': return i.dict.mod;
        case 's-include-from': return false;
        default: throw new ExhaustiveSwitchError(i);
      }
    }

    function getDependencies(p: PyretCode, uri: string): Dependency[] {
      const parsed = getAst(p, uri);
      const fromImports: Dependency[] = [];
      for (let i of listToArray(parsed.dict.imports)) {
        const it = getImportType(i);
        if (it) { fromImports.push(AU['import-to-dep'].app(it)); }
      }
      const _use = parsed.dict._use;
      if(_use.$name === 'some') {
        fromImports.unshift(AU['import-to-dep'].app(_use.dict.value.dict.mod));
      }
      let hasCheckBlock = false;
      const visitor = {
        's-check': (self, cb) => {
          hasCheckBlock = true;
        }
      }
      map<A.Program | A.Expr, A.Program>(visitor, parsed);
      if(hasCheckBlock) {
        fromImports.push(CS['builtin'].app('checker'));
      }
      return fromImports;
    }

    function getStandardDependencies(p: PyretCode, uri: string): Dependency[] {
      const ret = getDependencies(p, uri);
      const fromImports = listToArray((CS['minimal-imports'] as TCS.ExtraImports).dict.imports);
      const deps = fromImports.map((imp : TCS.ExtraImport) => imp.dict.dependency);
      return ret.concat(deps);
    }


    function compileWorklist<A>(dfind: PFunction<(context: A, dep: Dependency) => Located<A>>, locator: Locator, context: A): List<ToCompile> {
      return compileWorklistKnownModules(dfind, locator, context, mutableStringDictFromMap(new Map<string, Loadable>()))
    }
    // NOTE(Ben): I can't change currentModules to a Map<string, Loadable> yet, because `dfind` might have closed over it
    function compileWorklistKnownModules<A>(dfind: PFunction<(context: A, dep: Dependency) => Located<A>>, locator: Locator, context: A, currentModules: MutableStringDict<Loadable>): List<ToCompile> {
      return runtime.pauseStack((restarter) => {
        const tempMarked = new Map<string, boolean>();
        const topo: Array<{locator: Locator, dependencyMap: Map<string, string>}> = [];
        const currPath: Locator[] = [];
        const PROCESSING = true;
        const FINISHED = false;
        function visit(locator: Locator, context: A): void {
          runtime['RUNGAS'] = 500000000; // HACK! to make sure that compilation itself doesn't become asynchronous due to fuel-exhaustion
          const uri = callMethod(locator, "uri");
          const markStatus = tempMarked.get(uri);
          if (markStatus === PROCESSING) {
            // NOTE(joe/ben): should be restarter.error
            throw new InternalCompilerError(`Detected module cycle: ${currPath.map((l) => callMethod(l, "uri")).join(" => ")}`);
          } else if (markStatus === FINISHED) {
            return;
          } else {
            tempMarked.set(uri, PROCESSING); // mark current locator temporarily
            const pMap = new Map<string, string>();
            return runtime.safeCall(() => callMethod(locator, 'get-dependencies'),
              (rawDeps) => {
                const deps = listToArray(rawDeps);
                const foundMods: Located<A>[] = [];
                return runtime.safeCall(() => 
                  runtime.raw_array_map(runtime.makeFunction((d: Dependency) => {
                    switch(d.$name) {
                      case 'dependency': {
                        return runtime.safeCall(() => dfind.app(context, d),
                        (found) => {
                          pMap.set(callMethod(d, "key"), callMethod(found.dict.locator, "uri"));
                          foundMods.push(found);
                        });
                      }
                      case 'builtin': {
                        const builtinMod = callMethod(currentModules, "get-now", `builtin://${d.dict.modname}`);
                        switch(builtinMod.$name) {
                          case 'none': {
                            return runtime.safeCall(() => dfind.app(context, d),
                            (found) => {
                              pMap.set(callMethod(d, "key"), callMethod(found.dict.locator, "uri"));
                              foundMods.push(found);
                            });
                          }
                          case 'some': {
                            pMap.set(callMethod(d, "key"), builtinMod.dict.value.dict.provides.dict['from-uri']);
                            return;
                          }
                          default: throw new ExhaustiveSwitchError(builtinMod);
                        }
                      }
                      default: throw new ExhaustiveSwitchError(d);
                    }
                  }), deps),
                  () => {
                    // visit all dependents
                    return runtime.safeCall(() => {
                      return runtime.raw_array_map(runtime.makeFunction((f: Located<A>) => {
                        if (!callMethod(currentModules, "has-key-now", callMethod(f.dict.locator, "uri"))) {
                          currPath.push(f.dict.locator);
                          return runtime.safeCall(() =>  visit(f.dict.locator, f.dict.context),
                          () => currPath.pop());
                        }
                      }), foundMods);
                    }, (_) => {
                      // add current locator to *tail* of topo sort
                      topo.push({locator, dependencyMap: pMap});
                      // mark current locator permanently
                      tempMarked.set(uri, FINISHED);
                    },
                    "visiting foundMods");
                  },
                  "mapping over deps"
                );
              }, "getting dependencies");
          }
        }
        currPath.push(locator);
        runtime.runThunk(() => visit(locator, context), 
        (result) => {
          if(runtime.isFailureResult(result)) {
            restarter.error(result.exn);
          }
          else {
            // NOTE(joe): assume we have had all the right effects, so as long as there
            // isn't a failure, we can return the result of all of these updates.
            // since our dependencies were added at the tail of the list, our topo sort is in the correct order
            restarter.resume(runtime.ffi.makeList(topo.map((t) => runtime.makeObject({locator: t.locator, 'dependency-map': mutableStringDictFromMap(t.dependencyMap)}))));
          }
        });
      });
    }

    function modulesFromWorklistKnownModules(worklist: List<ToCompile>, modules: MutableStringDict<Loadable>, maxDepTimes: StringDict<number>, getLoadable: PFunction<(toCompile: ToCompile, maxDepTimes: StringDict<number>) => Option<Loadable>>): MutableStringDict<Loadable> {
      return runtime.pauseStack((restarter) => {
        const wl = listToArray(worklist);
        runtime.runThunk(() => runtime.raw_array_map(runtime.makeFunction((t: ToCompile) => getLoadable.app(t, maxDepTimes)), wl),
        (maybeModulesResult) => {
          runtime['RUNGAS'] = 500000000; // HACK! to make sure that compilation itself doesn't become asynchronous due to fuel-exhaustion
          if (runtime.isFailureResult(maybeModulesResult)) { return restarter.error(maybeModulesResult.exn); }
          const maybeModules = (maybeModulesResult as SuccessResult<Option<Loadable>[]>).result;
          for (let i = 0; i < wl.length; i++) {
            const m = maybeModules[i];
            switch(m.$name) {
              case 'none': break;
              case 'some': callMethod(modules, 'set-now', callMethod(wl[i].dict.locator, 'uri'), m.dict.value); break;
              default: throw new ExhaustiveSwitchError(m);
            }
          }
          restarter.resume(modules);
        });
      });
    }

    function depTimesFromWorklist(worklist: List<ToCompile>, baseTime: number): StringDict<number> {
      // NOTE(joe): base-time is usually the time the *compiler* was last edited.
      // Other clients might have another “min” time after which they want to
      // make sure all modules are recompiled.
      const wl = listToArray(worklist);
      return runtime.pauseStack((restarter) => {
        runtime.runThunk(() => runtime.raw_array_map(runtime.makeFunction((l: ToCompile) => callMethod(l.dict.locator, 'get-modified-time')), wl),
        (modTimesResult) => { // Note(Ben): reorganizing the loop to do all the safeCalling in advance
          // so modTimes[i] = wl[i].locator.get-modified-time()
          runtime['RUNGAS'] = 500000000; // HACK! to make sure that compilation itself doesn't become asynchronous due to fuel-exhaustion
          if (runtime.isFailureResult(modTimesResult)) { return restarter.error(modTimesResult.exn); }
          const modTimes = (modTimesResult as SuccessResult<number[]>).result;
          const ret = new Map<string, number>();
          for (let i = 0; i < wl.length; i++) {
            const located = wl[i];
            const modTime = modTimes[i];
            const curModTime = Math.max(baseTime, modTime);
            let maxDepTime = curModTime;
            const dm = mapFromMutableStringDict(located.dict['dependency-map']);
            for (let depLoc of dm.values()) {
              if (ret.has(depLoc)) {
                maxDepTime = Math.max(maxDepTime, ret.get(depLoc)!);
              }
            }
            ret.set(callMethod(located.dict.locator, 'uri'), maxDepTime);
          }
          restarter.resume(stringDictFromMap(ret));
        })
      });
    }

    function compileProgramWith(worklist : List<ToCompile>, modules: MutableStringDict<Loadable>, options: CompileOptions): CompiledProgram {
      const wl = listToArray(worklist);
      
      return runtime.pauseStack((restarter) => {
        runtime.runThunk(() => runtime.raw_array_map(runtime.makeFunction((w: ToCompile) => {
          runtime['RUNGAS'] = 500000000; // HACK! to make sure that compilation itself doesn't become asynchronous due to fuel-exhaustion
          const uri = callMethod(w.dict.locator, 'uri');
          if (callMethod(modules, 'has-key-now', uri)) {
            return callMethod(modules, 'get-value-now', uri);
          } else {
            const provideMap = callMethod(w.dict['dependency-map'], 'freeze');
            return runtime.safeCall(() => callMethod(options, 'before-compile', w.dict.locator),
            () => {
              return runtime.safeCall(() => compileModule(w.dict.locator, provideMap, modules, options),
              (compiledModule) => {
                const [loadable, trace]= compiledModule.vals;
                // I feel like here we want to generate two copies of the loadable:
                // - One local for calling on-compile with and serializing
                // - One canonicalized for the local cache
                callMethod(modules, 'set-now', uri, loadable);
                const { "compile-env": compileEnv, "post-compile-env": postCompileEnv, provides, "result-printer": result } = loadable.dict;
                const localLoadable = CS['module-as-string'].app(
                  AU['localize-provides'].app(provides, compileEnv),
                  compileEnv, postCompileEnv, result);
                return callMethod(options, 'on-compile', w.dict.locator, localLoadable, trace);
              })
            }, 'before-compile for locator');
          }
        }), wl),
        (loadablesResult) => {
          if (runtime.isFailureResult(loadablesResult)) { return restarter.error(loadablesResult.exn); }
          const loadables = (loadablesResult as SuccessResult<Loadable[]>).result;
          restarter.resume(runtime.makeObject({ loadables: runtime.ffi.makeList(loadables), modules }));
        })
      });
    }

    function compileProgram(worklist: List<ToCompile>, options: CompileOptions): CompiledProgram {
      return compileProgramWith(worklist, mutableStringDictFromMap(new Map()), options);
    }

    /**
     * Invariant: `provideMap` maps dependency keys to URIs that must *all* be keys in `modules`
     * 
     * @param locator 
     * @param provideMap 
     * @param modules 
     * @param options 
     */
    function compileModule(locator: Locator, provideMap: StringDict<string>, modules: MutableStringDict<Loadable>, options: CompileOptions): PTuple<[Loadable, List<any>]> {
      G.reset.app();
      A['global-names'].dict.reset.app();
      
      const env = CS['compile-env'].app(callMethod(locator, 'get-globals'), modules, provideMap);
      return runtime.pauseStack((restarter) => {
        runtime.runThunk(() => callMethod(locator, 'get-compiled', options),
        (todoResult) => {
          runtime['RUNGAS'] = 500000000; // HACK! to make sure that compilation itself doesn't become asynchronous due to fuel-exhaustion
          if (runtime.isFailureResult(todoResult)) { return restarter.error(todoResult.exn); }
          const todo = (todoResult as SuccessResult<CompileTODO>).result;
          switch(todo.$name) {
            case 'already-done': {
              const { provides, "compile-env": ceUnused, "post-compile-env": postEnv, "result-printer": resultPrinter } = todo.dict.result.dict;
              return restarter.resume(runtime.makeTuple([
                CS['module-as-string'].app(AU['canonicalize-provides'].app(provides, env), ceUnused, postEnv, resultPrinter),
                runtime.ffi.makeList([])
              ]));
            }
            case 'arr-js-file': {
              const { provides, "header-file": headerFile, "code-file": codeFile } = todo.dict;
              return restarter.resume(runtime.makeTuple([
                CS['module-as-string'].app(provides, CS['no-builtins'], CS['computed-none'], 
                  CS.ok.app(JSP['ccp-two-files'].app(headerFile, codeFile))),
                runtime.ffi.makeList([])
              ]));
            }
            case 'arr-file': {
              return restarter.resume(compileArrFile(todo, locator, provideMap, modules, env, options));
            }
            default: throw new ExhaustiveSwitchError(todo);
          }
        });
      });
    }

    type PhaseInfo = {
      name: string,
      result: any,
      time: number,
    }
    function phasesToList(phases: PhaseInfo[]): List<{dict: PhaseInfo}> {
      const ret: {dict: PhaseInfo}[] = [];
      for (let i = 1; i < phases.length; i++) {
        const { name, result, time } = phases[i];
        ret.push(runtime.makeObject({
          name,
          result,
          time: time - phases[i-1].time,
        }));
      }
      return runtime.ffi.makeList(ret);
    }
    function dummyProvides(uri: string) {
      const mtSd = stringDictFromMap(new Map());
      return CS.provides.app(uri, mtSd, mtSd, mtSd, mtSd);
    }
    function unique(errs: TCS.CompileError[]): TCS.CompileError[] {
      return errs; // TODO: Figure out how to remove duplicates?
    }

    function compileArrFile(todo: TJ.Variant<CompileTODO, 'arr-file'>, locator: Locator, provideMap: StringDict<string>, modules: MutableStringDict<Loadable>, env: TCS.CompileEnvironment, options: CompileOptions): PTuple<[Loadable, List<any>]> {
      const phases: PhaseInfo[] = [];
      const uri = callMethod(locator, 'uri');
      function addPhase<A>(name: string, result: A): A {
        if (options.dict['collect-all']) {
          phases.push({name, result, time: Date.now() });
        } else if (options.dict['collect-times']) {
          phases.push({name, result: runtime.nothing, time: Date.now() });
        }
        return result;
      }
      let ast: A.Program | undefined = getAst(todo.dict.mod, callMethod(locator, 'uri'));
      phases.push({name: "start", result: null, time: Date.now()});
      let astEnded : A.Program | undefined = AU['append-nothing-if-necessary'].app(ast);
      ast = undefined;
      addPhase("Added nothing", astEnded);
      let wf : CompileResult<A.Program> | undefined = W['check-well-formed'].app(astEnded, options);
      astEnded = undefined;
      addPhase("Checked well-formedness", wf);
      switch(wf.$name) {
        case 'ok': {
          let wfAst : A.Program | undefined = AU['wrap-toplevels'].app(wf.dict.code);
          wf = undefined;
          // NOTE(Joe, #anchor): no desugaring of check blocks happens here
          let imported : A.Program | undefined = AU['wrap-extra-imports'].app(wfAst, todo.dict.libs);
          wfAst = undefined;
          addPhase("Added imports", imported);
          let scoped : TCS.ScopeResolution | undefined = RS['desugar-scope'].app(imported, env, options);
          imported = undefined;
          addPhase("Desugared scope", scoped);
          let namedResult = RS['resolve-names'].app(scoped.dict.ast, uri, env);
          let anyErrors = [...listToArray(scoped.dict.errors), ...listToArray(namedResult.dict.errors)];
          scoped = undefined;
          if (anyErrors.length > 0) {
            addPhase("Result", namedResult.dict.ast);
            return runtime.makeTuple([
              CS['module-as-string'].app(dummyProvides(uri), env, CS['computed-none'], CS.err.app(runtime.ffi.makeList(unique(anyErrors)))),
              phasesToList(phases)
            ]);
          } else {
            addPhase("Resolved named", namedResult);
            let spied = namedResult.dict.ast;
            if (!options.dict['enable-spies']) {
              spied = map<A.Program | A.Expr, A.Program>({
                's-block': (visitor, block: TJ.Variant<A.Expr, 's-block'>) => {
                  const stmts = listToArray(block.dict.stmts);
                  const noSpies: A.Expr[] = [];
                  for (let stmt of stmts) {
                    if (stmt.$name !== "s-spy-block") {
                      noSpies.push(stmt);
                    }
                  }
                  return A['s-block'].app(block.dict.l, runtime.ffi.makeList(noSpies));
                }
              }, spied);
            }
            var provides = dummyProvides(uri);
            // Once name resolution has happened, any newly-created s-binds must be added to bindings...
            // var desugared = named-result.ast

            // NOTE(joe, anchor): removed this to see what un-desugared output looks like
            // and changed desugared.ast to desugared below
            var desugared : D.DesugarInfo | undefined = D.desugar.app(namedResult.dict.ast, options);
            callMethod(namedResult.dict.env.dict.bindings, 'merge-now', desugared.dict['new-binds']);

            // ...in order to be checked for bad assignments here
            anyErrors = listToArray(RS['check-unbound-ids-bad-assignments'].app(desugared.dict.ast, namedResult, env));
            addPhase("Fully desugared", desugared);
            let typeChecked: TCS.CompileResult<A.Program>;
            if (anyErrors.length > 0) {
              typeChecked = CS.err.app(runtime.ffi.makeList(unique(anyErrors)));
            } else if (options.dict['type-check']) {
              const typeCheckResults = TC['type-check'].app(desugared.dict.ast, env, namedResult.dict.env, modules, options);
              if (typeCheckResults.$name === 'ok') {
                provides = AU['get-typed-provides'].app(namedResult, typeCheckResults.dict.code, uri, env);
                typeChecked = CS.ok.app(desugared.dict.ast);
              } else {
                typeChecked = typeCheckResults;
              }
            } else {
              typeChecked = CS.ok.app(desugared.dict.ast);
            }
            desugared = undefined;
            addPhase("Type Checked", typeChecked);
            switch(typeChecked.$name) {
              case 'ok': {
                let tcAst : A.Program | undefined = typeChecked.dict.code;
                let dpAst : A.Program | undefined = DP['desugar-post-tc'].app(tcAst, env, options);
                tcAst = undefined;
                let cleaned : A.Program | undefined = dpAst;
                dpAst = undefined;
                cleaned = AU['set-safe-letrec-binds'].app(cleaned);
                cleaned = AU['inline-lams'].app(cleaned);
                cleaned = AU['set-recursive'].app(cleaned);
                cleaned = AU['set-tail-position'].app(cleaned);
                if (!options.dict['user-annotations']) {
                  cleaned = AU['strip-annotations'].app(cleaned);
                }
                addPhase("Cleaned AST", cleaned);
                if (!options.dict['type-check']) {
                  provides = AU['get-named-provides'].app(namedResult, uri, env);
                }
                let [finalProvides, cr] = JSP['make-compiled-pyret'].app(cleaned, uri, env, namedResult.dict.env, provides, options).vals
                cleaned = undefined;
                let canonicalProvides = AU['canonicalize-provides'].app(finalProvides, env);
                let modResult = CS['module-as-string'].app(canonicalProvides, env, namedResult.dict.env, cr);
                addPhase("Final output", cr);
                return runtime.makeTuple([modResult, phasesToList(phases)]);
              }
              case 'err': {
                addPhase("Result", typeChecked);
                return runtime.makeTuple([
                  CS['module-as-string'].app(dummyProvides(uri), env, CS['computed-none'], typeChecked),
                  phasesToList(phases)
                ]);
              }
              default: throw new ExhaustiveSwitchError(typeChecked);
            }
          }
        }
        case 'err': {
          addPhase("Result", wf);
          return runtime.makeTuple([
            CS['module-as-string'].app(dummyProvides(uri), env, CS['computed-none'], wf),
            phasesToList(phases)
          ]);
        }
        default: throw new ExhaustiveSwitchError(wf);
      }
    }

    function makeStandalone(worklist: List<ToCompile>, compiled: CompiledProgram, options): Either<List<any>, {dict: {'js-ast': J.ObjectExpression, natives: string[]}}> {
      const wl = listToArray(worklist);
      const natives = wl.flatMap((item) => {
        const natives = listToArray(callMethod(item.dict.locator, 'get-native-modules'));
        return natives.map((native) => native.dict.path);
      });
      const allCompileProblems : TCS.CompileError[] = [];
      const staticModules = ObjectExpression(
        wl.map((w) => {
          const uri = callMethod(w.dict.locator, 'uri');
          const loadable = compiled.dict.modules.$underlyingDict[uri];
          switch(loadable.$name) {
            case 'module-as-string': {
              const rp = loadable.dict['result-printer'];
              switch(rp.$name) {
                case 'ok': {
                  return Property(callMethod(w.dict.locator, 'uri'),
                    Literal(JSP['pyret-to-js-runnable'].app(rp.dict.code)));
                }
                case 'err': {
                  allCompileProblems.push(...listToArray(rp.dict.problems));
                  return Property(callMethod(w.dict.locator, 'uri'), Literal("error"));
                }
                default:
                  throw new ExhaustiveSwitchError(rp);
              }
            }
            default:
              throw new ExhaustiveSwitchError(loadable.$name);
          }
        })
      );
      const runtimeOptions = ObjectExpression([
        Property("checks", Literal(options.dict['checks'])),
        Property("disableAnnotationChecks", Literal(options.dict['runtime-annotations']))
      ]);
      if (allCompileProblems.length > 0) {
        return E.left.app(runtime.ffi.makeList(allCompileProblems));
      } else {
        const depMap = ObjectExpression(wl.map((w) => {
          const deps = w.dict['dependency-map'];
          return Property(callMethod(w.dict.locator, 'uri'),
            ObjectExpression(Object.keys(deps.$underlyingDict).map((k) => {
              return Property(k, Literal(deps.$underlyingDict[k]));
            })));
        }));

        const toLoad = ArrayExpression(wl.map((w) => Literal(callMethod(w.dict.locator, 'uri'))));

        const uris = ObjectExpression(wl.map((w) => {
          const uri = callMethod(w.dict.locator, 'uri');
          const hashed = SHA.sha256.app(uri);
          return Property(hashed, Literal(uri));
        }));

        const programAsJS = ObjectExpression([
          Property("staticModules", staticModules),
          Property("depMap", depMap),
          Property("toLoad", toLoad),
          Property("uris", uris),
          Property("runtime-options", runtimeOptions),
        ]);

        return E.right.app(runtime.makeObject({
          'js-ast': programAsJS,
          natives: natives,
        }));
      }
    }
    const exports: Exports['dict']['values']['dict'] = {
      'compile-worklist': runtime.makeFunction(compileWorklist),
      'compile-worklist-known-modules': runtime.makeFunction(compileWorklistKnownModules),
      'get-dependencies': runtime.makeFunction((p: PyretCode, uri: string) => runtime.ffi.makeList(getDependencies(p, uri))),
      'get-standard-dependencies': runtime.makeFunction((p: PyretCode, uri: string) => runtime.ffi.makeList(getStandardDependencies(p, uri))),
      'make-standalone': runtime.makeFunction(makeStandalone),
      'modules-from-worklist-known-modules': runtime.makeFunction(modulesFromWorklistKnownModules),
      'dep-times-from-worklist': runtime.makeFunction(depTimesFromWorklist),
      'compile-program-with': runtime.makeFunction(compileProgramWith),
      'compile-program': runtime.makeFunction(compileProgram),
    };
    return runtime.makeModuleReturn(exports, {});
  }
})