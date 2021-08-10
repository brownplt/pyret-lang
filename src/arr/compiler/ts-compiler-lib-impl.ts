import * as J from 'estree';
import type * as A from './ts-ast'
import { NativeModule, Loadable, URI, CompileOptions, Provides, CompileResult, Dependency, ExtraImports, Globals } from './ts-compile-structs';
import { List, PFunction, MutableStringDict, StringDict, Either, PMethod, Runtime, Option } from './ts-impl-types';
import type * as TAU from './ts-ast-util';
import type * as TJ from './ts-codegen-helpers';
import type * as TCSH from './ts-compile-structs-helpers';
import type * as P from './parse-pyret';
import type * as TCS from './ts-compile-structs';

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
  | { $name: "already-done", dict: { 'result': CompileResult<Loadable> } }
  | {
    $name: "arr-js-file",
    dict: { 'provides': unknown, 'header-file': string, 'code-file': string }
  }
  | {
    $name: "arr-file",
    dict: { 'mod': unknown, 'libs': unknown, 'options': unknown }
  }

export type Located<a> = 
  | { $name: "located", dict: { 'locator': Locator, 'context': a } }


type Locator = {
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
    'make-standalone': PFunction<(worklist: List<ToCompile>, compiled: CompiledProgram, options) => 
    Either<List<any>, {dict: {'js-ast': J.ObjectExpression, natives: string[]}}>>
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

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['parse-pyret']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-compile-structs-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast-util.arr'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr'] },
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
    },
  },
  theModule: function(runtime: Runtime, _, __, P: P.Exports, tj : TJ.Exports, TCSH: TCSH.Exports, AUin: TAU.Exports, CSin: TCS.Exports, E: EitherExports, SHA: ShaExports) {
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
    } = tj;
    const { callMethod } = TCSH;
    const AU = AUin.dict.values.dict;
    const CS = CSin.dict.values.dict;

    function getAst(p : PyretCode, uri: string): A.Program {
      switch(p.$name) {
        case 'pyret-string': return P.dict.values.dict['surface-parse'].app(p.dict.s, uri);
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
      const ret: Dependency[] = [];
      for (let i of listToArray(parsed.dict.imports)) {
        const it = getImportType(i);
        if (it) { ret.push(AU['import-to-dep'].app(it)); }
      }
      return ret;
    }

    function getStandardDependencies(p: PyretCode, uri: string): Dependency[] {
      const ret = getDependencies(p, uri);
      // NOTE that CS.minimal-imports is actually empty
      return ret;
    }


    function compileWorklist<A>(dfind: PFunction<(context: A, dep: Dependency) => Located<A>>, locator: Locator, context: A): List<ToCompile> {
      return compileWorklistKnownModules(dfind, locator, context, mutableStringDictFromMap(new Map<string, Loadable>()))
    }
    function compileWorklistKnownModules<A>(dfind: PFunction<(context: A, dep: Dependency) => Located<A>>, locator: Locator, context: A, currentModules: MutableStringDict<Loadable>): List<ToCompile> {
      const tempMarked = new Map<string, boolean>();
      const topo: Array<{locator: Locator, 'dependency-map': MutableStringDict<string>}> = [];
      const currPath: Locator[] = [];
      function visit(locator: Locator, context: A) {
        const uri = callMethod(locator, "uri");
        if (tempMarked.get(uri)) {
          throw new InternalCompilerError(`Detected module cycle: ${currPath.map((l) => callMethod(l, "uri")).join(" => ")}`);
        }
        tempMarked.set(uri, true); // mark current locator temporarily
        const pMap = new Map<string, string>();
        const deps = listToArray(callMethod(locator, 'get-dependencies'));
        const foundMods: Located<A>[] = [];
        for (let d of deps) {
          switch(d.$name) {
            case 'dependency': {
              const found = dfind.app(context, d);
              pMap.set(callMethod(d, "key"), callMethod(found.dict.locator, "uri"));
              foundMods.push(found);
              break;
            }
            case 'builtin': {
              const builtinMod = callMethod(currentModules, "get-now", `builtin://${d.dict.modname}`);
              switch(builtinMod.$name) {
                case 'none': {
                  const found = dfind.app(context, d);
                  pMap.set(callMethod(d, "key"), callMethod(found.dict.locator, "uri"));
                  foundMods.push(found);
                  break;
                }
                case 'some': {
                  pMap.set(callMethod(d, "key"), builtinMod.dict.value.dict.provides.dict['from-uri']);
                  break;
                }
                default: throw new ExhaustiveSwitchError(builtinMod);
              }
              break;
            }
            default: throw new ExhaustiveSwitchError(d);
          }
        }
        // visit all dependents
        for (let f of foundMods) {
          if (!callMethod(currentModules, "has-key-now", callMethod(f.dict.locator, "uri"))) {
            currPath.push(f.dict.locator);
            visit(f.dict.locator, f.dict.context);
            currPath.pop();
          }
        }
        // add current locator to *tail* of topo sort
        topo.push({locator, 'dependency-map': mutableStringDictFromMap(pMap)});
        // mark current locator permanently
        tempMarked.set(uri, false);
      }
      currPath.push(locator);
      visit(locator, context);
      // since our dependencies were added at the tail of the list, our topo sort is in the correct order
      return runtime.ffi.makeList(topo.map(runtime.makeObject));
    }

    function makeStandalone(worklist: List<ToCompile>, compiled: CompiledProgram, options): Either<List<any>, {dict: {'js-ast': J.ObjectExpression, natives: string[]}}> {
      const wl = listToArray(worklist);
      const natives = wl.flatMap((item) => {
        const natives = listToArray(callMethod(item.dict.locator, 'get-native-modules'));
        return natives.map((native) => native.dict.path);
      });
      const allCompileProblems = [];
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
                    Literal(callMethod(rp.dict.code, 'pyret-to-js-runnable')));
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
        return E.dict.values.dict.left.app(runtime.ffi.makeList(allCompileProblems));
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
          const hashed = SHA.dict.values.dict.sha256.app(uri);
          return Property(hashed, Literal(uri));
        }));

        const programAsJS = ObjectExpression([
          Property("staticModules", staticModules),
          Property("depMap", depMap),
          Property("toLoad", toLoad),
          Property("uris", uris),
          Property("runtime-options", runtimeOptions),
        ]);

        return E.dict.values.dict.right.app(runtime.makeObject({
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
    };
    return runtime.makeModuleReturn(exports, {});
  }
})