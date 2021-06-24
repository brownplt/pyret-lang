import * as J from 'estree'
import type * as A from './ts-ast'
import { NativeModule, Loadable, URI, CompileOptions, Provides, CompileResult, Dependency, ExtraImports, Globals } from './ts-compile-structs';
import { List, PFunction, MutableStringDict, StringDict, Either, PMethod } from './ts-impl-types';
import type * as TJ from './ts-codegen-helpers';
import type * as TCSH from './ts-compile-structs-helpers';

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
    'get-compiled': PMethod<Locator, () => CompileTODO>,
  }
}

type ToCompile = {
  dict: {
    locator: Locator,
    'dependency-map': MutableStringDict<Locator>,
  }
}

type CompiledProgram = {
  dict: {
    loadables : List<Loadable>, 
    modules : MutableStringDict<Loadable>
  }
}


export interface Exports {

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
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-compile-structs-helpers']},
    { 'import-type': 'builtin', name: 'either' },
    { 'import-type': 'builtin', name: 'sha' },
  ],
  nativeRequires: [],
  provides: {
    values: {
      'make-standalone': "tany",
    },
  },
  theModule: function(runtime, _, __, tj : TJ.Exports, TCSH: TCSH.Exports, E: EitherExports, SHA: ShaExports) {
    const { 
      ExhaustiveSwitchError,
      ArrayExpression,
      Literal,
      listToArray,
      ObjectExpression,
      Property,
    } = tj;
    const { callMethod } = TCSH.dict.values.dict;
    function makeStandalone(worklist: List<ToCompile>, compiled: CompiledProgram, options) {
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
    return runtime.makeModuleReturn({
      'make-standalone': runtime.makeFunction(makeStandalone),
    }, {});
  }
})