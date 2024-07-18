import type { List, PFunction, Runtime, StringDict, Option } from './ts-impl-types';
import type { CompileOptions } from './ts-compiler-lib-impl';
import type * as TCS from './ts-compile-structs';
import type * as TJ from './ts-codegen-helpers';
import type * as TCSH from './ts-compile-structs-helpers';
import type * as P from './ts-pathlib';

export type Exports = {
  dict: { 
    values: {
      dict: {
        'populate-options': PFunction<(dictionary: StringDict<any>, thisPyretDir: string) => CompileOptions>
      }
    }
  }
}

type BExports = {
  dict: {
    values: {
      dict: {
        'set-builtin-arr-dirs': PFunction<(dirs: List<string>) => void>,
        'set-allow-builtin-overrides': PFunction<(allow: boolean) => void> 
      }
    }
  }
}

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-compile-structs-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['locators/builtin.arr'] },
    { 'import-type': 'builtin', name: 'pathlib' },
  ],
  nativeRequires: [],
  provides: {
    values: {
      'populate-options': 'tany'
    },
  },
  theModule: function(runtime: Runtime, _, __, 
    tj : TJ.Exports,
    TCSH: TCSH.Exports,
    CSin: TCS.Exports,
    Bin: BExports,
    Pin: P.Exports,
  ) {
    const { 
      ExhaustiveSwitchError,
      InternalCompilerError,
      listToArray,
      mapFromStringDict,
    } = tj;
    const { callMethod } = TCSH;
    const CS = CSin.dict.values.dict;
    const P = Pin.dict.values.dict;
    const B = Bin.dict.values.dict;

    const defaultCompileOptions: CompileOptions['dict'] = {
      'add-profiling': false,
      'allow-shadowed': false,
      'base-dir': ".",
      'before-compile': runtime.makeMethod(((_) => (_) => runtime.nothing), ((_, __) => runtime.nothing)),
      'build-runnable' : "none",
      'builtin-js-dirs' : runtime.ffi.makeList([]),
      'check-all': true,
      'check-mode': true,
      'checks': "all",
      'collect-all': false,
      'collect-times': false,
      'compile-mode' : CS['cm-normal'],
      'compiled-cache': "compiled",
      'compiled-dir': "compiled",
      'compiled-read-only': runtime.ffi.makeList([]),
      'deps-file': "build/bundled-node-deps.js",
      'display-progress': true,
      'enable-spies': true,
      'html-file': runtime.ffi.makeNone(),
      'ignore-unbound': false,
      'inline-case-body-limit': 5,
      'lint': false,
      'log': runtime.makeFunction(function(s: string, toClear: Option<number>) {
        switch (toClear.$name) {
          case "none": {
            runtime.stdout(s);
            break;
          }
          case "some": {
            runtime.stdout("\r");
            runtime.stdout(" ".repeat(toClear.dict.value));
            runtime.stdout("\r");
            runtime.stdout(s);
            break;
          }
        }
      }),
      'log-error': runtime.makeFunction((v) => { runtime.stderr(v); return v; }),
      'module-eval': true,
      'on-compile': runtime.makeMethod(((_) => (_, loadable, __) => loadable), ((_, __, loadable, ___) => loadable)),
      'pipeline': CS['pipeline-anchor'],
      'proper-tail-calls': true,
      'recompile-builtins': true,
      'require-config': "MUST SET THIS VALUE",
      'runtime-annotations': true,
      'runtime-builtin-relative-path': runtime.ffi.makeNone(),
      'runtime-path': "../builtin/src/runtime",
      'session-delete': false,
      'session-filter': runtime.ffi.makeNone(),
      'session': "empty",
      'should-profile': runtime.makeMethod(((_) => (_) => (false as boolean)), ((_ , __) => false)),
      'standalone-file': "src/js/base/handalone.js",
      'this-pyret-dir': ".",
      'type-check': false,
      'user-annotations': true,
    };
    
    function makeDefaultCompileOptions(thisPyretDir: string): CompileOptions['dict'] {
      const copy = Object.assign(Object.create(null), defaultCompileOptions);
      Object.assign(copy, {
        'base-dir': ".",
        'this-pyret-dir': thisPyretDir,
        'deps-file': P.resolve.app(P.join.app(thisPyretDir, "bundled-node-deps.js")),
        'standalone-file': P.resolve.app(P.join.app(thisPyretDir, "js/handalone.js")),
        'builtin-js-dirs': runtime.ffi.makeList([]),
        'compile-mode': CS['cm-normal'],
      });
      return copy;
    }

    function populateOptions(dictionary: StringDict<any>, thisPyretDir: string): CompileOptions {
      const dict = mapFromStringDict(dictionary);
      const compileOpts = makeDefaultCompileOptions(thisPyretDir);

      let checks: string;
      if (dict.has("no-check-mode") || dict.has("library")) {
        checks = "none";
      } 
      else if (dict.has("checks")) {
        checks = dict.get("checks");
      }
      else {
        checks = "all";
      }

      let builtinJsDirs: string[] = listToArray(compileOpts['builtin-js-dirs']);
      if (dict.has("builtin-js-dir")) {
        const dir = dict.get("builtin-js-dir");
        if (typeof(dir) === "string") {
          builtinJsDirs.push(dir);
        }
        else {
          builtinJsDirs.push(...listToArray(dir as List<string>));
        }
      }

      let pipeline : TCS.Pipeline;
      if (dict.has("pipeline")) {
        const s: string = dict.get("pipeline");
        if (s === "anchor") {
          pipeline = CS['pipeline-anchor'];
        }
        else if (s.startsWith("ts-anchor")) {
          pipeline = CS['pipeline-ts-anchor'].app(runtime.ffi.makeList(s.split(":").slice(1)));
        }
        else {
          throw new InternalCompilerError("Unknown pipeline argument: " + s);
        }
      }
      else {
        pipeline = CS['pipeline-anchor'];
      }

      let compileMode: TCS.CompileMode;
      switch (dict.get('compile-mode') ?? "normal") {
        case "normal": {
          compileMode = CS['cm-normal'];
          break;
        }
        case "builtin-stage-1": {
          compileMode = CS['cm-builtin-stage-1'];
          break;
        }
        case "builtin-general": {
          compileMode = CS['cm-builtin-general'];
          break;
        }
        default:
          throw new InternalCompilerError("Unknown compile mode: " + dict.get("compile-mode"));
      }

      compileOpts["add-profiling"] = dict.has("profile");
      compileOpts["allow-shadowed"] = dict.has("allow-shadow");
      compileOpts["base-dir"] = dict.get('base-dir') ?? compileOpts['base-dir'];
      compileOpts["build-runnable"] = dict.get('build-runnable') ?? "none";
      compileOpts["builtin-js-dirs"] = runtime.ffi.makeList(builtinJsDirs);
      compileOpts["check-mode"] = !(dict.get('no-check-mode') ?? false);
      compileOpts["checks"] = checks;
      compileOpts["collect-all"] = dict.get('collect-all') ?? false;
      compileOpts["collect-times"] = dict.get('collect-times') ?? false;
      compileOpts["compile-mode"] = compileMode;
      compileOpts["compiled-cache"] = dict.get('compiled-dir') ?? "compiled";
      compileOpts["compiled-dir"] = dict.get('compiled-dir') ?? "compiled";
      compileOpts["compiled-read-only"] = dict.get('compiled-read-only-dir') ?? runtime.ffi.makeList([]);
      compileOpts["deps-file"] = dict.get('deps-file') ?? compileOpts['deps-file'];
      compileOpts["display-progress"] = !dict.has("no-display-progress");
      compileOpts["enable-spies"] = !dict.has("no-spies");
      compileOpts["html-file"] = makeOpt(dict, "html-file");
      compileOpts["inline-case-body-limit"] = dict.get('inline-case-body-limit') ?? compileOpts['inline-case-body-limit'];
      compileOpts["lint"] = dict.get('lint') ?? false;
      compileOpts["log-error"] = dict.get('log-error') ?? compileOpts['log-error'];
      compileOpts["log"] = dict.get('log') ?? compileOpts.log;
      compileOpts["module-eval"] = !dict.has("no-module-eval");
      compileOpts["pipeline"] = pipeline;
      compileOpts["proper-tail-calls"] = !dict.has("improper-tail-calls");
      compileOpts["recompile-builtins"] = dict.get('recompile-builtins') ?? true;
      compileOpts["require-config"] = dict.get('require-config') ?? P.resolve.app(P.join.app(thisPyretDir, "config.json"));
      compileOpts["runtime-annotations"] = !dict.has("no-runtime-annotations");
      compileOpts["runtime-builtin-relative-path"] = makeOpt(dict, "runtime-builtin-relative-path")
      compileOpts["session-delete"] = dict.has("session-delete");
      compileOpts["session-filter"] = makeOpt(dict, "session-filter");
      compileOpts["session"] = dict.get('session') ?? "empty";
      compileOpts["standalone-file"] = dict.get('standalone-file') ?? compileOpts['standalone-file'];
      compileOpts["type-check"] = dict.get('type-check') ?? false;
      compileOpts["user-annotations"] = !dict.has('no-user-annotations');

      if (dict.has("builtin-arr-dir")) {
        B['set-builtin-arr-dirs'].app(dict.get("builtin-arr-dir"));
      }

      if (dict.has("allow-builtin-overrides")) {
        B['set-allow-builtin-overrides'].app(dict.get("allow-builitn-overrides"));
      }

      return runtime.makeObject(compileOpts);
    }

    function makeOpt(dict: Map<string, any>, key: string): Option<any> {
      if (dict.has(key)) {
        return runtime.ffi.makeSome(dict.get(key));
      } 
      else {
        return runtime.ffi.makeNone();
      } 
    }

    const exports: Exports['dict']['values']['dict'] = {
      'populate-options' : runtime.makeFunction(populateOptions)
    };
    return runtime.makeModuleReturn(exports, {});
  }
})