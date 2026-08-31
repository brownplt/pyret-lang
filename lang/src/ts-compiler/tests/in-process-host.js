// In-process load-lib host for the TS compiler, for node tests.
//
// cli-module-loader.ts deliberately runs compiled programs in a CHILD PROCESS
// ("in-process load-lib execution is not ported"). But CPO runs them in-process
// in the browser via load-lib's run-program against a persistent, forking realm
// — and that machinery is plain JS that works in node too. This harness does
// exactly that, so repl.ts can be tested end-to-end (real
// compilation AND real execution) in lang/, without CPO.
//
// It bootstraps a Pyret runtime + the runtime-lib/load-lib troves (via the
// runtime's own runStandalone, the same path handalone.js uses), then exposes a
// ReplExecutor: run(realm, programJsSource, options) -> ModuleResult, mirroring
// code.pyret.org/src/web/js/ts-compiler-lib.js makeExecutor.

const path = require('path');
const fs = require('fs');

const LANG_ROOT = path.resolve(__dirname, '..', '..', '..');
const TS_OUT = path.join(LANG_ROOT, 'build', 'ts-compiler');

// --- requirejs configured for the in-process runtime natives ----------------
const requirejs = require('requirejs');
requirejs.config({
  baseUrl: TS_OUT,
  paths: {
    'pyret-base': './',
    'jglr': path.join(LANG_ROOT, 'lib', 'jglr'),
  },
  nodeRequire: require, // util / seedrandom / js-sha256 fall back to node
});

// --- TS compiler pieces (loaded from the build, like repl-test.js) ----------
function loadTs(mod) { return require(path.join(TS_OUT, mod)); }
const CS = loadTs('compile-structs.js');
const CL = loadTs('compile-lib.js');
const CML = loadTs('cli-module-loader.js');
const BL = loadTs('locators/builtin.js');

// Build a standalone program SOURCE string for a Pyret source, using the same
// compile path repl.ts uses internally (worklist -> compileProgramWith ->
// makeStandalone -> jsAst.toUglySource()).
// Async because this branch's compileWorklistKnownModules does its module I/O
// up front and returns a Promise<ToCompile[]> (the sync-backend split); the
// compileProgramWith/makeStandalone backend stays synchronous.
async function makeStandaloneSource(src, uri, context, options) {
  const finder = (ctx, dep) => CML.moduleFinder(ctx, dep);
  const loc = CL.stringLocator(uri, src);
  const ws = await CL.compileWorklistKnownModules(finder, loc, context, new Map());
  const compiled = CL.compileProgramWith(ws, new Map(), options);
  const errs = compiled.loadables.filter(CL.isErrorCompilation);
  if (errs.length > 0) {
    throw new Error('bootstrap compile failed: ' +
      JSON.stringify(errs.map((e) => e.resultPrinter && e.resultPrinter.$name)));
  }
  const standalone = CL.makeStandalone(ws, compiled, options);
  if (standalone.$name === 'left') {
    throw new Error('bootstrap makeStandalone failed');
  }
  return standalone.v.jsAst.toUglySource();
}

// Returns a Promise of an in-process host:
//   { executor, realm, modules, makeFinder, context, runtime, loadLibMod }
// ready to plug into repl.makeRepl.
function makeHost(opts) {
  opts = opts || {};
  const cacheBaseDir = opts.cacheBaseDir || 'tests/ts-compiled';
  const quiet = opts.quiet !== false;

  BL.setBuiltinJsDirs(['src/js/trove/']);
  BL.setBuiltinArrDirs(['src/arr/trove/']);

  const context = {
    currentLoadPath: LANG_ROOT,
    cacheBaseDir,
    compiledReadOnlyDirs: [],
    urlFileMode: CS.allRemote,
  };
  const bootstrapOptions = {
    ...CS.defaultCompileOptions,
    checks: 'none',
    displayProgress: false,
  };

  // Load the runtime natives via requirejs and FULLY SETTLE that callback
  // before doing any async compilation. compileWorklistKnownModules (this
  // branch's async worklist) itself loads modules through requirejs/amd, and
  // awaiting a nested requirejs load from inside a still-pending requirejs
  // callback deadlocks — so the compile is hoisted out to the top level here.
  function loadRuntimeLibs() {
    return new Promise((resolve, reject) => {
      requirejs(
        ['pyret-base/js/runtime', 'pyret-base/js/post-load-hooks', 'pyret-base/js/exn-stack-parser'],
        (runtimeLib, loadHooksLib, _stackLib) => resolve({ runtimeLib, loadHooksLib }),
        reject);
    });
  }

  async function boot() {
    const { runtimeLib, loadHooksLib } = await loadRuntimeLibs();

    const runtime = runtimeLib.makeRuntime({
      stdout: (s) => { if (!quiet) process.stdout.write(s); },
      stderr: (s) => { if (!quiet) process.stderr.write(s); },
      stdin: process.stdin,
    });

    // Bootstrap: instantiate runtime-lib + load-lib into the host runtime
    // by running a tiny program that imports them (same as handalone).
    const bootSrc = 'import load-lib as L\nimport runtime-lib as R\n';
    const bootStandalone = await makeStandaloneSource(
      bootSrc, 'bootstrap://', context, bootstrapOptions);
    const program = eval('(' + bootStandalone + ')'); // eslint-disable-line no-eval

    const realmObj = { instantiated: {}, static: {} };
    const main = program.toLoad[program.toLoad.length - 1];
    runtime.setParam('command-line-arguments', []);
    const postLoadHooks = loadHooksLib.makeDefaultPostLoadHooks(
      runtime, { main, checks: 'none' });

    // Compiled modules look each other up via thisRuntime.modules[uri],
    // but runStandalone stores into realm.instantiated[uri]; alias them
    // (handalone.js:196 and load-lib runProgram do the same).
    runtime.modules = realmObj.instantiated;
    await new Promise((resolve, reject) => {
      runtime.runThunk(
        () => runtime.runStandalone(
          program.staticModules, realmObj, program.depMap, program.toLoad, postLoadHooks),
        (bootResult) => {
          if (!runtime.isSuccessResult(bootResult)) {
            console.error(bootResult.exn);
            return reject(new Error('bootstrap runStandalone failed'));
          }
          resolve();
        });
    });
    return buildHost(runtime);
  }

  return boot();

    function buildHost(runtime) {
      const gf = (o, f) => runtime.getField(o, f);
      const ppt = (m) => gf(m, 'provide-plus-types');
      const internalOf = (m) => gf(ppt(m), 'internal');
      const valueOf = (m, f) => gf(gf(ppt(m), 'values'), f);

      const loadLibMod = runtime.modules['builtin://load-lib'];
      const runtimeLibMod = runtime.modules['builtin://runtime-lib'];
      if (!loadLibMod || !runtimeLibMod) {
        throw new Error('bootstrap did not instantiate load-lib/runtime-lib');
      }

      // Wrap the host runtime as a Pyret Runtime value, and make a fresh realm.
      const brandRuntime = internalOf(runtimeLibMod).brandRuntime;
      const pyRuntime = gf(brandRuntime, 'brand').app(
        runtime.makeObject({ runtime: runtime.makeOpaque(runtime) }));
      const makeRealm = internalOf(loadLibMod).makeRealm;
      const initialRealm = makeRealm({ instantiated: {}, static: {} });

      const runProgram = valueOf(loadLibMod, 'run-program');

      const executor = {
        run(realm, programJsSource, options) {
          return new Promise((res, rej) => {
            runtime.runThunk(
              () => runProgram.app(
                pyRuntime,
                realm,
                programJsSource,
                runtime.makeObject({ checks: (options && options.checks) || 'main' }),
                runtime.ffi.makeList([])),
              (result) => {
                if (runtime.isSuccessResult(result)) { res(result.result); }
                else { rej(result.exn); } // run-program itself threw (not the user program)
              });
          });
        },
        isSuccessResult(mr) { return mr.val.runtime.isSuccessResult(mr.val.result); },
        getResultRealm(mr) { return mr.val.realm; },
        // Extra helpers for assertions in tests (mirror load-lib's
        // getModuleResultAnswer): the last-expression value of the module.
        getAnswer(mr) { return mr.val.runtime.getField(mr.val.result.result, 'answer'); },
        runtimeOf(mr) { return mr.val.runtime; },
        // Run the checker's results-summary over a successful ModuleResult's
        // check blocks; resolves to { passed, failed, errored, total, message }.
        // With checks off the module still succeeds but has no check blocks, so
        // total === 0 — which is exactly what lets a test tell "checks ran" from
        // "checks were skipped".
        summarizeChecks(mr) {
          const execRt = mr.val.runtime;
          const checkerMod = execRt.modules['builtin://checker'];
          const checkerVals = execRt.getField(
            execRt.getField(checkerMod, 'provide-plus-types'), 'values');
          const resultsSummary = execRt.getField(checkerVals, 'results-summary');
          const checks = execRt.getField(mr.val.result.result, 'checks');
          const emptyStack = execRt.makeFunction(() => execRt.ffi.makeList([]), 'get-stack');
          return new Promise((res, rej) => {
            execRt.runThunk(
              () => resultsSummary.app(checks, emptyStack, execRt.makeString('text')),
              (r) => {
                if (!execRt.isSuccessResult(r)) { return rej(r.exn); }
                const s = r.result;
                const n = (f) => Number(execRt.num_to_string(execRt.getField(s, f)));
                res({
                  passed: n('passed'), failed: n('failed'),
                  errored: n('errored'), total: n('total'),
                  message: execRt.getField(s, 'message'),
                });
              });
          });
        },
      };

      return {
        executor,
        realm: initialRealm,
        modules: new Map(),
        makeFinder: () => (ctx, dep) => CML.moduleFinder(ctx, dep),
        context,
        runtime,
        loadLibMod,
      };
    }
}

module.exports = { makeHost };
