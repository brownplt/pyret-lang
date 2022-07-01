/* eslint-disable */
export interface RuntimeConfig {
  //spyMessgeHandler?: (x: SpyMessageResult) => void,
  spyMessgeHandler?: (x: any) => void,
  //spyExprHandler?: (x: SpyExprResult) => void,
  spyExprHandler?: (x: any) => void,
  imgUrlProxy?: (url: string) => string,
  checkBlockRunner?: (block: any) => void,
  checkBlockFilter?: (srcloc: string, name: string) => boolean,
}

export interface RunnerPerfResults {
  $rootOnly: number,
  $makeRootRequires: number,
  $dependencies: number,
  $total: number,
  [key: string]: number
}

const csv = require('csv-parse/lib/sync');
const assert = require('assert');
const immutable = require('immutable');
const seedrandom = require('seedrandom');
import * as stopify from '@stopify/stopify';
const browserFS = require('./browserfs-setup.ts');

(window as any).stopify = stopify;

const { fs } = browserFS;
const { path } = browserFS;

const nodeModules = {
  assert,
  'csv-parse/lib/sync': csv,
  fs: browserFS.fs,
  immutable,
  seedrandom
};


let timings: RunnerPerfResults = {
  $rootOnly: 0,
  $makeRootRequires: 0,
  $dependencies: 0,
  $total: 0,
};

export function resetTimings() {
  timings = {
    $rootOnly: 0,
    $makeRootRequires: 0,
    $dependencies: 0,
    $total: 0,
  }
}

export function getTimingResults(): RunnerPerfResults {
  return Object.assign({}, timings);
}

function calculateDependencyTime(rootModule: string) {
  let depTotal = 0;
  for (let k in timings) {
    if (!k.includes("$") && k !== rootModule) {
      depTotal += timings[k];
    }
  }
  timings.$dependencies = depTotal;
}

/**
  This wrapping is necessary because otherwise require, exports, and module
  will be interpreted as *global* by stopify. However, these really need to
  be module-local as they have context like the working directory, and
  exports/module are per-module even though they “act” like a global
  variable.

  We still set these fields on runner.g (g is the global field for
  stopify), but immediately apply the function to the values we just set
  before it evaluates and goes on to process any more requires. Stopify
  then won't compile uses of module/exports/require within the generated
  function to use .g, and each module gets its own copy.
*/
function wrapContent(content: string): string {
  return `(function(require, exports, module) { ${content} })(require, exports, module);`;
}

let asyncCache : {[key:string]: any} = {};
let currentRunner: any = null;
currentRunner = stopify.stopifyLocally("", { newMethod: 'direct' });
currentRunner.run(() => { console.log("initialized stopify runner")});

export function runStopify<A>(f : () => A) {
  return new Promise((resolve, reject) => {
    currentRunner.runStopifiedCode(f, (result : any) => {
      if (result.type !== 'normal') {
        console.log('runStopify reject', JSON.stringify(result));
        reject(result);
      } else {
        console.log('runStopify resolve', JSON.stringify(result));
        resolve(result.value);
      }
    });
  });
}

export function getAsyncModuleByName(s : string) {
  if(!(s in asyncCache)) { throw new Error(`Cannot get async module ${s}`); }
  return asyncCache[s];
}

const RUNTIME_PATH = "/compiled/builtin/runtime.js.stopped";
export function getAsyncRuntime() {
  return asyncCache[RUNTIME_PATH];
}

export const makeRequireAsync = (basePath: string, rtCfg?: RuntimeConfig): ((importPath: string) => Promise<any>
  ) => {
  const requireAsyncMain = (importPath: string) => new Promise(((resolve, reject) => {
    const startRootRequires = window.performance.now();
    if (importPath in nodeModules) {
      return (nodeModules as any)[importPath];
    }
    const nextPath = path.join(basePath, importPath);
    const cwd = path.parse(nextPath).dir;
    // This previously named the file .stopped.main, to avoid having it read
    // from cache. However, with REPL-like running we want to cache this program
    // for the next segment! So instead, we delete this particular program from
    // the cache if it already exists, but ultmately store it in the cache in
    // the right place
    const stoppedPath = `${nextPath}.stopped`;
    // Get the absolute path to uniquely identify modules
    // Cache modules based upon the absolute path for singleton modules
    const cachePath = path.resolve(stoppedPath);
    if (cachePath in asyncCache) {
      delete asyncCache[cachePath];
    }
    if (!fs.existsSync(nextPath)) {
      throw new Error(`Path did not exist in requireAsyncMain: ${nextPath}`);
    }

    const contents = String(fs.readFileSync(nextPath));

    const toStopify = wrapContent(contents);
    let toWrite = currentRunner.compile(toStopify);

    if (currentRunner.kind !== 'ok') { reject(currentRunner); }
    fs.writeFileSync(stoppedPath, toWrite);
    const stopifyModuleExports = {
      exports: {
        __pyretExports: nextPath,
      },
    };

    currentRunner.g = Object.assign(currentRunner.g, {
      document,
      Number,
      Math,
      Array,
      Object,
      RegExp,
      stopify,
      Error,
      Image,
      JSON,
      Date,
      decodeURIComponent,
      require: requireAsyncFromDir(cwd),
      module: stopifyModuleExports,
      // TS 'export' syntax desugars to 'exports.name = value;'
      exports: stopifyModuleExports.exports,
      String,
      $STOPIFY: currentRunner,
      setTimeout,
      clearTimeout,
      console,
      parseFloat,
      isNaN,
      isFinite,
      // @ts-ignore
      ide: window.ide,
    });
    currentRunner.path = nextPath;

    resolve({
      run: new Promise((resolve, reject) => {
        const endRootRequires = window.performance.now();
        timings.$makeRootRequires = endRootRequires - startRootRequires;
        const startRootExecution = endRootRequires;
        const cb =  (result : any) => {
          if (result.type !== 'normal') {
            reject(result);
          } else {
            const toReturn = currentRunner.g.module.exports;
            handleRuntimeConfig(cachePath, toReturn, rtCfg);
            asyncCache[cachePath] = toReturn;
            const endRootExecution = window.performance.now();
            timings[cachePath] = endRootExecution - startRootExecution;
            timings.$total = endRootExecution - startRootRequires;
            calculateDependencyTime(cachePath);
            timings.$rootOnly = timings.$total - timings.$dependencies - timings.$makeRootRequires;
            resolve(toReturn);
          }
        };
        currentRunner.runInit(cb);
        currentRunner.onYieldFlag = { kind: 'resume' };
        currentRunner.mayYieldFlag = { kind: 'resume' };
        currentRunner.evalCompiled(toWrite, cb);
      }),
      pause: (callback: (line: number) => void): void => {
        currentRunner.pause(callback);
      },
      resume: (): void => {
        currentRunner.resume();
      },
      onEnd: (result : any): void => {
        currentRunner.onEnd(result);
      },
    });
  }));

  const requireAsyncFromDir = (requiringWd : string) => {
    return (importPath: string) => {
      const startRequire = window.performance.now();
      if (importPath in nodeModules) {
        return (nodeModules as any)[importPath];
      }
      let nextPath = path.join(requiringWd, importPath);
      // NOTE(joe, June 2021):
      // Required, because some TypeScript/es module workflows have you write the module without the .js
      if(nextPath.slice(-3) !== ".js") {
        nextPath = nextPath + ".js";
      }
    
      const cwd = path.parse(nextPath).dir;
      const stoppedPath = `${nextPath}.stopped`;
      // Get the absolute path to uniquely identify modules
      // Cache modules based upon the absolute path for singleton modules
      const cachePath = path.resolve(stoppedPath);
      if (cachePath in asyncCache) { return asyncCache[cachePath]; }
      if (!fs.existsSync(nextPath)) {
        throw new Error(`Path did not exist in requireASync: ${nextPath}`);
      }
      currentRunner.pauseK((kontinue: (result: any) => void) => {
        const lastPath = currentRunner.path;
        const module = {
          exports: {
            __pyretExports: nextPath,
          },
        };
        const lastModule = currentRunner.g.module;
        // Note: It's important that module.exports is an alias of exports, and
        // that both module and exports are available globals. This has to do
        // with differing patterns in how e.g. TypeScript, best-practice JS
        // code, and so on generate export code.
        currentRunner.g.module = module;
        currentRunner.g.exports = module.exports;
        currentRunner.g.require = requireAsyncFromDir(cwd);
        currentRunner.path = nextPath;
        let stopifiedCode = '';
        if (fs.existsSync(stoppedPath) && (fs.statSync(stoppedPath).mtime > fs.statSync(nextPath).mtime)) {
          console.log(`[RUNNER] Found pre-stopified code at: ${nextPath}`);
          stopifiedCode = String(fs.readFileSync(stoppedPath));
        } else {
          console.log(`[RUNNER] Stopifying: ${nextPath}`);
          const contents = String(fs.readFileSync(nextPath));
          stopifiedCode = currentRunner.compile(wrapContent(contents));
          fs.writeFileSync(stoppedPath, stopifiedCode);
        }
        currentRunner.evalCompiled(stopifiedCode, (result: any) => {
          if (result.type !== 'normal') {
            kontinue(result);
            return;
          }
          const toReturn = currentRunner.g.module.exports;
          currentRunner.path = lastPath;
          // g.exports and g.module may be overwritten by JS code. Need to restore
          currentRunner.g.module = lastModule;
          // Need to set 'exports' global to work with TS export desugaring
          currentRunner.g.exports = lastModule.exports;
          handleRuntimeConfig(cachePath, toReturn, rtCfg);
          asyncCache[cachePath] = toReturn;
          const endRequire = window.performance.now();
          timings[cachePath] = endRequire - startRequire;
          kontinue({ type: 'normal', value: toReturn });
        });
      });
    };
  }
  return requireAsyncMain;
};

const syncCache : {[key:string]: any} = {};
export const makeRequire = (basePath: string, rtCfg?: RuntimeConfig): ((importPath: string) => any) => {
  let cwd = basePath;
  let isRoot = true;
  let rootPath: string = "";
  /*
    Recursively eval (with this definition of require in scope) all of the
    described JavaScript.

    Note that since JS code is generated/written with the assumption that
    require() is sync, we can only use sync versions of the FS function here;
    require must be entirely one synchronous run of the code.

    Future use of stopify could enable the definition of requireAsync, which
    could pause the stack while requiring and then resume.
   */
  const startMakeRequire = window.performance.now();
  const requireSync = (importPath: string) => {
    const startRequire = window.performance.now();

    if (importPath in nodeModules) {
      return (nodeModules as any)[importPath];
    }
    const oldWd = cwd;
    let nextPath = path.join(cwd, importPath);
    // NOTE(joe, June 2021):
    // Required, because some TypeScript/es module workflows have you write the module without the .js
    if (nextPath.slice(-3) !== ".js") {
      nextPath = nextPath + ".js";
    }

    if (isRoot) {
      isRoot = false;
      rootPath = nextPath;
    }

    cwd = path.parse(nextPath).dir;
    if (!fs.existsSync(nextPath)) {
      throw new Error(`Path did not exist in requireSync: ${nextPath}`);
    }
    const contents = fs.readFileSync(nextPath);
    // TS 'export' syntax desugars to 'exports.name = value;'
    // Adding an 'exports' parameter simulates the global 'exports' variable
    // Also, the comment below has meaning to eslint and makes it ignore the
    // use of the Function constructor (which we do intend)
    // eslint-disable-next-line
    const f = new Function("require", "module", "exports", contents);
    const module = {
      exports: {
        __pyretExports: nextPath,
      },
    };
    const requireSyncWithCache = (importPath: string) => {
      let nextPath = path.join(cwd, importPath);
      if (nextPath in syncCache) { return syncCache[nextPath]; }
      return requireSync(importPath);
    }
    const result = f(requireSyncWithCache, module, module.exports);
    const toReturn = module.exports ? module.exports : result;
    handleRuntimeConfig(nextPath, toReturn, rtCfg);
    cwd = oldWd;
    syncCache[nextPath] = toReturn;

    const endRequire = window.performance.now();
    timings[nextPath] = endRequire - startRequire;
    if (nextPath === rootPath) {
      calculateDependencyTime(rootPath);
      timings.$total = timings.$dependencies + (endRequire - startRequire) + timings.$makeRootRequires;
      timings.$rootOnly = timings.$total - timings.$dependencies - timings.$makeRootRequires;
    }
    return toReturn;
  };

  const endMakeRequire = window.performance.now();
  timings.$makeRootRequires = endMakeRequire - startMakeRequire;
  return requireSync;
};

function handleRuntimeConfig(currentPath: string, evaldModule: object, rtCfg?: RuntimeConfig) {
  // NOTE(alex): May need to find a better way to detect the runtime file eval
  if (!currentPath.includes("builtin/runtime.js")) {
    return;
  }

  if (rtCfg === undefined) {
    console.log('RUNNER: config undefined');
    return;
  }

  if (rtCfg.spyMessgeHandler) {
    // @ts-ignore
    evaldModule["$setSpyMessageHandler"](rtCfg.spyMessgeHandler);
  } else {
    console.log("RUNNER: No spy message handler");
  }

  if (rtCfg.spyExprHandler) {
    // @ts-ignore
    evaldModule["$setSpyValueHandler"](rtCfg.spyExprHandler);
  } else {
    console.log("RUNNER No spy expr handler");
  }

  if (rtCfg.imgUrlProxy) {
    // @ts-ignore
    evaldModule["$setImgUrlProxyWrapper"](rtCfg.imgUrlProxy);
  } else {
    console.log("RUNNER: No overriding image URL proxy function");
  }

  if (rtCfg.checkBlockRunner) {
    // @ts-ignore
    evaldModule["$setCheckBlockExecutor"](rtCfg.checkBlockRunner);
  } else {
    console.log("RUNNER: No overriding check block handler");
  }

  if (rtCfg.checkBlockFilter) {
    // @ts-ignore
    evaldModule["$setCheckBlockFilter"](rtCfg.checkBlockFilter);
  } else {
    console.log("RUNNER: Not overriding check block filter");
  }
}
