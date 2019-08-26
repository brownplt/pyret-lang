const assert = require('assert');
const immutable = require('immutable');
export const stopify = require('@stopify/stopify');
const browserFS = require('./browserfs-setup.ts');

(window as any)["stopify"] = stopify;

const fs = browserFS.fs;
const path = browserFS.path;

const nodeModules = {
  'assert': assert,
  'immutable': immutable,
};

function wrapContent(content: string): string {
  return `(function() { ${content} })();`;
}

export const makeRequireAsync = (
  basePath: string): ((importPath: string) => Promise<any>) => {
  let cwd = basePath;
  let currentRunner: any = null;

  const requireAsyncMain = (importPath: string) => {
    return new Promise(function (resolve, reject) {
      if(importPath in nodeModules) {
        return (nodeModules as any)[importPath];
      }
      const oldWd = cwd;
      const nextPath = path.join(cwd, importPath);
      cwd = path.parse(nextPath).dir;
      if(!fs.existsSync(nextPath)) {
        throw new Error("Path did not exist in requireSync: " + nextPath);
      }
      const stoppedPath = nextPath + ".stopped";
      let runner: any = null;
      const contents = String(fs.readFileSync(nextPath));
      const toStopify = wrapContent(contents);
      runner = stopify.stopifyLocally(toStopify, {});
      if(runner.kind !== "ok") { reject(runner); }
      fs.writeFileSync(stoppedPath, runner.code);
      const stopifyModuleExports = {
        exports: { 
          __pyretExports: nextPath,
        }
      };
      runner.g = Object.assign(runner.g, {
        document,
        Math,
        Array,
        Object,
        stopify,
        require: requireAsync,
        "module": stopifyModuleExports,
        // TS 'export' syntax desugars to 'exports.name = value;'
        "exports": stopifyModuleExports.exports,  
        String,
        $STOPIFY: runner,
        setTimeout: setTimeout,
        console: console,
        parseFloat,
        isNaN
      });
      runner.path = nextPath;
      currentRunner = runner;
      runner.run((result: any) => {
        // TODO(Alex): fix stopify bug where evaled result is not passed to AbstractRunner.onDone callback
        cwd = oldWd;
        if(result.type !== "normal") {
          reject(result);
          return;
        }
        const toReturn = runner.g.module.exports;
        resolve(toReturn);
      });
    });
  };

  const requireAsync = (importPath: string) => {
    if(importPath in nodeModules) {
      return (nodeModules as any)[importPath];
    }
    const oldWd = cwd;
    const nextPath = path.join(cwd, importPath);
    cwd = path.parse(nextPath).dir;
    if(!fs.existsSync(nextPath)) {
      throw new Error("Path did not exist in requireSync: " + nextPath);
    }
    const stoppedPath = nextPath + ".stopped";
    currentRunner.pauseK((kontinue: (result: any) => void) => {
      const lastPath = currentRunner.path;
      const module = {
        exports: { 
          __pyretExports: nextPath,
        }
      };
      const lastModule = currentRunner.g.module;
      currentRunner.g.module = module;
      currentRunner.path = nextPath;
      let stopifiedCode = "";
      if(fs.existsSync(stoppedPath) && (fs.statSync(stoppedPath).mtime > fs.statSync(nextPath).mtime)) {
        stopifiedCode = String(fs.readFileSync(stoppedPath));
      }
      else {
        const contents = String(fs.readFileSync(nextPath));
        stopifiedCode = currentRunner.compile(wrapContent(contents));
        fs.writeFileSync(stoppedPath, stopifiedCode);
      }
      currentRunner.evalCompiled(stopifiedCode, (result: any) => {
        if(result.type !== "normal") {
          kontinue(result);
          return;
        }
        const toReturn = currentRunner.g.module.exports;
        currentRunner.path = lastPath;
        currentRunner.module = lastModule;
        kontinue({ type: 'normal', value: toReturn });
      });
    });
  }

  return requireAsyncMain;
};

export const makeRequire = (basePath: string): ((importPath: string) => any) => {
  var cwd = basePath;
  /*
    Recursively eval (with this definition of require in scope) all of the
    described JavaScript.

    Note that since JS code is generated/written with the assumption that
    require() is sync, we can only use sync versions of the FS function here;
    require must be entirely one synchronous run of the code.

    Future use of stopify could enable the definition of requireAsync, which
    could pause the stack while requiring and then resume.
  */
  const requireSync = (importPath: string) => {
    if(importPath in nodeModules) {
      return (nodeModules as any)[importPath];
    }
    const oldWd = cwd;
    const nextPath = path.join(cwd, importPath);
    cwd = path.parse(nextPath).dir;
    if(!fs.existsSync(nextPath)) {
      throw new Error("Path did not exist in requireSync: " + nextPath);
    }
    const contents = fs.readFileSync(nextPath);
    const f = new Function("require", "module", contents);
    const module = {exports: false};
    const result = f(requireSync, module);
    const toReturn = module.exports ? module.exports : result;
    cwd = oldWd;
    return toReturn;
  };

  return requireSync;
};
