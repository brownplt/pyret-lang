const assert = require('assert');
const immutable = require('immutable');
const browserFS = window['BrowserFS'].BFSRequire('fs');
const path = window['BrowserFS'].BFSRequire('path');
const stopify = require('@stopify/stopify');
window['stopify'] = stopify;

const nodeModules = {
  'assert': assert,
  'immutable': immutable,
};

function makeRequireAsync(basePath : string) {
  let cwd = basePath;
  let currentRunner = null;

  function requireAsyncMain(importPath : string) {
    return new Promise(function (resolve, reject) {
      if(importPath in nodeModules) {
        return nodeModules[importPath];
      }
      const oldWd = cwd;
      const nextPath = path.join(cwd, importPath);
      cwd = path.parse(nextPath).dir;
      if(!browserFS.existsSync(nextPath)) {
        throw new Error("Path did not exist in requireSync: " + nextPath);
      }
      const stoppedPath = nextPath + ".stopped";
      let runner = null;
      if(browserFS.existsSync(stoppedPath) && (browserFS.statSync(stoppedPath).mtime > browserFS.statSync(nextPath).mtime)) {
        const stopifiedCode = String(browserFS.readFileSync(stoppedPath));
        runner = new stopify.Runner(stopifiedCode, {});
      }
      else {
        const contents = String(browserFS.readFileSync(nextPath));
        runner = stopify.stopifyLocally("(function() { " + contents + "})()", {});
        if(runner.kind !== "ok") { reject(runner); }
        browserFS.writeFileSync(stoppedPath, runner.code);
      }
      const module = {exports: false};
      runner.g = { stopify, require: requireAsync, module, String, $STOPIFY: runner, setTimeout: setTimeout, console: console };
      runner.path = nextPath;
      currentRunner = runner;
      runner.run((result) => {
        cwd = oldWd;
        if(result.type !== "normal") {
          reject(result);
          return;
        }
        const toReturn = module.exports ? module.exports : result;
        resolve(toReturn);
      });
    });
  }

  function requireAsync(importPath : string) {
    if(importPath in nodeModules) {
      return nodeModules[importPath];
    }
    const oldWd = cwd;
    const nextPath = path.join(cwd, importPath);
    cwd = path.parse(nextPath).dir;
    if(!browserFS.existsSync(nextPath)) {
      throw new Error("Path did not exist in requireSync: " + nextPath);
    }
    const stoppedPath = nextPath + ".stopped";
    currentRunner.pauseK((kontinue) => {
      const lastPath = currentRunner.path;
      const module = {exports: false};
      const lastModule = currentRunner.g.module;
      currentRunner.g.module = module;
      currentRunner.path = nextPath;
      let stopifiedCode = "";
      if(browserFS.existsSync(stoppedPath) && (browserFS.statSync(stoppedPath).mtime > browserFS.statSync(nextPath).mtime)) {
        stopifiedCode = String(browserFS.readFileSync(stoppedPath));
      }
      else {
        const contents = String(browserFS.readFileSync(nextPath));
        stopifiedCode = currentRunner.compile(contents);
        browserFS.writeFileSync(stoppedPath, stopifiedCode);
      }
      currentRunner.evalCompiled(stopifiedCode, (result) => {
        if(result.type !== "value") {
          kontinue(result);
          return;
        }
        const toReturn = module.exports ? module.exports : result.value;
        currentRunner.path = lastPath;
        currentRunner.module = lastModule;
        kontinue({ type: 'normal', value: toReturn });
      });
    });
  }

  return requireAsyncMain;
}

function makeRequire(basePath : string) {
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
  function requireSync(importPath : string) {
    if(importPath in nodeModules) {
      return nodeModules[importPath];
    }
    const oldWd = cwd;
    const nextPath = path.join(cwd, importPath);
    cwd = path.parse(nextPath).dir;
    if(!browserFS.existsSync(nextPath)) {
      throw new Error("Path did not exist in requireSync: " + nextPath);
    }
    const contents = browserFS.readFileSync(nextPath);
    const f = new Function("require", "module", contents);
    const module = {exports: false};
    const result = f(requireSync, module);
    const toReturn = module.exports ? module.exports : result;
    cwd = oldWd;
    return toReturn;
  }
  return requireSync;
}

module.exports = { makeRequire, makeRequireAsync };
