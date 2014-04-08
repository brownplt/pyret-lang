define(["require"], function(rjs) {
  if(requirejs.isBrowser) {
    // initSES.js had better be on the page already
    if(!cajaVM) {
      console.error("This page cannot load without loading initSES.js first!");
    }
    var defn = define;
  }
  else {
    var FS = require("fs");
    var VM = require("vm");

    var source = FS.readFileSync(require.nodeRequire.resolve("ses/initSES.js"));
    var oldLog = console.log;
    console.log = function() { /* intentional no-op to suppress SES logging */ }
    var script = new VM.Script(source);
    script.runInThisContext();
    console.log = oldLog;
    var defn = define;
  }

  function safeEval(string, env) {
    var f = cajaVM.compileExpr(string);
    var defaultEnv = {
      undefined: undefined,
      Object: Object
    };
    Object.keys(env).forEach(function(k) {
      defaultEnv[k] = env[k];
    });
    f(defaultEnv);
  }

  function goodIdea(name, src) {
    safeEval(src, { define: function(deps, body) {
        defn(name, deps, body);
      }
    });
  }

  return {
    safeEval: safeEval,
    goodIdea: goodIdea
  }
});
