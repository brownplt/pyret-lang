define(["require"], function(rjs) {
  function unsafeCaja() {
    var compileExpr = function(src) {
      return function(env) {
        var define = env.define;
        Function("define", src)(define);
      }
    };
    cajaVM = { compileExpr: compileExpr };
  }
  if(requirejs.isBrowser) {
    // initSES.js had better be on the page already
    if(!cajaVM) {
      console.warn("Loading without SES");
      unsafeCaja();
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
//    script.runInThisContext();
    unsafeCaja();
    console.log = oldLog;
    var defn = define;
  }

  function safeEval(string, env) {
    var f = cajaVM.compileExpr(string);
    f(env);
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
