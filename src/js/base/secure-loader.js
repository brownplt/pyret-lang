define(["require", "q"], function(rjs, Q) {
  var ourCajaVM;
  function unsafeCaja() {
    var compileExpr = function(src) {
      return function(env) {
        var define = env.define;
        Function("define", src)(define);
      }
    };
    ourCajaVM = { compileExpr: compileExpr };
  }
  if(requirejs.isBrowser) {
    // caja.js had better be on the page already
    if(typeof caja === "undefined") {
      console.warn("Page was loaded without SES, so evals will be unguarded. Does this page load https://caja.appspot.com/caja.js?");
      unsafeCaja();
    }
    else {
      caja.initialize({
        debug: true,
        forceES5Mode: true
      });
      caja.load(undefined, undefined, function(frame) {
        ourCajaVM = {
          compileExpr: function(s) {
            return function(env) { frame.code("https://", "application/javascript", s).api(env).run(); }
          }
        };
      });
    }
  }
  else {
    var FS = require("fs");
    var VM = require("vm");

    var source = FS.readFileSync(require.nodeRequire.resolve("ses/initSES.js"));
    var oldLog = console.log;
    console.log = function() { /* intentional no-op to suppress SES logging */ }
    var script = new VM.Script(source);
//    script.runInThisContext();
//    ourCajaVM = cajaVM;
    unsafeCaja();
    console.log = oldLog;
  }

  function safeEval(string, env) {
    var f = ourCajaVM.compileExpr(string);
    f(env);
  }

  function goodIdea(name, src) {
    var deferred = Q.defer();
    safeEval(src, { define: function(deps, body) {
        define(name, deps, body);
        deferred.resolve(name);
      }
    });
    return deferred.promise;
  }

  return {
    safeEval: safeEval,
    goodIdea: goodIdea
  }
});
