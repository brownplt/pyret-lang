define("pyret-base/js/secure-loader", ["require", "q", "pyret-base/js/runtime-util"], function(rjs, Q, util) {
  function compileInNewScriptContext(src) {
    var promise = Q.defer();
    var loader_callback_count = 0;
    if(util.isBrowser()) {
      loader_callback_count += 1;
      var callback = "loader_callback_" + loader_callback_count;
      window[callback] = function(result) {
        document.body.removeChild(script);
        delete window[callback];
        promise.resolve(result);
      }
      var script = document.createElement("script");
      script.innerHTML = "(window['" + callback + "'](" + src + "));";
      document.body.appendChild(script);
      return promise.promise;
    }
    else {
      var VM = require("vm");
    }
  }
  var ourCajaVM;
  function unsafeCaja() {
    var compileExpr = function(src) {
      return function(env) {
        var define = env.define;
        return Function("define", src)(define);
      }
    };
    ourCajaVM = { compileExpr: compileExpr };
  }
  if(util.isBrowser()) {
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

    var RUN_SES = false; // NOTE(joe): skipping on servers for now; SES isn't really there yet
    if(RUN_SES) {
      var source = FS.readFileSync(require.nodeRequire.resolve("ses/initSES.js"));
      var oldLog = console.log;
      console.log = function() { /* intentional no-op to suppress SES logging */ }
      var script = new VM.Script(source);
      script.runInThisContext();
      ourCajaVM = cajaVM;
      console.log = oldLog;
    } else {
      unsafeCaja();
    }
  }

  function safeEval(string, env) {
    var f = ourCajaVM.compileExpr(string);
    return f(env);
  }

  function loadClosure(runtime, mod, dependencies) {
    var deferred = Q.defer();
    try {
      var answer = mod.apply(null, dependencies);
      deferred.resolve(answer);
    } catch(e) {
      deferred.reject(e);
    }
    return deferred.promise;
  }


  function loadSingle(runtime, src, dependencies) {
    var deferred = Q.defer();
    try {
      safeEval(src, {
        define: function(_, body) {
          try {
            // NOTE(joe): dependencies should be empty list, or the whole
            // object should just be passed in here
            var moduleAsObject = body.apply(null, dependencies);
            // NOTE(joe): modules should be objects, but old ones are
            // functions
            if(typeof moduleAsObject === "function") {
              deferred.resolve(moduleAsObject);
            }
            else {
              deferred.resolve(moduleAsObject.theModule.apply(null, dependencies));
            }
          } catch(e) {
            deferred.reject(e);
          }
        }
      });
    }
    catch(e) {
      deferred.reject(e);
    }
    return deferred.promise;
  }

  function goodIdea(runtime, name, src) {
    var deferred = Q.defer();
    require.undef(name);
    try {
      safeEval(src, { define: function(deps, body) {
          define(name, deps, body);
          function success(val) {
            deferred.resolve(val);
          }
          // Since requirejs seems to not call our errback, use its global
          // error handler.
          var oldOnError = require.onError;
          require.onError = function(err) {
            require.onError = oldOnError;
            var names = [];
            for(var i = 0; i < err.requireModules.length; i++) {
              require.undef(err.requireModules[i]);
              names.push(err.requireModules[i]);
            }
            deferred.reject(runtime.makePyretFailException(runtime.ffi.makeModuleLoadFailureL(names)));
          };
          require([name], success);
        }
      });
    }
    catch(e) {
      deferred.reject(e);
    }
    return deferred.promise;
  }

  return {
    safeEval: safeEval,
    goodIdea: goodIdea,
    loadSingle: loadSingle,
    loadClosure: loadClosure,
    compileInNewScriptContext: compileInNewScriptContext,
  }
});
