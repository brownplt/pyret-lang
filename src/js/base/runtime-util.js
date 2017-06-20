define("pyret-base/js/runtime-util", [], function() {
  var gs = Math.floor(Math.random() * 10000);
  function gensym(name) {
    return name + String(gs++);
  }
  function isBrowser() {
    var maybeBrowser = !!(typeof window !== 'undefined' && typeof navigator !== 'undefined' && window.document);
    return maybeBrowser || typeof importScripts !== "undefined";
  }

  var suspend;
  if (typeof setImmediate !== 'undefined') {
    suspend = function(f) { return setImmediate(f); };
  }
  else if (isBrowser()
    && (typeof window !== "undefined")
    && window.postMessage
    && window.location.origin !== "file://"
    && window.location.origin !== "null") {
    var origin = String(window.location.origin);
    var postMessageCBToken = String(Math.random());
    var postMessageCBs = {};
    var postMessageIndex = 0;
    window.addEventListener('message', function(e) {
      if(e.data.token === postMessageCBToken && postMessageCBs[e.data.cb]) {
        var f = postMessageCBs[e.data.cb];
        delete postMessageCBs[e.data.cb];
        f();
      }
    });
    var suspend = function(f) {
      postMessageIndex += 1;
      postMessageCBs[postMessageIndex] = f;
      window.postMessage({
        token: postMessageCBToken,
        cb: postMessageIndex
      }, origin);
    };
  }
  else {
    suspend = function(f) { setTimeout(f, 0); };
  }

  function memoModule(modname, moduleFun) {
    return function(RUNTIME, NAMESPACE) {

      if(RUNTIME.modules[modname]) {
        return RUNTIME.modules[modname];
      }
      else {
        return RUNTIME.safeCall(function() {
            return moduleFun(RUNTIME, NAMESPACE);
          }, function(moduleFunVal) {
            RUNTIME.modules[modname] = moduleFunVal;
            return moduleFunVal;
          });
      }
    };
  }

  function modBuiltin(name) {
    return { "import-type": "builtin", name: name };
  }

  // NOTE(joe): This should become a flagged structure that explicitly
  // allows Pyret-tainted JS modules to be imported, which cannot be done from
  // pure Pyret imports (they cannot generate this kind of dependency
  // description).
  var modBuiltinJS = modBuiltin;


  function definePyretModule(name, oldDeps, deps, provides, func) {
    return defineModule(function(_, result) { return result; }, name, oldDeps, deps, provides, func);
  }

  function defineJSModule(name, oldDeps, deps, provides, func) {
    function createJSReturn(runtime, jsObj) {
      return runtime.makeObject({
        "provide-plus-types": runtime.makeObject({
          values: runtime.makeObject({}),
          types: {},
          internal: jsObj
        }),
        answer: runtime.nothing
      });
    }
    return defineModule(createJSReturn, name, oldDeps, deps, provides, func);
  }


  function defineModule(wrap, name, oldDeps, deps, provides, func) {
    var modname = gensym(name);
    return {
      name: name,
      dependencies: deps,
      provides: provides,
      theModule: function(/* varargs */) {
        var pyretDependencies = Array.prototype.slice.call(arguments);
        return memoModule(modname, function(runtime, namespace) {
          return runtime.loadModulesNew(namespace, pyretDependencies, function(/* instantiated modules */) {
            var deps = Array.prototype.slice.call(arguments);
            return runtime.safeCall(function() {
              return func.apply(null, [runtime, namespace].concat(deps));
            }, function(result) {
              return wrap(runtime, result);
            });
          });
        });
      }
    };
  }

  function makeModuleReturn(runtime, types, values) {
    return runtime.makeObject({
      "provide-plus-types": runtime.makeObject({
        types: types,
        values: runtime.makeObject(values)
      })
    });
  }

  return {
      modBuiltin: modBuiltin,
      modBuiltinJS: modBuiltinJS,

      memoModule: memoModule,
      makeModuleReturn: makeModuleReturn,
      isBrowser: isBrowser,
      suspend: suspend,
      definePyretModule: definePyretModule,
      defineJSModule: defineJSModule,
      isBrowser: isBrowser
    };
});
