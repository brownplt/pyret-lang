define([], function() {
  var gs = Math.floor(Math.random() * 10000);
  function gensym(name) {
    return name + String(gs++);
  }
  function isBrowser() {
    return requirejs.isBrowser || typeof importScripts !== "undefined";
  }

  var suspend;
  if (typeof setImmediate !== 'undefined') {
    suspend = setImmediate;
  }
  else if (isBrowser() && (typeof window !== "undefined") && window.postMessage) {
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

  function memoModule(name, moduleFun) {
    var modname = gensym(name);
    return function(RUNTIME, NAMESPACE) {

      if(RUNTIME.modules[modname]) {
        return RUNTIME.modules[modname];
      }
      else {
        RUNTIME.modules[modname] = moduleFun(RUNTIME, NAMESPACE);
        return RUNTIME.modules[modname];
        // TODO(joe): We are *not* safe for deep calls on module loads.
        // If running the module blows the stack, then we fail to load
        // the module.
        /*
        return RUNTIME.safeCall(function() {
            return moduleFun(RUNTIME, NAMESPACE);
          }, function(moduleFunVal) {
            RUNTIME.modules[modname] = moduleFunVal;
            return moduleFunVal;
          });
        */
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
      memoModule: memoModule,
      makeModuleReturn: makeModuleReturn,
      isBrowser: isBrowser,
      suspend: suspend
    };
});
