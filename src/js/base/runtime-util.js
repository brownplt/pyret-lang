define([], function() {
  var gs = Math.floor(Math.random() * 10000);
  function gensym(name) {
    return name + String(gs++);
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
      makeModuleReturn: makeModuleReturn
    };
});
