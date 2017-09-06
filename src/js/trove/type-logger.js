({
  requires:
    [
    ],
  nativeRequires: ["pyret-base/js/namespace"],
  provides: {
    shorthands: {
    },
    values: {
      "log": ["arrow", ["String", "Any"], "Nothing"]
    },
    aliases: {
    },
    datatypes: {
    }
  },
  theModule: function(runtime, namespace, uri, VSlib){
    var O = runtime.makeObject;
    var F = runtime.makeFunction;
    var arity = runtime.checkArity;
    var get = runtime.getField;

    function addToLog(name, loggedVal) {
      if (typeof window != 'undefined' && typeof window.logger != 'undefined') {
        window.logger.log(name, {value: loggedVal});
      }
      return runtime.nothing;
    }

    var vals = {
      "log": F(addToLog, "log")
    };
    var types = {
    };
    var internal = {
    };
    return runtime.makeModuleReturn(vals, types, internal);
  }
})
