define(["./runtime-util", "trove/list", "trove/option", "trove/either", "trove/error"], function(util, listLib, optLib, eitherLib, errorLib) {
  return util.memoModule("ffi-helpers", function(runtime, namespace) {
    
    var L = runtime.getField(listLib(runtime, namespace), "provide");
    var O = runtime.getField(optLib(runtime, namespace), "provide");
    var E = runtime.getField(eitherLib(runtime, namespace), "provide");
    var ERR = runtime.getField(errorLib(runtime, namespace), "provide");

    var gf = runtime.getField;

    function makeList(arr) {
      var lst = runtime.getField(L, "empty");
      for(var i = arr.length - 1; i >= 0; i--) {
        lst = runtime.getField(L, "link").app(arr[i], lst); 
      }
      return lst;
    }

    function err(str) { return gf(ERR, str).app; }
    var raise = runtime.raise;

    function throwInternalError(message, otherArgs) {
      raise(err("internal-error")(runtime.makeString(message), otherArgs));
    }

    function throwFieldNotFound(object, field) {
      raise(err("field-not-found")(object, runtime.makeString(field)));
    }
    function throwLookupNonObject(nonObject, field) {
      raise(err("lookup-non-object")(nonObject, runtime.makeString(field)));
    }

    return {
      throwInternalError: throwInternalError,
      throwFieldNotFound: throwFieldNotFound,
      throwLookupNonObject: throwLookupNonObject,
      makeList: makeList,
      makeNone: function() { return runtime.getField(O, "none"); },
      makeSome: function(v) { return runtime.getField(O, "some").app(v); },          
      makeLeft: function(l) { return runtime.getField(E, "left").app(l); },
      makeRight: function(r) { return runtime.getField(E, "right").app(r); },

      toArray: function(list) {
        var isList = runtime.getField(L, "List");
        var isEmpty = runtime.getField(L, "is-empty");
        var isLink = runtime.getField(L, "is-link");
        // console.error("list is " + JSON.stringify(list).substr(0, 100));
        // console.error("list is Object? " + runtime.isObject(list));
        // console.error("list.brands is " + JSON.stringify(list.brands));
        if(!(runtime.unwrap(isList.app(list)) === true)) {
          throw "Non-list given to toArray " + String(list);
        }
        var arr = [];
        try {
          while(!(runtime.unwrap(isEmpty.app(list)) === true)) {
            try {
              arr.push(runtime.getField(list, "first"));
            } catch(e) {
              console.error("***** getField first failed on list: " + JSON.stringify(list));
              console.error(e);
              throw e;
            }
            try {
              list = runtime.getField(list, "rest");
            } catch(e) {
              console.error("***** getField rest failed on list: " + JSON.stringify(list));
              console.error(e);
              throw e;
            }
          }
        } catch(e) {
          console.error("******* Calling isEmpty failed on list: " + JSON.stringify(list));
          console.error(e);
          throw e;
        }
        return arr;
      },
      isList: function(list) { return runtime.unwrap(runtime.getField(L, "List").app(list)); }
    };

  });
});
