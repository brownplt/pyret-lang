define(["./runtime-util", "trove/list", "trove/option", "trove/either", "trove/error", "trove/srcloc"], function(util, listLib, optLib, eitherLib, errorLib, srclocLib) {
  return util.memoModule("ffi-helpers", function(runtime, namespace) {
    
    var L = runtime.getField(listLib(runtime, namespace), "provide");
    var O = runtime.getField(optLib(runtime, namespace), "provide");
    var E = runtime.getField(eitherLib(runtime, namespace), "provide");
    var ERR = runtime.getField(errorLib(runtime, namespace), "provide");
    var S = runtime.getField(srclocLib(runtime, namespace), "provide");

    function makeList(arr) {
      var lst = runtime.getField(L, "empty");
      for(var i = arr.length - 1; i >= 0; i--) {
        lst = runtime.getField(L, "link").app(arr[i], lst); 
      }
      return lst;
    }
    var gf = runtime.getField;

    var checkSrcloc = runtime.makeCheckType(function(val) {
      return runtime.unwrap(gf(S, "Srcloc").app(val));
    });

    function err(str) { return gf(ERR, str).app; }
    var raise = runtime.raise;

    function throwInternalError(message, otherArgs) {
      runtime.checkString(message);
      runtime.checkList(otherArgs);
      raise(err("internal-error")(runtime.makeString(message), otherArgs));
    }

    function throwFieldNotFound(loc, object, field) {
      checkSrcloc(loc);
      runtime.checkPyretVal(object);
      runtime.checkString(field);
      raise(err("field-not-found")(loc, object, runtime.makeString(field)));
    }
    function throwLookupNonObject(loc, nonObject, field) {
      checkSrcloc(loc);
      runtime.checkPyretVal(nonObject);
      runtime.checkString(field);
      raise(err("lookup-non-object")(loc, nonObject, runtime.makeString(field)));
    }

    function throwMessageException(message) {
      runtime.checkString(message);
      raise(err("message-exception")(message));
    }

    function throwTypeMismatch(val, typeName) {
      // NOTE(joe): can't use checkPyretVal here, because it will re-enter
      // this function and blow up... so bottom out at "nothing"
      if(!runtime.isPyretVal(val)) {
        val = runtime.namespace.get("nothing");
      }
      runtime.checkString(typeName);
      raise(err("generic-type-mismatch")(val, typeName));
    }

    function throwPlusError(left, right) {
      runtime.checkPyretVal(left);
      runtime.checkPyretVal(right);
      raise(err("plus-error")(left, right));
    }

    function throwUninitializedId(loc, name) {
      checkSrcloc(loc);
      runtime.checkString(name);
      raise(err("uninitialized-id")(loc, name));
    }

    function throwUninitializedIdMkLoc(loc, name) {
      throwUninitializedId(runtime.makeSrcloc(loc), name);
    }

    return {
      throwPlusError: throwPlusError,
      throwInternalError: throwInternalError,
      throwFieldNotFound: throwFieldNotFound,
      throwLookupNonObject: throwLookupNonObject,
      throwTypeMismatch: throwTypeMismatch,
      throwMessageException: throwMessageException,
      throwUninitializedId: throwUninitializedId,
      throwUninitializedIdMkLoc: throwUninitializedIdMkLoc,
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
