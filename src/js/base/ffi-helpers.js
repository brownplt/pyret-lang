define(["./runtime-util", "trove/list", "trove/option", "trove/either", "trove/error", "trove/srcloc"], function(util, listLib, optLib, eitherLib, errorLib, srclocLib) {
  return util.memoModule("ffi-helpers", function(runtime, namespace) {
    
    return runtime.loadModules(namespace, [listLib, optLib, eitherLib, errorLib, srclocLib], function(L, O, E, ERR, S) {

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

      function cases(pred, predName, val, casesObj) {
        if(!pred.app(val)) {
          throwTypeMismatch(val, predName);
        }
        var pyretObj = {}
        var els = runtime.makeFunction(function(v) {
          throwMessageException("No cases matched");
        });
        Object.keys(casesObj).forEach(function(k) {
          if(k !== "else") {
            pyretObj[k] = runtime.makeFunction(casesObj[k]);
          } else {
            els = runtime.makeFunction(casesObj[k]);
          }
        });
        return runtime.safeTail(function() {
          return gf(val, "_match").app(runtime.makeObject(pyretObj), els);
        });
      }

      var checkArity = function(expected, args) {
        if (expected !== args.length) {
          throw runtime.ffi.throwArityErrorC(["builtin"], expected, args);
        }
      }


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

      function throwArityError(funLoc, arity, args) {
        checkSrcloc(funLoc);
        runtime.checkNumber(arity);
        runtime.checkList(args);
        raise(err("arity-mismatch")(funLoc, arity, args));
      }

      function throwArityErrorC(funLoc, arity, args) {
        var loc = runtime.makeSrcloc(funLoc);
        var argsPyret = makeList(Array.prototype.slice.call(args, 0, args.length));
        throwArityError(loc, arity, argsPyret);
      }

      function locFromObj(obj) {
        if (!runtime.hasField(obj, "source")) { return runtime.makeSrcloc(["builtin"]); }
        return runtime.makeSrcloc([
          gf(obj, "source"),
          gf(obj, "start-line"),
          gf(obj, "start-column"),
          gf(obj, "start-char"),
          gf(obj, "end-line"),
          gf(obj, "end-column"),
          gf(obj, "end-char")
        ]);
      }
      function throwNonBooleanCondition(locAsObj, type, val) {
        runtime.checkString(type);
        runtime.checkPyretVal(val);
        raise(err("non-boolean-condition")(locFromObj(locAsObj), type, val));
      }
      function throwNonBooleanOp(locAsObj, position, type, val) {
        runtime.checkString(position);
        runtime.checkString(type);
        runtime.checkPyretVal(val);
        raise(err("non-boolean-op")(locFromObj(locAsObj), position, type, val));
      }

      function throwNonFunApp(locArray, funVal, args) {
        runtime.checkPyretVal(funVal);
        var argList = makeList(args);
        raise(err("non-function-app")(runtime.makeSrcloc(locArray), funVal, argList));
      }

      function throwParseErrorNextToken(loc, nextToken) {
        raise(err("parse-error-next-token")(loc, nextToken));
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
        throwArityError: throwArityError,
        throwArityErrorC: throwArityErrorC,
        throwNonBooleanCondition: throwNonBooleanCondition,
        throwNonBooleanOp: throwNonBooleanOp,
        throwNonFunApp: throwNonFunApp,

        throwParseErrorNextToken: throwParseErrorNextToken,

        cases: cases,

        checkArity: checkArity,

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
});
