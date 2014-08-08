define(["js/runtime-util", "trove/lists", "trove/option", "trove/either", "trove/error", "trove/srcloc", "trove/contracts"], function(util, listLib, optLib, eitherLib, errorLib, srclocLib, contractsLib) {
  return util.memoModule("ffi-helpers", function(runtime, namespace) {
    
    return runtime.loadModules(namespace, [listLib, optLib, eitherLib, errorLib, srclocLib, contractsLib], function(L, O, E, ERR, S, C) {

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
      }, "Srcloc");

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

      var checkArity = runtime.checkArity;


      function err(str) { return gf(ERR, str).app; }
      function contract(str) { return gf(C, str).app; }
      function errPred(str) {
        return function(val) {
          return runtime.unwrap(gf(ERR, str).app(val));
        };
      }
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
      function throwExtendNonObject(loc, nonObject) {
        checkSrcloc(loc);
        runtime.checkPyretVal(nonObject);
        raise(err("extend-non-object")(loc, nonObject));
      }

      function throwMessageException(message) {
        runtime.checkString(message);
        raise(err("message-exception")(message));
      }

      function makeMessageException(message) {
        runtime.checkString(message);
        return err("message-exception")(message);
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

      function throwInvalidArrayIndex(methodName, array, index, reason) {
        runtime.checkString(methodName);
        runtime.checkArray(array);
        runtime.checkNumber(index);
        runtime.checkString(reason);
        raise(err("invalid-array-index")(methodName, array, index, reason));
      }

      function throwPlusError(left, right) {
        runtime.checkPyretVal(left);
        runtime.checkPyretVal(right);
        raise(err("plus-error")(left, right));
      }

      function throwNumericBinopError(left, right, opname, methodname) {
        runtime.checkPyretVal(left);
        runtime.checkPyretVal(right);
        runtime.checkString(opname);
        runtime.checkString(methodname);
        raise(err("numeric-binop-error")(left, right, opname, methodname));
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


      function throwCasesArityError(branchLoc, arity, fields) {
        checkSrcloc(branchLoc);
        runtime.checkNumber(arity);
        runtime.checkNumber(fields);
        raise(err("cases-arity-mismatch")(branchLoc, arity, fields));
      }

      function throwCasesArityErrorC(branchLoc, arity, fields) {
        var loc = runtime.makeSrcloc(branchLoc);
        throwCasesArityError(loc, arity, fields);
      }


      function throwCasesSingletonError(branchLoc, shouldBeSingleton) {
        checkSrcloc(branchLoc);
        runtime.checkBoolean(shouldBeSingleton);
        raise(err("cases-singleton-mismatch")(branchLoc, shouldBeSingleton));
      }

      function throwCasesSingletonErrorC(branchLoc, shouldBeSingleton) {
        var loc = runtime.makeSrcloc(branchLoc);
        throwCasesSingletonError(loc, shouldBeSingleton);
      }

      function throwNonBooleanCondition(locArray, type, val) {
        runtime.checkString(type);
        runtime.checkPyretVal(val);
        raise(err("non-boolean-condition")(runtime.makeSrcloc(locArray), type, val));
      }
      function throwNonBooleanOp(locArray, position, type, val) {
        runtime.checkString(position);
        runtime.checkString(type);
        runtime.checkPyretVal(val);
        raise(err("non-boolean-op")(runtime.makeSrcloc(locArray), position, type, val));
      }
      function throwNoBranchesMatched(locArray, type) {
        runtime.checkString(type);
        raise(err("no-branches-matched")(runtime.makeSrcloc(locArray), type));
      }
      function throwNonFunApp(locArray, funVal) {
        runtime.checkPyretVal(funVal);
        raise(err("non-function-app")(runtime.makeSrcloc(locArray), funVal));
      }

      function throwParseErrorNextToken(loc, nextToken) {
        raise(err("parse-error-next-token")(loc, nextToken));
      }
      function throwParseErrorEOF(loc) {
        raise(err("parse-error-eof")(loc));
      }

      function throwModuleLoadFailureL(names) {
        raise(makeModuleLoadFailureL(names));
      }

      function makeModuleLoadFailureL(names) {
        var namesList = makeList(names);
        return err("module-load-failure")(namesList);
      }

      function makeRecordFieldsFail(value, failures) {
        runtime.checkPyretVal(value);
        return contract("record-fields-fail")(value, failures);
      }

      function makeFieldFailure(loc, field, reason) {
        checkSrcloc(loc);
        runtime.checkString(field);
        return contract("field-failure")(loc, field, reason);
      }

      function makeMissingField(loc, field) {
        checkSrcloc(loc);
        runtime.checkString(field);
        return contract("missing-field")(loc, field);
      }

      function makeTypeMismatch(val, name) {
        runtime.checkString(name);
        runtime.checkPyretVal(val);
        return contract("type-mismatch")(val, name);
      }

      function makeRefInitFail(loc, reason) {
        checkSrcloc(loc);
        return contract("ref-init")(loc, reason);
      }

      function makePredicateFailure(val, name) {
        runtime.checkString(name);
        runtime.checkPyretVal(val);
        return contract("predicate-failure")(val, name);
      }

      function makeDotAnnNotPresent(name, field) {
        runtime.checkString(name);
        runtime.checkPyretVal(field);
        return contract("dot-ann-not-present")(name, field);
      }

      function isOk(val) {
        return contract("is-ok")(val);
      }

      function isFail(val) {
        return contract("is-fail")(val);
      }

      function isFailArg(val) {
        return contract("is-fail-arg")(val);
      }

      return {
        throwPlusError: throwPlusError,
        throwNumericBinopError: throwNumericBinopError,
        throwInternalError: throwInternalError,
        throwFieldNotFound: throwFieldNotFound,
        throwLookupNonObject: throwLookupNonObject,
        throwExtendNonObject: throwExtendNonObject,
        throwTypeMismatch: throwTypeMismatch,
        throwInvalidArrayIndex: throwInvalidArrayIndex,
        throwMessageException: throwMessageException,
        throwUninitializedId: throwUninitializedId,
        throwUninitializedIdMkLoc: throwUninitializedIdMkLoc,
        throwArityError: throwArityError,
        throwArityErrorC: throwArityErrorC,
        throwCasesArityError: throwCasesArityError,
        throwCasesArityErrorC: throwCasesArityErrorC,
        throwCasesSingletonError: throwCasesSingletonError,
        throwCasesSingletonErrorC: throwCasesSingletonErrorC,
        throwNonBooleanCondition: throwNonBooleanCondition,
        throwNonBooleanOp: throwNonBooleanOp,
        throwNoBranchesMatched: throwNoBranchesMatched,
        throwNonFunApp: throwNonFunApp,
        throwModuleLoadFailureL: throwModuleLoadFailureL,

        throwParseErrorNextToken: throwParseErrorNextToken,
        throwParseErrorEOF: throwParseErrorEOF,

        makeRecordFieldsFail: makeRecordFieldsFail,
        makeFieldFailure: makeFieldFailure,
        makeMissingField: makeMissingField,
        makeTypeMismatch: makeTypeMismatch,
        makeRefInitFail: makeRefInitFail,
        makePredicateFailure: makePredicateFailure,
        makeDotAnnNotPresent: makeDotAnnNotPresent,
        contractOk: gf(C, "ok"),
        contractFail: contract("fail"),
        contractFailArg: contract("fail-arg"),
        isOk: isOk,
        isFail: isFail,
        isFailArg: isFailArg,

        makeMessageException: makeMessageException,
        makeModuleLoadFailureL: makeModuleLoadFailureL,

        userBreak: gf(ERR, "user-break"),
        isUserBreak: errPred("is-user-break"),

        errPred: errPred,

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
