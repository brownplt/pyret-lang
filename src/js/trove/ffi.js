({
  requires: [
    { "import-type": "builtin", name: "lists" },
    { "import-type": "builtin", name: "sets" },
    { "import-type": "builtin", name: "option" },
    { "import-type": "builtin", name: "either" },
    { "import-type": "builtin", name: "equality" },
    { "import-type": "builtin", name: "error" },
    { "import-type": "builtin", name: "srcloc" },
    { "import-type": "builtin", name: "contracts" },
    // skipping checker
    { "import-type": "builtin", name: "error-display" },
    { "import-type": "builtin", name: "valueskeleton" }
  ],
  provides: {},
  nativeRequires: [],
  theModule: function(runtime, namespace, uri, L, Se, O, E, EQ, ERR, S, CON, /* CH, */ ED, VS) {
    var gf = runtime.getField;
    L = gf(L, "values");
    Se = gf(Se, "values");
    O = gf(O, "values");
    E = gf(E, "values");
    EQ = gf(EQ, "values");
    ERR = gf(ERR, "values");
    S = gf(S, "values");
    CON = gf(CON, "values");
    ED = gf(ED, "values");
    VS = gf(VS, "values");
    var link = gf(L, "link");
    var lnk = function(first, rest) { return link.app(first, rest); };
    var mt = gf(L, "empty");
    function makeList(arr) {
      if (!arr || typeof arr.length !== "number") {
        throw "Non-array given to makeList " + JSON.stringify(arr);
      }
      var lst = mt;
      for(var i = arr.length - 1; i >= 0; i--) {
        lst = lnk(arr[i], lst);
      }
      return lst;
    }

    function makeTreeSet(arr) {
      return gf(Se, 'list-to-tree-set').app(makeList(arr));
    }
    function toArray(list) {
      var isList = runtime.getField(L, "is-List");
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
    }
    var checkSrcloc = runtime.makeCheckType(function(val) {
      return runtime.unwrap(gf(S, "is-Srcloc").app(val));
    }, "Srcloc");

/* NOTE(joe): skipping checker
    function isTestResult(val) { return runtime.unwrap(runtime.getField(CH, "TestResult").app(val)); }
    var checkTestResult = runtime.makeCheckType(isTestResult, "TestResult");
*/

    function isErrorDisplay(val) { return runtime.unwrap(runtime.getField(ED, "ErrorDisplay").app(val)); }
    var checkErrorDisplay = runtime.makeCheckType(isErrorDisplay, "ErrorDisplay");

    function cases(pred, predName, val, casesObj) {
      if(!pred.app(val)) {
        throwTypeMismatch(val, predName);
      }
      var pyretObj = {}
      var els = runtime.makeFunction(function(v) {
        throwMessageException("No cases matched");
      }, "cases-else");
      Object.keys(casesObj).forEach(function(k) {
        if(k !== "else") {
          pyretObj[k] = runtime.makeFunction(casesObj[k], "cases-" + k);
        } else {
          els = runtime.makeFunction(casesObj[k], "cases-else");
        }
      });
      return runtime.safeTail(function() {
        return gf(val, "_match").app(runtime.makeObject(pyretObj), els);
      });
    }

    var checkArity = runtime.checkArity;

/* NOTE(joe): skipping checker
    function checkResultsSummary(checkResults) {
      return runtime.safeCall(
        function() {
          return gf(CH, "results-summary").app(checkResults);
        },
        function(pySummary) {
          return {
            message: gf(pySummary, "message"),
            passed: gf(pySummary, "passed"),
            failed: gf(pySummary, "failed"),
            total: gf(pySummary, "total")
          };
        },
        "results-summary");
    };
*/
    function err(str) { return gf(ERR, str).app; }
    function contract(str) { return gf(CON, str).app; }
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
    function throwLookupConstructorNotObject(loc, constrName, field) {
      checkSrcloc(loc);
      runtime.checkString(constrName);
      runtime.checkString(field);
      raise(err("lookup-constructor-not-object")(loc, constrName, runtime.makeString(field)));
    }
    function throwLookupNonObject(loc, nonObject, field) {
      checkSrcloc(loc);
      runtime.checkPyretVal(nonObject);
      runtime.checkString(field);
      raise(err("lookup-non-object")(loc, nonObject, runtime.makeString(field)));
    }
    function throwLookupNonTuple(loc, nonTuple, index) {
      checkSrcloc(loc);
      runtime.checkPyretVal(nonTuple);
      runtime.checkNumber(index);
      raise(err("lookup-non-tuple")(loc, nonTuple, runtime.makeNumber(index)));
    }
    function throwBadTupleBind(loc, tup, length, desiredLength) {
      checkSrcloc(loc);
      //runtime.checkPyretVal(tup);
      raise(err("bad-tuple-bind")(loc, tup, runtime.makeNumber(length), runtime.makeNumber(desiredLength)));
    }
    function throwLookupLargeIndex(loc, tup, index) {
      checkSrcloc(loc);
      runtime.checkPyretVal(tup);
      runtime.checkNumber(index);
      raise(err("lookup-large-index")(loc, tup, runtime.makeNumber(index)));
    }
    function throwExtendNonObject(loc, nonObject) {
      checkSrcloc(loc);
      runtime.checkPyretVal(nonObject);
      raise(err("extend-non-object")(loc, nonObject));
    }
    function throwLookupNonTuple(loc, nonTuple, index) {
      checkSrcloc(loc);
      runtime.checkPyretVal(nonTuple);
      runtime.checkNumber(index);
      raise(err("lookup-non-tuple")(loc, nonTuple, runtime.makeNumber(index)));
    }
    function throwBadTupleBind(loc, tup, length, desiredLength) {
      checkSrcloc(loc);
      runtime.checkPyretVal(tup);
      raise(err("bad-tuple-bind")(loc, tup, length, desiredLength));
    }
    function throwNonTupleBind(loc, non_tup) {
      checkSrcloc(loc);
      raise(err("non-tuple-bind")(loc, non_tup));
    }
    function throwLookupLargeIndex(loc, tup, index) {
      checkSrcloc(loc);
      runtime.checkPyretVal(tup);
      runtime.checkNumber(index);
      raise(err("lookup-large-index")(loc, tup, tup.vals.length, runtime.makeNumber(index)));
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

    function throwMultiErrorException(errs) {
      runtime.checkList(errs);
      raise(err("multi-error")(errs));
    }

    function throwUserException(errVal) {
      runtime.checkPyretVal(errVal);
      raise(err("user-exception")(errVal));
    }
    function makeUserException(errVal) {
      runtime.checkPyretVal(errVal);
      return err("user-exception")(errVal);
    }

    function throwEqualityException(reason, v1, v2) {
      runtime.checkString(reason);
      runtime.checkPyretVal(v1);
      runtime.checkPyretVal(v2);
      raise(err("equality-failure")(reason, v1, v2));
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

    function throwInvalidTableColumn(table, table_loc, column, column_loc) {
      runtime.checkTable(table);
      runtime.checkString(column);
      checkSrcloc(table_loc);
      checkSrcloc(column_loc);
      raise(err("invalid-table-column")(table, table_loc, column, column_loc));
    }

    function throwNumStringBinopError(left, right, opname, opdesc, methodname) {
      runtime.checkPyretVal(left);
      runtime.checkPyretVal(right);
      runtime.checkString(opname);
      runtime.checkString(opdesc);
      runtime.checkString(methodname);
      raise(err("num-string-binop-error")(left, right, opname, opdesc, methodname));
    }
    function throwNumericBinopError(left, right, opname, opdesc, methodname) {
      runtime.checkPyretVal(left);
      runtime.checkPyretVal(right);
      runtime.checkString(opname);
      runtime.checkString(opdesc);
      runtime.checkString(methodname);
      raise(err("numeric-binop-error")(left, right, opname, opdesc, methodname));
    }

    function throwUpdateNonObj(loc, objval, objloc) {
      runtime.checkPyretVal(objval);
      checkSrcloc(loc);
      checkSrcloc(objloc);
      raise(err("update-non-obj")(loc, objval, objloc));
    }
    
    function throwUpdateFrozenRef(loc, objval, objloc, fieldname, fieldloc) {
      runtime.checkPyretVal(objval);
      checkSrcloc(loc);
      checkSrcloc(objloc);
      runtime.checkString(fieldname);
      checkSrcloc(fieldloc);
      raise(err("update-frozen-ref")(loc, objval, objloc, fieldname, fieldloc));
    }
    
    function throwUpdateNonRef(loc, objval, objloc, fieldname, fieldloc) {
      runtime.checkPyretVal(objval);
      checkSrcloc(loc);
      checkSrcloc(objloc);
      runtime.checkString(fieldname);
      checkSrcloc(fieldloc);
      raise(err("update-non-ref")(loc, objval, objloc, fieldname, fieldloc));
    }
    
    function throwUpdateNonExistentField(loc, objval, objloc, fieldname, fieldloc) {
      runtime.checkPyretVal(objval);
      checkSrcloc(loc);
      checkSrcloc(objloc);
      runtime.checkString(fieldname);
      checkSrcloc(fieldloc);
      raise(err("update-non-existent-field")(loc, objval, objloc, fieldname, fieldloc));
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
      var argsPyret = makeList(args);
      throwArityError(loc, arity, argsPyret);
    }

    function throwConstructorArityErrorC(funLoc, name, arity, args) {
      runtime.checkString(name);
      runtime.checkNumber(arity);
      var loc = runtime.makeSrcloc(funLoc);
      var argsPyret = makeList(args);
      raise(err("constructor-arity-mismatch")(loc, name, arity, argsPyret));
    }

    function throwCasesArityError(branchLoc, arity, fields, casesLoc) {
      checkSrcloc(branchLoc);
      runtime.checkNumber(arity);
      runtime.checkNumber(fields);
      checkSrcloc(casesLoc)
      raise(err("cases-arity-mismatch")(branchLoc, arity, fields, casesLoc));
    }

    function throwCasesArityErrorC(branchLoc, arity, fields, casesLoc) {
      var loc = runtime.makeSrcloc(branchLoc);
      var cloc = runtime.makeSrcloc(casesLoc);
      throwCasesArityError(loc, arity, fields, cloc);
    }

    function throwCasesSingletonError(branchLoc, shouldBeSingleton, casesLoc) {
      checkSrcloc(branchLoc);
      runtime.checkBoolean(shouldBeSingleton);
      checkSrcloc(casesLoc)
      raise(err("cases-singleton-mismatch")(branchLoc, shouldBeSingleton, casesLoc));
    }

    function throwCasesSingletonErrorC(branchLoc, shouldBeSingleton, casesLoc) {
      var loc = runtime.makeSrcloc(branchLoc);
      var cloc = runtime.makeSrcloc(casesLoc);
      throwCasesSingletonError(loc, shouldBeSingleton, cloc);
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
    function throwNoCasesMatched(locArray, val) {
      runtime.checkPyretVal(val);
      raise(err("no-cases-matched")(runtime.makeSrcloc(locArray), val));
    }
    function throwUnfinishedTemplate(locArray) {
      raise(err("template-not-finished")(runtime.makeSrcloc(locArray)));
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
    function throwParseErrorUnterminatedString(loc) {
      raise(err("parse-error-unterminated-string")(loc));
    }
    function throwParseErrorBadNumber(loc) {
      raise(err("parse-error-bad-number")(loc));
    }
    function throwParseErrorBadOper(loc) {
      raise(err("parse-error-bad-operator")(loc));
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
  
    function makeTupleAnnsFail(value, failures) {
      return contract("tuple-anns-fail")(value, failures);
    }

    function makeTupleAnnsFail(value, failures) {
      return contract("tuple-anns-fail")(value, failures);
    }

    function makeFieldFailure(loc, field, reason) {
      checkSrcloc(loc);
      runtime.checkString(field);
      return contract("field-failure")(loc, field, reason);
    }

    function makeAnnFailure(loc, ann, reason) {
      checkSrcloc(loc);
      return contract("ann-failure")(loc, ann, reason);
    }

    function makeMissingField(loc, field) {
      checkSrcloc(loc);
      runtime.checkString(field);
      return contract("missing-field")(loc, field);
    }

    function makeTupleLengthMismatch(loc, val, annLength, tupLength) {
      checkSrcloc(loc);
      runtime.checkNumber(annLength);
      runtime.checkNumber(tupLength);
      return contract("tup-length-mismatch")(loc, val, annLength, tupLength);
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

    function makeFailureAtArg(loc, index, name, args, reason) {
      checkSrcloc(loc);
      runtime.checkNumber(index);
      runtime.checkString(name);
      runtime.checkList(args);
      return contract("failure-at-arg")(loc, index, name, args, reason);
    }

    var isOk = contract("is-ok");
    var isFail = contract("is-fail");
    var isFailArg = contract("is-fail-arg");

    var isEqualityResult = gf(EQ, "is-EqualityResult").app;
    var isEqual = gf(EQ, "is-Equal").app;
    var isNotEqual = gf(EQ, "is-NotEqual").app;
    var isUnknown = gf(EQ, "is-Unknown").app

    var isEmpty = gf(L, "is-empty").app;
    var isLink = gf(L, "is-link").app;

    return runtime.makeJSModuleReturn({
      throwUpdateNonObj : throwUpdateNonObj,
      throwUpdateFrozenRef : throwUpdateFrozenRef,
      throwUpdateNonRef : throwUpdateNonRef,
      throwUpdateNonExistentField : throwUpdateNonExistentField,
      throwNumStringBinopError: throwNumStringBinopError,
      throwNumericBinopError: throwNumericBinopError,
      throwInternalError: throwInternalError,
      throwFieldNotFound: throwFieldNotFound,
      throwLookupConstructorNotObject: throwLookupConstructorNotObject,
      throwLookupNonObject: throwLookupNonObject,
      throwLookupNonTuple: throwLookupNonTuple,
      throwBadTupleBind: throwBadTupleBind,
      throwNonTupleBind: throwNonTupleBind,
      throwLookupLargeIndex: throwLookupLargeIndex,
      throwExtendNonObject: throwExtendNonObject,
      throwTypeMismatch: throwTypeMismatch,
      throwInvalidArrayIndex: throwInvalidArrayIndex,
      throwMessageException: throwMessageException,
      throwMultiErrorException: throwMultiErrorException,
      throwUserException: throwUserException,
      throwEqualityException: throwEqualityException,
      throwUninitializedId: throwUninitializedId,
      throwUninitializedIdMkLoc: throwUninitializedIdMkLoc,
      throwArityError: throwArityError,
      throwArityErrorC: throwArityErrorC,
      throwConstructorArityErrorC: throwConstructorArityErrorC,
      throwCasesArityError: throwCasesArityError,
      throwCasesArityErrorC: throwCasesArityErrorC,
      throwCasesSingletonError: throwCasesSingletonError,
      throwCasesSingletonErrorC: throwCasesSingletonErrorC,
      throwNonBooleanCondition: throwNonBooleanCondition,
      throwNonBooleanOp: throwNonBooleanOp,
      throwNoBranchesMatched: throwNoBranchesMatched,
      throwNoCasesMatched: throwNoCasesMatched,
      throwNonFunApp: throwNonFunApp,
      throwUnfinishedTemplate: throwUnfinishedTemplate,
      throwModuleLoadFailureL: throwModuleLoadFailureL,
      throwInvalidTableColumn: throwInvalidTableColumn,

      throwParseErrorNextToken: throwParseErrorNextToken,
      throwParseErrorEOF: throwParseErrorEOF,
      throwParseErrorUnterminatedString: throwParseErrorUnterminatedString,
      throwParseErrorBadNumber: throwParseErrorBadNumber,
      throwParseErrorBadOper: throwParseErrorBadOper,

      makeRecordFieldsFail: makeRecordFieldsFail,
      makeTupleAnnsFail: makeTupleAnnsFail,
      makeFieldFailure: makeFieldFailure,
      makeAnnFailure: makeAnnFailure,
      makeMissingField: makeMissingField,
      makeTupleLengthMismatch: makeTupleLengthMismatch,
      makeTypeMismatch: makeTypeMismatch,
      makeTupleAnnsFail: makeTupleAnnsFail,
      makeTupleLengthMismatch: makeTupleLengthMismatch,
      makeAnnFailure: makeAnnFailure,
      makeRefInitFail: makeRefInitFail,
      makePredicateFailure: makePredicateFailure,
      makeDotAnnNotPresent: makeDotAnnNotPresent,
      makeFailureAtArg: makeFailureAtArg,
      contractOk: gf(CON, "ok"),
      contractFail: contract("fail"),
      contractFailArg: contract("fail-arg"),
      isOk: isOk,
      isFail: isFail,
      isFailArg: isFailArg,

      equal: gf(EQ, "Equal"),
      notEqual: gf(EQ, "NotEqual"),
      unknown: gf(EQ, "Unknown"),
      isEqual: isEqual,
      isNotEqual: isNotEqual,
      isUnknown: isUnknown,
      isEqualityResult: isEqualityResult,

      makeMessageException: makeMessageException,
      makeUserException: makeUserException,
      makeModuleLoadFailureL: makeModuleLoadFailureL,

      userBreak: gf(ERR, "user-break"),
      isUserBreak: errPred("is-user-break"),
      isExit: errPred("is-exit"),
      isExitQuiet: errPred("is-exit-quiet"),

      errPred: errPred,

      cases: cases,

      checkArity: checkArity,

      /*checkResultsSummary: checkResultsSummary,*/

      makeList: makeList,
      makeTreeSet: makeTreeSet,

      isOption: runtime.getField(O, "is-Option"),
      isNone: function(v) { return runtime.getField(O, "is-none").app(v); },
      isSome: function(v) { return runtime.getField(O, "is-some").app(v); },
      makeNone: function() { return runtime.getField(O, "none"); },
      makeSome: function(v) { return runtime.getField(O, "some").app(v); },

      isEither: runtime.getField(E, "is-Either"),
      isLeft: function(v) { return runtime.getField(E, "is-left").app(v); },
      isRight: function(v) { return runtime.getField(E, "is-right").app(v); },
      makeLeft: function(l) { return runtime.getField(E, "left").app(l); },
      makeRight: function(r) { return runtime.getField(E, "right").app(r); },

      toArray: toArray,
      isList: function(list) { return runtime.unwrap(runtime.getField(L, "is-List").app(list)); },
      isLink : isLink,
      isEmpty : isEmpty,

      isErrorDisplay: isErrorDisplay,
      checkErrorDisplay: checkErrorDisplay,
// NOTE(joe): skipping checker
//      isTestResult: isTestResult,
//      checkTestResult: checkTestResult,
//      isTestSuccess: function(val) { return runtime.unwrap(runtime.getField(CH, "is-success").app(val)); },

      isValueSkeleton: function(v) { return runtime.unwrap(runtime.getField(VS, "is-ValueSkeleton").app(v)); },
      isVSValue: function(v) { return runtime.unwrap(runtime.getField(VS, "is-vs-value").app(v)); },
      isVSTable: function(v) { return runtime.unwrap(runtime.getField(VS, "is-vs-table").app(v)); },
      isVSCollection: function(v) { return runtime.unwrap(runtime.getField(VS, "is-vs-collection").app(v)); },
      isVSConstr: function(v) { return runtime.unwrap(runtime.getField(VS, "is-vs-constr").app(v)); },
      isVSStr: function(v) { return runtime.unwrap(runtime.getField(VS, "is-vs-str").app(v)); },
      isVSSeq: function(v) { return runtime.unwrap(runtime.getField(VS, "is-vs-seq").app(v)); },
      vsStr: function(s) {
        runtime.checkString(s);
        return runtime.getField(VS, "vs-str").app(s);
      },

      edEmbed: function(v) {
        runtime.checkPyretVal(v);
        return runtime.getField(ED, "embed").app(v);
      },

      //TODO(joe): add more creation methods for error-display/valueskeleton
      //here, which are super-useful!

      skeletonValues: function(skel) {
        var isValueSkeleton = runtime.getField(VS, "is-ValueSkeleton");
        var isValue = runtime.getField(VS, "is-vs-value");
        var isTable = runtime.getField(VS, "is-vs-table");
        var isCollection = runtime.getField(VS, "is-vs-collection");
        var isConstr = runtime.getField(VS, "is-vs-constr");
        var isStr = runtime.getField(VS, "is-vs-str");
        var isSeq = runtime.getField(VS, "is-vs-seq");
        if(!(runtime.unwrap(isValueSkeleton.app(skel)) === true)) {
          throwTypeMismatch(skel, "ValueSkeleton");
        }
        var arr = [];
        var worklist = [skel];
        try {
          for (var i = 0; i < worklist.length; i++) { // length changes as the loop iterates
            var cur = worklist[i];
            if (runtime.unwrap(isValue.app(cur)) === true) {
              arr.push(runtime.getField(cur, "v"));
            } else if (runtime.unwrap(isCollection.app(cur)) === true) {
              Array.prototype.push.apply(worklist, toArray(runtime.getField(cur, "items")));
            } else if (runtime.unwrap(isTable.app(cur)) === true) {
              runtime.getField(cur, "rows").forEach(function(row){
                Array.prototype.push.apply(worklist, row); });
            } else if (runtime.unwrap(isConstr.app(cur)) === true) {
              Array.prototype.push.apply(worklist, toArray(runtime.getField(cur, "args")));
            } else if (runtime.unwrap(isStr.app(cur)) === true) {
              // nothing
            } else if (runtime.unwrap(isSeq.app(cur)) === true) {
              Array.prototype.push.apply(worklist, toArray(runtime.getField(cur, "items")));
            } else {
              throwMessageException("Non-value appeared in skeleton: " + String(cur));
            }
          }
        } catch(e) {
          console.error("******* Something went wrong in skeletonValues: " + JSON.stringify(skel));
          console.error(e);
          throw e;
        }
        return arr;
      }
    });
  }
})
