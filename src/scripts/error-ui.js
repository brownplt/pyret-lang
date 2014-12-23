define(["js/ffi-helpers", "trove/srcloc", "trove/error", "trove/contracts", "compiler/compile-structs.arr"], function(ffiLib, srclocLib, errorLib, contractsLib, csLib) {
  function drawError(runtime, exception) {
    var ffi = ffiLib(runtime, runtime.namespace);
    var cases = ffi.cases;
    var get = runtime.getField;

    runtime.loadModules(runtime.namespace, [srclocLib, errorLib, contractsLib, csLib], function(srcloc, error, contracts, cs) {
      function makePred(ns, funName) {
	return get(ns, funName).app;
      }

      var isSrcloc = function(s) {
	return runtime.unwrap(get(srcloc, "is-srcloc").app(s));
      };
      var isCompileError = makePred(cs, "CompileError");
      var isContractError = makePred(contracts, "ContractResult");
      var isParseError = makePred(error, "ParseError");
      var isRuntimeError = makePred(error, "RuntimeError");

      //TODO: split at interactions, add number, return
      function drawSrcloc(s) {
	return s ? get(s, "format").app(true) : "";
      }

      function drawCompileErrors(e) {
	function drawUnboundId(idExpr) {
	  var name = get(get(idExpr, "id"), "toname").app();
	  var loc = get(idExpr, "l");

	  return cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	    "builtin": function(_) {
	      return "Should not be allowed to have a builtin that's unbound"
		+ String(e);
	    },
	    "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	      return "The name `"
		+ name
		+ "` is used but not defined at\n"
		+ drawSrcloc(loc);
	    }
	  });
	}

	//Question(ben) why does this function accept different arguments?
	function drawUnboundVar(id, loc) {
	  return cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	    "builtin": function(_) {
	      return "Should not be allowed to have a builtin that's unbound"
		+ String(e);
	    },
	    "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	      return "The variable `"
		+ id
		+ "` is assigned to, but not defined, at\n"
		+ drawSrcloc(loc);
	    }
	  });
	}

	function drawUnboundTypeId(idExpr) {
	  var name = get(get(idExpr, "id"), "toname").app();
	  var loc = get(idExpr, "l");

	  return cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	    "builtin": function(_) {
	      return "Should not be allowed to have a builtin that's unbound"
		+ String(e);
	    },
	    "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	      return "The name `"
		+ name
		+ "` is used as a type but not defined as one, at\n"
		+ drawSrcloc(loc);
	    }
	  });
	}

	function drawShadowId(id, newLoc, oldLoc) {
	  return cases(get(srcloc, "Srcloc"), "Srcloc", oldLoc, {
	    "builtin": function(_) {
	      return "The name `"
		+ id
		+ "` is already defined. You need to pick a different name for `"
		+ id
		+ "` at\n"
		+ drawSrcloc(newLoc);
	    },
	    //NOTE(ben) this is unecessary
	    "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	      return "It looks like you defined the name `"
		+ id
		+ "` twice, at\n"
		+ drawSrcloc(oldLoc) + "\n"
		+ drawSrcloc(newLoc) + "\n"
		+ "You need to pick a new name for one of them";
	    }
	  });
	}

	function drawPointlessVar(loc) {
	  return cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	    "builtin": function(_) {
	      return "Should not be possible to have a builtin var that's anonymous"
		+ String(e);
	    },
	    "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	      return "Defining anonymous variables is pointless: you have no name to modify. "
		+ "Either give this expression a name, or bind it to an identifier rather than a variable.\n\n"
		+ drawSrcloc(loc);
	    }
	  });
	}

	function drawPointlessShadow(loc) {
	  return cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	    "builtin": function(_) {
	      return "Should not be possible to have a builtin var that's anonymous"
		+ String(e);
	    },
	    "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	      return "Anonymous identifiers cannot shadow anything: there is no name to shadow. "
		+ "Either give this expression a name, or remove the shadow annotation.\n\n"
		+ drawSrcloc(loc);
	    }
	  });
	}

	function drawPointlessRec(loc) {
	  return cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	    "builtin": function(_) {
	      return "Should not be possible to have a builtin var that's anonymous"
		+ String(e);
	    },
	    "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	      return "The anonymous recursive identifier at "
		+ drawSrcloc(loc)
		+ " can never be re-used";
	    }
	  });
	}

	function drawWfError(msg, loc) {
	  return msg + "\n" + drawSrcloc(loc);
	}

	function drawWfErrSplit(msg, locs) {
	  var s = msg;
	  var locArray = ffi.toArray(locs);
	  locArray.forEach(function(l) {
	   s += "\n" + drawSrcloc(l);
	  });
	  return s;
	}

	function drawReservedName(loc, id) {
	  return "Well-formedness: Pyret disallows the use of `"
	    + id
	    + "` as an identifier\n"
	    + drawSrcloc(loc);
	}

	function drawErrorToString(e) {
	  return function() {
	    runtime.safeCall(function() {
		return runtime.toReprJS(e, "tostring");
	      }, function(s) {
		return s;
	      });
	  };
	}

	function drawCompileError(e) {
	  return cases(get(cs, "CompileError"), "CompileError", e, {
	    "unbound-id": drawUnboundId,
	    "unbound-var": drawUnboundVar,
	    "shadow-id": drawShadowId,
	    "duplicate-id": drawShadowId,
	    "duplicate-field": drawShadowId,
	    "pointless-var": drawPointlessVar,
	    "pointless-shadow": drawPointlessShadow,
	    "pointless-rec": drawPointlessRec,
	    "wf-err": drawWfError,
	    "wf-err-split": drawWfErrSplit,
	    "reserved-name": drawReservedName,
	    "else": drawErrorToString(e)
	  });
	}

	if(e.length  !== 1) {
	  var msg = "";
	  e.forEach(function(e) {
	    msg += drawCompileError(e) + "\n\n";
	  });
	  return msg;
	}
	else {
	  return drawCompileError(e[0]);
	}
      }

      function drawExpandableStackTrace(e) {
        var srclocStack = e.pyretStack.map(runtime.makeSrcloc);
        var userLocs = srclocStack.filter(function(l) {
	  return l && isSrcloc(l);
	});
        if(userLocs.length > 0) {
	  var msg = "Stack trace:";
          userLocs.forEach(function(ul) {
	    msg += "\n" + drawSrcloc(ul);
          });
	  return msg;
        }
	else {
	  return "";
        }
      }

      function getLastUserLocation(e, ix) {
        var srclocStack = e.pyretStack.map(runtime.makeSrcloc);
        var userLocs = srclocStack.filter(function(l) {
          if(!(l && isSrcloc(l))) { return false; }
          var source = runtime.getField(l, "source");
          return (source === "definitions"
	    || source.indexOf("interactions") !== -1
	    || source.indexOf("gdrive") !== -1);
        });

        var probablyErrorLocation = userLocs[ix];
        return probablyErrorLocation;
      }

      function drawPyretException(e) {
	function drawRuntimeError(e) {
	  return function() {
	    if(runtime.isPyretVal(e.exn)) {
	      return runtime.toReprJS(e.exn, "_torepr");
	    }
	    else {
	      return String(e.exn);
	    }
	  }
	}

	function drawGenericTypeMismatch(value, type) {
          // TODO(joe): How to improve this search?
          var probablyErrorLocation = getLastUserLocation(e, 0);
	  return "Expcted to get a "
	    + type
	    + " as an argument, but got this instead:\n"
	    + String(value)
	    + "\nat\n"
	    + drawSrcloc(probablyErrorLocation);
        }

	function drawCasesArityMismatch(branchLoc, numArgs, actualArity) {
          var loc = drawSrcloc(branchLoc);
	  return "The cases branch at\n"
	    + loc
	    + "\nshould have only " + actualArity
	    + " arguments, but there are " + numArgs;
        }

	function drawCasesSingletonMismatch(branchLoc, shouldBeSingleton) {
          var loc = drawSrcloc(branchLoc);
	  var msg = "The cases branch at\n" + loc;
          if(shouldBeSingleton) {
	    msg += "\nhas an argument list, but the variant is a singleton";
          }
	  else {
	    msg += "\ndoesn't have an argument list, but the variant is not a singleton";
          }
	  return msg;
        }

        function drawArityMismatch(funLoc, arity, args) {
          argsList = ffi.toArray(args);
          var probablyErrorLocation = getLastUserLocation(e, 0);
	  var argsText = "";
	  argsList.forEach(function(a) {
	    argsText += "\n" + runtime.toReprJS(a, "_torepr");
	  });
          return cases(get(srcloc, "Srcloc"), "Srcloc", funLoc, {
            "srcloc": function() {
              var caller = drawSrcloc(probablyErrorLocation);
              var callee = drawSrcloc(funLoc);
              return "Expected to get "
		+ arity
		+ " arguments when calling the function at\n"
		+ callee
		+ "\nfrom\n"
		+ caller
		+ "\nbut got these "
		+ argsList.length + " arguments: "
		+ argsText;
            },
            "builtin": function(name) {
              var caller = drawSrcloc(probablyErrorLocation);
              return "Expected to get "
		+ arity
		+ " at\n"
		+ caller
		+ "\nbut got these "
		+ args.length + " arguments: "
		+ argsText;
	    }
          });
        }

        function drawPyretRuntimeError() {
          return cases(get(error, "RuntimeError"), "RuntimeError", e.exn, {
	    /*
	    "message-exception": drawMessageException,
	    "uninitialized-id": drawUninitializedId,
	    "no-branches-matched": drawNoBranchesMatched,
	    "no-cases-matched": drawNoCasesMatched,
	    "field-not-found": drawFieldNotFound,
	    "lookup-non-object": drawLookupNonObject,
	    "extend-non-object": drawExtendNonObject,
	    */
	    "generic-type-mismatch": drawGenericTypeMismatch,
	    "arity-mismatch": drawArityMismatch,
	    "cases-arity-mismatch": drawCasesArityMismatch,
	    "cases-singleton-mismatch": drawCasesSingletonMismatch,
	    /*
	    "plus-error": drawPlusError,
	    "numeric-binop-error": drawNumericBinopError,
	    "non-boolean-condition": drawNonBooleanCondition,
	    "non-boolean-op": drawNonBooleanOp,
	    "non-function-app": drawNonFunctionApp,
	    "module-load-failure": drawModuleLoadFailure,
	    "invalid-array-index": drawInvalidArrayIndex,
	    "user-break": drawUserBreak,
	    */
	    "else": drawRuntimeError(e)
	  });
	}

	if(isRuntimeError(e.exn)) {
	  return drawPyretRuntimeError();
	}
	else {
	  return "Sorry :(";
	}
      }

      function drawUnknownException(e) {
	return "An unexpected error occurred: " + String(e);
      }

      //TODO: Change rendering strategies
      if(exception instanceof Array) {
	console.log(drawCompileErrors(exception));
      }

      if(exception.exn instanceof Array) {
	console.log(drawCompileErrors(exception.exn));
      }
      else if(runtime.isPyretException(exception)) {
	console.log(drawPyretException(exception));
      }
      else {
	console.log("");
      }
      /*
      else {
	drawUnknownException(exception);
      }
      */
    });
  }

  return {
    drawError: drawError
  }
});
