var r = require("requirejs");
var readline = require('readline');

r.config({
  paths: {
    trove: "../../build/phase1/trove",
    js: "../../build/phase1/js",
    compiler: "../../build/phase1/arr/compiler"
  }
});

r(["q", "js/ffi-helpers", "trove/srcloc", "js/repl-lib", "js/runtime-anf", "compiler/compile-structs.arr"], function(Q, ffiLib, srclocLib, repl, runtime, csLib) {
  var rt = runtime.makeRuntime({});
  var rl = readline.createInterface({
    input: process.stdin,
      output: process.stdout
  });
  var ffi = ffiLib(rt, rt.namespace);
  var get = rt.getField;
  var cases = ffi.cases;

  rt.loadModules(rt.namespace, [srclocLib, csLib], function(srcloc, cs) {
    var sb = get(cs, "standard-builtins");
    var rp = repl.create(rt, rt.namespace, sb, { name: "repl-cli", dialect: "Pyret"});
    var res = rp.restartInteractions("");
    rl.setPrompt(">", 0); //make this a constant

    //Note(ben) split at interactions, add number, return
    function drawSrcloc(s) {
      return s ? get(s, "format").app(true) : "";
    }

    function drawCompileErrors(e) {
      //TODO: update draw functions to print relevant info
      function drawUnboundId(idExpr) {
	var name = get(get(idExpr, "id"), "toname").app();
	var loc = get(idExpr, "l");

	cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	  "builtin": function(_) {
	    console.error("Should not be allowed to have a builtin that's unbound", e);
	  },
	  "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	    var msg = "The name `"
	      + name
	      + "` is used but not defined at\n"
	      + drawSrcloc(loc);
	    console.log(msg);
	  }
	});
      }

      //Question(ben) why does this function accept different arguments?
      function drawUnboundVar(id, loc) {
	cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	  "builtin": function(_) {
	    console.error("Should not be allowed to have a builtin that's unbound", e);
	  },
	  "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	    var msg = "The variable `"
	      + id
	      + "` is assigned to, but not defined, at\n"
	      + drawSrcloc(loc);
	    console.log(msg);
	  }
	});
      }

      function drawUnboundTypeId(idExpr) {
	var name = get(get(idExpr, "id"), "toname").app();
	var loc = get(idExpr, "l");

	cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	  "builtin": function(_) {
	    console.error("Should not be allowed to have a builtin that's unbound", e);
	  },
	  "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	    var msg = "The name `"
	      + name
	      + "` is used as a type but not defined as one, at\n"
	      + drawSrcloc(loc);
	    console.log(msg);
	  }
	});
      }

      //Note(ben) it seems that all variables defined with run() are considered, builtins, because the first part of the cases statement is always matched
      function drawShadowId(id, newLoc, oldLoc) {
	cases(get(srcloc, "Srcloc"), "Srcloc", oldLoc, {
	  "builtin": function(_) {
	    var msg = "The name `"
	      + id
	      + "` is already defined. You need to pick a different name for `"
	      + id
	      + "` at\n"
	      + drawSrcloc(newLoc);
	    console.log(msg);
	  },
	  "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	    var msg = "It looks like you defined the name `"
	      + id
	      + "` twice, at\n"
	      + drawSrcloc(oldLoc) + "\n"
	      + drawSrcloc(newLoc) + "\n"
	      + "You need to pick a new name for one of them";
	    console.log(msg);
	  }
	});
      }

      function drawPointlessVar(loc) {
	cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	  "builtin": function(_) {
	    console.error("Should not be possible to have a builtin var that's anonymous", e);
	  },
	  "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	    var msg = "Defining anonymous variables is pointless: you have no name to modify. "
	      + "Either give this expression a name, or bind it to an identifier rather than a variable.\n\n"
	      + drawSrcloc(loc);
	    console.log(msg);
	  }
	});
      }

      function drawPointlessShadow(loc) {
	cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	  "builtin": function(_) {
	    console.error("Should not be possible to have a builtin var that's anonymous", e);
	  },
	  "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	    var msg = "Anonymous identifiers cannot shadow anything: there is no name to shadow. "
	      + "Either give this expression a name, or remove the shadow annotation.\n\n"
	      + drawSrcloc(loc);
	    console.log(msg);
	  }
	});
      }

      function drawPointlessRec(loc) {
	cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
	  "builtin": function(_) {
	    console.error("Should not be possible to have a builtin var that's anonymous", e);
	  },
	  "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
	    var msg = "The anonymous recursive identifier at "
	      + drawSrcloc(loc)
	      + " can never be re-used";
	    console.log(msg);
	  }
	});
      }

      function drawWfError(msg, loc) {
	console.log(msg + "\n" + drawSrcloc(loc));
      }

      function drawWfErrSplit(msg, locs) {
	var s = msg;
	var locArray = ffi.toArray(locs);
	locArray.forEach(function(l) {
	 s += "\n" + drawSrcloc;
	});
	console.log(s)
      }

      function drawReservedName(loc, id) {
	var msg = "Well-formedness: Pyret disallows the use of `"
	  + id
	  + "` as an identifier\n"
	  + drawSrcloc(loc);
      }

      function drawErrorToString(e) {
	return function() {
	  runtime.safeCall(function() {
	      return runtime.toReprJS(e, "tostring");
	    }, function(s) {
	      console.log(s);
	    });
	};
      }

      function drawCompileError(e) {
        cases(get(cs, "CompileError"), "CompileError", e, {
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

      e.forEach(drawCompileError);
    }

    //TODO: see errorsUI for other error draws

    /*TODO: see checksUI for check drawing format
     * Iterate over the check results
     * Keep track of the total number of check tests, and total passed
     * Keep track of number of check blocks errored
     * Save strings for each check block with name and results
     * If all results passed say so, otherwise print accordingly */

    rl.on("line", function(cmd) {
      res = res.then(function(_) {
        return rp.run(cmd);
      });

      res.then(function(new_res) {
        if(rt.isSuccessResult(new_res)) {
          //TODO: turn this into a handler
          //TODO: get the checks field and render checks (should be okay if none)
          console.log(get(new_res.result, "answer"));
        }
        else {
          //TODO: turn this into a handler as well
          var exceptions = new_res.exn.exn;
	  //not necessarly a compiler error, check beginning of doc
          drawCompileErrors(exceptions);
        }

        rl.prompt();
      });
    }).on('close', function() {
      process.exit(0);
    });
    rl.prompt();
  });
})
