/*global define */
/*jslint unparam: true, todo: true, node: true*/

var r = require("requirejs");
var readlineLib = require("readline");

r.config({
  paths: {
    trove: "../../build/phase1/trove",
    js: "../../build/phase1/js",
    compiler: "../../build/phase1/arr/compiler"
  }
});

r(["q", "js/repl-lib", "js/runtime-anf", "compiler/compile-structs.arr", "./error-ui", "./check-ui"], function(Q, replLib, runtimeLib, csLib, errorUI, checkUI) {
  var runtime = runtimeLib.makeRuntime({});
  var readline = readlineLib.createInterface({
    input: process.stdin,
      output: process.stdout
  });
  var get = runtime.getField;

  function renderValue(val) {
    if(runtime.isPyretVal(val)) {
      return runtime.toReprJS(val, "_torepr");
    }
    return String(val);
  }

  runtime.loadModules(runtime.namespace, [csLib], function(cs) {
    var sb = get(cs, "standard-builtins");
    var repl = replLib.create(runtime, runtime.namespace, sb, { name: "repl-cli", dialect: "Pyret"});
    var res = repl.restartInteractions("");
    var interactionsNo = 1;
    var cmdQueue = [];
    var nestStack = [];

    readline.setPrompt("1 :: >> ", 0);

    readline.on("line", function(cmd) {
      readline.pause();

      var cmdTrimmed = cmd.trim();
      var cmdLen = cmdTrimmed.length;
      var newCmd = cmdTrimmed;
      //Have to account for then, otherwise, and else
      if(cmdLen > 2) {
	if(cmdTrimmed == "end") {
	  if(nestStack.length === 1) {
	    cmdQueue.push(cmdTrimmed);
	    nestStack.shift();
	    newCmd = cmdQueue.join("\n");
	  }
	  else if (nestStack.length > 1) {
	    cmdQueue.push(cmdTrimmed);
	    nestStack.shift();
	    readline.prompt();
	    return;
	  }
	}
	else if(cmdTrimmed.substring(cmdLen - 2, cmdLen) == "=>") {
	  cmdQueue.push(cmdTrimmed);

	  if(cmdTrimmed.substring(0, 1) != "|") {
	    nestStack.unshift("=>");
	  }
	  readline.prompt();
	  return;
	}
	else if (cmdTrimmed.substring(cmdLen - 1, cmdLen) == ":") {
	  cmdQueue.push(cmdTrimmed);

	  if(cmdTrimmed.substring(0, 1) != "|" && cmdTrimmed.substring(0, 4) != "else") {
	    nestStack.unshift(":");
	  }
	  readline.prompt();
	  return;
	}
      }

      if(nestStack.length > 0) {
	cmdQueue.push(cmdTrimmed);
	readline.prompt();
	return;
      }

      cmdQueue = [];

      res = res.then(function(_) {
	//Note(ben) add a name to increase interactions count
	readline.setPrompt((interactionsNo + 1) + " :: >> ", 0);
        return repl.run(newCmd, "interactions::" + interactionsNo++);
      });

      res.then(function(new_res) {
        if(runtime.isSuccessResult(new_res)) {
          //TODO: turn this into a handler
	  console.log(renderValue(get(new_res.result, "answer")));
	  console.log(checkUI.drawCheckResults(runtime, get(new_res.result, "checks")));
        }
        else {
	  var exception = new_res.exn;
	  console.log(errorUI.drawError(runtime, exception));
        }

        readline.prompt();
      });
    }).on('close', function() {
      process.exit(0);
    });

    readline.prompt();
  });
});
