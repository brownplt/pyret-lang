const R = require("requirejs");

var paths = {
  trove: "../build/phase1/trove",
  js: "../build/phase1/js",
  compiler: "../build/phase1/arr/compiler"
}

R.config({ paths: paths });

function trimPath(path) {
  for (var p in paths) {
    var i = path.indexOf(p);
    if (i >= 0) return path.slice(i).replace("\\", "/", "g").replace(".js", "");
  }
  return path.replace("\\", "/", "g").replace(".js", "");
}

function importPath(path) {
  var paths = {
    trove: "",
    js: "js/", 
    base: "",
    compiler: "compiler/"
  };
  var ret = path.replace(new RegExp("\\\\", "g"), "/").replace(".js", "");
  for (var prefix in paths) {
    var idx = ret.indexOf(prefix);
    if (idx >= 0) {
      ret = ret.slice(idx).replace(prefix + "/", paths[prefix]);
      if (paths[prefix].indexOf("/") >= 0) {
        ret = '"' + ret + '"';
      }
    }
  }
  return ret;
}

R(["js/runtime-anf", "fs"], function(RT, fs) {
  var rt = RT.makeRuntime({
    initialGas: 500,
    stdout: function(str) { process.stdout.write(str); },
    stderr: function(str) { process.stderr.write(str); }
  });
  var modName = trimPath(process.argv[2]);
  var importName = importPath(process.argv[2]);
  function processPyretModule(module) {
    console.log("#lang scribble/base");
    console.log("@(require \"../scribble-api.rkt\")");
    console.log("@docmodule[" + JSON.stringify(importName) + "]{");
    console.log("  @section{Functions}");
    var fields = rt.getFields(module);
    fields.forEach(function(name) {
      var field = rt.getField(module, name);
      if (!rt.isFunction(field)) {
        console.log("  @ignore[(list \"" + name + "\")]");
      }
    });
    fields.forEach(function(name) {
      var field = rt.getField(module, name);
      if (rt.isFunction(field)) {
        var fun_str = field.app.toString().split("\n").join(" ");
        var args = fun_str.substring(fun_str.indexOf("(") + 1, fun_str.indexOf(")")).trim().split(",");
        console.log("  @function[" + JSON.stringify(name))
        if (args.length == 1 && args[0] == "")
          console.log("            #:args (list)]");
        else
          console.log("            #:args (list " 
                      + args.map(function(a) { return JSON.stringify(a.trim()); }).join(" ") + ")]");
      }
    });
    console.log("}");
  }
  function processRawModule(module) {
    console.log("(raw-module " + JSON.stringify(modName));
    for (var name in module) {
      var field = module[name];
      if (field instanceof Function) {
        console.log("  (fun-spec (name \"" + name + "\") (arity " + field.length + "))");
      } else {
        console.log("  (unknown-item (name \"" + name + "\"))");
      }
    }
    console.log(")");
  }
  R([modName], function(moduleLib) {
    if (moduleLib instanceof Function) {
      rt.runThunk(function() {
        rt.loadModules(rt.namespace, [moduleLib], processPyretModule);
      }, function(result) {
        //var module = moduleLib(rt, rt.namespace);
        //processRawModule(module);
      });
    } else {
      //processRawModule(moduleLib);
    }
  });
});
