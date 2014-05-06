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

R(["js/runtime-anf", "fs"], function(RT, fs) {
  var rt = RT.makeRuntime({
    initialGas: 500,
    stdout: function(str) { process.stdout.write(str); },
    stderr: function(str) { process.stderr.write(str); }
  });
  var modName = trimPath(process.argv[2]);
  console.log(";; Processing: " + process.argv[2]);
  function processPyretModule(module) {
    console.log("(pyret-module " + JSON.stringify(modName));
    var fields = rt.getFields(module);
    fields.forEach(function(name) {
      var field = rt.getField(module, name);
      if (rt.isFunction(field)) {
        console.log("  (fun-spec " + name + " (arity " + field.arity + "))");
      } else {
        console.log("  (unknown-item " + name + ")");
      }
    });
    console.log(")");
  }
  function processRawModule(module) {
    console.log("(raw-module " + JSON.stringify(modName));
    for (var name in module) {
      var field = module[name];
      if (field instanceof Function) {
        console.log("  (fun-spec " + name + " (arity " + field.length + "))");
      } else {
        console.log("  (unknown-item " + name + ")");
      }
    }
    console.log(")");
  }
  R([modName], function(moduleLib) {
    if (moduleLib instanceof Function) {
      try {
        rt.loadModules(rt.namespace, [moduleLib], processPyretModule);
      } catch(e) {
        var module = moduleLib(rt, rt.namespace);
        processRawModule(module);
      }
    } else {
      processRawModule(moduleLib);
    }
  });
});
