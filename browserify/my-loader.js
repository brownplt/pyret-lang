var define, myrequire;
(function () {
    var isArray = function(arg) {
        return Object.prototype.toString.call(arg) === '[object Array]';
    };

    // dependency name -> modules waiting for that dependency to be loaded
    var dependeeTable = {};

    // module name -> module object
    var moduleTable = {};

    function getModuleVal(depName) {
        return (depName in moduleTable) ? moduleTable[depName].val : null;
    }

    function evaluateModuleFn(moduleObj) {
        console.log("Evaluating " + moduleObj.name);
        var callbackArgs = [];
        for (var i = 0; i < moduleObj.deps.length; i++) {
            var depName = moduleObj.deps[i];
            var depVal = getModuleVal(depName);
            if (depVal) {
                callbackArgs.push(depVal);
            } else {
                throw new Error("Module " + depName + " not yet resolved");
            }
        }

        // Now call it
        moduleObj.val = moduleObj.callback.apply(null, callbackArgs);

        // Evaluate any modules which were waiting on this one
        if (moduleObj.name in dependeeTable) {
            for (var i = 0; i < dependeeTable[moduleObj.name].length; i++) {
                var dependee = dependeeTable[moduleObj.name][i];
                dependee.unresolvedDependencies -= 1;
                if (dependee.unresolvedDependencies < 0) {
                    throw new Error ("unresolved dependency invalid value");
                } else if (dependee.unresolvedDependencies === 0) {
                    evaluateModuleFn(dependee);
                }
            }
        }
    }

    define = function(name, deps, callback) {
        //This module may not have dependencies
        if (!isArray(deps)) {
            callback = deps;
            deps = null;
        }

        console.log("define(" + name + ", [" + deps + "])");
        if (name in moduleTable) {
            throw new Error("Module " + name + " already defined");
        }

        moduleTable[name] = {
            callback: callback,
            name: name,
            deps: deps,
            val: null,
            unresolvedDependencies: 0,
        };

        var visitedDeps = {};
        for (var i = 0; i < deps.length; i++) {
            var depName = deps[i];
            if (depName in visitedDeps) {
                // The same dependency repeated twice in the list. skip it
                continue;
            }
            
            if (!(depName in dependeeTable)) {
                dependeeTable[depName] = [moduleTable[name]];
            } else {
                dependeeTable[depName].push(moduleTable[name]);
            }
            moduleTable[name].unresolvedDependencies += 1;
            visitedDeps[depName] = true;
        }

        console.log("dependee table is\n" + JSON.stringify(dependeeTable, null, 4));
    };

    function getLoadOrder(deps) {
        // Do topsort on the "reverse dependency graph" then flip it at the end
        var topsorted = [];
        var visited = {};
        var queue = {};

        for (var i = 0; i < queue.length; i++) {
            var modName = queue[i];
            if (modName in visited) {
                continue;
            }

            if (!(modName in moduleTable)) {
                throw new Error("Unknown module : " + depName);
            }

            for (var j = 0; j < moduleTable[modName].deps.length; j++) {
                var depName = moduleTable[modName].deps[j];
                queue.push(depName);
            }

            if (dependeeTable[modName].length == 0) {
                topsorted.push(modName);
            }

            visited[modName] = true;
        }

        return topsorted.reverse();
    }
      

    myrequire = function(deps, callback) {
        var loadOrder = getLoadOrder(deps);
        console.log("The load order is ...\n" + JSON.stringify(loadOrder, null, 2));
    };
}());


define("a", ["b", "c"],
       function (bval, cval) {
           console.log("bval is " + bval);
           console.log("cval is " + cval);
       });
define("b", ["c"], function (cval) {
    console.log(cval);
    return "bval";
});
define("c", [], function () {return "mc";});
myrequire(["a", "c"], function () {console.log("in main!");});
