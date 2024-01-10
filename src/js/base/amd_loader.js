var define, requirejs;

// NOTE(joe): this is defined for use in dependency bundles.  Even though it
// isn't used in this file, it's an important definition
if(typeof require === "function") {
  var nodeRequire = require;
}

(function () {
    var isArray = function(arg) {
        return Object.prototype.toString.call(arg) === '[object Array]';
    };

    var nodeRequire = (typeof require != "undefined") ? require : null;

    function isAvailableNodeModule(name) {
      try {
        require.resolve(name);
        isNodeModuleAvailable = true;
      }
      catch(e) {
        isNodeModuleAvailable = false;
      }
      return isNodeModuleAvailable;
    }


    // module name -> module object
    var moduleTable = {};

    function moduleResolved(modName) {
        return (modName in moduleTable) && (moduleTable[modName].resolved);
    }

    function buildCallbackArgs(deps) {
        return deps.map(function(depName) {
            if (moduleResolved(depName)) {
                return moduleTable[depName].val;
            } else {
                throw new Error("Module " + depName + " not yet resolved");
            }
        });
    }

    function evaluateModuleFn(moduleObj) {

        if (moduleObj.val != null) {
            throw new Error("Already evaluated " + moduleObj.name);
        }

        if(isAvailableNodeModule(moduleObj.name)) {
          moduleObj.val = require(moduleObj.name);
          moduleObj.resolved = true;
          return;
        }

        var callbackArgs = buildCallbackArgs(moduleObj.deps);

        // NOTE(joe): This is independent of Pyret code running, so calling
        // this function doesn't interact with stack management in any way
        moduleObj.val = moduleObj.callback.apply(null, callbackArgs);
        moduleObj.resolved = true; }

    define = function(name, deps, callback) {
        //This module might have no dependencies
        if (!isArray(deps)) {
            callback = deps;
            deps = null;
        }

        if (callback === undefined) {
            throw new Error("no callback for " + name);
        }

        if (name in moduleTable) {
            throw new Error("Module " + name + " already defined");
        }

        moduleTable[name] = {
            callback: callback,
            name: name,
            deps: deps != null ? deps : [],
            val: null,

            // Since module functions might return weird values like undefined
            // we separately keep track of whether we've resolved it or not.
            resolved: false,
        };
    };

    function getLoadOrder(deps) {
        // Do DFS + topsort in one pass over the graph without destroying it
        var topsorted = [];

        var toVisit = {}; // Nodes we haven't touched
        var currentlyVisitedNodes = {}; // Nodes we're currently working on (used to find cycles)
        var visitedNodes = {}; // Nodes we're done with

        function visitNode(node) {
            if (isAvailableNodeModule(node)) {
              topsorted.push(node);
              visitedNodes[node] = true;
              delete toVisit[node];
              return;
            }

            if (!(node in moduleTable)) {
                throw new Error("Unknown module : " + node);
            }
            if (node in currentlyVisitedNodes) {
                throw new Error("We have a cycle, which includes " + node);
            }
            if (node in visitedNodes) {
                // already visited it
                return;
            }

            // It's unvisited, so mark it as currently being visited
            currentlyVisitedNodes[node] = true;
            if (node in toVisit) {
                delete toVisit[node];
            }

            // Now visit its children
            for (var i = 0; i < moduleTable[node].deps.length; i++) {
                var child = moduleTable[node].deps[i];
                visitNode(child);
            }

            // Now all of node's dependencies have been added to the list
            // so we're safe to add it
            topsorted.push(node);
            delete currentlyVisitedNodes[node];
            visitedNodes[node] = true;
        };

        for (var i = 0; i < deps.length; i++) {
            toVisit[deps[i]] = true;
        }

        while (Object.keys(toVisit).length > 0) {
            var node = Object.keys(toVisit)[0];
            visitNode(node);
        }

        return topsorted;
    }

    requirejs = function(deps, callback) {
        if (typeof deps === 'string') {
            // Trying to require("somemodule"). It needs to be in our table, or
            // we have to be in node with a synchronous require() function
            if ((deps in moduleTable) && moduleTable[deps].val) {
                return moduleTable[deps].val;
            }

            if (nodeRequire) {
                return require(deps);
            }

            // It's not in our table and we don't have require() so it's unknown.
            throw new Error("Unresolved module " + deps + "...use require([], callback)");
        }

        var loadOrder = getLoadOrder(deps);

        for (var i = 0; i < loadOrder.length; i++) {
            var modName = loadOrder[i];
            if (!moduleTable[modName] && isAvailableNodeModule(modName)) {
              moduleTable[modName] = {
                callback: null,
                name: modName,
                deps: [],
                val: null,
                resolved: false,
              };
            }
            if (!moduleTable[modName].resolved) {
                evaluateModuleFn(moduleTable[modName]);
            }
        }

        var callbackArgs = buildCallbackArgs(deps);
        callback.apply(null, callbackArgs);
    };
}());
