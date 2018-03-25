function vhull_runtime() {
  return {
    generateDefs(thisRuntime) {

      var require = requirejs;

      /* @stopify flat */
      function depToString(d) {
        if(d["import-type"] === "builtin") {
          return d["import-type"] + "(" + d.name + ")";
        }
        else if(d["import-type"] === "dependency") {
          return d["protocol"] + "(" + d["args"].join(", ") + ")";
        }
        else {
          throw new Error("Unknown dependency description: ", d);
        }
      }

      function runStandalone(staticMods, realm, depMap, toLoad, postLoadHooks) {
        // Assume that toLoad is in dependency order, so all of their requires
        // are already instantiated
        if(toLoad.length == 0) {
          return {
            "complete": "runStandalone completed successfully" ,
          };
        }
        else {
          var uri = toLoad[0];
          var mod = staticMods[uri];
          var reqs = mod.requires;

          if(depMap[uri] === undefined) {
            throw new Error("Module has no entry in depmap: " + uri);
          }

          var reqInstantiated = reqs.map(/* @stopify flat */ function(d) {
            var name = depToString(d);
            var duri = depMap[uri][name];
            if(duri === undefined) {
              throw new Error(
                `Module not found in depmap: ${name} while loading uri`);
            }
            if(realm[duri] === undefined) {
              throw new Error(
                `Module not loaded yet: ${name} while loading uri`);
            }
            return thisRuntime.getExported(realm[duri]);
          });

          var natives;

          if (mod.nativeRequires.length === 0) {
            natives = mod.nativeRequires;
          }
          else {
            require(mod.nativeRequires,
              /* @stopify flat */ function(/* varargs */) {
              var nativeInstantiated = Array.prototype.slice.call(arguments);
              natives = nativeInstantiated;
            });
          }

          if(!realm[uri]) {
            var result, theModFunction;

            if(typeof mod.theModule === "function") {
              theModFunction = mod.theModule;
              result = theModFunction.apply(null,
                [thisRuntime, thisRuntime.namespace, uri]
                .concat(reqInstantiated)
                .concat(natives));
            } else {
              throw new Error('Module was not in the form of a function: ' + uri)
            }

            realm[uri] = result;

            if(uri in postLoadHooks) {
              postLoadHooks[uri](result);
            }
          }

          return runStandalone(
            staticMods, realm, depMap, toLoad.slice(1), postLoadHooks);
        }
      }

      // NOTE(rachit): stackFrame is not used
      function safeCall(fun, after, stackFrame) {
        return after(fun())
      }

      function eachLoop(fun, start, stop) {
        for (var i = start; i < stop; i++) {
          fun.app(i)
        }
        return thisRuntime.nothing;
      }

      function run(program, namespace, options) {
        return program(thisRuntime, namespace)
      }

      function runThunk(f, then) {
        var fnResult, resultCtor;
        try {
          fnResult = f();
          resultCtor = thisRuntime.makeSuccessResult;
        } catch (e) {
          fnResult = e;
          resultCtor = thisRuntime.makeFailureResult;
        }
        return then(resultCtor(fnResult))
      }

      function execThunk(thunk) {
        if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["run-task"], 1, $a); }
        function wrapResult(res) {
          if(thisRuntime.isSuccessResult(res)) {
            return thisRuntime.ffi.makeLeft(res.result);
          } else if (thisRuntime.isFailureResult(res)) {
            if(thisRuntime.isPyretException(res.exn)) {
              return thisRuntime.ffi.makeRight(thisRuntime.makeOpaque(res.exn));
            }
            else {
              return thisRuntime.ffi.makeRight(thisRuntime.makeOpaque(thisRuntime.makePyretFailException(thisRuntime.ffi.makeMessageException(String(res.exn + "\n" + res.exn.stack)))));
            }
          } else {
            CONSOLE.error("Bad execThunk result: ", res);
            return;
          }
        }
        var result;
        try {
          // thunk is from pyret and can be a pyret callback
          result = thunk.app()
          result = thisRuntime.makeSuccessResult(result, {})
        } catch (e) {
          result = thisRuntime.makeFailureResult(e, {})
        }
        return wrapResult(result)
      }

      var raw_array_build = function(f, len) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-build"], 2, $a); }
        thisRuntime.checkFunction(f);
        thisRuntime.checkNumber(len);
        var curIdx = 0;
        var arr = new Array();
        while (curIdx < len) {
          arr.push(f.app(curIdx));
          curIdx++;
        }
        return arr;
      }

      var raw_array_build_opt = function(f, len) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-build"], 2, $a); }
        thisRuntime.checkFunction(f);
        thisRuntime.checkNumber(len);
        var curIdx = 0;
        var arr = new Array();
        var $ans;
        while (curIdx < len) {
          $ans = f.app(curIdx);
          if (thisRuntime.ffi.isSome($ans)) {
            arr.push(thisRuntime.getField($ans, "value"));
          }
          curIdx++;
        }
        return arr;
      }

      var raw_array_fold = function(f, init, arr, start) {
        if (arguments.length !== 4) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-fold"], 4, $a); }
        thisRuntime.checkFunction(f);
        thisRuntime.checkPyretVal(init);
        thisRuntime.checkArray(arr);
        thisRuntime.checkNumber(start);
        var currentIndex = -1;
        var currentAcc = init;
        var length = arr.length;
        while(currentIndex < (length - 1)) {
          currentIndex += 1;
          var res = f.app(currentAcc, arr[currentIndex], currentIndex + start);
          currentAcc = res;
        }
        return currentAcc;
      };

      var raw_array_map = function(f, arr) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-map"], 2, $a); }
        thisRuntime.checkFunction(f);
        thisRuntime.checkArray(arr);
        var currentIndex = -1;
        var length = arr.length;
        var newArray = new Array(length);
        while(currentIndex < (length - 1)) {
          currentIndex += 1;
          var res = f.app(arr[currentIndex]);
          newArray[currentIndex] = res;
        }
        return newArray;
      };

      var raw_array_each = function(f, arr) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-each"], 2, $a); }
        thisRuntime.checkFunction(f);
        thisRuntime.checkArray(arr);
        var currentIndex = -1;
        var length = arr.length;
        while(currentIndex < (length - 1)) {
          currentIndex += 1;
          f.app(arr[currentIndex]);
        }
        return thisRuntime.nothing;
      };

      var raw_array_mapi = function(f, arr) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-mapi"], 2, $a); }
        thisRuntime.checkFunction(f);
        thisRuntime.checkArray(arr);
        var currentIndex = -1;
        var length = arr.length;
        var newArray = new Array(length);
        while(currentIndex < (length - 1)) {
          currentIndex += 1;
          var res = f.app(arr[currentIndex], currentIndex);
          newArray[currentIndex] = res;
        }
        return newArray;
      };

      var raw_list_map = function(f, lst) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-list-map"], 2, $a); }
        thisRuntime.checkFunction(f);
        thisRuntime.checkList(lst);
        var currentAcc = [];
        var currentLst = lst;
        var currentFst;
        while(thisRuntime.ffi.isLink(currentLst)) {
          currentFst = thisRuntime.getColonField(currentLst, "first");
          currentLst = thisRuntime.getColonField(currentLst, "rest");
          var res = f.app(currentFst);
          currentAcc.push(res);
        }
        return thisRuntime.ffi.makeList(currentAcc);
      };

      /**
       * Similar to `raw_array_map`, but applies a specific function to
       * the first item in the array
       */
      var raw_array_map1 = function(f1, f, arr) {
        if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-map1"], 3, $a); }
        thisRuntime.checkFunction(f1);
        thisRuntime.checkFunction(f);
        thisRuntime.checkArray(arr);
        var currentIndex = -1;
        var length = arr.length;
        var newArray = new Array(length);
        while(currentIndex < (length - 1)) {
          currentIndex += 1;
          var toCall = currentIndex === 0 ? f1 : f;
          var res = toCall.app(arr[currentIndex]);
          newArray[currentIndex] = res;
        }
        return newArray;
      };

      var raw_list_filter = function(f, lst) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-list-filter"], 2, $a); }
        thisRuntime.checkFunction(f);
        thisRuntime.checkList(lst);
        var currentAcc = [];
        var currentLst = lst;
        var currentFst;
        while(thisRuntime.ffi.isLink(currentLst)) {
          currentFst = thisRuntime.getColonField(currentLst, "first");
          currentLst = thisRuntime.getColonField(currentLst, "rest");
          var res = f.app(currentFst);
          if(res) {
            currentAcc.push(currentFst);
          }
        }
        return thisRuntime.ffi.makeList(currentAcc);
      };

      var raw_array_filter = function(f, arr) {
        if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-array-filter"], 2, $a); }
        thisRuntime.checkFunction(f);
        thisRuntime.checkArray(arr);
        var currentIndex = -1;
        var length = arr.length;
        var newArray = new Array();
        while(currentIndex < (length - 1)) {
          currentIndex += 1;
          var res = f.app(arr[currentIndex]);
          if(!(isBoolean(res))) {
            return ffi.throwNonBooleanCondition(
              ["raw-array-filter"], "Boolean", res);
          }
          if(isPyretTrue(res)){
            newArray.push(arr[currentIndex]);
          }
        }
        return newArray;
      };

      var raw_list_fold = function(f, init, lst) {
        if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw thisRuntime.ffi.throwArityErrorC(["raw-list-fold"], 3, $a); }
        thisRuntime.checkFunction(f);
        thisRuntime.checkPyretVal(init);
        thisRuntime.checkList(lst);
        var currentAcc = init;
        var currentLst = lst;
        while(thisRuntime.ffi.isLink(currentLst)) {
          var fst = thisRuntime.getColonField(currentLst, "first");
          currentLst = thisRuntime.getColonField(currentLst, "rest");
          currentAcc = f.app(currentAcc, fst);
        }
        return currentAcc;
      };

      // Export the stopified functions here.
      return {
        /* Run functions */
        "runStandalone": runStandalone,
        "safeCall": safeCall,
        "eachLoop": eachLoop,
        "run": run,
        "runThunk": runThunk,
        "execThunk": execThunk,

        /* Array functions */
        "raw_array_build": raw_array_build,
        "raw_array_build_opt": raw_array_build_opt,
        "raw_array_fold": raw_array_fold,
        "raw_array_map": raw_array_map,
        "raw_array_each": raw_array_each,
        "raw_array_mapi": raw_array_mapi,
        "raw_array_map1": raw_array_map1,
        "raw_array_filter": raw_array_filter,

        /* List functions */
        "raw_list_map": raw_list_map,
        "raw_list_filter": raw_list_filter,
        "raw_list_fold": raw_list_fold
      }
    }
  }
}
