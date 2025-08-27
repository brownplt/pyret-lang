define("pyret-base/js/post-load-hooks", function() {
  function makeDefaultPostLoadHooks(runtime, hookOptions) {
    return {
      "builtin://srcloc": function(srcloc) {
        runtime.srcloc = runtime.getField(runtime.getField(srcloc, "provide-plus-types"), "values");
      },
      "builtin://ffi": function(ffi) {
        ffi = ffi.jsmod;
        runtime.ffi = ffi;
        runtime["throwMessageException"] = ffi.throwMessageException;
        runtime["throwNoBranchesMatched"] = ffi.throwNoBranchesMatched;
        runtime["throwNoCasesMatched"] = ffi.throwNoCasesMatched;
        runtime["throwNonBooleanCondition"] = ffi.throwNonBooleanCondition;
        runtime["throwNonBooleanOp"] = ffi.throwNonBooleanOp;
        runtime["throwUnfinishedTemplate"] = ffi.throwUnfinishedTemplate;
        runtime["throwInvalidTableColumn"] = ffi.throwInvalidTableColumn;
        runtime["toArray"] = ffi.toArray;

        var checkList = runtime.makeCheckType(ffi.isList, "List");
        runtime["checkList"] = checkList;

        runtime["checkEQ"] = runtime.makeCheckType(ffi.isEqualityResult, "EqualityResult");
      },
      "builtin://table": function(table) {
        table = runtime.getField(runtime.getField(table, "provide-plus-types"), "internal");
        runtime["makeTable"] = table.makeTable;
        runtime["makeRow"] = table.makeRow;
        runtime["makeRowFromArray"] = table.makeRowFromArray;
        runtime["openTable"] = table.openTable;
        runtime["checkTable"] = runtime.makeCheckType(table.isTable, "Table");
        runtime["checkRow"] = runtime.makeCheckType(table.isRow, "Row");
        runtime["isTable"] = table.isTable;
        runtime["isRow"] = table.isRow;
        runtime["checkWrapTable"] = function(val) {
          runtime.checkTable(val);
          return val;
        };
        runtime.makePrimAnn("Table", table.isTable);
        runtime.makePrimAnn("Row", table.isRow);
      },
      "builtin://data-source": function(ds) {
        ds = runtime.getField(runtime.getField(ds, "provide-plus-types"), "values");
        // Variadic convenience function for desugaring use.
        // 'type' corresponds to a loader option in `data-source.arr`

        runtime["asLoaderOption"] = function(type) {
          switch(type) {
          case "sanitizer":
            return runtime.getField(ds, "sanitize-col").app(arguments[1], arguments[2]);
          default:
            runtime.ffi.throwMessageException("Internal error: Invalid loader option type: " + type);
          }
        };
        // Convenience function for JS library use
        runtime["extractLoaderOption"] = function(opt) {
          var isSanitizer = runtime.getField(ds, "is-sanitize-col");
          if (runtime.unwrap(isSanitizer.app(opt))) {
            return {
              type: "sanitizer",
              col: runtime.getField(opt, "col"),
              sanitizer: runtime.getField(opt, "sanitizer")
            };
          } else {
            runtime.ffi.throwMessageException("Internal error: Cannot coerce non-loader option");
          }
        }
        runtime["builtin_sanitizers"] = {
          option : runtime.getField(ds, "option-sanitizer"),
          string : runtime.getField(ds, "string-sanitizer"),
          num : runtime.getField(ds, "num-sanitizer"),
          bool: runtime.getField(ds, "bool-sanitizer"),
          strict_num : runtime.getField(ds, "strict-num-sanitizer"),
          strings_only : runtime.getField(ds, "strings-only"),
          numbers_only : runtime.getField(ds, "numbers-only"),
          booleans_only : runtime.getField(ds, "booleans-only"),
          empty_only : runtime.getField(ds, "empty-only")
        };

        function reCache(src, constructorName, target, cachedName) {
          return function() {
            try {
              var constr = runtime.getField(src, constructorName);
              return constr.app.apply(constr, arguments);
            } finally {
              // after jitting, this will be faster than the call above
              target[cachedName] = runtime.getField(src, constructorName).app;
            }
          };
        }
        runtime["makeCStr"] = reCache(ds, "c-str", runtime, "makeCStr");
        runtime["makeCNum"] = reCache(ds, "c-num", runtime, "makeCNum");
        runtime["makeCBool"] = reCache(ds, "c-bool", runtime, "makeCBool");
        runtime["makeCCustom"] = reCache(ds, "c-custom", runtime, "makeCCustom");
        runtime["makeCEmpty"] = function() { return runtime.getField(ds, "c-empty"); };

        runtime["isCStr"] = function(v) { return runtime.unwrap(runtime.getField(ds, "is-c-str").app(v)); };
        runtime["isCNum"] = function(v) { return runtime.unwrap(runtime.getField(ds, "is-c-num").app(v)); };
        runtime["isCBool"] = function(v) { return runtime.unwrap(runtime.getField(ds, "is-c-bool").app(v)); };
        runtime["isCCustom"] = function(v) { return runtime.unwrap(runtime.getField(ds, "is-c-custom").app(v)); };
        runtime["isCEmpty"] = function(v) { return runtime.unwrap(runtime.getField(ds, "is-c-empty").app(v)); };

        runtime["unwrapCellContent"] = function(v) {
          if (runtime.isCStr(v)) {
            return {type: "str", value: runtime.getField(v, "s")};
          } else if (runtime.isCNum(v)) {
            return {type: "num", value: runtime.getField(v, "n")};
          } else if (runtime.isCBool(v)) {
            return {type: "bool", value: runtime.getField(v, "b")};
          } else if (runtime.isCCustom(v)) {
            return {type: "custom", value: runtime.getField(v, "datum")};
          } else if (runtime.isCEmpty(v)) {
            return {type: "empty"};
          } else {
            runtime.ffi.throwMessageException("Internal error: Cannot unwrap non-cell content");
          }
        };

        runtime["makeLoadedTable"] = function(headers, contents) {
          // Headers can either be [name, sanitizer] arrays or
          // {name: name, sanitizer: sanitizer} objects
          headers = headers.map(function(h) {
            if (h.sanitizer) {
              return runtime.makeTuple([h.name, h.sanitizer]);
            } else {
              return runtime.makeTuple(h);
            }
          });
          return runtime.makeTuple([headers, contents]);
        };
        runtime["checkCellContent"] = runtime.makeCheckType(
          runtime.getField(ds, "is-CellContent").app, "CellContent");
      },
      "builtin://reactors": function(reactor) {
        var r = runtime.getField(runtime.getField(reactor, "provide-plus-types"), "values");
        runtime.setParam("makeReactor", runtime.getField(r, "make-reactor").app);
      },
      "builtin://checker": function(checker) {
        var checker = runtime.getField(runtime.getField(checker, "provide-plus-types"), "values");
        const checks = hookOptions.checks || "main";
        var currentChecker = runtime.getField(checker, "make-check-context").app(runtime.makeString(hookOptions.main),
                                                                                 checks);
        runtime.setParam("current-checker", currentChecker);
      }
    };
  }
  return {
    makeDefaultPostLoadHooks: makeDefaultPostLoadHooks
  };
});
