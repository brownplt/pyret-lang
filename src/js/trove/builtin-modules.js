define(["js/runtime-util", "js/type-util"], function(util, t) {
  return util.definePyretModule("builtin-modules",
    [],
    [],
    {},
    function(runtime, ns) {
      var F = runtime.makeFunction;
      function getBuiltinLocator(name) {
        runtime.pauseStack(function(restarter) {
          // NOTE(joe): This is a bit of requireJS hackery that assumes a
          // certain layout for builtin modules
          if (name === undefined) {
            console.error("Got undefined name in builtin locator");
            console.trace();
          }
          require(["trove/" + name], function(m) {
            restarter.resume(runtime.makeObject({
              "get-raw-dependencies":
                F(function() {
                  if(m.dependencies) {
                    return m.dependencies.map(function(m) {
                      // NOTE(joe): This allows us to use builtin imports
                      // without a bootstrap for the compiler re-inserting
                      // import-type fields
                      if(!m["import-type"]) {
                        m["import-type"] = "dependency";
                      }
                      return runtime.makeObject(m);
                    });
                  } else {
                    return [];
                  }
                }),
              "get-raw-datatype-provides":
                F(function() {
                  if(m.provides && m.provides.datatypes) {
                    if(Array.isArray(m.provides.datatypes)) {
                      return m.provides.datatypes;
                    }
                    else if(typeof m.provides === "object") {
                      return Object.keys(m.provides.datatypes).map(function(k) {
                        return runtime.makeObject({
                          name: k,
                          typ: t.toPyret(runtime, m.provides.datatypes[k])
                        });
                      });
                    }
                  }
                  else {
                    return [];
                  }
                }),
              "get-raw-alias-provides":
                F(function() {
                  if(m.provides) {
                    if(Array.isArray(m.provides.types)) {
                      return m.provides.types;
                    }
                    else if(typeof m.provides.aliases === "object") {
                      return Object.keys(m.provides.aliases).map(function(k) {
                        return runtime.makeObject({
                          name: k,
                          typ: t.toPyret(runtime, m.provides.aliases[k])
                        });
                      });
                    }
                  }
                  else {
                    return [];
                  }
                }),
              "get-raw-value-provides":
                F(function() {
                  if(m.provides) {
                    if(Array.isArray(m.provides.values)) {
                      return m.provides.values;
                    }
                    else if(typeof m.provides === "object") {
                      return Object.keys(m.provides.values).map(function(k) {
                        return runtime.makeObject({
                          name: k,
                          typ: t.toPyret(runtime, m.provides.values[k])
                        });
                      });
                    }
                  }
                  else {
                    return [];
                  }
                }),
              "get-raw-compiled":
                F(function() {
  //                if(m.oldDependencies) {
  //                   m.theModule = m.theModule.apply(null, m.oldDependencies); 
  //                   return runtime.makeObject(function() { return m.theModule });
  //                }
  //                else
                  if(m.theModule) {
                    // NOTE(joe): this will b removed once polyglot is done
                    return runtime.makeOpaque(m.theModule);
                  }
                  else {
                    return runtime.makeOpaque(function() { return m; });
                  }
                })
            }));
          });

        });
      }
      var O = runtime.makeObject;
      return O({
        "provide-plus-types": O({
          types: { },
          values: O({
            "builtin-raw-locator": runtime.makeFunction(getBuiltinLocator)
          })
        }),
        "answer": runtime.nothing
      });
    });
});
