define(["js/runtime-util", "js/type-util"], function(util, t) {
  return util.definePyretModule("builtin-modules",
    [],
    [],
    {},
    function(RUNTIME, ns) {
      var F = RUNTIME.makeFunction;
      function getBuiltinLocator(name) {
        RUNTIME.pauseStack(function(restarter) {
          // NOTE(joe): This is a bit of requireJS hackery that assumes a
          // certain layout for builtin modules
          if (name === undefined) {
            console.error("Got undefined name in builtin locator");
            console.trace();
          }
          require(["trove/" + name], function(m) {
            restarter.resume(RUNTIME.makeObject({
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
                      return RUNTIME.makeObject(m);
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
                    else if(typeof m.provides.datatypes === "object") {
                      return Object.keys(m.provides.datatypes).map(function(k) {
                        return RUNTIME.makeObject({
                          name: k,
                          typ: t.toPyret(RUNTIME, m.provides.datatypes[k])
                        });
                      });
                    }
                  }
                  return [];
                }),
              "get-raw-alias-provides":
                F(function() {
                  if(m.provides) {
                    if(Array.isArray(m.provides.types)) {
                      return m.provides.types;
                    }
                    else if(typeof m.provides.aliases === "object") {
                      return Object.keys(m.provides.aliases).map(function(k) {
                        return RUNTIME.makeObject({
                          name: k,
                          typ: t.toPyret(RUNTIME, m.provides.aliases[k])
                        });
                      });
                    }
                  }
                  return [];
                }),
              "get-raw-value-provides":
                F(function() {
                  if(m.provides) {
                    if(Array.isArray(m.provides.values)) {
                      return m.provides.values;
                    }
                    else if(typeof m.provides.values === "object") {
                      return Object.keys(m.provides.values).map(function(k) {
                        return RUNTIME.makeObject({
                          name: k,
                          typ: t.toPyret(RUNTIME, m.provides.values[k])
                        });
                      });
                    }
                  }
                  return [];
                }),
              "get-raw-compiled":
                F(function() {
  //                if(m.oldDependencies) {
  //                   m.theModule = m.theModule.apply(null, m.oldDependencies); 
  //                   return RUNTIME.makeObject(function() { return m.theModule });
  //                }
  //                else
                  if(m.theModule) {
                    // NOTE(joe): this will b removed once polyglot is done
                    return RUNTIME.makeOpaque(m.theModule);
                  }
                  else {
                    return RUNTIME.makeOpaque(function() { return m; });
                  }
                })
            }));
          });

        });
      }
      var O = RUNTIME.makeObject;
      return O({
        "provide-plus-types": O({
          types: { },
          values: O({
            "builtin-raw-locator": RUNTIME.makeFunction(getBuiltinLocator)
          })
        }),
        "answer": RUNTIME.nothing
      });
    });
});
