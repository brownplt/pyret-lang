({
  requires: [],
  nativeRequires: [
    "fs",
    "pyret-base/js/secure-loader",
    "pyret-base/js/type-util"
  ],
  provides: {},
  theModule: function(RUNTIME, ns, uri, fs, loader, t) {
    var F = RUNTIME.makeFunction;
    function getBuiltinLocator(path) {
      // NOTE(joe): This is a bit of requireJS hackery that assumes a
      // certain layout for builtin modules
      if (path === undefined) {
        console.error("Got undefined name in builtin locator");
        console.trace();
      }
      
      var noModuleContent = {};
      var moduleContent = noModuleContent;

      function getData(moduleString) {
        if(moduleContent === noModuleContent) {
          var setAns = function(answer) {
            moduleContent = answer;
          }
          try {
            loader.safeEval("define(" + moduleString + ")", {define: setAns});
            return moduleContent;
          }
          catch(e) {
            console.error("Could not get contents: ", fs.realpathSync(path + ".js"));
            console.error("Content was: ", content);
            throw e;
          }
        }
        else {
          return moduleContent;
        }
      }

      var content = String(fs.readFileSync(fs.realpathSync(path + ".js")));
      return RUNTIME.makeObject({
          "get-raw-dependencies":
            F(function() {
              var m = getData(content);
              if(m.requires) {
                return m.requires.map(function(m) {
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
          "get-raw-native-modules":
            F(function() {
              var m = getData(content);
              if(Array.isArray(m.nativeRequires)) {
                return m.nativeRequires.map(RUNTIME.makeString);
              } else {
                return [];
              }
            }),
          "get-raw-datatype-provides":
            F(function() {
              var m = getData(content);
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
              var m = getData(content);
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
              var m = getData(content);
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
              return content;
              /*
              if(m.theModule) {
                // NOTE(joe): this will b removed once polyglot is done
                return RUNTIME.makeOpaque(m.theModule);
              }
              else {
                return RUNTIME.makeOpaque(function() { return m; });
              }
              */
            })
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
  }
})
