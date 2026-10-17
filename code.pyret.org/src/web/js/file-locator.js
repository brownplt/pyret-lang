define("cpo/file-locator", [], function() {
  /**
   * NOTE(joe): File locator in pyret-lang looks like this:
   * 
   * 
     lam(path, globals):
    var ast = nothing
    {
      path: path,
      globals: globals,
      method get-uncached(self): none end,
      method get-modified-time(self):
        file-ops.file-times(path).mtime
      end,
      method get-options(self, options):
        options
      end,
      method get-module(self) block:
        when ast == nothing block:
          when not(file-ops.file-exists(self.path)):
            raise("File " + self.path + " does not exist")
          end
          f = file-ops.input-file(self.path)
          str = f.read-file()
          f.close-file()
          # spy "Parsing": uri: self.uri() end
          ast := CL.pyret-ast(PP.surface-parse(str, self.uri()))
        end
        ast
      end,
      method get-dependencies(self):
        CL.get-standard-dependencies(self.get-module(), self.uri())
      end,
      method get-native-modules(self):
        [list:]
      end,
      method get-extra-imports(self):
        CS.standard-imports
      end,
      method get-globals(self): self.globals end,
      method set-compiled(self, cr, deps) block:
        ast := nothing
        nothing
      end,
      method needs-compile(self, provides):
        true
      end,
      method get-compiled(self):
        none
      end,
      method uri(self): "file://" + string-replace(file-ops.real-path(self.path), P.path-sep, "/") end,
      method name(self): P.basename(self.path, "") end,
      method _equals(self, other, eq): eq(self.uri(), other.uri()) end
    }
  end
   */
  function makeFileLocatorConstructor(
      sendRpc,
      runtime,
      compileLib,
      compileStructs,
      parsePyret,
      builtinModules,
      cpo
  ) {
    var gf = runtime.getField;
    var gmf = function(m, f) { return gf(gf(m, "values"), f); };


    // Every RPC here runs under a paused stack, so a rejected RPC has to
    // become restarter.error -- otherwise the run waits forever, silently.
    function rpcFailed(restarter, what) {
      return function(err) {
        restarter.error(runtime.ffi.makeMessageException(what + ": " + String(err)));
      };
    }

    function makeFileLocator(path) {
      return runtime.pauseStack((restarter) => {
        const realpath = sendRpc('path', 'resolve', [path]);
        const uri = realpath.then(rp => `file://${rp}`)
        let ast;
        uri.then(function(uri) { 

          function getModule(self) {
            if (ast) { return ast; }
            return runtime.pauseStack(function(restarter) {
              const contents = sendRpc('fs', 'readFile', [path, 'utf8']);
              contents.then(contents => {
                CPO.documents.set(uri, new CodeMirror.Doc(contents, "pyret"));
                runtime.runThunk(() => {
                  return runtime.safeCall(function() {
                    return gmf(parsePyret, "surface-parse").app(contents, uri);
                  }, function(ret) {
                    // Known flat constructor
                    ast = gmf(compileLib, "pyret-ast").app(ret);
                    return ast; 
                  }, "file-locator:parse-contents");
                }, function(result) {
                  if(runtime.isSuccessResult(result)) {
                    restarter.resume(result.result);
                  }
                  else {
                    restarter.error(result.exn);
                  }
                });
              }, rpcFailed(restarter, "Could not read file " + JSON.stringify(path)));
            });
          }

          function needsCompile() { return true; }

          function getDependencies(self) {
            return runtime.safeCall(function() {
              return gf(self, "get-module").app();
            }, function(mod) {
              return runtime.safeTail(function() {
                return gmf(compileLib, "get-standard-dependencies").app(mod, uri);
              });
            }, "filelocator:get-dependencies");
          }

          function getProvides(self) {
            return runtime.safeCall(function() {
              return gf(self, "get-module").app();
            }, function(mod) {
              return runtime.safeTail(function() {
                return gmf(compileLib, "get-provides").app(mod, uri);
              });
            }, "filelocator:get-provides");
          }

          function getExtraImports(self) {
            return gmf(compileStructs, "standard-imports");
          }

          function getGlobals(self) {
            return gmf(compileStructs, "standard-globals");
          }

          function getCompileEnv(_) {
            return gmf(compileStructs, "standard-builtins");
          }

          function getUri(self) { return uri; }
          function name(self) { return path; }
          function setCompiled(self) { return runtime.nothing; }
          function getModifiedTime(_) { return 0; }
          function getOptions(_, options) { return options; }
          function getNativeModules(_) { return runtime.ffi.makeList([]); }

          var m0 = runtime.makeMethod0;
          var m1 = runtime.makeMethod1;
          var m2 = runtime.makeMethod2;

          restarter.resume(runtime.makeObject({
            "get-modified-time": m0(getModifiedTime),
            "get-options": m1(getOptions),
            "get-native-modules": m0(getNativeModules),
            "needs-compile": m1(needsCompile),
            "get-module": m0(getModule),
            "get-dependencies": m0(getDependencies),
            "get-provides": m0(getProvides),
            "get-extra-imports": m0(getExtraImports),
            "get-globals": m0(getGlobals),
            "get-compile-env": m0(getCompileEnv),
            "uri": m0(getUri),
            "name": m0(name),
            "_equals": m2(function(self, other, rec) {
              return runtime.safeCall(function() {
                return runtime.getField(other, "uri").app();
              }, function(otherstr) {
                return runtime.safeTail(function() {
                  return rec.app(otherstr, uri);
                })
              }, "file-locator:_equals");
            }),
            "set-compiled": m2(setCompiled),
            "get-compiled": m1(function() { return runtime.ffi.makeNone(); })
          }));
        }, rpcFailed(restarter, "Could not resolve file path " + JSON.stringify(path)));
      });
    }

    return { makeFileLocator };

  }

  return { makeFileLocatorConstructor };
});