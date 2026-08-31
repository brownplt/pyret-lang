/*
  Support library for running code.pyret.org on the TypeScript compiler
  (lang/src/ts-compiler, bundled onto the page as window.PyretTSCompiler).

  This module adapts between the TS compiler's plain-JS world and the
  Pyret runtime world that the rest of CPO (repl-ui, error-ui, check-ui)
  expects:

    - builtin Loadables/locators for the TS compiler, built from the
      same staticModules record the page already carries;
    - an async, context-aware module finder for the dependency
      protocols CPO supports ((url-)file imports resolve relative to the
      importing module via the embedding host's fs/path RPCs, exactly
      like the stock finder);
    - a ReplExecutor that runs compiled programs through the builtin
      load-lib's run-program, so results are genuine load-lib
      ModuleResults and render exactly as they do today;
    - a bridge that converts TS compile errors (with their renderReason/
      renderFancyReason methods returning TS ErrorDisplay trees) into
      Pyret objects whose render-(fancy-)reason methods return genuine
      builtin://error-display values, so error-ui/output-ui render them
      unchanged;
    - a re-thrower that converts TS parse errors into the same Pyret
      parse exceptions the stock parser raises.
*/
({
  requires: [
    { "import-type": "builtin", name: "load-lib" },
    { "import-type": "builtin", name: "either" },
    { "import-type": "builtin", name: "error-display" }
  ],
  nativeRequires: [],
  provides: {},
  theModule: function(runtime, namespace, uri, loadLib, eitherLib, errorDisplayLib) {
    var gf = runtime.getField;
    var gmf = function(m, f) { return gf(gf(m, "values"), f); };
    // NOTE: runtime.ffi is not fully populated until the ffi builtin
    // loads, which can happen after this module instantiates; always
    // access it lazily.

    function tsCompiler() {
      if(!window.PyretTSCompiler) {
        throw new Error("The TypeScript compiler bundle (ts-compiler.gz.js) is not loaded on this page");
      }
      return window.PyretTSCompiler;
    }

    // ---------------------------------------------------------------
    // Srcloc conversions
    // ---------------------------------------------------------------

    function tsLocToPyret(loc) {
      if(!loc) { return runtime.makeSrcloc(["unknown location"]); }
      if(loc.$name === "builtin") {
        return runtime.makeSrcloc([loc.moduleName]);
      }
      return runtime.makeSrcloc([
        loc.source,
        loc.startLine, loc.startColumn, loc.startChar,
        loc.endLine, loc.endColumn, loc.endChar
      ]);
    }

    function pyretLocToTs(pyLoc) {
      var T = tsCompiler();
      if(runtime.hasField(pyLoc, "source")) {
        return new T.srcloc.Srcloc(
          gf(pyLoc, "source"),
          runtime.num_to_fixnum(gf(pyLoc, "start-line")),
          runtime.num_to_fixnum(gf(pyLoc, "start-column")),
          runtime.num_to_fixnum(gf(pyLoc, "start-char")),
          runtime.num_to_fixnum(gf(pyLoc, "end-line")),
          runtime.num_to_fixnum(gf(pyLoc, "end-column")),
          runtime.num_to_fixnum(gf(pyLoc, "end-char")));
      }
      else {
        return new T.srcloc.Builtin(gf(pyLoc, "module-name"));
      }
    }

    // ---------------------------------------------------------------
    // ErrorDisplay conversion: TS error-display tree -> Pyret
    // builtin://error-display values
    // ---------------------------------------------------------------

    var ED = gf(errorDisplayLib, "values");
    function edc(name) { return gf(ED, name); }

    function convertEmbed(val) {
      // Pyret numbers/strings/booleans are plain JS values at runtime, so
      // primitives can be embedded directly; anything else from the TS
      // world is shown via its toString (the TS port's data classes print
      // like the Pyret originals).
      if(typeof val === "number" || typeof val === "string" || typeof val === "boolean") {
        return edc("embed").app(val);
      }
      return edc("text").app(String(val));
    }

    function convertED(ed) {
      switch(ed.$name) {
        case "paragraph":
          return edc("paragraph").app(runtime.ffi.makeList(ed.contents.map(convertED)));
        case "bulleted-sequence":
          return edc("bulleted-sequence").app(runtime.ffi.makeList(ed.contents.map(convertED)));
        case "v-sequence":
          return edc("v-sequence").app(runtime.ffi.makeList(ed.contents.map(convertED)));
        case "h-sequence":
          return edc("h-sequence").app(runtime.ffi.makeList(ed.contents.map(convertED)), ed.sep);
        case "h-sequence-sep":
          return edc("h-sequence-sep").app(runtime.ffi.makeList(ed.contents.map(convertED)), ed.sep, ed.last);
        case "embed":
          return convertEmbed(ed.val);
        case "text":
          return edc("text").app(ed.str);
        case "loc":
          return edc("loc").app(tsLocToPyret(ed.loc));
        case "maybe-stack-loc":
          return edc("maybe-stack-loc").app(
            ed.n,
            ed.userFramesOnly,
            runtime.makeFunction(function(pyLoc) {
              return convertED(ed.contentsWithLoc(pyretLocToTs(pyLoc)));
            }, "contents-with-loc"),
            convertED(ed.contentsWithoutLoc));
        case "code":
          return edc("code").app(convertED(ed.contents));
        case "cmcode":
          return edc("cmcode").app(tsLocToPyret(ed.loc));
        case "loc-display":
          return edc("loc-display").app(tsLocToPyret(ed.loc), ed.style, convertED(ed.contents));
        case "optional":
          return edc("optional").app(convertED(ed.contents));
        case "highlight":
          return edc("highlight").app(
            convertED(ed.contents),
            runtime.ffi.makeList(ed.locs.map(tsLocToPyret)),
            ed.color);
        default:
          console.error("Unknown TS ErrorDisplay variant: ", ed);
          return edc("text").app(String(ed));
      }
    }

    // A TS CompileError -> a Pyret object renderable by error-ui. The
    // render methods are called off the data the TS error carries, and
    // produce genuine error-display values.
    function bridgeCompileError(tsError) {
      return runtime.makeObject({
        "render-fancy-reason": runtime.makeMethod0(function(_self) {
          return convertED(tsError.renderFancyReason());
        }),
        "render-reason": runtime.makeMethod0(function(_self) {
          return convertED(tsError.renderReason());
        })
      });
    }

    // The `left` payload from the TS repl is a list whose elements are
    // either CompileResult errs (with .problems) or raw problem lists
    // (from make-standalone); normalize to the shape repl-ui's
    // displayResult expects: a Pyret list of records with a `problems`
    // list of renderable errors.
    function bridgeCompileErrors(errs) {
      var asRecords = errs.map(function(e) {
        var problems;
        if(e && Array.isArray(e.problems)) { problems = e.problems; }
        else { problems = [e]; }
        return runtime.makeObject({
          "problems": runtime.ffi.makeList(problems.map(bridgeCompileError))
        });
      });
      return runtime.ffi.makeList(asRecords);
    }

    // ---------------------------------------------------------------
    // Parse errors: re-throw the TS parser's structured errors through
    // the runtime ffi, producing the same Pyret exceptions the stock
    // parse-pyret module raises. MUST be called on the Pyret stack.
    // ---------------------------------------------------------------

    function isTsParseError(e) {
      return e && e.name === "PyretParseError" && typeof e.kind === "string";
    }

    function throwPyretParseError(e) {
      var loc = tsLocToPyret(e.loc);
      switch(e.kind) {
        case "parse-error-next-token":
          return runtime.ffi.throwParseErrorNextToken(loc, e.nextToken);
        case "parse-error-eof":
          return runtime.ffi.throwParseErrorEOF(loc);
        case "parse-error-unterminated-string":
          return runtime.ffi.throwParseErrorUnterminatedString(loc);
        case "parse-error-bad-number":
          return runtime.ffi.throwParseErrorBadNumber(loc);
        case "parse-error-bad-operator":
          return runtime.ffi.throwParseErrorBadOper(loc);
        case "parse-error-bad-check-operator":
          return runtime.ffi.throwParseErrorBadCheckOper(tsLocToPyret(e.op && e.op.l ? e.op.l : e.loc));
        case "parse-error-colon-colon":
          return runtime.ffi.throwParseErrorColonColon(loc, e.nextToken);
        case "parse-error-bad-app":
          return runtime.ffi.throwParseErrorBadApp(tsLocToPyret(e.funLoc), tsLocToPyret(e.argsLoc));
        case "parse-error-bad-fun-header":
          return runtime.ffi.throwParseErrorBadFunHeader(tsLocToPyret(e.funLoc || e.loc), tsLocToPyret(e.argsLoc || e.loc));
        default:
          return runtime.ffi.throwMessageException(String(e.message || e));
      }
    }

    // ---------------------------------------------------------------
    // Builtin modules: loadables and locators from staticModules
    // ---------------------------------------------------------------

    function makeBuiltinSupport(staticModules) {
      var T = tsCompiler();
      var CS = T.compileStructs;
      var loadables = new Map();
      var locators = {};

      Object.keys(staticModules).forEach(function(modUri) {
        if(modUri.indexOf("builtin://") !== 0) { return; }
        var name = modUri.slice("builtin://".length);
        var m = staticModules[modUri];
        var raw = T.builtinModules.builtinRawLocatorFromModule(
          function() { return m; },
          function() { return JSON.stringify(m); });
        var loadable = null;
        function getLoadable() {
          if(loadable === null) {
            var provs = CS.providesFromRawProvides(modUri, {
              uri: modUri,
              values: raw.getRawValueProvides(),
              aliases: raw.getRawAliasProvides(),
              datatypes: raw.getRawDatatypeProvides(),
              modules: raw.getRawModuleProvides()
            });
            loadable = new CS.ModuleAsString(provs, CS.noBuiltins, CS.computedNone,
              CS.ok(new T.jsOfPyret.CCPString("")));
          }
          return loadable;
        }
        loadables.set(modUri, getLoadable());
        locators[modUri] = {
          getUncached: function() { return undefined; },
          needsCompile: function(_provides) { return false; },
          getModifiedTime: function() { return 0; },
          getOptions: function(options) {
            var o = Object.assign({}, options);
            o.checks = "none";
            return o;
          },
          getModule: function() {
            throw new Error("Should never fetch source for builtin module " + modUri);
          },
          getExtraImports: function() { return CS.standardImports; },
          getDependencies: function() {
            return raw.getRawDependencies().map(CS.makeDep);
          },
          getNativeModules: function() {
            return raw.getRawNativeModules().map(function(n) { return new CS.Requirejs(n); });
          },
          getGlobals: function() { return CS.standardGlobals; },
          uri: function() { return modUri; },
          name: function() { return name; },
          setCompiled: function(_loadable, _provides) { return; },
          getCompiled: function() { return getLoadable(); }
        };
      });

      return { loadables: loadables, locators: locators };
    }

    // ---------------------------------------------------------------
    // Module finder. The TS compiler's dependency chase is async
    // (CL.compileWorklist awaits each finder result), so modules are
    // located and loaded on demand -- fetch for url imports, the
    // embedding host's fs/path RPCs for (url-)file imports -- with NO
    // prefetch pass. Resolution is context-aware, mirroring the stock
    // finder (cpo-main.js makeFindModule / 913740d28eb): a (url-)file
    // import resolves relative to the directory of the importing
    // module, threaded as a "load-path", and all path arithmetic goes
    // through the same host path RPC the running program uses (the
    // filesystem-internal builtin), so imports and runtime file ops
    // resolve a given path identically.
    // ---------------------------------------------------------------

    function maybeAppendSlash(s) {
      if(s.endsWith("/")) { return s; }
      return s + "/";
    }

    function urlResolve(path, base) {
      return new URL(path, base).href;
    }

    // The same wire calls filesystem-internal.js makes; that module is
    // the single source of truth for path semantics in embedded hosts.
    function hostRpc(module, method, args) {
      if(!window.MESSAGES) {
        return Promise.reject(new Error(
          "This import needs an embedding host (vscode / embed) that provides '" +
          module + "." + method + "' over RPC"));
      }
      return window.MESSAGES.sendRpc(module, method, args);
    }
    function hostReadFileString(p) {
      return hostRpc('fs', 'readFile', [p]).then(function(contents) {
        // The RPC delivers whatever the host's fs produced after a
        // structured clone: a string, a Uint8Array (vscode), or a
        // JSONified Buffer. Decode without assuming a Buffer global --
        // this module's scope inside the jarr doesn't have one.
        if(typeof contents === "string") { return contents; }
        if(contents instanceof Uint8Array) { return new TextDecoder("utf-8").decode(contents); }
        if(contents && contents.type === "Buffer" && Array.isArray(contents.data)) {
          return new TextDecoder("utf-8").decode(new Uint8Array(contents.data));
        }
        if(Array.isArray(contents)) { return new TextDecoder("utf-8").decode(new Uint8Array(contents)); }
        return new TextDecoder("utf-8").decode(new Uint8Array(contents));
      });
    }
    function hostExists(p) {
      return hostRpc('fs', 'stat', [p]).then(function() { return true; }, function(e) {
        if(String(e).includes("EntryNotFound")) { return false; }
        throw e;
      });
    }

    function getLoadPath(context) {
      if(context && typeof context["load-path"] === "string") { return context["load-path"]; }
      return ".";
    }
    // get-real-path: an absolute REL is honored as-is, otherwise it is
    // joined onto the importer's load-path (both via the host path RPC).
    function getRealPath(context, rel) {
      return hostRpc('path', 'isAbsolute', [rel]).then(function(abs) {
        if(abs) { return rel; }
        return hostRpc('path', 'join', [getLoadPath(context), rel]);
      });
    }
    // For a dependency, compute a Promise of { path, context }: the local
    // path to load (null for non-local protocols) and the context to
    // thread to that module's own imports. Unlike the stock finder we
    // skip the path RPCs entirely when a url-file import will load
    // remotely (all-remote mode, or no embedding host) -- observable
    // behavior is identical, but the plain-web editor never touches RPCs.
    function dependencyResolveInfo(context, dep, urlFileMode) {
      var CS = tsCompiler().compileStructs;
      var rel = null;
      if(CS.isDependency(dep)) {
        if(dep.protocol === "file" || dep.protocol === "js-file") { rel = dep.arguments[0]; }
        else if(dep.protocol === "url-file" && urlFileMode !== "all-remote" && window.MESSAGES) {
          rel = dep.arguments[1];
        }
      }
      if(rel === null) {
        // builtin/gdrive/url and remote url-file: nothing local to track,
        // thread the importer's context through unchanged.
        return Promise.resolve({ path: null, context: context });
      }
      return getRealPath(context, rel).then(function(realPath) {
        return hostRpc('path', 'dirname', [realPath]).then(function(dir) {
          return { path: realPath, context: { "load-path": dir } };
        });
      });
    }

    // Register a loaded source with CPO's document map so error
    // highlighting (cmcode) can render srclocs into it.
    function registerDocument(uri, text) {
      if(window.CPO && CPO.documents && typeof CodeMirror !== "undefined" && !CPO.documents.has(uri)) {
        CPO.documents.set(uri, new CodeMirror.Doc(text, "pyret"));
      }
    }

    function fetchText(url) {
      return fetch(url).then(function(response) {
        if(!response.ok) {
          throw new Error("Failed to load " + url + ": " + response.status);
        }
        return response.text();
      });
    }

    function makeTextLocator(uri, text) {
      var T = tsCompiler();
      var CS = T.compileStructs;
      var CL = T.compileLib;
      var self = {
        getUncached: function() { return undefined; },
        needsCompile: function(_provides) { return true; },
        getModifiedTime: function() { return 0; },
        getOptions: function(options) { return options; },
        getModule: function() { return new CL.PyretString(text); },
        getExtraImports: function() { return CS.standardImports; },
        getDependencies: function() {
          return CL.getStandardDependencies(self.getModule(), uri);
        },
        getNativeModules: function() { return []; },
        getGlobals: function() { return CS.standardGlobals; },
        uri: function() { return uri; },
        name: function() { return uri; },
        setCompiled: function(_loadable, _provides) { return; },
        getCompiled: function() { return undefined; }
      };
      return self;
    }

    function makeFinderFactory(builtinSupport, urlFileMode) {
      return function makeFinder() {
        var locatorCache = {};
        return function finder(context, dep) {
          var T = tsCompiler();
          var CS = T.compileStructs;
          var CL = T.compileLib;
          function located(uri, locator, ctx) {
            locatorCache[uri] = locator;
            return new CL.Located(locator, ctx);
          }
          if(CS.isBuiltin(dep)) {
            var builtinLocator = builtinSupport.locators["builtin://" + dep.modname];
            if(!builtinLocator) {
              throw new Error("Unknown module: " + dep.modname);
            }
            return located("builtin://" + dep.modname, builtinLocator, context);
          }
          return dependencyResolveInfo(context, dep, urlFileMode).then(function(info) {
            function textLocated(uri, textPromise, ctx) {
              if(locatorCache[uri]) {
                return new CL.Located(locatorCache[uri], ctx);
              }
              return textPromise.then(function(text) {
                registerDocument(uri, text);
                return located(uri, makeTextLocator(uri, text), ctx);
              });
            }
            function localFileLocated(fullUrlForDocs, ctx) {
              return hostRpc('path', 'resolve', [info.path]).then(function(realpath) {
                var uri = "file://" + realpath;
                return textLocated(uri, hostReadFileString(info.path).then(function(text) {
                  if(fullUrlForDocs) { registerDocument(fullUrlForDocs, text); }
                  return text;
                }), ctx);
              });
            }
            if(dep.protocol === "file") {
              return localFileLocated(null, info.context);
            }
            if(dep.protocol === "url") {
              var url = dep.arguments[0];
              return textLocated(url, fetchText(url), context);
            }
            if(dep.protocol === "url-file") {
              var fullUrl = urlResolve(dep.arguments[1], maybeAppendSlash(dep.arguments[0]));
              if(info.path === null) {
                // all-remote (or no embedding host)
                return textLocated(fullUrl, fetchText(fullUrl), context);
              }
              if(urlFileMode === "all-local") {
                return localFileLocated(fullUrl, info.context);
              }
              // local-if-present
              return hostExists(info.path).then(function(exists) {
                if(exists) { return localFileLocated(fullUrl, info.context); }
                return textLocated(fullUrl, fetchText(fullUrl), context);
              });
            }
            throw new Error("The import protocol '" + dep.protocol +
              "' is not supported when running with the TypeScript compiler: " +
              dep.protocol + "://" + dep.arguments.join(":"));
          });
        };
      };
    }

    // ---------------------------------------------------------------
    // Executor over load-lib: runs compiled program text in the realm
    // ---------------------------------------------------------------

    function makeExecutor(pyRuntime) {
      var runProgramPy = gmf(loadLib, "run-program");
      return {
        run: function(realm, programJsSource, _options) {
          return new Promise(function(resolve, reject) {
            runtime.runThunk(function() {
              return runProgramPy.app(
                pyRuntime,
                realm,
                programJsSource,
                runtime.makeObject({ "checks": "main" }),
                runtime.ffi.makeList([]));
            }, function(result) {
              if(runtime.isSuccessResult(result)) {
                resolve(result.result);
              }
              else {
                // An error escaping run-program itself (not the
                // program): surface the whole FailureResult.
                reject(result);
              }
            });
          });
        },
        isSuccessResult: function(moduleResult) {
          return moduleResult.val.runtime.isSuccessResult(moduleResult.val.result);
        },
        getResultRealm: function(moduleResult) {
          return moduleResult.val.realm;
        }
      };
    }

    // ---------------------------------------------------------------
    // Result plumbing: convert the TS repl's Either (or thrown error)
    // into the same runtime-result-wrapped Pyret Either that repl-ui's
    // displayResult consumes.
    // ---------------------------------------------------------------

    var pyLeft = gmf(eitherLib, "left");
    var pyRight = gmf(eitherLib, "right");

    function resolveWithEither(deferred, either) {
      runtime.runThunk(function() {
        if(either.$name === "left") {
          return pyLeft.app(bridgeCompileErrors(either.v));
        }
        else {
          return pyRight.app(either.v);
        }
      }, function(result) {
        deferred.resolve(result);
      });
    }

    function resolveWithError(deferred, err) {
      // A FailureResult rejected out of the executor passes through
      // unchanged (it is already what displayResult expects).
      if(err && runtime.isFailureResult && runtime.isFailureResult(err)) {
        deferred.resolve(err);
        return;
      }
      runtime.runThunk(function() {
        if(isTsParseError(err)) {
          return throwPyretParseError(err);
        }
        console.error("Error while compiling with the TS compiler: ", err);
        return runtime.ffi.throwMessageException(String((err && err.message) || err));
      }, function(result) {
        deferred.resolve(result);
      });
    }

    /*
      Report a run that was stopped before it ever became a Pyret computation.
      (This happens for stops during the TS compiler's compile)
    */
    function resolveWithUserBreak(deferred) {
      runtime.runThunk(function() {
        return runtime.raise(runtime.ffi.userBreak);
      }, function(result) {
        deferred.resolve(result);
      });
    }

    return runtime.makeJSModuleReturn({
      makeBuiltinSupport: makeBuiltinSupport,
      makeFinderFactory: makeFinderFactory,
      makeExecutor: makeExecutor,
      resolveWithEither: resolveWithEither,
      resolveWithError: resolveWithError,
      resolveWithUserBreak: resolveWithUserBreak,
      bridgeCompileErrors: bridgeCompileErrors,
      tsCompiler: tsCompiler
    });
  }
})
