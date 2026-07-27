({
  requires: [
    { "import-type": "dependency",
      protocol: "file",
      args: ["../../../pyret/src/arr/compiler/compile-lib.arr"]
    },
    { "import-type": "dependency",
      protocol: "file",
      args: ["../../../pyret/src/arr/compiler/compile-structs.arr"]
    },
    { "import-type": "dependency",
      protocol: "file",
      args: ["../../../pyret/src/arr/compiler/locators/jsfile.arr"]
    },
    { "import-type": "dependency",
      protocol: "file",
      args: ["../../../pyret/src/arr/compiler/repl.arr"]
    },
    { "import-type": "dependency",
      protocol: "file",
      args: ["../../../pyret/src/arr/compiler/locators/url.arr"]
    },
    { "import-type": "dependency",
      protocol: "file",
      args: ["../arr/cpo.arr"]
    },
    { "import-type": "dependency",
      protocol: "js-file",
      args: ["./repl-ui"]
    },
    { "import-type": "dependency",
      protocol: "js-file",
      args: ["./text-handlers"]
    },
    { "import-type": "builtin",
      name: "parse-pyret"
    },
    { "import-type": "builtin",
      name: "runtime-lib"
    },
    { "import-type": "builtin",
      name: "load-lib"
    },
    { "import-type": "builtin",
      name: "builtin-modules"
    },
    { "import-type": "builtin",
      name: "cpo-builtins"
    },
    { "import-type": "builtin",
      name: "filesystem-internal"
    }
  ],
  nativeRequires: [
    "cpo/gdrive-locators",
    "cpo/file-locator",
    "cpo/http-imports",
    "cpo/cpo-builtin-modules",
    "cpo/modal-prompt",
    "pyret-base/js/runtime"
  ],
  provides: {
    values: {
      "repl": "tany"
    }
  },
  theModule: function(runtime, namespace, uri,
                      compileLib, compileStructs, jsfile, pyRepl, urlLoc, cpo, replUI, textHandlers,
                      parsePyret, runtimeLib, loadLib, builtinModules, cpoBuiltins, fsInternal,
                      gdriveLocators, fileLocator, http, cpoModules, _modalPrompt,
                      rtLib) {




    var replContainer = $("<div>").addClass("repl");
    replContainer.attr("tabindex", "-1").attr('role', 'application');
    //replContainer.attr("aria-hidden", "true");
    $("#REPL").append(replContainer);

    runtime.setParam("imgUrlProxy", function(s) {
      var a = document.createElement("a");
      a.href = s;
      if(a.origin === window.APP_BASE_URL) {
        return s;
      }
      else if(window.IMAGE_PROXY_BYPASS) {
        return s;
      }
      /*
      else if(a.hostname === "drive.google.com" && a.pathname === "/uc") {
        return s;
      }
      */
      else {
        return window.APP_BASE_URL + "/downloadImg?" + s;
      }
    });

    var gf = runtime.getField;
    var gmf = function(m, f) { return gf(gf(m, "values"), f); };
    var gtf = function(m, f) { return gf(m, "types")[f]; };

    var constructors = gdriveLocators.makeLocatorConstructors(storageAPI, runtime, compileLib, compileStructs, parsePyret, builtinModules, cpo);

    // NOTE(joe): In order to yield control quickly, this doesn't pause the
    // stack in order to save.  It simply sends the save requests and
    // immediately returns.  This avoids needlessly serializing multiple save
    // requests when this is called repeatedly from Pyret.
    function saveGDriveCachedFile(name, content) {
      var file = storageAPI.then(function(storageAPI) {
        var existingFile = storageAPI.getCachedFileByName(name);
        return existingFile.then(function(f) {
          if(f.length >= 1) {
            return f[0];
          }
          else {
            return storageAPI.createFile(name, {
              saveInCache: true,
              fileExtension: ".js",
              mimeType: "text/plain"
            });
          }
        });
      });
      file.then(function(f) {
        f.save(content, true);
      });
      return runtime.nothing;
    }

    // NOTE(joe): this function just allocates a closure, so it's stack-safe
    var onCompile = gmf(cpo, "make-on-compile").app(runtime.makeFunction(saveGDriveCachedFile, "save-gdrive-cached-file"));

    function maybeAppendSlash(s) {
      if(s.endsWith("/")) { return s; }
      return s + "/";
    }

    function urlResolve(path, base) {
      return new URL(path, base).href;
    }

    // Module resolution is context-aware: a (url-)file import resolves relative
    // to the directory of the importing module, NOT the open editor tab. We
    // thread that directory as a "load-path" (kept relative to the tab, which is
    // the base the embedding host resolves every fs path against, so no host-side
    // change is needed to land on the right file).
    //
    // All path arithmetic goes through the same filesystem-internal RPC the
    // running program uses (read-file / image-file / ...), so that
    // `import url-file(...)` and runtime file operations resolve a given path
    // identically -- one source of truth, in the embedding host. This mirrors
    // get-real-path / locate-file in cli-module-loader.arr (which thread a
    // current-load-path through the `filesystem` module the same way).
    function getLoadPath(context) {
      if (context && runtime.hasField(context, "load-path")) {
        return runtime.getField(context, "load-path");
      }
      return ".";
    }
    function makeContext(loadPath) {
      return runtime.makeObject({ "load-path": loadPath });
    }
    // get-real-path: an absolute REL is honored as-is, otherwise it is joined
    // onto the importer's load-path. Returns a Promise (the path ops are RPCs).
    function getRealPath(context, rel) {
      return fsInternal.isAbsolute(rel).then(function(abs) {
        if (abs) { return rel; }
        return fsInternal.join(getLoadPath(context), rel);
      });
    }
    // For a dependency, compute a Promise of { path, context }: the path to feed
    // the file locator (null for non-file protocols) and the context to thread
    // to that module when its own dependencies are resolved.
    function dependencyResolveInfo(context, dependency, urlFileMode) {
      return runtime.ffi.cases(gmf(compileStructs, "is-Dependency"), "Dependency", dependency,
        {
          builtin: function(name) { return Promise.resolve({ path: null, context: context }); },
          dependency: function(protocol, args) {
            var arr = runtime.ffi.toArray(args);
            var rel = null;
            if (protocol === "file" || protocol === "js-file") { rel = arr[0]; }
            else if (protocol === "url-file" && urlFileMode !== "all-remote") { rel = arr[1]; }
            if (rel === null || !window.MESSAGES) {
              // builtin/gdrive/url and remote url-file: nothing local to track.
              // Likewise without an embedding host (plain CPO in a browser)
              // there is no filesystem to resolve against, so (url-)file paths
              // stay untracked rather than erroring in the path RPCs.
              // Thread the importer's context through unchanged.
              return Promise.resolve({ path: null, context: context });
            }
            return getRealPath(context, rel).then(function(realPath) {
              return fsInternal.dirname(realPath).then(function(dir) {
                return { path: realPath, context: makeContext(dir) };
              });
            });
          }
        });
    }

    // NOTE(joe/ben): this function _used_ to be trivially stack safe, but files
    // need to resolve their absolute path to calculate their URI, which
    // requires an RPC, so this function is no-longer trivially flat
    function uriFromDependency(dependency, info) {
      return runtime.ffi.cases(gmf(compileStructs, "is-Dependency"), "Dependency", dependency,
        {
          builtin: function(name) {
            return "builtin://" + name;
          },
          dependency: function(protocol, args) {
            var arr = runtime.ffi.toArray(args);
            if (protocol === "my-gdrive") {
              return "my-gdrive://" + arr[0];
            }
            else if (protocol === "shared-gdrive") {
              return "shared-gdrive://" + arr[0] + ":" + arr[1];
            }
            else if (protocol === "gdrive-js") {
              return "gdrive-js://" + arr[1];
            }
            else if (protocol === "js-file") {
              if(!window.MESSAGES) {
                console.error("Unknown import: ", dependency);
                return protocol + "://" + arr.join(":");
              }
              // Identity uses the importer-relative path (info.path) so a
              // module's URI matches the file actually loaded.
              return runtime.pauseStack((restarter) => {
                const realpath = window.MESSAGES.sendRpc('path', 'resolve', [info.path]);
                realpath.then((realpath) => {
                  restarter.resume(`js-file://${realpath}`);
                });
              });
            }
            else if (protocol === "file") {
              if(!window.MESSAGES) {
                console.error("Unknown import: ", dependency);
                return protocol + "://" + arr.join(":");
              }
              return runtime.pauseStack((restarter) => {
                const realpath = window.MESSAGES.sendRpc('path', 'resolve', [info.path]);
                realpath.then((realpath) => {
                  restarter.resume(`file://${realpath}`);
                });
              });
            }
            else if (protocol === "url") {
              return arr[0];
            }
            else if (protocol === "url-file") {
              return urlResolve(arr[1], maybeAppendSlash(arr[0]));
            }
            else {
              console.error("Unknown import: ", dependency);
              return protocol + "://" + arr.join(":");
            }
          }
        });
    }

    function makeFindModule(urlFileMode) {
      // The locatorCache memoizes locators for the duration of an
      // interactions run
      var locatorCache = {};
      function findModule(context, dependency) {
        return runtime.safeCall(function() {
          // Resolve the import path via the host's filesystem RPC (a Promise),
          // pausing the Pyret stack until it settles.
          return runtime.pauseStack(function(restarter) {
            dependencyResolveInfo(context, dependency, urlFileMode).then(function(info) {
              restarter.resume(info);
            }, function(err) {
              // A Pyret exception value (e.g. from filesystem-internal) carries
              // its own message; stringifying it would render [object Object].
              if(runtime.isObject(err)) {
                restarter.error(err);
              }
              else {
                restarter.error(runtime.ffi.makeMessageException("Error resolving import path: " + String(err)));
              }
            });
          });
        }, function(info) {
        return runtime.safeCall(() => {
          return uriFromDependency(dependency, info);
        }, function(uri) {
          if(locatorCache.hasOwnProperty(uri)) {
            return gmf(compileLib, "located").app(locatorCache[uri], info.context);
          }
          return runtime.safeCall(function() {
            return runtime.ffi.cases(gmf(compileStructs, "is-Dependency"), "Dependency", dependency,
              {
                builtin: function(name) {
                  var raw = cpoModules.getBuiltinLoadableName(runtime, name);
                  if(!raw) {
                    throw runtime.throwMessageException("Unknown module: " + name);
                  }
                  else {
                    return gmf(cpo, "make-builtin-js-locator").app(name, raw);
                  }
                },
                dependency: function(protocol, args) {
                  var arr = runtime.ffi.toArray(args);
                  if (protocol === "my-gdrive") {
                    return constructors.makeMyGDriveLocator(arr[0]);
                  }
                  else if (protocol === "shared-gdrive") {
                    return constructors.makeSharedGDriveLocator(arr[0], arr[1]);
                  }
                  else if (protocol === "gdrive-js") {
                    return constructors.makeGDriveJSLocator(arr[0], arr[1]);
                  }
                  else if (protocol === "js-file" && window.MESSAGES) {
                    return gmf(jsfile, "make-jsfile-locator").app(info.path);
                  }
                  else if (protocol === "file" && window.MESSAGES) {
                    var fileLocatorConstructor = fileLocator.makeFileLocatorConstructor(window.MESSAGES.sendRpc, runtime, compileLib, compileStructs, parsePyret, builtinModules, cpo);
                    return fileLocatorConstructor.makeFileLocator(info.path);
                  }
                  else if (protocol === "url") {
                    fetch(arr[0]).then(async (response) => {
                      CPO.documents.set(arr[0], new CodeMirror.Doc(await response.text(), "pyret"));
                    });
                    return runtime.getField(runtime.getField(urlLoc, "values"), "url-locator").app(arr[0], replGlobals);
                  }
                  else if (protocol === "url-file") {
                    const fullUrl = urlResolve(arr[1], maybeAppendSlash(arr[0]));
                    switch(urlFileMode) {
                      case "all-remote":
                        fetch(fullUrl).then(async (response) => {
                          CPO.documents.set(fullUrl, new CodeMirror.Doc(await response.text(), "pyret"));
                        });
                        return runtime.getField(runtime.getField(urlLoc, "values"), "url-locator").app(fullUrl, replGlobals);
                      case "all-local":
                        fsInternal.readFile(info.path).then((contents) => {
                          const strContents = Buffer.from(contents).toString('utf8');
                          CPO.documents.set(fullUrl, new CodeMirror.Doc(strContents, "pyret"));
                        });
                        var fileLocatorConstructor = fileLocator.makeFileLocatorConstructor(window.MESSAGES.sendRpc, runtime, compileLib, compileStructs, parsePyret, builtinModules, cpo);
                        return fileLocatorConstructor.makeFileLocator(info.path);
                      case "local-if-present":
                        return runtime.pauseStack(async (restarter) => {
                          const exists = await fsInternal.exists(info.path);
                          if(exists) {
                            fsInternal.readFile(info.path).then((contents) => {
                              const strContents = Buffer.from(contents).toString('utf8');
                              CPO.documents.set(fullUrl, new CodeMirror.Doc(strContents, "pyret"));
                            });
                            return runtime.runThunk(() => {
                              var fileLocatorConstructor = fileLocator.makeFileLocatorConstructor(window.MESSAGES.sendRpc, runtime, compileLib, compileStructs, parsePyret, builtinModules, cpo);
                              return fileLocatorConstructor.makeFileLocator(info.path);
                            }, (result) => {
                              if(runtime.isSuccessResult(result)) {
                                restarter.resume(result.result);
                              }
                              else {
                                restarter.error(result.error);
                              }
                            });
                          }
                          else {
                            fetch(fullUrl).then(async (response) => {
                              CPO.documents.set(fullUrl, new CodeMirror.Doc(await response.text(), "pyret"));
                            });
                            return restarter.resume(runtime.getField(runtime.getField(urlLoc, "values"), "url-locator").app(fullUrl, replGlobals));
                          }
                        });
                    }

                  }
                  else {
                    throw runtime.throwMessageException("Unknown import: " + uri);
                  }
                }
            });
          }, function(l) {
              locatorCache[uri] = l;
              return gmf(compileLib, "located").app(l, info.context);
          }, "findModule");
        });
        }, "findModule-info");
      }
      return runtime.makeFunction(findModule, "cpo-find-module");
    }

    // NOTE(joe): This line is "cheating" by mixing runtime levels,
    // and uses the same runtime for the compiler and running code.
    // Usually you can only get a new Runtime by calling create, but
    // here we magic the current runtime into one.
    var pyRuntime = gf(gf(runtimeLib, "internal").brandRuntime, "brand").app(
      runtime.makeObject({
        "runtime": runtime.makeOpaque(runtime)
      }));
    var pyRealm = gf(loadLib, "internal").makeRealm(cpoModules.getRealm());


    var builtins = [];
    Object.keys(runtime.getParam("staticModules")).forEach(function(k) {
      if(k.indexOf("builtin://") === 0) {
        builtins.push(runtime.makeObject({
          uri: k,
          raw: cpoModules.getBuiltinLoadable(runtime, k)
        }));
      }
    });
    var builtinsForPyret = runtime.ffi.makeList(builtins);

    var getDefsForPyret = function(source) {
      return runtime.makeFunction(function() {
        return source;
      });
    };
    var replGlobals = gmf(compileStructs, "standard-globals");

    var defaultOptions = gmf(compileStructs, "default-compile-options");

    var replP = Q.defer();
    const urlFileMode = window.URL_FILE_MODE || "all-remote";
    return runtime.safeCall(function() {
        return gmf(cpo, "make-repl").app(
            builtinsForPyret,
            pyRuntime,
            pyRealm,
            runtime.makeFunction(() => makeFindModule(urlFileMode)));
      }, function(repl) {
        var jsRepl = {
          runtime: runtime.getField(pyRuntime, "runtime").val,
          /*
            This should not be called while a Pyret stack is running
          */
          restartInteractions: function(source, options) {
            var pyOptions = defaultOptions.extendWith({
              "type-check": options.typeCheck,
              "check-all": options.checkAll,
              "on-compile": onCompile,
              "checks": "main"
            });
            var ret = Q.defer();
            setTimeout(function() {
              runtime.runThunk(function() {
                return runtime.safeCall(
                  function() {
                    return gf(repl,
                    "make-definitions-locator").app(getDefsForPyret(source), replGlobals);
                  },
                  function(locator) {
                    return gf(repl, "restart-interactions").app(locator, pyOptions);
                  }, "restart-interactions:make-definitions-locator");
              }, function(result) {
                ret.resolve(result);
              });
            }, 0);
            return ret.promise;
          },
          run: function(str, name) {
            var ret = Q.defer();
            setTimeout(function() {
              runtime.runThunk(function() {
                return runtime.safeCall(
                  function() {
                    return gf(repl,
                    "make-interaction-locator").app(
                      runtime.makeFunction(function() { return str; }))
                  },
                  function(locator) {
                    return gf(repl, "run-interaction").app(locator);
                  }, "run:make-interaction-locator");
              }, function(result) {
                ret.resolve(result);
              }, "make-interaction-locator");
            }, 0);
            return ret.promise;
          },
          pause: function(afterPause) {
            runtime.schedulePause(function(resumer) {
              afterPause(resumer);
            });
          },
          stop: function() {
            runtime.breakAll();
          },
          runtime: runtime
        };
        return withRepl(jsRepl);
      }, "make-repl");

    function withRepl(repl) {
      var runButton = $("#runButton");

      var replWidget =
          replUI.makeRepl(replContainer, repl, runtime, {
            breakButton: $("#breakButton"),
            runButton: runButton,
            runDropdown: $('#runDropdown')
          });

      // NOTE(joe): assigned on window for embedding API in events.js, and for debugging
      // NOTE(joe): Some of the CPO internals use Q promises. The withResolvers pattern
      // promotes these to real JS promises that will work with e.g. async functions.
      window.RUN_CODE = CPO.RUN_CODE = async function(src) {
        const result = doRunAction(src, true);
        const { promise, resolve, reject } = Promise.withResolvers();
        result.then(resolve);
        result.catch(reject);
        return promise;
      };
      window.RUN_INTERACTION = CPO.RUN_INTERACTION = async function(src) {
        const result = replWidget.runner(src, true);
        const { promise, resolve, reject } = Promise.withResolvers();
        result.then(resolve);
        result.catch(reject);
        return promise;
      };
      window.replWidget = CPO.replWidget = replWidget;

      /*
      $("#runDropdown").click(function() {
        $("#run-dropdown-content").toggle();
      });
      */

      // CPO.editor is set in beforePyret.js
      var editor = CPO.editor;
      var currentAction = "run";

      $("#select-run").click(function() {
        runButton.text("Run");
        currentAction = "run";
        doRunAction(editor.cm.getValue(), false);
        $('#runDropdown').attr('aria-expanded', 'false');
        $("#run-dropdown-content").attr('aria-hidden', 'true').hide();
      });

      $("#select-tc-run").click(function() {
        runButton.text("Type-check and Run");
        currentAction = "tc-and-run";
        doRunAction(editor.cm.getValue(), false);
        $('#runDropdown').attr('aria-expanded', 'false');
        $("#run-dropdown-content").attr('aria-hidden', 'true').hide();
      });
      /*
      $("#select-scsh").click(function() {
        highlightMode = "scsh"; $("#run-dropdown-content").hide();});
      $("#select-scmh").click(function() {
        highlightMode = "scmh"; $("#run-dropdown-content").hide();});
      $("#select-mcmh").click(function() {
        highlightMode = "mcmh"; $("#run-dropdown-content").hide();});
      */
      function doRunAction(src, synthetic) {
        if(!synthetic) {
          CPO.events.triggerOnRun();
        }
        editor.cm.operation(function() {
          editor.cm.clearGutter("test-marker-gutter");
          var marks = editor.cm.getAllMarks();
          document.getElementById("main").dataset.highlights = "";
          editor.cm.eachLine(function(lh){
            editor.cm.removeLineClass(lh, "background");});
          for(var i = 0; i < marks.length; i++) {
            const attribs = marks[i].attributes;
            if(!(attribs && attribs.useline)) {
              marks[i].clear();
            }
          }
        });
        var sheet = document.getElementById("highlight-styles").sheet;
        for(var i=0; i< sheet.cssRules.length; i++) {
          sheet.deleteRule(i);
        }
        switch (currentAction) {
          case "run":
            return replWidget.runCode(src, {check: true, cm: editor.cm});
          case "tc-and-run":
            return replWidget.runCode(src, {check: true, cm: editor.cm, "type-check": true});
        }
      }

      runButton.on("click", function() { doRunAction(editor.cm.getValue(), false); });

      $(window).on("keyup", function(e) {
        if(e.keyCode === 27) { // "ESC"
          $("#help-keys").fadeOut(500);
          e.stopImmediatePropagation();
          e.preventDefault();
        }
      });

      /* Documentation Overlay */
      /*
      NOTE(joe): Skipping this for now, until HTTPS solution for docs worked out
      $("#docs").on("click", function(e){
        $("#doc-containment").toggle();
        e.stopImmediatePropagation();
        e.preventDefault();
      });
      */

      $("#doc-close").on("click", function(e){
        $("#doc-containment").toggle();
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      $("#doc-overlay").draggable({
        start: fixIframe,
        stop: fixIframe,
        handle: "#doc-bar",
        cancel: "#doc-close"
        });

      $("#doc-overlay").resizable({
        handles: {
          s:"#doc-bottom",
          e: "#doc-right",
          w:"#doc-left",
          sw: "#doc-sw-corner",
          se:"#doc-se-corner"},
        start: fixIframe,
        stop: fixIframe,
        containment: "#doc-containment",
        scroll: false
        });

        function fixIframe() {
          $("#doc-cover").toggle();
        }

      $('#font-plus').click(changeFont);
      $('#font-minus').click(changeFont);

      function changeFont(e){
        fontSize = parseInt($('#main').css("font-size"));
        if ($(e.target).is("#font-plus")) {
          if (fontSize < 32) {
            $('#main').css('font-size', '+=2');
          } else if (fontSize < 55) {
            $('#main').css('font-size', '+=4');
          }
        } else if ($(e.target).is("#font-minus")) {
          if (fontSize > 32) {
            $('#main').css('font-size', '-=4');
          } else if (fontSize > 10) {
            $('#main').css('font-size', '-=2');
          }
        }
        editor.refresh();
        replWidget.refresh();
        updateFontSizeMenuText();
      }
      function formatFontSizeMenuText(size) {
        return "Font size: " + Math.round(parseFloat(size));
      }
      function updateFontSizeMenuText() {
        $('#font-label').text(formatFontSizeMenuText($('#main').css("font-size")));
      }
      updateFontSizeMenuText();

      var curTheme = document.getElementById("theme-select").value;
      var themeSelect = $("#theme-select");

      function applyTheme(theme) {
        themeSelect.val(theme);
        $("body").removeClass(curTheme).addClass(theme);
        curTheme = theme;
      }

      applyTheme(curTheme);

      $("#theme").change(function(e) {
        var value = e.target.value;
        applyTheme(value);

        // track theme in local settings
        localSettings.setItem('theme', curTheme);
      });

      localSettings.change("theme", function(_, newTheme) {
        applyTheme(newTheme);
      });
      
      $('.notificationArea').click(function() {$('.notificationArea span').fadeOut(1000);});

      editor.cm.on('beforeChange', function(instance, changeObj){textHandlers.autoCorrect(instance, changeObj, editor.cm);});

      // Resizable
      var replHeight = $( "#REPL" ).height();
      var editorEvenSplit = true;
      $( "#REPL" ).resizable({
        maxHeight: replHeight,
        maxWidth: window.innerWidth - 128,
        minHeight: replHeight,
        minWidth: 100,
        handles: {"w": "#handle"}});

      $( "#REPL" ).on( "resize", leftResize);
      $( "#REPL" ).on( "resize", function() {editorEvenSplit = false;});

      function leftResize(event, ui) {
        var leftWidth = (window.innerWidth - ui.size.width)
        $(".replMain").css("width", leftWidth + "px");
        editor.refresh();
        replWidget.refresh();
      }

      $( "#REPL" ).on( "resizestop", toPercent);

      var rightResizePct = 50;
      var leftResizePct = 50;

      function toPercent(event, ui) {
        rightResizePct = (ui.size.width / window.innerWidth) * 100
        leftResizePct = 100 - rightResizePct
        setEditorSize(leftResizePct, rightResizePct);
      }

      $( window ).resize( function() {
        $( "#REPL" ).resizable( "option", "maxWidth", window.innerWidth - 128);
      });
      // End Resizable

      function setEditorSize(leftPct, rightPct) {
        $( "#REPL" ).css( "width", rightPct + "%");
        $( "#REPL" ).css( "left", leftPct + "%");
        $(".replMain").css("width", leftPct + "%");
      }

      function toggleEditorSize() {
        if(editorEvenSplit) {
          editorEvenSplit = false;
          setEditorSize(leftResizePct, rightResizePct);
        }
        else {
          editorEvenSplit = true;
          setEditorSize("50", "50");
        }
      }

      // save
      // On Mac mod ends up mapping to command+s whereas on Windows and Linux it maps to ctrl+s.
      // Saving has a special condition: when embedded we want the Ctrl-S to
      // propagate up. We could fire a special “save” event, but for contexts
      // like VScode it is nice to have the “real” Cmd-S event fire to get
      // good default behavior
      if(!PYRET_IS_EMBEDDED) {
        Mousetrap.bindGlobal('mod+s', function(e) {
          CPO.save();
          e.stopImmediatePropagation();
          e.preventDefault();
        });
      }

      // resize, Toggle sizing of the editor window between 50% and last resize
      Mousetrap.bindGlobal('ctrl+m', function(e){
        toggleEditorSize();
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      // run the definitions area
      Mousetrap.bindGlobal('ctrl+enter', function(e){
        doRunAction(editor.cm.getValue(), false);
        CPO.autoSave();
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      function reciteHelp() {
        CPO.sayAndForget(
          "Press Escape to exit help. " +
          "Control question mark: recite help. " +
          "Control s: save. " +
          "F6 and shift-F6: cycle focus through regions. " +
          "F7 or Control enter: run the code in the definitions window. " +
          "F11: insert image. " +
          "Control left: move cursor left by one word. " +
          "Control right: move cursor right by one word. " +
          "Alt left: if cursor is just before a right parenthesis or end keyword, " +
          "move left to matching delimiter, " +
          "otherwise move left by one word. " +
          "Alt right: like alt left, but move right. " +
          "Escape left: synonym for alt left, in case alt key is used by browser. " +
          "Escape right: synonym for alt right."
        );
      }

      // pull up help menu
      Mousetrap.bindGlobal('ctrl+shift+/', function(e) {
        $("#help-keys").fadeIn(100);
        reciteHelp();
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      $('#ctrl-question').click(function() {
        $('#help-keys').fadeIn(100);
        reciteHelp();
      });

      Mousetrap.bindGlobal('f6', function(e) {
        // cycle focus (forward)
        CPO.cycleFocus();
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      Mousetrap.bindGlobal('shift+f6', function(e) {
        // cycle focus backward
        CPO.cycleFocus(true);
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      Mousetrap.bindGlobal('shift+tab', function(e) {
        // cycle focus backward
        //console.log('mouse shift+tab')
        CPO.cycleFocus(true);
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      Mousetrap.bindGlobal('f7', function(e) {
        doRunAction(editor.cm.getValue(), false);
        CPO.autoSave();
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      Mousetrap.bindGlobal('f8', function(e) {
        $('#breakButton').click();
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      Mousetrap.bindGlobal('f9', function(e) {
        var sc = $('#shareContainer');
        if (sc) {
          var sl = sc[0].childNodes[0];
          sl.click();
        }
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      Mousetrap.bindGlobal('f11', function(e) {
        $('#insert').click();
        e.stopImmediatePropagation();
        e.preventDefault();
      });

      // Used for image definition naming (identifier: "img" + curImg)
      var curImg = 0;

      /**
       * Sets curImg to a value which will not clash with the code in
       * contents (note: this is done conservatively -- the estimation
       * is "dumb" in that it pays no attention to token types)
       */
      function inferCurImg(contents) {
        var query = /img([0-9]+)[\s\n\r]*=/g;
        var maxSoFar = 0;
        var res;
        while((res = query.exec(contents)) !== null) {
          maxSoFar = Math.max(maxSoFar, Number(res[1]));
        }
        curImg = maxSoFar + 1;
      }

      var photoPrompt = function(count) {
        var plural = count > 1;
        return new modalPrompt({
          title: "Import options",
          style: "radio",
          submitText: "Import",
          cancelText: "Close",
          options: [
            {
              message: "Value" + (plural ? "s" : ""),
              value: "values",
              example: 'image-url("<URL>")'
                + (plural ? '\n'
                      + (count > 2 ? '# ' + (count-2) + ' more...\n': '')
                      + 'image-url("<URL>")'
                    : '')
            },
            {
              message: "Definition" + (plural ? "s" : ""),
              value: "defs",
              example: 'image' + (plural ? '0' : '') + ' = image-url("<URL>")'
                + (plural ? 
                      (count > 2 ? '\n# ' + (count-2) + ' more...' : '')
                      + '\nimage' + (count-1) + ' = image-url("<URL>")'
                    : '')
            },
            {
              message: "List",
              value: "list",
              example: '[list: image-url("<URL>")'
                + (plural ? 
                      ',\n' + (count > 2 ? '       # ' + (count-2) + ' more...\n' : '')
                        + '       image-url("<URL>")]'
                    : ']')
            }]
        });
      }

      var lastSave = 0;
      function handlePickerData(documents, picker, drive) {
        // File loaded
        if (documents[0][picker.Document.TYPE] === "file") {
          var id = documents[0][picker.Document.ID];
          function load(here) {
            if(here) {

              window.CPO.save().then(function() {
                var p = drive.getFileById(id);

                window.CPO.showShareContainer(p);
                history.pushState(null, null, "#program=" + id);
                window.CPO.loadProgram(p).then(function(contents) {
                  window.CPO.editor.cm.setValue(contents);
                  window.CPO.editor.cm.clearHistory();
                });
              })
              .fail(function(err) {
                window.flashMessage("Currently unable to save, try opening that file in a new tab");
              });
            }
            else {
              window.open(window.APP_BASE_URL + "/editor#program=" + id, "_blank");
            }
          }
          function openFile(id) {
            var filePrompt = new modalPrompt({
                title: "Where would you like to open the file?",
                style: "tiles",
                hideSubmit: true,
                options: [
                  {
                    message: "Open here",
                    details: "The current file will be saved first",
                    on: {click: function() {
                      load(true);
                      filePrompt.onClose();
                    }}
                  },
                  {
                    message: "Open in new tab",
                    details: "The current file will remain open in this tab",
                    on: {click: function() {
                      load(false);
                      filePrompt.onClose();
                    }}
                  }]
              });
            filePrompt.show();
          }
          openFile(documents[0][picker.Document.ID]);
        }
        // Picture loaded
        else if (documents[0][picker.Document.TYPE] === picker.Type.PHOTO) {

          try {
            photoPrompt(documents.length).show(function(res) {
              // Name of event for CM undo history
              var origin = "+insertImage" + curImg;
              var asValues = (res === "values");
              var asDefs = (res === "defs");
              var asList = (res === "list");
              if (!(asValues || asDefs || asList)) {
                // Check for garbage and log it
                if (res !== null) {
                  console.warn("Unknown photoPrompt response: ", res);
                }
                return;
              }
              // http://stackoverflow.com/questions/23733455/inserting-a-new-text-at-given-cursor-position
              var cm = CPO.editor.cm;
              var doc = cm.getDoc();
              function placeInEditor(str) {
                var cursor = doc.getCursor();
                var line = doc.getLine(cursor.line);
                var pos = {
                  line: cursor.line,
                  ch: line.length
                };
                doc.replaceRange(str, pos, undefined, origin);
                reindent(cursor.line);
              }
              function reindent(line) {
                cm.indentLine(line || doc.getCursor().line);
              }
              function emitNewline() {
                var cursor = doc.getCursor();
                placeInEditor('\n');
                // FIXME: Dunno why this happens.
                if (cursor.line === doc.getCursor().line) {
                  doc.setCursor({line: cursor.line + 1, ch: 0});
                }
              }
              function emitLn(s) {
                placeInEditor(s);
                emitNewline();
              }
              function onEmptyLine() {
                var cursor = doc.getCursor("to");
                var line = doc.getLine(cursor.line);
                return (/^\s*$/.test(line));
              }
              // Make newline at cursor position if we are not on an empty line
              if (onEmptyLine()) {
                reindent();
              } else {
                emitNewline();
              }
              if (asList) {
                placeInEditor("[list:");
              }
              documents.forEach(function(d, idx) {
                var pathToImg = '"' + window.APP_BASE_URL + "/shared-image-contents?sharedImageId="
                  + d.id + '"';
                var outstr = asDefs ? ("img" + curImg + " = ") : "";
                ++curImg;
                outstr += "image-url(" + pathToImg + ")";
                var isLast = (idx === (documents.length - 1));
                if (asList) {
                  if (idx === 0) {
                    // The space after ":" gets eaten, so we need to enter it here
                    outstr = ' ' + outstr;
                  }
                  outstr += isLast ? "]" : ",";
                }
                if (isLast) {
                  placeInEditor(outstr);
                } else {
                  emitLn(outstr);
                }
              });
            });
          }
          catch(e) {
            console.error("The show() function failed: ", e);
          }
        } else {
          flashError("Invalid file type: " + documents[0][picker.Document.TYPE]);
        }
      }
      var insertPicker = new FilePicker({
        onLoaded: function() {
          $("#insert").attr("disabled", false);
          insertPicker.openOn($("#insert")[0], "click");
        },
        onSelect: handlePickerData,
        onError: flashError,
        onInternalError: stickError,
        views: ["imageView"],
        title: "Select images"
      });

      return runtime.makeModuleReturn({
        repl: runtime.makeOpaque(repl)
      }, {});
    }
  }
})
