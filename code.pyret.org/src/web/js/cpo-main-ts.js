/*
  TS-compiler flavor of cpo-main.js: the same editor/REPL UI, but with
  compilation done by the TypeScript compiler bundle (window.PyretTSCompiler)
  instead of the Pyret-hosted compiler modules. Execution still goes
  through the builtin load-lib against the page realm, so results,
  check blocks, and runtime errors render exactly as in cpo-main.js.

  The long list of builtin requires below exists so that the built
  ts jarr instantiates the same builtin:// realm as cpo-main.jarr:
  user programs compiled at runtime link against these instances.

  The UI sections of this file (the top of theModule and everything in
  withRepl) are copied verbatim from cpo-main.js; the compiler plumbing
  in between is TS-specific (see ts-compiler-lib.js).
*/
({
  requires: [
    { "import-type": "builtin",
      name: "runtime-lib"
    },
    { "import-type": "builtin",
      name: "load-lib"
    },
    { "import-type": "dependency",
      protocol: "js-file",
      args: ["./ts-compiler-lib"]
    },
    { "import-type": "dependency",
      protocol: "js-file",
      args: ["./repl-ui"]
    },
    { "import-type": "dependency",
      protocol: "js-file",
      args: ["./text-handlers"]
    },
    { "import-type": "builtin", name: "arrays" },
    { "import-type": "builtin", name: "ast" },
    { "import-type": "builtin", name: "base" },
    { "import-type": "builtin", name: "builtin-modules" },
    { "import-type": "builtin", name: "chart" },
    { "import-type": "builtin", name: "chart-lib" },
    { "import-type": "builtin", name: "charts" },
    { "import-type": "builtin", name: "charts-lib" },
    { "import-type": "builtin", name: "charts-util" },
    { "import-type": "builtin", name: "checker" },
    { "import-type": "builtin", name: "color" },
    { "import-type": "builtin", name: "constants" },
    { "import-type": "builtin", name: "contracts" },
    { "import-type": "builtin", name: "cpo-builtins" },
    { "import-type": "builtin", name: "csv" },
    { "import-type": "builtin", name: "csv-lib" },
    { "import-type": "builtin", name: "d3-lib" },
    { "import-type": "builtin", name: "d3-lib-list" },
    { "import-type": "builtin", name: "data-source" },
    { "import-type": "builtin", name: "dcic2024" },
    { "import-type": "builtin", name: "either" },
    { "import-type": "builtin", name: "empty-context" },
    { "import-type": "builtin", name: "equality" },
    { "import-type": "builtin", name: "error" },
    { "import-type": "builtin", name: "error-display" },
    { "import-type": "builtin", name: "essentials2020" },
    { "import-type": "builtin", name: "essentials2021" },
    { "import-type": "builtin", name: "essentials2024" },
    { "import-type": "builtin", name: "fetch" },
    { "import-type": "builtin", name: "ffi" },
    { "import-type": "builtin", name: "file" },
    { "import-type": "builtin", name: "filelib" },
    { "import-type": "builtin", name: "filesystem" },
    { "import-type": "builtin", name: "filesystem-internal" },
    { "import-type": "builtin", name: "format" },
    { "import-type": "builtin", name: "gdrive-sheets" },
    { "import-type": "builtin", name: "global" },
    { "import-type": "builtin", name: "image" },
    { "import-type": "builtin", name: "image-lib" },
    { "import-type": "builtin", name: "image-structs" },
    { "import-type": "builtin", name: "image-typed" },
    { "import-type": "builtin", name: "internal-image-shared" },
    { "import-type": "builtin", name: "internal-image-typed" },
    { "import-type": "builtin", name: "internal-image-untyped" },
    { "import-type": "builtin", name: "json" },
    { "import-type": "builtin", name: "json-structs" },
    { "import-type": "builtin", name: "lists" },
    { "import-type": "builtin", name: "make-image" },
    { "import-type": "builtin", name: "math" },
    { "import-type": "builtin", name: "matrices" },
    { "import-type": "builtin", name: "matrix-util" },
    { "import-type": "builtin", name: "multiple-regression" },
    { "import-type": "builtin", name: "option" },
    { "import-type": "builtin", name: "parse-pyret" },
    { "import-type": "builtin", name: "pathlib" },
    { "import-type": "builtin", name: "pick" },
    { "import-type": "builtin", name: "plot" },
    { "import-type": "builtin", name: "plot-lib" },
    { "import-type": "builtin", name: "plot-lib-list" },
    { "import-type": "builtin", name: "plot-list" },
    { "import-type": "builtin", name: "pprint" },
    { "import-type": "builtin", name: "reactor-events" },
    { "import-type": "builtin", name: "reactors" },
    { "import-type": "builtin", name: "render-error-display" },
    { "import-type": "builtin", name: "s-exp" },
    { "import-type": "builtin", name: "s-exp-structs" },
    { "import-type": "builtin", name: "sets" },
    { "import-type": "builtin", name: "sha" },
    { "import-type": "builtin", name: "source-map-lib" },
    { "import-type": "builtin", name: "srcloc" },
    { "import-type": "builtin", name: "starter2024" },
    { "import-type": "builtin", name: "statistics" },
    { "import-type": "builtin", name: "str-dict" },
    { "import-type": "builtin", name: "string-dict" },
    { "import-type": "builtin", name: "table" },
    { "import-type": "builtin", name: "tables" },
    { "import-type": "builtin", name: "timing" },
    { "import-type": "builtin", name: "type-logger" },
    { "import-type": "builtin", name: "valueskeleton" },
    { "import-type": "builtin", name: "vector-util" },
    { "import-type": "builtin", name: "world" },
    { "import-type": "builtin", name: "world-lib" }
  ],
  nativeRequires: [
    "cpo/cpo-builtin-modules",
    "cpo/modal-prompt"
  ],
  provides: {
    values: {
      "repl": "tany"
    }
  },
  theModule: function(runtime, namespace, uri,
                      runtimeLib, loadLib, tsLib, replUI, textHandlers
                      /* , ...parity builtins, cpoModules, modalPrompt */) {
    // The parity builtin requires and the native requires arrive
    // positionally after the named parameters; only the natives are used.
    var allArgs = Array.prototype.slice.call(arguments);
    var cpoModules = allArgs[allArgs.length - 2];
    var modalPrompt = allArgs[allArgs.length - 1];

    var replContainer = $("<div>").addClass("repl");
    replContainer.attr("tabindex", "-1").attr('role', 'application');
    //replContainer.attr("aria-hidden", "true");
    $("#REPL").append(replContainer);

    var logDetailedOption = $("#detailed-logging");

    if(localSettings.getItem('log-detailed') !== null) {
      logDetailedOption.prop("checked",
        localSettings.getItem('log-detailed') == 'true');
    } else {
      localSettings.setItem('log-detailed', false);
    }

    logDetailedOption.on('change', function () {
      localSettings.setItem('log-detailed', this.checked);
    });

    localSettings.change("log-detailed", function(_, newValue) {
      logDetailedOption[0].checked = newValue == 'true';
      logDetailedOption.attr('aria-pressed', '' + (newValue == 'true'));
    });

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

    // NOTE(joe): This line is "cheating" by mixing runtime levels,
    // and uses the same runtime for the compiler and running code.
    // Usually you can only get a new Runtime by calling create, but
    // here we magic the current runtime into one.
    var pyRuntime = gf(gf(runtimeLib, "internal").brandRuntime, "brand").app(
      runtime.makeObject({
        "runtime": runtime.makeOpaque(runtime)
      }));
    var pyRealm = gf(loadLib, "internal").makeRealm(cpoModules.getRealm());

    var T = tsLib.tsCompiler();
    var builtinSupport = tsLib.makeBuiltinSupport(runtime.getParam("staticModules"));
    var executor = tsLib.makeExecutor(pyRuntime);

    // Mirrors the stock editor's finder configuration (cpo-main.js): the
    // url-file mode comes from the host page, and the root compile context
    // seeds the load-path at "." (the open tab's directory, which is what
    // the embedding host resolves every fs path against).
    var urlFileMode = window.URL_FILE_MODE || "all-remote";
    var tsRepl = T.repl.makeRepl(
      executor,
      builtinSupport.loadables,
      pyRealm,
      { "load-path": "." },
      tsLib.makeFinderFactory(builtinSupport, urlFileMode));

    function tsOptions(options) {
      var o = Object.assign({}, T.compileStructs.defaultCompileOptions);
      o.checkMode = true;
      o.checks = "main";
      o.checkAll = !!options.checkAll;
      o.typeCheck = !!options.typeCheck;
      o.displayProgress = false;
      o.log = function(_s, _toClear) {};
      o.logError = function(s) { console.error(s); };
      o.onCompile = function(_locator, loadable, _trace) { return loadable; };
      return o;
    }

    var currentCancellation = null;
    function newCancellation() {
      currentCancellation = T.repl.makeCancellation();
      return currentCancellation;
    }

    /*
      A cancelled run is a user break that happened to land before the program
      became a Pyret computation, so it is reported as one: the same
      ffi.userBreak the runtime raises.
    */
    function reportRunEnd(ret, err) {
      if (err instanceof T.repl.Cancelled) { tsLib.resolveWithUserBreak(ret); }
      else { tsLib.resolveWithError(ret, err); }
    }

    var jsRepl = {
      runtime: runtime,
      /*
        This should not be called while a Pyret stack is running
      */
      restartInteractions: function(source, options) {
        var ret = Q.defer();
        var cancel = newCancellation();
        setTimeout(function() {
          var opts = tsOptions(options);
          var defsLocator = tsRepl.makeDefinitionsLocator(
            function() { return source; },
            T.compileStructs.standardGlobals);
          tsRepl.restartInteractions(defsLocator, opts, cancel)
            .then(function(either) { tsLib.resolveWithEither(ret, either); })
            .catch(function(err) { reportRunEnd(ret, err); });
        }, 0);
        return ret.promise;
      },
      run: function(str, name) {
        var ret = Q.defer();
        var cancel = newCancellation();
        setTimeout(function() {
          var locator = tsRepl.makeInteractionLocator(function() { return str; });
          tsRepl.runInteraction(locator, cancel)
            .then(function(either) { tsLib.resolveWithEither(ret, either); })
            .catch(function(err) { reportRunEnd(ret, err); });
        }, 0);
        return ret.promise;
      },
      pause: function(afterPause) {
        runtime.schedulePause(function(resumer) {
          afterPause(resumer);
        });
      },
      /*
        breakAll() kills all Pyret computation; the cancellation
        is for pure JS computation (the compiler) that is no longer
        Pyret code on a break-able Pyret stack
      */
      stop: function() {
        if (currentCancellation !== null) { currentCancellation.cancel(); }
        runtime.breakAll();
      }
    };

    return withRepl(jsRepl);

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
