define(["trove/image-lib", "./check-ui"], function(imageLib, checkUI) {
  function merge(obj, extension) {
    return _.merge(_.clone(obj), extension);
  }
  function makeEditor(container, options) {
    var initial = "";
    if (options.hasOwnProperty("initial")) {
      initial = options.initial;
    }

    var runButton = drawRunButton();
    if (options.run && !options.simpleEditor) {
      container.append(runButton);
      container.append(drawClearFix());
    }

    var textarea = jQuery("<textarea>");
    textarea.val(initial);
    container.append(textarea);

    var runFun = function (code, options) {};
    if (options.hasOwnProperty("run")) {
      runFun = function (code, replOptions) {
        options.run(code, {cm: CM}, replOptions);
      }
    }

    var useLineNumbers = !options.simpleEditor;

    if(options.cmOptions && options.cmOptions.gutters) {
      var optGutters = options.cmOptions.gutters;
      delete options.cmOptions.gutters;
    }
    else {
      var optGutters = [];
    }
    var cmOptions = {
      extraKeys: {
        "Shift-Enter": function(cm) { runFun(cm.getValue(), {check: true, "type-env": !options.simpleEditor }); },
        "Shift-Ctrl-Enter": function(cm) { runFun(cm.getValue(), {check: false, "type-env": !options.simpleEditor}); },
        "Tab": "indentAuto"
      },
      indentUnit: 2,
      tabSize: 2,
      viewportMargin: Infinity,
      lineNumbers: useLineNumbers,
      matchBrackets: true,
      lineWrapping: true,
      foldGutter: {
        rangeFinder: CodeMirror.fold.indent
      },
      gutters: optGutters.concat(["CodeMirror-foldgutter"])
    };

    cmOptions = merge(cmOptions, options.cmOptions);

    var CM = CodeMirror.fromTextArea(textarea[0], cmOptions);

    if (options.run) {
      runButton.on("click", function () {
        runFun(CM.getValue(), {check: true});
      });
    }


    return CM;
  }

  function formatCode(container, src) {
    CodeMirror.runMode(src, "pyret", container);
  }

  function makeHighlightingRunCode(runtime, codeRunner) {
    var image = imageLib(runtime, runtime.namespace);

    return function(src, uiOptions, options) {
      function highlightLineAt(cm, loc, className) {
        cm.addLineClass(
            loc.line - 1,
            'background',
            className
        );
      }
      function normalizeLoc(loc) {
        if(loc.path) { return { file: loc.path, line: loc.line, column: loc.column }; }
        if(loc.source) { return { file: loc.source, line: loc.line, column: loc.column }; }
        else if(!loc.file) {
          ct_exn("normalizeLoc: Doesn't look like a valid location", loc);
        }
        else { return loc; }
      }
      function showLink(loc) {
        return loc && loc.line > 0 && loc.file && loc.file === uiOptions.name;
      }
      function makeScrollingLocationLink(cm, loc) {
        function locToStr(loc) {
          return "In " + loc.file + ": Line " + loc.line + ", Column " + loc.column;
        }
        loc = normalizeLoc(loc);
        if(!showLink(loc)) { return $("<span>"); }
        var errorLink = $("<a>");
        errorLink.text(locToStr(loc));
        errorLink.attr("href", "#");
        errorLink.on("click", function(e) {
          clear();
          highlightLineAt(uiOptions.cm, loc, 'lineError');
          var coords = cm.charCoords({ line: loc.line - 1, ch: loc.column - 1 });
          if ((window.pageYOffset > coords.top) ||
              (window.pageYOffset < coords.top - window.innerHeight)) {
            $("body").animate({
              scrollTop: coords.top - (window.innerHeight / 2)
            });
          }
          e.preventDefault();
          return false;
        });
        return errorLink;
      }

      function clear() {
        uiOptions.cm.eachLine(function(l) {
          uiOptions.cm.removeLineClass(l, 'background', 'lineError');
          uiOptions.cm.removeLineClass(l, 'background', 'lineSuccess');
        });
      };

      uiOptions.cm.on("change", clear);

      function highlightingOnError(output) { return function(err) {
        ct_log(err);
        if (!runtime.isFailureResult(err)) {
          ct_err("Got a non-failure result in OnError handler: ", err);
        }
        else {
          var exn = err.exn;
          // compile errors
          if(exn instanceof Array) {
            output.append($("<div>").text(String(exn.map(function(e) { return runtime.toReprJS(e, "tostring"); }))));
          }
          else if('exn' in exn) {
            output.append($("<div>").text(String(exn)));
          } else {
            output.append($("<div>").text("An unexpected error occurred"));
          }
        }
      };}

      function highlightingCheckReturn(output) { return function(obj) {
        var successCount = 0;
        function drawSuccess(name, message, location) {
          var link = $("<span>");
          successCount += 1;
          if(location.value) {
            highlightLineAt(uiOptions.cm, location.value, 'lineSuccess');
            link = makeScrollingLocationLink(uiOptions.cm, location.value);
          }
          return $("<div>").text(name +  ": " + message)
            .addClass("check check-success")
            .append("<br/>");
        }
        function drawFailure(name, message, location) {
          var link = $("<span>");
          if(location.value) {
            highlightLineAt(uiOptions.cm, location.value, 'lineError');
            link = makeScrollingLocationLink(uiOptions.cm, location.value);
          }
          return $('<div>').text(name + ": " + message)
            .addClass("check check-failure")
            .append("<br/>")
            .append(link)
            .append("<br/>");
        }
        function drawException(name, exception, location) {
          if(typeof exception === "string") {
            return $('<div>')
              .addClass("check check-failure")
              .append($("<p>").text(name))
              .append($("<p>").text(exception))
              .append("<br/>")
              .append($("<span>").text("In check starting at:"))
              .append("<br/>")
              .append(makeScrollingLocationLink(uiOptions.cm, location.value));
          }
          var link = $("<span>");
          var loc = exception.trace[0] || location.value;
          if(loc) {
            highlightLineAt(uiOptions.cm, location.value, 'lineError');
            link = makeScrollingLocationLink(uiOptions.cm, location.value);
          }
          var traceDom = drawErrorLocations(
             exception.trace.map(function (l) {
               return makeScrollingLocationLink(uiOptions.cm,l)
             }));
          return $('<div>').text(name + ": " + exception.message)
            .addClass("check check-failure")
            .append("<br/>")
            .append($("<span>").text("In check starting at:"))
            .append("<br/>")
            .append(makeScrollingLocationLink(uiOptions.cm, location.value))
            .append("<br/>")
            .append($("<span>").text("Using (at least) these lines:"))
            .append("<br/>")
            .append(traceDom)
            .append("<br/>");
        }
        var answer = runtime.getField(obj.result, "answer");
        if(runtime.isOpaque(answer) && image.isImage(answer.val)) {
          var imageDom = output.append(answer.val.toDomNode());
          $(imageDom).trigger({type: 'afterAttach'});
          $('*', imageDom).trigger({type : 'afterAttach'});
        } else {
          output.append($("<div>").text(runtime.toReprJS(runtime.getField(obj.result, "answer"), "_torepr")));
        }
        var toCall = runtime.getField(runtime.getParam("current-checker"), "render");
        runtime.run(function(_, _) {
            return toCall.app();
          },
          runtime.namespace,
          {sync: false},
          function(result) {
            if(runtime.isSuccessResult(result) && runtime.isString(result.result)) {
              output.append($("<pre>").text(runtime.unwrap(result.result)));
            }
            else {
              output.append($("<div>").text(String(result.exn)));
            }
          });

        checkUI.drawCheckResults(output, uiOptions.cm, runtime, runtime.getField(obj.result, "checks"));

        console.log(JSON.stringify(obj.stats));

        return true;

  /*
        var blockResultsJSON = pyretMaps.pyretToJSON(obj);

        if(blockResultsJSON.results.length === 0) {
          var blockResultVal = pyretMaps.get(pyretMaps.toDictionary(obj), "val");
          whalesongFFI.callPyretFun(
              whalesongFFI.getPyretLib("Image"),
              [blockResultVal],
              function(isImage) {
                if(pyretMaps.pyretToJSON(isImage) === true) {
                  whalesongFFI.callRacketFun(whalesongFFI.getPyretLib("p:p-opaque-val"),
                      [blockResultVal],
                      function(imageVal) {
                        var imageDom = plt.runtime.toDomNode(imageVal, 'display');
                        output.append(imageDom);
                        $(imageDom).trigger({type: 'afterAttach'});
                        $('*', imageDom).trigger({type : 'afterAttach'});
                        output.append("<br/>");

                      },
                      function(fail) {
                        console.error("Failed to get opaque: ", fail); 
                      });
                } else {
                  whalesongFFI.callPyretFun(
                      whalesongFFI.getPyretLib("torepr"),
                      [pyretMaps.get(pyretMaps.toDictionary(obj), "val")],
                      function(s) {
                        var str = pyretMaps.getPrim(s);
                        output.append(jQuery("<pre class='repl-output'>").text(str));
                        output.append(jQuery('<br/>'));
                      }, function(e) {
                        ct_err("Failed to tostring: ", result);
                      });

                }
              });
          return true;
        }

        var somethingFailed = false;
        blockResultsJSON.results.map(function(result) {
          result.map(function(checkBlockResult) {
            var container = $("<div>");
            var errorLink;
            var name = checkBlockResult.name;
            container.append($("<p>").text(name + ": Avast, there be bugs!"));
            container.addClass("check-block");
            var messageText = "";
            console.log(checkBlockResult);
            if (checkBlockResult.err) {
              somethingFailed = true;
              if (checkBlockResult.err.message) {
                messageText = checkBlockResult.err.message;
              }
              else {
                messageText = checkBlockResult.err;
              }
              if(checkBlockResult.err.trace) {
                var loc = checkBlockResult.err.trace[0] || checkBlockResult.err.location;
                if(loc) {
                  errorLink = makeScrollingLocationLink(uiOptions.cm, loc);
                  container.append(drawErrorMessageWithLoc(messageText, errorLink));
                }
                else {
                  container.append(drawErrorMessage(messageText));
                }
              } else {
                container.append(drawErrorMessage(messageText));
              }
              container.addClass("check-block-failed");
            }
            checkBlockResult.results.forEach(function(individualResult) {
              if ("reason" in individualResult) {
                somethingFailed = true;
                container.append(
                  drawFailure(
                      individualResult.name,
                      individualResult.reason,
                      individualResult.location
                    ));
              } else if ("exception" in individualResult) {
                somethingFailed = true;
                container.append(
                  drawException(
                      individualResult.name,
                      individualResult.exception,
                      individualResult.location
                    ));
              } else {
                var successDiv = drawSuccess(
                    individualResult.name,
                    "Success!",
                    individualResult.location
                );
                if(!individualResult.location.value) {
                  container.append(successDiv);
                }
              }
            });
            if(somethingFailed) {
              output.append(container);
            }
          });
        });
        if(!somethingFailed) {
          var container = $("<div>")
            .addClass("check-block")
            .addClass("check-block-success");
          var text = "";
          if(successCount === 1) {
            text = "Your test passed, mate!";
          }
          else {
            text = "All " + successCount + " tests passed, mate!";
          }
          container.append($("<p>").text(text));
          output.append(container);
        }
        return true;
  */
      };}


      var theseUIOptions = merge(uiOptions, {
          wrappingOnError: highlightingOnError
      });
      theseUIOptions.wrappingReturnHandler = highlightingCheckReturn;
      //clear();
      codeRunner(src, theseUIOptions, options);
    }
  }

  //: -> (code -> printing it on the repl)
  function makeRepl(container, repl, runtime) {
    var items = [];
    var pointer = -1;
    var current = "";
    function loadItem() {
      CM.setValue(items[pointer]);
    }
    function saveItem() {
      items.unshift(CM.getValue());
    }
    function prevItem() {
      if (pointer === -1) {
        current = CM.getValue();
      }
      if (pointer < items.length - 1) {
        pointer++;
        loadItem();
      }
    }
    function nextItem() {
      if (pointer >= 1) {
        pointer--;
        loadItem();
      } else if (pointer === 0) {
        CM.setValue(current);
        pointer--;
      }
    }

    var promptContainer = jQuery("<div id='prompt-container'>");
    var prompt = jQuery("<div>").addClass("repl-prompt");
    promptContainer.append(prompt);

    var output = jQuery("<div id='output' class='cm-s-default'>");
    runtime.setStdout(function(str) {
        ct_log(str);
        output.append($("<div>").text(str));
      });
    runtime.setParam("current-animation-port", function(dom) {
        output.append(dom);
      });

    var clearDiv = jQuery("<div class='clear'>");

    var clearRepl = function() {
      output.empty();
      promptContainer.hide();
      promptContainer.fadeIn(100);
      lastNameRun = 'interactions';
      lastEditorRun = null;
    };

    var clearButton = $("<button>").addClass("blueButton").text("Clear")
      .click(clearRepl);
    var breakButton = $("<button>").addClass("blueButton").text("Stop");
    container.append(clearButton).append(breakButton).append(output).append(promptContainer).
      append(clearDiv);


    var runCode = makeHighlightingRunCode(runtime, function (src, uiOptions, options) {
      breakButton.attr("disabled", false);
      CM.setOption("readOnly", "nocursor");
      CM.getDoc().eachLine(function (line) {
        CM.addLineClass(line, 'background', 'cptteach-fixed');
      });
      output.empty();
      promptContainer.hide();
      promptContainer.fadeIn(100);
      var defaultReturnHandler = options.check ? checkModePrettyPrint : prettyPrint;
      var thisReturnHandler;
      if (uiOptions.wrappingReturnHandler) {
        thisReturnHandler = uiOptions.wrappingReturnHandler(output);
        console.log("Using the wrapping return handler");
      } else {
        console.log("Using default return handler");
        thisReturnHandler = uiOptions.handleReturn || defaultReturnHandler;
      }
      var thisError;
      if (uiOptions.wrappingOnError) {
        thisError = uiOptions.wrappingOnError(output);
      } else {
        thisError = uiOptions.error || onError;
      }
      var thisWrite = uiOptions.write || write;
      lastNameRun = uiOptions.name || "interactions";
      lastEditorRun = uiOptions.cm || null;
      evaluator.runMain(uiOptions.name || "run", src, clear, enablePrompt(thisReturnHandler), thisWrite, enablePrompt(thisError), options);
    });

    var enablePrompt = function (handler) { return function (result) {
        breakButton.attr("disabled", true);
        CM.setValue("");
        CM.setOption("readOnly", false);
        CM.getDoc().eachLine(function (line) {
          CM.removeLineClass(line, 'background', 'cptteach-fixed');
        });
        output.get(0).scrollTop = output.get(0).scrollHeight;
        return handler(result);
      };
    }

    var runner = makeLoggingRunCode(function(code, opts, replOpts){
          items.unshift(code);
          pointer = -1;
          var echoContainer = $("<div>");
          var echo = $("<textarea class='repl-echo CodeMirror'>");
          echoContainer.append(echo);
          write(echoContainer);
          var echoCM = CodeMirror.fromTextArea(echo[0], { readOnly: 'nocursor' });
          echoCM.setValue(code);
          write(jQuery('<br/>'));
          breakButton.attr("disabled", false);
          CM.setOption("readOnly", "nocursor");
          CM.getDoc().eachLine(function (line) {
            CM.addLineClass(line, 'background', 'cptteach-fixed');
          });
          makeHighlightingRunCode(runtime, function(src, uiOptions, options) {
            evaluator.runRepl('interactions',
                        src,
                        clear,
                        enablePrompt(uiOptions.wrappingReturnHandler(output)),
                        write,
                        enablePrompt(uiOptions.wrappingOnError(output)),
                        merge(options, _.merge(replOpts, {check: true})));
          })(code, merge(opts, {name: lastNameRun, cm: lastEditorRun || echoCM}), replOpts);
        },
        "interactions");

    var CM = makeEditor(prompt, {
      simpleEditor: true,
      run: runner,
      initial: "",
      cmOptions: {
        extraKeys: {
          'Enter': function(cm) { runner(cm.getValue(), {cm: cm}, {check: true, "type-env": false }); },
          'Shift-Enter': "newlineAndIndent",
          'Up': prevItem,
          'Down': nextItem,
          'Ctrl-Up': "goLineUp",
          'Ctrl-Alt-Up': "goLineUp",
          'Ctrl-Down': "goLineDown",
          'Ctrl-Alt-Down': "goLineDown"
        }
      }
    });

    var lastNameRun = 'interactions';
    var lastEditorRun = null;

    var write = function(dom) {
      output.append(dom);
    };

    var clear = function() {
      allowInput(CM, true)();
    };

    var onError = function(err, editor) {
      ct_log("onError: ", err);
      if (err.message) {
        write(jQuery('<span/>').css('color', 'red').append(err.message));
        write(jQuery('<br/>'));
      }
      clear();
    };

    var prettyPrint = function(result) {
      if (result.hasOwnProperty('_constructorName')) {
        switch(result._constructorName.val) {
        case 'p-num':
        case 'p-bool':
        case 'p-str':
        case 'p-object':
        case 'p-fun':
        case 'p-method':
          whalesongFFI.callPyretFun(
              whalesongFFI.getPyretLib("torepr"),
              [result],
              function(s) {
                var str = pyretMaps.getPrim(s);
                write(jQuery("<pre class='repl-output'>").text(str));
                write(jQuery('<br/>'));
              }, function(e) {
                ct_err("Failed to tostring: ", result);
              });
          return true;
        case 'p-nothing':
          return true;
        default:
          return false;
        }
      } else {
        console.log(result);
        return false;
      }
    };

    var checkModePrettyPrint = function(obj) {
      function drawSuccess(name, message) {
        return $("<div>").text(name +  ": " + message)
          .addClass("check check-success")
          .append("<br/>");
      }
      function drawFailure(name, message) {
        return $('<div>').text(name + ": " + message)
          .addClass("check check-failure")
          .append("<br/>");
      }
      var dict = pyretMaps.toDictionary(obj);
      var blockResults = pyretMaps.toDictionary(pyretMaps.get(dict, "results"));
      function getPrimField(v, field) {
        return pyretMaps.getPrim(pyretMaps.get(pyretMaps.toDictionary(v), field));
      }

      pyretMaps.map(blockResults, function(result) {
        pyretMaps.map(pyretMaps.toDictionary(result), function(checkBlockResult) {
          var cbDict = pyretMaps.toDictionary(checkBlockResult);
          var container = $("<div>");
          var message = $("<p>");
          var name = getPrimField(checkBlockResult, "name");
          container.append("<p>").text(name);
          container.append(message);
          container.addClass("check-block");
          if (pyretMaps.hasKey(cbDict, "err")) {
            var messageText = pyretMaps.get(cbDict, "err");
            if (pyretMaps.hasKey(pyretMaps.toDictionary(messageText), "message")) {
              messageText = getPrimField(pyretMaps.get(cbDict, "err"), "message");
            } else {
              messageText = pyretMaps.getPrim(pyretMaps.get(cbDict, "err"));
            }
            message.text("Check block ended in error: " + messageText);
            container.css({
              "background-color": "red"
            });
          }


          pyretMaps.map(pyretMaps.toDictionary(pyretMaps.get(pyretMaps.toDictionary(checkBlockResult), "results")), function(individualResult) {
            if (pyretMaps.hasKey(pyretMaps.toDictionary(individualResult), "reason")) {
              container.append(drawFailure(
                  getPrimField(individualResult, "name"),
                  getPrimField(individualResult, "reason")));
            } else {
              container.append(drawSuccess(
                  getPrimField(individualResult, "name"),
                  "Success!"));
            }
          });
          output.append(container);
        });
      });
      return true;
    }

    var evaluator = makeEvaluator(container, repl, runtime, prettyPrint);


    var onBreak = function() {
      breakButton.attr("disabled", true);
      evaluator.requestBreak(function(restarter) {
          restarter.break();
        });
    };


    var allowInput = function(CM, clear) { return function() {
      if (clear) {
        CM.setValue("");
      }

      CM.setOption("readOnly", false);;
      CM.getDoc().eachLine(function (line) {
        CM.removeLineClass(line, 'background', 'cptteach-fixed');
      });
      breakButton.attr("disabled", true);

      CM.focus();
    } };

    var onReset = function() {
      evaluator.requestReset(function() {
        output.empty();
        clear();
      });
    };



    breakButton.attr("disabled", true);
    breakButton.click(onBreak);

    return runCode;
  }

  function makeEvaluator(container, repl, runtime, handleReturnValue) {
    var runMainCode = function(name, src, afterRun, returnHandler, writer, onError, options) {
      repl.restartInteractions(src).then(function(result) {
        if(runtime.isSuccessResult(result)) {
          returnHandler(result);
        } else {
          onError(result);
        }
      });
    };

    var runReplCode = function(name, src, afterRun, returnHandler, writer, onError, options) {
      repl.run(src).then(function(result) {
        if(runtime.isSuccessResult(result)) {
          returnHandler(result);
        } else {
          onError(result);
        }
      });
    };

    var breakFun = function(afterBreak) {
      repl.pause(afterBreak);
    };

    var resetFun = function(afterReset) {
      repl.restartInteractions("").then(afterReset);
    };

    return {runMain: runMainCode, runRepl: runReplCode, requestBreak: breakFun, requestReset: resetFun};
  }

  function namedRunner(runFun, name) {
    return function(src, uiOptions, langOptions) {
      runFun(src, merge(uiOptions, { name: name }), langOptions);
    };
  }

  function makeLoggingRunCode(codeRunner, name) {
    return codeRunner;
/*
    var toLog = [];
    function codeLog(src, uiOpts, replOpts) {
      toLog.push({name: name, url: String(window.location), src: src, name: uiOpts.name, time: String(Date.now())});
    }
    function sendLog() {
      if(toLog.length > 0) {
        $.ajax("/notification/code_run", {
          type: "POST",
          dataType: "json",
          data: {run_events: JSON.stringify(toLog)}
        });
        toLog = [];
      }
      window.setTimeout(sendLog, 30000);
    }
    sendLog();
    
    return namedRunner(function(src, uiOpts, replOpts) {
      codeLog(src, uiOpts, replOpts);
      codeRunner(src, uiOpts, replOpts);
    }, name);
*/
  }
  
  return {
    makeRepl: makeRepl,
    makeEditor: makeEditor
  };


});
