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
      output.append($("<div>").text(runtime.toReprJS(runtime.getField(obj.result, "answer"), "_torepr")));
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
    if (options.check) {
      theseUIOptions.wrappingReturnHandler = highlightingCheckReturn;
    }
    clear();
    codeRunner(src, theseUIOptions, options);
  }
}
