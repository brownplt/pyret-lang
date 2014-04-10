define(["js/ffi-helpers", "trove/srcloc", "trove/error", "compiler/compile-structs.arr", "trove/image-lib"], function(ffiLib, srclocLib, errorLib, csLib, imageLib) {

  function drawError(container, editor, runtime, exception) {
    var ffi = ffiLib(runtime, runtime.namespace);
    var image = imageLib(runtime, runtime.namespace);
    var cases = ffi.cases;
    runtime.loadModules(runtime.namespace, [srclocLib, errorLib, csLib], function(srcloc, error, cs) {
      var get = runtime.getField;
      function mkPred(pyretFunName) {
        return function(val) { return get(error, pyretFunName).app(val); }
      }

      var isRuntimeError = mkPred("RuntimeError");

      // Exception will be one of:
      // - an Array of compileErrors,
      // - a PyretException with a stack and a Pyret value error
      // - something internal and JavaScripty, which we don't want
      //   users to see but will have a hard time ruling out

      if(exception instanceof Array) {
        drawCompileErrors(exception);
      } else if(runtime.isPyretException(exception)) {
        drawPyretException(exception);
      } else {
        drawUnknownException(exception);
      }

      function highlightSrcloc(s, withMarker) {
        return runtime.safeCall(function() {
          return cases(get(srcloc, "Srcloc"), "Srcloc", s, {
            "builtin": function(_) { /* no-op */ },
            "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
              var marker = editor.markText({ line: startL - 1, ch: startC }, { line: endL - 1, ch: endC }, { className: "error-highlight" });
              return marker;
            }
          })
        }, withMarker);
      }
      function mapK(inList, f, k, outList) {
        if (inList.length === 0) { k(outList); }
        else {
          var newInList = inList.slice(1, inList.length);
          f(inList[0], function(v) {
            mapK(newInList, f, k, (outList || []).concat([v]))
          });
        }
      }
      function hoverLocs(elt, locs) {
        // CLICK to *cycle* through locations
        var marks = [];
        elt.on("mouseenter", function() {
          mapK(locs, highlightSrcloc, function(ms) {
            marks = marks.concat(ms);
          });
        });
        elt.on("mouseleave", function() {
          marks.forEach(function(m) { m.clear(); })
          marks = [];
        });
      }

      function drawSrcloc(s) {
        return $("<span>").addClass("srcloc").text(get(s, "format").app(true))
      }

      function drawCompileErrors(e) {
        function drawShadowId(id, newLoc, oldLoc) {
          var dom = $("<div>").addClass("compile-error");
          cases(get(srcloc, "Srcloc"), "Srcloc", oldLoc, {
            "builtin": function(_) {
              var p = $("<p>");
              p.append("Oops!  The name ");
              p.append($("<span>").addClass("code").text(id));
              p.append(" is taken by Pyret, and your program isn't allowed to define it.  You need to pick a different name for ");
              p.append($("<span>").addClass("code").text(id));
              p.append(" at ");
              p.append(drawSrcloc(newLoc));
              dom.append(p);
              hoverLocs(dom, [newLoc]);
              container.append(dom);
            },
            "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
              var p = $("<p>");
              p.append("It looks like you defined the name ");
              p.append($("<span>").addClass("code").text(id));
              p.append(" twice, at ");
              var loc1 = drawSrcloc(oldLoc);
              var loc2 = drawSrcloc(newLoc);
              var p2 = $("<p>");
              p2.text("You need to pick a new name for one of them");
              dom.append(p).append("<br>").append(loc1).append("<br>").append(loc2).append("<br>").append(p2);
              hoverLocs(dom, [oldLoc, newLoc]);
              container.append(dom);
            }
          });
        }

        function drawWfError(msg, loc) {
          var dom = $("<div>").addClass("compile-error");
          dom.append("<p>").text(msg);
          dom.append("<br>");
          dom.append(drawSrcloc(loc));
          hoverLocs(dom, [loc]);
          container.append(dom);
        }

        function drawWfErrSplit(msg, locs) {
          var dom = $("<div>").addClass("compile-error");
          dom.append("<p>").text(msg);
          dom.append("<br>")
          var locArray = ffi.toArray(locs)
          locArray.forEach(function(l) {
            dom.append(drawSrcloc(l)).append("<br>");
          });
          hoverLocs(dom, locArray);
          container.append(dom);
        }

        function drawErrorToString(e) {
          return function() {
            runtime.safeCall(function() {
              return get(e, "tostring").app()
            }, function(s) {
              container.append($("<div>").addClass("compile-error").text(s));
            });
          };
        }

        function drawCompileError(e) {
          cases(get(cs, "CompileError"), "CompileError", e, {
              "shadow-id": drawShadowId,
              "wf-err": drawWfError,
              "wf-err-split": drawWfErrSplit,
              "else": drawErrorToString(e)
            });
        }
        e.forEach(drawCompileError);
      }

      function getDomValue(v, f) {
        if(runtime.isOpaque(v) && image.isImage(v.val)) {
          f(v.val.toDomNode());
        } else {
          runtime.safeCall(function() {
            return runtime.toReprJS(v, "_torepr")
          }, function(str) {
            f($("<div>").text(str));
          });
        }
      }

      function drawPyretException(e) {
        function drawRuntimeErrorToString(e) {
          return function() {
            container.append($("<div>").text(String(e)));
          }
        }
        function drawGenericTypeMismatch(value, type) {
          // TODO(joe): How to improve this search?
          var srclocStack = e.pyretStack.map(runtime.makeSrcloc);
          var isSrcloc = function(s) { return runtime.unwrap(get(srcloc, "is-srcloc").app(s)); }
          var userLocs = srclocStack.filter(function(l) { return l && isSrcloc(l); });
          var probablyErrorLocation = userLocs[0];
          var dom = $("<div>").addClass("compile-error");
          getDomValue(value, function(valDom) {
            dom.append($("<p>").text("Expected to get a " + type + " as an argument, but got this instead: "));
            dom.append($("<br>"));
            dom.append(valDom);
            $(valDom).trigger({type: 'afterAttach'});
            $('*', valDom).trigger({type : 'afterAttach'});
            container.append(dom);
            hoverLocs(dom, [probablyErrorLocation]);
          });
        }
        function drawPyretRuntimeError() {
          cases(get(error, "RuntimeError"), "RuntimeError", e.exn, {
              "generic-type-mismatch": drawGenericTypeMismatch,
              "else": drawRuntimeErrorToString(e)
            });
        }
        if(!runtime.isObject(e.exn)) {
          drawRuntimeErrorToString(e)();
        }
        else if(mkPred("RuntimeError")(e.exn)) {
          drawPyretRuntimeError();
        } else {
          drawRuntimeErrorToString(e);
        }
      }

      function drawUnknownException(e) {
        container.append($("<div>").text("An unexpected error occurred: " + String(e)));
      }


    });
  }

  return {
    drawError: drawError
  }

});

