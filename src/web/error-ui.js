define(["js/ffi-helpers", "trove/srcloc", "trove/error", "compiler/compile-structs.arr", "trove/image-lib", "./output-ui"], function(ffiLib, srclocLib, errorLib, csLib, imageLib, outputUI) {

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

      function cmPosFromSrcloc(s) {
        return cases(get(srcloc, "Srcloc"), "Srcloc", s, {
          "builtin": function(_) { throw new Error("Cannot get CodeMirror loc from builtin location"); },
          "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
            var extraCharForZeroWidthLocs = endCh === startCh ? 1 : 0;
            return {
              start: { line: startL - 1, ch: startC },
              end: { line: endL - 1, ch: endC + extraCharForZeroWidthLocs }
            };
          }
        });
      }

      function highlightSrcloc(s, withMarker) {
        return runtime.safeCall(function() {
          return cases(get(srcloc, "Srcloc"), "Srcloc", s, {
            "builtin": function(_) { /* no-op */ },
            "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
              var cmLoc = cmPosFromSrcloc(s);
              var marker = editor.markText(
                cmLoc.start,
                cmLoc.end,
                { className: "error-highlight" });
              return marker;
            }
          })
        }, withMarker);
      }
      function mapK(inList, f, k, outList) {
        if (inList.length === 0) { k(outList || []); }
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
          var curLoc = locs[locIndex];
          var view = editor.getScrollInfo();
          cases(get(srcloc, "Srcloc"), "Srcloc", curLoc, {
            "builtin": function(_) { },
            "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
              var charCh = editor.charCoords(cmPosFromSrcloc(curLoc).start, "local");
              if (view.top > charCh.top) {
                jQuery(".warning-upper").fadeIn("fast");
              } else if (view.top + view.clientHeight < charCh.bottom) {
                jQuery(".warning-lower").fadeIn("fast");
              }
            }
          });
          mapK(locs, highlightSrcloc, function(ms) {
            marks = marks.concat(ms);
          });
        });
        elt.on("mouseleave", function() {
          jQuery(".warning-upper").fadeOut("fast");
          jQuery(".warning-lower").fadeOut("fast");
          marks.forEach(function(m) { return m && m.clear(); })
          marks = [];
        });
        var locIndex = 0;
        if (locs.filter(function(e) { return runtime.isObject(e) && get(srcloc, "is-srcloc").app(e); }).length > 0) {
          elt.on("click", function() {
            jQuery(".warning-upper").fadeOut("fast");
            jQuery(".warning-lower").fadeOut("fast");
            function gotoNextLoc() {
              var curLoc = locs[locIndex];
              function rotateLoc() { locIndex = (locIndex + 1) % locs.length; }
              
              return cases(get(srcloc, "Srcloc"), "Srcloc", curLoc, {
                "builtin": function(_) { rotateLoc(); gotoNextLoc(); },
                "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
                  editor.scrollIntoView(cmPosFromSrcloc(curLoc).start, 100);
                  rotateLoc();
                }
              });
            }
            gotoNextLoc();
          });
        }
      }

      function drawSrcloc(s) {
        return s ? $("<span>").addClass("srcloc").text(get(s, "format").app(true)) : $("<span>");
      }

      function drawCompileErrors(e) {
        function drawUnboundId(idExpr) {
          var dom = $("<div>").addClass("compile-error");
          var name = get(get(idExpr, "id"), "toname").app();
          var loc = get(idExpr, "l");
          cases(get(srcloc, "Srcloc"), "Srcloc", loc, {
            "builtin": function(_) {
              console.error("Should not be allowed to have a builtin that's unbound", e);
            },
            "srcloc": function(source, startL, startC, startCh, endL, endC, endCh) {
              var p = $("<p>");
              p.append("The name ");
              p.append($("<span>").addClass("code").text(name));
              p.append(" is used but not defined at ");
              dom.append(p);
              dom.append(drawSrcloc(loc));
              hoverLocs(dom, [loc]);
              container.append(dom);
            }
          });
        }
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
              "unbound-id": drawUnboundId,
              "shadow-id": drawShadowId,
              "duplicate-id": drawShadowId, // NOTE(joe): intentional re-use, not copypasta
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
        function getLastUserLocation(e) {
          var srclocStack = e.pyretStack.map(runtime.makeSrcloc);
          var isSrcloc = function(s) { return runtime.unwrap(get(srcloc, "is-srcloc").app(s)); }
          var userLocs = srclocStack.filter(function(l) { return l && isSrcloc(l); });
          var probablyErrorLocation = userLocs[0];
          return probablyErrorLocation;
        }
        function drawGenericTypeMismatch(value, type) {
          // TODO(joe): How to improve this search?
          var probablyErrorLocation = getLastUserLocation(e);
          var dom = $("<div>").addClass("compile-error");
          getDomValue(value, function(valDom) {
            dom.append($("<p>").text("Expected to get a " + type + " as an argument, but got this instead: "))
              .append($("<br>"))
              .append(valDom)
              .append($("<br>"))
              .append($("<p>").text("at "))
              .append($("<br>"))
              .append(drawSrcloc(probablyErrorLocation));
            $(valDom).trigger({type: 'afterAttach'});
            $('*', valDom).trigger({type : 'afterAttach'});
            container.append(dom);
            hoverLocs(dom, [probablyErrorLocation]);
          });
        }
        function drawArityMismatch(funLoc, arity, args) {
          args = ffi.toArray(args);
          var probablyErrorLocation = getLastUserLocation(e);
          var dom = $("<div>").addClass("compile-error");
          var argDom = $("<div>");
          setTimeout(function() {
            args.forEach(function(a) {
              outputUI.renderPyretValue(argDom, runtime, a);
            });
          }, 0);
          cases(get(srcloc, "Srcloc"), "Srcloc", funLoc, {
            "srcloc": function(/* skip args */) {
              dom.append($("<p>").text("Expected to get " + arity + " arguments when calling the function at"))
                .append($("<br>"))
                .append(drawSrcloc(funLoc))
                .append($("<br>"))
                .append($("<p>").text("from"))
                .append($("<br>"))
                .append(drawSrcloc(probablyErrorLocation))
                .append($("<br>"))
                .append($("<p>").text("but got these " + args.length + " arguments: "))
                .append($("<br>"))
                .append(argDom)
              container.append(dom);
              hoverLocs(dom, [funLoc, probablyErrorLocation]);
            },
            "builtin": function(name) {
              dom.append($("<p>").text("Expected to get " + arity + " arguments at"))
                .append($("<br>"))
                .append(drawSrcloc(probablyErrorLocation))
                .append($("<br>"))
                .append($("<p>").text("but got these " + args.length + " arguments: "))
                .append($("<br>"))
                .append(argDom);
              container.append(dom);
              hoverLocs(dom, [probablyErrorLocation]);
            }
          });
        }
        function drawMessageException(message) {
          var probablyErrorLocation = getLastUserLocation(e);
          var dom = $("<div>").addClass("compile-error");
          if(probablyErrorLocation !== undefined) {
            dom.append($("<p>").text(message + " At:"))
              .append($("<br>"))
              .append(drawSrcloc(probablyErrorLocation));
            hoverLocs(dom, [probablyErrorLocation]);
          } else {
            dom.append($("<p>").text(message));
          }
          container.append(dom);
        }
        function drawNonBooleanCondition(loc, type, value) {
          getDomValue(value, function(v) {
            var dom = $("<div>").addClass("compile-error");
            dom.append($("<p>").text("Expected true or false for the test in an " + type + " expression, but got:"));
            dom.append($("<br>"));
            dom.append(v);
            $(v).trigger({type: 'afterAttach'});
            $('*', v).trigger({type : 'afterAttach'});
            dom.append(drawSrcloc(loc));
            hoverLocs(dom, [loc]);
            container.append(dom);
          });
        }
        function drawNonBooleanOp(loc, position, type, value) {
          getDomValue(value, function(v) {
            var dom = $("<div>").addClass("compile-error");
            dom.append($("<p>").text("Expected true or false for the " + position + " argument in " + type + " expression, but got:"));
            dom.append($("<br>"));
            dom.append(v);
            $(v).trigger({type: 'afterAttach'});
            $('*', v).trigger({type : 'afterAttach'});
            dom.append($("<br>"));
            dom.append(drawSrcloc(loc));
            hoverLocs(dom, [loc]);
            container.append(dom);
          });
        }
        function drawNonFunctionApp(loc, nonFunVal, args) {
          getDomValue(nonFunVal, function(v) {
            var dom = $("<div>").addClass("compile-error");
            dom.append($("<p>").text("Expected a function in application but got:"));
            dom.append($("<br>"));
            dom.append(v);
            $(v).trigger({type: 'afterAttach'});
            $('*', v).trigger({type : 'afterAttach'});
            dom.append($("<br>"));
            dom.append(drawSrcloc(loc));
            hoverLocs(dom, [loc]);
            container.append(dom);
          });
        }
        function drawNoBranchesMatched(loc, type) {
          var dom = $("<div>").addClass("compile-error");
          dom.append($("<p>").text("No branches matched in this `" + type + "` expression"));
          dom.append($("<br>"));
          dom.append(drawSrcloc(loc));
          hoverLocs(dom, [loc]);
          container.append(dom);
        }
        function drawUserBreak() {
          container.append($("<div>").addClass("compile-error").text("Program stopped by user"));
        }
        
        function drawPyretRuntimeError() {
          cases(get(error, "RuntimeError"), "RuntimeError", e.exn, {
              "generic-type-mismatch": drawGenericTypeMismatch,
              "arity-mismatch": drawArityMismatch,
              "message-exception": drawMessageException,
              "non-boolean-condition": drawNonBooleanCondition,
              "non-boolean-op": drawNonBooleanOp,
              "non-function-app": drawNonFunctionApp,
              "user-break": drawUserBreak,
              "no-branches-matched": drawNoBranchesMatched,
              "else": drawRuntimeErrorToString(e)
            });
        }

        function drawParseErrorNextToken(loc, nextToken) {
          var dom = $("<div>").addClass("compile-error");
          dom.append($("<p>").text("Parse error near ").append(drawSrcloc(loc)))
            .append($("<br>"))
            .append($("<p>").text("The next token was " + nextToken));
          hoverLocs(dom, [loc]);
          container.append(dom);
        }
        function drawParseErrorEOF(loc) {
          var dom = $("<div>").addClass("compile-error");
          dom.append($("<p>").text("Parse error at the end of the file, at ").append(drawSrcloc(loc)))
            .append($("<br>"))
            .append($("<p>").text("Is the program incomplete?"));
          hoverLocs(dom, [loc]);
          container.append(dom);
        }

        function drawPyretParseError() {
          cases(get(error, "ParseError"), "ParseError", e.exn, {
              "parse-error-next-token": drawParseErrorNextToken,
              "parse-error-eof": drawParseErrorEOF,
              "else": drawRuntimeErrorToString(e)
            });
        }
        if(!runtime.isObject(e.exn)) {
          drawRuntimeErrorToString(e)();
        }
        else if(mkPred("RuntimeError")(e.exn)) {
          drawPyretRuntimeError();
        }
        else if(mkPred("ParseError")(e.exn)) {
          drawPyretParseError();
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

