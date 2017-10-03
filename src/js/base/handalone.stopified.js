requirejs(["pyret-base/js/runtime", "pyret-base/js/exn-stack-parser", "program"], function(runtimeLib, stackLib, program) {

const $__R = $__T.getRTS();
const $handleNew = $__R.handleNew.bind($__R);
const captureCC = $__R.captureCC;
const suspendCC = $__R.suspendCC;
const SENTINAL = {};
let app127;
let app61;
let arg011;
let arg010;
let app60;
var uris;
var runtimeLib;
var runtime;
var stackLib;
var program;
var staticModules;
var depMap;
var toLoad;
var main;
var runtime;
var EXIT_SUCCESS;
var EXIT_ERROR;
var EXIT_ERROR_RENDERING_ERROR;
var EXIT_ERROR_DISPLAYING_ERROR;
var EXIT_ERROR_CHECK_FAILURES;
var EXIT_ERROR_JS;
var EXIT_ERROR_UNKNOWN;
var postLoadHooks;
var renderErrorMessageAndExit;
var isExit;
var processExit;
var onComplete;
if ($__R.mode) $__R.delimit(function delimit0() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) onComplete = {
      box: function fun3(result) {
        let target = null;
        let app11;
        let app10;
        let app9;
        let app8;
        let app7;
        let app6;
        let app5;
        let app4;
        let and1;
        let x2;
        let app3;
        let app2;
        let app1;
        let app0;

        if (!$__R.mode) {
          [app10, app11, app0, app1, app2, app3, app4, app5, app6, app7, app8, app9, and1, x2] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 1;
            app0 = $__R.suspend();
          } else if (target === 1) app0 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 2;
            app1 = runtime.box.isSuccessResult(result);
          } else if (target === 2) app1 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode && app1 || !$__R.mode && target === 3) {
            if ($__R.mode) {
              target = 3;
              app2 = process.exit(EXIT_SUCCESS.box);
            } else if (target === 3) app2 = $__R.stack[$__R.stack.length - 1].f();
            //console.log("The program completed successfully");
            //console.log(result);

          } else if ($__R.mode || !$__R.mode && (target === 13 || target === 12 || target === 11 || target === 10 || target === 9 || target === 8 || target === 7 || target === 6 || target === 5 || target === 4)) {
            if ($__R.mode) {
              target = 4;
              app3 = runtime.box.isFailureResult(result);
            } else if (target === 4) app3 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode && app3 || !$__R.mode && (target === 10 || target === 9 || target === 8 || target === 7 || target === 6 || target === 5)) {
              if ($__R.mode) {
                target = 5;
                x2 = runtime.box.isPyretException(result.exn);
              } else if (target === 5) x2 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode && x2 || !$__R.mode && target === 6) {
                if ($__R.mode) {
                  target = 6;
                  app4 = isExit.box(runtime.box, result);
                } else if (target === 6) app4 = $__R.stack[$__R.stack.length - 1].f();

                if ($__R.mode) and1 = app4;
              } else if ($__R.mode) {
                if ($__R.mode) and1 = x2;
              }

              if ($__R.mode && and1 || !$__R.mode && target === 7) {
                if ($__R.mode) {
                  target = 7;
                  app5 = processExit.box(runtime.box, result.exn.exn);
                } else if (target === 7) app5 = $__R.stack[$__R.stack.length - 1].f();
              }

              if ($__R.mode) {
                target = 8;
                app6 = console.error("The run ended in error:");
              } else if (target === 8) app6 = $__R.stack[$__R.stack.length - 1].f();

              try {
                if ($__R.mode) {
                  target = 9;
                  app7 = renderErrorMessageAndExit.box(runtime.box, result);
                } else if (target === 9) app7 = $__R.stack[$__R.stack.length - 1].f();
              } catch (e) {
                if (e instanceof $__T.Capture || e instanceof $__T.Restore) throw e;

                if ($__R.mode) {
                  target = 10;
                  app8 = console.error("EXCEPTION!", e);
                } else if (target === 10) app8 = $__R.stack[$__R.stack.length - 1].f();
              }
            } else if ($__R.mode || !$__R.mode && (target === 13 || target === 12 || target === 11)) {
              if ($__R.mode) {
                target = 11;
                app9 = console.error("The run ended in an unknown error: ", result);
              } else if (target === 11) app9 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 12;
                app10 = console.error(result.exn.stack);
              } else if (target === 12) app10 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 13;
                app11 = process.exit(EXIT_ERROR_UNKNOWN.box);
              } else if (target === 13) app11 = $__R.stack[$__R.stack.length - 1].f();
            }
          }
        } catch (exn00) {
          if (exn00 instanceof $__T.Capture) {
            exn00.stack.push({
              kind: "rest",
              f: () => fun3.call(this, result),
              locals: [app10, app11, app0, app1, app2, app3, app4, app5, app6, app7, app8, app9, and1, x2],
              index: target
            });
          }

          throw exn00;
        }
      }
    };
  } catch (exn01) {
    if (exn01 instanceof $__T.Capture) {
      exn01.stack.push({
        kind: "rest",
        f: () => delimit0.call(this),
        locals: [],
        index: target
      });
    }

    throw exn01;
  }
});
if ($__R.mode) $__R.delimit(function delimit1() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) processExit = {
      box: function fun2(execRt, exn) {
        let target = null;
        let app16;
        let app15;
        var message;
        let app14;
        let app13;
        var exitCode;
        let app12;

        if (!$__R.mode) {
          [app12, app13, app14, app15, app16, message, exitCode] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 15;
            app12 = $__R.suspend();
          } else if (target === 15) app12 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 16;
            exitCode = execRt.getField(exn, "code");
          } else if (target === 16) exitCode = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 17;
            app13 = execRt.ffi.isExit(exn);
          } else if (target === 17) app13 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode && app13 || !$__R.mode && (target === 19 || target === 18)) {
            if ($__R.mode) {
              target = 18;
              app14 = exitCode.toString();
            } else if (target === 18) app14 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) message = "Exited with code " + app14 + "\n";

            if ($__R.mode) {
              target = 19;
              app15 = process.stdout.write(message);
            } else if (target === 19) app15 = $__R.stack[$__R.stack.length - 1].f();
          }

          if ($__R.mode) {
            target = 20;
            app16 = process.exit(exitCode);
          } else if (target === 20) app16 = $__R.stack[$__R.stack.length - 1].f();
        } catch (exn02) {
          if (exn02 instanceof $__T.Capture) {
            exn02.stack.push({
              kind: "rest",
              f: () => fun2.call(this, execRt, exn),
              locals: [app12, app13, app14, app15, app16, message, exitCode],
              index: target
            });
          }

          throw exn02;
        }
      }
    };
  } catch (exn03) {
    if (exn03 instanceof $__T.Capture) {
      exn03.stack.push({
        kind: "rest",
        f: () => delimit1.call(this),
        locals: [],
        index: target
      });
    }

    throw exn03;
  }
});
if ($__R.mode) $__R.delimit(function delimit2() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) isExit = {
      box: function fun1(execRt, result) {
        let target = null;
        let app18;
        let or0;
        let x1;
        var exn;
        let app17;

        if (!$__R.mode) {
          [app17, exn, x1, app18, or0] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 22;
            app17 = $__R.suspend();
          } else if (target === 22) app17 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) exn = result.exn.exn;

          if ($__R.mode) {
            target = 23;
            x1 = execRt.ffi.isExit(exn);
          } else if (target === 23) x1 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode && x1) {
            if ($__R.mode) or0 = x1;
          } else if ($__R.mode || !$__R.mode && target === 24) {
            if ($__R.mode) {
              target = 24;
              app18 = execRt.ffi.isExitQuiet(exn);
            } else if (target === 24) app18 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) or0 = app18;
          }

          return or0;
        } catch (exn04) {
          if (exn04 instanceof $__T.Capture) {
            exn04.stack.push({
              kind: "rest",
              f: () => fun1.call(this, execRt, result),
              locals: [app17, exn, x1, app18, or0],
              index: target
            });
          }

          throw exn04;
        }
      }
    };
  } catch (exn05) {
    if (exn05 instanceof $__T.Capture) {
      exn05.stack.push({
        kind: "rest",
        f: () => delimit2.call(this),
        locals: [],
        index: target
      });
    }

    throw exn05;
  }
});
if ($__R.mode) $__R.delimit(function delimit3() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) renderErrorMessageAndExit = {
      box: function fun0(execRt, res) {
        let target = null;
        let app55;
        let app54;
        let app53;
        let app52;
        let app51;
        let app50;
        let app22;
        let app21;
        var rendererrorMod;
        let app20;
        let app19;
        var rendererror;
        var gf;
        var exnStack;

        if (!$__R.mode) {
          [app21, app54, app22, app55, app19, exnStack, rendererrorMod, rendererror, app50, app51, app52, app20, app53, gf] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        if ($__R.mode) {
          res = {
            box: res
          };
          execRt = {
            box: execRt
          };
        }

        try {
          if ($__R.mode) exnStack = {
            box: undefined
          };
          if ($__R.mode) gf = {
            box: undefined
          };
          if ($__R.mode) rendererror = {
            box: undefined
          };

          if ($__R.mode) {
            target = 26;
            app19 = $__R.suspend();
          } else if (target === 26) app19 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 27;
            app20 = execRt.box.isPyretException(res.box.exn);
          } else if (target === 27) app20 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode && app20 || !$__R.mode && (target === 30 || target === 29 || target === 28)) {
            if ($__R.mode) rendererrorMod = execRt.box.modules["builtin://render-error-display"];

            if ($__R.mode) {
              target = 28;
              app21 = execRt.box.getField(rendererrorMod, "provide-plus-types");
            } else if (target === 28) app21 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) {
              rendererror.box = app21;
              gf.box = execRt.box.getField;
              exnStack.box = res.box.exn.stack;
            }

            if ($__R.mode) {
              target = 29;
              app22 = stackLib.convertExceptionToPyretStackTrace(res.box.exn, program);
            } else if (target === 29) app22 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) res.box.exn.pyretStack = app22;

            if ($__R.mode) {
              target = 30;
              app50 = execRt.box.runThunk(function funExpr24() {
                let target = null;
                let app25;
                let arg00;
                let app24;
                let and0;
                let x0;
                let app23;

                if (!$__R.mode) {
                  [app23, app25, arg00, app24, and0, x0] = $__R.stack[$__R.stack.length - 1].locals;
                  target = $__R.stack[$__R.stack.length - 1].index;
                  $__R.stack.pop();
                }

                try {
                  if ($__R.mode) {
                    target = 31;
                    app23 = $__R.suspend();
                  } else if (target === 31) app23 = $__R.stack[$__R.stack.length - 1].f();

                  if ($__R.mode) {
                    target = 32;
                    x0 = execRt.box.isObject(res.box.exn.exn);
                  } else if (target === 32) x0 = $__R.stack[$__R.stack.length - 1].f();

                  if ($__R.mode && x0 || !$__R.mode && target === 33) {
                    if ($__R.mode) {
                      target = 33;
                      app24 = execRt.box.hasField(res.box.exn.exn, "render-reason");
                    } else if (target === 33) app24 = $__R.stack[$__R.stack.length - 1].f();

                    if ($__R.mode) and0 = app24;
                  } else if ($__R.mode) {
                    if ($__R.mode) and0 = x0;
                  }
                } catch (exn06) {
                  if (exn06 instanceof $__T.Capture) {
                    exn06.stack.push({
                      kind: "rest",
                      f: () => funExpr24.call(this),
                      locals: [app23, app25, arg00, app24, and0, x0],
                      index: target
                    });
                  }

                  throw exn06;
                }

                if ($__R.mode && and0 || !$__R.mode && target === 34) {
                  try {
                    if ($__R.mode) arg00 = res.box.exn.exn;

                    if ($__R.mode) {
                      target = 34;
                      app25 = execRt.box.getColonField(res.box.exn.exn, "render-reason");
                    } else if (target === 34) app25 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn07) {
                    if (exn07 instanceof $__T.Capture) {
                      exn07.stack.push({
                        kind: "rest",
                        f: () => funExpr24.call(this),
                        locals: [app23, app25, arg00, app24, and0, x0],
                        index: target
                      });
                    }

                    throw exn07;
                  }

                  return app25.full_meth(arg00);
                } else if ($__R.mode) {
                  return execRt.box.ffi.edEmbed(res.box.exn.exn);
                }
              }, function funExpr25(reasonResult) {
                let target = null;
                let app49;
                let app29;
                let app28;
                let arg01;
                let app33;
                let app32;
                let app31;
                let app30;
                let app27;
                let app26;

                if (!$__R.mode) {
                  [app32, app33, app26, app27, app49, app28, app29, arg01, app30, app31] = $__R.stack[$__R.stack.length - 1].locals;
                  target = $__R.stack[$__R.stack.length - 1].index;
                  $__R.stack.pop();
                }

                if ($__R.mode) reasonResult = {
                  box: reasonResult
                };

                try {
                  if ($__R.mode) {
                    target = 37;
                    app26 = $__R.suspend();
                  } else if (target === 37) app26 = $__R.stack[$__R.stack.length - 1].f();

                  if ($__R.mode) {
                    target = 38;
                    app27 = execRt.box.isFailureResult(reasonResult.box);
                  } else if (target === 38) app27 = $__R.stack[$__R.stack.length - 1].f();

                  if ($__R.mode && app27 || !$__R.mode && (target === 44 || target === 43 || target === 42 || target === 41 || target === 40 || target === 39)) {
                    if ($__R.mode) {
                      target = 39;
                      app30 = JSON.stringify(res.box);
                    } else if (target === 39) app30 = $__R.stack[$__R.stack.length - 1].f();

                    if ($__R.mode) {
                      target = 40;
                      app31 = JSON.stringify(reasonResult.box);
                    } else if (target === 40) app31 = $__R.stack[$__R.stack.length - 1].f();

                    if ($__R.mode) {
                      target = 41;
                      app32 = JSON.stringify(exnStack.box);
                    } else if (target === 41) app32 = $__R.stack[$__R.stack.length - 1].f();

                    if ($__R.mode) {
                      target = 42;
                      app33 = execRt.box.printPyretStack(res.box.exn.pyretStack, true);
                    } else if (target === 42) app33 = $__R.stack[$__R.stack.length - 1].f();

                    if ($__R.mode) arg01 = "While trying to report that Pyret terminated with an error:\n" + app30 + "\nPyret encountered an error rendering that error:\n" + app31 + "\nStack:\n" + app32 + "\nPyret stack:\n" + app33;

                    if ($__R.mode) {
                      target = 43;
                      app28 = console.error(arg01);
                    } else if (target === 43) app28 = $__R.stack[$__R.stack.length - 1].f();

                    if ($__R.mode) {
                      target = 44;
                      app29 = process.exit(EXIT_ERROR_RENDERING_ERROR.box);
                    } else if (target === 44) app29 = $__R.stack[$__R.stack.length - 1].f();
                  } else if ($__R.mode || !$__R.mode && target === 45) {
                    if ($__R.mode) {
                      target = 45;
                      app49 = execRt.box.runThunk(function funExpr26() {
                        let target = null;
                        let app36;
                        let arg06;
                        let arg05;
                        let arg04;
                        let arg07;
                        let arg03;
                        let arg02;
                        var cliRender;
                        let app34;

                        if (!$__R.mode) {
                          [app34, app36, cliRender, arg02, arg03, arg04, arg05, arg06, arg07] = $__R.stack[$__R.stack.length - 1].locals;
                          target = $__R.stack[$__R.stack.length - 1].index;
                          $__R.stack.pop();
                        }

                        try {
                          if ($__R.mode) {
                            target = 46;
                            app34 = $__R.suspend();
                          } else if (target === 46) app34 = $__R.stack[$__R.stack.length - 1].f();

                          if ($__R.mode) {
                            target = 47;
                            cliRender = execRt.box.makeFunction(function funExpr27(val) {
                              let target = null;
                              let app35;

                              if (!$__R.mode) {
                                [app35] = $__R.stack[$__R.stack.length - 1].locals;
                                target = $__R.stack[$__R.stack.length - 1].index;
                                $__R.stack.pop();
                              }

                              try {
                                if ($__R.mode) {
                                  target = 48;
                                  app35 = $__R.suspend();
                                } else if (target === 48) app35 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn08) {
                                if (exn08 instanceof $__T.Capture) {
                                  exn08.stack.push({
                                    kind: "rest",
                                    f: () => funExpr27.call(this, val),
                                    locals: [app35],
                                    index: target
                                  });
                                }

                                throw exn08;
                              }

                              return execRt.box.toReprJS(val, execRt.box.ReprMethods["$cli"]);
                            }, "cliRender");
                          } else if (target === 47) cliRender = $__R.stack[$__R.stack.length - 1].f();

                          if ($__R.mode) arg02 = reasonResult.box.result;
                          if ($__R.mode) arg03 = cliRender;

                          if ($__R.mode) {
                            target = 50;
                            arg07 = res.box.exn.pyretStack.map(execRt.box.makeSrcloc);
                          } else if (target === 50) arg07 = $__R.stack[$__R.stack.length - 1].f();

                          if ($__R.mode) {
                            target = 51;
                            arg04 = execRt.box.ffi.makeList(arg07);
                          } else if (target === 51) arg04 = $__R.stack[$__R.stack.length - 1].f();

                          if ($__R.mode) {
                            target = 52;
                            arg05 = gf.box(rendererror.box, "values");
                          } else if (target === 52) arg05 = $__R.stack[$__R.stack.length - 1].f();

                          if ($__R.mode) arg06 = "display-to-string";

                          if ($__R.mode) {
                            target = 53;
                            app36 = gf.box(arg05, arg06);
                          } else if (target === 53) app36 = $__R.stack[$__R.stack.length - 1].f();
                        } catch (exn09) {
                          if (exn09 instanceof $__T.Capture) {
                            exn09.stack.push({
                              kind: "rest",
                              f: () => funExpr26.call(this),
                              locals: [app34, app36, cliRender, arg02, arg03, arg04, arg05, arg06, arg07],
                              index: target
                            });
                          }

                          throw exn09;
                        }

                        return app36.app(arg02, arg03, arg04);
                      }, function funExpr28(printResult) {
                        let target = null;
                        let app44;
                        let app43;
                        let arg09;
                        let app48;
                        let app47;
                        let app46;
                        let app45;
                        let app41;
                        let app40;
                        let arg08;
                        let app42;
                        let app39;
                        let app38;
                        let app37;

                        if (!$__R.mode) {
                          [app43, app44, app45, app46, app47, app37, app48, app38, app39, app40, app41, arg08, app42, arg09] = $__R.stack[$__R.stack.length - 1].locals;
                          target = $__R.stack[$__R.stack.length - 1].index;
                          $__R.stack.pop();
                        }

                        try {
                          if ($__R.mode) {
                            target = 55;
                            app37 = $__R.suspend();
                          } else if (target === 55) app37 = $__R.stack[$__R.stack.length - 1].f();

                          if ($__R.mode) {
                            target = 56;
                            app38 = execRt.box.isSuccessResult(printResult);
                          } else if (target === 56) app38 = $__R.stack[$__R.stack.length - 1].f();

                          if ($__R.mode && app38 || !$__R.mode && (target === 60 || target === 59 || target === 58 || target === 57)) {
                            if ($__R.mode) {
                              target = 57;
                              app39 = console.error(printResult.result);
                            } else if (target === 57) app39 = $__R.stack[$__R.stack.length - 1].f();

                            if ($__R.mode) {
                              target = 58;
                              app42 = execRt.box.printPyretStack(res.box.exn.pyretStack);
                            } else if (target === 58) app42 = $__R.stack[$__R.stack.length - 1].f();

                            if ($__R.mode) arg08 = "\nPyret stack:\n" + app42;

                            if ($__R.mode) {
                              target = 59;
                              app40 = console.error(arg08);
                            } else if (target === 59) app40 = $__R.stack[$__R.stack.length - 1].f();

                            if ($__R.mode) {
                              target = 60;
                              app41 = process.exit(EXIT_ERROR.box);
                            } else if (target === 60) app41 = $__R.stack[$__R.stack.length - 1].f();
                          } else if ($__R.mode || !$__R.mode && (target === 66 || target === 65 || target === 64 || target === 63 || target === 62 || target === 61)) {
                            if ($__R.mode) {
                              target = 61;
                              app45 = JSON.stringify(res.box);
                            } else if (target === 61) app45 = $__R.stack[$__R.stack.length - 1].f();

                            if ($__R.mode) {
                              target = 62;
                              app46 = JSON.stringify(printResult);
                            } else if (target === 62) app46 = $__R.stack[$__R.stack.length - 1].f();

                            if ($__R.mode) {
                              target = 63;
                              app47 = JSON.stringify(exnStack.box);
                            } else if (target === 63) app47 = $__R.stack[$__R.stack.length - 1].f();

                            if ($__R.mode) {
                              target = 64;
                              app48 = execRt.box.printPyretStack(res.box.exn.pyretStack, true);
                            } else if (target === 64) app48 = $__R.stack[$__R.stack.length - 1].f();

                            if ($__R.mode) arg09 = "While trying to report that Pyret terminated with an error:\n" + app45 + "\ndisplaying that error produced another error:\n" + app46 + "\nStack:\n" + app47 + "\nPyret stack:\n" + app48;

                            if ($__R.mode) {
                              target = 65;
                              app43 = console.error(arg09);
                            } else if (target === 65) app43 = $__R.stack[$__R.stack.length - 1].f();

                            if ($__R.mode) {
                              target = 66;
                              app44 = process.exit(EXIT_ERROR_DISPLAYING_ERROR.box);
                            } else if (target === 66) app44 = $__R.stack[$__R.stack.length - 1].f();
                          }
                        } catch (exn010) {
                          if (exn010 instanceof $__T.Capture) {
                            exn010.stack.push({
                              kind: "rest",
                              f: () => funExpr28.call(this, printResult),
                              locals: [app43, app44, app45, app46, app47, app37, app48, app38, app39, app40, app41, arg08, app42, arg09],
                              index: target
                            });
                          }

                          throw exn010;
                        }
                      }, "errordisplay->to-string");
                    } else if (target === 45) app49 = $__R.stack[$__R.stack.length - 1].f();
                  }
                } catch (exn011) {
                  if (exn011 instanceof $__T.Capture) {
                    exn011.stack.push({
                      kind: "rest",
                      f: () => funExpr25.call(this, reasonResult),
                      locals: [app32, app33, app26, app27, app49, app28, app29, arg01, app30, app31],
                      index: target
                    });
                  }

                  throw exn011;
                }
              }, "error->display");
            } else if (target === 30) app50 = $__R.stack[$__R.stack.length - 1].f();
          } else if ($__R.mode || !$__R.mode && (target === 71 || target === 70 || target === 69 || target === 68 || target === 67)) {
            if ($__R.mode && res.box.exn && res.box.exn.stack || !$__R.mode && (target === 69 || target === 68 || target === 67)) {
              if ($__R.mode) {
                target = 67;
                app51 = console.error("Abstraction breaking: Uncaught JavaScript error:\n", res.box.exn);
              } else if (target === 67) app51 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 68;
                app52 = console.error("Stack trace:\n", res.box.exn.stack);
              } else if (target === 68) app52 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 69;
                app53 = process.exit(EXIT_ERROR_JS.box);
              } else if (target === 69) app53 = $__R.stack[$__R.stack.length - 1].f();
            } else if ($__R.mode || !$__R.mode && (target === 71 || target === 70)) {
              if ($__R.mode) {
                target = 70;
                app54 = console.error("Unknown error result: ", res.box.exn);
              } else if (target === 70) app54 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 71;
                app55 = process.exit(EXIT_ERROR_UNKNOWN.box);
              } else if (target === 71) app55 = $__R.stack[$__R.stack.length - 1].f();
            }
          }
        } catch (exn012) {
          if (exn012 instanceof $__T.Capture) {
            exn012.stack.push({
              kind: "rest",
              f: () => fun0.call(this, execRt, res),
              locals: [app21, app54, app22, app55, app19, exnStack, rendererrorMod, rendererror, app50, app51, app52, app20, app53, gf],
              index: target
            });
          }

          throw exn012;
        }
      }
    };
  } catch (exn013) {
    if (exn013 instanceof $__T.Capture) {
      exn013.stack.push({
        kind: "rest",
        f: () => delimit3.call(this),
        locals: [],
        index: target
      });
    }

    throw exn013;
  }
});
if ($__R.mode) $__R.delimit(function delimit4() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) postLoadHooks = {
      box: undefined
    };
  } catch (exn014) {
    if (exn014 instanceof $__T.Capture) {
      exn014.stack.push({
        kind: "rest",
        f: () => delimit4.call(this),
        locals: [],
        index: target
      });
    }

    throw exn014;
  }
});
if ($__R.mode) $__R.delimit(function delimit5() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR_UNKNOWN = {
      box: undefined
    };
  } catch (exn015) {
    if (exn015 instanceof $__T.Capture) {
      exn015.stack.push({
        kind: "rest",
        f: () => delimit5.call(this),
        locals: [],
        index: target
      });
    }

    throw exn015;
  }
});
if ($__R.mode) $__R.delimit(function delimit6() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR_JS = {
      box: undefined
    };
  } catch (exn016) {
    if (exn016 instanceof $__T.Capture) {
      exn016.stack.push({
        kind: "rest",
        f: () => delimit6.call(this),
        locals: [],
        index: target
      });
    }

    throw exn016;
  }
});
if ($__R.mode) $__R.delimit(function delimit7() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR_CHECK_FAILURES = {
      box: undefined
    };
  } catch (exn017) {
    if (exn017 instanceof $__T.Capture) {
      exn017.stack.push({
        kind: "rest",
        f: () => delimit7.call(this),
        locals: [],
        index: target
      });
    }

    throw exn017;
  }
});
if ($__R.mode) $__R.delimit(function delimit8() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR_DISPLAYING_ERROR = {
      box: undefined
    };
  } catch (exn018) {
    if (exn018 instanceof $__T.Capture) {
      exn018.stack.push({
        kind: "rest",
        f: () => delimit8.call(this),
        locals: [],
        index: target
      });
    }

    throw exn018;
  }
});
if ($__R.mode) $__R.delimit(function delimit9() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR_RENDERING_ERROR = {
      box: undefined
    };
  } catch (exn019) {
    if (exn019 instanceof $__T.Capture) {
      exn019.stack.push({
        kind: "rest",
        f: () => delimit9.call(this),
        locals: [],
        index: target
      });
    }

    throw exn019;
  }
});
if ($__R.mode) $__R.delimit(function delimit10() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR = {
      box: undefined
    };
  } catch (exn020) {
    if (exn020 instanceof $__T.Capture) {
      exn020.stack.push({
        kind: "rest",
        f: () => delimit10.call(this),
        locals: [],
        index: target
      });
    }

    throw exn020;
  }
});
if ($__R.mode) $__R.delimit(function delimit11() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_SUCCESS = {
      box: undefined
    };
  } catch (exn021) {
    if (exn021 instanceof $__T.Capture) {
      exn021.stack.push({
        kind: "rest",
        f: () => delimit11.call(this),
        locals: [],
        index: target
      });
    }

    throw exn021;
  }
});
if ($__R.mode) $__R.delimit(function delimit12() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) runtime = {
      box: undefined
    };
  } catch (exn022) {
    if (exn022 instanceof $__T.Capture) {
      exn022.stack.push({
        kind: "rest",
        f: () => delimit12.call(this),
        locals: [],
        index: target
      });
    }

    throw exn022;
  }
});
if ($__R.mode) $__R.delimit(function delimit13() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) main = {
      box: undefined
    };
  } catch (exn023) {
    if (exn023 instanceof $__T.Capture) {
      exn023.stack.push({
        kind: "rest",
        f: () => delimit13.call(this),
        locals: [],
        index: target
      });
    }

    throw exn023;
  }
});
if ($__R.mode) $__R.delimit(function delimit14() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) toLoad = {
      box: undefined
    };
  } catch (exn024) {
    if (exn024 instanceof $__T.Capture) {
      exn024.stack.push({
        kind: "rest",
        f: () => delimit14.call(this),
        locals: [],
        index: target
      });
    }

    throw exn024;
  }
});
if ($__R.mode) $__R.delimit(function delimit15() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) depMap = {
      box: undefined
    };
  } catch (exn025) {
    if (exn025 instanceof $__T.Capture) {
      exn025.stack.push({
        kind: "rest",
        f: () => delimit15.call(this),
        locals: [],
        index: target
      });
    }

    throw exn025;
  }
});
if ($__R.mode) $__R.delimit(function delimit16() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) staticModules = {
      box: undefined
    };
  } catch (exn026) {
    if (exn026 instanceof $__T.Capture) {
      exn026.stack.push({
        kind: "rest",
        f: () => delimit16.call(this),
        locals: [],
        index: target
      });
    }

    throw exn026;
  }
});
if ($__R.mode) $__R.delimit(function delimit18() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) stackLib = {
      box: undefined
    };
  } catch (exn028) {
    if (exn028 instanceof $__T.Capture) {
      exn028.stack.push({
        kind: "rest",
        f: () => delimit18.call(this),
        locals: [],
        index: target
      });
    }

    throw exn028;
  }
});
if ($__R.mode) $__R.delimit(function delimit19() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) runtime = {
      box: undefined
    };
  } catch (exn029) {
    if (exn029 instanceof $__T.Capture) {
      exn029.stack.push({
        kind: "rest",
        f: () => delimit19.call(this),
        locals: [],
        index: target
      });
    }

    throw exn029;
  }
});
    /*
    TODO(joe): see how the lack of this interacts with CPO

    if(typeof window === 'undefined') {
    var require = require("requirejs");
    }
    require(["pyret-base/js/runtime", "pyret-base/js/exn-stack-parser", "program"], function(runtimeLib, stackLib, program) {

    */
    // TODO: Change to myrequire

if ($__R.mode) $__R.delimit(function delimit24() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) staticModules.box = program.staticModules;
  } catch (exn034) {
    if (exn034 instanceof $__T.Capture) {
      exn034.stack.push({
        kind: "rest",
        f: () => delimit24.call(this),
        locals: [],
        index: target
      });
    }

    throw exn034;
  }
});
if ($__R.mode) $__R.delimit(function delimit25() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) depMap.box = program.depMap;
  } catch (exn035) {
    if (exn035 instanceof $__T.Capture) {
      exn035.stack.push({
        kind: "rest",
        f: () => delimit25.call(this),
        locals: [],
        index: target
      });
    }

    throw exn035;
  }
});
if ($__R.mode) $__R.delimit(function delimit26() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) toLoad.box = program.toLoad;
  } catch (exn036) {
    if (exn036 instanceof $__T.Capture) {
      exn036.stack.push({
        kind: "rest",
        f: () => delimit26.call(this),
        locals: [],
        index: target
      });
    }

    throw exn036;
  }
});
if ($__R.mode) $__R.delimit(function delimit27() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) uris = program.uris;
  } catch (exn037) {
    if (exn037 instanceof $__T.Capture) {
      exn037.stack.push({
        kind: "rest",
        f: () => delimit27.call(this),
        locals: [],
        index: target
      });
    }

    throw exn037;
  }
});
if ($__R.mode) $__R.delimit(function delimit28() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) main.box = toLoad.box[toLoad.box.length - 1];
  } catch (exn038) {
    if (exn038 instanceof $__T.Capture) {
      exn038.stack.push({
        kind: "rest",
        f: () => delimit28.call(this),
        locals: [],
        index: target
      });
    }

    throw exn038;
  }
});
if ($__R.mode) $__R.delimit(function delimit29() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) {
      target = 98;
      app60 = runtimeLib.makeRuntime({
        stdout: function funExpr0(s) {
          let target = null;
          let app57;
          let app56;

          if (!$__R.mode) {
            [app56, app57] = $__R.stack[$__R.stack.length - 1].locals;
            target = $__R.stack[$__R.stack.length - 1].index;
            $__R.stack.pop();
          }

          try {
            if ($__R.mode) {
              target = 99;
              app56 = $__R.suspend();
            } else if (target === 99) app56 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) {
              target = 100;
              app57 = process.stdout.write(s);
            } else if (target === 100) app57 = $__R.stack[$__R.stack.length - 1].f();
          } catch (exn039) {
            if (exn039 instanceof $__T.Capture) {
              exn039.stack.push({
                kind: "rest",
                f: () => funExpr0.call(this, s),
                locals: [app56, app57],
                index: target
              });
            }

            throw exn039;
          }
        },
        stderr: function funExpr1(s) {
          let target = null;
          let app59;
          let app58;

          if (!$__R.mode) {
            [app58, app59] = $__R.stack[$__R.stack.length - 1].locals;
            target = $__R.stack[$__R.stack.length - 1].index;
            $__R.stack.pop();
          }

          try {
            if ($__R.mode) {
              target = 101;
              app58 = $__R.suspend();
            } else if (target === 101) app58 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) {
              target = 102;
              app59 = process.stderr.write(s);
            } else if (target === 102) app59 = $__R.stack[$__R.stack.length - 1].f();
          } catch (exn040) {
            if (exn040 instanceof $__T.Capture) {
              exn040.stack.push({
                kind: "rest",
                f: () => funExpr1.call(this, s),
                locals: [app58, app59],
                index: target
              });
            }

            throw exn040;
          }
        }
      });
    } else if (target === 98) app60 = $__R.stack[$__R.stack.length - 1].f();
  } catch (exn041) {
    if (exn041 instanceof $__T.Capture) {
      exn041.stack.push({
        kind: "rest",
        f: () => delimit29.call(this),
        locals: [],
        index: target
      });
    }

    throw exn041;
  }
});
if ($__R.mode) $__R.delimit(function delimit30() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) runtime.box = app60;
  } catch (exn042) {
    if (exn042 instanceof $__T.Capture) {
      exn042.stack.push({
        kind: "rest",
        f: () => delimit30.call(this),
        locals: [],
        index: target
      });
    }

    throw exn042;
  }
});
if ($__R.mode) $__R.delimit(function delimit31() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_SUCCESS.box = 0;
  } catch (exn043) {
    if (exn043 instanceof $__T.Capture) {
      exn043.stack.push({
        kind: "rest",
        f: () => delimit31.call(this),
        locals: [],
        index: target
      });
    }

    throw exn043;
  }
});
if ($__R.mode) $__R.delimit(function delimit32() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR.box = 1;
  } catch (exn044) {
    if (exn044 instanceof $__T.Capture) {
      exn044.stack.push({
        kind: "rest",
        f: () => delimit32.call(this),
        locals: [],
        index: target
      });
    }

    throw exn044;
  }
});
if ($__R.mode) $__R.delimit(function delimit33() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR_RENDERING_ERROR.box = 2;
  } catch (exn045) {
    if (exn045 instanceof $__T.Capture) {
      exn045.stack.push({
        kind: "rest",
        f: () => delimit33.call(this),
        locals: [],
        index: target
      });
    }

    throw exn045;
  }
});
if ($__R.mode) $__R.delimit(function delimit34() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR_DISPLAYING_ERROR.box = 3;
  } catch (exn046) {
    if (exn046 instanceof $__T.Capture) {
      exn046.stack.push({
        kind: "rest",
        f: () => delimit34.call(this),
        locals: [],
        index: target
      });
    }

    throw exn046;
  }
});
if ($__R.mode) $__R.delimit(function delimit35() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR_CHECK_FAILURES.box = 4;
  } catch (exn047) {
    if (exn047 instanceof $__T.Capture) {
      exn047.stack.push({
        kind: "rest",
        f: () => delimit35.call(this),
        locals: [],
        index: target
      });
    }

    throw exn047;
  }
});
if ($__R.mode) $__R.delimit(function delimit36() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR_JS.box = 5;
  } catch (exn048) {
    if (exn048 instanceof $__T.Capture) {
      exn048.stack.push({
        kind: "rest",
        f: () => delimit36.call(this),
        locals: [],
        index: target
      });
    }

    throw exn048;
  }
});
if ($__R.mode) $__R.delimit(function delimit37() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) EXIT_ERROR_UNKNOWN.box = 6;
  } catch (exn049) {
    if (exn049 instanceof $__T.Capture) {
      exn049.stack.push({
        kind: "rest",
        f: () => delimit37.call(this),
        locals: [],
        index: target
      });
    }

    throw exn049;
  }
});
if ($__R.mode) $__R.delimit(function delimit38() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) arg010 = "command-line-arguments";
  } catch (exn050) {
    if (exn050 instanceof $__T.Capture) {
      exn050.stack.push({
        kind: "rest",
        f: () => delimit38.call(this),
        locals: [],
        index: target
      });
    }

    throw exn050;
  }
});
if ($__R.mode) $__R.delimit(function delimit39() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) {
      target = 113;
      arg011 = process.argv.slice(1);
    } else if (target === 113) arg011 = $__R.stack[$__R.stack.length - 1].f();
  } catch (exn051) {
    if (exn051 instanceof $__T.Capture) {
      exn051.stack.push({
        kind: "rest",
        f: () => delimit39.call(this),
        locals: [],
        index: target
      });
    }

    throw exn051;
  }
});
if ($__R.mode) $__R.delimit(function delimit40() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) {
      target = 115;
      app61 = runtime.box.setParam(arg010, arg011);
    } else if (target === 115) app61 = $__R.stack[$__R.stack.length - 1].f();
  } catch (exn052) {
    if (exn052 instanceof $__T.Capture) {
      exn052.stack.push({
        kind: "rest",
        f: () => delimit40.call(this),
        locals: [],
        index: target
      });
    }

    throw exn052;
  }
});
if ($__R.mode) $__R.delimit(function delimit41() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) postLoadHooks.box = {
      "builtin://srcloc": function funExpr2(srcloc) {
        let target = null;
        let app63;
        let arg013;
        let arg012;
        let app62;

        if (!$__R.mode) {
          [app62, arg012, app63, arg013] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 117;
            app62 = $__R.suspend();
          } else if (target === 117) app62 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 118;
            arg012 = runtime.box.getField(srcloc, "provide-plus-types");
          } else if (target === 118) arg012 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) arg013 = "values";

          if ($__R.mode) {
            target = 119;
            app63 = runtime.box.getField(arg012, arg013);
          } else if (target === 119) app63 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box.srcloc = app63;
        } catch (exn053) {
          if (exn053 instanceof $__T.Capture) {
            exn053.stack.push({
              kind: "rest",
              f: () => funExpr2.call(this, srcloc),
              locals: [app62, arg012, app63, arg013],
              index: target
            });
          }

          throw exn053;
        }
      },
      "builtin://ffi": function funExpr3(ffi) {
        let target = null;
        let app65;
        var checkList;
        let app64;

        if (!$__R.mode) {
          [app64, checkList, app65] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 120;
            app64 = $__R.suspend();
          } else if (target === 120) app64 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) ffi = ffi.jsmod;
          if ($__R.mode) runtime.box.ffi = ffi;
          if ($__R.mode) runtime.box["throwMessageException"] = ffi.throwMessageException;
          if ($__R.mode) runtime.box["throwNoBranchesMatched"] = ffi.throwNoBranchesMatched;
          if ($__R.mode) runtime.box["throwNoCasesMatched"] = ffi.throwNoCasesMatched;
          if ($__R.mode) runtime.box["throwNonBooleanCondition"] = ffi.throwNonBooleanCondition;
          if ($__R.mode) runtime.box["throwNonBooleanOp"] = ffi.throwNonBooleanOp;
          if ($__R.mode) runtime.box["throwUnfinishedTemplate"] = ffi.throwUnfinishedTemplate;
          if ($__R.mode) runtime.box["throwInvalidTableColumn"] = ffi.throwInvalidTableColumn;
          if ($__R.mode) runtime.box["toArray"] = ffi.toArray;

          if ($__R.mode) {
            target = 121;
            checkList = runtime.box.makeCheckType(ffi.isList, "List");
          } else if (target === 121) checkList = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box["checkList"] = checkList;

          if ($__R.mode) {
            target = 122;
            app65 = runtime.box.makeCheckType(ffi.isEqualityResult, "EqualityResult");
          } else if (target === 122) app65 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box["checkEQ"] = app65;
        } catch (exn054) {
          if (exn054 instanceof $__T.Capture) {
            exn054.stack.push({
              kind: "rest",
              f: () => funExpr3.call(this, ffi),
              locals: [app64, checkList, app65],
              index: target
            });
          }

          throw exn054;
        }
      },
      "builtin://table": function funExpr4(table) {
        let target = null;
        let app70;
        let app67;
        let app66;

        if (!$__R.mode) {
          [app66, app67, app70] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 123;
            app66 = $__R.suspend();
          } else if (target === 123) app66 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) table = table.jsmod;
          if ($__R.mode) runtime.box["makeTable"] = table.makeTable;
          if ($__R.mode) runtime.box["openTable"] = table.openTable;

          if ($__R.mode) {
            target = 124;
            app67 = runtime.box.makeCheckType(table.isTable, "Table");
          } else if (target === 124) app67 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box["checkTable"] = app67;
          if ($__R.mode) runtime.box["isTable"] = table.isTable;
          if ($__R.mode) runtime.box["checkWrapTable"] = function funExpr5(val) {
            let target = null;
            let app69;
            let app68;

            if (!$__R.mode) {
              [app68, app69] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            try {
              if ($__R.mode) {
                target = 125;
                app68 = $__R.suspend();
              } else if (target === 125) app68 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 126;
                app69 = runtime.box.checkTable(val);
              } else if (target === 126) app69 = $__R.stack[$__R.stack.length - 1].f();

              return val;
            } catch (exn055) {
              if (exn055 instanceof $__T.Capture) {
                exn055.stack.push({
                  kind: "rest",
                  f: () => funExpr5.call(this, val),
                  locals: [app68, app69],
                  index: target
                });
              }

              throw exn055;
            }
          };

          if ($__R.mode) {
            target = 127;
            app70 = runtime.box.makePrimAnn("Table", table.isTable);
          } else if (target === 127) app70 = $__R.stack[$__R.stack.length - 1].f();
        } catch (exn056) {
          if (exn056 instanceof $__T.Capture) {
            exn056.stack.push({
              kind: "rest",
              f: () => funExpr4.call(this, table),
              locals: [app66, app67, app70],
              index: target
            });
          }

          throw exn056;
        }
      },
      "builtin://data-source": function funExpr6(ds) {
        let target = null;
        let app104;
        let arg030;
        let arg029;
        let app105;
        let app82;
        let app81;
        let app80;
        let app79;
        let element11;
        let element10;
        let element9;
        let element8;
        let element7;
        let element6;
        let element5;
        let element4;
        let element3;
        let app72;
        let arg015;
        let arg014;
        let app71;

        if (!$__R.mode) {
          [arg029, app79, app104, app105, arg030, element3, element4, element5, app80, element6, app81, element7, app71, app82, element10, element8, app72, element11, arg014, element9, arg015] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        if ($__R.mode) ds = {
          box: ds
        };

        try {
          if ($__R.mode) {
            target = 128;
            app71 = $__R.suspend();
          } else if (target === 128) app71 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 129;
            arg014 = runtime.box.getField(ds.box, "provide-plus-types");
          } else if (target === 129) arg014 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) arg015 = "values";

          if ($__R.mode) {
            target = 130;
            app72 = runtime.box.getField(arg014, arg015);
          } else if (target === 130) app72 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) ds.box = app72;
          // Variadic convenience function for desugaring use.
          // 'type' corresponds to a loader option in `data-source.arr`

          if ($__R.mode) runtime.box["asLoaderOption"] = function funExpr7(type) {
            let target = null;
            let app75;
            let app74;
            let arg017;
            let arg016;

            let _fallthrough;

            let _test;

            let app73;

            if (!$__R.mode) {
              [arg017, _fallthrough, _test, app73, app74, arg016, app75] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            const materializedArguments = arguments;

            try {
              if ($__R.mode) {
                target = 131;
                app73 = $__R.suspend();
              } else if (target === 131) app73 = $__R.stack[$__R.stack.length - 1].f();
            } catch (exn057) {
              if (exn057 instanceof $__T.Capture) {
                exn057.stack.push({
                  kind: "rest",
                  f: () => funExpr7.apply(this, materializedArguments),
                  locals: [arg017, _fallthrough, _test, app73, app74, arg016, app75],
                  index: target
                });
              }

              throw exn057;
            }

            _switch: {
              try {
                if ($__R.mode) _test = type;
                if ($__R.mode) _fallthrough = false;
              } catch (exn058) {
                if (exn058 instanceof $__T.Capture) {
                  exn058.stack.push({
                    kind: "rest",
                    f: () => funExpr7.apply(this, materializedArguments),
                    locals: [arg017, _fallthrough, _test, app73, app74, arg016, app75],
                    index: target
                  });
                }

                throw exn058;
              }

              if ($__R.mode && (_test === "sanitizer" || _fallthrough) || !$__R.mode && target === 132) {
                try {
                  if ($__R.mode) _fallthrough = true;
                  if ($__R.mode) arg016 = arguments[1];
                  if ($__R.mode) arg017 = arguments[2];

                  if ($__R.mode) {
                    target = 132;
                    app74 = runtime.box.getField(ds.box, "sanitize-col");
                  } else if (target === 132) app74 = $__R.stack[$__R.stack.length - 1].f();
                } catch (exn059) {
                  if (exn059 instanceof $__T.Capture) {
                    exn059.stack.push({
                      kind: "rest",
                      f: () => funExpr7.apply(this, materializedArguments),
                      locals: [arg017, _fallthrough, _test, app73, app74, arg016, app75],
                      index: target
                    });
                  }

                  throw exn059;
                }

                return app74.app(arg016, arg017);
              }

              try {
                if ($__R.mode) {
                  target = 134;
                  app75 = runtime.box.ffi.throwMessageException("Internal error: Invalid loader option type: " + type);
                } else if (target === 134) app75 = $__R.stack[$__R.stack.length - 1].f();
              } catch (exn060) {
                if (exn060 instanceof $__T.Capture) {
                  exn060.stack.push({
                    kind: "rest",
                    f: () => funExpr7.apply(this, materializedArguments),
                    locals: [arg017, _fallthrough, _test, app73, app74, arg016, app75],
                    index: target
                  });
                }

                throw exn060;
              }
            }
          };
          // Convenience function for JS library use

          if ($__R.mode) runtime.box["extractLoaderOption"] = function funExpr8(opt) {
            let target = null;
            let app78;
            let element2;
            let element1;
            let element0;
            let app77;
            let arg018;
            var isSanitizer;
            let app76;

            if (!$__R.mode) {
              [app76, arg018, app77, app78, element0, element1, element2, isSanitizer] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            try {
              if ($__R.mode) {
                target = 135;
                app76 = $__R.suspend();
              } else if (target === 135) app76 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 136;
                isSanitizer = runtime.box.getField(ds.box, "is-sanitize-col");
              } else if (target === 136) isSanitizer = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 137;
                arg018 = isSanitizer.app(opt);
              } else if (target === 137) arg018 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 138;
                app77 = runtime.box.unwrap(arg018);
              } else if (target === 138) app77 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode && app77 || !$__R.mode && (target === 140 || target === 139)) {
                if ($__R.mode) element0 = "sanitizer";

                if ($__R.mode) {
                  target = 139;
                  element1 = runtime.box.getField(opt, "col");
                } else if (target === 139) element1 = $__R.stack[$__R.stack.length - 1].f();

                if ($__R.mode) {
                  target = 140;
                  element2 = runtime.box.getField(opt, "sanitizer");
                } else if (target === 140) element2 = $__R.stack[$__R.stack.length - 1].f();

                return {
                  type: element0,
                  col: element1,
                  sanitizer: element2
                };
              } else if ($__R.mode || !$__R.mode && target === 141) {
                if ($__R.mode) {
                  target = 141;
                  app78 = runtime.box.ffi.throwMessageException("Internal error: Cannot coerce non-loader option");
                } else if (target === 141) app78 = $__R.stack[$__R.stack.length - 1].f();
              }
            } catch (exn061) {
              if (exn061 instanceof $__T.Capture) {
                exn061.stack.push({
                  kind: "rest",
                  f: () => funExpr8.call(this, opt),
                  locals: [app76, arg018, app77, app78, element0, element1, element2, isSanitizer],
                  index: target
                });
              }

              throw exn061;
            }
          };

          if ($__R.mode) {
            target = 142;
            element3 = runtime.box.getField(ds.box, "option-sanitizer");
          } else if (target === 142) element3 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 143;
            element4 = runtime.box.getField(ds.box, "string-sanitizer");
          } else if (target === 143) element4 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 144;
            element5 = runtime.box.getField(ds.box, "num-sanitizer");
          } else if (target === 144) element5 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 145;
            element6 = runtime.box.getField(ds.box, "bool-sanitizer");
          } else if (target === 145) element6 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 146;
            element7 = runtime.box.getField(ds.box, "strict-num-sanitizer");
          } else if (target === 146) element7 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 147;
            element8 = runtime.box.getField(ds.box, "strings-only");
          } else if (target === 147) element8 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 148;
            element9 = runtime.box.getField(ds.box, "numbers-only");
          } else if (target === 148) element9 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 149;
            element10 = runtime.box.getField(ds.box, "booleans-only");
          } else if (target === 149) element10 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 150;
            element11 = runtime.box.getField(ds.box, "empty-only");
          } else if (target === 150) element11 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box["builtin_sanitizers"] = {
            option: element3,
            string: element4,
            num: element5,
            bool: element6,
            strict_num: element7,
            strings_only: element8,
            numbers_only: element9,
            booleans_only: element10,
            empty_only: element11
          };

          if ($__R.mode) {
            target = 151;
            app79 = runtime.box.getField(ds.box, "c-str");
          } else if (target === 151) app79 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box["makeCStr"] = app79.app;

          if ($__R.mode) {
            target = 152;
            app80 = runtime.box.getField(ds.box, "c-num");
          } else if (target === 152) app80 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box["makeCNum"] = app80.app;

          if ($__R.mode) {
            target = 153;
            app81 = runtime.box.getField(ds.box, "c-bool");
          } else if (target === 153) app81 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box["makeCBool"] = app81.app;

          if ($__R.mode) {
            target = 154;
            app82 = runtime.box.getField(ds.box, "c-custom");
          } else if (target === 154) app82 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box["makeCCustom"] = app82.app;
          if ($__R.mode) runtime.box["makeCEmpty"] = function funExpr9() {
            let target = null;
            let app83;

            if (!$__R.mode) {
              [app83] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            try {
              if ($__R.mode) {
                target = 155;
                app83 = $__R.suspend();
              } else if (target === 155) app83 = $__R.stack[$__R.stack.length - 1].f();
            } catch (exn062) {
              if (exn062 instanceof $__T.Capture) {
                exn062.stack.push({
                  kind: "rest",
                  f: () => funExpr9.call(this),
                  locals: [app83],
                  index: target
                });
              }

              throw exn062;
            }

            return runtime.box.getField(ds.box, "c-empty");
          };
          if ($__R.mode) runtime.box["isCStr"] = function funExpr10(v) {
            let target = null;
            let arg019;
            let app85;
            let arg020;
            let app84;

            if (!$__R.mode) {
              [app84, arg020, arg019, app85] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            try {
              if ($__R.mode) {
                target = 157;
                app84 = $__R.suspend();
              } else if (target === 157) app84 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) arg020 = v;

              if ($__R.mode) {
                target = 158;
                app85 = runtime.box.getField(ds.box, "is-c-str");
              } else if (target === 158) app85 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 159;
                arg019 = app85.app(arg020);
              } else if (target === 159) arg019 = $__R.stack[$__R.stack.length - 1].f();
            } catch (exn063) {
              if (exn063 instanceof $__T.Capture) {
                exn063.stack.push({
                  kind: "rest",
                  f: () => funExpr10.call(this, v),
                  locals: [app84, arg020, arg019, app85],
                  index: target
                });
              }

              throw exn063;
            }

            return runtime.box.unwrap(arg019);
          };
          if ($__R.mode) runtime.box["isCNum"] = function funExpr11(v) {
            let target = null;
            let arg021;
            let app87;
            let arg022;
            let app86;

            if (!$__R.mode) {
              [app86, arg022, arg021, app87] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            try {
              if ($__R.mode) {
                target = 161;
                app86 = $__R.suspend();
              } else if (target === 161) app86 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) arg022 = v;

              if ($__R.mode) {
                target = 162;
                app87 = runtime.box.getField(ds.box, "is-c-num");
              } else if (target === 162) app87 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 163;
                arg021 = app87.app(arg022);
              } else if (target === 163) arg021 = $__R.stack[$__R.stack.length - 1].f();
            } catch (exn064) {
              if (exn064 instanceof $__T.Capture) {
                exn064.stack.push({
                  kind: "rest",
                  f: () => funExpr11.call(this, v),
                  locals: [app86, arg022, arg021, app87],
                  index: target
                });
              }

              throw exn064;
            }

            return runtime.box.unwrap(arg021);
          };
          if ($__R.mode) runtime.box["isCBool"] = function funExpr12(v) {
            let target = null;
            let arg023;
            let app89;
            let arg024;
            let app88;

            if (!$__R.mode) {
              [app88, arg024, arg023, app89] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            try {
              if ($__R.mode) {
                target = 165;
                app88 = $__R.suspend();
              } else if (target === 165) app88 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) arg024 = v;

              if ($__R.mode) {
                target = 166;
                app89 = runtime.box.getField(ds.box, "is-c-bool");
              } else if (target === 166) app89 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 167;
                arg023 = app89.app(arg024);
              } else if (target === 167) arg023 = $__R.stack[$__R.stack.length - 1].f();
            } catch (exn065) {
              if (exn065 instanceof $__T.Capture) {
                exn065.stack.push({
                  kind: "rest",
                  f: () => funExpr12.call(this, v),
                  locals: [app88, arg024, arg023, app89],
                  index: target
                });
              }

              throw exn065;
            }

            return runtime.box.unwrap(arg023);
          };
          if ($__R.mode) runtime.box["isCCustom"] = function funExpr13(v) {
            let target = null;
            let arg025;
            let app91;
            let arg026;
            let app90;

            if (!$__R.mode) {
              [app90, arg026, arg025, app91] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            try {
              if ($__R.mode) {
                target = 169;
                app90 = $__R.suspend();
              } else if (target === 169) app90 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) arg026 = v;

              if ($__R.mode) {
                target = 170;
                app91 = runtime.box.getField(ds.box, "is-c-custom");
              } else if (target === 170) app91 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 171;
                arg025 = app91.app(arg026);
              } else if (target === 171) arg025 = $__R.stack[$__R.stack.length - 1].f();
            } catch (exn066) {
              if (exn066 instanceof $__T.Capture) {
                exn066.stack.push({
                  kind: "rest",
                  f: () => funExpr13.call(this, v),
                  locals: [app90, arg026, arg025, app91],
                  index: target
                });
              }

              throw exn066;
            }

            return runtime.box.unwrap(arg025);
          };
          if ($__R.mode) runtime.box["isCEmpty"] = function funExpr14(v) {
            let target = null;
            let arg027;
            let app93;
            let arg028;
            let app92;

            if (!$__R.mode) {
              [app92, arg028, arg027, app93] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            try {
              if ($__R.mode) {
                target = 173;
                app92 = $__R.suspend();
              } else if (target === 173) app92 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) arg028 = v;

              if ($__R.mode) {
                target = 174;
                app93 = runtime.box.getField(ds.box, "is-c-empty");
              } else if (target === 174) app93 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 175;
                arg027 = app93.app(arg028);
              } else if (target === 175) arg027 = $__R.stack[$__R.stack.length - 1].f();
            } catch (exn067) {
              if (exn067 instanceof $__T.Capture) {
                exn067.stack.push({
                  kind: "rest",
                  f: () => funExpr14.call(this, v),
                  locals: [app92, arg028, arg027, app93],
                  index: target
                });
              }

              throw exn067;
            }

            return runtime.box.unwrap(arg027);
          };
          if ($__R.mode) runtime.box["unwrapCellContent"] = function funExpr15(v) {
            let target = null;
            let app100;
            let app99;
            let element19;
            let element18;
            let app98;
            let element17;
            let element16;
            let app97;
            let element15;
            let element14;
            let app96;
            let element13;
            let element12;
            let app95;
            let app94;

            if (!$__R.mode) {
              [app98, element15, app99, element16, element17, element18, element19, app100, app94, app95, element12, app96, element13, app97, element14] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            try {
              if ($__R.mode) {
                target = 177;
                app94 = $__R.suspend();
              } else if (target === 177) app94 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 178;
                app95 = runtime.box.isCStr(v);
              } else if (target === 178) app95 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode && app95 || !$__R.mode && target === 179) {
                if ($__R.mode) element12 = "str";

                if ($__R.mode) {
                  target = 179;
                  element13 = runtime.box.getField(v, "s");
                } else if (target === 179) element13 = $__R.stack[$__R.stack.length - 1].f();

                return { type: element12, value: element13 };
              } else if ($__R.mode || !$__R.mode && (target === 187 || target === 186 || target === 185 || target === 184 || target === 183 || target === 182 || target === 181 || target === 180)) {
                if ($__R.mode) {
                  target = 180;
                  app96 = runtime.box.isCNum(v);
                } else if (target === 180) app96 = $__R.stack[$__R.stack.length - 1].f();

                if ($__R.mode && app96 || !$__R.mode && target === 181) {
                  if ($__R.mode) element14 = "num";

                  if ($__R.mode) {
                    target = 181;
                    element15 = runtime.box.getField(v, "n");
                  } else if (target === 181) element15 = $__R.stack[$__R.stack.length - 1].f();

                  return { type: element14, value: element15 };
                } else if ($__R.mode || !$__R.mode && (target === 187 || target === 186 || target === 185 || target === 184 || target === 183 || target === 182)) {
                  if ($__R.mode) {
                    target = 182;
                    app97 = runtime.box.isCBool(v);
                  } else if (target === 182) app97 = $__R.stack[$__R.stack.length - 1].f();

                  if ($__R.mode && app97 || !$__R.mode && target === 183) {
                    if ($__R.mode) element16 = "bool";

                    if ($__R.mode) {
                      target = 183;
                      element17 = runtime.box.getField(v, "b");
                    } else if (target === 183) element17 = $__R.stack[$__R.stack.length - 1].f();

                    return { type: element16, value: element17 };
                  } else if ($__R.mode || !$__R.mode && (target === 187 || target === 186 || target === 185 || target === 184)) {
                    if ($__R.mode) {
                      target = 184;
                      app98 = runtime.box.isCCustom(v);
                    } else if (target === 184) app98 = $__R.stack[$__R.stack.length - 1].f();

                    if ($__R.mode && app98 || !$__R.mode && target === 185) {
                      if ($__R.mode) element18 = "custom";

                      if ($__R.mode) {
                        target = 185;
                        element19 = runtime.box.getField(v, "datum");
                      } else if (target === 185) element19 = $__R.stack[$__R.stack.length - 1].f();

                      return { type: element18, value: element19 };
                    } else if ($__R.mode || !$__R.mode && (target === 187 || target === 186)) {
                      if ($__R.mode) {
                        target = 186;
                        app99 = runtime.box.isCEmpty(v);
                      } else if (target === 186) app99 = $__R.stack[$__R.stack.length - 1].f();

                      if ($__R.mode && app99) {
                        return { type: "empty" };
                      } else if ($__R.mode || !$__R.mode && target === 187) {
                        if ($__R.mode) {
                          target = 187;
                          app100 = runtime.box.ffi.throwMessageException("Internal error: Cannot unwrap non-cell content");
                        } else if (target === 187) app100 = $__R.stack[$__R.stack.length - 1].f();
                      }
                    }
                  }
                }
              }
            } catch (exn068) {
              if (exn068 instanceof $__T.Capture) {
                exn068.stack.push({
                  kind: "rest",
                  f: () => funExpr15.call(this, v),
                  locals: [app98, element15, app99, element16, element17, element18, element19, app100, app94, app95, element12, app96, element13, app97, element14],
                  index: target
                });
              }

              throw exn068;
            }
          };
          if ($__R.mode) runtime.box["makeLoadedTable"] = function funExpr16(headers, contents) {
            let target = null;
            let app103;
            let app101;

            if (!$__R.mode) {
              [app101, app103] = $__R.stack[$__R.stack.length - 1].locals;
              target = $__R.stack[$__R.stack.length - 1].index;
              $__R.stack.pop();
            }

            try {
              if ($__R.mode) {
                target = 188;
                app101 = $__R.suspend();
              } else if (target === 188) app101 = $__R.stack[$__R.stack.length - 1].f();

              if ($__R.mode) {
                target = 189;
                app103 = headers.map(function funExpr17(h) {
                  let target = null;
                  let app102;

                  if (!$__R.mode) {
                    [app102] = $__R.stack[$__R.stack.length - 1].locals;
                    target = $__R.stack[$__R.stack.length - 1].index;
                    $__R.stack.pop();
                  }

                  try {
                    if ($__R.mode) {
                      target = 190;
                      app102 = $__R.suspend();
                    } else if (target === 190) app102 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn069) {
                    if (exn069 instanceof $__T.Capture) {
                      exn069.stack.push({
                        kind: "rest",
                        f: () => funExpr17.call(this, h),
                        locals: [app102],
                        index: target
                      });
                    }

                    throw exn069;
                  }

                  if ($__R.mode && h.sanitizer) {
                    return runtime.box.makeTuple([h.name, h.sanitizer]);
                  } else if ($__R.mode) {
                    return runtime.box.makeTuple(h);
                  }
                });
              } else if (target === 189) app103 = $__R.stack[$__R.stack.length - 1].f();

              // Headers can either be [name, sanitizer] arrays or
              // {name: name, sanitizer: sanitizer} objects
              if ($__R.mode) headers = app103;
            } catch (exn070) {
              if (exn070 instanceof $__T.Capture) {
                exn070.stack.push({
                  kind: "rest",
                  f: () => funExpr16.call(this, headers, contents),
                  locals: [app101, app103],
                  index: target
                });
              }

              throw exn070;
            }

            return runtime.box.makeTuple([headers, contents]);
          };

          if ($__R.mode) {
            target = 194;
            app105 = runtime.box.getField(ds.box, "is-CellContent");
          } else if (target === 194) app105 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) arg029 = app105.app;
          if ($__R.mode) arg030 = "CellContent";

          if ($__R.mode) {
            target = 195;
            app104 = runtime.box.makeCheckType(arg029, arg030);
          } else if (target === 195) app104 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box["checkCellContent"] = app104;
        } catch (exn071) {
          if (exn071 instanceof $__T.Capture) {
            exn071.stack.push({
              kind: "rest",
              f: () => funExpr6.call(this, ds),
              locals: [arg029, app79, app104, app105, arg030, element3, element4, element5, app80, element6, app81, element7, app71, app82, element10, element8, app72, element11, arg014, element9, arg015],
              index: target
            });
          }

          throw exn071;
        }
      },
      "builtin://reactors": function funExpr18(reactor) {
        let target = null;
        let app107;
        let arg034;
        let app108;
        let arg033;
        var r;
        let arg032;
        let arg031;
        let app106;

        if (!$__R.mode) {
          [r, app106, app107, app108, arg031, arg032, arg033, arg034] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 196;
            app106 = $__R.suspend();
          } else if (target === 196) app106 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 197;
            arg031 = runtime.box.getField(reactor, "provide-plus-types");
          } else if (target === 197) arg031 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) arg032 = "values";

          if ($__R.mode) {
            target = 198;
            r = runtime.box.getField(arg031, arg032);
          } else if (target === 198) r = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) arg033 = "makeReactor";

          if ($__R.mode) {
            target = 199;
            app108 = runtime.box.getField(r, "make-reactor");
          } else if (target === 199) app108 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) arg034 = app108.app;

          if ($__R.mode) {
            target = 200;
            app107 = runtime.box.setParam(arg033, arg034);
          } else if (target === 200) app107 = $__R.stack[$__R.stack.length - 1].f();
        } catch (exn072) {
          if (exn072 instanceof $__T.Capture) {
            exn072.stack.push({
              kind: "rest",
              f: () => funExpr18.call(this, reactor),
              locals: [r, app106, app107, app108, arg031, arg032, arg033, arg034],
              index: target
            });
          }

          throw exn072;
        }
      },
      "builtin://checker": function funExpr19(checker) {
        let target = null;
        let app112;
        var currentChecker;
        let app111;
        let arg038;
        let arg037;
        let app110;
        let arg036;
        let arg035;
        let app109;

        if (!$__R.mode) {
          [app110, app111, app112, app109, arg035, currentChecker, arg036, arg037, arg038] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 201;
            app109 = $__R.suspend();
          } else if (target === 201) app109 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 202;
            arg035 = runtime.box.getField(checker, "provide-plus-types");
          } else if (target === 202) arg035 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) arg036 = "values";

          if ($__R.mode) {
            target = 203;
            app110 = runtime.box.getField(arg035, arg036);
          } else if (target === 203) app110 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) checker = app110;
          // NOTE(joe): This is the place to add checkAll

          if ($__R.mode) {
            target = 204;
            arg037 = runtime.box.makeString(main.box);
          } else if (target === 204) arg037 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) arg038 = true;

          if ($__R.mode) {
            target = 205;
            app111 = runtime.box.getField(checker, "make-check-context");
          } else if (target === 205) app111 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 206;
            currentChecker = app111.app(arg037, arg038);
          } else if (target === 206) currentChecker = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 207;
            app112 = runtime.box.setParam("current-checker", currentChecker);
          } else if (target === 207) app112 = $__R.stack[$__R.stack.length - 1].f();
        } catch (exn073) {
          if (exn073 instanceof $__T.Capture) {
            exn073.stack.push({
              kind: "rest",
              f: () => funExpr19.call(this, checker),
              locals: [app110, app111, app112, app109, arg035, currentChecker, arg036, arg037, arg038],
              index: target
            });
          }

          throw exn073;
        }
      }
    };
    // last thing to run
  } catch (exn074) {
    if (exn074 instanceof $__T.Capture) {
      exn074.stack.push({
        kind: "rest",
        f: () => delimit41.call(this),
        locals: [],
        index: target
      });
    }

    throw exn074;
  }
});
if ($__R.mode) $__R.delimit(function delimit42() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    if ($__R.mode) postLoadHooks.box[main.box] = function funExpr20(answer) {
      let target = null;
      let app118;
      let app117;
      let app116;
      var getStack;
      var checker;
      let arg040;
      let arg039;
      var checkerLib;
      let app113;
      var getStackP;
      var toCall;
      var checks;

      if (!$__R.mode) {
        [arg039, checks, checkerLib, app113, getStack, app116, checker, app117, arg040, app118, toCall, getStackP] = $__R.stack[$__R.stack.length - 1].locals;
        target = $__R.stack[$__R.stack.length - 1].index;
        $__R.stack.pop();
      }

      try {
        if ($__R.mode) checks = {
          box: undefined
        };
        if ($__R.mode) toCall = {
          box: undefined
        };
        if ($__R.mode) getStackP = {
          box: undefined
        };

        if ($__R.mode) {
          target = 209;
          app113 = $__R.suspend();
        } else if (target === 209) app113 = $__R.stack[$__R.stack.length - 1].f();

        if ($__R.mode) checkerLib = runtime.box.modules["builtin://checker"];

        if ($__R.mode) {
          target = 210;
          arg039 = runtime.box.getField(checkerLib, "provide-plus-types");
        } else if (target === 210) arg039 = $__R.stack[$__R.stack.length - 1].f();

        if ($__R.mode) arg040 = "values";

        if ($__R.mode) {
          target = 211;
          checker = runtime.box.getField(arg039, arg040);
        } else if (target === 211) checker = $__R.stack[$__R.stack.length - 1].f();

        if ($__R.mode) getStack = function funExpr21(err) {
          let target = null;
          var locList;
          var locArray;
          let app115;
          let app114;

          if (!$__R.mode) {
            [app114, app115, locList, locArray] = $__R.stack[$__R.stack.length - 1].locals;
            target = $__R.stack[$__R.stack.length - 1].index;
            $__R.stack.pop();
          }

          try {
            if ($__R.mode) {
              target = 212;
              app114 = $__R.suspend();
            } else if (target === 212) app114 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) {
              target = 213;
              app115 = stackLib.convertExceptionToPyretStackTrace(err.val, program);
            } else if (target === 213) app115 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) err.val.pyretStack = app115;

            if ($__R.mode) {
              target = 214;
              locArray = err.val.pyretStack.map(runtime.box.makeSrcloc);
            } else if (target === 214) locArray = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) {
              target = 215;
              locList = runtime.box.ffi.makeList(locArray);
            } else if (target === 215) locList = $__R.stack[$__R.stack.length - 1].f();

            return locList;
          } catch (exn075) {
            if (exn075 instanceof $__T.Capture) {
              exn075.stack.push({
                kind: "rest",
                f: () => funExpr21.call(this, err),
                locals: [app114, app115, locList, locArray],
                index: target
              });
            }

            throw exn075;
          }
        };

        if ($__R.mode) {
          target = 216;
          app116 = runtime.box.makeFunction(getStack, "get-stack");
        } else if (target === 216) app116 = $__R.stack[$__R.stack.length - 1].f();

        if ($__R.mode) getStackP.box = app116;

        if ($__R.mode) {
          target = 217;
          app117 = runtime.box.getField(checker, "render-check-results-stack");
        } else if (target === 217) app117 = $__R.stack[$__R.stack.length - 1].f();

        if ($__R.mode) toCall.box = app117;

        if ($__R.mode) {
          target = 218;
          app118 = runtime.box.getField(answer, "checks");
        } else if (target === 218) app118 = $__R.stack[$__R.stack.length - 1].f();

        if ($__R.mode) checks.box = app118;
      } catch (exn076) {
        if (exn076 instanceof $__T.Capture) {
          exn076.stack.push({
            kind: "rest",
            f: () => funExpr20.call(this, answer),
            locals: [arg039, checks, checkerLib, app113, getStack, app116, checker, app117, arg040, app118, toCall, getStackP],
            index: target
          });
        }

        throw exn076;
      }

      return runtime.box.safeCall(function funExpr22() {
        let target = null;
        let app119;

        if (!$__R.mode) {
          [app119] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 220;
            app119 = $__R.suspend();
          } else if (target === 220) app119 = $__R.stack[$__R.stack.length - 1].f();
        } catch (exn077) {
          if (exn077 instanceof $__T.Capture) {
            exn077.stack.push({
              kind: "rest",
              f: () => funExpr22.call(this),
              locals: [app119],
              index: target
            });
          }

          throw exn077;
        }

        return toCall.box.app(checks.box, getStackP.box);
      }, function funExpr23(summary) {
        let target = null;
        let app125;
        let app124;
        var failed;
        var errs;
        let app123;
        let app122;
        let arg041;
        let app121;
        let app120;

        if (!$__R.mode) {
          [app120, errs, app121, app122, app123, app124, app125, arg041, failed] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 222;
            app120 = $__R.suspend();
          } else if (target === 222) app120 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) {
            target = 223;
            app121 = runtime.box.isObject(summary);
          } else if (target === 223) app121 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode && app121 || !$__R.mode && (target === 230 || target === 229 || target === 228 || target === 227 || target === 226 || target === 225 || target === 224)) {
            if ($__R.mode) {
              target = 224;
              arg041 = runtime.box.getField(summary, "message");
            } else if (target === 224) arg041 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) {
              target = 225;
              app122 = process.stdout.write(arg041);
            } else if (target === 225) app122 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) {
              target = 226;
              app123 = process.stdout.write("\n");
            } else if (target === 226) app123 = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) {
              target = 227;
              errs = runtime.box.getField(summary, "errored");
            } else if (target === 227) errs = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode) {
              target = 228;
              failed = runtime.box.getField(summary, "failed");
            } else if (target === 228) failed = $__R.stack[$__R.stack.length - 1].f();

            if ($__R.mode && (errs !== 0 || failed !== 0) || !$__R.mode && target === 229) {
              if ($__R.mode) {
                target = 229;
                app124 = process.exit(EXIT_ERROR_CHECK_FAILURES.box);
              } else if (target === 229) app124 = $__R.stack[$__R.stack.length - 1].f();
            } else if ($__R.mode || !$__R.mode && target === 230) {
              if ($__R.mode) {
                target = 230;
                app125 = process.exit(EXIT_SUCCESS.box);
              } else if (target === 230) app125 = $__R.stack[$__R.stack.length - 1].f();
            }
          }
        } catch (exn078) {
          if (exn078 instanceof $__T.Capture) {
            exn078.stack.push({
              kind: "rest",
              f: () => funExpr23.call(this, summary),
              locals: [app120, errs, app121, app122, app123, app124, app125, arg041, failed],
              index: target
            });
          }

          throw exn078;
        }
      }, "postLoadHooks[main]:render-check-results-stack");
    };
  } catch (exn079) {
    if (exn079 instanceof $__T.Capture) {
      exn079.stack.push({
        kind: "rest",
        f: () => delimit42.call(this),
        locals: [],
        index: target
      });
    }

    throw exn079;
  }
});
if ($__R.mode) $__R.delimit(function delimit43() {
  let target = null;

  if (!$__R.mode) {
    [] = $__R.stack[$__R.stack.length - 1].locals;
    target = $__R.stack[$__R.stack.length - 1].index;
    $__R.stack.pop();
  }

  try {
    //});
    if ($__R.mode) {
      target = 232;
      app127 = runtime.box.runThunk(function funExpr29() {
        let target = null;
        let app126;

        if (!$__R.mode) {
          [app126] = $__R.stack[$__R.stack.length - 1].locals;
          target = $__R.stack[$__R.stack.length - 1].index;
          $__R.stack.pop();
        }

        try {
          if ($__R.mode) {
            target = 233;
            app126 = $__R.suspend();
          } else if (target === 233) app126 = $__R.stack[$__R.stack.length - 1].f();

          if ($__R.mode) runtime.box.modules = {};
          // staticModules contains the stopified code
        } catch (exn080) {
          if (exn080 instanceof $__T.Capture) {
            exn080.stack.push({
              kind: "rest",
              f: () => funExpr29.call(this),
              locals: [app126],
              index: target
            });
          }

          throw exn080;
        }

        return runtime.box.runStandalone(staticModules.box, runtime.box.modules, depMap.box, toLoad.box, postLoadHooks.box);
      }, onComplete.box);
    } else if (target === 232) app127 = $__R.stack[$__R.stack.length - 1].f();

    /*return*/
  } catch (exn081) {
    if (exn081 instanceof $__T.Capture) {
      exn081.stack.push({
        kind: "rest",
        f: () => delimit43.call(this),
        locals: [],
        index: target
      });
    }

    throw exn081;
  }
});
})
