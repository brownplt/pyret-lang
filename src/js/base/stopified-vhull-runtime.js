  define("pyret-base/js/stopified-vhull-runtime", function() {
      return {
          generateDefs(thisRuntime) {
              const $__R = $__T.getRTS();
              const $handleNew = $__R.handleNew.bind($__R);
              const captureCC = $__R.captureCC;
              const suspendCC = $__R.suspendCC;
              const SENTINAL = {};
              var raw_list_fold;
              var raw_array_filter;
              var raw_list_filter;
              var raw_array_map1;
              var raw_list_map;
              var raw_array_mapi;
              var raw_array_each;
              var raw_array_map;
              var raw_array_fold;
              var raw_array_build_opt;
              var raw_array_build;
              var safeCall;
              var eachLoop;
              var run;
              var runThunk;
              var execThunk;
              thisRuntime = {
                  box: thisRuntime
              };
              execThunk = {
                  box: function fun05(thunk) {
                      let app10;
                      let app9;
                      let app8;
                      var result;
                      let app7;
                      let app6;
                      var $i00;
                      var $a;
                      let app5;
                      var wrapResult;

                      if (!$__R.mode) {
                          [app10, $i00, wrapResult, app5, app6, app7, app8, app9, result, $a] = $__R.stack[$__R.stack.length - 1].locals;
                          target = $__R.stack[$__R.stack.length - 1].index;
                          $__R.stack.pop();
                      }

                      function captureLocals(frame) {
                          frame.locals = [app10, $i00, wrapResult, app5, app6, app7, app8, app9, result, $a];
                      }

                      const materializedArguments = arguments;
                      if ($__R.mode) wrapResult = {
                          box: function fun04(res) {
                              let app4;
                              let arg01;
                              let arg02;
                              let arg03;
                              let arg00;
                              let app3;
                              let app2;
                              let app1;
                              let app0;

                              if (!$__R.mode) {
                                  [app0, app1, app2, app3, app4, arg00, arg01, arg02, arg03] = $__R.stack[$__R.stack.length - 1].locals;
                                  target = $__R.stack[$__R.stack.length - 1].index;
                                  $__R.stack.pop();
                              }

                              function captureLocals(frame) {
                                  frame.locals = [app0, app1, app2, app3, app4, arg00, arg01, arg02, arg03];
                              }

                              try {
                                  if ($__R.mode) {
                                      app0 = $__R.suspend();
                                  } else if (target === 1) app0 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn0) {
                                  if (exn0 instanceof $__T.Capture) {
                                      exn0.stack.push({
                                          kind: "rest",
                                          f: () => fun04.call(this, res),
                                          index: 1
                                      });
                                      captureLocals(exn0.stack[exn0.stack.length - 1]);
                                  }

                                  throw exn0;
                              }

                              try {
                                  if ($__R.mode) {
                                      app1 = isSuccessResult(res);
                                  } else if (target === 2) app1 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn1) {
                                  if (exn1 instanceof $__T.Capture) {
                                      exn1.stack.push({
                                          kind: "rest",
                                          f: () => fun04.call(this, res),
                                          index: 2
                                      });
                                      captureLocals(exn1.stack[exn1.stack.length - 1]);
                                  }

                                  throw exn1;
                              }

                              if ($__R.mode && app1) {
                                  return thisRuntime.box.ffi.makeLeft(res.result);
                              } else if ($__R.mode || !$__R.mode && (target === 13 || target === 11 || target === 10 || target === 8 || target === 6 || target === 5 || target === 4)) {
                                  try {
                                      if ($__R.mode) {
                                          app2 = isFailureResult(res);
                                      } else if (target === 4) app2 = $__R.stack[$__R.stack.length - 1].f();
                                  } catch (exn2) {
                                      if (exn2 instanceof $__T.Capture) {
                                          exn2.stack.push({
                                              kind: "rest",
                                              f: () => fun04.call(this, res),
                                              index: 4
                                          });
                                          captureLocals(exn2.stack[exn2.stack.length - 1]);
                                      }

                                      throw exn2;
                                  }

                                  if ($__R.mode && app2 || !$__R.mode && (target === 11 || target === 10 || target === 8 || target === 6 || target === 5)) {
                                      try {
                                          if ($__R.mode) {
                                              app3 = isPyretException(res.exn);
                                          } else if (target === 5) app3 = $__R.stack[$__R.stack.length - 1].f();
                                      } catch (exn3) {
                                          if (exn3 instanceof $__T.Capture) {
                                              exn3.stack.push({
                                                  kind: "rest",
                                                  f: () => fun04.call(this, res),
                                                  index: 5
                                              });
                                              captureLocals(exn3.stack[exn3.stack.length - 1]);
                                          }

                                          throw exn3;
                                      }

                                      if ($__R.mode && app3 || !$__R.mode && target === 6) {
                                          try {
                                              if ($__R.mode) {
                                                  arg00 = makeOpaque(res.exn);
                                              } else if (target === 6) arg00 = $__R.stack[$__R.stack.length - 1].f();
                                          } catch (exn4) {
                                              if (exn4 instanceof $__T.Capture) {
                                                  exn4.stack.push({
                                                      kind: "rest",
                                                      f: () => fun04.call(this, res),
                                                      index: 6
                                                  });
                                                  captureLocals(exn4.stack[exn4.stack.length - 1]);
                                              }

                                              throw exn4;
                                          }

                                          return thisRuntime.box.ffi.makeRight(arg00);
                                      } else if ($__R.mode || !$__R.mode && (target === 11 || target === 10 || target === 8)) {
                                          try {
                                              if ($__R.mode) {
                                                  arg03 = thisRuntime.box.ffi.makeMessageException(String(res.exn + "\n" + res.exn.stack));
                                              } else if (target === 8) arg03 = $__R.stack[$__R.stack.length - 1].f();
                                          } catch (exn5) {
                                              if (exn5 instanceof $__T.Capture) {
                                                  exn5.stack.push({
                                                      kind: "rest",
                                                      f: () => fun04.call(this, res),
                                                      index: 8
                                                  });
                                                  captureLocals(exn5.stack[exn5.stack.length - 1]);
                                              }

                                              throw exn5;
                                          }

                                          try {
                                              if ($__R.mode) {
                                                  arg02 = makePyretFailException(arg03);
                                              } else if (target === 10) arg02 = $__R.stack[$__R.stack.length - 1].f();
                                          } catch (exn6) {
                                              if (exn6 instanceof $__T.Capture) {
                                                  exn6.stack.push({
                                                      kind: "rest",
                                                      f: () => fun04.call(this, res),
                                                      index: 10
                                                  });
                                                  captureLocals(exn6.stack[exn6.stack.length - 1]);
                                              }

                                              throw exn6;
                                          }

                                          try {
                                              if ($__R.mode) {
                                                  arg01 = makeOpaque(arg02);
                                              } else if (target === 11) arg01 = $__R.stack[$__R.stack.length - 1].f();
                                          } catch (exn7) {
                                              if (exn7 instanceof $__T.Capture) {
                                                  exn7.stack.push({
                                                      kind: "rest",
                                                      f: () => fun04.call(this, res),
                                                      index: 11
                                                  });
                                                  captureLocals(exn7.stack[exn7.stack.length - 1]);
                                              }

                                              throw exn7;
                                          }

                                          return thisRuntime.box.ffi.makeRight(arg01);
                                      }
                                  } else if ($__R.mode || !$__R.mode && target === 13) {
                                      try {
                                          if ($__R.mode) {
                                              app4 = CONSOLE.error("Bad execThunk result: ", res);
                                          } else if (target === 13) app4 = $__R.stack[$__R.stack.length - 1].f();
                                      } catch (exn8) {
                                          if (exn8 instanceof $__T.Capture) {
                                              exn8.stack.push({
                                                  kind: "rest",
                                                  f: () => fun04.call(this, res),
                                                  index: 13
                                              });
                                              captureLocals(exn8.stack[exn8.stack.length - 1]);
                                          }

                                          throw exn8;
                                      }

                                      return;
                                  }
                              }
                          }
                      };

                      try {
                          if ($__R.mode) {
                              app5 = $__R.suspend();
                          } else if (target === 14) app5 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn9) {
                          if (exn9 instanceof $__T.Capture) {
                              exn9.stack.push({
                                  kind: "rest",
                                  f: () => fun05.apply(this, materializedArguments),
                                  index: 14
                              });
                              captureLocals(exn9.stack[exn9.stack.length - 1]);
                          }

                          throw exn9;
                      }

                      if ($__R.mode && arguments.length !== 1 || !$__R.mode && (target === 17 || target === 16 || target === 15)) {
                          if ($__R.mode) {
                              $a = new Array(arguments.length);
                              $i00 = 0;
                          }

                          loop_break1: while (!$__R.mode && target === 16 || $__R.mode && $i00 < arguments.length) {
                              loop_continue1: {
                                  try {
                                      if ($__R.mode) {
                                          app6 = $__R.suspend();
                                      } else if (target === 16) app6 = $__R.stack[$__R.stack.length - 1].f();
                                  } catch (exn10) {
                                      if (exn10 instanceof $__T.Capture) {
                                          exn10.stack.push({
                                              kind: "rest",
                                              f: () => fun05.apply(this, materializedArguments),
                                              index: 16
                                          });
                                          captureLocals(exn10.stack[exn10.stack.length - 1]);
                                      }

                                      throw exn10;
                                  }

                                  if ($__R.mode) $a[$i00] = arguments[$i00];
                              }

                              if ($__R.mode) $i00++;
                          }

                          try {
                              if ($__R.mode) {
                                  app7 = thisRuntime.box.ffi.throwArityErrorC(["run-task"], 1, $a);
                              } else if (target === 17) app7 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn11) {
                              if (exn11 instanceof $__T.Capture) {
                                  exn11.stack.push({
                                      kind: "rest",
                                      f: () => fun05.apply(this, materializedArguments),
                                      index: 17
                                  });
                                  captureLocals(exn11.stack[exn11.stack.length - 1]);
                              }

                              throw exn11;
                          }

                          throw app7;
                      }

                      try {
                          try {
                              if ($__R.mode) {
                                  app8 = thunk.app();
                              } else if (target === 18) app8 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn12) {
                              if (exn12 instanceof $__T.Capture) {
                                  exn12.stack.push({
                                      kind: "rest",
                                      f: () => fun05.apply(this, materializedArguments),
                                      index: 18
                                  });
                                  captureLocals(exn12.stack[exn12.stack.length - 1]);
                              }

                              throw exn12;
                          }

                          // thunk is from pyret and can be a pyret callback
                          if ($__R.mode) result = app8;

                          try {
                              if ($__R.mode) {
                                  app9 = $handleNew(SuccessResult, result, {});
                              } else if (target === 19) app9 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn13) {
                              if (exn13 instanceof $__T.Capture) {
                                  exn13.stack.push({
                                      kind: "rest",
                                      f: () => fun05.apply(this, materializedArguments),
                                      index: 19
                                  });
                                  captureLocals(exn13.stack[exn13.stack.length - 1]);
                              }

                              throw exn13;
                          }

                          if ($__R.mode) result = app9;
                      } catch (e) {
                          if (e instanceof $__T.Capture || e instanceof $__T.Restore) throw e;

                          try {
                              if ($__R.mode) {
                                  app10 = makeFailureResult(e, {});
                              } else if (target === 20) app10 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn14) {
                              if (exn14 instanceof $__T.Capture) {
                                  exn14.stack.push({
                                      kind: "rest",
                                      f: () => fun05.apply(this, materializedArguments),
                                      index: 20
                                  });
                                  captureLocals(exn14.stack[exn14.stack.length - 1]);
                              }

                              throw exn14;
                          }

                          if ($__R.mode) result = app10;
                      }
                      return wrapResult.box(result);
                  }
              };
              runThunk = {
                  box: function fun03(f, then) {
                      let arg04;
                      let app12;
                      var resultCtor;
                      var fnResult;
                      let app11;

                      if (!$__R.mode) {
                          [app11, fnResult, resultCtor, arg04, app12] = $__R.stack[$__R.stack.length - 1].locals;
                          target = $__R.stack[$__R.stack.length - 1].index;
                          $__R.stack.pop();
                      }

                      function captureLocals(frame) {
                          frame.locals = [app11, fnResult, resultCtor, arg04, app12];
                      }

                      try {
                          if ($__R.mode) {
                              app11 = $__R.suspend();
                          } else if (target === 22) app11 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn15) {
                          if (exn15 instanceof $__T.Capture) {
                              exn15.stack.push({
                                  kind: "rest",
                                  f: () => fun03.call(this, f, then),
                                  index: 22
                              });
                              captureLocals(exn15.stack[exn15.stack.length - 1]);
                          }

                          throw exn15;
                      }

                      try {
                          try {
                              if ($__R.mode) {
                                  app12 = f();
                              } else if (target === 23) app12 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn16) {
                              if (exn16 instanceof $__T.Capture) {
                                  exn16.stack.push({
                                      kind: "rest",
                                      f: () => fun03.call(this, f, then),
                                      index: 23
                                  });
                                  captureLocals(exn16.stack[exn16.stack.length - 1]);
                              }

                              throw exn16;
                          }

                          if ($__R.mode) {
                              fnResult = app12;
                              resultCtor = thisRuntime.box.makeSuccessResult;
                          }
                      } catch (e) {
                          if (e instanceof $__T.Capture || e instanceof $__T.Restore) throw e;

                          if ($__R.mode) {
                              fnResult = e;
                              resultCtor = thisRuntime.box.makeFailureResult;
                          }
                      }

                      try {
                          if ($__R.mode) {
                              arg04 = resultCtor(fnResult);
                          } else if (target === 24) arg04 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn17) {
                          if (exn17 instanceof $__T.Capture) {
                              exn17.stack.push({
                                  kind: "rest",
                                  f: () => fun03.call(this, f, then),
                                  index: 24
                              });
                              captureLocals(exn17.stack[exn17.stack.length - 1]);
                          }

                          throw exn17;
                      }

                      return then(arg04);
                  }
              };
              run = {
                  box: function fun02(program, namespace, options) {
                      let app13;

                      if (!$__R.mode) {
                          [app13] = $__R.stack[$__R.stack.length - 1].locals;
                          target = $__R.stack[$__R.stack.length - 1].index;
                          $__R.stack.pop();
                      }

                      function captureLocals(frame) {
                          frame.locals = [app13];
                      }

                      try {
                          if ($__R.mode) {
                              app13 = $__R.suspend();
                          } else if (target === 26) app13 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn18) {
                          if (exn18 instanceof $__T.Capture) {
                              exn18.stack.push({
                                  kind: "rest",
                                  f: () => fun02.call(this, program, namespace, options),
                                  index: 26
                              });
                              captureLocals(exn18.stack[exn18.stack.length - 1]);
                          }

                          throw exn18;
                      }

                      return program(thisRuntime.box, namespace);
                  }
              };
              eachLoop = {
                  box: function fun01(fun, start, stop) {
                      let app16;
                      let app15;
                      var i;
                      let app14;

                      if (!$__R.mode) {
                          [app14, i, app15, app16] = $__R.stack[$__R.stack.length - 1].locals;
                          target = $__R.stack[$__R.stack.length - 1].index;
                          $__R.stack.pop();
                      }

                      function captureLocals(frame) {
                          frame.locals = [app14, i, app15, app16];
                      }

                      try {
                          if ($__R.mode) {
                              app14 = $__R.suspend();
                          } else if (target === 28) app14 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn19) {
                          if (exn19 instanceof $__T.Capture) {
                              exn19.stack.push({
                                  kind: "rest",
                                  f: () => fun01.call(this, fun, start, stop),
                                  index: 28
                              });
                              captureLocals(exn19.stack[exn19.stack.length - 1]);
                          }

                          throw exn19;
                      }

                      if ($__R.mode) i = start;

                      loop_break0: while (!$__R.mode && (target === 30 || target === 29) || $__R.mode && i < stop) {
                          loop_continue0: {
                              try {
                                  if ($__R.mode) {
                                      app15 = $__R.suspend();
                                  } else if (target === 29) app15 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn20) {
                                  if (exn20 instanceof $__T.Capture) {
                                      exn20.stack.push({
                                          kind: "rest",
                                          f: () => fun01.call(this, fun, start, stop),
                                          index: 29
                                      });
                                      captureLocals(exn20.stack[exn20.stack.length - 1]);
                                  }

                                  throw exn20;
                              }

                              try {
                                  if ($__R.mode) {
                                      app16 = fun.app(i);
                                  } else if (target === 30) app16 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn21) {
                                  if (exn21 instanceof $__T.Capture) {
                                      exn21.stack.push({
                                          kind: "rest",
                                          f: () => fun01.call(this, fun, start, stop),
                                          index: 30
                                      });
                                      captureLocals(exn21.stack[exn21.stack.length - 1]);
                                  }

                                  throw exn21;
                              }
                          }

                          if ($__R.mode) i++;
                      }

                      return thisRuntime.box.nothing;
                  }
              };
              safeCall = {
                  box: function fun00(fun, after, stackFrame) {
                      let arg05;
                      let app17;

                      if (!$__R.mode) {
                          [app17, arg05] = $__R.stack[$__R.stack.length - 1].locals;
                          target = $__R.stack[$__R.stack.length - 1].index;
                          $__R.stack.pop();
                      }

                      function captureLocals(frame) {
                          frame.locals = [app17, arg05];
                      }

                      try {
                          if ($__R.mode) {
                              app17 = $__R.suspend();
                          } else if (target === 31) app17 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn22) {
                          if (exn22 instanceof $__T.Capture) {
                              exn22.stack.push({
                                  kind: "rest",
                                  f: () => fun00.call(this, fun, after, stackFrame),
                                  index: 31
                              });
                              captureLocals(exn22.stack[exn22.stack.length - 1]);
                          }

                          throw exn22;
                      }

                      try {
                          if ($__R.mode) {
                              arg05 = fun();
                          } else if (target === 32) arg05 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn23) {
                          if (exn23 instanceof $__T.Capture) {
                              exn23.stack.push({
                                  kind: "rest",
                                  f: () => fun00.call(this, fun, after, stackFrame),
                                  index: 32
                              });
                              captureLocals(exn23.stack[exn23.stack.length - 1]);
                          }

                          throw exn23;
                      }

                      return after(arg05);
                  }
              };

              // NOTE(rachit): stackFrame is not used

              raw_array_build = function funExpr0(f, len) {
                  let app24;
                  let arg06;
                  let app23;
                  var arr;
                  var curIdx;
                  let app22;
                  let app21;
                  let app20;
                  let app19;
                  var $i01;
                  var $a;
                  let app18;

                  if (!$__R.mode) {
                      [app21, app22, arr, app23, app24, $i01, app18, app19, arg06, curIdx, $a, app20] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [app21, app22, arr, app23, app24, $i01, app18, app19, arg06, curIdx, $a, app20];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app18 = $__R.suspend();
                      } else if (target === 34) app18 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn24) {
                      if (exn24 instanceof $__T.Capture) {
                          exn24.stack.push({
                              kind: "rest",
                              f: () => funExpr0.apply(this, materializedArguments),
                              index: 34
                          });
                          captureLocals(exn24.stack[exn24.stack.length - 1]);
                      }

                      throw exn24;
                  }

                  if ($__R.mode && arguments.length !== 2 || !$__R.mode && (target === 37 || target === 36 || target === 35)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i01 = 0;
                      }

                      loop_break2: while (!$__R.mode && target === 36 || $__R.mode && $i01 < arguments.length) {
                          loop_continue2: {
                              try {
                                  if ($__R.mode) {
                                      app19 = $__R.suspend();
                                  } else if (target === 36) app19 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn25) {
                                  if (exn25 instanceof $__T.Capture) {
                                      exn25.stack.push({
                                          kind: "rest",
                                          f: () => funExpr0.apply(this, materializedArguments),
                                          index: 36
                                      });
                                      captureLocals(exn25.stack[exn25.stack.length - 1]);
                                  }

                                  throw exn25;
                              }

                              if ($__R.mode) $a[$i01] = arguments[$i01];
                          }

                          if ($__R.mode) $i01++;
                      }

                      try {
                          if ($__R.mode) {
                              app20 = thisRuntime.box.ffi.throwArityErrorC(["raw-array-build"], 2, $a);
                          } else if (target === 37) app20 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn26) {
                          if (exn26 instanceof $__T.Capture) {
                              exn26.stack.push({
                                  kind: "rest",
                                  f: () => funExpr0.apply(this, materializedArguments),
                                  index: 37
                              });
                              captureLocals(exn26.stack[exn26.stack.length - 1]);
                          }

                          throw exn26;
                      }

                      throw app20;
                  }

                  try {
                      if ($__R.mode) {
                          app21 = thisRuntime.box.checkFunction(f);
                      } else if (target === 38) app21 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn27) {
                      if (exn27 instanceof $__T.Capture) {
                          exn27.stack.push({
                              kind: "rest",
                              f: () => funExpr0.apply(this, materializedArguments),
                              index: 38
                          });
                          captureLocals(exn27.stack[exn27.stack.length - 1]);
                      }

                      throw exn27;
                  }

                  try {
                      if ($__R.mode) {
                          app22 = thisRuntime.box.checkNumber(len);
                      } else if (target === 39) app22 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn28) {
                      if (exn28 instanceof $__T.Capture) {
                          exn28.stack.push({
                              kind: "rest",
                              f: () => funExpr0.apply(this, materializedArguments),
                              index: 39
                          });
                          captureLocals(exn28.stack[exn28.stack.length - 1]);
                      }

                      throw exn28;
                  }

                  if ($__R.mode) {
                      curIdx = 0;
                      arr = new Array();
                  }

                  loop_break3: while (!$__R.mode && (target === 43 || target === 42 || target === 41) || $__R.mode && curIdx < len) {
                      loop_continue3: {
                          try {
                              if ($__R.mode) {
                                  app23 = $__R.suspend();
                              } else if (target === 41) app23 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn29) {
                              if (exn29 instanceof $__T.Capture) {
                                  exn29.stack.push({
                                      kind: "rest",
                                      f: () => funExpr0.apply(this, materializedArguments),
                                      index: 41
                                  });
                                  captureLocals(exn29.stack[exn29.stack.length - 1]);
                              }

                              throw exn29;
                          }

                          try {
                              if ($__R.mode) {
                                  arg06 = f.app(curIdx);
                              } else if (target === 42) arg06 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn30) {
                              if (exn30 instanceof $__T.Capture) {
                                  exn30.stack.push({
                                      kind: "rest",
                                      f: () => funExpr0.apply(this, materializedArguments),
                                      index: 42
                                  });
                                  captureLocals(exn30.stack[exn30.stack.length - 1]);
                              }

                              throw exn30;
                          }

                          try {
                              if ($__R.mode) {
                                  app24 = arr.push(arg06);
                              } else if (target === 43) app24 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn31) {
                              if (exn31 instanceof $__T.Capture) {
                                  exn31.stack.push({
                                      kind: "rest",
                                      f: () => funExpr0.apply(this, materializedArguments),
                                      index: 43
                                  });
                                  captureLocals(exn31.stack[exn31.stack.length - 1]);
                              }

                              throw exn31;
                          }

                          if ($__R.mode) curIdx++;
                      }
                  }

                  return arr;
              };

              raw_array_build_opt = function funExpr1(f, len) {
                  let app33;
                  let arg07;
                  let app32;
                  let app31;
                  let app30;
                  var $ans;
                  var arr;
                  var curIdx;
                  let app29;
                  let app28;
                  let app27;
                  let app26;
                  var $i02;
                  var $a;
                  let app25;

                  if (!$__R.mode) {
                      [app32, app33, arr, $ans, app25, app26, app27, app28, $i02, app29, curIdx, $a, arg07, app30, app31] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [app32, app33, arr, $ans, app25, app26, app27, app28, $i02, app29, curIdx, $a, arg07, app30, app31];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app25 = $__R.suspend();
                      } else if (target === 44) app25 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn32) {
                      if (exn32 instanceof $__T.Capture) {
                          exn32.stack.push({
                              kind: "rest",
                              f: () => funExpr1.apply(this, materializedArguments),
                              index: 44
                          });
                          captureLocals(exn32.stack[exn32.stack.length - 1]);
                      }

                      throw exn32;
                  }

                  if ($__R.mode && arguments.length !== 2 || !$__R.mode && (target === 47 || target === 46 || target === 45)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i02 = 0;
                      }

                      loop_break4: while (!$__R.mode && target === 46 || $__R.mode && $i02 < arguments.length) {
                          loop_continue4: {
                              try {
                                  if ($__R.mode) {
                                      app26 = $__R.suspend();
                                  } else if (target === 46) app26 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn33) {
                                  if (exn33 instanceof $__T.Capture) {
                                      exn33.stack.push({
                                          kind: "rest",
                                          f: () => funExpr1.apply(this, materializedArguments),
                                          index: 46
                                      });
                                      captureLocals(exn33.stack[exn33.stack.length - 1]);
                                  }

                                  throw exn33;
                              }

                              if ($__R.mode) $a[$i02] = arguments[$i02];
                          }

                          if ($__R.mode) $i02++;
                      }

                      try {
                          if ($__R.mode) {
                              app27 = thisRuntime.box.ffi.throwArityErrorC(["raw-array-build"], 2, $a);
                          } else if (target === 47) app27 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn34) {
                          if (exn34 instanceof $__T.Capture) {
                              exn34.stack.push({
                                  kind: "rest",
                                  f: () => funExpr1.apply(this, materializedArguments),
                                  index: 47
                              });
                              captureLocals(exn34.stack[exn34.stack.length - 1]);
                          }

                          throw exn34;
                      }

                      throw app27;
                  }

                  try {
                      if ($__R.mode) {
                          app28 = thisRuntime.box.checkFunction(f);
                      } else if (target === 48) app28 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn35) {
                      if (exn35 instanceof $__T.Capture) {
                          exn35.stack.push({
                              kind: "rest",
                              f: () => funExpr1.apply(this, materializedArguments),
                              index: 48
                          });
                          captureLocals(exn35.stack[exn35.stack.length - 1]);
                      }

                      throw exn35;
                  }

                  try {
                      if ($__R.mode) {
                          app29 = thisRuntime.box.checkNumber(len);
                      } else if (target === 49) app29 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn36) {
                      if (exn36 instanceof $__T.Capture) {
                          exn36.stack.push({
                              kind: "rest",
                              f: () => funExpr1.apply(this, materializedArguments),
                              index: 49
                          });
                          captureLocals(exn36.stack[exn36.stack.length - 1]);
                      }

                      throw exn36;
                  }

                  if ($__R.mode) {
                      curIdx = 0;
                      arr = new Array();
                  }

                  loop_break5: while (!$__R.mode && (target === 55 || target === 54 || target === 53 || target === 52 || target === 51) || $__R.mode && curIdx < len) {
                      loop_continue5: {
                          try {
                              if ($__R.mode) {
                                  app30 = $__R.suspend();
                              } else if (target === 51) app30 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn37) {
                              if (exn37 instanceof $__T.Capture) {
                                  exn37.stack.push({
                                      kind: "rest",
                                      f: () => funExpr1.apply(this, materializedArguments),
                                      index: 51
                                  });
                                  captureLocals(exn37.stack[exn37.stack.length - 1]);
                              }

                              throw exn37;
                          }

                          try {
                              if ($__R.mode) {
                                  app31 = f.app(curIdx);
                              } else if (target === 52) app31 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn38) {
                              if (exn38 instanceof $__T.Capture) {
                                  exn38.stack.push({
                                      kind: "rest",
                                      f: () => funExpr1.apply(this, materializedArguments),
                                      index: 52
                                  });
                                  captureLocals(exn38.stack[exn38.stack.length - 1]);
                              }

                              throw exn38;
                          }

                          if ($__R.mode) $ans = app31;

                          try {
                              if ($__R.mode) {
                                  app32 = thisRuntime.box.ffi.isSome($ans);
                              } else if (target === 53) app32 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn39) {
                              if (exn39 instanceof $__T.Capture) {
                                  exn39.stack.push({
                                      kind: "rest",
                                      f: () => funExpr1.apply(this, materializedArguments),
                                      index: 53
                                  });
                                  captureLocals(exn39.stack[exn39.stack.length - 1]);
                              }

                              throw exn39;
                          }

                          if ($__R.mode && app32 || !$__R.mode && (target === 55 || target === 54)) {
                              try {
                                  if ($__R.mode) {
                                      arg07 = thisRuntime.box.getField($ans, "value");
                                  } else if (target === 54) arg07 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn40) {
                                  if (exn40 instanceof $__T.Capture) {
                                      exn40.stack.push({
                                          kind: "rest",
                                          f: () => funExpr1.apply(this, materializedArguments),
                                          index: 54
                                      });
                                      captureLocals(exn40.stack[exn40.stack.length - 1]);
                                  }

                                  throw exn40;
                              }

                              try {
                                  if ($__R.mode) {
                                      app33 = arr.push(arg07);
                                  } else if (target === 55) app33 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn41) {
                                  if (exn41 instanceof $__T.Capture) {
                                      exn41.stack.push({
                                          kind: "rest",
                                          f: () => funExpr1.apply(this, materializedArguments),
                                          index: 55
                                      });
                                      captureLocals(exn41.stack[exn41.stack.length - 1]);
                                  }

                                  throw exn41;
                              }
                          }

                          if ($__R.mode) curIdx++;
                      }
                  }

                  return arr;
              };

              raw_array_fold = function funExpr2(f, init, arr, start) {
                  var res;
                  let app41;
                  var length;
                  var currentAcc;
                  var currentIndex;
                  let app40;
                  let app39;
                  let app38;
                  let app37;
                  let app36;
                  let app35;
                  var $i03;
                  var $a;
                  let app34;

                  if (!$__R.mode) {
                      [res, app34, app35, app36, app37, app38, length, app39, currentAcc, $i03, currentIndex, app40, $a, app41] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [res, app34, app35, app36, app37, app38, length, app39, currentAcc, $i03, currentIndex, app40, $a, app41];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app34 = $__R.suspend();
                      } else if (target === 56) app34 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn42) {
                      if (exn42 instanceof $__T.Capture) {
                          exn42.stack.push({
                              kind: "rest",
                              f: () => funExpr2.apply(this, materializedArguments),
                              index: 56
                          });
                          captureLocals(exn42.stack[exn42.stack.length - 1]);
                      }

                      throw exn42;
                  }

                  if ($__R.mode && arguments.length !== 4 || !$__R.mode && (target === 59 || target === 58 || target === 57)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i03 = 0;
                      }

                      loop_break6: while (!$__R.mode && target === 58 || $__R.mode && $i03 < arguments.length) {
                          loop_continue6: {
                              try {
                                  if ($__R.mode) {
                                      app35 = $__R.suspend();
                                  } else if (target === 58) app35 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn43) {
                                  if (exn43 instanceof $__T.Capture) {
                                      exn43.stack.push({
                                          kind: "rest",
                                          f: () => funExpr2.apply(this, materializedArguments),
                                          index: 58
                                      });
                                      captureLocals(exn43.stack[exn43.stack.length - 1]);
                                  }

                                  throw exn43;
                              }

                              if ($__R.mode) $a[$i03] = arguments[$i03];
                          }

                          if ($__R.mode) $i03++;
                      }

                      try {
                          if ($__R.mode) {
                              app36 = thisRuntime.box.ffi.throwArityErrorC(["raw-array-fold"], 4, $a);
                          } else if (target === 59) app36 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn44) {
                          if (exn44 instanceof $__T.Capture) {
                              exn44.stack.push({
                                  kind: "rest",
                                  f: () => funExpr2.apply(this, materializedArguments),
                                  index: 59
                              });
                              captureLocals(exn44.stack[exn44.stack.length - 1]);
                          }

                          throw exn44;
                      }

                      throw app36;
                  }

                  try {
                      if ($__R.mode) {
                          app37 = thisRuntime.box.checkFunction(f);
                      } else if (target === 60) app37 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn45) {
                      if (exn45 instanceof $__T.Capture) {
                          exn45.stack.push({
                              kind: "rest",
                              f: () => funExpr2.apply(this, materializedArguments),
                              index: 60
                          });
                          captureLocals(exn45.stack[exn45.stack.length - 1]);
                      }

                      throw exn45;
                  }

                  try {
                      if ($__R.mode) {
                          app38 = thisRuntime.box.checkPyretVal(init);
                      } else if (target === 61) app38 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn46) {
                      if (exn46 instanceof $__T.Capture) {
                          exn46.stack.push({
                              kind: "rest",
                              f: () => funExpr2.apply(this, materializedArguments),
                              index: 61
                          });
                          captureLocals(exn46.stack[exn46.stack.length - 1]);
                      }

                      throw exn46;
                  }

                  try {
                      if ($__R.mode) {
                          app39 = thisRuntime.box.checkArray(arr);
                      } else if (target === 62) app39 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn47) {
                      if (exn47 instanceof $__T.Capture) {
                          exn47.stack.push({
                              kind: "rest",
                              f: () => funExpr2.apply(this, materializedArguments),
                              index: 62
                          });
                          captureLocals(exn47.stack[exn47.stack.length - 1]);
                      }

                      throw exn47;
                  }

                  try {
                      if ($__R.mode) {
                          app40 = thisRuntime.box.checkNumber(start);
                      } else if (target === 63) app40 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn48) {
                      if (exn48 instanceof $__T.Capture) {
                          exn48.stack.push({
                              kind: "rest",
                              f: () => funExpr2.apply(this, materializedArguments),
                              index: 63
                          });
                          captureLocals(exn48.stack[exn48.stack.length - 1]);
                      }

                      throw exn48;
                  }

                  if ($__R.mode) {
                      currentIndex = -1;
                      currentAcc = init;
                      length = arr.length;
                  }

                  loop_break7: while (!$__R.mode && (target === 65 || target === 64) || $__R.mode && currentIndex < length - 1) {
                      loop_continue7: {
                          try {
                              if ($__R.mode) {
                                  app41 = $__R.suspend();
                              } else if (target === 64) app41 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn49) {
                              if (exn49 instanceof $__T.Capture) {
                                  exn49.stack.push({
                                      kind: "rest",
                                      f: () => funExpr2.apply(this, materializedArguments),
                                      index: 64
                                  });
                                  captureLocals(exn49.stack[exn49.stack.length - 1]);
                              }

                              throw exn49;
                          }

                          if ($__R.mode) currentIndex += 1;

                          try {
                              if ($__R.mode) {
                                  res = f.app(currentAcc, arr[currentIndex], currentIndex + start);
                              } else if (target === 65) res = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn50) {
                              if (exn50 instanceof $__T.Capture) {
                                  exn50.stack.push({
                                      kind: "rest",
                                      f: () => funExpr2.apply(this, materializedArguments),
                                      index: 65
                                  });
                                  captureLocals(exn50.stack[exn50.stack.length - 1]);
                              }

                              throw exn50;
                          }

                          if ($__R.mode) currentAcc = res;
                      }
                  }

                  return currentAcc;
              };

              raw_array_map = function funExpr3(f, arr) {
                  var res;
                  let app47;
                  var newArray;
                  var length;
                  var currentIndex;
                  let app46;
                  let app45;
                  let app44;
                  let app43;
                  var $i04;
                  var $a;
                  let app42;

                  if (!$__R.mode) {
                      [app43, res, app44, app45, app46, app47, length, $i04, newArray, currentIndex, $a, app42] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [app43, res, app44, app45, app46, app47, length, $i04, newArray, currentIndex, $a, app42];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app42 = $__R.suspend();
                      } else if (target === 66) app42 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn51) {
                      if (exn51 instanceof $__T.Capture) {
                          exn51.stack.push({
                              kind: "rest",
                              f: () => funExpr3.apply(this, materializedArguments),
                              index: 66
                          });
                          captureLocals(exn51.stack[exn51.stack.length - 1]);
                      }

                      throw exn51;
                  }

                  if ($__R.mode && arguments.length !== 2 || !$__R.mode && (target === 69 || target === 68 || target === 67)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i04 = 0;
                      }

                      loop_break8: while (!$__R.mode && target === 68 || $__R.mode && $i04 < arguments.length) {
                          loop_continue8: {
                              try {
                                  if ($__R.mode) {
                                      app43 = $__R.suspend();
                                  } else if (target === 68) app43 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn52) {
                                  if (exn52 instanceof $__T.Capture) {
                                      exn52.stack.push({
                                          kind: "rest",
                                          f: () => funExpr3.apply(this, materializedArguments),
                                          index: 68
                                      });
                                      captureLocals(exn52.stack[exn52.stack.length - 1]);
                                  }

                                  throw exn52;
                              }

                              if ($__R.mode) $a[$i04] = arguments[$i04];
                          }

                          if ($__R.mode) $i04++;
                      }

                      try {
                          if ($__R.mode) {
                              app44 = thisRuntime.box.ffi.throwArityErrorC(["raw-array-map"], 2, $a);
                          } else if (target === 69) app44 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn53) {
                          if (exn53 instanceof $__T.Capture) {
                              exn53.stack.push({
                                  kind: "rest",
                                  f: () => funExpr3.apply(this, materializedArguments),
                                  index: 69
                              });
                              captureLocals(exn53.stack[exn53.stack.length - 1]);
                          }

                          throw exn53;
                      }

                      throw app44;
                  }

                  try {
                      if ($__R.mode) {
                          app45 = thisRuntime.box.checkFunction(f);
                      } else if (target === 70) app45 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn54) {
                      if (exn54 instanceof $__T.Capture) {
                          exn54.stack.push({
                              kind: "rest",
                              f: () => funExpr3.apply(this, materializedArguments),
                              index: 70
                          });
                          captureLocals(exn54.stack[exn54.stack.length - 1]);
                      }

                      throw exn54;
                  }

                  try {
                      if ($__R.mode) {
                          app46 = thisRuntime.box.checkArray(arr);
                      } else if (target === 71) app46 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn55) {
                      if (exn55 instanceof $__T.Capture) {
                          exn55.stack.push({
                              kind: "rest",
                              f: () => funExpr3.apply(this, materializedArguments),
                              index: 71
                          });
                          captureLocals(exn55.stack[exn55.stack.length - 1]);
                      }

                      throw exn55;
                  }

                  if ($__R.mode) {
                      currentIndex = -1;
                      length = arr.length;
                      newArray = new Array(length);
                  }

                  loop_break9: while (!$__R.mode && (target === 74 || target === 73) || $__R.mode && currentIndex < length - 1) {
                      loop_continue9: {
                          try {
                              if ($__R.mode) {
                                  app47 = $__R.suspend();
                              } else if (target === 73) app47 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn56) {
                              if (exn56 instanceof $__T.Capture) {
                                  exn56.stack.push({
                                      kind: "rest",
                                      f: () => funExpr3.apply(this, materializedArguments),
                                      index: 73
                                  });
                                  captureLocals(exn56.stack[exn56.stack.length - 1]);
                              }

                              throw exn56;
                          }

                          if ($__R.mode) currentIndex += 1;

                          try {
                              if ($__R.mode) {
                                  res = f.app(arr[currentIndex]);
                              } else if (target === 74) res = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn57) {
                              if (exn57 instanceof $__T.Capture) {
                                  exn57.stack.push({
                                      kind: "rest",
                                      f: () => funExpr3.apply(this, materializedArguments),
                                      index: 74
                                  });
                                  captureLocals(exn57.stack[exn57.stack.length - 1]);
                              }

                              throw exn57;
                          }

                          if ($__R.mode) newArray[currentIndex] = res;
                      }
                  }

                  return newArray;
              };

              raw_array_each = function funExpr4(f, arr) {
                  let app54;
                  let app53;
                  var length;
                  var currentIndex;
                  let app52;
                  let app51;
                  let app50;
                  let app49;
                  var $i05;
                  var $a;
                  let app48;

                  if (!$__R.mode) {
                      [app54, app48, app49, length, $i05, currentIndex, app50, app51, $a, app52, app53] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [app54, app48, app49, length, $i05, currentIndex, app50, app51, $a, app52, app53];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app48 = $__R.suspend();
                      } else if (target === 75) app48 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn58) {
                      if (exn58 instanceof $__T.Capture) {
                          exn58.stack.push({
                              kind: "rest",
                              f: () => funExpr4.apply(this, materializedArguments),
                              index: 75
                          });
                          captureLocals(exn58.stack[exn58.stack.length - 1]);
                      }

                      throw exn58;
                  }

                  if ($__R.mode && arguments.length !== 2 || !$__R.mode && (target === 78 || target === 77 || target === 76)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i05 = 0;
                      }

                      loop_break10: while (!$__R.mode && target === 77 || $__R.mode && $i05 < arguments.length) {
                          loop_continue10: {
                              try {
                                  if ($__R.mode) {
                                      app49 = $__R.suspend();
                                  } else if (target === 77) app49 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn59) {
                                  if (exn59 instanceof $__T.Capture) {
                                      exn59.stack.push({
                                          kind: "rest",
                                          f: () => funExpr4.apply(this, materializedArguments),
                                          index: 77
                                      });
                                      captureLocals(exn59.stack[exn59.stack.length - 1]);
                                  }

                                  throw exn59;
                              }

                              if ($__R.mode) $a[$i05] = arguments[$i05];
                          }

                          if ($__R.mode) $i05++;
                      }

                      try {
                          if ($__R.mode) {
                              app50 = thisRuntime.box.ffi.throwArityErrorC(["raw-array-each"], 2, $a);
                          } else if (target === 78) app50 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn60) {
                          if (exn60 instanceof $__T.Capture) {
                              exn60.stack.push({
                                  kind: "rest",
                                  f: () => funExpr4.apply(this, materializedArguments),
                                  index: 78
                              });
                              captureLocals(exn60.stack[exn60.stack.length - 1]);
                          }

                          throw exn60;
                      }

                      throw app50;
                  }

                  try {
                      if ($__R.mode) {
                          app51 = thisRuntime.box.checkFunction(f);
                      } else if (target === 79) app51 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn61) {
                      if (exn61 instanceof $__T.Capture) {
                          exn61.stack.push({
                              kind: "rest",
                              f: () => funExpr4.apply(this, materializedArguments),
                              index: 79
                          });
                          captureLocals(exn61.stack[exn61.stack.length - 1]);
                      }

                      throw exn61;
                  }

                  try {
                      if ($__R.mode) {
                          app52 = thisRuntime.box.checkArray(arr);
                      } else if (target === 80) app52 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn62) {
                      if (exn62 instanceof $__T.Capture) {
                          exn62.stack.push({
                              kind: "rest",
                              f: () => funExpr4.apply(this, materializedArguments),
                              index: 80
                          });
                          captureLocals(exn62.stack[exn62.stack.length - 1]);
                      }

                      throw exn62;
                  }

                  if ($__R.mode) {
                      currentIndex = -1;
                      length = arr.length;
                  }

                  loop_break11: while (!$__R.mode && (target === 82 || target === 81) || $__R.mode && currentIndex < length - 1) {
                      loop_continue11: {
                          try {
                              if ($__R.mode) {
                                  app53 = $__R.suspend();
                              } else if (target === 81) app53 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn63) {
                              if (exn63 instanceof $__T.Capture) {
                                  exn63.stack.push({
                                      kind: "rest",
                                      f: () => funExpr4.apply(this, materializedArguments),
                                      index: 81
                                  });
                                  captureLocals(exn63.stack[exn63.stack.length - 1]);
                              }

                              throw exn63;
                          }

                          if ($__R.mode) currentIndex += 1;

                          try {
                              if ($__R.mode) {
                                  app54 = f.app(arr[currentIndex]);
                              } else if (target === 82) app54 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn64) {
                              if (exn64 instanceof $__T.Capture) {
                                  exn64.stack.push({
                                      kind: "rest",
                                      f: () => funExpr4.apply(this, materializedArguments),
                                      index: 82
                                  });
                                  captureLocals(exn64.stack[exn64.stack.length - 1]);
                              }

                              throw exn64;
                          }
                      }
                  }

                  return nothing;
              };

              raw_array_mapi = function funExpr5(f, arr) {
                  var res;
                  let app60;
                  var newArray;
                  var length;
                  var currentIndex;
                  let app59;
                  let app58;
                  let app57;
                  let app56;
                  var $i06;
                  var $a;
                  let app55;

                  if (!$__R.mode) {
                      [res, app55, app56, app57, app58, app59, length, $i06, newArray, currentIndex, app60, $a] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [res, app55, app56, app57, app58, app59, length, $i06, newArray, currentIndex, app60, $a];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app55 = $__R.suspend();
                      } else if (target === 83) app55 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn65) {
                      if (exn65 instanceof $__T.Capture) {
                          exn65.stack.push({
                              kind: "rest",
                              f: () => funExpr5.apply(this, materializedArguments),
                              index: 83
                          });
                          captureLocals(exn65.stack[exn65.stack.length - 1]);
                      }

                      throw exn65;
                  }

                  if ($__R.mode && arguments.length !== 2 || !$__R.mode && (target === 86 || target === 85 || target === 84)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i06 = 0;
                      }

                      loop_break12: while (!$__R.mode && target === 85 || $__R.mode && $i06 < arguments.length) {
                          loop_continue12: {
                              try {
                                  if ($__R.mode) {
                                      app56 = $__R.suspend();
                                  } else if (target === 85) app56 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn66) {
                                  if (exn66 instanceof $__T.Capture) {
                                      exn66.stack.push({
                                          kind: "rest",
                                          f: () => funExpr5.apply(this, materializedArguments),
                                          index: 85
                                      });
                                      captureLocals(exn66.stack[exn66.stack.length - 1]);
                                  }

                                  throw exn66;
                              }

                              if ($__R.mode) $a[$i06] = arguments[$i06];
                          }

                          if ($__R.mode) $i06++;
                      }

                      try {
                          if ($__R.mode) {
                              app57 = thisRuntime.box.ffi.throwArityErrorC(["raw-array-mapi"], 2, $a);
                          } else if (target === 86) app57 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn67) {
                          if (exn67 instanceof $__T.Capture) {
                              exn67.stack.push({
                                  kind: "rest",
                                  f: () => funExpr5.apply(this, materializedArguments),
                                  index: 86
                              });
                              captureLocals(exn67.stack[exn67.stack.length - 1]);
                          }

                          throw exn67;
                      }

                      throw app57;
                  }

                  try {
                      if ($__R.mode) {
                          app58 = thisRuntime.box.checkFunction(f);
                      } else if (target === 87) app58 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn68) {
                      if (exn68 instanceof $__T.Capture) {
                          exn68.stack.push({
                              kind: "rest",
                              f: () => funExpr5.apply(this, materializedArguments),
                              index: 87
                          });
                          captureLocals(exn68.stack[exn68.stack.length - 1]);
                      }

                      throw exn68;
                  }

                  try {
                      if ($__R.mode) {
                          app59 = thisRuntime.box.checkArray(arr);
                      } else if (target === 88) app59 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn69) {
                      if (exn69 instanceof $__T.Capture) {
                          exn69.stack.push({
                              kind: "rest",
                              f: () => funExpr5.apply(this, materializedArguments),
                              index: 88
                          });
                          captureLocals(exn69.stack[exn69.stack.length - 1]);
                      }

                      throw exn69;
                  }

                  if ($__R.mode) {
                      currentIndex = -1;
                      length = arr.length;
                      newArray = new Array(length);
                  }

                  loop_break13: while (!$__R.mode && (target === 91 || target === 90) || $__R.mode && currentIndex < length - 1) {
                      loop_continue13: {
                          try {
                              if ($__R.mode) {
                                  app60 = $__R.suspend();
                              } else if (target === 90) app60 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn70) {
                              if (exn70 instanceof $__T.Capture) {
                                  exn70.stack.push({
                                      kind: "rest",
                                      f: () => funExpr5.apply(this, materializedArguments),
                                      index: 90
                                  });
                                  captureLocals(exn70.stack[exn70.stack.length - 1]);
                              }

                              throw exn70;
                          }

                          if ($__R.mode) currentIndex += 1;

                          try {
                              if ($__R.mode) {
                                  res = f.app(arr[currentIndex], currentIndex);
                              } else if (target === 91) res = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn71) {
                              if (exn71 instanceof $__T.Capture) {
                                  exn71.stack.push({
                                      kind: "rest",
                                      f: () => funExpr5.apply(this, materializedArguments),
                                      index: 91
                                  });
                                  captureLocals(exn71.stack[exn71.stack.length - 1]);
                              }

                              throw exn71;
                          }

                          if ($__R.mode) newArray[currentIndex] = res;
                      }
                  }

                  return newArray;
              };

              raw_list_map = function funExpr6(f, lst) {
                  let app70;
                  var res;
                  let app69;
                  let app68;
                  let app67;
                  let app66;
                  var currentFst;
                  var currentLst;
                  var currentAcc;
                  let app65;
                  let app64;
                  let app63;
                  let app62;
                  var $i07;
                  var $a;
                  let app61;

                  if (!$__R.mode) {
                      [res, app65, app66, app67, app68, app69, currentAcc, $i07, currentFst, currentLst, app70, app61, app62, $a, app63, app64] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [res, app65, app66, app67, app68, app69, currentAcc, $i07, currentFst, currentLst, app70, app61, app62, $a, app63, app64];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app61 = $__R.suspend();
                      } else if (target === 92) app61 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn72) {
                      if (exn72 instanceof $__T.Capture) {
                          exn72.stack.push({
                              kind: "rest",
                              f: () => funExpr6.apply(this, materializedArguments),
                              index: 92
                          });
                          captureLocals(exn72.stack[exn72.stack.length - 1]);
                      }

                      throw exn72;
                  }

                  if ($__R.mode && arguments.length !== 2 || !$__R.mode && (target === 95 || target === 94 || target === 93)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i07 = 0;
                      }

                      loop_break14: while (!$__R.mode && target === 94 || $__R.mode && $i07 < arguments.length) {
                          loop_continue14: {
                              try {
                                  if ($__R.mode) {
                                      app62 = $__R.suspend();
                                  } else if (target === 94) app62 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn73) {
                                  if (exn73 instanceof $__T.Capture) {
                                      exn73.stack.push({
                                          kind: "rest",
                                          f: () => funExpr6.apply(this, materializedArguments),
                                          index: 94
                                      });
                                      captureLocals(exn73.stack[exn73.stack.length - 1]);
                                  }

                                  throw exn73;
                              }

                              if ($__R.mode) $a[$i07] = arguments[$i07];
                          }

                          if ($__R.mode) $i07++;
                      }

                      try {
                          if ($__R.mode) {
                              app63 = thisRuntime.box.ffi.throwArityErrorC(["raw-list-map"], 2, $a);
                          } else if (target === 95) app63 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn74) {
                          if (exn74 instanceof $__T.Capture) {
                              exn74.stack.push({
                                  kind: "rest",
                                  f: () => funExpr6.apply(this, materializedArguments),
                                  index: 95
                              });
                              captureLocals(exn74.stack[exn74.stack.length - 1]);
                          }

                          throw exn74;
                      }

                      throw app63;
                  }

                  try {
                      if ($__R.mode) {
                          app64 = thisRuntime.box.checkFunction(f);
                      } else if (target === 96) app64 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn75) {
                      if (exn75 instanceof $__T.Capture) {
                          exn75.stack.push({
                              kind: "rest",
                              f: () => funExpr6.apply(this, materializedArguments),
                              index: 96
                          });
                          captureLocals(exn75.stack[exn75.stack.length - 1]);
                      }

                      throw exn75;
                  }

                  try {
                      if ($__R.mode) {
                          app65 = thisRuntime.box.checkList(lst);
                      } else if (target === 97) app65 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn76) {
                      if (exn76 instanceof $__T.Capture) {
                          exn76.stack.push({
                              kind: "rest",
                              f: () => funExpr6.apply(this, materializedArguments),
                              index: 97
                          });
                          captureLocals(exn76.stack[exn76.stack.length - 1]);
                      }

                      throw exn76;
                  }

                  if ($__R.mode) {
                      currentAcc = [];
                      currentLst = lst;
                  }

                  loop_break15: while (!$__R.mode && (target === 103 || target === 102 || target === 101 || target === 100 || target === 99 || target === 98) || $__R.mode) {
                      try {
                          if ($__R.mode) {
                              app66 = thisRuntime.box.ffi.isLink(currentLst);
                          } else if (target === 98) app66 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn77) {
                          if (exn77 instanceof $__T.Capture) {
                              exn77.stack.push({
                                  kind: "rest",
                                  f: () => funExpr6.apply(this, materializedArguments),
                                  index: 98
                              });
                              captureLocals(exn77.stack[exn77.stack.length - 1]);
                          }

                          throw exn77;
                      }

                      if ($__R.mode && app66 || !$__R.mode && (target === 103 || target === 102 || target === 101 || target === 100 || target === 99)) {
                          loop_continue15: {
                              try {
                                  if ($__R.mode) {
                                      app67 = $__R.suspend();
                                  } else if (target === 99) app67 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn78) {
                                  if (exn78 instanceof $__T.Capture) {
                                      exn78.stack.push({
                                          kind: "rest",
                                          f: () => funExpr6.apply(this, materializedArguments),
                                          index: 99
                                      });
                                      captureLocals(exn78.stack[exn78.stack.length - 1]);
                                  }

                                  throw exn78;
                              }

                              try {
                                  if ($__R.mode) {
                                      app68 = thisRuntime.box.getColonField(currentLst, "first");
                                  } else if (target === 100) app68 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn79) {
                                  if (exn79 instanceof $__T.Capture) {
                                      exn79.stack.push({
                                          kind: "rest",
                                          f: () => funExpr6.apply(this, materializedArguments),
                                          index: 100
                                      });
                                      captureLocals(exn79.stack[exn79.stack.length - 1]);
                                  }

                                  throw exn79;
                              }

                              if ($__R.mode) currentFst = app68;

                              try {
                                  if ($__R.mode) {
                                      app69 = thisRuntime.box.getColonField(currentLst, "rest");
                                  } else if (target === 101) app69 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn80) {
                                  if (exn80 instanceof $__T.Capture) {
                                      exn80.stack.push({
                                          kind: "rest",
                                          f: () => funExpr6.apply(this, materializedArguments),
                                          index: 101
                                      });
                                      captureLocals(exn80.stack[exn80.stack.length - 1]);
                                  }

                                  throw exn80;
                              }

                              if ($__R.mode) currentLst = app69;

                              try {
                                  if ($__R.mode) {
                                      res = f.app(currentFst);
                                  } else if (target === 102) res = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn81) {
                                  if (exn81 instanceof $__T.Capture) {
                                      exn81.stack.push({
                                          kind: "rest",
                                          f: () => funExpr6.apply(this, materializedArguments),
                                          index: 102
                                      });
                                      captureLocals(exn81.stack[exn81.stack.length - 1]);
                                  }

                                  throw exn81;
                              }

                              try {
                                  if ($__R.mode) {
                                      app70 = currentAcc.push(res);
                                  } else if (target === 103) app70 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn82) {
                                  if (exn82 instanceof $__T.Capture) {
                                      exn82.stack.push({
                                          kind: "rest",
                                          f: () => funExpr6.apply(this, materializedArguments),
                                          index: 103
                                      });
                                      captureLocals(exn82.stack[exn82.stack.length - 1]);
                                  }

                                  throw exn82;
                              }
                          }
                      }
                      else if ($__R.mode) break;
                  }

                  return thisRuntime.box.ffi.makeList(currentAcc);
              };

              /**
               * Similar to `raw_array_map`, but applies a specific function to
               * the first item in the array
               */


              raw_array_map1 = function funExpr7(f1, f, arr) {
                  var res;
                  var toCall;
                  let app77;
                  var newArray;
                  var length;
                  var currentIndex;
                  let app76;
                  let app75;
                  let app74;
                  let app73;
                  let app72;
                  var $i08;
                  var $a;
                  let app71;

                  if (!$__R.mode) {
                      [res, app76, app77, length, $i08, newArray, currentIndex, toCall, app71, app72, $a, app73, app74, app75] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [res, app76, app77, length, $i08, newArray, currentIndex, toCall, app71, app72, $a, app73, app74, app75];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app71 = $__R.suspend();
                      } else if (target === 105) app71 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn83) {
                      if (exn83 instanceof $__T.Capture) {
                          exn83.stack.push({
                              kind: "rest",
                              f: () => funExpr7.apply(this, materializedArguments),
                              index: 105
                          });
                          captureLocals(exn83.stack[exn83.stack.length - 1]);
                      }

                      throw exn83;
                  }

                  if ($__R.mode && arguments.length !== 3 || !$__R.mode && (target === 108 || target === 107 || target === 106)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i08 = 0;
                      }

                      loop_break16: while (!$__R.mode && target === 107 || $__R.mode && $i08 < arguments.length) {
                          loop_continue16: {
                              try {
                                  if ($__R.mode) {
                                      app72 = $__R.suspend();
                                  } else if (target === 107) app72 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn84) {
                                  if (exn84 instanceof $__T.Capture) {
                                      exn84.stack.push({
                                          kind: "rest",
                                          f: () => funExpr7.apply(this, materializedArguments),
                                          index: 107
                                      });
                                      captureLocals(exn84.stack[exn84.stack.length - 1]);
                                  }

                                  throw exn84;
                              }

                              if ($__R.mode) $a[$i08] = arguments[$i08];
                          }

                          if ($__R.mode) $i08++;
                      }

                      try {
                          if ($__R.mode) {
                              app73 = thisRuntime.box.ffi.throwArityErrorC(["raw-array-map1"], 3, $a);
                          } else if (target === 108) app73 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn85) {
                          if (exn85 instanceof $__T.Capture) {
                              exn85.stack.push({
                                  kind: "rest",
                                  f: () => funExpr7.apply(this, materializedArguments),
                                  index: 108
                              });
                              captureLocals(exn85.stack[exn85.stack.length - 1]);
                          }

                          throw exn85;
                      }

                      throw app73;
                  }

                  try {
                      if ($__R.mode) {
                          app74 = thisRuntime.box.checkFunction(f1);
                      } else if (target === 109) app74 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn86) {
                      if (exn86 instanceof $__T.Capture) {
                          exn86.stack.push({
                              kind: "rest",
                              f: () => funExpr7.apply(this, materializedArguments),
                              index: 109
                          });
                          captureLocals(exn86.stack[exn86.stack.length - 1]);
                      }

                      throw exn86;
                  }

                  try {
                      if ($__R.mode) {
                          app75 = thisRuntime.box.checkFunction(f);
                      } else if (target === 110) app75 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn87) {
                      if (exn87 instanceof $__T.Capture) {
                          exn87.stack.push({
                              kind: "rest",
                              f: () => funExpr7.apply(this, materializedArguments),
                              index: 110
                          });
                          captureLocals(exn87.stack[exn87.stack.length - 1]);
                      }

                      throw exn87;
                  }

                  try {
                      if ($__R.mode) {
                          app76 = thisRuntime.box.checkArray(arr);
                      } else if (target === 111) app76 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn88) {
                      if (exn88 instanceof $__T.Capture) {
                          exn88.stack.push({
                              kind: "rest",
                              f: () => funExpr7.apply(this, materializedArguments),
                              index: 111
                          });
                          captureLocals(exn88.stack[exn88.stack.length - 1]);
                      }

                      throw exn88;
                  }

                  if ($__R.mode) {
                      currentIndex = -1;
                      length = arr.length;
                      newArray = new Array(length);
                  }

                  loop_break17: while (!$__R.mode && (target === 114 || target === 113) || $__R.mode && currentIndex < length - 1) {
                      loop_continue17: {
                          try {
                              if ($__R.mode) {
                                  app77 = $__R.suspend();
                              } else if (target === 113) app77 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn89) {
                              if (exn89 instanceof $__T.Capture) {
                                  exn89.stack.push({
                                      kind: "rest",
                                      f: () => funExpr7.apply(this, materializedArguments),
                                      index: 113
                                  });
                                  captureLocals(exn89.stack[exn89.stack.length - 1]);
                              }

                              throw exn89;
                          }

                          if ($__R.mode) {
                              currentIndex += 1;
                              toCall = currentIndex === 0 ? f1 : f;
                          }

                          try {
                              if ($__R.mode) {
                                  res = toCall.app(arr[currentIndex]);
                              } else if (target === 114) res = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn90) {
                              if (exn90 instanceof $__T.Capture) {
                                  exn90.stack.push({
                                      kind: "rest",
                                      f: () => funExpr7.apply(this, materializedArguments),
                                      index: 114
                                  });
                                  captureLocals(exn90.stack[exn90.stack.length - 1]);
                              }

                              throw exn90;
                          }

                          if ($__R.mode) newArray[currentIndex] = res;
                      }
                  }

                  return newArray;
              };

              raw_list_filter = function funExpr8(f, lst) {
                  let app87;
                  var res;
                  let app86;
                  let app85;
                  let app84;
                  let app83;
                  var currentFst;
                  var currentLst;
                  var currentAcc;
                  let app82;
                  let app81;
                  let app80;
                  let app79;
                  var $i09;
                  var $a;
                  let app78;

                  if (!$__R.mode) {
                      [res, app87, app78, app79, currentAcc, currentFst, $i09, currentLst, app80, app81, app82, app83, $a, app84, app85, app86] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [res, app87, app78, app79, currentAcc, currentFst, $i09, currentLst, app80, app81, app82, app83, $a, app84, app85, app86];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app78 = $__R.suspend();
                      } else if (target === 115) app78 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn91) {
                      if (exn91 instanceof $__T.Capture) {
                          exn91.stack.push({
                              kind: "rest",
                              f: () => funExpr8.apply(this, materializedArguments),
                              index: 115
                          });
                          captureLocals(exn91.stack[exn91.stack.length - 1]);
                      }

                      throw exn91;
                  }

                  if ($__R.mode && arguments.length !== 2 || !$__R.mode && (target === 118 || target === 117 || target === 116)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i09 = 0;
                      }

                      loop_break18: while (!$__R.mode && target === 117 || $__R.mode && $i09 < arguments.length) {
                          loop_continue18: {
                              try {
                                  if ($__R.mode) {
                                      app79 = $__R.suspend();
                                  } else if (target === 117) app79 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn92) {
                                  if (exn92 instanceof $__T.Capture) {
                                      exn92.stack.push({
                                          kind: "rest",
                                          f: () => funExpr8.apply(this, materializedArguments),
                                          index: 117
                                      });
                                      captureLocals(exn92.stack[exn92.stack.length - 1]);
                                  }

                                  throw exn92;
                              }

                              if ($__R.mode) $a[$i09] = arguments[$i09];
                          }

                          if ($__R.mode) $i09++;
                      }

                      try {
                          if ($__R.mode) {
                              app80 = thisRuntime.box.ffi.throwArityErrorC(["raw-list-filter"], 2, $a);
                          } else if (target === 118) app80 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn93) {
                          if (exn93 instanceof $__T.Capture) {
                              exn93.stack.push({
                                  kind: "rest",
                                  f: () => funExpr8.apply(this, materializedArguments),
                                  index: 118
                              });
                              captureLocals(exn93.stack[exn93.stack.length - 1]);
                          }

                          throw exn93;
                      }

                      throw app80;
                  }

                  try {
                      if ($__R.mode) {
                          app81 = thisRuntime.box.checkFunction(f);
                      } else if (target === 119) app81 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn94) {
                      if (exn94 instanceof $__T.Capture) {
                          exn94.stack.push({
                              kind: "rest",
                              f: () => funExpr8.apply(this, materializedArguments),
                              index: 119
                          });
                          captureLocals(exn94.stack[exn94.stack.length - 1]);
                      }

                      throw exn94;
                  }

                  try {
                      if ($__R.mode) {
                          app82 = thisRuntime.box.checkList(lst);
                      } else if (target === 120) app82 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn95) {
                      if (exn95 instanceof $__T.Capture) {
                          exn95.stack.push({
                              kind: "rest",
                              f: () => funExpr8.apply(this, materializedArguments),
                              index: 120
                          });
                          captureLocals(exn95.stack[exn95.stack.length - 1]);
                      }

                      throw exn95;
                  }

                  if ($__R.mode) {
                      currentAcc = [];
                      currentLst = lst;
                  }

                  loop_break19: while (!$__R.mode && (target === 126 || target === 125 || target === 124 || target === 123 || target === 122 || target === 121) || $__R.mode) {
                      try {
                          if ($__R.mode) {
                              app83 = thisRuntime.box.ffi.isLink(currentLst);
                          } else if (target === 121) app83 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn96) {
                          if (exn96 instanceof $__T.Capture) {
                              exn96.stack.push({
                                  kind: "rest",
                                  f: () => funExpr8.apply(this, materializedArguments),
                                  index: 121
                              });
                              captureLocals(exn96.stack[exn96.stack.length - 1]);
                          }

                          throw exn96;
                      }

                      if ($__R.mode && app83 || !$__R.mode && (target === 126 || target === 125 || target === 124 || target === 123 || target === 122)) {
                          loop_continue19: {
                              try {
                                  if ($__R.mode) {
                                      app84 = $__R.suspend();
                                  } else if (target === 122) app84 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn97) {
                                  if (exn97 instanceof $__T.Capture) {
                                      exn97.stack.push({
                                          kind: "rest",
                                          f: () => funExpr8.apply(this, materializedArguments),
                                          index: 122
                                      });
                                      captureLocals(exn97.stack[exn97.stack.length - 1]);
                                  }

                                  throw exn97;
                              }

                              try {
                                  if ($__R.mode) {
                                      app85 = thisRuntime.box.getColonField(currentLst, "first");
                                  } else if (target === 123) app85 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn98) {
                                  if (exn98 instanceof $__T.Capture) {
                                      exn98.stack.push({
                                          kind: "rest",
                                          f: () => funExpr8.apply(this, materializedArguments),
                                          index: 123
                                      });
                                      captureLocals(exn98.stack[exn98.stack.length - 1]);
                                  }

                                  throw exn98;
                              }

                              if ($__R.mode) currentFst = app85;

                              try {
                                  if ($__R.mode) {
                                      app86 = thisRuntime.box.getColonField(currentLst, "rest");
                                  } else if (target === 124) app86 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn99) {
                                  if (exn99 instanceof $__T.Capture) {
                                      exn99.stack.push({
                                          kind: "rest",
                                          f: () => funExpr8.apply(this, materializedArguments),
                                          index: 124
                                      });
                                      captureLocals(exn99.stack[exn99.stack.length - 1]);
                                  }

                                  throw exn99;
                              }

                              if ($__R.mode) currentLst = app86;

                              try {
                                  if ($__R.mode) {
                                      res = f.app(currentFst);
                                  } else if (target === 125) res = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn100) {
                                  if (exn100 instanceof $__T.Capture) {
                                      exn100.stack.push({
                                          kind: "rest",
                                          f: () => funExpr8.apply(this, materializedArguments),
                                          index: 125
                                      });
                                      captureLocals(exn100.stack[exn100.stack.length - 1]);
                                  }

                                  throw exn100;
                              }

                              if ($__R.mode && res || !$__R.mode && target === 126) {
                                  try {
                                      if ($__R.mode) {
                                          app87 = currentAcc.push(currentFst);
                                      } else if (target === 126) app87 = $__R.stack[$__R.stack.length - 1].f();
                                  } catch (exn101) {
                                      if (exn101 instanceof $__T.Capture) {
                                          exn101.stack.push({
                                              kind: "rest",
                                              f: () => funExpr8.apply(this, materializedArguments),
                                              index: 126
                                          });
                                          captureLocals(exn101.stack[exn101.stack.length - 1]);
                                      }

                                      throw exn101;
                                  }
                              }
                          }
                      }
                      else if ($__R.mode) break;
                  }

                  return thisRuntime.box.ffi.makeList(currentAcc);
              };

              raw_array_filter = function funExpr9(f, arr) {
                  let app96;
                  let app95;
                  let app94;
                  var res;
                  let app93;
                  var newArray;
                  var length;
                  var currentIndex;
                  let app92;
                  let app91;
                  let app90;
                  let app89;
                  var $i010;
                  var $a;
                  let app88;

                  if (!$__R.mode) {
                      [res, app88, app89, length, $i010, app90, newArray, app91, currentIndex, app92, app93, app94, $a, app95, app96] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [res, app88, app89, length, $i010, app90, newArray, app91, currentIndex, app92, app93, app94, $a, app95, app96];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app88 = $__R.suspend();
                      } else if (target === 128) app88 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn102) {
                      if (exn102 instanceof $__T.Capture) {
                          exn102.stack.push({
                              kind: "rest",
                              f: () => funExpr9.apply(this, materializedArguments),
                              index: 128
                          });
                          captureLocals(exn102.stack[exn102.stack.length - 1]);
                      }

                      throw exn102;
                  }

                  if ($__R.mode && arguments.length !== 2 || !$__R.mode && (target === 131 || target === 130 || target === 129)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i010 = 0;
                      }

                      loop_break20: while (!$__R.mode && target === 130 || $__R.mode && $i010 < arguments.length) {
                          loop_continue20: {
                              try {
                                  if ($__R.mode) {
                                      app89 = $__R.suspend();
                                  } else if (target === 130) app89 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn103) {
                                  if (exn103 instanceof $__T.Capture) {
                                      exn103.stack.push({
                                          kind: "rest",
                                          f: () => funExpr9.apply(this, materializedArguments),
                                          index: 130
                                      });
                                      captureLocals(exn103.stack[exn103.stack.length - 1]);
                                  }

                                  throw exn103;
                              }

                              if ($__R.mode) $a[$i010] = arguments[$i010];
                          }

                          if ($__R.mode) $i010++;
                      }

                      try {
                          if ($__R.mode) {
                              app90 = thisRuntime.box.ffi.throwArityErrorC(["raw-array-filter"], 2, $a);
                          } else if (target === 131) app90 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn104) {
                          if (exn104 instanceof $__T.Capture) {
                              exn104.stack.push({
                                  kind: "rest",
                                  f: () => funExpr9.apply(this, materializedArguments),
                                  index: 131
                              });
                              captureLocals(exn104.stack[exn104.stack.length - 1]);
                          }

                          throw exn104;
                      }

                      throw app90;
                  }

                  try {
                      if ($__R.mode) {
                          app91 = thisRuntime.box.checkFunction(f);
                      } else if (target === 132) app91 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn105) {
                      if (exn105 instanceof $__T.Capture) {
                          exn105.stack.push({
                              kind: "rest",
                              f: () => funExpr9.apply(this, materializedArguments),
                              index: 132
                          });
                          captureLocals(exn105.stack[exn105.stack.length - 1]);
                      }

                      throw exn105;
                  }

                  try {
                      if ($__R.mode) {
                          app92 = thisRuntime.box.checkArray(arr);
                      } else if (target === 133) app92 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn106) {
                      if (exn106 instanceof $__T.Capture) {
                          exn106.stack.push({
                              kind: "rest",
                              f: () => funExpr9.apply(this, materializedArguments),
                              index: 133
                          });
                          captureLocals(exn106.stack[exn106.stack.length - 1]);
                      }

                      throw exn106;
                  }

                  if ($__R.mode) {
                      currentIndex = -1;
                      length = arr.length;
                      newArray = new Array();
                  }

                  loop_break21: while (!$__R.mode && (target === 140 || target === 139 || target === 137 || target === 136 || target === 135) || $__R.mode && currentIndex < length - 1) {
                      loop_continue21: {
                          try {
                              if ($__R.mode) {
                                  app93 = $__R.suspend();
                              } else if (target === 135) app93 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn107) {
                              if (exn107 instanceof $__T.Capture) {
                                  exn107.stack.push({
                                      kind: "rest",
                                      f: () => funExpr9.apply(this, materializedArguments),
                                      index: 135
                                  });
                                  captureLocals(exn107.stack[exn107.stack.length - 1]);
                              }

                              throw exn107;
                          }

                          if ($__R.mode) currentIndex += 1;

                          try {
                              if ($__R.mode) {
                                  res = f.app(arr[currentIndex]);
                              } else if (target === 136) res = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn108) {
                              if (exn108 instanceof $__T.Capture) {
                                  exn108.stack.push({
                                      kind: "rest",
                                      f: () => funExpr9.apply(this, materializedArguments),
                                      index: 136
                                  });
                                  captureLocals(exn108.stack[exn108.stack.length - 1]);
                              }

                              throw exn108;
                          }

                          try {
                              if ($__R.mode) {
                                  app94 = isBoolean(res);
                              } else if (target === 137) app94 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn109) {
                              if (exn109 instanceof $__T.Capture) {
                                  exn109.stack.push({
                                      kind: "rest",
                                      f: () => funExpr9.apply(this, materializedArguments),
                                      index: 137
                                  });
                                  captureLocals(exn109.stack[exn109.stack.length - 1]);
                              }

                              throw exn109;
                          }

                          if ($__R.mode && !app94) {
                              return ffi.throwNonBooleanCondition(["raw-array-filter"], "Boolean", res);
                          }

                          try {
                              if ($__R.mode) {
                                  app95 = isPyretTrue(res);
                              } else if (target === 139) app95 = $__R.stack[$__R.stack.length - 1].f();
                          } catch (exn110) {
                              if (exn110 instanceof $__T.Capture) {
                                  exn110.stack.push({
                                      kind: "rest",
                                      f: () => funExpr9.apply(this, materializedArguments),
                                      index: 139
                                  });
                                  captureLocals(exn110.stack[exn110.stack.length - 1]);
                              }

                              throw exn110;
                          }

                          if ($__R.mode && app95 || !$__R.mode && target === 140) {
                              try {
                                  if ($__R.mode) {
                                      app96 = newArray.push(arr[currentIndex]);
                                  } else if (target === 140) app96 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn111) {
                                  if (exn111 instanceof $__T.Capture) {
                                      exn111.stack.push({
                                          kind: "rest",
                                          f: () => funExpr9.apply(this, materializedArguments),
                                          index: 140
                                      });
                                      captureLocals(exn111.stack[exn111.stack.length - 1]);
                                  }

                                  throw exn111;
                              }
                          }
                      }
                  }

                  return newArray;
              };

              raw_list_fold = function funExpr10(f, init, lst) {
                  let app106;
                  let app105;
                  var fst;
                  let app104;
                  let app103;
                  var currentLst;
                  var currentAcc;
                  let app102;
                  let app101;
                  let app100;
                  let app99;
                  let app98;
                  var $i011;
                  var $a;
                  let app97;

                  if (!$__R.mode) {
                      [app98, app99, fst, currentAcc, $i011, app100, app101, app102, app103, app104, currentLst, app105, app106, $a, app97] = $__R.stack[$__R.stack.length - 1].locals;
                      target = $__R.stack[$__R.stack.length - 1].index;
                      $__R.stack.pop();
                  }

                  function captureLocals(frame) {
                      frame.locals = [app98, app99, fst, currentAcc, $i011, app100, app101, app102, app103, app104, currentLst, app105, app106, $a, app97];
                  }

                  const materializedArguments = arguments;

                  try {
                      if ($__R.mode) {
                          app97 = $__R.suspend();
                      } else if (target === 141) app97 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn112) {
                      if (exn112 instanceof $__T.Capture) {
                          exn112.stack.push({
                              kind: "rest",
                              f: () => funExpr10.apply(this, materializedArguments),
                              index: 141
                          });
                          captureLocals(exn112.stack[exn112.stack.length - 1]);
                      }

                      throw exn112;
                  }

                  if ($__R.mode && arguments.length !== 3 || !$__R.mode && (target === 144 || target === 143 || target === 142)) {
                      if ($__R.mode) {
                          $a = new Array(arguments.length);
                          $i011 = 0;
                      }

                      loop_break22: while (!$__R.mode && target === 143 || $__R.mode && $i011 < arguments.length) {
                          loop_continue22: {
                              try {
                                  if ($__R.mode) {
                                      app98 = $__R.suspend();
                                  } else if (target === 143) app98 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn113) {
                                  if (exn113 instanceof $__T.Capture) {
                                      exn113.stack.push({
                                          kind: "rest",
                                          f: () => funExpr10.apply(this, materializedArguments),
                                          index: 143
                                      });
                                      captureLocals(exn113.stack[exn113.stack.length - 1]);
                                  }

                                  throw exn113;
                              }

                              if ($__R.mode) $a[$i011] = arguments[$i011];
                          }

                          if ($__R.mode) $i011++;
                      }

                      try {
                          if ($__R.mode) {
                              app99 = thisRuntime.box.ffi.throwArityErrorC(["raw-list-fold"], 3, $a);
                          } else if (target === 144) app99 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn114) {
                          if (exn114 instanceof $__T.Capture) {
                              exn114.stack.push({
                                  kind: "rest",
                                  f: () => funExpr10.apply(this, materializedArguments),
                                  index: 144
                              });
                              captureLocals(exn114.stack[exn114.stack.length - 1]);
                          }

                          throw exn114;
                      }

                      throw app99;
                  }

                  try {
                      if ($__R.mode) {
                          app100 = thisRuntime.box.checkFunction(f);
                      } else if (target === 145) app100 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn115) {
                      if (exn115 instanceof $__T.Capture) {
                          exn115.stack.push({
                              kind: "rest",
                              f: () => funExpr10.apply(this, materializedArguments),
                              index: 145
                          });
                          captureLocals(exn115.stack[exn115.stack.length - 1]);
                      }

                      throw exn115;
                  }

                  try {
                      if ($__R.mode) {
                          app101 = thisRuntime.box.checkPyretVal(init);
                      } else if (target === 146) app101 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn116) {
                      if (exn116 instanceof $__T.Capture) {
                          exn116.stack.push({
                              kind: "rest",
                              f: () => funExpr10.apply(this, materializedArguments),
                              index: 146
                          });
                          captureLocals(exn116.stack[exn116.stack.length - 1]);
                      }

                      throw exn116;
                  }

                  try {
                      if ($__R.mode) {
                          app102 = thisRuntime.box.checkList(lst);
                      } else if (target === 147) app102 = $__R.stack[$__R.stack.length - 1].f();
                  } catch (exn117) {
                      if (exn117 instanceof $__T.Capture) {
                          exn117.stack.push({
                              kind: "rest",
                              f: () => funExpr10.apply(this, materializedArguments),
                              index: 147
                          });
                          captureLocals(exn117.stack[exn117.stack.length - 1]);
                      }

                      throw exn117;
                  }

                  if ($__R.mode) {
                      currentAcc = init;
                      currentLst = lst;
                  }

                  loop_break23: while (!$__R.mode && (target === 152 || target === 151 || target === 150 || target === 149 || target === 148) || $__R.mode) {
                      try {
                          if ($__R.mode) {
                              app103 = thisRuntime.box.ffi.isLink(currentLst);
                          } else if (target === 148) app103 = $__R.stack[$__R.stack.length - 1].f();
                      } catch (exn118) {
                          if (exn118 instanceof $__T.Capture) {
                              exn118.stack.push({
                                  kind: "rest",
                                  f: () => funExpr10.apply(this, materializedArguments),
                                  index: 148
                              });
                              captureLocals(exn118.stack[exn118.stack.length - 1]);
                          }

                          throw exn118;
                      }

                      if ($__R.mode && app103 || !$__R.mode && (target === 152 || target === 151 || target === 150 || target === 149)) {
                          loop_continue23: {
                              try {
                                  if ($__R.mode) {
                                      app104 = $__R.suspend();
                                  } else if (target === 149) app104 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn119) {
                                  if (exn119 instanceof $__T.Capture) {
                                      exn119.stack.push({
                                          kind: "rest",
                                          f: () => funExpr10.apply(this, materializedArguments),
                                          index: 149
                                      });
                                      captureLocals(exn119.stack[exn119.stack.length - 1]);
                                  }

                                  throw exn119;
                              }

                              try {
                                  if ($__R.mode) {
                                      fst = thisRuntime.box.getColonField(currentLst, "first");
                                  } else if (target === 150) fst = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn120) {
                                  if (exn120 instanceof $__T.Capture) {
                                      exn120.stack.push({
                                          kind: "rest",
                                          f: () => funExpr10.apply(this, materializedArguments),
                                          index: 150
                                      });
                                      captureLocals(exn120.stack[exn120.stack.length - 1]);
                                  }

                                  throw exn120;
                              }

                              try {
                                  if ($__R.mode) {
                                      app105 = thisRuntime.box.getColonField(currentLst, "rest");
                                  } else if (target === 151) app105 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn121) {
                                  if (exn121 instanceof $__T.Capture) {
                                      exn121.stack.push({
                                          kind: "rest",
                                          f: () => funExpr10.apply(this, materializedArguments),
                                          index: 151
                                      });
                                      captureLocals(exn121.stack[exn121.stack.length - 1]);
                                  }

                                  throw exn121;
                              }

                              if ($__R.mode) currentLst = app105;

                              try {
                                  if ($__R.mode) {
                                      app106 = f.app(currentAcc, fst);
                                  } else if (target === 152) app106 = $__R.stack[$__R.stack.length - 1].f();
                              } catch (exn122) {
                                  if (exn122 instanceof $__T.Capture) {
                                      exn122.stack.push({
                                          kind: "rest",
                                          f: () => funExpr10.apply(this, materializedArguments),
                                          index: 152
                                      });
                                      captureLocals(exn122.stack[exn122.stack.length - 1]);
                                  }

                                  throw exn122;
                              }

                              if ($__R.mode) currentAcc = app106;
                          }
                      }
                      else if ($__R.mode) break;
                  }

                  return currentAcc;
              };

              // Export the stopified functions here.


              return {
                  /* Run functions */
                  "safeCall": safeCall.box,
                  "eachLoop": eachLoop.box,
                  "run": run.box,
                  "runThunk": runThunk.box,
                  "execThunk": execThunk.box,

                  /* Array functions */
                  "raw_array_build": raw_array_build,
                  "raw_array_build_opt": raw_array_build_opt,
                  "raw_array_fold": raw_array_fold,
                  "raw_array_map": raw_array_map,
                  "raw_array_each": raw_array_each,
                  "raw_array_mapi": raw_array_mapi,
                  "raw_array_map1": raw_array_map1,
                  "raw_array_filter": raw_array_filter,

                  /* List functions */
                  "raw_list_map": raw_list_map,
                  "raw_list_filter": raw_list_filter,
                  "raw_list_fold": raw_list_fold
              };
          }
      }
  })
