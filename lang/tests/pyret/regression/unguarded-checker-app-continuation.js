({
  requires: [
    { "import-type": "builtin", name: "checker" },
    { "import-type": "builtin", name: "load-lib" }
  ],
  // The *real* post-load-hooks module -- the same code load-lib.js:345 loads.
  // Its "builtin://checker" hook contains one of the two unguarded calls under
  // test; the other is inline in load-lib.js's runProgram, exercised below via
  // the real run-program.
  nativeRequires: ["pyret-base/js/post-load-hooks"],
  provides: {
    values: {
      "checker-hook-stores-continuation-under-low-gas": "tany",
      "load-lib-run-program-stores-continuation-under-low-gas": "tany"
    }
  },
  theModule: function(runtime, namespace, uri, checkerVals, loadLibVals, loadHooksLib) {
    // Both load-lib.js:340 (inline in runProgram) and post-load-hooks.js:140
    // (the "builtin://checker" hook) do, with no run-loop / safeCall wrapper
    // around the .app():
    //
    //   var currentChecker = getField(checker, "make-check-context")
    //                          .app(makeString(main), checks);
    //   setParam("current-checker", currentChecker);
    //
    // make-check-context is a compiled Pyret function, so its body checks GAS
    // on entry (runtime.js safeCall): if GAS is already spent, .app() returns
    // a *continuation object* instead of the check-context, expecting an
    // enclosing trampoline to catch and retry it. Because these call sites
    // have no such wrapper, the continuation escapes and gets stored in the
    // "current-checker" param -- and the run detonates later when something
    // pulls "current-checker" back out expecting a check-context.
    //
    // Each probe below forces GAS to 1 (make-check-context is entered with no
    // GAS left) and reports whether a continuation ended up stored in
    // "current-checker". Both should report `false` (a real check-context was
    // stored); they report `true` while the call sites remain unguarded.

    // A required builtin is handed to us as its provide-plus-types object, but
    // both call sites start from the raw module object and dig out
    // .provide-plus-types themselves, so wrap it back up the way the module
    // system would have.
    function rawCheckerModule() {
      return runtime.makeObject({ "provide-plus-types": checkerVals });
    }

    // Save/restore runtime params that the probes perturb. getParam throws on
    // unset params, so remember whether each was present.
    function saveParams(names) {
      return names.map(function(n) {
        return { name: n, had: runtime.hasParam(n), val: runtime.hasParam(n) ? runtime.getParam(n) : undefined };
      });
    }
    function restoreParams(saved) {
      saved.forEach(function(s) { if (s.had) { runtime.setParam(s.name, s.val); } });
    }

    // ---- post-load-hooks.js:140, driving the real hook ------------------
    function probeCheckerHookUnderLowGas() {
      var hooks = loadHooksLib.makeDefaultPostLoadHooks(runtime, {
        main: "unguarded-checker-app-continuation-regression",
        checks: "all"
      });
      var raw = rawCheckerModule();
      var saved = saveParams(["current-checker"]);
      var savedGas = runtime.GAS;
      try {
        runtime.GAS = 1;
        hooks["builtin://checker"](raw); // real post-load-hooks.js:140 runs here
        return runtime.makeBoolean(runtime.isContinuation(runtime.getParam("current-checker")));
      } finally {
        runtime.GAS = savedGas;
        restoreParams(saved);
      }
    }

    // ---- load-lib.js:340, driving the real run-program ------------------
    // code.pyret.org runs user programs on a runtime it shares with load-lib,
    // so make-check-context ends up being called (unguarded) on a runtime
    // that's already mid-execution -- which is how the original bug bit. We
    // reproduce that here: hand load-lib the *current* runtime as its guest
    // runtime, hand it a realm that already has the checker instantiated (so
    // runProgram takes the line 336-342 branch), force GAS to 1, and call the
    // real run-program.
    //
    // run-program is called directly (not through the compiled trampoline) so
    // GAS stays at 1 the whole way into line 340 -- getField is GAS-neutral, so
    // nothing between the entry and the offending .app() resets it. pauseStack
    // is stubbed for the duration so run-program stops right after its checker
    // setup (line 341) instead of pausing and running a whole guest program;
    // the bug has fully manifested by then (current-checker is set).
    function probeLoadLibRunProgramUnderLowGas() {
      var runProgram = runtime.getField(runtime.getField(loadLibVals, "values"), "run-program");

      // Force the current runtime to be load-lib's guest runtime. runProgram
      // does `getField(otherRuntimeObj, "runtime").val`, with no brand check.
      var sharedRuntimeObj = runtime.makeObject({
        "runtime": runtime.makeOpaque(runtime)
      });
      var realmObj = runtime.makeObject({
        "realm": runtime.makeOpaque({
          instantiated: { "builtin://checker": rawCheckerModule() },
          static: {}
        })
      });
      var options = runtime.makeObject({ "checks": runtime.makeString("all") });
      var programString = '{staticModules: {}, depMap: {}, toLoad: ["test://main"], uris: {}}';
      var cmdArgs = runtime.ffi.makeList([]);

      var saved = saveParams(["current-checker", "currentMainURL", "command-line-arguments"]);
      var savedGas = runtime.GAS;
      var savedPause = runtime.pauseStack;
      try {
        runtime.pauseStack = function(_resumer) { return runtime.nothing; };
        runtime.GAS = 1;
        runProgram.app(sharedRuntimeObj, realmObj, programString, options, cmdArgs);
        return runtime.makeBoolean(runtime.isContinuation(runtime.getParam("current-checker")));
      } finally {
        runtime.pauseStack = savedPause;
        runtime.GAS = savedGas;
        restoreParams(saved);
      }
    }

    return runtime.makeModuleReturn({
      "checker-hook-stores-continuation-under-low-gas":
        runtime.makeFunction(probeCheckerHookUnderLowGas,
                             "checker-hook-stores-continuation-under-low-gas"),
      "load-lib-run-program-stores-continuation-under-low-gas":
        runtime.makeFunction(probeLoadLibRunProgramUnderLowGas,
                             "load-lib-run-program-stores-continuation-under-low-gas")
    }, {});
  }
})
