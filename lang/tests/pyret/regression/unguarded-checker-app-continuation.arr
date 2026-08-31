import js-file("./unguarded-checker-app-continuation") as U

# Regression for the unguarded `.app()` in load-lib.js (runProgram, ~line 340)
# and its identical twin in post-load-hooks.js (~line 140):
#
#   var currentChecker = getField(checker, "make-check-context")
#                          .app(makeString(main), checks);
#   setParam("current-checker", currentChecker);
#
# make-check-context is a compiled Pyret function whose body checks GAS on
# entry. Because the `.app()` has no surrounding run-loop / safeCall wrapper,
# if GAS happens to be spent at that moment the call returns a *continuation*
# instead of the check-context, and that continuation gets stored in the
# "current-checker" param -- corrupting every check that follows.
#
# The companion JS drives each call site with GAS forced to 1 and reports
# whether a continuation was stored. These assert the fixed behavior (no
# continuation), so they fail while the call sites are unguarded.
check "post-load-hooks.js: make-check-context .app() must not store a continuation under low GAS":
  # Drives the real post-load-hooks.js "builtin://checker" hook.
  U.checker-hook-stores-continuation-under-low-gas() is false
end

check "load-lib.js: make-check-context .app() must not store a continuation under low GAS":
  # Drives the real load-lib run-program on a runtime shared with load-lib
  # (the code.pyret.org scenario), reaching runProgram's inline checker setup.
  U.load-lib-run-program-stores-continuation-under-low-gas() is false
end
