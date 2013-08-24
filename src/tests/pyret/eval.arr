#lang pyret

import pyret-eval as E
import ast as ast

fun eval(str, env, settings):
  stx = ast.parse(str, "simple-parse", { ["check"]: false })
  E.eval(ast.to-native(stx.pre-desugar), env, settings)
end

check:
  eval("5", {}, {}) is 5
  eval("\"a string\"", {}, {}) is "a string"
  eval("true", {}, {}) is true

  eval("5 + 3", {}, {}) is 8

  checkers.check-exn("Unbound id in eval",
    fun: eval("x", {}, {}) end,
    fun(e): e.message.contains("during parsing or typechecking") end)

end
