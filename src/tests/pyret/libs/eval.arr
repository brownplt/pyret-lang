#lang pyret

import pyret-eval as E
import ast as ast
import namespaces as N

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
    fun(e): e.message.contains("Unbound identifier: x") end)

  y = 10
  checkers.check-exn("Unbound id in eval, avoiding enclosing scope",
    fun: eval("y", {}, {}) end,
    fun(e): e.message.contains("Unbound identifier: y") end)

  eval("z", {z: 5}, {}) is 5 

  # Builtins provided by the runtime are present
  eval("prim-has-field", {}, {})({x:5}, "x") is true

  # But builtins defined in libraries (like list) are not
  checkers.check-exn("Unbound id in eval, even on library built-in",
    fun: eval("list", {}, {}) end,
    fun(e): e.message.contains("Unbound identifier: list") end)

  # annotations work across eval for builtins
  eval("fun(x :: Number): x + 1 end", {}, {})(10) is 11

  # we cannot mess with true built-ins via shadowing (should this be an error?)
  eval("Number", {Number: fun(x): 22 end}, {})(10) is true

  # but we can mess with library-provided built-ins
  eval("[]", {list: {empty: 22}}, {}) is 22

  # Lists created in eval when passed appropriate values are real lists
  eval("[1,2,3]", {list: list}, {}) is [1,2,3]

  checkers.check-exn("Provide not allowed",
    fun: eval("provide 5 end", {}, {}) end,
    fun(e): e.message.contains("Import and provide not allowed in eval") end)
  checkers.check-exn("Import not allowed",
    fun: eval("import 'foo.arr' as foo", {}, {}) end,
    fun(e): e.message.contains("Import and provide not allowed in eval") end)

  check-results = eval("check: 5 is 5 end", {
      checkers: checkers,
      list: list,
      error: error
    }, {check-mode: true})
  results = check-results.results
  results.length() is 1
  block-results = results.first
  block-results.length() is 1
  checkers.is-normal-result(block-results.first) is true
  checkers.is-success(block-results.first.results.first) is true

  eval("try: raise(43) except(e): 22 end", N.pyret-env, {}) is 22
end

