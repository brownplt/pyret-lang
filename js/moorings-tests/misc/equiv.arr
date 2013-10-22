#lang pyret
  eq = checkers.check-equals
  test-print(eq("empty objects", builtins.equiv({}, {}), true))
  test-print(eq("",builtins.equiv({x : 5}, {y : 6}), false))
  test-print(eq("",builtins.equiv({x : 5}, {x : 6}), false))
  test-print(eq("",builtins.equiv({x : 5}, {x : 5}), true))
  test-print(eq("",builtins.equiv({x : 5, y : 6}, {y : 6, x : 5}), true))
  test-print(eq("",builtins.equiv({x : {z: "foo"}, y : 6}, {y : 6, x : {z: "foo"}}), true))
  test-print(eq("",builtins.equiv({x : {z: "foo"}, y : [true, 6]}, {y : [true, 6], x : {z: "foo"}}), true))
  test-print(eq("",builtins.equiv(fun: end, fun: end), false))

  f = fun: end
  test-print(eq("functions in objects aren't ever equal", builtins.equiv({my_fun:f}, {my_fun:f}), false))
  m = method(self): end
  test-print(eq("methods in objects aren't ever equal", builtins.equiv({my_meth:m}, {my_meth:m}), false))

  test-print(eq("lists of objects", builtins.equiv([{}], [{}]), true))
  test-print(eq("lists of prims", builtins.equiv([5], [5]), true))
