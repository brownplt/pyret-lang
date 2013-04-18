#lang pyret

import "test.arr" as T
provide { run-tests: run-tests } end

fun run-tests():

  T.check-equals("constant function",
    \(fun f(): 2 end f()),
    2)

  T.check-equals("identity function on a number",
    \(fun f(x): x end f(2)), 2)

  T.check-equals("shadowing",
    \(fun f(x): x end fun g(x): x end f(2) g(10) f(2)),
    2)

  T.check-equals("shadowing2",
    \(fun f(x): fun g(x): x end g(x) end f(5)),
    5)

  T.check-not-equals("constant function",
    \(fun f(x): x end f(3)),
    2)

end

