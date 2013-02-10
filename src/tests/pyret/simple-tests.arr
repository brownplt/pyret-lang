#lang pyret

import "unittest.arr" as Test
provide { run-tests: run-tests } end

fun run-tests():
  Test.check-equals("constant function",
                    \(fun f(): 2 end f()),
                    2)
  Test.check-equals("identity on numbers",
                    \(fun f(x): x end f(2)), 2)
  Test.check-equals("shadowing",
                    \(fun f(x): x end fun g(x): x end f(2) g(10) f(2)),
                    2)
  Test.check-equals("identity on numbers",
                    \(fun f(x): x.hie end f(2)), 2)
end

