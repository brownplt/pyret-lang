#lang pyret

import test as T
provide { run-tests: run-tests } end

fun run-tests():

  T.check-equals("constant function",
    fun: fun f(): 2 end f() end,
    2)

  T.check-equals("identity function on a number",
    fun: fun f(x): x end f(2) end, 2)

  T.check-equals("shadowing",
    fun: fun f(x): x end fun g(x): x end f(2) g(10) f(2) end,
    2)

  T.check-equals("shadowing2",
    fun: fun f(x): fun g(x): x end g(x) end f(5) end,
    5)

  T.check-not-equals("constant function",
    fun: fun f(x): x end f(3) end,
    2)

end

