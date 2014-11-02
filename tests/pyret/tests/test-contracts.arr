#lang pyret

import exec as X
import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS

exec-result = lam(result):
  str = result.code.pyret-to-js-runnable()
  X.exec(str, "test", ".", true, "Pyret", [list:])
end
compile-str = lam(str):
  CM.compile-js(
          CM.start,
          "Pyret",
          str,
          "test",
          CS.standard-builtins,
          {
            check-mode : true,
            allow-shadowed : false,
            collect-all: false,
            type-check: false,
            ignore-unbound: false
          }
          ).result
end
run-str = lam(str):
  compiled = compile-str(str)
  cases(CS.CompileResult) compiled:
    | ok(code) => exec-result(compiled)
    | err(errs) => raise("Compilation failure when a run was expected " + torepr(errs) + "\n Program was:\n " + str)
  end
end

fun is-contract-error-str(result):
  (result.success == false) and string-contains(result.render-error-message(), "Contract Error")
end

fun is-refinement-error-str(result):
  (result.success == false) and string-contains(result.render-error-message(), "Predicate")
end

fun is-field-error-str(msg, fields):
  for lists.all(f from fields):
    string-contains(msg, "field " + f)
  end
end

fun is-unbound-contract(result):
  unbound-contracts = result.problems.filter(CS.is-unbound-type-id)
  unbound-contracts.length() > 0
end

check "should work for flat contracts":
  contract-errors = [list:
    "x :: String = 5",
    "x :: (String -> Number) = 5",
    "x :: Number = {x: 'not-a-num'}",
    "x :: Boolean = 'foo'",
    "x :: Boolean = 5"
  ]
  for each(program from contract-errors):
    result = run-str(program)
    result.success is false
    when result.success == true:
      "Should be error" is program
    end
    when result.success == false:
      result satisfies is-contract-error-str
    end
  end
end

is-sorted = 
  "fun is-sorted(l):\n" +
  "  cases(List) l:\n" +
  "    | empty => true\n" +
  "    | link(f, r) =>\n" +
  "      cases(List) r:\n" +
  "        | empty => true\n" +
  "        | link(f2, r2) =>\n" +
  "          (f <= f2) and is-sorted(r)\n" +
  "      end\n" +
  "  end\n" +
  "end\n"

check "should work for refinements":
  contract-errors = [list:
    "is-odd = lam(n): num-modulo(n, 2) == 1 end\nx :: String%(is-odd) = 5",
    "is-odd = lam(n): num-modulo(n, 2) == 1 end\nx :: Number%(is-odd) = 6",
    "is-zero-length = lam(s): string-length(s) == 0 end\ns :: String%(is-zero-length) = 'foo'"
  ]
  for each(program from contract-errors):
    result = run-str(program)
    result.success is false
    when result.success == true:
      "Should be error" is program
    end
    when result.success == false:
      result satisfies is-contract-error-str
    end
  end
  non-errors = [list:
    "is-odd = lam(n): num-modulo(n, 2) == 1 end\nx :: Number%(is-odd) = 5",
    "is-zero-length = lam(s): string-length(s) == 0 end\ns :: String%(is-zero-length) = ''",
    is-sorted + "l :: Any%(is-sorted) = lists.range(0, 100)"
  ]
  for each(program from non-errors):
    result = run-str(program)
    result.success is true
    when result.success == false:
      "Should succeed" is program
    end
  end
end

check "should work for records":
  contract-errors = [list:
    { p: "x :: { x :: Number } = { x : 'foo' }", f: [list: "x"] },
    { p: "x :: { r :: { y :: String } } = { r: { y: 5 } }", f: [list: "y", "r"] },
    { p: "x :: { r :: Number } = { r: { y: 5 } }", f: [list: "r"] },
    { p: "type-let R = { x :: Number }: x :: R = { r: lam(v): v end }\n x end",
      f: [list: "x"] },
    { p: "x :: { y :: Number } = {}", f: [list: "y"] },
    { p: "x :: { x :: Number, y :: Number } = { x: 'foo' }", f: [list: "y"] },
    { p: is-sorted + "o :: { l1 :: Any%(is-sorted), l2 :: Number } = { l1: lists.range(0, 100), l2: 'foo'}", f: [list: "l2"] }
  ]
  for each(program from contract-errors):
    result = run-str(program.p)
    result.success is false
    when result.success == true:
      "Should be error" is program.p
    end
    when result.success == false:
      msg = result.render-error-message()
      msg satisfies is-field-error-str(_, program.f)
      when not(is-field-error-str(msg, program.f)):
        print("Failed for program " + program.p + "\n" + torepr(program.f) + " not present in " + msg)
      end
    end
  end

  non-errors = [list:
    is-sorted + "o :: { l1 :: Any%(is-sorted), l2 :: Any%(is-sorted) } = { l1: lists.range(0, 100), l2: lists.range(0, 100) }",
    "x :: {} = { x : 'foo' }"
  ]
  for each(program from non-errors):
    result = run-str(program)
    result.success is true
    when result.success == false:
      "Should succeed" is program
    end
  end
end


check "should notice unbound contracts":
  contract-errors = [list:
    "x :: NotAType = 5",
    "x :: (Number -> Fail) = 5",
    "x :: (Number -> { x:: Fail }) = 5",
    "x :: Numba % (is-even) = 5",
    "x :: lisst.List = 10",
    "y = 5\nx :: y = 5"
  ]
  for each(program from contract-errors):
    result = compile-str(program)
    CS.is-err(result) is true
    when CS.is-ok(result):
      "Should be error" is program
    end
    when CS.is-err(result) == true:
      result satisfies is-unbound-contract
    end
  end
end

check "should bind types":
  contract-errors = [list:
    "type-let N = Number: x :: N = 'foo'\n x end",
    "type-let S = String, N = Number: x :: (S -> N) = 'foo'\n x end"
  ]
  for each(program from contract-errors):
    result = run-str(program)
    result.success is false
    when result.success == true:
      "Should be error" is program
    end
    when result.success == false:
      result satisfies is-contract-error-str
    end
  end
end

check "should work for lambda-bound annotations":
  run-str("fun f(x :: Number): x end\n f('foo')") satisfies is-contract-error-str
  run-str("fun id(x): x end\n fun f(x) -> Number: id(x) end\n f('foo')") satisfies is-contract-error-str
  run-str("fun f(x) -> Number: if true: x else: x end end\n f('foo')") satisfies is-contract-error-str
  run-str("fun f(x) -> Number: cases(List) empty: | empty => x | link(_, _) => x end end\n f('foo')") satisfies is-contract-error-str
  run-str("fun id(x): x end\n fun f(x) -> Number: if true: id(x) else: id(x) end end\n f('foo')") satisfies is-contract-error-str
  run-str("fun id(x): x end\n fun f(x) -> Number: cases(List) empty: | empty => id(x) | link(_, _) => id(x) end end\n f('foo')") satisfies is-contract-error-str
end

check "should work for method-bound annotations":
  run-str("o = { m(self, x :: Number): x end }\n o.m('foo')") satisfies is-contract-error-str
  run-str("o = { m(self, x) -> Number: x end }\n o.m('foo')") satisfies is-contract-error-str
  run-str(```
    o = {
      id(self, n): n end,
      m(self, x) -> Number: self.id(x) end
    }
    o.m('foo')```) satisfies is-contract-error-str
  run-str(```
    o = {
      m(self, x) -> Number: if true: x else: x end end
    }
    o.m('foo')```) satisfies is-contract-error-str
  run-str(```
    o = {
      m(self, x) -> Number: cases(List) empty:
        | empty => x
        | link(_, _) => x
      end end
    }
    o.m('foo')```) satisfies is-contract-error-str
  run-str(```
    o = {
      id(self, n): n end,
      m(self, x) -> Number: if true: self.id(x) else: self.id(x) end end
    }
    o.m('foo')```) satisfies is-contract-error-str
  run-str(```
    o = {
      id(self, n): n end,
      m(self, x) -> Number: cases(List) empty:
        | empty => self.id(x)
        | link(_, _) => self.id(x)
      end end
    }
    o.m('foo')```) satisfies is-contract-error-str
end


check "should work for standalone binding constructs":
  run-str("type N = Number\nx :: N = 'foo'") satisfies is-contract-error-str
  run-str("newtype Lyst as LystT").success is true
end

check "should work for data":
  run-str("data D: | var1 end\nx :: D = 5") satisfies is-contract-error-str
  run-str("data D: | var1 end\nx :: D = var1").success is true
end

check "should work with deep refinements":
  long-is-even = "fun is-even(n): if n == 0: true else if n < 0: false else: is-even(n - 2) end end\n"
  run-str(
    long-is-even +
    "x :: Number % (is-even) = 100000"
  ).success is true
  run-str(
    long-is-even +
    "x :: Number % (is-even) = 100000\n" +
    "x2 :: Number % (is-even) = x + 1"
  ) satisfies is-contract-error-str
  run-str(
    long-is-even +
    "x :: Number % (is-even) = 100001"
  ) satisfies is-contract-error-str
  run-str(
    long-is-even +
    "fun f(x) -> Number % (is-even): if is-number(x) and is-even(x): x else: 100001 end end\n" +
    "f(10000)"
  ).success is true
  run-str(
    long-is-even +
    "fun f(x) -> Number % (is-even): if is-number(x) and is-even(x): x else: 100001 end end\n" +
    "f(\"nan\")"
  ) satisfies is-contract-error-str
end 


check "should work with named refinements":
  data-def = ```

  fun true-of-n(n): if n == 0: true else: true-of-n(n - 1);;
  fun false-of-n(n): if n == 0: false else: false-of-n(n - 1);;

  type TrueNum = Number % (true-of-n)
  type FalseNum = Number % (false-of-n)

  data D:
    | c(a :: TrueNum)
    | c3(a1 :: TrueNum, a2 :: TrueNum, a3 :: TrueNum)
    | d(b :: FalseNum)
    | d2(a :: TrueNum, b :: FalseNum)
  end

  check:
    [list: c(10000), c(10000)].first.a is 10000
  end
  ```
  run-str(data-def).render-check-results() satisfies string-contains(_, "Looks shipshape")

end

check:

  data-def = ```
    data D: d end
    cases(D) 5:
      | c => true
    end
  ```

  run-str(data-def) satisfies is-contract-error-str
end
