#lang pyret

import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../test-compile-helper.arr") as CH
import contracts as C
import load-lib as L
import either as E

compile-str = CH.compile-str
run-to-result = CH.run-to-result

run-str = lam(str): 
  result = run-to-result(str)
  cases(E.Either) result block:
    | left(err) =>
      { program: str, success: false, runtime: false, errors: err }
    | right(ans) =>
      if L.is-success-result(ans):
        { program: str, success: true, runtime: true, ans: ans }
      else:
        { program: str, success: false, runtime: true, errors: ans }
      end
  end
end

#  compiled = C.compile-str(str)
#  cases(CS.CompileResult) compiled:
#    | ok(code) => exec-result(compiled)
#    | err(errs) => raise("Compilation failure when a run was expected " + torepr(errs) + "\n Program was:\n " + str)
#  end
#end

fun output(act, exp):
  if exp.success:
    act.success
  else:
    (exp.runtime == act.runtime) and exp.check-errors(act.errors)
  end
end

success = { success: true, runtime: true }

fun check-contract-error(errors):
  string-contains(L.render-error-message(errors).message, "annotation")
end

contract-error = { success: false, runtime: true, check-errors: check-contract-error }

fun field-error(fields):
  { success: false,
    runtime: true,
    check-errors: lam(errors):
        msg = L.render-error-message(errors).message
        for lists.all(f from fields):
          string-contains(msg, "field `" + f + "`")
        end
      end
  }
end

fun compile-error(check-err):
  {
    success: false,
    runtime: false,
    check-errors:
      lam(errors):
        for lists.any(err from errors):
          lists.any(check-err, err.problems)
        end
      end
  }
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
    run-str(program) is%(output) contract-error
  end
end

is-sorted = 
  ```
  fun is-sorted(l):
    cases(List) l:
      | empty => true
      | link(f, r) =>
        cases(List) r:
          | empty => true
          | link(f2, r2) =>
            (f <= f2) and is-sorted(r)
        end
    end
  end
  ``` + "\n"
check "should work for refinements":
  contract-errors = [list:
    "is-odd = lam(n): num-modulo(n, 2) == 1 end\nx :: String%(is-odd) = 5",
    "is-odd = lam(n): num-modulo(n, 2) == 1 end\nx :: Number%(is-odd) = 6",
    "is-zero-length = lam(s): string-length(s) == 0 end\ns :: String%(is-zero-length) = 'foo'"
  ]
  for each(program from contract-errors) block:
    run-str(program) is%(output) contract-error
  end
  non-errors = [list:
    "is-odd = lam(n): num-modulo(n, 2) == 1 end\nx :: Number%(is-odd) = 5",
    "is-zero-length = lam(s): string-length(s) == 0 end\ns :: String%(is-zero-length) = ''",
    is-sorted + "l :: Any%(is-sorted) = lists.range(0, 100)"
  ]
  for each(program from non-errors) block:
    run-str(program) is%(output) success
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
  for each(program from contract-errors) block:
    run-str(program.p) is%(output) contract-error # NOTE(Ben): used to be field-error(program.f)
  end

  non-errors = [list:
    is-sorted + "o :: { l1 :: Any%(is-sorted), l2 :: Any%(is-sorted) } = { l1: lists.range(0, 100), l2: lists.range(0, 100) }",
    "x :: {} = { x : 'foo' }"
  ]
  for each(program from non-errors) block:
    run-str(program) is%(output) success
  end
end

check "should notice unbound contracts":
  contract-errors = [list:
    "x :: NotAType = 5",
    "x :: (Number -> Fail) = 5",
    "x :: (Number -> { x:: Fail }) = 5",
    "x :: Numba%(is-even) = 5",
    "x :: lisst.List = 10",
    "y = 5\nx :: y = 5"
  ]
  for each(program from contract-errors) block:
    run-str(program) is%(output) compile-error(CS.is-unbound-type-id)
  end
end

check "should bind types":
  contract-errors = [list:
    "type-let N = Number: x :: N = 'foo'\n x end",
    "type-let S = String, N = Number: x :: (S -> N) = 'foo'\n x end"
  ]
  for each(program from contract-errors) block:
    run-str(program) is%(output) contract-error
  end
end

check "should work for lambda-bound annotations":
  run-str("fun f(x :: Number): x end\n f('foo')") is%(output) contract-error
  run-str("fun id(x): x end\n fun f(x) -> Number: id(x) end\n f('foo')") is%(output) contract-error
  run-str("fun f(x) -> Number: if true: x else: x end end\n f('foo')") is%(output) contract-error
  run-str("fun f(x) -> Number: cases(List) empty: | empty => x | link(_, _) => x end end\n f('foo')") is%(output) contract-error
  run-str("fun id(x): x end\n fun f(x) -> Number: if true: id(x) else: id(x) end end\n f('foo')") is%(output) contract-error
  run-str("fun id(x): x end\n fun f(x) -> Number: cases(List) empty: | empty => id(x) | link(_, _) => id(x) end end\n f('foo')") is%(output) contract-error
end

check "should work for method-bound annotations":
  run-str("o = { method m(self, x :: Number): x end }\n o.m('foo')") is%(output) contract-error
  run-str("o = { method m(self, x) -> Number: x end }\n o.m('foo')") is%(output) contract-error
  run-str(```
    o = {
      method id(self, n): n end,
      method m(self, x) -> Number: self.id(x) end
    }
    o.m('foo')```) is%(output) contract-error
  run-str(```
    o = {
      method m(self, x) -> Number: if true: x else: x end end
    }
    o.m('foo')```) is%(output) contract-error
  run-str(```
    o = {
      method m(self, x) -> Number: cases(List) empty:
        | empty => x
        | link(_, _) => x
      end end
    }
    o.m('foo')```) is%(output) contract-error
  run-str(```
    o = {
      method id(self, n): n end,
      method m(self, x) -> Number: if true: self.id(x) else: self.id(x) end end
    }
    o.m('foo')```) is%(output) contract-error
  run-str(```
    o = {
      method id(self, n): n end,
      method m(self, x) -> Number: cases(List) empty:
        | empty => self.id(x)
        | link(_, _) => self.id(x)
      end end
    }
    o.m('foo')```) is%(output) contract-error
end


check "should work for standalone binding constructs":
  run-str("type N = Number\nx :: N = 'foo'") is%(output) contract-error
  run-str("newtype Lyst as LystT") is%(output) success
end

check "should work for data":
  run-str("data D: | var1 end\nx :: D = 5") is%(output) contract-error
  run-str("data D: | var1 end\nx :: D = var1") is%(output) success
end

check "should work with deep refinements":
  long-is-even = "fun is-even(n): if n == 0: true else if n < 0: false else: is-even(n - 2) end end\n"
  run-str(
    long-is-even +
    "x :: Number % (is-even) = 100000"
  ) is%(output) success
  run-str(
    long-is-even +
    "x :: Number % (is-even) = 100000\n" +
    "x2 :: Number % (is-even) = x + 1"
  ) is%(output) contract-error
  run-str(
    long-is-even +
    "x :: Number % (is-even) = 100001"
  ) is%(output) contract-error
  run-str(
    long-is-even +
    "fun f(x) -> Number % (is-even): if is-number(x) and is-even(x): x else: 100001 end end\n" +
    "f(10000)"
  ) is%(output) success
  run-str(
    long-is-even +
    "fun f(x) -> Number % (is-even): if is-number(x) and is-even(x): x else: 100001 end end\n" +
    "f(\"nan\")"
  ) is%(output) contract-error
end 


check "should work with named refinements":
  data-def = ```

  fun true-of-n(n): if n == 0: true else: true-of-n(n - 1) end end
  fun false-of-n(n): if n == 0: false else: false-of-n(n - 1) end end

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
  run-str(data-def) is%(output) success

end

check "should work in cases positions":

  data-def = ```
    data D: d end
    cases(D) 5:
      | c => true
    end
  ```

  run-str(data-def) is%(output) contract-error

  data-def2 = ```
    data D: d end
    cases(D) 5:
      | c => true
      | else => 42
    end
  ```

  run-str(data-def2) is%(output) contract-error
end

check "Number contracts":

  run-str("x :: Exactnum = 5") is%(output) success
  run-str("x :: Exactnum = ~5") is%(output) contract-error
  run-str("x :: Roughnum = 5") is%(output) contract-error
  run-str("x :: Roughnum = ~5") is%(output) success
  run-str("x :: NumRational = ~5") is%(output) contract-error

  run-str("x :: NumInteger = 5.1") is%(output) contract-error
  run-str("x :: NumInteger = 5") is%(output) success

  run-str("x :: NumRational = 5.1") is%(output) success
  run-str("x :: NumRational = ~1") is%(output) contract-error

  run-str("x :: NumPositive = ~5") is%(output) success
  run-str("x :: NumPositive = ~-5") is%(output) contract-error
  run-str("x :: NumPositive = 0.1") is%(output) success
  run-str("x :: NumPositive = -0.1") is%(output) contract-error
  run-str("x :: NumPositive = 0") is%(output) contract-error

  run-str("x :: NumNegative = ~-5") is%(output) success
  run-str("x :: NumNegative = ~5") is%(output) contract-error
  run-str("x :: NumNegative = -0.1") is%(output) success
  run-str("x :: NumNegative = 0.1") is%(output) contract-error
  run-str("x :: NumNegative = 0") is%(output) contract-error

  run-str("x :: NumNonNegative = ~5") is%(output) success
  run-str("x :: NumNonNegative = ~-5") is%(output) contract-error
  run-str("x :: NumNonNegative = 0.1") is%(output) success
  run-str("x :: NumNonNegative = -0.1") is%(output) contract-error
  run-str("x :: NumNonNegative = 0") is%(output) success

  run-str("x :: NumNonPositive = ~-5") is%(output) success
  run-str("x :: NumNonPositive = ~5") is%(output) contract-error
  run-str("x :: NumNonPositive = -0.1") is%(output) success
  run-str("x :: NumNonPositive = 0.1") is%(output) contract-error
  run-str("x :: NumNonPositive = 0") is%(output) success
end

check "tuple contracts":
  run-str("x :: {Number; String; {Number; Number; {String}}; String} = {4124; \"frwfq\"; {5123;531;{\"fqswf\"}}; \"fqwfq\"}") is%(output) success
  run-str("x :: {String; String} = {\"sewhwr\"; 4124}") is%(output) contract-error
  run-str("x :: {Number; String; {Number; Number; {String}}; String} = {4124; \"frwfq\"; {5123;531;{5351}}; \"fqwfq\"}") is%(output) contract-error
  run-str("x :: {Number; Number; {Number; String}} = {412; 5412; {412; \"fgwdef\"; 5135}}") is%(output) contract-error
end

check "standalone contract statements":
  run-str(
    ```
    double :: Number -> Number
    fun double(n): n + n end
    double(5)
    ```
    ) is%(output) success
  run-str(
    ```
    double :: Number -> Number
    fun double(n): n + n end
    double("hi")
    ```) is%(output) contract-error
  run-str(
    ```
    double :: Number -> Number
    check: double(5) is 10 end
    fun double(n): n + n end
    ```
    ) is%(output) success
  run-str(
    ```
    fun double(n): n + n end
    double :: Number -> Number
    ```
    ) is%(output) compile-error(CS.is-contract-unused)
  run-str(
    ```
    include file
    input-file :: String -> Boolean
    ```
    ) is%(output) compile-error(CS.is-contract-on-import)
  run-str(
    ```
    double :: Number -> Number
    double :: Number -> Number
    fun double(n): n * 2 end
    ```) is%(output) compile-error(CS.is-contract-redefined)
  run-str(
    ```
    double :: Number -> Number
    fun double(n :: Number): n * 2 end
    ```) is%(output) compile-error(CS.is-contract-redefined)
  run-str(
    ```
    double :: Number
    fun double(n): n * 2 end
    ```) is%(output) compile-error(CS.is-contract-non-function)
  run-str(
    ```
    double :: Number -> Number
    double = 5
    ```) is%(output) compile-error(CS.is-contract-non-function)
  run-str(
    ```
    double :: (x :: Number) -> Number
    fun double(n): n * 2 end
    ```) is%(output) compile-error(CS.is-contract-inconsistent-names)
  run-str(
    ```
    is-even :: Number -> Boolean
    fun is-odd(n): true end
    is-odd :: Number -> Boolean
    fun is-even(n): true end
    ```) is%(output) compile-error(lam(e): CS.is-contract-bad-loc(e) and (e.name == "is-odd") end)
end

