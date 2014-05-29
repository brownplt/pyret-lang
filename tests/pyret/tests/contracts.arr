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
  string-contains(result.render-error-message(), "Contract Error")
end

fun is-refinement-error-str(result):
  string-contains(result.render-error-message(), "Predicate")
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
    "is-odd = lam(n): num-modulo(n, 2) == 1 end x :: String%(is-odd) = 5",
    "is-odd = lam(n): num-modulo(n, 2) == 1 end x :: Number%(is-odd) = 6",
    "is-zero-length = lam(s): string-length(s) == 0 end s :: String%(is-zero-length) = 'foo'"
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
    "is-odd = lam(n): num-modulo(n, 2) == 1 end x :: Number%(is-odd) = 5",
    "is-zero-length = lam(s): string-length(s) == 0 end s :: String%(is-zero-length) = ''",
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
    { p: "type-let R = { x :: Number }: x :: R = { r: lam(v): v end } x end",
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
    is-sorted + "o :: { l1 :: Any%(is-sorted), l2 :: Any%(is-sorted) } = { l1: lists.range(0, 100), l2: lists.range(0, 100) }"
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
    "y = 5 x :: y = 5"
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
    "type-let N = Number: x :: N = 'foo' x end",
    "type-let S = String, N = Number: x :: (S -> N) = 'foo' x end"
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
  run-str("fun f(x :: Number): x end f('foo')") satisfies is-contract-error-str
  run-str("fun f(x) -> Number: x end f('foo')") satisfies is-contract-error-str
end

check "should work for standalone binding constructs":
  run-str("type N = Number x :: N = 'foo'") satisfies is-contract-error-str
  run-str("newtype List as ListT").success is true
end
