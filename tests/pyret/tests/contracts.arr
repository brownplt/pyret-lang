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
run-str = lam(str): exec-result(compile-str(str));

fun is-contract-error-str(result):
  string-contains(result.render-error-message(), "Contract Error")
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
