#lang pyret

import "compiler/compile-structs.arr" as CS
import "../test-compile-helper.arr" as C

fun get-compile-errors(str):
  compiled = C.compile-str(str)
  cases(CS.CompileResult) compiled:
    | ok(code) => raise("Code compiles successfully: " + torepr(code))
    | err(errs) => errs
  end
end

fun expect-wf-error(str):
  errs = get-compile-errors(str)
  if CS.is-wf-err(errs.first):
    errs.first.msg
  else:
    raise("Compile error is not a well-formedness error: " + torepr(errs))
  end
end

check "should detect last statement":
  s-contract = ```
    block:
      123
      v :: Number
    end
  ```
  expect-wf-error(s-contract) is "Cannot end a block in a contract"

  s-let = ```
    block:
      123
      a = 1
    end
  ```
  expect-wf-error(s-let) is "Cannot end a block in a let-binding"
end