import file("../test-compile-helper.arr") as C
import file("../../../src/arr/compiler/compile-structs.arr") as CS

run-str = C.run-str
output = C.output
compile-error = C.compile-error

check:
  ans = run-task(lam(): C.get-compile-errs(```
check:
  avg3(one(1)) is 1
  avg3(more(1, more(2, one(3))) is 2
    avg3(more(1, more(2, more(3, one(10))))) is 4
end
```)
end)

  exn-unwrap(ans.v).render-reason() does-not-raise
end