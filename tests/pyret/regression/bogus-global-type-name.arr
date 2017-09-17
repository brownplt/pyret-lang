import file("../test-compile-helper.arr") as C
import file("../../../src/arr/compiler/compile-structs.arr") as CS

run-str = C.run-str
output = C.output
compile-error = C.compile-error

check "https://github.com/brownplt/pyret-lang/issues/815":
  run-str("x :: Bool = true")
    is%(output) compile-error(CS.is-unbound-type-id)
  run-str("fun f(x :: Bool): x end")
    is%(output) compile-error(CS.is-unbound-type-id)
  run-str("x :: { y :: Num } = {y: 5}")
    is%(output) compile-error(CS.is-unbound-type-id)
end
