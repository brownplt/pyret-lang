import file("../test-compile-helper.arr") as C
import file("../../../src/arr/compiler/compile-structs.arr") as CS

run-str = C.run-str
output = C.output
compile-error = C.compile-error

check:
  run-str("method(self, arg) -> Any%(arg): 'no-good' end")
    is%(output) compile-error(CS.is-unbound-id)
  run-str("data Foo: sharing: method f(self, arg) -> Any%(arg): self end end")
    is%(output) compile-error(CS.is-unbound-id)
  run-str("method(self, arg :: Any%(self)): 'no-good' end")
    is%(output) compile-error(CS.is-unbound-id)
  run-str("data Foo: sharing: method f(self, arg :: Any%(self)): self end end")
    is%(output) compile-error(CS.is-unbound-id)
end
