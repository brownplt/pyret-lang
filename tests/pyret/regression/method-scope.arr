import "../test-compile-helper.arr" as C
import "compiler/compile-structs.arr" as CS

check:
  fun c(str):
    result = C.compile-str(str)
    result satisfies CS.is-err
    cases(CS.CompileResult) result:
      | ok(_) => "No error for " + str
      | err(probs) => probs.first
    end
  end

  c("method(self, arg) -> Any%(arg): 'no-good' end") satisfies CS.is-unbound-id
  c("data Foo: sharing: f(self, arg) -> Any%(arg): self; end") satisfies CS.is-unbound-id
  c("method(self, arg :: Any%(self)): 'no-good' end") satisfies CS.is-unbound-id
  c("data Foo: sharing: f(self, arg :: Any%(self)): self; end") satisfies CS.is-unbound-id
end
