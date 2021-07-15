import file("../test-compile-helper.arr") as C
import file("../../../src/arr/compiler/compile-structs.arr") as CS

run-str = C.run-str
output = C.output
compile-error = C.compile-error

check "desugaring of named arrow annotations works":
  ans = C.get-compile-errs(```
func1 :: ((var1 :: Number) -> Number) = lambda(var1): 5 end 
```)

  ans.length() is 0
end

