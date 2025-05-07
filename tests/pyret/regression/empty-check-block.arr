import file("../test-compile-helper.arr") as C
import file("../../../src/arr/compiler/compile-structs.arr") as CS

run-str = C.run-str
output = C.output
compile-error = C.compile-error

check "https://github.com/brownplt/pyret-lang/issues/1486":
  ans = C.get-compile-errs(```
check:
end
```)

  ans.length() is 1
end

