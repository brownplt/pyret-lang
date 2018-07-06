import file("../test-compile-helper.arr") as C
import file("../../../src/arr/compiler/compile-structs.arr") as CS

run-str = C.run-str
output = C.output
compile-error = C.compile-error

check "https://github.com/brownplt/pyret-lang/issues/1178":
  ans = C.get-compile-errs(```
check:
  f(x)
end
```)

  ans.length() is 1
end

check "https://github.com/brownplt/pyret-lang/issues/1178":
  ans = C.get-compile-errs(```
check:
  f(5) f(6)
end
```)

  ans.length() is 1
end
