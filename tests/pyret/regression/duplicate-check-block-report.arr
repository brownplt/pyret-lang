import file("../test-compile-helper.arr") as C
import file("../../../src/arr/compiler/compile-structs.arr") as CS

run-str = C.run-str
output = C.output
compile-error = C.compile-error

check "https://github.com/brownplt/pyret-lang/issues/1178":
  ans = C.get-compile-errs(```
check:
  num-sqr(x)
end
```)

  ans.length() is 2 # unbound-id 'x'
end

check "https://github.com/brownplt/pyret-lang/issues/1178":
  ans = C.get-compile-errs(```
check:
  num-sqr(5) num-sqr(6)
end
```)

  ans.length() is 1 # two expressions on same line
end
