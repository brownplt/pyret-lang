import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../test-compile-helper.arr") as C

fun cp(str):
  C.get-compile-result("provide *\nprovide-types *\n" + str).provides
end

check:
  p = cp("fun returns-constant(): 8 end")
  p.values.get-value("returns-constant").flatness is some(0)
end


check:
  p = cp("fun f(y): y end")
  p.values.get-value("f").flatness is some(0)
end

check:
  p = cp("x = lam(y): y end")
  p.values.get-value("x").flatness is some(0)
end
