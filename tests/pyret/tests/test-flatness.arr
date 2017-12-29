import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../test-compile-helper.arr") as C

fun provides-of-program(str):
  C.get-compile-result("provide *\nprovide-types *\n" + str).provides
end
pp = provides-of-program

fun flatness-of-fun(str):
  provides-of-program(str).values.get-value("f").flatness
end
ff = flatness-of-fun

fun flatness-of-fun-body(str):
  flatness-of-fun("fun f():\n" + str + "\nend")
end
ffb = flatness-of-fun-body

check "returns-constant": ff("fun f(): 8 end") is some(0) end
check "arg-returns-constant": ff("fun f(x): 8 end") is some(0) end
check "args-returns-constant": ff("fun f(x, y, z): 8 end") is some(0) end

check "fun-id": ff("fun f(y): y end") is some(0) end
check "lam-id": ff("f = lam(y): y end") is some(0) end

check "dot": ff("f = lam(o): o.x end") is some(0) end
check "bang": ff("f = lam(o): o!x end") is some(0) end
check "tuple-get": ff("f = lam(o): o.{3} end") is some(0) end
check "lam": ff("f = lam(o): lam(p): o + p end end") is some(0) end

check "letrec": ff("f = lam(n): letrec o = 10, p = 11: {o; p} end end") is some(0) end
check "let": ff("f = lam(n): let o = 10, p = 11: {o; p} end end") is some(0) end

check "tuple": ff("f = lam(o, p): {o; p} end") is some(0) end
check "object": ff("f = lam(o, p): {o: o, p: p} end") is some(0) end

# NOTE(joe): The binary operators are not flat because they may
# call a method, make sure this is the case
check "plus": ff("f = lam(o): o + 1 end") is none end
check "minus": ff("f = lam(o): o - 1 end") is none end
check "times": ff("f = lam(o): o * 1 end") is none end
check "divide": ff("f = lam(o): o / 1 end") is none end


check "if-const": ff("f = lam(x, y, z): if x: y else: z end end") is some(0) end

check "if-nested": ff(```

f = lam(x, y, z):
  if if x: y else: z end:
    if y: 3 else: 4 end
  else:
    x
  end
end
                      ```) is some(0) end


