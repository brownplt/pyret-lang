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

check "if-nested":
  ff(```

f = lam(x, y, z):
  if if x: y else: z end:
    if y: 3 else: 4 end
  else:
    x
  end
end
                      ```) is some(0)
end

check "builtins":
  # NOTE(joe): these should be defined in globals.js; just trying a handful to
  # ensure the pattern used there works and does not regress
  ff("f = lam(x): num-to-string(x) end") is some(1)
  ff("f = lam(x): string-split(x, \" \") end") is some(1)
  ff("f = lam(x): num-sqrt(x) end") is some(1)
  ff("f = lam(x): num-sin(num-cos(num-tan(x))) end") is some(1)
  ff("f = lam(x): within(x) end") is some(1)
end

check "nonflat builtins":
  ff("f = lam(x): print(x) end") is none
  ff("f = lam(x): print-error(x) end") is none
  ff("f = lam(x): display(x) end") is none
  ff("f = lam(x): display-error(x) end") is none
  ff("f = lam(x): to-string(x) end") is none
  ff("f = lam(x): tostring(x) end") is none
  ff("f = lam(x): to-repr(x) end") is none
  ff("f = lam(x): torepr(x) end") is none
end

check "call-a-flat":
  ff(```
g = lam(x): x end
f = lam(y): g(y) end
```) is some(1)
end

check "call-a-flat-2":
  ff(```
g = lam(x): x end
f = lam(y): g(g(y)) end
```) is some(1)
end

check "bind-nested-call":
  ff(```
g = lam(x): x end
f = lam(y): let x = g(g(y)): num-modulo(x, 2) end end
```) is some(1)
end

check "two-deep":
  ff(```
h = lam(x): x end
g = lam(x): h(x) end
f = lam(y): g(y) end
```) is some(2)
end

check "imported constructor":
  ff(```
include either
fun f(x):
  right(x)
end
```) is some(1)
end

check "direct constructor":
  ff(```
include either
data D:
  | f(x)
end
```) is some(0)
end

check "constructor":
  ff(```
data D:
  | c(x)
end
fun f(o):
  c(o)
end
```) is some(1)
end

check "constructor annotated":
  ff(```
data D:
  | c(x :: Number)
end
fun f(o):
  c(o)
end
```) is some(1)
end

#| TODO(joe): this is the next thing to do
check "constructor refined":
  ff(```
fun is-foo(x): x == "foo" end
data D:
  | c(x :: Number%(is-foo))
end
fun f(o):
  c(o)
end
```) is none
end
|#

