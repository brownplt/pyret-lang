
var a = 5
b :: Number = a
c = a
d :: Number = c

a := 6

fun synth-assign():
  a := 7
  8
end

data Foo:
  | foo(ref bar :: Number)
end

my-foo = foo(5)

e :: Number = my-foo!bar # Checking
f = my-foo!bar # Synthesis
g :: Number = f
