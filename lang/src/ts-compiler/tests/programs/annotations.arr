fun add1(n :: Number) -> Number:
  n + 1
end
print(tostring(add1(5)) + "\n")

fun greet(s :: String) -> String:
  "hi " + s
end
print(greet("there") + "\n")

x :: Number = 10
print(tostring(x) + "\n")

data Box: | box(ref v) end
b = box(5)
b!{v: 7}
print(tostring(b!v) + "\n")
