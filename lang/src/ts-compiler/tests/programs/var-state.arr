var counter = 0
fun bump() block:
  counter := counter + 1
  counter
end
bump()
bump()
print(tostring(bump()) + "\n")

fun make-counter() block:
  var c = 0
  lam() block:
    c := c + 1
    c
  end
end

c1 = make-counter()
c1()
print(tostring(c1()) + "\n")

a = [raw-array: 1, 2, 3]
raw-array-set(a, 1, 42)
print(tostring(raw-array-get(a, 1)) + "\n")

when counter > 2:
  print("counter big\n")
end
