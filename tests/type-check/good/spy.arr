spy:
  a: 1
end

x = 10
spy: x end

y = [list: 1, 2, 3]
spy: x, y end

fun square(num :: Number):
  spy "in square": num end
  num * num
end

fun cube(num :: Number):
  spy "in cube": num end
  num * num * num
end

square(x)
cube(x)
square(x)

fun reverse<A>(lst :: List<A>, sofar :: List<A>) -> List<A>:
  spy "lengths":
    lst-length: lst.length(),
    sofar-length: sofar.length(),
    sum: lst.length() + sofar.length()
  end
  cases(List<A>) lst:
    | empty => sofar
    | link(first, rest) =>
      reverse(rest, link(first, sofar))
  end
end

check:
  reverse([list: "a", "b", "c"], empty) is [list: "c", "b", "a"]
end

spy:
  x,
  y: 20
end

fun f(n :: Number):
  n * n
end
for each(i from range(0, 10)):
  result = f(i)
  spy "iteration " + to-string(i):
    result
  end
  result
end