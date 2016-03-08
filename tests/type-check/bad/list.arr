fun f<A>(x :: List<A>) -> List<A>:
  x
end

a = f([list: "a", 1, "c"])

fun g(x :: List<Number>) -> List<Number>:
  x
end

b = g([list: 1, "a", 3])

fun h<A>(x :: List<List<A>>) -> List<List<A>>:
  x
end

c = h([list: [list: 1], [list: "a"], [list: 3]])
