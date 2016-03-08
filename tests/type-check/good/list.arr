fun f<A>(x :: List<A>) -> List<A>:
  x
end

a = f([list: "a", "b", "c"])

fun g(x :: List<Number>) -> List<Number>:
  x
end

b = g([list: 1, 2, 3])

fun h<A>(x :: List<List<A>>) -> List<List<A>>:
  x
end

c = h([list: [list: 1], [list: 2], [list: 3]])
