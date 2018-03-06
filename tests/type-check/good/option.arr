fun to-nothing(x :: Number) -> Nothing:
  nothing
end

fun test(x :: Option<Number>) -> Nothing:
  x.and-then(to-nothing)
end
