#lang pyret

fun build-some-lists(n):
  case:
    | (n <= 0) => []
    | n > 0 => list.link(list.range(0, 100), build-some-lists(n - 1))
  end
end

build-some-lists(1000)
